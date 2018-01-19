; Copyright (c) 2015 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Text Field widget"
      :author "Denys Lebediev"}
  flatgui.widgets.textfield2
  (:require [flatgui.awt :as awt]
            [flatgui.base :as fg]
            [flatgui.widgets.component]
            [flatgui.widgets.scrollpanel]
            [flatgui.widgets.textcommons :as textcommons]
            [flatgui.inputchannels.keyboard :as keyboard]
            [flatgui.inputchannels.mouse :as mouse]
            [flatgui.inputchannels.mousewheel :as mousewheel]
            [flatgui.inputchannels.timer :as timer]
            [flatgui.inputchannels.clipboard :as clipboard]
            [flatgui.inputchannels.awtbase :as inputbase]
            [flatgui.util.matrix :as m]
            [flatgui.comlogic :as fgc])
  (:import [java.awt.event KeyEvent]
           (flatgui.core.engine.ui FGTransferable)
           (java.util ArrayList)))


(def default-style
  {:font nil
   :foreground :prime-6
   :background :prime-4})

(defn glyph [type data style]
  {:type type
   :data data
   :style style})

(defn char-glyph
  ([c style]
   (condp = c
     \space (glyph :whitespace nil style)
     \newline (glyph :linebreak nil style)
     (glyph :char c style)))
  ([c] (char-glyph c default-style)))

(def linebreak-glyph (char-glyph \newline))

(def whitespace-glyph (char-glyph \space))

(defn image-glyph [image-url size] {:type :image :data image-url :style {:size size}})

(defn video-glyph [video-url size] {:type :video :data video-url :style {:size size}})

(defn whitespace? [g] (= :whitespace (:type g)))

(defn linebreak? [g] (= :linebreak (:type g)))

(defn delimiter? [g] (#{:whitespace :linebreak} (:type g)))

(defmulti glyph-size (fn [g _interop] (:type g)))

(def empty-glyph-size {:w 0 :h 0})

(defn- text-size [interop text font]
  {:w (.getStringWidth interop text font)
   :h (.getFontHeight interop font)})

;; TODO cache the size in the word instance
(defmethod glyph-size :char [g interop]
  (let [font (:font (:style g))
        text (str (:data g))]
    (text-size interop text font)))

(defmethod glyph-size :whitespace [g interop]
  (let [font (:font (:style g))]
    (text-size interop " " font)))

(defmethod glyph-size :linebreak [_g _interop] empty-glyph-size)

(defmethod glyph-size :test [g _interop] {:w (:w (:style g)) :h (:h (:style g))})

(defmethod glyph-size :image [g _interop] (:size (:style g)))

(defmethod glyph-size :video [g _interop] (:size (:style g)))

(defn word->str [word] (apply str (map :data (:glyphs word))))


(defrecord Model [lines caret-line mark-line])

(defrecord Line [words caret-word mark-word h primitives])

(defrecord Word [glyphs caret-pos mark-pos w-content w-total h]
  Object
  (toString [word] (word->str word)))

(defrecord Primitive [type data style x])


(defn glyph-type->primitive-type [g]
  (let [type (:type g)]
    (cond
      (= type :char) :string
      (= type :whitespace) :string
      (= type :linebreak) nil
      :else type)))

(defn glyps->primitive-data [glyphs primitive-type]
  (if (= primitive-type :string)
    (apply str (map :data glyphs))
    glyphs))

(defn create-render-line-primitives-transducer []
  (fn [rf]
    (let [glyphs-state (volatile! [])
          type-state (volatile! nil)
          style-state (volatile! nil)
          w-total-state (volatile! 0)
          x-state (volatile! 0)]
      (fn
        ([] (rf))
        ([result]
         (let [glyphs @glyphs-state]                         ; TODO if last glyph is linebreak then this will go to else branch, right?
           (if (not (empty? glyphs))
             (let [style @style-state
                   type @type-state
                   x @x-state
                   data (glyps->primitive-data glyphs type)
                   p (Primitive. type data style x)]
               (rf result p))
             result)))
        ([result g]
         (if-let [g-type (glyph-type->primitive-type g)]
           (let [glyphs @glyphs-state
                 style @style-state
                 type @type-state
                 ;g-w (:w (if-let [size (:size g)] size (throw (IllegalStateException. (str "Glyph must be sized at this point. g=" g)))))
                 g-w (:w (if-let [size (:size g)] size 1))
                 g-style (:style g)
                 empty-glyphs (empty? glyphs)]
             (if (or empty-glyphs (= style g-style) (= type g-type))
               (do
                 (vswap! glyphs-state conj g)
                 (vswap! w-total-state + g-w)
                 (if empty-glyphs
                   (do
                     (vreset! type-state g-type)
                     (vreset! style-state g-style)))
                 result)
               (let [data (glyps->primitive-data glyphs type)
                     p (Primitive. type data style @x-state)]
                 (vreset! glyphs-state [g])
                 (vreset! type-state g-type)
                 (vreset! style-state g-style)
                 (vswap! x-state + @w-total-state)
                 (vreset! w-total-state g-w)
                 (rf result p))
               ))
           result))))))

(defn make-line [words caret-word mark-word h]
  (let [glyphs (mapcat :glyphs words)
        primitives (transduce (create-render-line-primitives-transducer) conj glyphs)]
    (Line. words caret-word mark-word h primitives)))

(defn make-word [caret-pos w-content w-total h w-g total-g-count source-g-count]
  (let [w-g-count (.size w-g) ;TODO w-g-count
        cp-up-bound-fn (if (< total-g-count source-g-count) < <=)
        word-caret-pos (if (and caret-pos (>= caret-pos (- total-g-count w-g-count)) (cp-up-bound-fn caret-pos total-g-count)) (- caret-pos (- total-g-count (.size w-g))))]
    (Word.
      (vec (.toArray w-g))
      word-caret-pos
      word-caret-pos
      w-content
      w-total
      h)))

(defn create-make-words-transducer [caret-pos w interop source-g-count]
  (fn [rf]
    (let [state (volatile! {:w-content 0 :w-total 0 :h 0 :w-g (ArrayList.) :total-g-count 0 :init-whitespace true})]
      (fn
        ([] (rf))
        ([result]
         (let [s @state
               w-content (:w-content s)
               w-total (:w-total s)
               h (:h s)
               w-g (:w-g s)
               total-g-count (:total-g-count s)]
           (if (pos? (count w-g)) (rf result (make-word caret-pos w-content w-total h w-g total-g-count source-g-count)) result)))
        ([result g]
         (let [s @state
               w-content (:w-content s)
               w-total (:w-total s)
               h (:h s)
               w-g (:w-g s)
               total-g-count (:total-g-count s)
               whitespace (whitespace? g)
               init-whitespace (:init-whitespace s)
               cached-g-size (:size g) ; Size might be already cached in glyph, e.g. if this called from kill-glyphs. Also will need size further.
               g-size (if cached-g-size (:size g) (glyph-size g interop))
               sized-g (if cached-g-size g (assoc g :size g-size))
               g-w (:w g-size)
               effective-g-w (if (or (not whitespace) (:init-whitespace s)) g-w 0)
               g-h (:h g-size)
               w&g-content (+ w-content effective-g-w)
               w&g-total (+ w-total g-w)
               w&h-h (max h g-h)]
           (if (> w&g-content w)
             (do
               (vreset! state {:w-content effective-g-w :w-total g-w :h g-h :w-g (let [a (ArrayList.)] (do (.add a sized-g) a)) :total-g-count (inc total-g-count) :init-whitespace whitespace})
               (if (pos? (count w-g)) (rf result (make-word caret-pos w-content w-total h w-g total-g-count source-g-count))))
             (do
               (vreset! state {:w-content w&g-content :w-total w&g-total :h w&h-h :w-g (do (.add w-g sized-g) w-g) :total-g-count (inc total-g-count) :init-whitespace (if init-whitespace whitespace false)})
               result))))))))

(defn make-words ([glyphs caret-pos w interop] (transduce (create-make-words-transducer caret-pos w interop (count glyphs)) conj glyphs)))

(defmulti glyph-> (fn [entity _g _w _interop] (class entity)))

(defmethod glyph-> Word [word g w interop]
  (let [caret-pos (:caret-pos word)
        glyphs (:glyphs word)
        g-is-delimiter (delimiter? g)
        g-goes-after-whitespace (and (> caret-pos 0) (whitespace? (nth glyphs (dec caret-pos))))
        part-before-caret-pos (vec (take caret-pos glyphs))
        part-after-caret-pos (vec (drop caret-pos glyphs))]
    (cond

      (and (not g-is-delimiter) (not g-goes-after-whitespace))
      (make-words (vec (concat part-before-caret-pos (list g) part-after-caret-pos)) (inc caret-pos) w interop)

      (and (not g-is-delimiter) g-goes-after-whitespace)
      (concat
        (make-words (vec part-before-caret-pos) nil w interop)
        (make-words (vec (conj part-after-caret-pos g)) 1 w interop))

      (and (whitespace? g) (not g-goes-after-whitespace) (some #(not (whitespace? %)) part-before-caret-pos))
      (concat
        (make-words (vec (conj part-before-caret-pos g)) (inc caret-pos) w interop)
        (make-words (vec part-after-caret-pos) nil  w interop))

      (whitespace? g)
      (make-words (vec (concat part-before-caret-pos (list g) part-after-caret-pos)) (inc caret-pos) w interop)

      (and (linebreak? g))
      (concat
        (make-words (vec part-before-caret-pos) nil w interop)
        (make-words (vec part-after-caret-pos) 0 w interop)))))

(defn make-glyph-line-reducer [w interop]
  (fn [words g]
    (concat (butlast words) (glyph-> (last words) g w interop))))

(defn glyphs->words [glyphs w interop]
  (reduce
    (make-glyph-line-reducer w interop)
    [(make-word 0 0 0 0 [] 0 0)]
    glyphs))

(defn lines->strings [lines]
  (mapv (fn [line] (mapv (fn [word] (word->str word)) line)) lines))

(defn lines->total-word-widths [lines]
  (mapv (fn [line] (mapv (fn [word] (:w-total word)) line)) lines))

(defn lines->line-heights [lines] (mapv :h lines))

(defn wrap-lines
  ([w]
    (fn [rf]
      (let [line-state (volatile! [])
            line-w-state (volatile! 0)
            line-h-state (volatile! 0)
            line-caret-index-state (volatile! 0)
            line-caret-met-state (volatile! false)
            model-caret-index-state (volatile! 0)
            model-caret-met-state (volatile! false)]
        (fn
          ([] (rf))
          ([result]
           (let [caret-word (if @line-caret-met-state @line-caret-index-state)
                 final-result (rf result (make-line @line-state caret-word caret-word @line-h-state))
                 caret-line (if @model-caret-met-state @model-caret-index-state)]
             (Model. final-result caret-line caret-line)))
          ([result word]
           (let [line @line-state
                 line-w @line-w-state
                 line-h @line-h-state
                 w-content (:w-content word)
                 w-total (:w-total word)
                 word-h (:h word)
                 has-caret (:caret-pos word)
                 end-line (> (+ line-w w-content) w)
                 process-caret (fn []
                                 (if has-caret
                                   (do
                                     (vreset! model-caret-met-state true)
                                     (vreset! line-caret-met-state true))
                                   (if (not @line-caret-met-state) (vswap! line-caret-index-state inc))))]
             (do
               (if end-line
                 (let [line-caret-index (if @line-caret-met-state @line-caret-index-state)]
                   (vreset! line-state [word])
                   (vreset! line-w-state w-total)
                   (vreset! line-h-state word-h)
                   (vreset! line-caret-index-state 0)
                   (vreset! line-caret-met-state false)
                   (if (not @model-caret-met-state) (vswap! model-caret-index-state inc))
                   (process-caret)
                   (rf result (make-line line line-caret-index line-caret-index line-h)))
                 (do
                   (vreset! line-state (conj line word))
                   (vreset! line-w-state (+ line-w w-total)) ;TODO vswap!
                   (vswap! line-h-state max word-h)
                   (process-caret)
                   result)))))))))
  ([words w] (transduce (wrap-lines w) conj words)))

;; Example 1:
;; line 0 - will be contained in prior-lines
;; line 1 - so line-num-to-start-rewrap will be = 1
;; line 2 - caret/mark line; caret-line-and-following-words will contain words of all lines starting from this one
;; line 3 ...
;;
;; Example 2:
;; line 0 - caret/mark line; line-num-to-start-rewrap will be = -1;
;; line 1 ...
(defn rewrap-partially [model w caret-line-and-following-words]
  (let [line-num-to-start-rewrap (dec (min (:caret-line model) (:mark-line model)))]
    (if (>= line-num-to-start-rewrap 0)
      (let [prior-lines (take line-num-to-start-rewrap (:lines model))
            words-to-wrap (concat
                            (:words (nth (:lines model) line-num-to-start-rewrap))
                            caret-line-and-following-words)
            remainder-model (wrap-lines words-to-wrap w)
            result-caret-line (+ (count prior-lines) (:caret-line remainder-model))]
        (Model. (vec (concat prior-lines (:lines remainder-model))) result-caret-line result-caret-line)) ;TODO resut-mark-line
      (wrap-lines caret-line-and-following-words w))))


;;; TODO more than one glyph at once (e.g. from clipboard) and only then rewrap

(defmethod glyph-> Model [model g w interop]
  (let [line-with-caret (nth (:lines model) (:caret-line model))
        caret-word-index (:caret-word line-with-caret)
        word-with-caret (nth (:words line-with-caret) caret-word-index)
        remainder-words (concat
                          (flatten (assoc (:words line-with-caret) caret-word-index (glyph-> word-with-caret g w interop)))
                          (mapcat :words (take-last (- (count (:lines model)) (:caret-line model) 1) (:lines model))))]
    (rewrap-partially model w remainder-words)))

(defn has-selection? [model]
  (if (not= (:caret-line model) (:mark-line model))
    true
    (let [line (nth (:lines model) (:caret-line model))]
      (if (not= (:caret-word line) (:mark-word line))
        true
        (let [word (nth (:words line) (:caret-word line))]
          (not= (:caret-pos word) (:mark-pos word)))))))

(defmulti move-caret-mark (fn [_model what where _viewport-h _interop]
                            (assert (#{:caret :mark :caret-&-mark} what))
                            where))

(defn- move-for-keys [model key-count model-transrofm-fn]
  (loop [m model
         k 0]
    (if (< k key-count)
      (recur
        (model-transrofm-fn m k)
        (inc k))
      m)))

(defn- move-mark-to-caret-if-needed [model move-caret move-mark]
  (if (and move-caret move-mark)
    (let [caret-line-index (:caret-line model)
          mark-line-index (:mark-line model)
          caret-line (nth (:lines model) caret-line-index)
          mark-line (nth (:lines model) mark-line-index)
          caret-word-index (:caret-word caret-line)
          mark-word-index (:mark-word mark-line)
          caret-word (nth (:words caret-line) caret-word-index)
          mark-pos (:mark-pos caret-word)
          caret-pos (:caret-pos caret-word)]
      (if (or
            (not= caret-pos mark-pos)
            (not= caret-word-index mark-word-index)
            (not= caret-line-index mark-line-index))
        (->
          (assoc-in model [:lines mark-line-index :words mark-word-index :mark-pos] nil)
          (assoc-in [:lines mark-line-index :mark-word] nil)
          (assoc-in [:lines caret-line-index :mark-word] caret-word-index)
          (assoc-in [:lines caret-line-index :words caret-word-index :mark-pos] caret-pos)
          (assoc :mark-line caret-line-index))
        model))
    model))

(defn move-caret-mark-1-char [model move-caret move-mark edge-fn edge-last-in-line-fn move-fn]
  (let [src-line-key (if move-caret :caret-line :mark-line)
        src-word-key (if move-caret :caret-word :mark-word)
        src-pos-key (if move-caret :caret-pos :mark-pos)
        line-index (src-line-key model)
        lines (:lines model)
        line (nth lines line-index)
        word-index (src-word-key line)
        words (:words line)
        word (nth words word-index)
        pos (src-pos-key word)
        edge-line (= line-index (edge-fn lines))
        edge-word (= word-index (edge-fn words))
        edge-glyph (= pos ((if (= word-index (dec (count words))) edge-last-in-line-fn edge-fn) (:glyphs word)))
        dest-pos-keys (filter keyword? [(if move-caret :caret-pos) (if move-mark :mark-pos)])
        dest-word-keys (filter keyword? [(if move-caret :caret-word) (if move-mark :mark-word)])
        dest-line-keys (filter keyword? [(if move-caret :caret-line) (if move-mark :mark-line)])
        key-count (count dest-pos-keys)]
    (cond
      (not edge-glyph) (move-mark-to-caret-if-needed
                         (move-for-keys model key-count (fn [m k] (assoc-in m [:lines line-index :words word-index (nth dest-pos-keys k)] (move-fn pos))))
                         move-caret move-mark)
      (not edge-word) (move-mark-to-caret-if-needed
                        (move-for-keys
                          model
                          key-count
                          (fn [m k] (let [new-word-index (move-fn word-index)
                                          backward (< new-word-index word-index)
                                          new-pos (if backward
                                                    (dec (count (get-in model [:lines line-index :words new-word-index :glyphs])))
                                                    0)]
                                      (->
                                        (assoc-in m [:lines line-index :words word-index (nth dest-pos-keys k)] nil)
                                        (assoc-in [:lines line-index (nth dest-word-keys k)] new-word-index)
                                        (assoc-in [:lines line-index :words new-word-index (nth dest-pos-keys k)] new-pos)))))
                        move-caret move-mark)
      (not edge-line) (let [new-line-index (move-fn line-index)
                            backward (< new-line-index line-index)
                            new-word-index (if backward
                                             (let [word-count (count (get-in model [:lines new-line-index :words]))] (dec word-count))
                                             0)
                            new-pos (if backward
                                            (count (get-in model [:lines new-line-index :words new-word-index :glyphs]))
                                            0)]
                        (move-for-keys
                          model
                          key-count
                          (fn [m k] (->
                                      (assoc-in m [:lines line-index :words word-index (nth dest-pos-keys k)] nil)
                                      (assoc-in [(nth dest-line-keys k)] new-line-index)
                                      (assoc-in [:lines line-index (nth dest-word-keys k)] nil)
                                      (assoc-in [:lines new-line-index (nth dest-word-keys k)] new-word-index)
                                      (assoc-in [:lines new-line-index :words new-word-index (nth dest-pos-keys k)] new-pos)))))
      :else model)))

(defn move-caret-1-char [model edge-fn edge-last-in-line-fn move-fn]
  (move-caret-mark-1-char model true false edge-fn edge-last-in-line-fn move-fn))

(defn move-mark-1-char [model edge-fn edge-last-in-line-fn move-fn]
  (move-caret-mark-1-char model false true edge-fn edge-last-in-line-fn move-fn))

(defn move-caret-&-mark-1-char [model edge-fn edge-last-in-line-fn move-fn]
  (move-caret-mark-1-char model true true edge-fn edge-last-in-line-fn move-fn))

(defn move-caret-mark-generic [model what edge-fn edge-last-in-line-fn move-fn]
  (condp = what
    :caret (move-caret-1-char model edge-fn edge-last-in-line-fn move-fn)
    :mark (move-mark-1-char model edge-fn edge-last-in-line-fn move-fn)
    :caret-&-mark (move-caret-&-mark-1-char model edge-fn edge-last-in-line-fn move-fn)))

(defmethod move-caret-mark :forward [model what _where _viewport-h _interop]
  (move-caret-mark-generic model what (fn [coll] (dec (count coll))) (fn [coll] (count coll)) inc))

(defmethod move-caret-mark :backward [model what _where _viewport-h _interop]
  (move-caret-mark-generic model what (fn [_] 0) (fn [_] 0) dec))

(defn truncated-word-reducer [words word]
  (cond
    (or (nil? word) (empty? (:glyphs word)))
    words

    (or
      (every? whitespace? (:glyphs word))
      (and (not (empty? words)) (not (whitespace? (last (:glyphs (last words)))))))
    (let [last-word (last words)
          result-caret-pos (cond
                             (:caret-pos last-word) (:caret-pos last-word)
                             (:caret-pos word) (+ (:caret-pos word) (count (:glyphs last-word)))
                             :else nil)]
      (conj
        (vec (butlast words))
        (Word.
          (vec (concat (:glyphs last-word) (:glyphs word)))
          result-caret-pos
          result-caret-pos
          (+ (:w-content last-word) (:w-content word))
          (+ (:w-total last-word) (:w-total word))
          (max (:h last-word) (:h word)))))

    :else
    (conj words word)))

(defn truncate-words [words] (vec (reduce truncated-word-reducer [] words)))

(defn- kill-glyph-range-in-word [word from-index-incl to-index-excl w interop]
  (let [old-glyphs (:glyphs word)
        glyphs (mapv (fn [i] (nth old-glyphs i)) (filter (fn [i] (not (and (>= i from-index-incl) (< i to-index-excl)))) (range (count old-glyphs))))
        replacement-words (make-words glyphs (min from-index-incl to-index-excl) w interop)]
    (assert (= (count replacement-words) 1) (str "Kill resulted in words: " (mapv str replacement-words)))
    (first replacement-words)))

(defn- kill-glyphs-in-word [word first-in-word-range last-in-word-range w interop]
  (let [                                                    ;old-glyphs (:glyphs word)
        old-glyph-count (count (:glyphs word))
        caret (:caret-pos word)
        mark (:mark-pos word)]
    (cond
      (or
        (and (nil? caret) (nil? mark))
        (and (= caret 0) (= mark old-glyph-count))
        (and (= caret old-glyph-count) (= mark 0)))
      (if (or first-in-word-range last-in-word-range) (throw (IllegalStateException.)))

      (and caret mark)
      (do
        (if (not (or first-in-word-range last-in-word-range)) (throw (IllegalStateException.)))
        (let [from (min caret mark)
              to (if (= caret mark) (inc caret) (max caret mark))]
          (kill-glyph-range-in-word word from to w interop)))

      :else
      (let [sel-edge (if caret caret mark)]
        (cond
          first-in-word-range (kill-glyph-range-in-word word sel-edge old-glyph-count w interop)
          last-in-word-range (kill-glyph-range-in-word word 0 sel-edge w interop)
          :else (throw (IllegalStateException.)))))))

(defn- kill-glyphs [model w interop]
  (let [caret-line-index (:caret-line model)
        mark-line-index (:mark-line model)
        same-line (= caret-line-index mark-line-index)
        caret-line-first (<= caret-line-index mark-line-index)
        from-line-index (if caret-line-first caret-line-index mark-line-index)
        to-line-index (if caret-line-first mark-line-index caret-line-index)
        lines (:lines model)
        from-line (nth lines from-line-index)
        to-line (nth lines to-line-index)
        from-word-index (cond
                          same-line (min (:caret-word from-line) (:mark-word from-line))
                          caret-line-first (:caret-word from-line)
                          :else (:mark-word from-line))
        to-word-index (cond
                        same-line (max (:mark-word to-line) (:caret-word to-line))
                        caret-line-first (:mark-word to-line)
                        :else (:caret-word to-line))
        lines (:lines model)]
    (loop [m model
           li from-line-index
           wi from-word-index
           first-w-of-all true]
      (if (<= li to-line-index)
        (let [words (:words (nth lines li))
              last-line (= li to-line-index)
              last-wi-in-line (if last-line to-word-index (dec (count words)))
              now-is-last-wi (= wi last-wi-in-line)
              now-is-last-w-of-all (and last-line now-is-last-wi)]
          (recur
            (assoc-in m [:lines li :words wi] (kill-glyphs-in-word (nth words wi) first-w-of-all now-is-last-w-of-all w interop))
            (if now-is-last-wi (inc li) li)
            (if now-is-last-wi 0 (inc wi))
            false))
        (let [remainder-words (mapcat :words (take-last (- (count lines) from-line-index) (:lines m)))]
          (rewrap-partially model w (truncate-words remainder-words)))))))

(defn do-delete [model w interop]
  (let [line-index (:caret-line model)
        mark-line-index (:mark-line model)
        lines (:lines model)
        line (nth lines line-index)
        mark-line (nth lines mark-line-index)
        word-index (:caret-word line)
        mark-word-index (:mark-word mark-line)
        caret-line-words (:words line)
        mark-line-words (:words mark-line)
        word (nth caret-line-words word-index)
        caret-pos (:caret-pos word)
        mark-word (nth mark-line-words mark-word-index)
        mark-pos (:mark-pos mark-word)
        caret-word-glyphs (:glyphs word)
        mark-word-glyphs (:glyphs mark-word)
        last-line-index (dec (count lines))
        edge-line-by-caret (= line-index last-line-index)
        edge-line-by-mark (= mark-line-index last-line-index)
        edge-word-by-caret (= word-index (dec (count caret-line-words)))
        edge-word-by-mark (= mark-word-index (dec (count mark-line-words)))
        edge-glyph-by-caret (= caret-pos (count caret-word-glyphs))
        edge-glyph-by-mark (= mark-pos (count mark-word-glyphs))]
    (cond
      (and
        edge-line-by-caret edge-word-by-caret edge-glyph-by-caret
        edge-line-by-mark  edge-word-by-mark edge-glyph-by-mark)
      model

      (or
        (and edge-glyph-by-caret (not edge-word-by-caret))
        (and edge-glyph-by-mark (not edge-word-by-mark)))
      (throw (IllegalStateException. "Cursor may be in edge glyph position of the word only at the end of the line"))

      (or edge-glyph-by-caret edge-glyph-by-mark)
      (let [caret-first (if (= line-index mark-line-index)
                          (if (= word-index mark-word-index) (< caret-pos mark-pos) (< word-index mark-word-index))
                          (< line-index mark-line-index))]
        (-> (move-caret-mark model (cond (= (:mark-pos word) caret-pos) :caret-&-mark caret-first :caret :else :mark) :forward nil nil) (kill-glyphs w interop)))

      :else
      (kill-glyphs model w interop))))

(defn do-backspace [model w interop]
  (let [line-index (:caret-line model)
        lines (:lines model)
        line (nth lines line-index)
        word-index (:caret-word line)
        words (:words line)
        word (nth words word-index)
        pos (:caret-pos word)
        edge-line (= line-index 0)
        edge-word (= word-index 0)
        edge-glyph (= pos 0)]
    (if (and edge-line edge-word edge-glyph)
      model
      (->
        (move-caret-mark model :caret-&-mark :backward nil nil)
        (do-delete w interop)))))


(fg/defevolverfn :model
  (cond

    ;; TODO not good because might need to rewrap aacording to :clip-size
    ;(false? (get-property [:this] :editable))
    ;old-model

    (mouse/is-mouse-event? component)
    old-model

    (or (keyboard/key-event? component) (clipboard/clipboard-paste? component))
    (if-let [supplied-text (if (clipboard/clipboard-paste? component)
                             (clipboard/get-plain-text component)
                             ((get-property [:this] :text-supplier) component))]
      (let [_ (println "Supplied text = " supplied-text)
            glyphs (mapv char-glyph supplied-text)
            w (- (m/x (get-property component [:this] :clip-size)) (awt/strh component))
            r (glyph-> old-model (first glyphs) w (get-property component [:this] :interop))
            _ (println "Model: " r)]
        r)
      old-model)

    :else old-model))

(def empty-model (Model.
                   [(make-line
                      [(make-word 0 0 0 0 [] 0 0)] ;(make-words [] 0 0 nil)
                      0 0 0)]
                   0 0))

(fg/defwidget "textfield"
  {:text-supplier textcommons/textfield-dflt-text-suplier
   :caret-visible true;false
   :model empty-model
   ;:text ""
   ;:->clipboard nil

   :focusable true
   :cursor nil
   :skin-key [:textfield2]
   :editable true
   :background :prime-4
   :foreground :prime-1
   :no-mouse-press-capturing true
   :paint-border true
   :margin 0.0625
   :evolvers {:model model-evolver
              ;:text text-evolver

              ;:caret-visible caret-visible-evolver
              ;:->clipboard ->clipboard-evolver
              ;:cursor cursor-evolver
              ;:clip-size auto-size-evolver
              :background (fg/accessorfn (if (get-property component [:this] :editable) :prime-4 :prime-1))
              :foreground (fg/accessorfn (if (get-property component [:this] :editable) :prime-1 :prime-4))}}
  flatgui.widgets.component/component)