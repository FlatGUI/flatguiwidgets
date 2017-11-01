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

;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;

(defmulti glyph-size (fn [g _interop] (:type g)))

(def empty-glyph-size {:w 0 :h 0})

(defn- text-size [interop text font]
  {:w (.getStringWidth interop text font)
   :h (.getFontHeight interop font)})

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

;;;;;;;;;;;;;;;;;;;;;

(defn word->str [word] (apply str (map :data (:glyphs word))))

(defrecord Model [lines caret-line mark-line])

(defrecord Line [words caret-word mark-word h])

(defrecord Word [glyphs caret-pos mark-pos w-content w-total h]
  Object
  (toString [word] (word->str word)))

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
           (if (pos? (count w-g)) (rf result (make-word caret-pos w-content w-total h w-g total-g-count source-g-count)))))
        ([result g]
         (let [s @state
               w-content (:w-content s)
               w-total (:w-total s)
               h (:h s)
               w-g (:w-g s)
               total-g-count (:total-g-count s)
               whitespace (whitespace? g)
               init-whitespace (:init-whitespace s)
               g-size (glyph-size g interop)
               g-w (:w g-size)
               effective-g-w (if (or (not whitespace) (:init-whitespace s)) g-w 0)
               g-h (:h g-size)
               w&g-content (+ w-content effective-g-w)
               w&g-total (+ w-total g-w)
               w&h-h (max h g-h)]
           (if (> w&g-content w)
             (do
               (vreset! state {:w-content effective-g-w :w-total g-w :h g-h :w-g (let [a (ArrayList.)] (do (.add a g) a)) :total-g-count (inc total-g-count) :init-whitespace whitespace})
               (if (pos? (count w-g)) (rf result (make-word caret-pos w-content w-total h w-g total-g-count source-g-count))))
             (do
               (vreset! state {:w-content w&g-content :w-total w&g-total :h w&h-h :w-g (do (.add w-g g) w-g) :total-g-count (inc total-g-count) :init-whitespace (if init-whitespace whitespace false)})
               result)
             )))))))

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
      (let [;_(println "------------------------------------whitespace after g")
            ;_ (println "part-before-caret-pos=" part-before-caret-pos)
            ;_ (println "before" (make-words (vec (conj part-before-caret-pos g)) (inc caret-pos) w interop))
            ;_ (println "after" (make-words (vec part-after-caret-pos) nil  w interop))
            ]
        (concat
          (make-words (vec (conj part-before-caret-pos g)) (inc caret-pos) w interop)
          (make-words (vec part-after-caret-pos) nil  w interop)))

      ;(and (whitespace? g) (not g-goes-after-whitespace))
      (whitespace? g)
      ;(list
      ;  (Word. (vec (concat part-before-caret-pos (list g) part-after-caret-pos)) (inc caret-pos)))
      (make-words (vec (concat part-before-caret-pos (list g) part-after-caret-pos)) (inc caret-pos) w interop)

      (and (linebreak? g))
      (concat
        (make-words (vec part-before-caret-pos) nil w interop)
        (make-words (vec part-after-caret-pos) 0 w interop))
      
      )))

(defn make-glyph-line-reducer [w interop]
  (fn [words g]
    (let [                                                  ;_ (println "-----------words=" words " g=" g)
          ;_ (println "------------>" (glyph-> (last words) g w interop))
          ]

      (concat (butlast words) (glyph-> (last words) g w interop)))
    ))

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
                 final-result (rf result (Line. @line-state caret-word caret-word @line-h-state))
                 caret-line (if @model-caret-met-state @model-caret-index-state)
                 _ (println "FR: " (lines->strings result) "@line-h-state" @line-h-state)
                 ]
             (Model. final-result caret-line caret-line)))                             ;TODO add final line here, same as in words
          ([result word]
           (let [                                           ;_ (println "wrap-lines [result word]------" (lines->strings result) word)
                 line @line-state
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
                   (rf result (Line. line line-caret-index line-caret-index line-h)))
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
;; line 2 - caret line; caret-line-and-following-words will contain words of all lines starting from this one
;; line 3 ...
;;
;; Example 2:
;; line 0 - caret line; line-num-to-start-rewrap will be = -1;
;; line 1 ...
(defn rewrap-partially [model w caret-line-and-following-words]
  (let [line-num-to-start-rewrap (dec (:caret-line model))]
    (if (>= line-num-to-start-rewrap 0)
      (let [prior-lines (take line-num-to-start-rewrap (:lines model))
            words-to-wrap (concat
                            (:words (nth (:lines model) line-num-to-start-rewrap))
                            caret-line-and-following-words)
            remainder-model (wrap-lines words-to-wrap w)
            result-caret-line (+ (count prior-lines) (:caret-line remainder-model))]
        (Model. (vec (concat prior-lines (:lines remainder-model))) result-caret-line result-caret-line))
      (wrap-lines caret-line-and-following-words w))))

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

(defn- kill-glyph [model w interop]
  (let [line-index (:caret-line model)
        lines (:lines model)
        line (nth lines line-index)
        word-index (:caret-word line)
        words (:words line)
        word (nth words word-index)
        pos (:caret-pos word)
        old-glyphs (:glyphs word)
        glyphs (mapv (fn [i] (nth old-glyphs i)) (filter (fn [i] (not= i pos)) (range (count old-glyphs))))
        replacement-words (make-words glyphs pos w interop)
        glyph-killed (assoc-in model [:lines line-index :words word-index] (first replacement-words))
        remainder-words (mapcat :words (take-last (- (count lines) line-index) (:lines glyph-killed)))]
    (assert (= (count replacement-words) 1))
    (rewrap-partially model w (truncate-words remainder-words))))

(defn do-delete-no-sel [model w interop]
  (let [line-index (:caret-line model)
        lines (:lines model)
        line (nth lines line-index)
        word-index (:caret-word line)
        words (:words line)
        word (nth words word-index)
        pos (:caret-pos word)
        old-glyphs (:glyphs word)
        edge-line (= line-index (dec (count lines)))
        edge-word (= word-index (dec (count words)))
        edge-glyph (= pos (count old-glyphs))]
    (cond
      (and edge-line edge-word edge-glyph)
      model

      (and edge-glyph (not edge-word))
      (throw (IllegalStateException. "Cursor may be in edge glyph position of the word only at the end of the line"))

      edge-glyph
      (-> (move-caret-mark model :caret-&-mark :forward nil nil) (kill-glyph w interop))

      :else
      (kill-glyph model w interop)

      ;:else
      ;(throw (IllegalStateException. (str "Inconsistent calculations: edge-line=" edge-line " edge-word=" edge-word " edge-glyph=" edge-glyph)))
      )))

(defn do-backspace-no-sel [model w interop]
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
        (do-delete-no-sel w interop)))))

(defn cut-selection [model w interop] )