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
            [flatgui.util.vectorutil :as vu]
            [flatgui.comlogic :as fgc])
  (:import [java.awt.event KeyEvent]
           (flatgui.core.engine.ui FGTransferable)
           (java.util ArrayList Collections)))


(def default-style
  {:font nil
   :foreground :prime-6
   :background :prime-4})

(defn default-line-h [interop] (.getFontHeight interop (:font default-style)))

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

(defmethod glyph-size :char [g interop]
  (let [font (:font (:style g))
        text (str (:data g))]
    (text-size interop text font)))

(defmethod glyph-size :whitespace [g interop]
  (let [font (:font (:style g))]
    (text-size interop " " font)))

(defmethod glyph-size :linebreak [g interop] {:w 0.0 :h (.getFontHeight interop (:font (:style g)))})

(defmethod glyph-size :test [g _interop] {:w (:w (:style g)) :h (:h (:style g))})

(defmethod glyph-size :image [g _interop] (:size (:style g)))

(defmethod glyph-size :video [g _interop] (:size (:style g)))

(defn- glyph-data-mapper [g]
  (if (= :whitespace (:type g))
    " "
    (:data g)))

(defn word->str [word] (apply str (map glyph-data-mapper (:glyphs word))))


(defrecord Model [lines caret-line mark-line total-h])

(defrecord Line [words caret-word mark-word y h primitives])

(defrecord Word [glyphs caret-pos mark-pos w-content w-total h]
  Object
  (toString [word] (word->str word)))

(defrecord Primitive [type data style x w caret-x s-start s-end])

(defn- glyph-data-mapper-ext [g]
  (cond
    (= :whitespace (:type g)) " "
    (= :linebreak (:type g)) \u23CE
    :else (:data g)))

(def caret-str \u2502)

(def mark-str \u205E)

(def caret-mark-str \u2551)

(defn word->str-ext [word]
  (let [raw-str (apply str (map glyph-data-mapper-ext (:glyphs word)))
        caret-pos (:caret-pos word)
        mark-pos (:mark-pos word)]
    (str
      "`"
      (cond
        (and caret-pos (nil? mark-pos)) (str (subs raw-str 0 caret-pos) caret-str (subs raw-str caret-pos))
        (and (nil? caret-pos) mark-pos) (str (subs raw-str 0 mark-pos) mark-str (subs raw-str mark-pos))
        (and caret-pos mark-pos (= caret-pos mark-pos)) (str (subs raw-str 0 caret-pos) caret-mark-str (subs raw-str caret-pos))
        (and caret-pos mark-pos) (let [caret-first (< caret-pos mark-pos)]
                                   (if caret-first
                                     (str (subs raw-str 0 caret-pos) caret-str (subs raw-str caret-pos mark-pos) mark-str (subs raw-str mark-pos))
                                     (str (subs raw-str 0 mark-pos) mark-str (subs raw-str mark-pos caret-pos) caret-str (subs raw-str caret-pos))))

        :else raw-str)
      "`")))

(defn line->str [line] (str (apply str (map word->str-ext (:words line))) "\n"))

(defn model->str [model] (apply str (map line->str (:lines model))))

(defn glyph-type->primitive-type [g]
  (let [type (:type g)]
    (cond
      (= type :char) :string
      (= type :whitespace) :string
      (= type :linebreak) nil
      :else type)))

(defn glyps->primitive-data [glyphs primitive-type]
  (if (= primitive-type :string)
    (apply str (map glyph-data-mapper glyphs))
    glyphs))

(defn create-render-line-primitives-transducer [selection-continued-line]
  (fn [rf]
    (let [glyphs-state (volatile! [])
          type-state (volatile! nil)
          style-state (volatile! nil)
          w-total-state (volatile! 0)
          x-state (volatile! 0.0)
          caret-state (volatile! nil)
          sel-marks (volatile! [])]
      (fn
        ([] (rf))
        ([result]
         (let [glyphs @glyphs-state]                         ; TODO if last glyph is linebreak then this will go to else branch, right?
           (if (not (vu/emptyv? glyphs))
             (let [style @style-state
                   type @type-state
                   x @x-state
                   data (glyps->primitive-data glyphs type)
                   s-marks @sel-marks
                   has-both-s-marks (= 2 (count s-marks))
                   s-start (cond
                             has-both-s-marks (vu/firstv s-marks)
                             selection-continued-line 0.0
                             :else (vu/firstv s-marks))
                   s-end (cond
                           has-both-s-marks (second s-marks)
                           selection-continued-line (vu/firstv s-marks)
                           :else (if s-start @w-total-state))
                   has-sel (not= s-start s-end)
                   p (Primitive. type data style x @w-total-state @caret-state (if has-sel s-start) (if has-sel s-end))]
               (rf result p))
             result)))
        ([result g]
         (if-let [g-type (glyph-type->primitive-type g)]
           (let [glyphs @glyphs-state
                 style @style-state
                 type @type-state
                 x @x-state
                 g-w (:w (if-let [size (:size g)] size (throw (IllegalStateException. (str "Glyph must be sized at this point. g=" g)))))
                 g-style (:style g)
                 empty-glyphs (vu/emptyv? glyphs)
                 caret (:caret g)
                 mark (:mark g)
                 x-before (+ x @w-total-state)
                 x-after (+ x @w-total-state g-w)
                 caret-x (if caret (if (= :before caret) x-before x-after))
                 mark-x (if mark (if (= :before mark) x-before x-after))]
             (if (or empty-glyphs (= style g-style) (= type g-type))
               (do
                 (vswap! glyphs-state conj g)
                 (vswap! w-total-state + g-w)
                 (if empty-glyphs
                   (do
                     (vreset! type-state g-type)
                     (vreset! style-state g-style)))
                 (if caret-x (vreset! caret-state caret-x))
                 (if (or caret mark)
                   (if (and caret mark)
                     (vreset! sel-marks [(min caret-x mark-x) (max caret-x mark-x)])
                     (vswap! sel-marks conj (if caret caret-x mark-x))))
                 result)
               (let [data (glyps->primitive-data glyphs type)
                     s-marks @sel-marks
                     has-both-s-marks (= 2 (count s-marks))
                     s-start (cond
                               has-both-s-marks (vu/firstv s-marks)
                               selection-continued-line 0.0
                               :else (vu/firstv s-marks))
                     s-end (cond
                             has-both-s-marks (second s-marks)
                             selection-continued-line (vu/firstv s-marks)
                             :else (if s-start @w-total-state))
                     has-sel (not= s-start s-end)
                     p (Primitive. type data style x @w-total-state @caret-state (if has-sel s-start) (if has-sel s-end))]
                 (vreset! glyphs-state [g])
                 (vreset! type-state g-type)
                 (vreset! style-state g-style)
                 (vswap! x-state + @w-total-state)
                 (vreset! w-total-state g-w)
                 (vreset! caret-state nil)
                 (rf result p))))
           ;; This handles case when caret or mark is in a nil-data glyph, like linebreak
           (let [caret (:caret g)
                 ;; assuming g-w is 0 for a nil-data glyph
                 caret-or-mark-x (+ @x-state @w-total-state)]
             (do
               (if caret (vreset! caret-state caret-or-mark-x))
               (if (or caret (:mark g)) (vswap! sel-marks conj caret-or-mark-x))
               result))))))))

(defn- insert-g-marker [glyphs pos mark-type]
  (if pos
    (let [inside (or (= pos 0) (< pos (count glyphs)))
          g-index (if inside pos (dec pos))
          location (if inside :before :after)]
      (try
        (assoc-in glyphs [g-index mark-type] location)
        (catch Exception ex
          (do
            (println "Debug: glyphs size=" (count glyphs) "pos=" pos "mark-type=" mark-type)
            (.printStackTrace ex)
            glyphs)))
      )
    glyphs))

(defn- glyph-primitive-extractor [word]
  (let [caret-pos (:caret-pos word)
        mark-pos (:mark-pos word)
        glyphs (:glyphs word)]
    (->
      (insert-g-marker glyphs caret-pos :caret)
      (insert-g-marker mark-pos :mark))))

(defn words->primitives [words selection-continued-line]
  (let [glyphs (mapcat glyph-primitive-extractor words)]
    (transduce (create-render-line-primitives-transducer selection-continued-line) conj glyphs)))

(defn make-line [words caret-word mark-word selection-continued-line y h]
  (Line. words caret-word mark-word y h (words->primitives words selection-continued-line)))

(defn make-word [caret-pos mark-pos w-content w-total h w-g total-g-count source-g-count]
  (let [w-g-count (.size w-g)
        cp-up-bound-fn (if (< total-g-count source-g-count) < <=)
        total->word-pos (fn [pos] (if (and pos (>= pos (- total-g-count w-g-count)) (cp-up-bound-fn pos total-g-count)) (- pos (- total-g-count (.size w-g)))))
        word-caret-pos (total->word-pos caret-pos)
        word-mark-pos (total->word-pos mark-pos)]
    (Word.
      (vec (.toArray w-g))
      word-caret-pos
      word-mark-pos
      w-content
      w-total
      h)))

(defn create-make-words-transducer [caret-pos mark-pos w interop source-g-count]
  (fn [rf]
    (let [state (volatile! {:w-content 0.0 :w-total 0.0 :h 0 :w-g (ArrayList.) :total-g-count 0})]
      (fn
        ([] (rf))
        ([result]
         (let [s @state
               w-content (:w-content s)
               w-total (:w-total s)
               h (:h s)
               w-g (:w-g s)
               total-g-count (:total-g-count s)]
           (if (pos? (count w-g)) (rf result (make-word caret-pos mark-pos w-content w-total h w-g total-g-count source-g-count)) result)))
        ([result g]
         (let [s @state
               w-content (:w-content s)
               w-total (:w-total s)
               h (:h s)
               w-g (:w-g s)
               count-w-g (count w-g)
               total-g-count (:total-g-count s)
               whitespace (whitespace? g)
               cached-g-size (:size g) ; Size might be already cached in glyph, e.g. if this called from kill-glyphs. Also will need size further.
               g-size (if cached-g-size cached-g-size (glyph-size g interop))
               sized-g (if cached-g-size g (assoc g :size g-size))
               g-w (:w g-size)
               effective-g-w (if (not whitespace) g-w 0.0)
               g-h (:h g-size)
               w&g-content (+ w-content effective-g-w)
               w&g-total (+ w-total g-w)
               w&h-h (max h g-h)]
           (if (or
                 (> w&g-content w)
                 (and
                   (pos? count-w-g)
                   (or
                     ;; after linebreak new word always starts
                     (linebreak? (nth w-g (dec count-w-g)))
                     ;; if this is not a delimiter going after delimiter - this is new word starting
                     ;; however linebreak going after whitespace stays at the end of a word
                     (and (not whitespace) (not (linebreak? g)) (delimiter? (nth w-g (dec count-w-g)))))))
             (do
               (vreset! state {:w-content effective-g-w :w-total g-w :h g-h :w-g (let [a (ArrayList.)] (do (.add a sized-g) a)) :total-g-count (inc total-g-count)})
               (if (pos? count-w-g) (rf result (make-word caret-pos mark-pos w-content w-total h w-g total-g-count source-g-count))))     ;MWds
             (do
               (vreset! state {:w-content w&g-content :w-total w&g-total :h w&h-h :w-g (do (.add w-g sized-g) w-g) :total-g-count (inc total-g-count)})
               result))))))))

(defn make-words
  ([glyphs caret-pos mark-pos w interop] (transduce (create-make-words-transducer caret-pos mark-pos w interop (count glyphs)) conj glyphs))
  ([glyphs caret-&-mark-pos w interop] (make-words glyphs caret-&-mark-pos caret-&-mark-pos w interop)))

(def empty-word-with-caret-&-mark (make-word 0 0 0.0 0.0 0 [] 0 0))

(def empty-model (Model.
                   [(make-line
                      [empty-word-with-caret-&-mark]
                      0 0
                      false
                      0.0
                      0)]
                   0 0 0))


(defmulti glyph-> (fn [entity _g _w _interop] (class entity)))

(defmethod glyph-> Word [word g w interop]
  (let [caret-pos (:caret-pos word)
        _ (if (nil? caret-pos) (throw (IllegalStateException. "Can insert glyph into a word only when word has caret")))
        mark-pos (:mark-pos word)
        _ (if (not= caret-pos mark-pos) (throw (IllegalStateException. "Can insert glyph into a word only when selection is reduced")))
        glyphs (:glyphs word)
        g-is-delimiter (delimiter? g)
        g-goes-after-whitespace (and (> caret-pos 0) (whitespace? (nth glyphs (dec caret-pos))))
        part-before-caret-pos (vu/takev caret-pos glyphs)
        part-after-caret-pos (vu/dropv caret-pos glyphs)]
    (cond

      (and (not g-is-delimiter) (not g-goes-after-whitespace))
      (make-words (into (conj part-before-caret-pos g) part-after-caret-pos) (inc caret-pos) w interop)

      (and (not g-is-delimiter) g-goes-after-whitespace)
      (concat
        (make-words part-before-caret-pos nil w interop)
        (make-words (into [g] part-after-caret-pos) 1 w interop))

      (and (whitespace? g) (not g-goes-after-whitespace) (some #(not (whitespace? %)) part-before-caret-pos))
      (concat
        (make-words (conj part-before-caret-pos g) (inc caret-pos) w interop)
        (make-words part-after-caret-pos nil w interop))
      ;(if (< caret-pos (count part-before-caret-pos))    ;TODO probably can do it as early as here
      ;  (concat
      ;    (make-words (conj part-before-caret-pos g) (inc caret-pos) w interop)
      ;    (make-words part-after-caret-pos nil w interop))
      ;  (concat
      ;    (make-words (conj part-before-caret-pos g) nil w interop)
      ;    (make-words part-after-caret-pos 0 w interop)))

      (whitespace? g)
      (make-words (into (conj part-before-caret-pos g) part-after-caret-pos) (inc caret-pos) w interop)

      (linebreak? g)
      (concat
        (make-words (conj part-before-caret-pos g) nil w interop)
        (if (vu/emptyv? part-after-caret-pos)
          [empty-word-with-caret-&-mark]
          (make-words part-after-caret-pos 0 w interop))))))

;; TODO not needed since there is make-words which does the same and supports cm-pos
(defn make-glyph-line-reducer [w interop]
  (fn [words g]
    (concat (butlast words) (glyph-> (last words) g w interop))))

;; TODO not needed since there is make-words which does the same and supports cm-pos
(defn glyphs->words [glyphs w interop]
  (reduce
    (make-glyph-line-reducer w interop)
    [(make-word 0 0 0 0 0 [] 0 0)]
    glyphs))

(defn lines->strings [lines]
  (mapv (fn [line] (mapv (fn [word] (word->str word)) line)) lines))

(defn lines->total-word-widths [lines]
  (mapv (fn [line] (mapv (fn [word] (:w-total word)) line)) lines))

(defn lines->line-heights [lines] (mapv :h lines))

(defn collapse-words [last-word word]
  (let [result-caret-pos (cond
                           (:caret-pos last-word) (:caret-pos last-word)
                           (:caret-pos word) (+ (:caret-pos word) (count (:glyphs last-word)))
                           :else nil)
        result-mark-pos (cond
                          (:mark-pos last-word) (:mark-pos last-word)
                          (:mark-pos word) (+ (:mark-pos word) (count (:glyphs last-word)))
                          :else nil)]
    (Word.
      (into (:glyphs last-word) (:glyphs word))
      result-caret-pos
      result-mark-pos
      (+ (:w-content last-word) (if (every? whitespace? (:glyphs word)) 0 (:w-content word)))
      (+ (:w-total last-word) (:w-total word))
      (max (:h last-word) (:h word)))))

(defn wrap-lines
  ([w interop]
    (fn [rf]
      (let [line-state (volatile! [])
            line-w-state (volatile! 0)
            line-h-state (volatile! 0)
            model-h-state (volatile! 0.0)
            line-caret-index-state (volatile! 0)
            line-caret-met-state (volatile! false)
            model-caret-index-state (volatile! 0)
            model-caret-met-state (volatile! false)
            model-sel-met-state (volatile! false)
            model-selection-continues-to-next-line-state (volatile! false)]
        (fn
          ([] (rf))
          ([result]
           (let [caret-word (if @line-caret-met-state @line-caret-index-state)
                 line-h @line-h-state
                 line-y @model-h-state
                 final-result (rf result (make-line @line-state caret-word caret-word @model-selection-continues-to-next-line-state line-y line-h))
                 caret-line (if @model-caret-met-state @model-caret-index-state)]
             (Model. final-result caret-line caret-line (+ line-y line-h))))
          ([result word]
           (let [line @line-state
                 line-w @line-w-state
                 line-h @line-h-state
                 w-content (:w-content word)
                 w-total (:w-total word)
                 word-h (if (= empty-word-with-caret-&-mark word) (default-line-h interop) (:h word))
                 caret-pos (:caret-pos word)
                 mark-pos (:mark-pos word)
                 end-line (or (> (+ line-w w-content) w) (linebreak? (peek (:glyphs (peek line)))))
                 process-caret (fn []
                                 (if caret-pos
                                   (do
                                     (vreset! model-caret-met-state true)
                                     (vreset! line-caret-met-state true))
                                   (if (not @line-caret-met-state) (vswap! line-caret-index-state inc))))
                 process-sel (fn [] (if (and (or caret-pos mark-pos) (not= caret-pos mark-pos))
                                      (vreset! model-sel-met-state true)))]
             (do
               (if end-line
                 (let [line-caret-index (if @line-caret-met-state @line-caret-index-state)
                       line-y @model-h-state]
                   (vreset! line-state [word])
                   (vreset! line-w-state w-total)
                   (vswap! model-h-state + line-h)
                   (vreset! line-h-state word-h)
                   (vreset! line-caret-index-state 0)
                   (vreset! line-caret-met-state false)
                   (if (not @model-caret-met-state) (vswap! model-caret-index-state inc))
                   (let [sel-cont @model-selection-continues-to-next-line-state
                         process-result (rf result (make-line line line-caret-index line-caret-index sel-cont line-y line-h))]
                     (do
                       (if @model-sel-met-state (vreset! model-selection-continues-to-next-line-state true))
                       (process-caret)
                       (process-sel)
                       process-result)))
                 (do
                   (if (and (not (vu/emptyv? line)) (delimiter? (vu/firstv (:glyphs word))))
                     (do
                       (vreset! line-state (conj (pop line) (collapse-words (peek line) word)))
                       (if (not @line-caret-met-state) (vswap! line-caret-index-state dec)))
                     (vswap! line-state conj word))
                   (vswap! line-w-state + w-total)
                   (vswap! line-h-state max word-h)
                   (process-caret)
                   (process-sel)
                   result)))))))))
  ([words w interop] (transduce (wrap-lines w interop) conj words)))

(defn assert-word [model line-index word-index]
  (let [word (get-in model [:lines line-index :words word-index])]
    (do
      (assert word (str "No word line-index=" line-index " word-index=" word-index))
      model)))

(defn rewrap-partially
   ([model w caret-line-and-following-words interop because-glyphs-killed line-num-to-start-rewrap]
     (let [caret-line-index (:caret-line model)
           caret-line (get-in model [:lines caret-line-index])
           caret-word-index (:caret-word caret-line)
           caret-word (get-in caret-line [:words caret-word-index])
           caret-pos (:caret-pos caret-word)
           mark-line-index (:mark-line model)
           mark-line (get-in model [:lines mark-line-index])
           mark-word-index (:mark-word mark-line)
           mark-word (get-in mark-line [:words mark-word-index])
           mark-pos (:mark-pos mark-word)]
       (if (and (= caret-line-index mark-line-index) (= caret-word-index mark-word-index) (= caret-pos mark-pos))
         (cond

           (>= line-num-to-start-rewrap 0)
           (let [prior-lines (vu/takev line-num-to-start-rewrap (:lines model))
                 prior-h (apply + (map :h prior-lines))
                 prior-words-in-line (:words (nth (:lines model) line-num-to-start-rewrap))
                 words-to-wrap (concat
                                 prior-words-in-line
                                 caret-line-and-following-words)
                 remainder-model (wrap-lines words-to-wrap w interop)
                 result-caret-line (if because-glyphs-killed (:caret-line model) (+ (count prior-lines) (:caret-line remainder-model)))
                 result-mark-line (if because-glyphs-killed (:mark-line model) (+ (count prior-lines) (:mark-line remainder-model))) ;TODO result-mark-line is the same. Rewrite this method, do not compure caret line/word/pos
                 new-model (Model.
                             (into prior-lines (mapv (fn [l] (update l :y + prior-h)) (:lines remainder-model)))
                             result-caret-line result-mark-line (+ prior-h (:total-h remainder-model)))]
             (if (and because-glyphs-killed (<= caret-line-index (dec (count (:lines new-model)))))
               (let [count-following (count caret-line-and-following-words)
                     caret-word-killed (>= caret-word-index count-following)
                     new-caret-line (get-in new-model [:lines caret-line-index])
                     new-caret-line-words (:words new-caret-line)
                     new-caret-line-word-count (count new-caret-line-words)
                     word-count-reduced (>= caret-word-index new-caret-line-word-count)]
                 new-model)
               new-model))

           (pos? (count caret-line-and-following-words))
           (let [words (if because-glyphs-killed
                         (let [count-following (count caret-line-and-following-words)
                               word-killed (and (pos? count-following) (>= caret-word-index count-following))
                               new-caret-word-index (if word-killed (dec count-following) caret-word-index)
                               _ (assert (< new-caret-word-index (count caret-line-and-following-words)) (str "No word in vec, index=" new-caret-word-index))]
                           caret-line-and-following-words)
                         caret-line-and-following-words)]
             (wrap-lines words w interop))

           :else empty-model)
         (throw (IllegalStateException. "model selection is not reduced")))))
  ([model w caret-line-and-following-words interop because-glyphs-killed]
    ;; Example 1:
    ;; line 0 - will be contained in prior-lines
    ;; line 1 - so line-num-to-start-rewrap will be = 1
    ;; line 2 - caret/mark line; caret-line-and-following-words will contain words of all lines starting from this one
    ;; line 3 ...
    ;;
    ;; Example 2:
    ;; line 0 - caret/mark line; line-num-to-start-rewrap will be = -1;
    ;; line 1 ...
   (rewrap-partially model w caret-line-and-following-words interop because-glyphs-killed (dec (:caret-line model)))))

(defn has-selection? [model]
  (if (not= (:caret-line model) (:mark-line model))
    true
    (let [line (nth (:lines model) (:caret-line model))]
      (if (not= (:caret-word line) (:mark-word line))
        true
        (let [word (nth (:words line) (:caret-word line))]
          (not= (:caret-pos word) (:mark-pos word)))))))

(defn- inside? [a b i]
  (if (> b a)
    (and (> i a) (< i b))
    (and (> i b) (< i a))))

(defn rebuild-primitives
  ([model old-caret-line-index old-mark-line-index new-caret-line-index new-mark-line-index caret-pos-changed mark-pos-changed]
   (let [from (min old-caret-line-index old-mark-line-index new-caret-line-index new-mark-line-index)
         to (max old-caret-line-index old-mark-line-index new-caret-line-index new-mark-line-index)]
     (loop [i from
            sel-met false
            m model]
       (if (<= i to)
         (let [caret-met (= i new-caret-line-index)
               mark-met (= i new-mark-line-index)
               far-edge-old (or (= i old-mark-line-index) (= i old-caret-line-index))
               any-edge-new (or (= i new-mark-line-index) (= i new-caret-line-index))
               inside-old (inside? old-mark-line-index old-caret-line-index i)
               inside-new (inside? new-mark-line-index new-caret-line-index i)]
           (recur
             (inc i)
             (if (not sel-met) (or caret-met mark-met) sel-met)
             (cond

               any-edge-new
               (assoc-in m [:lines i :primitives] (words->primitives (get-in model [:lines i :words]) sel-met))

               (and (or far-edge-old inside-old) (not inside-new))
               (update-in m [:lines i :primitives] (fn [primitives] (mapv (fn [p] (assoc p :caret-x nil :s-start nil :s-end nil)) primitives)))

               (and inside-new (not inside-old))
               (update-in m [:lines i :primitives] (fn [primitives] (mapv (fn [p] (assoc p :caret-x nil :s-start 0.0 :s-end (:w p))) primitives)))

               :else m)))
         m))))
  ([model old-caret-line-index old-mark-line-index caret-pos-changed mark-pos-changed]
   (rebuild-primitives model old-caret-line-index old-mark-line-index (:caret-line model) (:mark-line model) caret-pos-changed mark-pos-changed)))

(defn- pos-bsearch [x-marks x]
  (let [i (Collections/binarySearch x-marks x)]
    (if (>= i 0)
      i
      (- (+ i 1)))))

(defn x->pos-in-word [word x]
  (let [glyphs (:glyphs word)
        g-count (if (linebreak? (peek glyphs)) (dec (count glyphs)) (count glyphs))]
    (if (pos? g-count)
      (let [x-marks (loop [xm []
                           w 0.0
                           i 0]
                      (if (< i g-count)
                        (let [g (nth glyphs i)
                              g-size (:size g)   ; Rely on that size is cached already
                              g-w (:w g-size)
                              w+half (+ w (/ g-w 2))
                              w+full (+ w g-w)]
                          (recur
                            (-> (conj xm w+half) (conj w+full))
                            w+full
                            (inc i)))
                        xm))]
        (cond
          (<= x (vu/firstv x-marks)) 0
          (>= x (peek x-marks)) g-count ;; cursor pos may be = g-count -- after last glyph
          :else (Math/round (double (/ (pos-bsearch x-marks x) 2)))))
      0)))

(defn x->pos-in-line [line x]
  (let [words (:words line)
        w-count (count words)]
    (if (pos? w-count)
      (let [x-marks (loop [xm []
                           w 0.0
                           i 0]
                      (if (< i w-count)
                        (let [word (nth words i)
                              glyphs (:glyphs word)
                              word-w  (:w-total word)  ;(apply + (map (fn [g] (:w (:size g))) glyphs))
                              w+full (+ w word-w)
                              last-glyph (peek glyphs)
                              ;; Let's assume next word zone starts in the middle of the last inter-word space char
                              space-allow (if (whitespace? last-glyph) (/ (:w (:size last-glyph)) 2) 0)]
                          (recur
                            (conj xm (- w+full space-allow))
                            w+full
                            (inc i)))
                        xm))]
        (cond
          (<= x (vu/firstv x-marks)) 0
          (>= x (peek x-marks)) (dec w-count) ;; word index may not be >= w-count
          :else (let [p (pos-bsearch x-marks x)]
                  (do
                    (if (>= p w-count)
                      (throw (IllegalStateException.)))
                    p))))
      0)))

(defmulti move-caret-mark (fn [_model what where _viewport-h _interop]
                            (assert (#{:caret :mark :caret-&-mark} what))
                            where))

(defn- move-mark-to-caret [model need-rebuild-primitives]
  (let [caret-line-index (:caret-line model)
        mark-line-index (:mark-line model)
        caret-line (nth (:lines model) caret-line-index)
        mark-line (nth (:lines model) mark-line-index)
        caret-word-index (:caret-word caret-line)
        mark-word-index (:mark-word mark-line)
        caret-word (nth (:words caret-line) caret-word-index)
        caret-pos (:caret-pos caret-word)
        rebuild-primitives-fn (fn [model] (if need-rebuild-primitives (rebuild-primitives model caret-line-index mark-line-index caret-line-index caret-line-index false true) model))]
    (->
      (assoc-in model [:lines mark-line-index :words mark-word-index :mark-pos] nil)     ;(!)
      (assoc-in [:lines mark-line-index :mark-word] nil)
      (assoc-in [:lines caret-line-index :mark-word] caret-word-index)
      (assoc-in [:lines caret-line-index :words caret-word-index :mark-pos] caret-pos)
      (assoc :mark-line caret-line-index)
      (rebuild-primitives-fn))))

(defn- move-caret-to-mark [model need-rebuild-primitives]
  (let [mark-line-index (:mark-line model)
        caret-line-index (:caret-line model)
        mark-line (nth (:lines model) mark-line-index)
        caret-line (nth (:lines model) caret-line-index)
        mark-word-index (:mark-word mark-line)
        caret-word-index (:caret-word caret-line)
        mark-word (nth (:words mark-line) mark-word-index)
        mark-pos (:mark-pos mark-word)
        rebuild-primitives-fn (fn [model] (if need-rebuild-primitives (rebuild-primitives model caret-line-index mark-line-index mark-line-index mark-line-index true false) model))]
    (->
      (assoc-in model [:lines caret-line-index :words caret-word-index :caret-pos] nil)     ;(!)
      (assoc-in [:lines caret-line-index :caret-word] nil)
      (assoc-in [:lines mark-line-index :caret-word] mark-word-index)
      (assoc-in [:lines mark-line-index :words mark-word-index :caret-pos] mark-pos)
      (assoc :caret-line mark-line-index)
      (rebuild-primitives-fn))))

(defn- move-for-keys [model key-count model-transrofm-fn mark-to-caret-afterwards]
  (loop [m model
         k 0]
    (if (< k key-count)
      (recur
        (model-transrofm-fn m k)
        (inc k))
      (if mark-to-caret-afterwards
        (move-mark-to-caret m true)
        m))))

(defn move-caret-mark-1-char [model move-caret move-mark edge-fn edge-last-in-line-fn move-fn need-rebuid-primitives need-backskip-last-space]
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
        key-count (count dest-pos-keys)
        caret-line-index (:caret-line model)
        mark-line-index (:mark-line model)
        rebuild-primitives-fn (if need-rebuid-primitives rebuild-primitives (fn [model _1 _2 _3 _4] model))
        move-within-line (fn [model-transrofm-fn]
                           (let [caret-line (nth (:lines model) caret-line-index)
                                 mark-line (nth (:lines model) mark-line-index)
                                 caret-word-index (:caret-word caret-line)
                                 mark-word-index (:mark-word mark-line)
                                 caret-word (nth (:words caret-line) caret-word-index)
                                 mark-word (nth (:words mark-line) mark-word-index)
                                 mark-pos (:mark-pos mark-word)
                                 caret-pos (:caret-pos caret-word)
                                 mark-to-caret (and
                                                 (and move-caret move-mark)
                                                 (or
                                                   (not= caret-pos mark-pos)
                                                   (not= caret-word-index mark-word-index)
                                                   (not= caret-line-index mark-line-index)))]
                             (->
                               (move-for-keys model key-count model-transrofm-fn mark-to-caret)
                               (rebuild-primitives-fn
                                 caret-line-index mark-line-index
                                 move-caret (or move-mark mark-to-caret)))))]
    (cond
      (not edge-glyph) (move-within-line (fn [m k] (assoc-in m [:lines line-index :words word-index (nth dest-pos-keys k)] (move-fn pos))))
      (not edge-word) (move-within-line
                        (fn [m k] (let [new-word-index (move-fn word-index)
                                        backward (< new-word-index word-index)
                                        new-pos (if backward
                                                  (let [prevword-glyph-cnt (count (get-in model [:lines line-index :words new-word-index :glyphs]))]
                                                    (if need-backskip-last-space (dec prevword-glyph-cnt) prevword-glyph-cnt))
                                                  0)]
                                    (->
                                      (assoc-in m [:lines line-index :words word-index (nth dest-pos-keys k)] nil)
                                      (assoc-in [:lines line-index (nth dest-word-keys k)] new-word-index)
                                      (assoc-in [:lines line-index :words new-word-index (nth dest-pos-keys k)] new-pos)))))
      (not edge-line) (let [new-line-index (move-fn line-index)
                            backward (< new-line-index line-index)
                            new-word-index (if backward
                                             (let [word-count (count (get-in model [:lines new-line-index :words]))] (dec word-count))
                                             0)
                            new-pos (if backward
                                            (let [glyphs (get-in model [:lines new-line-index :words new-word-index :glyphs])
                                                  glyph-count (count glyphs)]
                                              (if (linebreak? (peek glyphs)) (dec glyph-count) glyph-count))
                                            0)]
                        (->
                          (move-for-keys
                            model
                            key-count
                            (fn [m k] (->
                                        (assoc-in m [:lines line-index :words word-index (nth dest-pos-keys k)] nil)
                                        (assoc-in [(nth dest-line-keys k)] new-line-index)
                                        (assoc-in [:lines line-index (nth dest-word-keys k)] nil)
                                        (assoc-in [:lines new-line-index (nth dest-word-keys k)] new-word-index)
                                        (assoc-in [:lines new-line-index :words new-word-index (nth dest-pos-keys k)] new-pos)))
                            false)
                          (rebuild-primitives-fn
                            caret-line-index mark-line-index
                            move-caret move-mark)
                          ))
      :else model)))

(defn move-caret-1-char [model edge-fn edge-last-in-line-fn move-fn]
  (move-caret-mark-1-char model true false edge-fn edge-last-in-line-fn move-fn true true))

(defn move-mark-1-char [model edge-fn edge-last-in-line-fn move-fn]
  (move-caret-mark-1-char model false true edge-fn edge-last-in-line-fn move-fn true true))

(defn move-caret-&-mark-1-char [model edge-fn edge-last-in-line-fn move-fn]
  (move-caret-mark-1-char model true true edge-fn edge-last-in-line-fn move-fn true true))

(defn move-caret-mark-generic [model what edge-fn edge-last-in-line-fn move-fn]
  (condp = what
    :caret (move-caret-1-char model edge-fn edge-last-in-line-fn move-fn)
    :mark (move-mark-1-char model edge-fn edge-last-in-line-fn move-fn)
    :caret-&-mark (move-caret-&-mark-1-char model edge-fn edge-last-in-line-fn move-fn)))

(def forward-edge-fn (fn [coll] (dec (count coll))))

(def forward-edge-last-in-line-fn (fn [coll] (if (linebreak? (peek coll)) (dec (count coll)) (count coll))))

(defmethod move-caret-mark :forward [model what _where _viewport-h _interop]
  (move-caret-mark-generic
    model
    what
    forward-edge-fn
    forward-edge-last-in-line-fn
    inc))

(def backward-edge-fn (fn [_] 0))

(def backward-edge-last-in-line-fn (fn [_] 0))

(defmethod move-caret-mark :backward [model what _where _viewport-h _interop]
  (move-caret-mark-generic model what backward-edge-fn backward-edge-last-in-line-fn dec))

(defn move-line-home-end [model what where]
  (let [caret-line-index (:caret-line model)
        caret-line (nth (:lines model) caret-line-index)
        caret-word-index (:caret-word caret-line)
        new-word-index (if (= where :home) 0 (dec (count (:words caret-line))))
        new-pos (let [new-w-glyphs (:glyphs (nth (:words caret-line) new-word-index))
                      new-w-glyph-count (count new-w-glyphs)]
                  (if (= where :home) 0 (if (linebreak? (peek new-w-glyphs)) (dec new-w-glyph-count) new-w-glyph-count)))
        word-keys (if (= what :caret-&-mark) [:caret-word :mark-word] [:caret-word])
        pos-keys (if (= what :caret-&-mark) [:caret-pos :mark-pos] [:caret-pos])]
    (->
      (move-for-keys
        model
        (count pos-keys)
        (fn [m k] (->
                    (assert-word m caret-line-index new-word-index)
                    (assoc-in [:lines caret-line-index :words caret-word-index (nth pos-keys k)] nil)       ;(!)
                    (assoc-in [:lines caret-line-index (nth word-keys k)] new-word-index)
                    (assoc-in [:lines caret-line-index :words new-word-index (nth pos-keys k)] new-pos)))
        false)
      (rebuild-primitives
        caret-line-index (:mark-line model)
        true false))))

(defmethod move-caret-mark :home [model what where _viewport-h _interop]
  (move-line-home-end model what where))

(defmethod move-caret-mark :end [model what where _viewport-h _interop]
  (move-line-home-end model what where))

(defn get-caret-x [model]
  (let [line (nth (:lines model) (:caret-line model))
        caret-word-index (:caret-word line)
        words (:words line)
        caret-word (nth words caret-word-index)
        caret-pos (:caret-pos caret-word)]
    (+
      (apply + (map :w-total (vu/takev caret-word-index words)))
      (apply + (map #(:w (:size %)) (vu/takev caret-pos (:glyphs caret-word)))))))


(defn move-up-down [model what _where new-line-index]
  (let [caret-line-index (:caret-line model)
        lines (:lines model)]
    (let [mark-line-index (:mark-line model)
          caret-word-index (get-in lines [caret-line-index :caret-word])
          mark-word-index (get-in lines [mark-line-index :mark-word])
          new-line (nth lines new-line-index)
          line-x (get-caret-x model)
          new-caret-word-index (x->pos-in-line new-line line-x)
          new-words (:words new-line)
          new-caret-word (nth new-words new-caret-word-index)
          new-caret-in-word-x (- line-x (apply + (map :w-total (vu/takev new-caret-word-index new-words))))
          new-caret-pos (x->pos-in-word new-caret-word new-caret-in-word-x)
          updated-caret-model (->
                                (assert-word model new-line-index new-caret-word-index)
                                (assoc-in [:lines caret-line-index :words caret-word-index :caret-pos] nil)
                                (assoc-in [:lines caret-line-index :caret-word] nil)
                                (assoc-in [:lines new-line-index :caret-word] new-caret-word-index)
                                (assoc-in [:lines new-line-index :words new-caret-word-index :caret-pos] new-caret-pos)
                                (assoc :caret-line new-line-index))
          mark-change (= what :caret-&-mark)
          updated-cm-model (if mark-change
                             (->
                               (assoc-in updated-caret-model [:lines mark-line-index :words mark-word-index :mark-pos] nil) ;(!)
                               (assoc-in [:lines mark-line-index :mark-word] nil)
                               (assoc-in [:lines new-line-index :mark-word] new-caret-word-index)
                               (assoc-in [:lines new-line-index :words new-caret-word-index :mark-pos] new-caret-pos)
                               (assoc :mark-line new-line-index))
                             updated-caret-model)]
      (rebuild-primitives updated-cm-model caret-line-index mark-line-index new-line-index (if mark-change new-line-index mark-line-index) nil nil))))

(defmethod move-caret-mark :up [model what where _viewport-h _interop]
  (let [caret-line-index (:caret-line model)]
    (if (> caret-line-index 0)
      (move-up-down model what where (dec caret-line-index))
      model)))

(defmethod move-caret-mark :down [model what where _viewport-h _interop]
  (let [caret-line-index (:caret-line model)]
    (if (< caret-line-index (dec (count (:lines model))))
      (move-up-down model what where (inc caret-line-index))
      model)))

(defn scan-line-heights [model caret-line-index viewport-h where]
  (let [lines (:lines model)
        line-count (count lines)
        dir-fn (if (= :page-up where) dec inc)
        stop-fn (if (= :page-up where) inc dec)]
    (loop [i caret-line-index
           h 0]
      (if (and (>= i 0) (< i line-count) (< h viewport-h))
        (recur
          (dir-fn i)
          (+ h (:h (nth lines i))))
        (max 0 (min (stop-fn i) (dec line-count)))))))

(defn move-page-up-down [model what where viewport-h _interop]
  (let [caret-line-index (:caret-line model)
        new-line-index (scan-line-heights model caret-line-index viewport-h where)]
    (if (not= new-line-index caret-line-index)
      (move-up-down model what where new-line-index)
      model)))

(defmethod move-caret-mark :page-up [model what where viewport-h _interop]
  (move-page-up-down model what where viewport-h _interop))

(defmethod move-caret-mark :page-down [model what where viewport-h _interop]
  (move-page-up-down model what where viewport-h _interop))

(defn truncated-word-reducer [words word]
  (cond

    ;; This is for the last word of a line survival
    (= empty-word-with-caret-&-mark word)
    (conj words word)

    (or (nil? word) (vu/emptyv? (:glyphs word)))
    words

    (and
      (not (vu/emptyv? words))
      (let [last-glyph (peek (:glyphs (peek words)))]
        (and
          (not (linebreak? last-glyph))
          (or
            (not (whitespace? last-glyph))
            (every? whitespace? (:glyphs word))
            (linebreak? (vu/firstv (:glyphs word)))))))
    (let [last-word (peek words)]
      (conj
        (pop words)
        (collapse-words last-word word)))

    :else
    (conj words word)))

(defn truncate-words [words] (vec (reduce truncated-word-reducer [] words)))

(defn- kill-glyph-range-in-word [word from-index-incl to-index-excl w interop]
  (let [old-glyphs (:glyphs word)
        old-caret-pos (:caret-pos word)
        old-mark-pos (:mark-pos word)
        new-pos (min from-index-incl to-index-excl)
        glyphs (mapv (fn [i] (nth old-glyphs i)) (filter (fn [i] (not (and (>= i from-index-incl) (< i to-index-excl)))) (range (count old-glyphs))))
        replacement-words (make-words glyphs (if old-caret-pos new-pos) (if old-mark-pos new-pos) w interop)]
    (vu/firstv replacement-words)))

(defn- kill-glyphs-in-word [word edge1 edge2 first-in-word-range last-in-word-range w interop]
  (let [old-glyph-count (count (:glyphs word))]
    (cond
      (and (nil? edge1) (nil? edge2))
      (if (or first-in-word-range last-in-word-range)
        (throw (IllegalStateException. "First or last word in selection range must have edge1 or edge2"))
        ;;Erasing whole word
        nil)

      (= word empty-word-with-caret-&-mark) nil

      (or
        (and (= edge1 0) (= edge2 old-glyph-count))
        (and (= edge1 old-glyph-count) (= edge2 0)))
      nil ;Erasing whole word

      (and edge1 edge2)
      (do
        (if (not (or first-in-word-range last-in-word-range)) (throw (IllegalStateException.)))
        (let [from (min edge1 edge2)
              to (if (= edge1 edge2) (inc edge1) (max edge1 edge2))]
          (kill-glyph-range-in-word word from to w interop)))

      :else
      (let [sel-edge (if edge1 edge1 edge2)]
        (cond
          first-in-word-range (kill-glyph-range-in-word word sel-edge old-glyph-count w interop)
          last-in-word-range (kill-glyph-range-in-word word 0 sel-edge w interop)
          :else (throw (IllegalStateException.)))))))

(defn- propagate-cm-index-up [model after-kill old-model]
  (let [lines (:lines model)
        count-lines (count lines)]
    (if (not= (count (:lines old-model)) count-lines)
      (let [old-caret-line-index (:caret-line old-model)
            start-backward after-kill
            inc-radius? (if start-backward pos? neg?)
            recur-delta (fn [delta] (if (= 0 delta) (if start-backward -1 1) (if (inc-radius? delta) (* (inc delta) -1) (* delta -1))))
            max-delta (max old-caret-line-index (- (inc count-lines) old-caret-line-index))]
        (loop [delta 0]
          (if (<= delta max-delta)
            (let [try-line-index (+ old-caret-line-index delta)]
              (if (and (>= try-line-index 0) (< try-line-index count-lines))
                (let [try-line-words (:words (nth lines try-line-index))
                      tw-cnt (count try-line-words)
                      caret-word-index (loop [twi 0] (if (< twi tw-cnt) (if (:caret-pos (nth try-line-words twi)) twi (recur (inc twi)))))]
                  (if caret-word-index
                    (let [recovered-sel-model (->
                                                (assoc model :caret-line try-line-index)
                                                (assoc :mark-line try-line-index)
                                                (assoc-in [:lines try-line-index :caret-word] caret-word-index)
                                                (assoc-in [:lines try-line-index :mark-word] caret-word-index))
                          caret-word (nth try-line-words caret-word-index)
                          caret-pos (:caret-pos caret-word)]
                      (if (and (= caret-pos (count (:glyphs caret-word))) (< caret-word-index (dec tw-cnt)))
                        (->
                          (move-caret-mark-1-char recovered-sel-model true true backward-edge-fn backward-edge-last-in-line-fn dec false false)
                          (move-caret-mark :caret-&-mark :forward nil nil))
                        recovered-sel-model))
                    (recur (recur-delta delta))))
                (recur (recur-delta delta))))
            (throw (IllegalStateException. "no line with caret located in model")))))
      model)))

(defn- prepare-selection-for-kill [model]
  (let [caret-line-index (:caret-line model)
        caret-line (get-in model [:lines caret-line-index])
        caret-word-index (:caret-word caret-line)
        caret-word (get-in caret-line [:words caret-word-index])
        caret-pos (:caret-pos caret-word)
        mark-line-index (:mark-line model)
        mark-line (get-in model [:lines mark-line-index])
        mark-word-index (:mark-word mark-line)
        mark-word (get-in mark-line [:words mark-word-index])
        mark-pos (:mark-pos mark-word)]
    (if (and (= caret-line-index mark-line-index) (= caret-word-index mark-word-index) (= caret-pos mark-pos))
      (if (and (> caret-word-index 0) (= 1 (count (:glyphs caret-word))))
        (move-caret-mark-1-char model true true backward-edge-fn backward-edge-last-in-line-fn dec false false)
        model)
      (let [caret-before-mark (if (= caret-line-index mark-line-index)
                                (if (= caret-word-index mark-word-index) (< caret-pos mark-pos) (< caret-word-index mark-word-index))
                                (< caret-line-index mark-line-index))
            first-word-killing (if caret-before-mark (= caret-pos 0) (= mark-pos 0))
            last-word-killing (if caret-before-mark (= mark-pos (count (:glyphs mark-word))) (= caret-pos (count (:glyphs caret-word))))]
        (cond
          (and first-word-killing last-word-killing)
          (let [notlast-fn (fn [li line wi] (not (and (= wi (dec (count (:words line)))) (= li (dec (count (:lines model)))))))
                notfirst-fn (fn [li wi] (not (and (= wi 0) (= li 0))))]
            (if caret-before-mark
              (if (notlast-fn mark-line-index mark-line mark-word-index)
                (->
                  (move-caret-to-mark model false)
                  (move-caret-mark-1-char true true forward-edge-fn forward-edge-last-in-line-fn inc false nil))
                (let [cm (move-mark-to-caret model false)]
                  (if (notfirst-fn caret-line-index caret-word-index)
                    (move-caret-mark-1-char cm true true backward-edge-fn backward-edge-last-in-line-fn dec false false)
                    cm)))
              (if (notlast-fn caret-line-index caret-line caret-word-index)
                (->
                  (move-mark-to-caret model false)
                  (move-caret-mark-1-char true true forward-edge-fn forward-edge-last-in-line-fn inc false nil))
                (let [cm (move-caret-to-mark model false)]
                  (if (notfirst-fn mark-line-index mark-word-index)
                    (move-caret-mark-1-char cm true true backward-edge-fn backward-edge-last-in-line-fn dec false false)
                    cm)))
              ))

          caret-before-mark
          (if first-word-killing
            (move-caret-to-mark model false)
            (move-mark-to-caret model false))

          :else
          (if first-word-killing
            (move-mark-to-caret model false)
            (move-caret-to-mark model false)))))))

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
        nonreduced-sel-lines (:lines model)
        reduced-selection-model (prepare-selection-for-kill model) ;TODO prepare-selection-for-kill should process simple delete case
        lines (:lines reduced-selection-model)]
    (loop [m reduced-selection-model
           li from-line-index
           wi from-word-index
           first-w-of-all true]
      (if (<= li to-line-index)
        (let [words (:words (nth lines li))
              last-line (= li to-line-index)
              last-wi-in-line (if last-line to-word-index (dec (count words)))
              now-is-last-wi (= wi last-wi-in-line)
              now-is-last-w-of-all (and last-line now-is-last-wi)
              word (nth words wi)
              nonreduced-sel-word (nth (:words (nth nonreduced-sel-lines li)) wi)
              from-pos (:caret-pos nonreduced-sel-word)
              to-pos (:mark-pos nonreduced-sel-word)
              adj-word (kill-glyphs-in-word word from-pos to-pos first-w-of-all now-is-last-w-of-all w interop)
              killing-in-last-wi-in-line (and now-is-last-wi (= (count words) 1) (= li caret-line-index))
              ]
          (recur
            (assoc-in m [:lines li :words wi]
                      ;; This 'if' handles backspace that kills the line. The last word of a line should survive with cursor in it.
                      ;; On the next step it should be finally killed and cursor should move up one line
                      (if (and killing-in-last-wi-in-line (nil? adj-word) (not (vu/emptyv? (:glyphs word))))
                        empty-word-with-caret-&-mark
                        adj-word))
            (if now-is-last-wi (inc li) li)
            (if now-is-last-wi 0 (inc wi))
            false))
        (let [remainder-words (mapcat :words (vu/take-lastv (- (count lines) from-line-index) (:lines m)))]
          (if (and (= (count (:lines model)) 1) (= (count remainder-words) 1) (nil? (first remainder-words)))
            empty-model
            (->
              (rewrap-partially reduced-selection-model w (truncate-words remainder-words) interop true (dec (min caret-line-index mark-line-index)))
              (propagate-cm-index-up true model))))))))

(defn- glyph->model [model g w interop]                         ;TODO rebuild-primitives??
  (let [line-with-caret (nth (:lines model) (:caret-line model))
        caret-word-index (:caret-word line-with-caret)
        word-with-caret (nth (:words line-with-caret) caret-word-index)
        adj-word (glyph-> word-with-caret g w interop)
        words-after-adj (mapcat :words (vu/take-lastv (- (count (:lines model)) (:caret-line model) 1) (:lines model)))
        move-cm-to-next-word-if-needed (fn [new-model]
                                         (let [new-caret-line-index (:caret-line new-model)
                                               new-caret-line (nth (:lines new-model) new-caret-line-index)
                                               new-caret-word-index (:caret-word new-caret-line)
                                               new-caret-line-words (:words new-caret-line)
                                               new-caret-word (nth new-caret-line-words new-caret-word-index)
                                               new-caret-pos (:caret-pos new-caret-word)]
                                           (if (and (= new-caret-pos (count (:glyphs new-caret-word))) (< new-caret-word-index (dec (count new-caret-line-words))))
                                             (move-caret-mark-1-char new-model true true count nil inc false nil)
                                             new-model)))
        remainder-words (vec
                          (concat
                            (flatten (assoc (:words line-with-caret) caret-word-index adj-word))
                            words-after-adj))]
    (->
      (rewrap-partially model w remainder-words interop false)
      (propagate-cm-index-up false model)
      (move-cm-to-next-word-if-needed))))

(defmethod glyph-> Model [model g w interop]
  (let [m (if (has-selection? model) (kill-glyphs model w interop) model)]
    (glyph->model m g w interop)))

(defn- glyphs->model-insert-impl [model glyphs w interop]
  (let [line-with-caret (nth (:lines model) (:caret-line model))
        caret-word-index (:caret-word line-with-caret)
        word-with-caret (nth (:words line-with-caret) caret-word-index)
        remainder-words (vec
                          (concat
                            (flatten (assoc (:words line-with-caret) caret-word-index
                                                                     (let [caret-pos (:caret-pos word-with-caret)
                                                                           word-glyphs (:glyphs word-with-caret)
                                                                           all-glyphs (concat (subvec word-glyphs 0 caret-pos) glyphs (subvec word-glyphs caret-pos))]
                                                                       ;(glyphs->words all-glyphs w interop)
                                                                       (make-words all-glyphs (+ caret-pos (count glyphs)) w interop)
                                                                       )  ))
                            (mapcat :words (vu/take-lastv (- (count (:lines model)) (:caret-line model) 1) (:lines model)))))]
    (rewrap-partially model w remainder-words interop false)))

(defn glyphs->model [model glyphs w interop]
  (let [m (if (has-selection? model) (kill-glyphs model w interop) model)]
    (glyphs->model-insert-impl m glyphs w interop)))

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
        edge-glyph-by-caret (= caret-pos (if (linebreak? (peek caret-word-glyphs)) (dec (count caret-word-glyphs)) (count caret-word-glyphs)))
        edge-glyph-by-mark (= mark-pos (if (linebreak? (peek mark-word-glyphs)) (dec (count mark-word-glyphs)) (count mark-word-glyphs)))]
    (cond
      (and
        edge-line-by-caret edge-word-by-caret edge-glyph-by-caret
        edge-line-by-mark  edge-word-by-mark edge-glyph-by-mark)
      model

      (or
        (and edge-glyph-by-caret (not edge-word-by-caret))
        (and edge-glyph-by-mark (not edge-word-by-mark)))
      (throw (IllegalStateException. "Cursor may be in edge glyph position of the word only at the end of the line"))

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
    (cond
      (and edge-line edge-word edge-glyph)
      model

      (and (= 1 (count words)) (= empty-word-with-caret-&-mark word))
      (->
        (move-caret-mark model :caret-&-mark :backward nil nil)
        (do-delete w interop)
        (do-delete w interop))

      :else
      (->
        (move-caret-mark model :caret-&-mark :backward nil nil)
        (do-delete w interop)))))

(fg/defaccessorfn get-effective-w [component]
  (- (m/x (get-property component [:this] :clip-size)) (awt/strh component)))

(fg/defevolverfn :content-size
  (let [cs (get-property [:this] :clip-size)
        model (get-property [:this] :model)]
    (m/defpoint (m/x cs) (max (:total-h model) (m/y cs)))))

(fg/defevolverfn :viewport-matrix
  (let [model (get-property [:this] :model)
        lines (:lines model)
        caret-line (nth lines (:caret-line model))
        caret-x (get-caret-x model)
        caret-y (:y caret-line)
        caret-h (:h caret-line)
        clip-size (get-property [:this] :clip-size)
        content-size (get-property [:this] :content-size)
        vmx (- (m/mx-x old-viewport-matrix))
        vmy (- (m/mx-y old-viewport-matrix))]
    (if (not (and
               (>= caret-x vmx)
               (< caret-x (+ vmx (m/x clip-size)))
               (>= caret-y vmy)
               (< (+ caret-y caret-h) (+ vmy (m/y clip-size)))))
      (textcommons/keep-in-range
        (m/translation
          (cond
            (< caret-x vmx) (- caret-x)
            (>= caret-x (+ vmx (m/x clip-size))) (- (- caret-x (m/x clip-size)))
            :else (- vmx))
          (cond
            (< caret-y vmy) (- caret-y)
            (>= (+ caret-y caret-h) (m/y clip-size)) (- (- (+ caret-y caret-h) (m/y clip-size)))
            :else (- vmy)))
        clip-size
        content-size)
      old-viewport-matrix)))


;; TODO for each reason, try-catch with detailed debug output
;; TODO optional sanity check after evolve - situations like:
;; - `ew ``fv  `` `` \n`
;; - `x\n `
;; - caret-word is null in caret line

(defn check-spaces-after-space [words wi]
  (if (pos? wi)
    (let [prev-word (nth words (dec wi))]
      (if (and
            (whitespace? (vu/lastv (:glyphs prev-word)))
            (every? #(or (whitespace? %) (linebreak? %)) (:glyphs (nth words wi))))
        (throw (IllegalStateException. "every-space word after word ending with space"))))))

(defn check-unattached-linebreak [words wi]
  (if (and
        (pos? wi)
        (linebreak? (vu/firstv (:glyphs (nth words wi)))))
    (throw (IllegalStateException. "unattached linebreak"))))

(defn check-delimiter-after-linebreak [words wi]
  (let [word (nth words wi)
        glyphs (:glyphs word)]
    (loop [i (dec (count glyphs))
           delimiter-met false]
      (if (and (>= i 0) (delimiter? (nth glyphs i)))
        (do
          (if (and delimiter-met (linebreak? (nth glyphs i))) (throw (IllegalStateException. "linebreak not the last glyph")))
          (recur
            (dec i)
            (if (delimiter? (nth glyphs i)) true delimiter-met)))))))

(defn check-no-delimiter-at-word-end [words wi]
  (if (and
        (< wi (dec (count words)))
        (not (delimiter? (vu/lastv (:glyphs (nth words wi))))))
    (throw (IllegalStateException. "non-last in line word without delimiter at end"))))

(def sanity-checks-word [check-spaces-after-space
                         check-unattached-linebreak
                         check-delimiter-after-linebreak
                         check-no-delimiter-at-word-end])

(defn check-inconsistent-cm [model]
  (let [out-range? (fn [x cnt] (not (and (>= x 0) (< x cnt))))
        caret-line-index (:caret-line model)
        mark-line-index (:mark-line model)
        lines (:lines model)
        line-cnt (count lines)]
    (do
      (if (nil? caret-line-index) (throw (IllegalStateException. "caret-line-index nil")))
      (if (nil? mark-line-index) (throw (IllegalStateException. "mark-line-index nil")))
      (if (out-range? caret-line-index line-cnt) (throw (IllegalStateException. (str "caret-line-index " caret-line-index " out of range of " line-cnt))))
      (if (out-range? mark-line-index line-cnt) (throw (IllegalStateException. (str "mark-line-index " mark-line-index " out of range of " line-cnt))))
      (let [caret-line (nth lines caret-line-index)
            mark-line (nth lines mark-line-index)
            caret-word-index (:caret-word caret-line)
            mark-word-index (:mark-word mark-line)
            caret-line-words (:words caret-line)
            caret-line-word-count (count (:words caret-line))
            mark-line-words (:words mark-line)
            mark-line-word-count (count (:words mark-line))]
        (do
          (if (nil? caret-word-index) (throw (IllegalStateException. "caret-word-index nil")))
          (if (nil? mark-word-index) (throw (IllegalStateException. "mark-word-index nil")))
          (if (out-range? caret-word-index caret-line-word-count) (throw (IllegalStateException. (str "caret-word-index " caret-word-index " out of range of " caret-line-word-count))))
          (if (out-range? mark-word-index mark-line-word-count) (throw (IllegalStateException. (str "mark-word-index " mark-word-index " out of range of " mark-line-word-count))))
          (let [caret-word (nth caret-line-words caret-word-index)
                mark-word (nth mark-line-words mark-word-index)
                caret-word-max-pos (inc (count (:glyphs caret-word)))
                mark-word-max-pos (inc (count (:glyphs mark-word)))
                caret-pos (:caret-pos caret-word)
                mark-pos (:mark-pos mark-word)]
            (do
              (if (nil? caret-pos) (throw (IllegalStateException. "caret-pos nil")))
              (if (nil? mark-pos) (throw (IllegalStateException. "mark-pos nil")))
              (if (out-range? caret-pos caret-word-max-pos) (throw (IllegalStateException. (str "caret-pos " caret-pos " out of range of " caret-line-word-count))))
              (if (out-range? mark-pos mark-word-max-pos) (throw (IllegalStateException. (str "mark-pos " mark-pos " out of range of " mark-word-max-pos)))))))))))

(def sanity-checks-model [check-inconsistent-cm])

(def perform-sanity-check true)

(defn sanity-check [model]                                        ;;TODO primitives maybe
  (do
    (loop [i 0]
      (if (< i (count sanity-checks-model))
        (do
          ((nth sanity-checks-model i) model)
          (recur (inc i)))))
    (let [lines (:lines model)]
      (loop [l 0]
        (if (< l (count lines))
          (recur
            (do
              (let [words (:words (nth lines l))]
                (loop [w 0]
                  (if (< w (count words))
                    (do
                      (loop [s 0]
                        (if (< s (count sanity-checks-word))
                          (do
                            (if (not (vu/emptyv? (:glyphs (nth words w))))
                              ((nth sanity-checks-word s) words w))
                            (recur (inc s)))))
                      (recur (inc w))))))
              (inc l))))))))

(fg/defaccessorfn evolve-model-supplied-text [component old-model]
  (if-let [supplied-text (if (clipboard/clipboard-paste? component)
                           (clipboard/get-plain-text component)
                           ((get-property [:this] :text-supplier) component))
           ]
    (let [supplied-text-len (count supplied-text)
          ;_ (println "---------------------------------------")
          ;_ (println "typed = " (keyboard/key-typed? component))
          ;_ (println "pressed = " (keyboard/key-pressed? component))
          ;_ (println "Supplied text = " (if (nil? supplied-text) "<nil>" supplied-text) "Len=" supplied-text-len)
          ]
      (try
        (cond

          (= supplied-text-len 1)
          (let [
                glyph (char-glyph (first supplied-text))
                w (get-effective-w component)
                r (glyph-> old-model glyph w (get-property component [:this] :interop))
                ;_ (println "Model:")
                ;_ (println (model->str r))
                ;_ (println r)
                ]
            r)

          (> supplied-text-len 1)
          (let [glyphs (mapv char-glyph supplied-text)
                w (get-effective-w component)]
            (glyphs->model old-model glyphs w (get-property component [:this] :interop)))

          :else old-model)
        (catch Exception ex
          (do
            (println "Evolving model for supplied-text =" supplied-text)
            (println (model->str old-model))
            (throw ex)))))
    old-model))

(fg/defaccessorfn evolve-model-key-pressed [component old-model]
  (let [key (keyboard/get-key component)
        w (get-effective-w component)
        ;_ (println "---------------------------------------")
        ;_ (println "-----pressed " + key)
        shift (inputbase/with-shift? component)
        ]
    (try
      (condp = key
        KeyEvent/VK_BACK_SPACE (do-backspace old-model w (get-property component [:this] :interop))
        KeyEvent/VK_DELETE (do-delete old-model w (get-property component [:this] :interop))
        KeyEvent/VK_LEFT (move-caret-mark old-model (if shift :caret :caret-&-mark) :backward nil nil)
        KeyEvent/VK_RIGHT (move-caret-mark old-model (if shift :caret :caret-&-mark) :forward nil nil)
        KeyEvent/VK_HOME (move-caret-mark old-model (if shift :caret :caret-&-mark) :home nil nil)
        KeyEvent/VK_END (move-caret-mark old-model (if shift :caret :caret-&-mark) :end nil nil)
        KeyEvent/VK_UP (move-caret-mark old-model (if shift :caret :caret-&-mark) :up nil nil)
        KeyEvent/VK_DOWN (move-caret-mark old-model (if shift :caret :caret-&-mark) :down nil nil)
        KeyEvent/VK_PAGE_UP (move-caret-mark old-model (if shift :caret :caret-&-mark) :page-up (m/y (get-property [:this] :clip-size)) nil)
        KeyEvent/VK_PAGE_DOWN (move-caret-mark old-model (if shift :caret :caret-&-mark) :page-down (m/y (get-property [:this] :clip-size)) nil)
        old-model)
      (catch Exception ex
        (do
          (println "Evolving model for key =" key)
          (println (model->str old-model))
          (throw ex))))
    ))

(fg/defevolverfn :model
  (let [new-model (cond

                    ;; TODO not good because might need to rewrap aacording to :clip-size
                    ;(false? (get-property [:this] :editable))
                    ;old-model

                    (mouse/is-mouse-event? component)
                    old-model

                    (or (keyboard/key-typed? component) (clipboard/clipboard-paste? component))
                    (evolve-model-supplied-text component old-model)


                    (keyboard/key-pressed? component)
                    (evolve-model-key-pressed component old-model)

                    :else old-model)]
    (do
      (if perform-sanity-check (sanity-check new-model))
      new-model)))

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
              :content-size content-size-evolver
              :viewport-matrix viewport-matrix-evolver
              ;:text text-evolver

              ;:caret-visible caret-visible-evolver
              ;:->clipboard ->clipboard-evolver
              ;:cursor cursor-evolver
              ;:clip-size auto-size-evolver
              :background (fg/accessorfn (if (get-property component [:this] :editable) :prime-4 :prime-1))
              :foreground (fg/accessorfn (if (get-property component [:this] :editable) :prime-1 :prime-4))}}
  flatgui.widgets.component/component)