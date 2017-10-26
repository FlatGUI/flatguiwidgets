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

(defmethod glyph-> Model [model g w interop]
  (let [line-num-to-start-rewrap (max 0 (dec (:caret-line model)))
        prior-lines (take line-num-to-start-rewrap (:lines model))
        line-with-caret (nth (:lines model) (:caret-line model))
        caret-word-index (:caret-word line-with-caret)
        word-with-caret (nth (:words line-with-caret) caret-word-index)
        words-to-wrap (concat
                        (if (not= line-num-to-start-rewrap (:caret-line model)) (:words (nth (:lines model) line-num-to-start-rewrap)))
                        (flatten (assoc (:words line-with-caret) caret-word-index (glyph-> word-with-caret g w interop)))
                        (mapcat :words (take-last (- (count (:lines model)) (:caret-line model) 1) (:lines model))))
        remainder-model (wrap-lines words-to-wrap w)
        result-caret-line (+ (count prior-lines) (:caret-line remainder-model))]
    (Model. (vec (concat prior-lines (:lines remainder-model))) result-caret-line result-caret-line)))

(defn has-selection? [model]
  (if (not= (:caret-line model) (:mark-line model))
    true
    (let [line (nth (:lines model) (:caret-line model))]
      (if (not= (:caret-word line) (:mark-word line))
        true
        (let [word (nth (:words line) (:caret-word line))]
          (not= (:caret-pos word) (:mark-pos word)))))))

(defn move-caret-mark [model what where viewport-h interop]
  (assert (#{:caret :mark :caret-&-mark} what))
  (assert (#{:text-home :home :left :right :end :text-end :up :down :page-up :page-down}) where)
  )

(defn do-backspace-no-sel [model] )

(defn do-delete-no-sel [model] )

(defn cut-selection [model] )

(defn truncate-words [model] )

(defn truncated-word-reducer [words word]
  (cond
    (or (nil? word) (empty? (:glyphs word)))
    words

    (every? whitespace? (:glyphs word))
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
          (:w-content last-word)
          (+ (:w-total last-word) (:w-total word))
          (max (:h last-word) (:h word)))))

    :else
    (conj words word)))