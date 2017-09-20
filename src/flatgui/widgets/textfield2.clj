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

;(def empty-rendition
;  {:glyphs []
;   :lines nil
;   :caret-line 0
;   :caret-pos 0
;   :selection-mark 0
;   :caret-coords [0 0 0]
;   :w 0
;   :rendition nil})


(defrecord Model [lines caret-line])

(defrecord Line [words caret-word])

(defrecord Word [glyphs caret-pos w-content w-total])

;(defn id-duplets
;  ([]
;    (fn [rf]
;      (let [pv (volatile! nil)]
;        (fn
;          ([] (rf))
;          ([result] (rf result))
;          ([result input]
;            (let [prior @pv]
;              (vreset! pv input)
;              (if (and prior (= 1 (- input prior)))
;                (rf result 0)
;                (rf result input))))
;          ))))
;  ([coll] (sequence (id-duplets) coll)))

(defn make-words
  ([caret-pos w interop]
    (fn [rf]
      (let [state (volatile! {:w-content 0 :w-total 0 :w-g (ArrayList.) :total-g-count 0 :init-whitespace true})
            make-word (fn [w-content w-total w-g total-g-count]
                        (Word.
                          (vec (.toArray w-g))
                          (if (and (>= caret-pos (- total-g-count (.size w-g))) (< caret-pos total-g-count)) (- caret-pos (- total-g-count (.size w-g))))
                          w-content
                          w-total))]
        (fn
          ([] (rf))
          ([result]
            (let [s @state
                  w-content (:w-content s)
                  w-total (:w-total s)
                  w-g (:w-g s)
                  total-g-count (:total-g-count s)]
              (rf result (make-word w-content w-total w-g total-g-count))))
          ([result g]
            (let [s @state
                  w-content (:w-content s)
                  w-total (:w-total s)
                  w-g (:w-g s)
                  total-g-count (:total-g-count s)
                  whitespace (whitespace? g)
                  init-whitespace (:init-whitespace s)
                  g-w (:w (glyph-size g interop))
                  effective-g-w (if (or (not whitespace) (:init-whitespace s)) g-w 0)
                  w&g-content (+ w-content effective-g-w)
                  w&g-total (+ w-total g-w)]
              (if (> w&g-content w)
                (do
                  (vreset! state {:w-content effective-g-w :w-total g-w :w-g (let [a (ArrayList.)] (do (.add a g) a)) :total-g-count (inc total-g-count) :init-whitespace whitespace})
                  (rf result (make-word w-content w-total w-g total-g-count)))
                (do
                  (vreset! state {:w-content w&g-content :w-total w&g-total :w-g (do (.add w-g g) w-g) :total-g-count (inc total-g-count) :init-whitespace (if init-whitespace whitespace false)})
                  result)
                )))))))
  ([glyphs caret-pos w interop] (transduce (make-words caret-pos w interop) conj glyphs)))

;; TODO
;(defn make-words [glyphs caret-pos w interop]
;  (let [w-content 0                                         ;; TODO
;        w-total 0]
;    (if (<= w-content w)
;      (list
;        (Word. glyphs caret-pos w-content w-total))
;      (list ))
;    ))

;(def empty-model)

(defmulti glyph-> (fn [entity _g _w _interop] (class entity)))

(defmethod glyph-> Word [word g w interop]
  (let [caret-pos (:caret-pos word)
        glyphs (:glyphs word)
        g-is-delimiter (delimiter? g)
        g-goes-after-whitespace (and (> caret-pos 0) (whitespace? (nth glyphs (dec caret-pos))))
        part-before-caret-pos (take caret-pos glyphs)
        part-after-caret-pos (drop caret-pos glyphs)]
    (cond

      (and (not g-is-delimiter) (not g-goes-after-whitespace))
      ;(list
      ;  (Word. (vec (concat part-before-caret-pos (list g) part-after-caret-pos)) (inc caret-pos)))
      (make-words (vec (concat part-before-caret-pos (list g) part-after-caret-pos)) (inc caret-pos) w interop)

      (and (not g-is-delimiter) g-goes-after-whitespace)
      (concat
        (make-words (vec part-before-caret-pos) nil w interop)
        (make-words (vec (conj part-after-caret-pos g)) 1 w interop))

      (and (whitespace? g) (not g-goes-after-whitespace) (some #(not (whitespace? %)) part-before-caret-pos))
      (concat
        (make-words (vec (conj part-before-caret-pos g)) (inc caret-pos) w interop)
        (make-words (vec part-after-caret-pos) nil  w interop))

      (and (whitespace? g) (not g-goes-after-whitespace))
      ;(list
      ;  (Word. (vec (concat part-before-caret-pos (list g) part-after-caret-pos)) (inc caret-pos)))
      (make-words (vec (concat part-before-caret-pos (list g) part-after-caret-pos)) (inc caret-pos) w interop)

      (and (linebreak? g))
      (concat
        (make-words (vec part-before-caret-pos) nil w interop)
        (make-words (vec part-after-caret-pos) 0 w interop))
      
      )))

;(defmethod glyph-> Line [line g]
;  (let [words (:words line)
;        caret-word (:caret-word line)]
;    (if (not (empty? words))
;      (let [replacement-words (glyph-> (nth words caret-word))]
;
;        (Line. (vec (concat (take caret-word words) replacement-words (drop (inc caret-word) words))))
;
;        )
;      (Line. [(Word. [g] 1)] 0))))

;(defn glyphs->words
;  ([w interop]
;    (fn [rf]
;      ))
;  ([glyphs w interop] (transduce (glyphs->words w interop) conj glyphs)))

(defn wrap-lines
  ([w]
    (fn [rf]
      (let [state (volatile! [])]
        (fn
          ([] (rf))
          ([result]
           (let [final-result (rf result @state)]
             (rf final-result)))
          ([result word]
           (let [s @state
                 line {:line s}
                 line-w {:line-w s}
                 w-content (:w-content word)
                 w-total (:w-total word)
                 end-line (> (+ line-w w-content) w)]
             (if end-line
               (do
                 (vreset! state {:line [word] :line-w w-total})
                 (rf result line))
               (vreset! state {:line (conj (:line s) word) :line-w (+ (:line-w s) w-total)})
               )))
          ))))
  ([words w] (transduce (wrap-lines w) conj words)))

;;;;;;
;
; Forward caret for model
;   => do sequence through lines
;     => do sequence with line to

;(defn id-duplets
;  ([]
;    (fn [rf]
;      (let [pv (volatile! nil)]
;        (fn
;          ([] (rf))
;          ([result] (rf result))
;          ([result input]
;            (let [prior @pv]
;              (vreset! pv input)
;              (if (and prior (= 1 (- input prior)))
;                (rf result 0)
;                (rf result input))))
;          ))))
;  ([coll] (sequence (id-duplets) coll)))

(defn id-duplets
  ([]
   (fn [rf]
     (let [_ (println "-----------------stepped into xf construction------------------------with rf=" rf)
           pv (volatile! nil)]
       (fn
         ([] (do (println "  --called xf[]") (rf)))
         ([result] (do (println "  --called xf[r]" result) (rf result)))
         ([result input]
          (let [prior @pv
                _ (println "  --called xf[r i]" result input ", prior=" prior)]
            (vreset! pv input)
            (if (and prior (= 1 (- input prior)))
              (rf result 0)
              (rf result input))))
         ))))
  ([coll] (sequence (id-duplets) coll)))

(defn id-duplets2
  ([]
   (fn [rf]
     (let [_ (println "-----------------stepped into xf construction------------------------with rf=" rf)
           pv (volatile! nil)]
       (fn
         ([] (do (println "  --called xf[]") (rf)))
         ([result] (do (println "  --called xf[r]" result) (rf result)))
         ([result input]
          (let [prior @pv
                _ (println "  --called xf[r i]" result input ", prior=" prior)]
            (vreset! pv input)
            (if (and prior (= 1 (- input prior)))
              (do

                (rf result 0)
                (rf result "x")

                )
              (rf result input))))
         ))))
  ([coll] (sequence (id-duplets2) coll)))

