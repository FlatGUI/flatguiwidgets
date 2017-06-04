; Copyright (c) 2015 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Rich Text widget"
      :author "Denys Lebediev"}
flatgui.widgets.textrich
  (:require [flatgui.awt :as awt]
            [flatgui.base :as fg]
            [flatgui.widgets.component]
            [flatgui.widgets.scrollpanel]
            [flatgui.inputchannels.keyboard :as keyboard]
            [flatgui.inputchannels.mouse :as mouse]
            [flatgui.inputchannels.mousewheel :as mousewheel]
            [flatgui.inputchannels.timer :as timer]
            [flatgui.inputchannels.clipboard :as clipboard]
            [flatgui.inputchannels.awtbase :as inputbase]
            [flatgui.util.matrix :as m]
            [flatgui.comlogic :as fgc])
  (:import [java.awt.event KeyEvent]
           (flatgui.core.engine.ui FGTransferable)))


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
     (glyph :char c style)))
  ([c] (char-glyph c default-style)))

(def whitespace-glyph (char-glyph \space))

(def empty-model
  {:glyphs []
   :caret-pos 0
   :selection-mark 0})

(defmulti glyph-size (fn [g _interop] (:type g)))

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

(defmethod glyph-size :test [g _interop] {:w (:w (:style g)) :h (:h (:style g))})

(def delimiters #{:whitespace :linebreak})

;; We assume that a string's width is the sum of its characters' widths (due to web impl limitations)
(defn line-size [glyphs line-start line-size interop]
  (loop [w 0
         h 0
         c 0]
    (if (< c line-size)
      (let [s (glyph-size (nth glyphs (+ line-start c)) interop)]
        (recur
          (+ w (:w s))
          (max h (:h s))
          (inc c)))
      {:w w :h h})))

(defn wrap-lines [glyphs w interop]
  (let [g-count (count glyphs)]
    (loop [line-start 0
           lines []
           last-delim-index -1
           g-index 0
           line-h 0]
      (if (>= g-index g-count)
        (if (and (> g-index line-start) (not (delimiters (:type (nth glyphs line-start)))))
          (conj lines [line-start (- g-index line-start) line-h])
          lines)
        (let [g (nth glyphs g-index)
              is-delim (delimiters (:type g))
              current-size (line-size glyphs line-start (- g-index line-start) interop)
              current-len (:w current-size)
              current-h (:h current-size)
              g-line-h (max current-h line-h)]
          (if (and is-delim (>= current-len w))
            (let [step-back (and (> current-len w) (not= last-delim-index -1))
                  next-line-start (if step-back (inc last-delim-index) (inc g-index))]
              (recur
                next-line-start
                (if (delimiters (:type (nth glyphs line-start)))
                  lines
                  (conj lines [line-start (if step-back (- last-delim-index line-start) (- g-index line-start)) g-line-h]))
                -1
                next-line-start
                0))
            (recur
              (if (and is-delim (= g-index line-start)) (inc g-index) line-start)
              lines
              (if is-delim g-index last-delim-index)
              (inc g-index)
              g-line-h)
            ))))))

(defn render-glyph [lr glyph]
  (let [last-primitive (last lr)]
    (if (#{:char :whitespace} (:type glyph))
      (let [append-to-last (and last-primitive (= :string (:type last-primitive)) (= (:style last-primitive) (:style glyph)))
            addition (if (= :char (:type glyph)) (:data glyph) " ")]
        (if append-to-last
          (assoc-in lr [(dec (count lr)) :data] (str (:data last-primitive) addition))
          (conj lr {:type :string :data addition :style (:style glyph)})))
      (conj lr glyph))))

(defn render-lines [glyphs lines]
  (loop [l 0
         line-rendition []]
    (if (< l (count lines))
      (let [line (nth lines l)
            line-start (first line)
            line-len (second line)]
        (recur
          (inc l)
          (conj
            line-rendition
            (loop [lr []
                   i 0]
              (if (< i line-len)
                (let [g-index (+ line-start i)
                      g (nth glyphs g-index)]
                  (recur
                    (render-glyph lr g)
                    (inc i)))
                {:h (nth line 2) :primitives lr})))))
      line-rendition)))

;(fg/defevolverfn :model
;  (let [w (m/x (get-property [:this] :clip-size))]
;    (if (not= w (:w old-model))
;
;      old-model)))

(fg/defevolverfn :rendition
  (let [model (get-property [:this] :model)
        glyphs (:glyphs model)
        w (m/x (get-property [:this] :clip-size))
        interop (get-property component [:this] :interop)
        lines (wrap-lines glyphs w interop)
        rendition (render-lines glyphs lines)]
    rendition))

(fg/defwidget "textrich"
              {:model empty-model
               :rendition nil
               :caret-visible true;false
               :->clipboard nil
               :first-visible-symbol 0
               :focusable true
               ;:cursor :text
               :skin-key [:textrich]
               :background :prime-4
               :foreground :prime-1
               :no-mouse-press-capturing true
               :evolvers {;:model model-evolver
                          :rendition rendition-evolver
                          ;:caret-visible caret-visible-evolver
                          ;:->clipboard ->clipboard-evolver
                          ;:cursor cursor-evolver
                          }}
              flatgui.widgets.component/component)