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

(defn char-glyph [c]
  (condp = c
    \space (glyph :whitespace nil default-style)
    (glyph :char c default-style)))

(def model
  {:w 0
   :glyphs []
   :caret-pos
   :selection-mark})


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

(def delimiters #{:whitespace :linebreak})

;; We assume that a string's width is the sum of its characters' widths (due to web impl limitations)
(defn line-len [glyphs line-start line-size interop]
  (loop [len 0
         c 0]
    (if (< c line-size)
      (recur
        (+ len (:w (glyph-size (nth glyphs (+ line-start c)) interop)))
        (inc c))
      len)))

(defn wrap-lines [model w interop]
  (let [glyphs (:glyphs model)
        g-count (count glyphs)]
    (loop [line-start 0
           lines []
           last-delim-index -1
           g-index 0]
      (if (>= g-index g-count)
        (if (and (> g-index line-start) (not (delimiters (:type (nth glyphs line-start)))))
          (conj lines [line-start (- g-index line-start)])
          lines)
        (let [g (nth glyphs g-index)
              is-delim (delimiters (:type g))
              current-len (line-len glyphs line-start (- g-index line-start) interop)]
          (if (and is-delim (>= current-len w))
            (let [step-back (and (> current-len w) (not= last-delim-index -1))
                  next-line-start (if step-back (inc last-delim-index) (inc g-index))
                  line [line-start (if step-back (- last-delim-index line-start) (- g-index line-start))]
                  _ (println "--" line)]
              (recur
                next-line-start
                (if (delimiters (:type (nth glyphs line-start)))
                  lines
                  (conj lines [line-start (if step-back (- last-delim-index line-start) (- g-index line-start))]))
                -1
                next-line-start))
            (recur
              (if (and is-delim (= g-index line-start)) (inc g-index) line-start)
              lines
              (if is-delim g-index last-delim-index)
              (inc g-index))
            ))))))

(fg/defevolverfn :model
  (let [w (m/x (get-property [:this] :clip-size))]
    (if (not= w (:w old-model))

      old-model)))