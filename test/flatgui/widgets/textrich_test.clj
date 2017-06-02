; Copyright (c) 2017 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns flatgui.widgets.textrich-test
  (:require [clojure.test :as test]
            [clojure.string :as str]
            [flatgui.base :as fg]
            [flatgui.util.matrix :as m]
            [flatgui.widgets.textrich :as textrich])
  (:import (flatgui.core.engine IResultCollector Container ClojureContainerParser)
           (flatgui.core IFGInteropUtil)))

(def dummy-interop
  (proxy [IFGInteropUtil] []
    (getStringWidth [str _font] (.length str))
    (getFontHeight [_font] 1)))

(defn test-glyph [w h] (textrich/glyph :test nil {:w w :h h}))

;; Good only for test: 1 whitespace = 1 char of text here
(defn lines->strins [text lines] (mapv #(subs text (first %) (+ (first %) (second %))) lines))

(defn test-lines [text w expected-lines]
  (let [glyphs (map textrich/char-glyph text)
        lines (textrich/wrap-lines {:glyphs glyphs} w dummy-interop)]
    (test/is (= expected-lines (lines->strins text lines)))))

(test/deftest wrap-test
  (test-lines "The quick brown fox jumps over the lazy dog" 9 ["The quick" "brown fox" "jumps" "over the" "lazy dog"])
  (test-lines "11 22" 2 ["11" "22"])
  (test-lines "11 22 " 2 ["11" "22"])
  (test-lines "11 22 3" 2 ["11" "22" "3"])
  (test-lines "11 22 33 44" 5 ["11 22" "33 44"])
  (test-lines "11 22 33 44 " 5 ["11 22" "33 44"])
  (test-lines "11 22 33 44  " 5 ["11 22" "33 44"])
  (test-lines "11 22 33 44   " 5 ["11 22" "33 44"])
  (test-lines "11 22 33 44     " 5 ["11 22" "33 44"])
  (test-lines "11 22 33 44      " 5 ["11 22" "33 44"])
  (test-lines " 11 22" 2 ["11" "22"])
  (test-lines "  11 22" 2 ["11" "22"])
  (test-lines "   11 22" 2 ["11" "22"])
  (test-lines "11  22" 2 ["11" "22"])
  (test-lines "11   22" 2 ["11" "22"])
  (test-lines "11    22" 2 ["11" "22"])
  (test-lines "  11  22  " 2 ["11" "22"])
  (test-lines "   11   22   " 2 ["11" "22"])
  (test-lines "  11   22   " 2 ["11" "22"])
  (test-lines "   11  22   " 2 ["11" "22"])
  (test-lines "   11   22  " 2 ["11" "22"])
  (test-lines " 11   22   " 2 ["11" "22"])
  (test-lines "   11 22   " 2 ["11" "22"])
  (test-lines "   11   22 " 2 ["11" "22"]))

(test/deftest wrap-line-h-lines
  (let [glyphs [(test-glyph 1 1) (test-glyph 1 2) (test-glyph 1 1) textrich/whitespace-glyph (test-glyph 1 3) (test-glyph 1 2)]
        lines (textrich/wrap-lines {:glyphs glyphs} 3 dummy-interop)]
    (test/is (= [[0 3 2] [4 2 3]] lines))))