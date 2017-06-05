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
(defn lines->strings [text lines] (mapv #(subs text (first %) (+ (first %) (second %))) lines))

(defn test-lines [text w expected-lines]
  (let [glyphs (map textrich/char-glyph text)
        lines (textrich/wrap-lines glyphs w dummy-interop)]
    (test/is (= expected-lines (lines->strings text lines)))))

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
        lines (textrich/wrap-lines glyphs 3 dummy-interop)]
    (test/is (= [[0 3 2 3] [4 2 3 2]] lines))))

(test/deftest render-test
  (let [data ["The quick brown fox " (test-glyph 1.0 2.0) "jumps" (test-glyph 2.0 1.0) " over the lazy dog"]
        glyphs (mapcat (fn [d] (if (string? d) (map textrich/char-glyph d) [d])) data)
        lines (textrich/wrap-lines glyphs 9 dummy-interop)
        rendition (textrich/render-lines glyphs lines)]
    (test/is (= [[0 9 1.0 9] [10 9 1.0 9] [20 7 2.0 8] [28 8 1.0 8] [37 8 1.0 8]]) lines)
    (test/is (= [{:h 1.0 :primitives [{:type :string :data "The quick" :style textrich/default-style}] }
                 {:h 1.0 :primitives [{:type :string :data "brown fox" :style textrich/default-style}] }
                 {:h 2.0 :primitives [(test-glyph 1.0 2.0) {:type :string :data "jumps" :style textrich/default-style} (test-glyph 2.0 1.0)] }
                 {:h 1.0 :primitives [{:type :string :data "over the" :style textrich/default-style}] }
                 {:h 1.0 :primitives [{:type :string :data "lazy dog" :style textrich/default-style}] }]) rendition)))

(test/deftest render-test-1
  (let [strings-&-styles [["aaa" :x] ["bbb" :y] ["ccc" :z]]
        glyphs (mapcat (fn [ss] (map (fn [c] (textrich/char-glyph c (second ss))) (first ss))) strings-&-styles)
        lines (textrich/wrap-lines glyphs 9 dummy-interop)
        rendition (textrich/render-lines glyphs lines)]
    (test/is (= [[0 9 1.0 9.0]] lines))
    (test/is (= [{:h 1.0 :primitives [{:type :string :data "aaa" :style :x} {:type :string :data "bbb" :style :y} {:type :string :data "ccc" :style :z}]}] rendition))))