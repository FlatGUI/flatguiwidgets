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
            [flatgui.widgets.textrich :as textrich]
            [flatgui.test :as fgtest])
  (:import (flatgui.core.engine IResultCollector Container ClojureContainerParser)
           (flatgui.core IFGInteropUtil)
           (java.awt.event KeyEvent)))

(def dummy-interop
  (proxy [IFGInteropUtil] []
    (getStringWidth [str _font] (.length str))
    (getFontHeight [_font] 1)
    (getFontAscent [_font] 0)
    ))

(defn test-glyph [w h] (textrich/glyph :test nil {:w w :h h}))

;; Good only for test: 1 whitespace = 1 char of text here
(defn lines->strings [text lines] (mapv #(subs text (first %) (+ (first %) (second %))) lines))

(defn test-lines [text w expected-lines]
  (let [glyphs (map textrich/char-glyph text)
        lines (textrich/wrap-lines glyphs w dummy-interop)]
    (test/is (= expected-lines (lines->strings text lines)))))

;; TODO Implement what happens in real life:
;;  1. It allows trailing space to stay in line even if total line length exceeds w.
;;     This means the cursor may blink on the right margin
;;  2. When right arrow pressed when at the end of the line #1, or left arrow pressed when at
;;     the beginning of line #2, then it jumps between lines skipping the trailing space of line #1
;;
(test/deftest wrap-test
  ;(test-lines "The quick brown fox jumps over the lazy dog" 9 ["The quick" "brown fox" "jumps" "over the" "lazy dog"])

  ;            0  3     9     15  19    25   30
  (test-lines "The quick brown fox jumps over the lazy dog" 9 ["The quick " "brown fox " "jumps " "over the " "lazy dog"])

  (test-lines "11 22" 2 ["11 " "22"])
  (test-lines "11 22 " 2 ["11 " "22 "])
  (test-lines "11 22 3" 2 ["11 " "22 " "3"])
  (test-lines "11 22 33 44" 5 ["11 22 " "33 44"])
  (test-lines "11 22 33 44 " 5 ["11 22 " "33 44 "])

  (test-lines "11 22 33 44  " 5 ["11 22 " "33 44  "])
  (test-lines "11 22  33 44  " 5 ["11 22  " "33 44  "])
  (test-lines "11 22   33 44  " 5 ["11 22   " "33 44  "])
  (test-lines "11 22 33 44   " 5 ["11 22 " "33 44   "])
  (test-lines "11 22 33 44     " 5 ["11 22 " "33 44     "])
  (test-lines "11 22 33 44      " 5 ["11 22 " "33 44      "])

  (test-lines " 11 22" 2 [" 1" "1 " "22"])
  (test-lines " 11 22" 4 [" 11 " "22"])

  (test-lines "  11 22" 2 ["  " "11 " "22"])
  (test-lines "   11 22" 2 ["   " "11 " "22"])
  (test-lines "11  22" 2 ["11  " "22"])
  (test-lines "11   22" 2 ["11   " "22"])
  (test-lines "11    22" 2 ["11    " "22"])
  (test-lines "  11  22  " 2 ["  " "11  " "22  "])
  (test-lines "   11   22   " 2 ["   " "11   " "22   "])
  (test-lines "  11   22   " 2 ["  " "11   " "22   "])
  (test-lines "   11  22   " 2 ["   " "11  " "22   "])
  (test-lines "   11   22  " 2 ["   " "11   " "22  "])
  (test-lines " 11   22   " 2 [" 1" "1   " "22   "])
  (test-lines "   11 22   " 2 ["   " "11 " "22   "])
  (test-lines "   11   22 " 2 ["   " "11   " "22 "])
  )

(test/deftest wrap-test-1
  (let [glyphs [(textrich/char-glyph \1) (textrich/char-glyph \1) (textrich/char-glyph \newline) (textrich/char-glyph \1) (textrich/char-glyph \1)]
        lines (textrich/wrap-lines glyphs 6 dummy-interop)]
    (test/is (= [[0 2 1.0 2.0] [3 2 1.0 2.0]] lines))))

(test/deftest wrap-test-2
  (let [glyphs [(textrich/char-glyph \1) (textrich/char-glyph \1) (textrich/char-glyph \newline) (textrich/char-glyph \1) (textrich/char-glyph \1) (textrich/char-glyph \newline) (textrich/char-glyph \newline)]
        lines (textrich/wrap-lines glyphs 6 dummy-interop)]
    (test/is (= [[0 2 1.0 2.0] [3 2 1.0 2.0] [6 0 1.0 0] [7 0 1.0 0]] lines))))

(test/deftest wrap-test-3
  (let [glyphs [(textrich/char-glyph \1) (textrich/char-glyph \1) (textrich/char-glyph \newline) (textrich/char-glyph \1)]
        lines (textrich/wrap-lines glyphs 6 dummy-interop)]
    (test/is (= [[0 2 1.0 2.0] [3 1 1.0 1.0]] lines))))

(test/deftest wrap-line-h-lines
  (let [glyphs [(test-glyph 1 1) (test-glyph 1 2) (test-glyph 1 1) textrich/whitespace-glyph (test-glyph 1 3) (test-glyph 1 2)]
        lines (textrich/wrap-lines glyphs 3 dummy-interop)]
    (test/is (= [[0 3 2 3] [4 2 3 2]] lines))))

(test/deftest wrap-line-h-lines-2
  (let [data ["aaaaaa bbb" (test-glyph 1.0 2.0) "bbb cccccc"]
        glyphs (mapcat (fn [d] (if (string? d) (map textrich/char-glyph d) [d])) data)
        lines (textrich/wrap-lines glyphs 7 dummy-interop)]
    (test/is (= [[0 6 1.0 6.0] [7 7 2.0 7.0] [15 6 1.0 6.0]] lines))))

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

(test/deftest text-test
  (let [rendition {:rendition [{:h 1.0 :primitives [{:type :string :data "a" :style textrich/default-style}] }
                               {:h 1.0 :primitives [{:type :string :data "b" :style textrich/default-style}] }
                               {:h 2.0 :primitives [(test-glyph 1.0 2.0) {:type :string :data "c" :style textrich/default-style} (test-glyph 2.0 1.0)] }
                               {:h 1.0 :primitives [{:type :string :data "d" :style textrich/default-style}] }
                               {:h 1.0 :primitives [{:type :string :data "e" :style textrich/default-style}] }]}
        container (fg/defcomponent textrich/textrich :main {:rendition rendition
                                                            :evolvers {:rendition nil}})
        text (fgtest/evolve container :text nil)]
    (test/is (= "abcde" text))))

(fgtest/enable-traces-for-failed-tests)

(test/deftest type-text-live-test
  (let [w 3
        root (fg/defroot (fg/defcomponent textrich/textrich :main {:clip-size (m/defpoint w 10)}))
        container (fgtest/init-container root dummy-interop)]

    (fgtest/wait-for-property container [:main] :rendition (assoc textrich/empty-rendition :w w :lines [] :rendition []))

    (fgtest/type-string container [:main] "12")

    (fgtest/wait-for-property-pred container [:main] :rendition
                                   (fn [r] (and (= (:caret-pos r) 2) (= (count (:lines r)) 1))))

    (fgtest/type-key container [:main] KeyEvent/VK_ENTER KeyEvent/VK_ENTER 1)

    (fgtest/wait-for-property-pred container [:main] :rendition
                                   (fn [r] (and (= (:caret-pos r) 3) (= (:caret-line r) 1) (= (count (:lines r)) 2))))

    (fgtest/type-string container [:main] "34 56")

    (fgtest/wait-for-property-pred container [:main] :rendition
                                   (fn [r] (and  (= (count (:lines r)) 3))))

    ))