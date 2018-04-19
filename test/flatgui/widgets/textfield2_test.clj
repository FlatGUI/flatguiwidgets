; Copyright (c) 2017 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns flatgui.widgets.textfield2-test
  (:require [clojure.test :as test]
            [clojure.string :as str]
            [flatgui.base :as fg]
            [flatgui.util.matrix :as m]
            [flatgui.widgets.textfield2 :as textfield2]
            [flatgui.test :as fgtest]
            [flatgui.awt :as awt])
  (:import (flatgui.core.engine IResultCollector Container ClojureContainerParser)
           (flatgui.core IFGInteropUtil)
           (java.awt.event KeyEvent)
           (flatgui.widgets.textfield2 Word)
           (java.util.function Supplier)
           ))

(def dummy-interop
  (proxy [IFGInteropUtil] []
    (getStringWidth [str _font] (double (.length str)))
    (getFontHeight [_font] 1)
    (getFontAscent [_font] 0)))

(def dummy-var-interop
  (proxy [IFGInteropUtil] []
    (getStringWidth [s _font] (apply + (map (fn [c] (Integer/valueOf (str c))) s)))
    (getFontHeight [_font] 1)
    (getFontAscent [_font] 0)))

(defn test-glyph
  ([data w h] (textfield2/glyph :test data {:w (double w) :h (double h)}))
  ([w h] (test-glyph nil w h)))

(defn test-sized-glyph-impl
  ([data w h interop]
   (let [g (textfield2/glyph :test data {:w (double w) :h (double h)})]
     (assoc g :size (textfield2/glyph-size g interop))))
  ([w h interop] (test-sized-glyph-impl nil w h interop)))

(defn test-sized-char-glyph-impl [c interop]
  (let [g (textfield2/char-glyph c)]
    (assoc g :size (textfield2/glyph-size g interop))))

(defn test-sized-glyph
  ([data w h] (test-sized-glyph-impl data w h dummy-interop))
  ([w h] (test-sized-glyph-impl w h dummy-interop)))

(defn test-sized-char-glyph [c] (test-sized-char-glyph-impl c dummy-interop))

(defn test-wrap-lines [words w] (textfield2/wrap-lines words w dummy-interop))

(defn tg
  ([s-clean interop] (mapv (fn [c] (test-sized-char-glyph-impl c interop)) s-clean))
  ([s-clean] (tg s-clean dummy-interop)))

(defn tw-impl [s interop]
  (let [caret-pos (.indexOf s "|")
        s-clean (.replace s "|" "")
        s-clean-no-linebreaks (.replace s-clean (str \newline) "")
        w-total (.getStringWidth interop s-clean-no-linebreaks nil)
        trailing-space-count (count (take-while #(= % \space) (reverse s-clean-no-linebreaks)))
        w-content (- w-total trailing-space-count) ; whole word may consist of spaces
        glyphs (tg s-clean interop)
        result-caret-pos (if (>= caret-pos 0) caret-pos)]
    (Word. glyphs result-caret-pos result-caret-pos w-content w-total 1.0)))

(defn tw [s] (tw-impl s dummy-interop))

(defn twv [s] (tw-impl s dummy-var-interop))

(test/deftest make-words-test-1
  (let [glyphs (mapv test-sized-char-glyph "111")
        words (textfield2/make-words glyphs 1 3 dummy-interop)]
    (test/is (= (list (Word. glyphs 1 1 3.0 3.0 1.0)) words))))

(test/deftest make-words-test-2
  (let [glyphs-1 (mapv test-sized-char-glyph "111")
        glyphs-2 [(test-sized-char-glyph \2)]
        glyphs (vec (concat glyphs-1 glyphs-2))
        words-cp-0 (textfield2/make-words glyphs 0 3 dummy-interop)
        words-cp-1 (textfield2/make-words glyphs 1 3 dummy-interop)
        words-cp-2 (textfield2/make-words glyphs 2 3 dummy-interop)
        words-cp-3 (textfield2/make-words glyphs 3 3 dummy-interop)
        words-cp-4 (textfield2/make-words glyphs 4 3 dummy-interop)]
    (test/is (= (list (Word. glyphs-1 0 0 3.0 3.0 1.0) (Word. glyphs-2 nil nil 1.0 1.0 1.0)) words-cp-0))
    (test/is (= (list (Word. glyphs-1 1 1 3.0 3.0 1.0) (Word. glyphs-2 nil nil 1.0 1.0 1.0)) words-cp-1))
    (test/is (= (list (Word. glyphs-1 2 2 3.0 3.0 1.0) (Word. glyphs-2 nil nil 1.0 1.0 1.0)) words-cp-2))
    (test/is (= (list (Word. glyphs-1 nil nil 3.0 3.0 1.0) (Word. glyphs-2 0 0 1.0 1.0 1.0)) words-cp-3))
    (test/is (= (list (Word. glyphs-1 nil nil 3.0 3.0 1.0) (Word. glyphs-2 1 1 1.0 1.0 1.0)) words-cp-4))))

(test/deftest make-words-test-3
  (let [glyphs-1 (mapv test-sized-char-glyph "111")
        glyphs-2 (mapv test-sized-char-glyph "222")
        glyphs-3 (mapv test-sized-char-glyph "33")
        glyphs (vec (concat glyphs-1 glyphs-2 glyphs-3))
        words (textfield2/make-words glyphs 7 3 dummy-interop)]
    (test/is (= (list (Word. glyphs-1 nil nil 3.0 3.0 1.0) (Word. glyphs-2 nil nil 3.0 3.0 1.0) (Word. glyphs-3 1 1 2.0 2.0 1.0)) words))))

(test/deftest make-words-test-4
  (let [glyphs-1 [(test-sized-glyph 2 1.5) (test-sized-glyph 1 1)]
        glyphs-2 [(test-sized-glyph 1 0.5) (test-sized-glyph 1 0.5) (test-sized-glyph 1 0.5)]
        glyphs-3 [(test-sized-glyph 2 1) (test-sized-glyph 1 1.1)]
        glyphs (vec (concat glyphs-1 glyphs-2 glyphs-3))
        words (textfield2/make-words glyphs 6 3 dummy-interop)]
    (test/is (= (list (Word. glyphs-1 nil nil 3.0 3.0 1.5) (Word. glyphs-2 nil nil 3.0 3.0 0.5) (Word. glyphs-3 1 1 3.0 3.0 1.1)) words))))

(test/deftest make-words-test-5
  (let [glyphs (tg "a b c ")
        words (textfield2/make-words glyphs 6 6 dummy-interop)]
    (test/is (= [(tw "a ") (tw "b ") (tw "c |")] words))))

(test/deftest make-words-test-6
  (let [glyphs (tg "a b c  d ")
        words (textfield2/make-words glyphs 6 6 dummy-interop)]
    (test/is (= [(tw "a ") (tw "b ") (tw "c | ") (tw "d ")] words))))

(test/deftest make-words-test-7
  (let [glyphs (tg "a   b ")
        words (textfield2/make-words glyphs 2 3 dummy-interop)]
    (test/is (= [(tw "a |  ") (tw "b ")] words))))

(test/deftest make-words-test-8
  (let [glyphs (tg "abcda b ")
        words (textfield2/make-words glyphs 6 3 dummy-interop)]
    (test/is (= [(tw "abc") (tw "da ") (tw "|b ")] words))))

(test/deftest make-words-test-9
  (let [glyphs (tg "abc\n\n")
        words (textfield2/make-words glyphs 4 3 dummy-interop)]
    (test/is (= [(tw "abc\n") (tw "|\n")] words))))

(defn test-model [model expected-lines expected-total-word-widths expected-caret-line expected-caret-word expected-line-heights]
  (let [model-lines (:lines model)
        lines (mapv :words model-lines)]
    (test/is (= expected-lines (textfield2/lines->strings lines)))
    (test/is (= expected-total-word-widths (textfield2/lines->total-word-widths lines)))
    (if expected-caret-line (test/is (= expected-caret-line (:caret-line model))))
    (if expected-caret-word (test/is (= expected-caret-word (:caret-word (nth model-lines (:caret-line model))))))
    (if expected-line-heights (test/is (= expected-line-heights (textfield2/lines->line-heights model-lines))))))

(defn test-words [words w expected-lines expected-total-word-widths expected-caret-line expected-caret-word]
  (let [model (test-wrap-lines words w)]
    (test-model model expected-lines expected-total-word-widths expected-caret-line expected-caret-word nil)))

(defn test-lines [text w expected-lines expected-total-word-widths]
  (let [glyphs (map textfield2/char-glyph text)
        words (textfield2/glyphs->words glyphs w dummy-interop)]
    (test-words words w expected-lines expected-total-word-widths nil nil)))

(defn model->caret-mark-pos [model]
  (let [caret-line-index (:caret-line model)
        mark-line-index (:mark-line model)
        caret-line (nth (:lines model) caret-line-index)
        mark-line (nth (:lines model) mark-line-index)
        caret-word-index (:caret-word caret-line)
        mark-word-index (:mark-word mark-line)
        caret-word (nth (:words caret-line) caret-word-index)
        mark-word (nth (:words mark-line) mark-word-index)
        caret-pos (:caret-pos caret-word)
        mark-pos (:mark-pos mark-word)]
    [caret-line-index caret-word-index caret-pos mark-line-index mark-word-index mark-pos]))

(defn model-line->caret-sel-coords [model line-index]
  (let [primitives (get-in model [:lines line-index :primitives])
        p (first primitives)]
    [(:caret-x p) (:s-start p) (:s-end p)]))

(test/deftest wrap-test

  (test-lines
    "The quick brown fox jumps over the lazy dog"
    9
    [["The " "quick "] ["brown " "fox "] ["jumps "] ["over " "the "] ["lazy " "dog"]]
    [[4.0 6.0]        [6.0 4.0]        [6.0]      [5.0 4.0]       [5.0 3.0]])

  (test-lines "11 22" 2 [["11 "] ["22"]] [[3.0] [2.0]])
  (test-lines "11 22 " 2 [["11 "] ["22 "]] [[3.0] [3.0]])
  (test-lines "11 22 3" 2 [["11 "] ["22 "] ["3"]] [[3.0] [3.0] [1.0]])
  (test-lines "11 22 33 44" 5 [["11 " "22 "] ["33 " "44"]] [[3.0 3.0] [3.0 2.0]])
  (test-lines "11 22 33 44 " 5 [["11 " "22 "] ["33 " "44 "]] [[3.0 3.0] [3.0 3.0]])
  (test-lines "11 22 33 44   " 5 [["11 " "22 "] ["33 " "44   "]] [[3.0 3.0] [3.0 5.0]])
  (test-lines "11 22 33 44  " 5 [["11 " "22 "] ["33 " "44  "]] [[3.0 3.0] [3.0 4.0]])
  (test-lines "11 22  33 44  " 5 [["11 " "22  "] ["33 " "44  "]] [[3.0 4.0] [3.0 4.0]])
  (test-lines "11 22   33 44  " 5 [["11 " "22   "] ["33 " "44  "]] [[3.0 5.0] [3.0 4.0]])
  (test-lines "11 22 33 44   " 5 [["11 " "22 "] ["33 " "44   "]] [[3.0 3.0] [3.0 5.0]])
  (test-lines "11 22 33 44     " 5 [["11 " "22 "] ["33 " "44     "]] [[3.0 3.0] [3.0 7.0]])
  (test-lines "11 22 33 44      " 5 [["11 " "22 "] ["33 " "44      "]] [[3.0 3.0] [3.0 8.0]])
  (test-lines " 11 22" 2 [[" "] ["11 "] ["22"]] [[1.0] [3.0] [2.0]])
  (test-lines " 11 22" 4 [[" " "11 "] ["22"]] [[1.0 3.0] [2.0]])
  (test-lines " 111 22" 2 [[" "] ["11"] ["1 "] ["22"]] [[1.0] [2.0] [2.0] [2.0]])
  (test-lines "  11 22" 2 [["  "] ["11 "] ["22"]] [[2.0] [3.0] [2.0]])

  (test-lines "   11 22" 2 [["   "] ["11 "] ["22"]] [[3.0] [3.0] [2.0]])

  (test-lines "11  22" 2 [["11  "] ["22"]] [[4.0] [2.0]])
  (test-lines "11   22" 2 [["11   "] ["22"]] [[5.0] [2.0]])
  (test-lines "11    22" 2 [["11    "] ["22"]] [[6.0] [2.0]])
  (test-lines "  11  22  " 2 [["  "] ["11  "] ["22  "]] [[2.0] [4.0] [4.0]])

  (test-lines "   11   22   " 2 [["   "] ["11   "] ["22   "]] [[3.0] [5.0] [5.0]])

  (test-lines "  11   22   " 2 [["  "] ["11   "] ["22   "]] [[2.0] [5.0] [5.0]])
  (test-lines "   11  22   " 2 [["   "] ["11  "] ["22   "]] [[3.0] [4.0] [5.0]])
  (test-lines "   11   22  " 2 [["   "] ["11   "] ["22  "]] [[3.0] [5.0] [4.0]])
  (test-lines " 11   22   " 2 [[" "] ["11   "] ["22   "]] [[1.0] [5.0] [5.0]])
  (test-lines "   11 22   " 2 [["   "] ["11 "] ["22   "]] [[3.0] [3.0] [5.0]])
  (test-lines "   11   22 " 2 [["   "] ["11   "] ["22 "]] [[3.0] [5.0] [3.0]]))

(test/deftest wrap-test2

  (test-words
    [(tw "The ") (tw "quick ")
     (tw "brown ") (tw "fox ")
     (tw "ju|mps ")
     (tw "over ") (tw "the ")
     (tw "lazy ") (tw "dog")]
    9
    [["The " "quick "] ["brown " "fox "] ["jumps "] ["over " "the "] ["lazy " "dog"]]
    [[4.0 6.0] [6.0 4.0] [6.0] [5.0 4.0] [5.0 3.0]]
    2
    0)

  (test-words
    [(tw "|The ") (tw "quick ")
     (tw "brown ") (tw "fox ")
     (tw "jumps ")
     (tw "over ") (tw "the ")
     (tw "lazy ") (tw "dog")]
    9
    [["The " "quick "] ["brown " "fox "] ["jumps "] ["over " "the "] ["lazy " "dog"]]
    [[4.0 6.0] [6.0 4.0] [6.0] [5.0 4.0] [5.0 3.0]]
    0
    0)

  (test-words
    [(tw "T|he ") (tw "quick ")
     (tw "brown ") (tw "fox ")
     (tw "jumps ")
     (tw "over ") (tw "the ")
     (tw "lazy ") (tw "dog")]
    9
    [["The " "quick "] ["brown " "fox "] ["jumps "] ["over " "the "] ["lazy " "dog"]]
    [[4.0 6.0] [6.0 4.0] [6.0] [5.0 4.0] [5.0 3.0]]
    0
    0)

  (test-words
    [(tw "The ") (tw "quick ")
     (tw "brown ") (tw "fox ")
     (tw "jumps ")
     (tw "over ") (tw "the ")
     (tw "lazy ") (tw "do|g")]
    9
    [["The " "quick "] ["brown " "fox "] ["jumps "] ["over " "the "] ["lazy " "dog"]]
    [[4.0 6.0] [6.0 4.0] [6.0] [5.0 4.0] [5.0 3.0]]
    4
    1)

  (test-words
    [(tw "The ") (tw "quick ")
     (tw "brown ") (tw "fox ")
     (tw "jumps ")
     (tw "over ") (tw "the ")
     (tw "lazy ") (tw "dog|")]
    9
    [["The " "quick "] ["brown " "fox "] ["jumps "] ["over " "the "] ["lazy " "dog"]]
    [[4.0 6.0] [6.0 4.0] [6.0] [5.0 4.0] [5.0 3.0]]
    4
    1))

(test/deftest wrap-test3
  (let [words [(tw "T|he ") (tw "quick ")
               (tw "brown ") (tw "fox ")
               (tw "jumps ")
               (tw "over ") (tw "the ")
               (tw "lazy ") (tw "dog")]
        model (test-wrap-lines words 9)]
    (test/is (= 5.0 (:total-h model)))
    (test/is (= [0.0 1.0 2.0 3.0 4.0] (mapv :y (:lines model))))))

(test/deftest wrap-test4
  (let [w 9
        words [(tw "The ") (tw "quick ")
               (tw "brown ") (tw "fox ")
               (tw "jumps ")
               (tw "overZ ") (tw "the| ")
               (tw "lazy ") (tw (str "dog" \newline))]
        model-before (test-wrap-lines words w)
        model-after (textfield2/glyph-> model-before (textfield2/char-glyph \Z) w dummy-interop)]

    (test/is (= 5.0 (:total-h model-before)))
    (test/is (= [0.0 1.0 2.0 3.0 4.0] (mapv :y (:lines model-before))))

    (test/is (= 6.0 (:total-h model-after)))
    (test/is (= [0.0 1.0 2.0 3.0 4.0 5.0] (mapv :y (:lines model-after))))))

(test/deftest wrap-test5
  (let [w 9
        words [(tw "dog|")]
        model-before (test-wrap-lines words w)
        model-after (textfield2/glyph-> model-before textfield2/linebreak-glyph w dummy-interop)]
    (test/is (= 2 (count (:lines model-after))))
    (test/is (= 2.0 (:total-h model-after)))
    (test/is (= [1.0 1.0] (mapv :h (:lines model-after))))
    (test/is (= [0.0 1.0] (mapv :y (:lines model-after))))))

(test/deftest insert-symbol-test-1
  (let [words [(tw "T|he ") (tw "quick ")
               (tw "brown ") (tw "fox ")
               (tw "jumps ")
               (tw "over ") (tw "the ")
               (tw "lazy ") (tw "dog")]
        model-before (test-wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \Z) 9 dummy-interop)
        caret-line (nth (:lines model-after) (:caret-line model-after))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after
      [["TZhe "] ["quick "] ["brown " "fox "] ["jumps "] ["over " "the "] ["lazy " "dog"]]
      [[5.0]     [6.0]      [6.0 4.0]         [6.0]      [5.0 4.0]        [5.0 3.0]]
      0
      0
      [1.0      1.0       1.0             1.0       1.0            1.0])
    (test/is (= 2 (:caret-pos caret-word)))))

(test/deftest insert-symbol-test-2
  (let [words [(tw "The ") (tw "quick ")
               (tw "brown ") (tw "fox ")
               (tw "jumps| ")
               (tw "over ") (tw "the ")
               (tw "lazy ") (tw "dog")]
        model-before (test-wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \Z) 9 dummy-interop)
        caret-line (nth (:lines model-after) (:caret-line model-after))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after
      [["The " "quick "] ["brown " "fox "] ["jumpsZ "] ["over " "the "] ["lazy " "dog"]]
      [[4.0 6.0]         [6.0 4.0]         [7.0]       [5.0 4.0]        [5.0 3.0]]
      2
      0
      [1.0             1.0             1.0        1.0            1.0])
    (test/is (= 6 (:caret-pos caret-word)))))

(test/deftest insert-symbol-test-3
  (let [words [(tw "The ") (tw "quick ")
               (tw "brown ") (tw "fox| ")
               (tw "jumps ")
               (tw "over ") (tw "the ")
               (tw "lazy ") (tw "dog")]
        model-before (test-wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \Z) 9 dummy-interop)
        caret-line (nth (:lines model-after) (:caret-line model-after))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after
      [["The " "quick "] ["brown "] ["foxZ "] ["jumps "] ["over " "the "] ["lazy " "dog"]]
      [[4.0 6.0]         [6.0]      [5.0]     [6.0]      [5.0 4.0]        [5.0 3.0]]
      2
      0
      [1.0             1.0        1.0      1.0       1.0            1.0])
    (test/is (= 4 (:caret-pos caret-word)))))

(test/deftest insert-symbol-test-4
  (let [words [(tw "aaa |")]
        model-before (test-wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \Z) 9 dummy-interop)
        caret-line (nth (:lines model-after) (:caret-line model-after))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after
      [["aaa " "Z"]]
      [[4.0    1.0]]
      0
      1
      [1.0])
    (test/is (= 1 (:caret-pos caret-word)))))

(test/deftest insert-symbol-test-5
  (let [words [(tw "aa|a")]
        model-before (test-wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before textfield2/linebreak-glyph 9 dummy-interop)
        caret-line (nth (:lines model-after) (:caret-line model-after))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after
      [["aa"] ["a"]]
      [[2.0] [1.0]]
      1
      0
      [1.0 1.0])
    (test/is (= 0 (:caret-pos caret-word)))))

(test/deftest insert-symbol-test-6
  (let [words [(tw "aa|a")]
        model-before (test-wrap-lines words 9)
        model-after-0 (textfield2/glyph-> model-before textfield2/linebreak-glyph 9 dummy-interop)
        model-after-1 (textfield2/glyph-> model-after-0 textfield2/linebreak-glyph 9 dummy-interop)
        caret-line (nth (:lines model-after-1) (:caret-line model-after-1))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after-1
      [["aa"] [""] ["a"]]
      [[2.0] [0.0] [1.0]]
      2
      0
      [1.0 1.0 1.0])
    (test/is (= 0 (:caret-pos caret-word)))))

(test/deftest insert-symbol-test-7
  (let [words [(tw "aa|")]
        model-before (test-wrap-lines words 9)
        model-after-0 (textfield2/glyph-> model-before textfield2/linebreak-glyph 9 dummy-interop)
        model-after-1 (textfield2/glyph-> model-after-0 (textfield2/char-glyph \a) 9 dummy-interop)
        caret-line (nth (:lines model-after-1) (:caret-line model-after-1))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after-0
      [["aa"] [""]]
      [[2.0] [0.0]]
      1
      0
      [1.0 1.0])
    (test-model
      model-after-1
      [["aa"] ["a"]]
      [[2.0][1.0]]
      1
      0
      [1.0 1.0])
    (test/is (= 1 (:caret-pos caret-word)))))

(test/deftest insert-symbol-test-8
  (let [words [(tw "a|aa")]
        model-before (textfield2/move-caret-mark (test-wrap-lines words 9) :caret :forward nil nil)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \b) 9 dummy-interop)
        expected-words [(tw "ab|a")]]
    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest insert-symbol-test-9
  (let [w 50
        model (test-wrap-lines [(tw (str "|aa" \newline))
                                (tw (str "bb" \newline))
                                (tw "cc")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :caret :down nil nil)
                          (textfield2/move-caret-mark :caret :down nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after (textfield2/glyph-> model-before-cm (textfield2/char-glyph \X) w dummy-interop)
        expected-words [(tw "X|")]]
    (test/is (= 1 (count (:lines model-after))))
    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest insert-symbol-test-10
  (let [w 10
        words [(tw "a|b")]
        model-before (test-wrap-lines words w)
        model-after  (textfield2/glyph-> model-before textfield2/whitespace-glyph w dummy-interop)
        expected-words [(tw "a ") (tw "|b")]]
    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest insert-symbol-test-11
  (let [w 10
        words [(tw "a|b")]
        model-before (test-wrap-lines words w)
        model-after  (textfield2/glyphs->model model-before [(textfield2/char-glyph \c) textfield2/whitespace-glyph] w dummy-interop)
        expected-words [(tw "ac ") (tw "|b")]]
    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest insert-symbol-test-12
  (let [words [(tw "a|b")]
        model-before (textfield2/move-caret-mark (test-wrap-lines words 9) :caret :forward nil nil)
        model-after  (textfield2/glyph-> model-before textfield2/whitespace-glyph 9 dummy-interop)
        expected-words [(tw "a |")]]
    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest insert-symbol-test-13
  (let [w 11
        words [(tw "    ") (tw "abc ")
               (tw "|bbbbbbbbbb")]
        model-before (test-wrap-lines words w)
        model-after (textfield2/glyph-> model-before textfield2/linebreak-glyph w dummy-interop)
        ]
    (test/is (= [(tw "    ") (tw "abc \n")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "|bbbbbbbbbb")] (:words (nth (:lines model-after) 1))))))

(test/deftest insert-symbol-test-14
  (let [words [(tw "a | ") (tw "b")]
        model-before (test-wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \c) 9 dummy-interop)
        expected-words [(tw "a ") (tw "c| ") (tw "b")]]
    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest insert-symbol-test-15
  (let [words [(tw "a|cd")]
        model-before (test-wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before textfield2/whitespace-glyph 9 dummy-interop)
        expected-words [(tw "a ") (tw "|cd")]]
    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest insert-symbol-test-16
  (let [w 7
        glyphs (tg "bb aaaaaaaaa")
        words (textfield2/make-words glyphs 4 w dummy-interop)
        model-before (test-wrap-lines words w)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \X) w dummy-interop)]
    (test/is (= [(tw "bb ")] (:words (nth (:lines model-before) 0))))
    (test/is (= [(tw "a|aaaaaa")] (:words (nth (:lines model-before) 1))))
    (test/is (= [(tw "aa")] (:words (nth (:lines model-before) 2))))
    (test/is (= [(tw "bb ")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "aX|aaaaa")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "aaa")] (:words (nth (:lines model-after) 2))))))

(test/deftest insert-symbol-test-17
  (let [w 7
        glyphs (tg "bb aaaaaaaaa")
        words (textfield2/make-words glyphs 3 w dummy-interop)
        model-before (test-wrap-lines words w)
        model-after  (textfield2/glyph-> model-before textfield2/whitespace-glyph w dummy-interop)]
    (test/is (= [(tw "bb ")] (:words (nth (:lines model-before) 0))))
    (test/is (= [(tw "|aaaaaaa")] (:words (nth (:lines model-before) 1))))
    (test/is (= [(tw "aa")] (:words (nth (:lines model-before) 2))))
    (test/is (= [(tw "bb  ")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "|aaaaaaa")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "aa")] (:words (nth (:lines model-after) 2))))))

(test/deftest insert-symbol-test-18
  (let [w 5
        words [(tw "a |") (tw "bbbbb")]
        model-before (test-wrap-lines words w)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \b) w dummy-interop)]
    (test/is (= [(tw "a |")] (:words (nth (:lines model-before) 0))))
    (test/is (= [(tw "bbbbb")] (:words (nth (:lines model-before) 1))))
    (test/is (= [(tw "a ")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "b|bbbb")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "b")] (:words (nth (:lines model-after) 2))))))

(defrecord CGlyph [type data style]
  Supplier
  (get [_this] data))

(test/deftest insert-symbol-custom-glyph-test
  (let [w 11
        model-before textfield2/empty-model
        custom-glyph (CGlyph. :char "Alpha" textfield2/default-style)
        model-after (textfield2/glyph-> model-before custom-glyph w dummy-interop)
        ]
    (test/is (= "Alpha" (.get (get-in model-after [:lines 0 :words 0 :glyphs 0]))))))

(test/deftest primitive-test-1
  (let [w 5
        glyphs (map textfield2/char-glyph "a")
        words (textfield2/glyphs->words glyphs w dummy-interop)
        model (test-wrap-lines words w)
        lines (:lines model)
        line (first lines)
        primitives (:primitives line)
        primitive (first primitives)]
    (test/is (= :string (:type primitive)))
    (test/is (= "a" (:data primitive)))))

(test/deftest primitive-test-2
  (let [w 5
        glyphs (concat (map textfield2/char-glyph "abc") [textfield2/linebreak-glyph] (map textfield2/char-glyph "d"))
        words (textfield2/glyphs->words glyphs w dummy-interop)
        model (test-wrap-lines words w)
        lines (:lines model)]
    (test/is (= :string (:type (first (:primitives (first lines))))))
    (test/is (= "abc" (:data (first (:primitives (first lines))))))
    (test/is (= :string (:type (first (:primitives (second lines))))))
    (test/is (= "d" (:data (first (:primitives (second lines))))))))

(test/deftest primitive-test-3
  (let [w 50
        model (test-wrap-lines [(tw (str "aa" \newline)) (tw (str "b|b" \newline)) (tw "cc")] w)]
    (test/is (= [1 0 1 1 0 1] (model->caret-mark-pos model)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model 0)))
    (test/is (= [1.0 nil nil] (model-line->caret-sel-coords model 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model 2)))))

(test/deftest primitive-test-4
  (let [words [(tw "aaa ") (tw "b ") (tw "cc ")
               (tw "f ") (tw "gggg ") (tw "|h ")
               (tw "i ")]
        model-before (test-wrap-lines words 8)
        model-after (->
                      (textfield2/move-caret-mark model-before :caret-&-mark :forward nil nil)
                      (textfield2/move-caret-mark :caret-&-mark :forward nil nil)
                      (textfield2/move-caret-mark :caret-&-mark :forward nil nil)
                      (textfield2/move-caret-mark :caret-&-mark :backward nil nil)
                      (textfield2/move-caret-mark :caret-&-mark :backward nil nil)
                      (textfield2/move-caret-mark :caret-&-mark :backward nil nil))]
    (test/is (= [1 2 0 1 2 0] (model->caret-mark-pos model-after)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-after 0)))
    (test/is (= [7.0 nil nil] (model-line->caret-sel-coords model-after 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-after 2)))))

(test/deftest primitive-test-5
  (let [words [(tw (str "a|a" \newline)) (tw "cc")]
        model-before (test-wrap-lines words 8)
        model-after (textfield2/move-caret-mark model-before :caret-&-mark :forward nil nil)]
    (test/is (= [0 0 2 0 0 2] (model->caret-mark-pos model-after)))
    (test/is (= [2.0 nil nil] (model-line->caret-sel-coords model-after 0)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-after 1)))
    ))

(test/deftest primitive-test-6
  (let [words [(tw (str "|aa" \newline)) (tw (str "bb" \newline)) (tw (str \newline))]
        model-before (test-wrap-lines words 8)
        model-after (->
                      (textfield2/move-caret-mark model-before :caret :forward nil nil)
                      (textfield2/move-caret-mark :caret :forward nil nil)
                      (textfield2/move-caret-mark :caret :forward nil nil)
                      (textfield2/move-caret-mark :caret :forward nil nil)
                      (textfield2/move-caret-mark :caret :forward nil nil))]
    (test/is (= [1 0 2 0 0 0] (model->caret-mark-pos model-after)))
    (test/is (= [nil 0.0 2.0] (model-line->caret-sel-coords model-after 0)))
    (test/is (= [2.0 0.0 2.0] (model-line->caret-sel-coords model-after 1)))))

(test/deftest rebuild-primitives-test-1
  (let [w 7
        model (test-wrap-lines [(tw "aa ") (tw "bb ")
                                      (tw "cccc| ")
                                      (tw "dddd ")
                                      (tw "eee ") (tw "ff ")] w)
        model-after (->
                      (assoc-in model [:lines 1 :words 0 :caret-pos] nil)
                      (assoc-in [:lines 1 :caret-word] nil)
                      (assoc-in [:lines 2 :caret-word] 0)
                      (assoc-in [:lines 2 :words 0 :caret-pos] 4)
                      (textfield2/rebuild-primitives 1 1 2 1 true false))]
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-after 0)))
    (test/is (= [nil 4.0 5.0] (model-line->caret-sel-coords model-after 1)))
    (test/is (= [4.0 0.0 4.0] (model-line->caret-sel-coords model-after 2)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-after 3)))))

(test/deftest line-h-test-1
  (let [words [(tw "The ")                  (assoc (tw "quick ") :h 2.0)
               (assoc (tw "brown ") :h 0.5) (tw "fox| ")
               (assoc (tw "jumps ") :h 1.2)
               (assoc (tw "over ") :h 1.3)  (assoc (tw "the ") :h 1.4)
               (assoc (tw "lazy ") :h 1.5)  (assoc (tw "dog") :h 1.4)]
        model (test-wrap-lines words 9)]
    (test-model
      model
      [["The " "quick "] ["brown " "fox "] ["jumps "] ["over " "the "] ["lazy " "dog"]]
      [[4.0 6.0]         [6.0 4.0]         [6.0]      [5.0 4.0]        [5.0 3.0]]
      1
      1
      [2.0             1.0              1.2       1.4            1.5])))

(test/deftest line-h-test-2
  (let [words [(tw "aaa ")                  (tw "bbb ")              (assoc (tw "ccc ") :h 2.0)
               (assoc (tw "dddd ") :h 0.5)  (tw "eeee ")
               (assoc (tw "ffffffffff ") :h 1.2)
               (assoc (tw "g|g ") :h 1.3)  (assoc (tw "hh ") :h 1.4) (assoc (tw "iii ") :h 1.5)
               (assoc (tw "jjj ") :h 1.5)  (assoc (tw "lll ") :h 1.4) (assoc (tw "mmm") :h 1.3)]
        model (test-wrap-lines words 12)
        caret-line (nth (:lines model) (:caret-line model))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model
      [["aaa " "bbb " "ccc "] ["dddd " "eeee "] ["ffffffffff "] ["gg " "hh " "iii "] ["jjj " "lll " "mmm"]]
      [[4.0    4.0    4.0]    [5.0     5.0]     [11.0]          [3.0   3.0   4.0]    [4.0    4.0    3.0]]
      3
      0
      [2.0                 1.0              1.2           1.5               1.5])
    (test/is (= 1 (:caret-pos caret-word)))))

(test/deftest truncated-word-reducer-test-1
  (let [words [(tw "The ") nil (tw "|  ")]
        reduction (textfield2/truncate-words words 20 dummy-interop)]
    (test/is (= [(tw "The |  ")] reduction))))

(test/deftest truncated-word-reducer-test-2
  (let [words [(tw "The| ") nil (tw "  ")]
        reduction (textfield2/truncate-words words 20 dummy-interop)]
    (test/is (= [(tw "The|   ")] reduction))))

(test/deftest truncated-word-reducer-test-3
  (let [words [(tw "The ") (tw " | ") (tw "  ")]
        reduction (textfield2/truncate-words words 20 dummy-interop)]
    (test/is (= [(tw "The  |   ")] reduction))))

(test/deftest truncated-word-reducer-test-4
  (let [words [(tw "aa") (tw "b|b")]
        reduction (textfield2/truncate-words words 20 dummy-interop)]
    (test/is (= [(tw "aab|b")] reduction))))

(test/deftest truncated-word-reducer-test-5
  (let [words [(tw "a ") nil nil (tw "|d")]
        reduction (textfield2/truncate-words words 20 dummy-interop)]
    (test/is (= [(tw "a ") (tw "|d")] reduction))))

(test/deftest truncated-word-reducer-test-6
  (let [words [(tw (str "aa" \newline)) (tw "b|b")]
        reduction (textfield2/truncate-words words 20 dummy-interop)]
    (test/is (= words reduction))))

(test/deftest truncated-word-reducer-test-7
  ;; Here caret at the end of the 1st word is OK because this may be intermediate state after Del pressed for "a|\n"
  (let [words [(tw (str "a")) (tw " ") (tw "b\n") (tw " ") (tw "b")]
        reduction (textfield2/truncate-words words 20 dummy-interop)]
    (test/is (= [(tw (str "a ")) (tw "b\n") (tw " ") (tw "b")] reduction))))

(test/deftest test-glyphs->Word-1
  (let [w 5
        word-before (tw " |")
        words-after (vec (textfield2/glyph-> word-before (test-sized-char-glyph \b) w dummy-interop))]
    (test/is (= [(tw " ") (tw "b|")] words-after))))

(test/deftest test-glyphs->Word-2
  (let [w 10
        word-before (tw "a | ")
        words-after (vec (textfield2/glyph-> word-before (test-sized-char-glyph \b) w dummy-interop))]
    (test/is (= [(tw "a ") (tw "b| ")] words-after))))

(test/deftest test-glyphs->Word-3
  (let [w 10
        word-before (tw "a|cd")
        words-after (vec (textfield2/glyph-> word-before (test-sized-char-glyph \b) w dummy-interop))]
    (test/is (= [(tw "ab|cd")] words-after))))

(test/deftest test-glyphs->Word-4
  (let [w 10
        word-before (tw "a|cd")
        words-after (vec (textfield2/glyph-> word-before textfield2/whitespace-glyph w dummy-interop))]
    ;; glyph-> Word is not supposed to be smart enough to move caret to the next word, this happens later
    (test/is (= [(tw "a |") (tw "cd")] words-after))))

(test/deftest test-glyphs->Word-5
  (let [w 10
        word-before (tw "a | ")
        words-after (vec (textfield2/glyph-> word-before textfield2/whitespace-glyph w dummy-interop))]
    ;; glyph-> Word is not supposed to be smart enough to move caret to the next word, this happens later
    (test/is (= [(tw "a  | ")] words-after))))

(test/deftest test-glyphs->Word-6
  (let [w 10
        word-before (tw "ab|c")
        words-after (vec (textfield2/glyph-> word-before (textfield2/char-glyph \newline) w dummy-interop))]
    (test/is (= [(tw "ab\n") (tw "|c")] words-after))))

(test/deftest test-glyphs->Word-7
  (let [w 10
        word-before (tw "abc|")
        words-after (vec (textfield2/glyph-> word-before (textfield2/char-glyph \newline) w dummy-interop))]
    (test/is (= [(tw "abc\n") textfield2/empty-word-with-caret-&-mark] words-after))))

(test/deftest test-glyphs->Word-8
  (let [w 10
        word-before (tw "ab |c")
        words-after (vec (textfield2/glyph-> word-before (textfield2/char-glyph \newline) w dummy-interop))]
    (test/is (= [(tw "ab \n") (tw "|c")] words-after))))

(test/deftest test-glyphs->Word-9
  (let [w 10
        word-before (tw "abc |")
        words-after (vec (textfield2/glyph-> word-before (textfield2/char-glyph \newline) w dummy-interop))]
    (test/is (= [(tw "abc \n") textfield2/empty-word-with-caret-&-mark] words-after))))

(test/deftest test-glyphs->Word-10
  (let [w 10
        word-before (tw "ab | ")
        words-after (vec (textfield2/glyph-> word-before (textfield2/char-glyph \newline) w dummy-interop))]
    (test/is (= [(tw "ab \n") (tw "| ")] words-after))))

(test/deftest test-glyphs->Model-1
  (let [w 5
        model-before textfield2/empty-model
        model-after-1 (->
                        (textfield2/glyph-> model-before (test-sized-char-glyph \a) w dummy-interop)
                        (textfield2/glyph-> (textfield2/char-glyph \newline) w dummy-interop)
                        (textfield2/glyph-> textfield2/whitespace-glyph w dummy-interop)
                        (textfield2/glyph-> (textfield2/char-glyph \b) w dummy-interop))
        model-after-2 (textfield2/do-backspace model-after-1 w dummy-interop)]
    (test-model
      model-after-1
      [["a"] [" " "b"]]
      [[1.0] [1.0 1.0]]
      1
      1
      [1.0    1.0])
    (test-model
      model-after-2
      [["a"] [" "]]
      [[1.0] [1.0]]
      1
      0
      [1.0    1.0])))

(test/deftest test-glyphs->Model-2
  (let [w 5
        model-before textfield2/empty-model
        model-after (->
                        (textfield2/glyph-> model-before (test-sized-char-glyph \h) w dummy-interop)
                        (textfield2/glyph-> textfield2/whitespace-glyph w dummy-interop)
                        (textfield2/glyph-> (textfield2/char-glyph \h) w dummy-interop)
                        (textfield2/do-backspace w dummy-interop))
        ]
    (test-model
      model-after
      [["h "]]
      [[2.0]]
      0
      0
      [1.0])
    (test/is (= 2 (get-in model-after [:lines 0 :words 0 :caret-pos])))))

(test/deftest test-glyphs->Model-3
  (let [w 5
        model-before textfield2/empty-model
        model-after (->
                      (textfield2/glyph-> model-before (test-sized-char-glyph \g) w dummy-interop)
                      (textfield2/glyph-> (textfield2/char-glyph \newline) w dummy-interop)
                      (textfield2/do-backspace w dummy-interop))
        ]
    (test-model
      model-after
      [["g"]]
      [[1.0]]
      0
      0
      [1.0])
    (test/is (= 1 (get-in model-after [:lines 0 :words 0 :caret-pos])))))

(test/deftest test-glyphs->Model-4
  (let [w 8
        words [(tw (str "aa" \newline)) (tw (str "|bb" \newline)) (tw (str "cc" \newline))]
        model-before (test-wrap-lines words w)
        model-after (textfield2/do-backspace model-before w dummy-interop)
        ]
    (test-model
      model-after
      [[(str "aabb")] [(str "cc")]]
      [[4.0]          [2.0]]
      0
      0
      [1.0            1.0])
    (test/is (= 2 (get-in model-after [:lines 0 :words 0 :caret-pos])))))

(test/deftest test-glyphs->Model-5
  (let [w 5
        words [(tw "a |") (tw "bbbbb")]
        model-before (test-wrap-lines words w)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \b) w dummy-interop)]
    (test/is (= [(tw "a |")] (:words (nth (:lines model-before) 0))))
    (test/is (= [(tw "bbbbb")] (:words (nth (:lines model-before) 1))))
    (test/is (= [(tw "a ")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "b|bbbb")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "b")] (:words (nth (:lines model-after) 2))))))

(defn word-content-equal [w1 w2] (= (:glyphs w1) (:glyphs w2)))

(defn line-content-equal [l1 l2]
  (and
    (= (count (:words l1)) (count (:words l2)))
    (every? true? (map #(word-content-equal (nth (:words l1) %) (nth (:words l2) %)) (range (count (:words l1)))))))

(defn model-content-equal [m1 m2]
  (and
    (= (count (:lines m1)) (count (:lines m2)))
    (every? true? (map #(line-content-equal (nth (:lines m1) %) (nth (:lines m2) %)) (range (count (:lines m1)))))))

(test/deftest move-caret-mark-test-1
  (let [words [(tw "a|aa ") (tw "b ") (tw "cc ")
               (tw "f ") (tw "gggg ") (tw "h ")
               (tw "i ")]
        model-cm-0-0-1 (test-wrap-lines words 8)
        model-cm-0-0-2 (textfield2/move-caret-mark model-cm-0-0-1 :caret-&-mark :forward nil nil)
        model-cm-0-0-1a (textfield2/move-caret-mark model-cm-0-0-2 :caret-&-mark :backward nil nil)
        model-cm-0-0-2a (textfield2/move-caret-mark model-cm-0-0-1a :caret-&-mark :forward nil nil)
        model-cm-0-0-3 (textfield2/move-caret-mark model-cm-0-0-2 :caret-&-mark :forward nil nil)
        model-cm-0-1-0 (textfield2/move-caret-mark model-cm-0-0-3 :caret-&-mark :forward nil nil)
        model-cm-0-0-3a (textfield2/move-caret-mark model-cm-0-1-0 :caret-&-mark :backward nil nil)
        model-cm-0-1-0a (textfield2/move-caret-mark model-cm-0-0-3a :caret-&-mark :forward nil nil)
        model-cm-0-1-1 (textfield2/move-caret-mark model-cm-0-1-0 :caret-&-mark :forward nil nil)
        model-m-0-1-1-c-0-2-0 (textfield2/move-caret-mark model-cm-0-1-1 :caret :forward nil nil)
        model-m-0-1-1-c-0-2-1 (textfield2/move-caret-mark model-m-0-1-1-c-0-2-0 :caret :forward nil nil)
        model-m-0-1-1-c-0-2-2 (textfield2/move-caret-mark model-m-0-1-1-c-0-2-1 :caret :forward nil nil)
        model-m-0-1-1-c-0-2-3 (textfield2/move-caret-mark model-m-0-1-1-c-0-2-2 :caret :forward nil nil)
        model-m-0-1-1-c-1-0-0 (textfield2/move-caret-mark model-m-0-1-1-c-0-2-3 :caret :forward nil nil)
        model-m-0-1-1-c-1-0-1 (textfield2/move-caret-mark model-m-0-1-1-c-1-0-0 :caret :forward nil nil)
        model-m-0-1-1-c-1-1-0 (textfield2/move-caret-mark model-m-0-1-1-c-1-0-1 :caret :forward nil nil)
        model-cm-1-1-1 (textfield2/move-caret-mark model-m-0-1-1-c-1-1-0 :caret-&-mark :forward nil nil)
        model-cm-1-1-2 (textfield2/move-caret-mark model-cm-1-1-1 :caret-&-mark :forward nil nil)
        model-c-1-1-2-m-1-1-3 (textfield2/move-caret-mark model-cm-1-1-2 :mark :forward nil nil)
        model-c-1-1-2-m-1-1-4 (textfield2/move-caret-mark model-c-1-1-2-m-1-1-3 :mark :forward nil nil)
        model-c-1-1-2-m-1-2-0 (textfield2/move-caret-mark model-c-1-1-2-m-1-1-4 :mark :forward nil nil)
        model-cm-1-1-1a (textfield2/move-caret-mark model-c-1-1-2-m-1-2-0 :caret-&-mark :backward nil nil)
        model-cm-1-1-2a (textfield2/move-caret-mark model-cm-1-1-1a :caret-&-mark :forward nil nil)
        model-cm-1-1-3 (textfield2/move-caret-mark model-cm-1-1-2a :caret-&-mark :forward nil nil)
        model-cm-1-1-4 (textfield2/move-caret-mark model-cm-1-1-3 :caret-&-mark :forward nil nil)
        model-cm-1-2-0 (textfield2/move-caret-mark model-cm-1-1-4 :caret-&-mark :forward nil nil)
        model-cm-1-2-1 (textfield2/move-caret-mark model-cm-1-2-0 :caret-&-mark :forward nil nil)
        model-cm-1-2-2 (textfield2/move-caret-mark model-cm-1-2-1 :caret-&-mark :forward nil nil)
        model-cm-2-0-0 (textfield2/move-caret-mark model-cm-1-2-2 :caret-&-mark :forward nil nil)
        model-cm-1-2-2a (textfield2/move-caret-mark model-cm-2-0-0 :caret-&-mark :backward nil nil)

        model-cm-2-0-1 (textfield2/move-caret-mark model-cm-2-0-0 :caret-&-mark :forward nil nil)
        model-m-2-0-1-c-0-2-1 (->
                                (textfield2/move-caret-mark model-cm-2-0-1 :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil)
                                (textfield2/move-caret-mark :caret :backward nil nil))]
    (test/is (= [0 0 1 0 0 1] (model->caret-mark-pos model-cm-0-0-1)))
    (test/is (= [1.0 nil nil] (model-line->caret-sel-coords model-cm-0-0-1 0)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-0-0-1 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-0-0-1 2)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-0-0-2))
    (test/is (= [0 0 2 0 0 2] (model->caret-mark-pos model-cm-0-0-2)))
    (test/is (= [2.0 nil nil] (model-line->caret-sel-coords model-cm-0-0-2 0)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-0-0-2 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-0-0-2 2)))

    (test/is (= model-cm-0-0-1 model-cm-0-0-1a))
    (test/is (= model-cm-0-0-2 model-cm-0-0-2a))
    (test/is (= [1.0 nil nil] (model-line->caret-sel-coords model-cm-0-0-1a 0)))
    (test/is (= [2.0 nil nil] (model-line->caret-sel-coords model-cm-0-0-2a 0)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-0-0-3))
    (test/is (= [0 0 3 0 0 3] (model->caret-mark-pos model-cm-0-0-3)))
    (test/is (= [3.0 nil nil] (model-line->caret-sel-coords model-cm-0-0-3 0)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-0-1-0))
    (test/is (= [0 1 0 0 1 0] (model->caret-mark-pos model-cm-0-1-0)))
    (test/is (= [4.0 nil nil] (model-line->caret-sel-coords model-cm-0-1-0 0)))

    (test/is (= model-cm-0-0-3 model-cm-0-0-3a))
    (test/is (= model-cm-0-1-0 model-cm-0-1-0a))
    (test/is (= [3.0 nil nil] (model-line->caret-sel-coords model-cm-0-0-3a 0)))
    (test/is (= [4.0 nil nil] (model-line->caret-sel-coords model-cm-0-1-0a 0)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-0-1-1))
    (test/is (= [0 1 1 0 1 1] (model->caret-mark-pos model-cm-0-1-1)))
    (test/is (= [5.0 nil nil] (model-line->caret-sel-coords model-cm-0-1-1 0)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-0-2-0))
    (test/is (= [0 2 0 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-0-2-0)))
    (test/is (= [6.0 5.0 6.0] (model-line->caret-sel-coords model-m-0-1-1-c-0-2-0 0)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-0-2-1))
    (test/is (= [0 2 1 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-0-2-1)))
    (test/is (= [7.0 5.0 7.0] (model-line->caret-sel-coords model-m-0-1-1-c-0-2-1 0)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-0-2-2))
    (test/is (= [0 2 2 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-0-2-2)))
    (test/is (= [8.0 5.0 8.0] (model-line->caret-sel-coords model-m-0-1-1-c-0-2-2 0)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-0-2-3))
    (test/is (= [0 2 3 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-0-2-3)))
    (test/is (= [9.0 5.0 9.0] (model-line->caret-sel-coords model-m-0-1-1-c-0-2-3 0)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-1-0-0))
    (test/is (= [1 0 0 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-1-0-0)))
    (test/is (= [nil 5.0 9.0] (model-line->caret-sel-coords model-m-0-1-1-c-1-0-0 0)))
    (test/is (= [0.0 nil nil] (model-line->caret-sel-coords model-m-0-1-1-c-1-0-0 1)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-1-0-1))
    (test/is (= [1 0 1 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-1-0-1)))
    (test/is (= [nil 5.0 9.0] (model-line->caret-sel-coords model-m-0-1-1-c-1-0-1 0)))
    (test/is (= [1.0 0.0 1.0] (model-line->caret-sel-coords model-m-0-1-1-c-1-0-1 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-m-0-1-1-c-1-0-1 2)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-1-1-0))
    (test/is (= [1 1 0 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-1-1-0)))
    (test/is (= [nil 5.0 9.0] (model-line->caret-sel-coords model-m-0-1-1-c-1-1-0 0)))
    (test/is (= [2.0 0.0 2.0] (model-line->caret-sel-coords model-m-0-1-1-c-1-1-0 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-m-0-1-1-c-1-1-0 2)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-1))
    (test/is (= [1 1 1 1 1 1] (model->caret-mark-pos model-cm-1-1-1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-1-1 0)))
    (test/is (= [3.0 nil nil] (model-line->caret-sel-coords model-cm-1-1-1 1)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-2))
    (test/is (= [1 1 2 1 1 2] (model->caret-mark-pos model-cm-1-1-2)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-1-2 0)))
    (test/is (= [4.0 nil nil] (model-line->caret-sel-coords model-cm-1-1-2 1)))

    (test/is (model-content-equal model-cm-0-0-1 model-c-1-1-2-m-1-1-3))
    (test/is (= [1 1 2 1 1 3] (model->caret-mark-pos model-c-1-1-2-m-1-1-3)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-c-1-1-2-m-1-1-3 0)))
    (test/is (= [4.0 4.0 5.0] (model-line->caret-sel-coords model-c-1-1-2-m-1-1-3 1)))

    (test/is (model-content-equal model-cm-0-0-1 model-c-1-1-2-m-1-1-4))
    (test/is (= [1 1 2 1 1 4] (model->caret-mark-pos model-c-1-1-2-m-1-1-4)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-c-1-1-2-m-1-1-4 0)))
    (test/is (= [4.0 4.0 6.0] (model-line->caret-sel-coords model-c-1-1-2-m-1-1-4 1)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-2a))
    (test/is (= [1 1 2 1 1 2] (model->caret-mark-pos model-cm-1-1-2a)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-1-2a 0)))
    (test/is (= [4.0 nil nil] (model-line->caret-sel-coords model-cm-1-1-2a 1)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-3))
    (test/is (= [1 1 3 1 1 3] (model->caret-mark-pos model-cm-1-1-3)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-1-3 0)))
    (test/is (= [5.0 nil nil] (model-line->caret-sel-coords model-cm-1-1-3 1)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-4))
    (test/is (= [1 1 4 1 1 4] (model->caret-mark-pos model-cm-1-1-4)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-1-4 0)))
    (test/is (= [6.0 nil nil] (model-line->caret-sel-coords model-cm-1-1-4 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-1-4 2)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-2-0))
    (test/is (= [1 2 0 1 2 0] (model->caret-mark-pos model-cm-1-2-0)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-2-0 0)))
    (test/is (= [7.0 nil nil] (model-line->caret-sel-coords model-cm-1-2-0 1)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-2-1))
    (test/is (= [1 2 1 1 2 1] (model->caret-mark-pos model-cm-1-2-1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-2-1 0)))
    (test/is (= [8.0 nil nil] (model-line->caret-sel-coords model-cm-1-2-1 1)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-2-2))
    (test/is (= [1 2 2 1 2 2] (model->caret-mark-pos model-cm-1-2-2)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-2-2 0)))
    (test/is (= [9.0 nil nil] (model-line->caret-sel-coords model-cm-1-2-2 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-2-2 2)))

    (test/is (= model-cm-1-1-1 model-cm-1-1-1a))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-2-0-0))
    (test/is (= [2 0 0 2 0 0] (model->caret-mark-pos model-cm-2-0-0)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-2-0-0 0)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-2-0-0 1)))
    (test/is (= [0.0 nil nil] (model-line->caret-sel-coords model-cm-2-0-0 2)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-2-2a))
    (test/is (= [1 2 2 1 2 2] (model->caret-mark-pos model-cm-1-2-2a)))
    (test/is (= model-cm-1-2-2 model-cm-1-2-2a))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-2-2a 0)))
    (test/is (= [9.0 nil nil] (model-line->caret-sel-coords model-cm-1-2-2a 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-2-2a 2)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-2-0-1-c-0-2-1))
    (test/is (= [0 2 1 2 0 1] (model->caret-mark-pos model-m-2-0-1-c-0-2-1)))
    (test/is (= [7.0 7.0 9.0] (model-line->caret-sel-coords model-m-2-0-1-c-0-2-1 0)))
    (test/is (= [nil 0.0 9.0] (model-line->caret-sel-coords model-m-2-0-1-c-0-2-1 1)))
    (test/is (= [nil 0.0 1.0] (model-line->caret-sel-coords model-m-2-0-1-c-0-2-1 2)))))

(test/deftest move-caret-mark-test-2
  (let [w 7
        model-before (test-wrap-lines [(tw "xyz ") (tw "|bb")] w)
        model-after (textfield2/move-caret-mark model-before :caret-&-mark :backward nil nil)]
    (test/is (= [0 0 3 0 0 3] (model->caret-mark-pos model-after)))))

(test/deftest move-caret-mark-test-3
  (let [w 7
        model-before (test-wrap-lines [(tw "xyz| ") (tw "bb")] w)
        model-after (textfield2/move-caret-mark model-before :caret-&-mark :forward nil nil)]
    (test/is (= [0 1 0 0 1 0] (model->caret-mark-pos model-after)))))

(test/deftest move-caret-mark-test-4
  (let [w 50
        model-before (test-wrap-lines [(tw (str "a|" \newline)) (tw "bb")] w)
        model-after (textfield2/move-caret-mark model-before :caret-&-mark :forward nil nil)
        model-after-1 (->
                        (textfield2/move-caret-mark model-after :caret-&-mark :forward nil nil)
                        (textfield2/move-caret-mark :caret-&-mark :backward nil nil)
                        (textfield2/move-caret-mark :caret-&-mark :forward nil nil)
                        (textfield2/move-caret-mark :caret-&-mark :backward nil nil)
                        (textfield2/move-caret-mark :caret-&-mark :backward nil nil)
                        )]
    (test/is (= [1 0 0 1 0 0] (model->caret-mark-pos model-after)))
    (test/is (= [0 0 1 0 0 1] (model->caret-mark-pos model-after-1)))))

(def move-home-end-model-before-0 (test-wrap-lines [(tw "a|a ") (tw (str "aa" \newline)) (tw (str \newline)) (tw "bb")] 50))

(test/deftest move-home-end-1
  (let [model-after (textfield2/move-caret-mark move-home-end-model-before-0 :caret-&-mark :home nil nil)]
    (test/is (= [0 0 0 0 0 0] (model->caret-mark-pos model-after)))))

(test/deftest move-home-end-2
  (let [model-after (textfield2/move-caret-mark move-home-end-model-before-0 :caret :home nil nil)]
    (test/is (= [0 0 0 0 0 1] (model->caret-mark-pos model-after)))))

(test/deftest move-home-end-3
  (let [model-after (textfield2/move-caret-mark move-home-end-model-before-0 :caret-&-mark :end nil nil)]
    (test/is (= [0 1 2 0 1 2] (model->caret-mark-pos model-after)))))

(test/deftest move-home-end-4
  (let [model-after (textfield2/move-caret-mark move-home-end-model-before-0 :caret :end nil nil)]
    (test/is (= [0 1 2 0 0 1] (model->caret-mark-pos model-after)))))

(def move-home-end-model-before-1
  (->
    (textfield2/move-caret-mark move-home-end-model-before-0 :caret-&-mark :end nil nil)
    (textfield2/move-caret-mark :caret-&-mark :forward nil nil)))

(test/deftest move-home-end-5
  (let [model-after-1 (textfield2/move-caret-mark move-home-end-model-before-1 :caret :home nil nil)
        model-after-2 (textfield2/move-caret-mark move-home-end-model-before-1 :caret :end nil nil)]
    (test/is (= move-home-end-model-before-1 model-after-1))
    (test/is (= move-home-end-model-before-1 model-after-2))))

(def move-home-end-model-before-2
  (->
    (textfield2/move-caret-mark move-home-end-model-before-1 :caret-&-mark :end nil nil)
    (textfield2/move-caret-mark :caret-&-mark :forward nil nil)
    (textfield2/move-caret-mark :caret-&-mark :forward nil nil)))

(test/deftest move-home-end-6
  (let [model-after (textfield2/move-caret-mark move-home-end-model-before-2 :caret-&-mark :end nil nil)]
    (test/is (= [2 0 2 2 0 2] (model->caret-mark-pos model-after)))))

(test/deftest move-home-end-7
  (let [model-after (textfield2/move-caret-mark move-home-end-model-before-2 :caret :end nil nil)]
    (test/is (= [2 0 2 2 0 1] (model->caret-mark-pos model-after)))))

(test/deftest has-selection?-test
  (let [model-cm-0-1-1 (test-wrap-lines [(tw "aa ") (tw "b|b")
                                               (tw "cc ") (tw "dd")] 5)
        model-m-0-1-1-c-0-1-2 (textfield2/move-caret-mark model-cm-0-1-1 :caret :forward nil nil)
        model-c-0-1-1-m-0-1-2 (textfield2/move-caret-mark model-cm-0-1-1 :mark :forward nil nil)
        model-c-0-1-1-m-1-0-0 (textfield2/move-caret-mark model-c-0-1-1-m-0-1-2 :mark :forward nil nil)
        model-c-0-1-1-m-1-0-1 (textfield2/move-caret-mark model-c-0-1-1-m-1-0-0 :mark :forward nil nil)
        model-c-0-1-1-m-1-0-2 (textfield2/move-caret-mark model-c-0-1-1-m-1-0-1 :mark :forward nil nil)
        model-c-0-1-1-m-1-0-3 (textfield2/move-caret-mark model-c-0-1-1-m-1-0-2 :mark :forward nil nil)
        model-c-0-1-1-m-1-1-0 (textfield2/move-caret-mark model-c-0-1-1-m-1-0-3 :mark :forward nil nil)
        model-c-0-1-1-m-1-1-1 (textfield2/move-caret-mark model-c-0-1-1-m-1-1-0 :mark :forward nil nil)
        model-cm-1-1-2 (textfield2/move-caret-mark model-c-0-1-1-m-1-1-1 :caret-&-mark :forward nil nil)
        model-c-1-1-1-m-1-1-2 (textfield2/move-caret-mark model-cm-1-1-2 :caret :backward nil nil)
        model-c-1-1-0-m-1-1-2 (textfield2/move-caret-mark model-c-1-1-1-m-1-1-2 :caret :backward nil nil)
        model-c-1-0-3-m-1-1-2 (textfield2/move-caret-mark model-c-1-1-0-m-1-1-2 :caret :backward nil nil)
        model-c-1-0-2-m-1-1-2 (textfield2/move-caret-mark model-c-1-0-3-m-1-1-2 :caret :backward nil nil)
        model-cm-1-0-1 (textfield2/move-caret-mark model-c-1-0-2-m-1-1-2 :caret-&-mark :backward nil nil)
        model-m-1-0-1-c-1-0-2 (textfield2/move-caret-mark model-cm-1-0-1 :caret :forward nil nil)
        model-m-1-0-1-c-1-0-3 (textfield2/move-caret-mark model-m-1-0-1-c-1-0-2 :caret :forward nil nil)
        model-m-1-0-1-c-1-1-0 (textfield2/move-caret-mark model-m-1-0-1-c-1-0-3 :caret :forward nil nil)
        model-m-1-0-1-c-1-1-1 (textfield2/move-caret-mark model-m-1-0-1-c-1-1-0 :caret :forward nil nil)]
    (test/is (false? (textfield2/has-selection? model-cm-0-1-1)))
    (test/is (textfield2/has-selection? model-m-0-1-1-c-0-1-2))
    (test/is (textfield2/has-selection? model-c-0-1-1-m-0-1-2))
    (test/is (textfield2/has-selection? model-c-0-1-1-m-1-0-0))
    (test/is (textfield2/has-selection? model-c-0-1-1-m-1-0-1))
    (test/is (textfield2/has-selection? model-c-0-1-1-m-1-0-2))
    (test/is (textfield2/has-selection? model-c-0-1-1-m-1-0-3))
    (test/is (textfield2/has-selection? model-c-0-1-1-m-1-1-0))
    (test/is (textfield2/has-selection? model-c-0-1-1-m-1-1-1))
    (test/is (false? (textfield2/has-selection? model-cm-1-1-2)))
    (test/is (textfield2/has-selection? model-c-1-1-1-m-1-1-2))
    (test/is (textfield2/has-selection? model-c-1-1-0-m-1-1-2))
    (test/is (textfield2/has-selection? model-c-1-0-3-m-1-1-2))
    (test/is (textfield2/has-selection? model-c-1-0-2-m-1-1-2))
    (test/is (false? (textfield2/has-selection? model-cm-1-0-1)))
    (test/is (textfield2/has-selection? model-m-1-0-1-c-1-0-2))
    (test/is (textfield2/has-selection? model-m-1-0-1-c-1-0-3))
    (test/is (textfield2/has-selection? model-m-1-0-1-c-1-1-0))
    (test/is (textfield2/has-selection? model-m-1-0-1-c-1-1-1))))

(test/deftest nosel-delete-test-1
  (let [w 7
        model-before (test-wrap-lines [(tw "aa ") (tw "b|b ")
                                             (tw "cc ")] w)
        model-after (textfield2/do-backspace model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "|b " ) (tw "cc ")] (:words (first (:lines model-after)))))))

(test/deftest nosel-delete-test-2
  (let [w 7
        model-before (test-wrap-lines [(tw "aa ") (tw "b|b ")
                                             (tw "cc ")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "b| " ) (tw "cc ")] (:words (first (:lines model-after)))))))

(test/deftest nosel-delete-test-3
  (let [w 7
        model-before (test-wrap-lines [(tw "xyz ") (tw "bb ")
                                             (tw "aa ") (tw "bb ")
                                             (tw "c|c ")] w)
        model-after (textfield2/do-backspace model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "bb " ) (tw "|c ")] (:words (second (:lines model-after)))))))

(test/deftest nosel-delete-test-4
  (let [w 7
        model-before (test-wrap-lines [(tw "xy|z ") (tw "bb ")
                                             (tw "aa ") (tw "bb ")
                                             (tw "cc ")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)]
    (test/is (= [(tw "xy| ") (tw "bb " )] (:words (first (:lines model-after)))))
    (test/is (= [(tw "aa ") (tw "bb " )] (:words (second (:lines model-after)))))
    (test/is (= [(tw "cc ")] (:words (nth (:lines model-after) 2))))))

(test/deftest nosel-delete-test-5
  (let [w 7
        model-before (test-wrap-lines [(tw "xyz ") (tw "|bb")] w)
        model-after (textfield2/do-backspace model-before w dummy-interop)]
    (test/is (= [(tw "xyz|bb")] (:words (first (:lines model-after)))))))


(test/deftest nosel-delete-test-6
  (let [w 7
        model-before (test-wrap-lines [(tw "xyz ") (tw "bb ")
                                             (tw "aa ") (tw "bb|b")
                                             (tw "cc ")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "bb|cc " )] (:words (second (:lines model-after)))))))

(test/deftest nosel-delete-test-7
  (let [w 7
        model-before (test-wrap-lines [(tw "aa ") (tw "bb ")
                                             (tw "|cc ")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "bb " ) (tw "|c ")] (:words (first (:lines model-after)))))))

(test/deftest nosel-delete-test-8
  (let [w 7
        model-before (test-wrap-lines [(tw "aa ") (tw "bb ")
                                             (tw "cc |")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "bb " ) (tw "cc |")] (concat (:words (first (:lines model-after))) (:words (second (:lines model-after))))))))

(test/deftest nosel-delete-test-9
  (let [w 7
        model-before (test-wrap-lines [(tw "a|")] w)
        model-after (textfield2/do-backspace model-before w dummy-interop)]
    (test/is (= [textfield2/empty-word-with-caret-&-mark] (:words (first (:lines model-after)))))
    (test/is (= 0 (:caret-line model-after)))
    (test/is (= 0 (:caret-word (first (:lines model-after)))))))

(test/deftest nosel-delete-test-10
  (let [w 7
        a-&-newline (tw (str "a" \newline))
        model-before (test-wrap-lines [a-&-newline
                                             (tw "b|")] w)
        model-after (textfield2/do-backspace model-before w dummy-interop)
        lines-after (:lines model-after)
        model-after-1 (textfield2/do-backspace model-after w dummy-interop)
        lines-after-1 (:lines model-after-1)
        ]
    (test/is (= 2 (count lines-after)))
    (test/is (= [a-&-newline] (:words (first (:lines model-after)))))
    (test/is (= [textfield2/empty-word-with-caret-&-mark] (:words (second (:lines model-after)))))
    (test/is (= 1 (count lines-after-1)))
    (test/is (= [(tw (str "a|"))] (:words (first (:lines model-after-1)))))))

(test/deftest nosel-delete-test-11
  (let [w 7
        a-&-newline (tw (str "a" \newline))
        model-before (test-wrap-lines [a-&-newline
                                             (tw " b|")] w)
        model-after (textfield2/do-backspace model-before w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= 2 (count lines-after)))
    (test/is (= [a-&-newline] (:words (first (:lines model-after)))))
    (test/is (= [(tw " |")] (:words (second (:lines model-after)))))))

(test/deftest nosel-delete-test-12
  (let [w 10
        model-before (test-wrap-lines [(tw (str "a|" \newline))
                                       (tw " ") (tw (str "b" \newline))
                                       (tw " ") (tw "b")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= 2 (count lines-after)))
    (test/is (= [(tw "a| ") (tw (str "b" \newline))] (:words (first (:lines model-after)))))
    (test/is (= [(tw " ") (tw "b")] (:words (second (:lines model-after)))))))

(test/deftest nosel-delete-test-13
  (let [w 3
        model-before (test-wrap-lines [(tw "ab ")
                                       (tw "|c ")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= 1 (count lines-after)))
    (test/is (= 0 (:caret-line model-after)))
    (test/is (= [(tw "ab | ")] (:words (first (:lines model-after)))))))

(test/deftest nosel-delete-test-14
  (let [w 30
        model-before (test-wrap-lines [(tw " ") (tw "a\n")
                                       (tw "| ") (tw "a\n")
                                       (tw " ") (tw "a\n")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= 3 (count lines-after)))
    (test/is (= 1 (:caret-line model-after)))
    (test/is (= [(tw "|a\n")] (:words (second (:lines model-after)))))))

(test/deftest nosel-delete-test-15
  (let [w 3
        model-before (test-wrap-lines [(tw " ") (tw "a\n")
                                       (tw "| ")
                                       (tw "aaa\n")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= 2 (count lines-after)))
    (test/is (= 1 (:caret-line model-after)))
    (test/is (= [(tw "|aaa\n")] (:words (second (:lines model-after)))))))

(test/deftest nosel-delete-test-16
  (let [w 3
        model-before (test-wrap-lines [(tw " ") (tw "a\n")
                                       (tw "|\n")
                                       (tw "")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= 2 (count lines-after)))
    (test/is (= 1 (:caret-line model-after)))
    (test/is (= [(tw " ") (tw "a\n")] (:words (first (:lines model-after)))))
    (test/is (= [textfield2/empty-word-with-caret-&-mark] (:words (second (:lines model-after)))))))

(test/deftest nosel-delete-test-17
  (let [w 30
        model-before (test-wrap-lines [(tw "| ")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= 1 (count lines-after)))
    (test/is (= 0 (:caret-line model-after)))
    (test/is (= [textfield2/empty-word-with-caret-&-mark] (:words (first (:lines model-after)))))))

(test/deftest nosel-delete-test-18
  (let [w 7
        glyphs (tg "xy\n  a bbbbbbbb cccc ")
        words (textfield2/make-words glyphs 10 w dummy-interop)
        model-before (test-wrap-lines words w)
        model-after (textfield2/do-delete model-before w dummy-interop)]
    (test/is (= [(tw "  ") (tw "a ")] (:words (nth (:lines model-before) 1))))
    (test/is (= [(tw "bbb|bbbb")] (:words (nth (:lines model-before) 2))))
    (test/is (= [(tw "b ")  (tw "cccc ")] (:words (nth (:lines model-before) 3))))
    (test/is (= [(tw "  ") (tw "a ")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "bbb|bbbb ")] (:words (nth (:lines model-after) 2))))
    (test/is (= [(tw "cccc ")] (:words (nth (:lines model-after) 3))))))

(test/deftest nosel-delete-test-19
  (let [w 7
        glyphs (tg "aaaaa bbbbbbbbbb")
        words (textfield2/make-words glyphs 5 w dummy-interop)
        model-before (test-wrap-lines words w)
        model-after (textfield2/do-delete model-before w dummy-interop)]
    (test/is (= [(tw "aaaaa| ")] (:words (nth (:lines model-before) 0))))
    (test/is (= [(tw "bbbbbbb")] (:words (nth (:lines model-before) 1))))
    (test/is (= [(tw "bbb")] (:words (nth (:lines model-before) 2))))
    (test/is (= 3 (count (:lines model-after))))
    (test/is (= 0 (:caret-line model-after)))
    (test/is (= 0 (:mark-line model-after)))
    (test/is (= [(tw "aaaaa|bb")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "bbbbbbb")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "b")] (:words (nth (:lines model-after) 2))))))

(test/deftest nosel-delete-test-20
  (let [w 3
        model-before (test-wrap-lines [(tw "aaa")
                                       (tw "aaa")
                                       (tw "aaa|")
                                       (tw "aa")] w)
        model-after (textfield2/do-delete model-before w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= 4 (count lines-after)))
    (test/is (= [3 0 0 3 0 0] (model->caret-mark-pos model-after)))
    (test/is (= [(tw "|a")] (:words (nth (:lines model-after) 3))))))

(test/deftest nosel-delete-test-21
  (let [w 3
        model-before (test-wrap-lines [(tw "aaa")
                                       (tw "aaa")
                                       (tw "aaa")
                                       (tw "|aa")] w)
        model-after (textfield2/do-backspace model-before w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= 4 (count lines-after)))
    (test/is (= [2 0 2 2 0 2] (model->caret-mark-pos model-after)))
    (test/is (= [(tw "aa|a")] (:words (nth (:lines model-after) 2))))
    (test/is (= [(tw "a")] (:words (nth (:lines model-after) 3))))))

(test/deftest sel-delete-test-1
  (let [w 7
        model (test-wrap-lines [(tw "aa ") (tw "|bb ")
                                      (tw "cc ")] w)
        model-before-cm (textfield2/move-caret-mark model :mark :forward nil nil)
        model-before-mc (textfield2/move-caret-mark model :caret :forward nil nil)
        model-after-cm (textfield2/do-delete model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete model-before-mc w dummy-interop)
        expected-words [(tw "aa ") (tw "|b " ) (tw "cc ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-2
  (let [w 7
        model (test-wrap-lines [(tw "aa| ") (tw "bb ")
                                      (tw "cc ")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil))
        model-before-mc (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after-cm (textfield2/do-delete model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete model-before-mc w dummy-interop)
        expected-words [(tw "aa|b ") (tw "cc ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-3
  (let [w 7
        model (test-wrap-lines [(tw "aa| ") (tw "bb ")
                                      (tw "cc ")] w)
        model-before-cm (textfield2/move-caret-mark model :mark :forward nil nil)
        model-before-mc (textfield2/move-caret-mark model :caret :forward nil nil)
        model-after-cm (textfield2/do-delete model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete model-before-mc w dummy-interop)
        expected-words [(tw "aa|bb ") (tw "cc ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-4
  (let [w 7
        model (test-wrap-lines [(tw "aa ") (tw "b|b ")
                                      (tw "cc ")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil))
        model-before-mc (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after-cm (textfield2/do-delete model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete model-before-mc w dummy-interop)
        expected-words [(tw "aa ") (tw "b|c ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-5
  (let [w 5
        model (test-wrap-lines [(tw "a|a ") (tw "b ")
                                      (tw "c ") (tw "dd ")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil))
        model-before-mc (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after-cm (textfield2/do-delete model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete model-before-mc w dummy-interop)
        expected-words [(tw "a|d ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-6
  (let [w 70
        model (test-wrap-lines [(tw "aa ") (tw (str "bb |" \newline))
                                (tw "cccc ")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil))
        model-before-mc (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after-cm (textfield2/do-delete model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete model-before-mc w dummy-interop)
        expected-words [(tw "aa ") (tw "bb ") (tw "|cc ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-7
  (let [w 7
        model (test-wrap-lines [(tw "aa ") (tw (str "bb |" \newline))
                                      (tw "cccc ")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil))
        model-before-mc (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after-cm (textfield2/do-delete model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete model-before-mc w dummy-interop)
        expected-words-0 [(tw "aa ") (tw "bb |")]
        expected-words-1 [(tw "cc ")]]
    (test/is (= expected-words-0 (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words-1 (:words (second (:lines model-after-cm)))))
    (test/is (= expected-words-0 (:words (first (:lines model-after-mc)))))
    (test/is (= expected-words-1 (:words (second (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-8
  (let [w 100
        model (test-wrap-lines [(tw "a ") (tw "|b ") (tw "c ") (tw "dd ")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil))
        model-before-mc (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after-cm (textfield2/do-delete model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete model-before-mc w dummy-interop)
        expected-words [(tw "a ") (tw "|d ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-9
  (let [w 50
        model (test-wrap-lines [(tw (str "|aa" \newline))
                                (tw (str "bb" \newline))
                                (tw "cc")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :caret :down nil nil)
                          (textfield2/move-caret-mark :caret :down nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        expected-words [textfield2/empty-word-with-caret-&-mark]]

    (test/is (= [2 0 2 0 0 0] (model->caret-mark-pos model-before-cm)))

    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest sel-delete-test-10
  (let [w 50
        model (test-wrap-lines [(tw "|a ") (tw "b")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        expected-words [(tw "|b")]]

    (test/is (= [0 1 0 0 0 0] (model->caret-mark-pos model-before-cm)))

    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest sel-delete-test-10a
  (let [w 50
        model (test-wrap-lines [(tw "f ") (tw "|a ") (tw "b")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        expected-words [(tw "f |")]]

    (test/is (= [0 2 1 0 1 0] (model->caret-mark-pos model-before-cm)))

    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest sel-delete-test-10b
  (let [w 50
        model (test-wrap-lines [(tw "|a ") (tw "b") (tw "l ")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        expected-words [(tw "|l ")]]

    (test/is (= [0 1 1 0 0 0] (model->caret-mark-pos model-before-cm)))

    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest sel-delete-test-10c
  (let [w 50
        model (test-wrap-lines [(tw "|a ") (tw "b")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        expected-words [textfield2/empty-word-with-caret-&-mark]]

    (test/is (= [0 1 1 0 0 0] (model->caret-mark-pos model-before-cm)))

    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest sel-delete-test-11
  (let [w 50
        model (test-wrap-lines [(tw "|a")] w)
        model-before-cm (textfield2/move-caret-mark model :caret :forward nil nil)
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        expected-words [textfield2/empty-word-with-caret-&-mark]]
    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest sel-delete-test-12
  (let [w 50
        model (test-wrap-lines [(tw (str "|a" \newline))
                                (tw "a ") (tw "a")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        expected-words [textfield2/empty-word-with-caret-&-mark]]
    (test/is (= [1 1 1 0 0 0] (model->caret-mark-pos model-before-cm)))
    (test/is (= expected-words (:words (first (:lines model-after)))))))

(test/deftest sel-delete-test-13
  (let [w 50
        model (test-wrap-lines [(tw "    ") (tw "aaa ") (tw "bbbbb\n")
                                (tw "  |  ") (tw "aa ") (tw "bbbb\n")
                                (tw "    ") (tw "aa ") (tw "bbb\n")
                                (tw "  \n")
                                (tw "xx \n")
                                ] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :caret :down nil nil)
                          (textfield2/move-caret-mark :caret :down nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        ]
    (test/is (= [3 0 2 1 0 2] (model->caret-mark-pos model-before-cm)))
    (test/is (= [(tw "    ") (tw "aaa ") (tw (str "bbbbb" \newline))] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "  |\n")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "xx \n")] (:words (nth (:lines model-after) 2))))))

(test/deftest sel-delete-test-14
  (let [w 50
        model (test-wrap-lines [(tw "abc ") (tw "def\n")
                                (tw "    ") (tw "|rge\n")
                                (tw "aaa ") (tw "bbbb\n")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        ]
    (test/is (= [1 1 0 1 1 3] (model->caret-mark-pos model-before-cm)))
    (test/is (= [(tw "abc ") (tw "def\n")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "    |\n")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "aaa ") (tw "bbbb\n")] (:words (nth (:lines model-after) 2))))))

(test/deftest sel-delete-test-15
  (let [w 50
        model (test-wrap-lines [(tw "    ") (tw "|rge\n")
                                (tw "aaa ") (tw "bbbb\n")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)
        ]
    (test/is (= [0 1 0 0 1 3] (model->caret-mark-pos model-before-cm)))
    (test/is (= [(tw "    |\n")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "aaa ") (tw "bbbb\n")] (:words (nth (:lines model-after) 1))))))

(test/deftest sel-delete-test-16
  (let [w 250
        model (test-wrap-lines [(tw "aaa\n")
                                (tw "bbb\n")
                                (tw "ccc|\n")
                                (tw "ddd")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :home nil nil)
                          (textfield2/move-caret-mark :mark :up nil nil))
        model-after (textfield2/do-delete model-before-cm w dummy-interop)]
    (test/is (= [1 0 0 2 0 3] (model->caret-mark-pos model-before-cm)))
    (test/is (= 3 (count (:lines model-after))))
    (test/is (= [(tw "aaa\n")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "|\n")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "ddd")] (:words (nth (:lines model-after) 2))))
    ))

(test/deftest glyphs->model-test-1
  (let [w 50
        model (test-wrap-lines [(tw "|a")] w)
        glyphs (tg "xyz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)]
    (test/is (= [(tw "xyz|a")] (:words (first (:lines model-after)))))))

(test/deftest glyphs->model-test-2
  (let [w 50
        model (test-wrap-lines [(tw "a|")] w)
        glyphs (tg "xyz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)]
    (test/is (= [(tw "axyz|")] (:words (first (:lines model-after)))))))

(test/deftest glyphs->model-test-3
  (let [w 50
        model textfield2/empty-model
        glyphs (tg "xyz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)]
    (test/is (= [(tw "xyz|")] (:words (first (:lines model-after)))))))

(test/deftest glyphs->model-test-4
  (let [w 50
        model (test-wrap-lines [(tw "|a")] w)
        glyphs (tg "x yz ")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)]
    (test/is (= [(tw "x ") (tw "yz ") (tw "|a")] (:words (first (:lines model-after)))))))

(test/deftest glyphs->model-test-5
  (let [w 50
        model (test-wrap-lines [(tw "a|")] w)
        glyphs (tg "x yz ")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)]
    (test/is (= [(tw "ax ") (tw "yz |")] (:words (first (:lines model-after)))))))

(test/deftest glyphs->model-test-6
  (let [w 50
        model textfield2/empty-model
        glyphs (tg "x yz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)]
    (test/is (= [(tw "x ") (tw "yz|")] (:words (first (:lines model-after)))))))

(test/deftest glyphs->model-test-7
  (let [w 3
        model (test-wrap-lines [(tw "|a")] w)
        glyphs (tg "xx yz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "xx ")] (:words (first lines-after))))
    (test/is (= [(tw "yz|a")] (:words (second lines-after))))))

(test/deftest glyphs->model-test-8
  (let [w 3
        model (test-wrap-lines [(tw "a|")] w)
        glyphs (tg "cc yz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "acc ")] (:words (first lines-after))))
    (test/is (= [(tw "yz|")] (:words (second lines-after))))))

(test/deftest glyphs->model-test-9
  (let [w 3
        model (test-wrap-lines [(tw "a|")] w)
        glyphs (tg "ccc yz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "acc")] (:words (first lines-after))))
    (test/is (= [(tw "c ")] (:words (second lines-after))))
    (test/is (= [(tw "yz|")] (:words (nth lines-after 2))))))

(test/deftest glyphs->model-test-10
  (let [w 3
        model textfield2/empty-model
        glyphs (tg "cccc yz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "ccc")] (:words (first lines-after))))
    (test/is (= [(tw "c ")] (:words (second lines-after))))
    (test/is (= [(tw "yz|")] (:words (nth lines-after 2))))))

(test/deftest glyphs->model-test-11
  (let [w 50
        model (test-wrap-lines [(tw "|a")] w)
        glyphs (tg "xx\nyz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "xx\n")] (:words (first lines-after))))
    (test/is (= [(tw "yz|a")] (:words (second lines-after))))))

(test/deftest glyphs->model-test-12
  (let [w 50
        model (test-wrap-lines [(tw "a|")] w)
        glyphs (tg "cc \nyz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw (str "acc \n"))] (:words (first lines-after))))
    (test/is (= [(tw "yz|")] (:words (second lines-after))))))

(test/deftest glyphs->model-test-13
  (let [w 3
        model (test-wrap-lines [(tw "a|")] w)
        glyphs (tg "ccc \nyz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "acc")] (:words (first lines-after))))
    (test/is (= [(tw "c \n")] (:words (second lines-after))))
    (test/is (= [(tw "yz|")] (:words (nth lines-after 2))))))

(test/deftest glyphs->model-test-14
  (let [w 3
        model textfield2/empty-model
        glyphs (tg "cccc\nyz")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "ccc")] (:words (first lines-after))))
    (test/is (= [(tw "c\n")] (:words (second lines-after))))
    (test/is (= [(tw "yz|")] (:words (nth lines-after 2))))))

(test/deftest glyphs->model-test-15
  (let [w 5
        model (test-wrap-lines [(tw "aa ") (tw "bb ")
                                (tw "cc |\n")
                                (tw "dddd ") (tw "efg")] w)
        glyphs (tg "uuuu\nvvv ww")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "aa ") (tw "bb ")] (:words (nth lines-after 0))))
    (test/is (= [(tw "cc ")] (:words (nth lines-after 1))))
    (test/is (= [(tw "uuuu\n")] (:words (nth lines-after 2))))
    (test/is (= [(tw "vvv ")] (:words (nth lines-after 3))))
    (test/is (= [(tw "ww|\n")] (:words (nth lines-after 4))))
    (test/is (= [(tw "dddd ")] (:words (nth lines-after 5))))
    (test/is (= [(tw "efg")] (:words (nth lines-after 6))))))

(test/deftest glyphs->model-test-16
  (let [w 5
        model (test-wrap-lines [(tw "aa ") (tw "bb ")
                                (tw "cc |\n")
                                (tw "dddd ") (tw "efg")] w)
        glyphs (tg "uuuu\nvvv ww\n")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "aa ") (tw "bb ")] (:words (nth lines-after 0))))
    (test/is (= [(tw "cc ")] (:words (nth lines-after 1))))
    (test/is (= [(tw "uuuu\n")] (:words (nth lines-after 2))))
    (test/is (= [(tw "vvv ")] (:words (nth lines-after 3))))
    (test/is (= [(tw "ww\n")] (:words (nth lines-after 4))))
    (test/is (= [(tw "|\n")] (:words (nth lines-after 5))))
    (test/is (= [(tw "dddd ")] (:words (nth lines-after 6))))
    (test/is (= [(tw "efg")] (:words (nth lines-after 7))))))

(test/deftest glyphs->model-test-17
  (let [w 50
        model textfield2/empty-model
        glyphs (tg "/*\n*")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)]
    (test/is (= [(tw "/*\n")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "*|")] (:words (nth (:lines model-after) 1))))))

(test/deftest glyphs->model-test-18
  (let [w 50
        model textfield2/empty-model
        glyphs (tg "/*\n *")
        model-after (textfield2/glyphs->model model glyphs w dummy-interop)]
    (test/is (= [(tw "/*\n")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw " ") (tw "*|")] (:words (nth (:lines model-after) 1))))))

(test/deftest glyphs->model-test-19
  (let [w 50
        model (test-wrap-lines [(tw "aa\n")
                                (tw "bb|")] w)
        model-before (->
                       (textfield2/move-caret-mark model :caret :backward nil nil)
                       (textfield2/move-caret-mark :caret :backward nil nil))
        glyphs (tg "aa")
        model-after (textfield2/glyphs->model model-before glyphs w dummy-interop)
        lines-after (:lines model-after)]
    (test/is (= [(tw "aa\n")] (:words (nth lines-after 0))))
    (test/is (= [(tw "aa|")] (:words (nth lines-after 1))))))

(test/deftest glyphs->model-test-20
  (let [w 5
        words [(tw "a |") (tw "bbbbb")]
        model-before (test-wrap-lines words w)
        glyphs (tg "bb")
        model-after (textfield2/glyphs->model model-before glyphs w dummy-interop)]
    (test/is (= [(tw "a |")] (:words (nth (:lines model-before) 0))))
    (test/is (= [(tw "bbbbb")] (:words (nth (:lines model-before) 1))))
    (test/is (= [(tw "a ")] (:words (nth (:lines model-after) 0))))
    (test/is (= [(tw "bb|bbb")] (:words (nth (:lines model-after) 1))))
    (test/is (= [(tw "bb")] (:words (nth (:lines model-after) 2))))))

(test/deftest x->pos-in-line-test
  (let [w 7
        source-word (twv "|1321")
        model (test-wrap-lines [source-word] w)
        lines (:lines model)
        line (nth lines 0)
        words (:words line)
        word (nth words 0)]
    (test/is (= 1 (count lines)))
    (test/is (= 1 (count words)))
    (test/is (= 0 (textfield2/x->pos-in-word word 0.0)))
    (test/is (= 0 (textfield2/x->pos-in-word word 0.49)))
    (test/is (= 1 (textfield2/x->pos-in-word word 0.51)))
    (test/is (= 1 (textfield2/x->pos-in-word word 1.0)))
    (test/is (= 1 (textfield2/x->pos-in-word word 2.49)))
    (test/is (= 2 (textfield2/x->pos-in-word word 2.51)))
    (test/is (= 2 (textfield2/x->pos-in-word word 4.0)))
    (test/is (= 2 (textfield2/x->pos-in-word word 4.99)))
    (test/is (= 3 (textfield2/x->pos-in-word word 5.01)))
    (test/is (= 3 (textfield2/x->pos-in-word word 6.0)))
    (test/is (= 3 (textfield2/x->pos-in-word word 6.49)))
    (test/is (= 4 (textfield2/x->pos-in-word word 6.51)))
    (test/is (= 4 (textfield2/x->pos-in-word word 7.0)))
    (test/is (= 4 (textfield2/x->pos-in-word word 8.0)))))

(test/deftest x->pos-in-line-test-1
  (let [w 50
        model (test-wrap-lines [(tw "|aa ") (tw "bbbbb ") (tw "ccc")] w)
        lines (:lines model)
        line (nth lines 0)]
    (test/is (= 1 (count lines)))
    (test/is (= 0 (textfield2/x->pos-in-line line 0.0)))
    (test/is (= 0 (textfield2/x->pos-in-line line 1.0)))
    (test/is (= 0 (textfield2/x->pos-in-line line 2.4)))
    (test/is (= 0 (textfield2/x->pos-in-line line 2.5)))
    (test/is (= 1 (textfield2/x->pos-in-line line 2.51)))
    (test/is (= 1 (textfield2/x->pos-in-line line 2.9)))
    (test/is (= 1 (textfield2/x->pos-in-line line 3.0)))
    (test/is (= 1 (textfield2/x->pos-in-line line 3.1)))
    (test/is (= 1 (textfield2/x->pos-in-line line 8.49)))
    (test/is (= 2 (textfield2/x->pos-in-line line 8.51)))
    (test/is (= 2 (textfield2/x->pos-in-line line 8.9)))
    (test/is (= 2 (textfield2/x->pos-in-line line 9.0)))
    (test/is (= 2 (textfield2/x->pos-in-line line 10.5)))
    (test/is (= 2 (textfield2/x->pos-in-line line 12.0)))
    (test/is (= 2 (textfield2/x->pos-in-line line 13.0)))))

(test/deftest move-up-down-test-1
  (let [w 7
        model-cm-1-0-4 (test-wrap-lines [(tw "aa ") (tw "bb ")
                                               (tw "cccc| ")
                                               (tw "dddd ")
                                               (tw "eee ") (tw "ff ")] w)
        model-cm-0-1-1 (textfield2/move-caret-mark model-cm-1-0-4 :caret-&-mark :up nil nil)
        model-cm-1-0-4a (textfield2/move-caret-mark model-cm-0-1-1 :caret-&-mark :down nil nil)
        model-c-2-0-4-m-1-0-4 (textfield2/move-caret-mark model-cm-1-0-4a :caret :down nil nil)
        model-c-3-1-1-m-1-0-4 (textfield2/move-caret-mark model-c-2-0-4-m-1-0-4 :caret :down nil nil)
        model-c-0-1-1-m-1-0-4 (->
                                (textfield2/move-caret-mark model-cm-1-0-4a :caret :up nil nil)
                                (textfield2/move-caret-mark :caret :up nil nil))]

    (test/is (= [0 1 1 0 1 1] (model->caret-mark-pos model-cm-0-1-1)))
    (test/is (= [4.0 nil nil] (model-line->caret-sel-coords model-cm-0-1-1 0)))

    (test/is (= [1 0 4 1 0 4] (model->caret-mark-pos model-cm-1-0-4a)))
    (test/is (model-content-equal model-cm-1-0-4 model-cm-1-0-4a))

    (test/is (= [2 0 4 1 0 4] (model->caret-mark-pos model-c-2-0-4-m-1-0-4)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-c-2-0-4-m-1-0-4 0)))
    (test/is (= [nil 4.0 5.0] (model-line->caret-sel-coords model-c-2-0-4-m-1-0-4 1)))
    (test/is (= [4.0 0.0 4.0] (model-line->caret-sel-coords model-c-2-0-4-m-1-0-4 2)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-c-2-0-4-m-1-0-4 3)))

    (test/is (= [3 1 0 1 0 4] (model->caret-mark-pos model-c-3-1-1-m-1-0-4)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-c-3-1-1-m-1-0-4 0)))
    (test/is (= [nil 4.0 5.0] (model-line->caret-sel-coords model-c-3-1-1-m-1-0-4 1)))
    (test/is (= [nil 0.0 5.0] (model-line->caret-sel-coords model-c-3-1-1-m-1-0-4 2)))
    (test/is (= [4.0 0.0 4.0] (model-line->caret-sel-coords model-c-3-1-1-m-1-0-4 3)))

    (test/is (= [0 1 1 1 0 4] (model->caret-mark-pos model-c-0-1-1-m-1-0-4)))
    (test/is (= [4.0 4.0 6.0] (model-line->caret-sel-coords model-c-0-1-1-m-1-0-4 0)))
    (test/is (= [nil 0.0 4.0] (model-line->caret-sel-coords model-c-0-1-1-m-1-0-4 1)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-c-0-1-1-m-1-0-4 2)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-c-0-1-1-m-1-0-4 3)))))

(test/deftest move-up-down-test-2
  (let [w 7
        model-cm-1-0-4 (test-wrap-lines [(tw (str "aaa" \newline)) (tw "bbbb|")] w)
        model-cm-0-0-3 (textfield2/move-caret-mark model-cm-1-0-4 :caret-&-mark :up nil nil)]

    (test/is (= [0 0 3 0 0 3] (model->caret-mark-pos model-cm-0-0-3)))
    (test/is (= [3.0 nil nil] (model-line->caret-sel-coords model-cm-0-0-3 0)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-0-0-3 1)))
    ))

(test/deftest move-up-down-test-3
  (let [w 4
        model-cm-0-0-4 (test-wrap-lines [(tw "bbbb|") (tw (str "aaa" \newline))] w)
        model-cm-1-0-3 (textfield2/move-caret-mark model-cm-0-0-4 :caret-&-mark :down nil nil)]

    (test/is (= [1 0 3 1 0 3] (model->caret-mark-pos model-cm-1-0-3)))
    (test/is (= [nil nil nil] (model-line->caret-sel-coords model-cm-1-0-3 0)))
    (test/is (= [3.0 nil nil] (model-line->caret-sel-coords model-cm-1-0-3 1)))))

(test/deftest page-up-down-test-1
  (let [w 4
        model-before (test-wrap-lines [(tw "0000")
                                       (tw "1111|")
                                       (tw "2222")
                                       (tw "3333")
                                       (tw "4444")
                                       (tw "5555")
                                       (tw "6666")
                                       (tw "7777")
                                       (tw "8888")] w)
        model-c3 (textfield2/move-caret-mark model-before :caret-&-mark :page-down 3 nil)
        model-c5 (textfield2/move-caret-mark model-c3 :caret-&-mark :page-down 3 nil)
        model-c7 (textfield2/move-caret-mark model-c5 :caret-&-mark :page-down 3 nil)
        model-c8 (textfield2/move-caret-mark model-c7 :caret-&-mark :page-down 3 nil)
        model-c6 (textfield2/move-caret-mark model-c8 :caret-&-mark :page-up 3 nil)
        model-c4 (textfield2/move-caret-mark model-c6 :caret-&-mark :page-up 3 nil)
        model-c2 (textfield2/move-caret-mark model-c4 :caret-&-mark :page-up 3 nil)
        model-c0 (textfield2/move-caret-mark model-c2 :caret-&-mark :page-up 3 nil)]

    (test/is (= [3 0 4 3 0 4] (model->caret-mark-pos model-c3)))
    (test/is (= [5 0 4 5 0 4] (model->caret-mark-pos model-c5)))
    (test/is (= [7 0 4 7 0 4] (model->caret-mark-pos model-c7)))
    (test/is (= [8 0 4 8 0 4] (model->caret-mark-pos model-c8)))
    (test/is (= [6 0 4 6 0 4] (model->caret-mark-pos model-c6)))
    (test/is (= [4 0 4 4 0 4] (model->caret-mark-pos model-c4)))
    (test/is (= [2 0 4 2 0 4] (model->caret-mark-pos model-c2)))
    (test/is (= [0 0 4 0 0 4] (model->caret-mark-pos model-c0)))))

(test/deftest page-up-down-test-2
  (let [w 4
        model-before (test-wrap-lines [(tw "0000")
                                       (tw "1111|")
                                       (tw "2222")] w)
        model-c2 (textfield2/move-caret-mark model-before :caret :page-down 3 nil)
        model-c0 (textfield2/move-caret-mark model-c2 :caret :page-up 3 nil)]

    (test/is (= [2 0 4 1 0 4] (model->caret-mark-pos model-c2)))
    (test/is (= [0 0 4 1 0 4] (model->caret-mark-pos model-c0)))))

;;;
;;; Sanity checks
;;;

(test/deftest check-spaces-after-space-test-1
  (let [words [(tw "abc ") (tw "  ")]]
    (test/is (thrown? IllegalStateException (textfield2/check-spaces-after-space words 1)))))

(test/deftest check-spaces-after-space-test-2
  (let [words [(tw "abc ") (tw "  \n")]]
    (test/is (thrown? IllegalStateException (textfield2/check-spaces-after-space words 1)))))

(test/deftest check-unattached-linebreak-test
  (let [words [(tw "abc ") (tw "\n")]]
    (test/is (thrown? IllegalStateException (textfield2/check-unattached-linebreak words 1)))))

(test/deftest check-delimiter-after-linebreak-test-1
  (let [words [(tw "abc ") (tw "abc\n ")]]
    (test/is (thrown? IllegalStateException (textfield2/check-delimiter-after-linebreak words 1)))))

(test/deftest check-delimiter-after-linebreak-test-2
  (let [words [(tw "abc \n  ") (tw "abc")]]
    (test/is (thrown? IllegalStateException (textfield2/check-delimiter-after-linebreak words 0)))))

(test/deftest check-no-delimiter-at-word-end-test
  (let [words [(tw "abc") (tw "abc ")]]
    (test/is (thrown? IllegalStateException (textfield2/check-no-delimiter-at-word-end words 0)))))

(test/deftest check-inconsistent-cm-test
  (let [model textfield2/empty-model]
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc model :caret-line nil))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc model :caret-line -1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc model :caret-line 1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc model :mark-line nil))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc model :mark-line -1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc model :mark-line 1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :caret-word] nil))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :caret-word] -1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :caret-word] 1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :mark-word] nil))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :mark-word] -1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :mark-word] 1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :words 0 :caret-pos] nil))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :words 0 :caret-pos] -1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :words 0 :caret-pos] 1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :words 0 :mark-pos] nil))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :words 0 :mark-pos] -1))))
    (test/is (thrown? IllegalStateException (textfield2/check-inconsistent-cm (assoc-in model [:lines 0 :words 0 :mark-pos] 1))))))

;;;
;;; Live tests
;;;

;(test/deftest type-text-test
;  (let [initial-size (m/defpoint 6 4)
;        text-component (fg/defcomponent textfield2/textfield :text
;                                        {:clip-size initial-size
;                                         :evolvers {:clip-size (fg/accessorfn (get-property [] :clip-size))}})
;        root (fg/defcomponent
;               panel/panel
;               :main
;               {:clip-size  initial-size}
;               text-component)
;
;        container (fgtest/init-container root)
;        ]
;
;    (fgtest/wait-table-cell-property container [:main] [0 0] :atomic-state (fn [as] (not (:selected as))))
;    ))