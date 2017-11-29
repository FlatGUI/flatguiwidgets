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
            [flatgui.test :as fgtest])
  (:import (flatgui.core.engine IResultCollector Container ClojureContainerParser)
           (flatgui.core IFGInteropUtil)
           (java.awt.event KeyEvent)
           (flatgui.widgets.textfield2 Word)))

(def dummy-interop
  (proxy [IFGInteropUtil] []
    (getStringWidth [str _font] (.length str))
    (getFontHeight [_font] 1)
    (getFontAscent [_font] 0)
    ))

(defn test-glyph
  ([data w h] (textfield2/glyph :test data {:w (double w) :h (double h)}))
  ([w h] (test-glyph nil w h)))

(defn test-sized-glyph
  ([data w h]
   (let [g (textfield2/glyph :test data {:w (double w) :h (double h)})]
     (assoc g :size (textfield2/glyph-size g dummy-interop))))
  ([w h] (test-sized-glyph nil w h)))

(defn test-sized-char-glyph [c]
  (let [g (textfield2/char-glyph c)]
    (assoc g :size (textfield2/glyph-size g dummy-interop))))

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

(defn test-model [model expected-lines expected-total-word-widths expected-caret-line expected-caret-word expected-line-heights]
  (let [model-lines (:lines model)
        lines (mapv :words model-lines)]
    (test/is (= expected-lines (textfield2/lines->strings lines)))
    (test/is (= expected-total-word-widths (textfield2/lines->total-word-widths lines)))
    (if expected-caret-line (test/is (= expected-caret-line (:caret-line model))))
    (if expected-caret-word (test/is (= expected-caret-word (:caret-word (nth model-lines (:caret-line model))))))
    (if expected-line-heights (test/is (= expected-line-heights (textfield2/lines->line-heights model-lines))))))

(defn test-words [words w expected-lines expected-total-word-widths expected-caret-line expected-caret-word]
  (let [model (textfield2/wrap-lines words w)]
    (test-model model expected-lines expected-total-word-widths expected-caret-line expected-caret-word nil)))

(defn test-lines [text w expected-lines expected-total-word-widths]
  (let [glyphs (map textfield2/char-glyph text)
        words (textfield2/glyphs->words glyphs w dummy-interop)]
    (test-words words w expected-lines expected-total-word-widths nil nil)))

(test/deftest wrap-test

  (test-lines
    "The quick brown fox jumps over the lazy dog"
    9
    [["The" "quick"] ["brown" "fox"] ["jumps"] ["over" "the"] ["lazy" "dog"]]
    [[4.0 6.0] [6.0 4.0] [6.0] [5.0 4.0] [5.0 3.0]])

  (test-lines "11 22" 2 [["11"] ["22"]] [[3.0] [2.0]])
  (test-lines "11 22 " 2 [["11"] ["22"]] [[3.0] [3.0]])
  (test-lines "11 22 3" 2 [["11"] ["22"] ["3"]] [[3.0] [3.0] [1.0]])
  (test-lines "11 22 33 44" 5 [["11" "22"] ["33" "44"]] [[3.0 3.0] [3.0 2.0]])
  (test-lines "11 22 33 44 " 5 [["11" "22"] ["33" "44"]] [[3.0 3.0] [3.0 3.0]])
  (test-lines "11 22 33 44   " 5 [["11" "22"] ["33" "44"]] [[3.0 3.0] [3.0 5.0]])
  (test-lines "11 22 33 44  " 5 [["11" "22"] ["33" "44"]] [[3.0 3.0] [3.0 4.0]])
  (test-lines "11 22  33 44  " 5 [["11" "22"] ["33" "44"]] [[3.0 4.0] [3.0 4.0]])
  (test-lines "11 22   33 44  " 5 [["11" "22"] ["33" "44"]] [[3.0 5.0] [3.0 4.0]])
  (test-lines "11 22 33 44   " 5 [["11" "22"] ["33" "44"]] [[3.0 3.0] [3.0 5.0]])
  (test-lines "11 22 33 44     " 5 [["11" "22"] ["33" "44"]] [[3.0 3.0] [3.0 7.0]])
  (test-lines "11 22 33 44      " 5 [["11" "22"] ["33" "44"]] [[3.0 3.0] [3.0 8.0]])
  (test-lines " 11 22" 2 [[""] ["11"] ["22"]] [[1.0] [3.0] [2.0]])
  (test-lines " 11 22" 4 [["" "11"] ["22"]] [[1.0 3.0] [2.0]])
  (test-lines " 111 22" 2 [[""] ["11"] ["1"] ["22"]] [[1.0] [2.0] [2.0] [2.0]])
  (test-lines "  11 22" 2 [[""] ["11"] ["22"]] [[2.0] [3.0] [2.0]])

  ;;;; TODO this is wrong. Should be like what's commented out
  ;(test-lines "   11 22" 2 [[""] ["11"] ["22"]] [[3.0] [3.0] [2.0]])
  (test-lines "   11 22" 2 [[""] [""] ["11"] ["22"]] [[2.0] [1.0] [3.0] [2.0]])
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (test-lines "11  22" 2 [["11"] ["22"]] [[4.0] [2.0]])
  (test-lines "11   22" 2 [["11"] ["22"]] [[5.0] [2.0]])
  (test-lines "11    22" 2 [["11"] ["22"]] [[6.0] [2.0]])
  (test-lines "  11  22  " 2 [[""] ["11"] ["22"]] [[2.0] [4.0] [4.0]])

  ;; TODO Should be like what's commented out
  ;(test-lines "   11   22   " 2 [[""] ["11"] ["22"]] [[3.0] [5.0] [5.0]])
  (test-lines "   11   22   " 2 [[""] [""] ["11"] ["22"]] [[2.0] [1.0] [5.0] [5.0]])

  ;(test-lines "  11   22   " 2 ["  " "11   " "22   "])
  ;(test-lines "   11  22   " 2 ["   " "11  " "22   "])
  ;(test-lines "   11   22  " 2 ["   " "11   " "22  "])
  ;(test-lines " 11   22   " 2 [" 1" "1   " "22   "])
  ;(test-lines "   11 22   " 2 ["   " "11 " "22   "])
  ;(test-lines "   11   22 " 2 ["   " "11   " "22 "])
  )

(defn tw [s]
  (let [caret-pos (.indexOf s "|")
        s-clean (.replace s "|" "")
        w-total (double (.length s-clean))
        trailing-space-count (count (take-while #(= % \space) (reverse s-clean)))
        w-content (- w-total trailing-space-count)
        glyphs (mapv test-sized-char-glyph s-clean)
        result-caret-pos (if (>= caret-pos 0) caret-pos)]
    (Word. glyphs result-caret-pos result-caret-pos w-content w-total 1.0)))

(test/deftest wrap-test2

  (test-words
    [(tw "The ") (tw "quick ")
     (tw "brown ") (tw "fox ")
     (tw "ju|mps ")
     (tw "over ") (tw "the ")
     (tw "lazy ") (tw "dog")]
    9
    [["The" "quick"] ["brown" "fox"] ["jumps"] ["over" "the"] ["lazy" "dog"]]
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
    [["The" "quick"] ["brown" "fox"] ["jumps"] ["over" "the"] ["lazy" "dog"]]
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
    [["The" "quick"] ["brown" "fox"] ["jumps"] ["over" "the"] ["lazy" "dog"]]
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
    [["The" "quick"] ["brown" "fox"] ["jumps"] ["over" "the"] ["lazy" "dog"]]
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
    [["The" "quick"] ["brown" "fox"] ["jumps"] ["over" "the"] ["lazy" "dog"]]
    [[4.0 6.0] [6.0 4.0] [6.0] [5.0 4.0] [5.0 3.0]]
    4
    1)

  )

(test/deftest insert-symbol-test-1
  (let [words [(tw "T|he ") (tw "quick ")
               (tw "brown ") (tw "fox ")
               (tw "jumps ")
               (tw "over ") (tw "the ")
               (tw "lazy ") (tw "dog")]
        model-before (textfield2/wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \Z) 9 dummy-interop)
        caret-line (nth (:lines model-after) (:caret-line model-after))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after
      [["TZhe"] ["quick"] ["brown" "fox"] ["jumps"] ["over" "the"] ["lazy" "dog"]]
      [[5.0]    [6.0]     [6.0 4.0]       [6.0]     [5.0 4.0]      [5.0 3.0]]
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
        model-before (textfield2/wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \Z) 9 dummy-interop)
        caret-line (nth (:lines model-after) (:caret-line model-after))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after
      [["The" "quick"] ["brown" "fox"] ["jumpsZ"] ["over" "the"] ["lazy" "dog"]]
      [[4.0 6.0]       [6.0 4.0]       [7.0]      [5.0 4.0]      [5.0 3.0]]
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
        model-before (textfield2/wrap-lines words 9)
        model-after  (textfield2/glyph-> model-before (textfield2/char-glyph \Z) 9 dummy-interop)
        caret-line (nth (:lines model-after) (:caret-line model-after))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model-after
      [["The" "quick"] ["brown"] ["foxZ"] ["jumps"] ["over" "the"] ["lazy" "dog"]]
      [[4.0 6.0]       [6.0]     [5.0]    [6.0]     [5.0 4.0]      [5.0 3.0]]
      2
      0
      [1.0             1.0        1.0      1.0       1.0            1.0])
    (test/is (= 4 (:caret-pos caret-word)))))

(test/deftest line-h-test-1
  (let [words [(tw "The ")                  (assoc (tw "quick ") :h 2.0)
               (assoc (tw "brown ") :h 0.5) (tw "fox| ")
               (assoc (tw "jumps ") :h 1.2)
               (assoc (tw "over ") :h 1.3)  (assoc (tw "the ") :h 1.4)
               (assoc (tw "lazy ") :h 1.5)  (assoc (tw "dog") :h 1.4)]
        model (textfield2/wrap-lines words 9)]
    (test-model
      model
      [["The" "quick"] ["brown" "fox"] ["jumps"] ["over" "the"] ["lazy" "dog"]]
      [[4.0 6.0]       [6.0 4.0]       [6.0]     [5.0 4.0]      [5.0 3.0]]
      1
      1
      [2.0             1.0              1.2       1.4            1.5])))

(test/deftest line-h-test-2
  (let [words [(tw "aaa ")                  (tw "bbb ")              (assoc (tw "ccc ") :h 2.0)
               (assoc (tw "dddd ") :h 0.5)  (tw "eeee ")
               (assoc (tw "ffffffffff ") :h 1.2)
               (assoc (tw "g|g ") :h 1.3)  (assoc (tw "hh ") :h 1.4) (assoc (tw "iii ") :h 1.5)
               (assoc (tw "jjj ") :h 1.5)  (assoc (tw "lll ") :h 1.4) (assoc (tw "mmm") :h 1.3)]
        model (textfield2/wrap-lines words 12)
        caret-line (nth (:lines model) (:caret-line model))
        caret-word (nth (:words caret-line) (:caret-word caret-line))]
    (test-model
      model
      [["aaa" "bbb" "ccc"] ["dddd" "eeee"] ["ffffffffff"] ["gg" "hh" "iii"] ["jjj" "lll" "mmm"]]
      [[4.0   4.0   4.0]   [5.0    5.0]    [11.0]         [3.0  3.0  4.0]   [4.0   4.0   3.0]]
      3
      0
      [2.0                 1.0              1.2           1.5               1.5])
    (test/is (= 1 (:caret-pos caret-word)))))

(test/deftest truncated-word-reducer-test-1
  (let [words [(tw "The ") nil (tw "|  ")]
        reduction (textfield2/truncate-words words)]
    (test/is (= [(tw "The |  ")] reduction))))

(test/deftest truncated-word-reducer-test-2
  (let [words [(tw "The| ") nil (tw "  ")]
        reduction (textfield2/truncate-words words)]
    (test/is (= [(tw "The|   ")] reduction))))

(test/deftest truncated-word-reducer-test-3
  (let [words [(tw "The ") (tw " | ") (tw "  ")]
        reduction (textfield2/truncate-words words)]
    (test/is (= [(tw "The  |   ")] reduction))))

(test/deftest truncated-word-reducer-test-4
  (let [words [(tw "aa") (tw "b|b")]
        reduction (textfield2/truncate-words words)]
    (test/is (= [(tw "aab|b")] reduction))))

(defn word-content-equal [w1 w2] (= (:glyphs w1) (:glyphs w2)))

(defn line-content-equal [l1 l2]
  (and
    (= (count (:words l1)) (count (:words l2)))
    (every? true? (map #(word-content-equal (nth (:words l1) %) (nth (:words l2) %)) (range (count (:words l1)))))))

(defn model-content-equal [m1 m2]
  (and
    (= (count (:lines m1)) (count (:lines m2)))
    (every? true? (map #(line-content-equal (nth (:lines m1) %) (nth (:lines m2) %)) (range (count (:lines m1)))))))

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

(test/deftest move-caret-mark-test-1
  (let [words [(tw "a|aa ") (tw "b ") (tw "cc ")
               (tw "f ") (tw "gggg ") (tw "h ")
               (tw "i ")]
        model-cm-0-0-1 (textfield2/wrap-lines words 8)
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
        model-cm-1-2-2a (textfield2/move-caret-mark model-cm-2-0-0 :caret-&-mark :backward nil nil)]
    (test/is (= [0 0 1 0 0 1] (model->caret-mark-pos model-cm-0-0-1)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-0-0-2))
    (test/is (= [0 0 2 0 0 2] (model->caret-mark-pos model-cm-0-0-2)))

    (test/is (= model-cm-0-0-1 model-cm-0-0-1a))
    (test/is (= model-cm-0-0-2 model-cm-0-0-2a))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-0-0-3))
    (test/is (= [0 0 3 0 0 3] (model->caret-mark-pos model-cm-0-0-3)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-0-1-0))
    (test/is (= [0 1 0 0 1 0] (model->caret-mark-pos model-cm-0-1-0)))

    (test/is (= model-cm-0-0-3 model-cm-0-0-3a))
    (test/is (= model-cm-0-1-0 model-cm-0-1-0a))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-0-1-1))
    (test/is (= [0 1 1 0 1 1] (model->caret-mark-pos model-cm-0-1-1)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-0-2-0))
    (test/is (= [0 2 0 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-0-2-0)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-0-2-1))
    (test/is (= [0 2 1 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-0-2-1)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-0-2-2))
    (test/is (= [0 2 2 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-0-2-2)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-0-2-3))
    (test/is (= [0 2 3 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-0-2-3)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-1-0-0))
    (test/is (= [1 0 0 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-1-0-0)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-1-0-1))
    (test/is (= [1 0 1 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-1-0-1)))

    (test/is (model-content-equal model-cm-0-0-1 model-m-0-1-1-c-1-1-0))
    (test/is (= [1 1 0 0 1 1] (model->caret-mark-pos model-m-0-1-1-c-1-1-0)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-1))
    (test/is (= [1 1 1 1 1 1] (model->caret-mark-pos model-cm-1-1-1)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-2))
    (test/is (= [1 1 2 1 1 2] (model->caret-mark-pos model-cm-1-1-2)))

    (test/is (model-content-equal model-cm-0-0-1 model-c-1-1-2-m-1-1-3))
    (test/is (= [1 1 2 1 1 3] (model->caret-mark-pos model-c-1-1-2-m-1-1-3)))

    (test/is (model-content-equal model-cm-0-0-1 model-c-1-1-2-m-1-1-4))
    (test/is (= [1 1 2 1 1 4] (model->caret-mark-pos model-c-1-1-2-m-1-1-4)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-2a))
    (test/is (= [1 1 2 1 1 2] (model->caret-mark-pos model-cm-1-1-2a)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-3))
    (test/is (= [1 1 3 1 1 3] (model->caret-mark-pos model-cm-1-1-3)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-1-4))
    (test/is (= [1 1 4 1 1 4] (model->caret-mark-pos model-cm-1-1-4)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-2-0))
    (test/is (= [1 2 0 1 2 0] (model->caret-mark-pos model-cm-1-2-0)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-2-1))
    (test/is (= [1 2 1 1 2 1] (model->caret-mark-pos model-cm-1-2-1)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-2-2))
    (test/is (= [1 2 2 1 2 2] (model->caret-mark-pos model-cm-1-2-2)))

    (test/is (= model-cm-1-1-1 model-cm-1-1-1a))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-2-0-0))
    (test/is (= [2 0 0 2 0 0] (model->caret-mark-pos model-cm-2-0-0)))

    (test/is (model-content-equal model-cm-0-0-1 model-cm-1-2-2a))
    (test/is (= [1 2 2 1 2 2] (model->caret-mark-pos model-cm-1-2-2a)))
    (test/is (= model-cm-1-2-2 model-cm-1-2-2a))))

(test/deftest move-caret-mark-test-2
  (let [w 7
        model-before (textfield2/wrap-lines [(tw "xyz ") (tw "|bb")] w)
        model-after (textfield2/move-caret-mark model-before :caret-&-mark :backward nil nil)]
    (test/is (= [0 0 3 0 0 3] (model->caret-mark-pos model-after)))))

(test/deftest has-selection?-test
  (let [model-cm-0-1-1 (textfield2/wrap-lines [(tw "aa ") (tw "b|b")
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
        model-before (textfield2/wrap-lines [(tw "aa ") (tw "b|b ")
                                             (tw "cc ")] w)
        model-after (textfield2/do-backspace-no-sel model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "|b " ) (tw "cc ")] (:words (first (:lines model-after)))))))

(test/deftest nosel-delete-test-2
  (let [w 7
        model-before (textfield2/wrap-lines [(tw "aa ") (tw "b|b ")
                                             (tw "cc ")] w)
        model-after (textfield2/do-delete-no-sel model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "b| " ) (tw "cc ")] (:words (first (:lines model-after)))))))

(test/deftest nosel-delete-test-3
  (let [w 7
        model-before (textfield2/wrap-lines [(tw "xyz ") (tw "bb ")
                                             (tw "aa ") (tw "bb ")
                                             (tw "c|c ")] w)
        model-after (textfield2/do-backspace-no-sel model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "bb " ) (tw "|c ")] (:words (second (:lines model-after)))))))

(test/deftest nosel-delete-test-4
  (let [w 7
        model-before (textfield2/wrap-lines [(tw "xy|z ") (tw "bb ")
                                             (tw "aa ") (tw "bb ")
                                             (tw "cc ")] w)
        model-after (textfield2/do-delete-no-sel model-before w dummy-interop)]
    (test/is (= [(tw "xy| ") (tw "bb " )] (:words (first (:lines model-after)))))
    (test/is (= [(tw "aa ") (tw "bb " )] (:words (second (:lines model-after)))))
    (test/is (= [(tw "cc ")] (:words (nth (:lines model-after) 2))))))

(test/deftest nosel-delete-test-5
  (let [w 7
        model-before (textfield2/wrap-lines [(tw "xyz ") (tw "|bb")] w)
        model-after (textfield2/do-backspace-no-sel model-before w dummy-interop)]
    (test/is (= [(tw "xyz|bb")] (:words (first (:lines model-after)))))))

(test/deftest nosel-delete-test-6
  (let [w 7
        model-before (textfield2/wrap-lines [(tw "xyz ") (tw "bb ")
                                             (tw "aa ") (tw "bb|b")
                                             (tw "cc ")] w)
        model-after (textfield2/do-delete-no-sel model-before w dummy-interop)]
    (test/is (= [(tw "aa ") (tw "bb|cc " )] (:words (second (:lines model-after)))))))

(test/deftest sel-delete-test-1
  (let [w 7
        model (textfield2/wrap-lines [(tw "aa ") (tw "|bb ")
                                      (tw "cc ")] w)
        model-before-cm (textfield2/move-caret-mark model :mark :forward nil nil)
        model-before-mc (textfield2/move-caret-mark model :caret :forward nil nil)
        model-after-cm (textfield2/do-delete-no-sel model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete-no-sel model-before-mc w dummy-interop)
        expected-words [(tw "aa ") (tw "|b " ) (tw "cc ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-2
  (let [w 7
        model (textfield2/wrap-lines [(tw "aa| ") (tw "bb ")
                                      (tw "cc ")] w)
        model-before-cm (->
                          (textfield2/move-caret-mark model :mark :forward nil nil)
                          (textfield2/move-caret-mark :mark :forward nil nil))
        model-before-mc (->
                          (textfield2/move-caret-mark model :caret :forward nil nil)
                          (textfield2/move-caret-mark :caret :forward nil nil))
        model-after-cm (textfield2/do-delete-no-sel model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete-no-sel model-before-mc w dummy-interop)
        expected-words [(tw "aa|b ") (tw "cc ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-3
  (let [w 7
        model (textfield2/wrap-lines [(tw "aa| ") (tw "bb ")
                                      (tw "cc ")] w)
        model-before-cm (textfield2/move-caret-mark model :mark :forward nil nil)
        model-before-mc (textfield2/move-caret-mark model :caret :forward nil nil)
        model-after-cm (textfield2/do-delete-no-sel model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete-no-sel model-before-mc w dummy-interop)
        expected-words [(tw "aa|bb ") (tw "cc ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-2
  (let [w 7
        model (textfield2/wrap-lines [(tw "aa ") (tw "b|b ")
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
        model-after-cm (textfield2/do-delete-no-sel model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete-no-sel model-before-mc w dummy-interop)
        expected-words [(tw "aa ") (tw "b|c ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))

(test/deftest sel-delete-test-2
  (let [w 5
        model (textfield2/wrap-lines [(tw "a|a ") (tw "b ")
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
        model-after-cm (textfield2/do-delete-no-sel model-before-cm w dummy-interop)
        model-after-mc (textfield2/do-delete-no-sel model-before-mc w dummy-interop)
        expected-words [(tw "a|d ")]]
    (test/is (= expected-words (:words (first (:lines model-after-cm)))))
    (test/is (= expected-words (:words (first (:lines model-after-mc)))))
    (test/is (= model-after-cm model-after-mc))))
;
;;;;
;;;; Live tests
;;;;
;
;(test/deftest type-text-test
;  (let [root (fg/defroot
;               (fg/defcomponent table/table :main
;                                {:header-model-loc {:positions init-header-model-pos
;                                                    :sizes init-header-model-size}
;                                 :selection [nil nil]
;                                 :avg-min-cell-w 1
;                                 :avg-min-cell-h 1
;                                 :child-count-dim-margin 1
;                                 :viewport-matrix m/identity-matrix
;                                 :clip-size (m/defpoint 3 4)
;                                 :evolvers {:header-model-loc table/shift-header-model-loc-evolver
;                                            :selection table/cbc-selection}}))
;
;        container (fgtest/init-container root)
;        cid-0-0 (fgtest/wait-table-cell-id container [:main] [0 0])
;        cid-1-0 (fgtest/wait-table-cell-id container [:main] [1 0])
;        _cid-0-1 (fgtest/wait-table-cell-id container [:main] [0 1]) ;Even if cell id is not used, the call still checks that cell has been created
;        cid-1-1 (fgtest/wait-table-cell-id container [:main] [1 1])
;        cid-0-2 (fgtest/wait-table-cell-id container [:main] [0 2])
;        _cid-1-2 (fgtest/wait-table-cell-id container [:main] [1 2])]
;
;    (fgtest/wait-table-cell-property container [:main] [0 0] :atomic-state (fn [as] (not (:selected as))))
;    ))