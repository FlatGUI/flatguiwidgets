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

(defn test-glyph [w h] (textfield2/glyph :test nil {:w w :h h}))

(test/deftest make-words-test-1
  (let [glyphs (mapv textfield2/char-glyph "111")
        words (textfield2/make-words glyphs 1 3 dummy-interop)]
    (test/is (= (list (Word. glyphs 1 1 3.0 3.0)) words))))

(test/deftest make-words-test-2
  (let [glyphs-1 (mapv textfield2/char-glyph "111")
        glyphs-2 [(textfield2/char-glyph \2)]
        glyphs (vec (concat glyphs-1 glyphs-2))
        words-cp-0 (textfield2/make-words glyphs 0 3 dummy-interop)
        words-cp-1 (textfield2/make-words glyphs 1 3 dummy-interop)
        words-cp-2 (textfield2/make-words glyphs 2 3 dummy-interop)
        _ (println "=================================================== before words-cp-3 ======================================")
        words-cp-3 (textfield2/make-words glyphs 3 3 dummy-interop)
        _ (println "=================================================== before words-cp-4 ======================================")
        words-cp-4 (textfield2/make-words glyphs 4 3 dummy-interop)
        ]
    (test/is (= (list (Word. glyphs-1 0 0 3.0 3.0) (Word. glyphs-2 nil nil 1.0 1.0)) words-cp-0))
    (test/is (= (list (Word. glyphs-1 1 1 3.0 3.0) (Word. glyphs-2 nil nil 1.0 1.0)) words-cp-1))
    (test/is (= (list (Word. glyphs-1 2 2 3.0 3.0) (Word. glyphs-2 nil nil 1.0 1.0)) words-cp-2))
    (test/is (= (list (Word. glyphs-1 nil nil 3.0 3.0) (Word. glyphs-2 0 0 1.0 1.0)) words-cp-3))
    (test/is (= (list (Word. glyphs-1 nil nil 3.0 3.0) (Word. glyphs-2 1 1 1.0 1.0)) words-cp-4))
    ))

(test/deftest make-words-test-3
  (let [glyphs-1 (mapv textfield2/char-glyph "111")
        glyphs-2 (mapv textfield2/char-glyph "222")
        glyphs-3 (mapv textfield2/char-glyph "33")
        glyphs (vec (concat glyphs-1 glyphs-2 glyphs-3))
        words (textfield2/make-words glyphs 7 3 dummy-interop)]
    (test/is (= (list (Word. glyphs-1 nil nil 3.0 3.0) (Word. glyphs-2 nil nil 3.0 3.0) (Word. glyphs-3 1 1 2.0 2.0)) words))))

(defn test-model [model expected-lines expected-total-word-widths expected-caret-line expected-caret-word]
  (let [model-lines (:lines model)
        lines (mapv :words model-lines)]
    (test/is (= expected-lines (textfield2/lines->strings lines)))
    (test/is (= expected-total-word-widths (textfield2/lines->total-word-widths lines)))
    (if expected-caret-line (test/is (= expected-caret-line (:caret-line model))))
    (if expected-caret-word (test/is (= expected-caret-word (:caret-word (nth model-lines (:caret-line model))))))
    ))

(defn test-words [words w expected-lines expected-total-word-widths expected-caret-line expected-caret-word]
  (let [model (textfield2/wrap-lines words w)]
    (test-model model expected-lines expected-total-word-widths expected-caret-line expected-caret-word))
  )

(defn test-lines [text w expected-lines expected-total-word-widths]
  (let [glyphs (map textfield2/char-glyph text)
        words (textfield2/glyphs->words glyphs w dummy-interop)]
    (test-words words w expected-lines expected-total-word-widths nil nil)
    ))

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
        glyphs (mapv textfield2/char-glyph s-clean)
        result-caret-pos (if (>= caret-pos 0) caret-pos)]
    (Word. glyphs result-caret-pos result-caret-pos w-content w-total)))

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
      0)
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
      [[4.0 6.0]       [6.0 4.0]       [7.0]     [5.0 4.0]      [5.0 3.0]]
      2
      0)
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
      0)
    (test/is (= 4 (:caret-pos caret-word)))))

(test/deftest truncated-word-reducer-test-1
  (let [words [(tw "The ") nil (tw "|  ")]
        reduction (vec (reduce textfield2/truncated-word-reducer [] words))]
    (test/is (= [(tw "The |  ")] reduction))))

(test/deftest truncated-word-reducer-test-2
  (let [words [(tw "The| ") nil (tw "  ")]
        reduction (vec (reduce textfield2/truncated-word-reducer [] words))]
    (test/is (= [(tw "The|   ")] reduction))))

(test/deftest truncated-word-reducer-test-3
  (let [words [(tw "The ") (tw " | ") (tw "  ")]
        reduction (vec (reduce textfield2/truncated-word-reducer [] words))]
    (test/is (= [(tw "The  |   ")] reduction))))

(test/deftest has-selection?-test
  (let [model-0 (textfield2/wrap-lines [(tw "The ") (tw "q|uick")] 3)
        ;model-1 (textfield2/wrap-lines [(tw "The ") (assoc (tw "q|uick") :mark-pos 2)] 3)
        ;model-2 (textfield2/wrap-lines [(tw "T|he ") (assoc (tw "quick") :mark-pos 2)] 3)
        ;model-3 (textfield2/wrap-lines [(tw "T|he ") (tw "quick" ) (assoc (tw "brown ") :mark-pos 2) ] 9)
        ]))