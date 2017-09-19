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

;(test/deftest id-duplets-test
;  (let [c [1 4 7 10 11 22 34 45 46 8]]
;    (test/is (= [1 4 7 10 0 22 34 45 0 8] (textfield2/id-duplets c)))))

;(test/deftest id-duplets2-test
;  (let [c [1 4 7 10 11 22 34 45 46 8]]
;    (test/is (= [1 4 7 10 0 "x" 22 34 45 0 "x" 8] (textfield2/id-duplets2 c)))))

;(test/deftest id-duplets2-test
;  (let [c [1 4 7 10 11 22 34 45 46 8]
;        r (textfield2/id-duplets2 c)]
;    (test/is (= [1 4 7 10 0 "x" 22 34 45 0 "x" 8] r))))

(test/deftest make-words-test-1
  (let [glyphs [(textfield2/char-glyph \1) (textfield2/char-glyph \1) (textfield2/char-glyph \1)]
        words (textfield2/make-words glyphs 1 3 dummy-interop)]
    (test/is (= (list (Word. glyphs 1 3 3)) words))))