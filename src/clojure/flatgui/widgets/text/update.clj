; Copyright (c) 2018 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc    "Text model incremental update utilities"
      :author "Denys Lebediev"}
flatgui.widgets.text.update
  (:require [flatgui.base :as fg])
  (:import (java.awt.event KeyEvent)))

;; TODO 20180420
;; So to compute an update (glyphs inserted or removed at glyph abs location)
;; need to have an easy/fast way to convert line/word/pos -> glyps abs location

;; todo probably kill-glyphs-in-word should accept from & to as parameters

;;
;; to- are exclusive, and they correspond to caret/mark position such that
;; Del press has the same effect on the model
;;
(defrecord Delete [from-line from-word from-glyph to-line to-word to-glyph])

(defrecord Insert [at-line at-word at-pos glyphs])

(defrecord Update [delete insert])


(defn make-delete-dflt [from-line from-word from-glyph to-line to-word to-glyph]
  (Delete. from-line from-word from-glyph to-line to-word to-glyph))

(defn make-insert-dflt [at-line at-word at-pos glyphs]
  (Insert. at-line at-word at-pos glyphs))

(defn make-update-dflt [delete insert] (Update. delete insert))


(defn create-update-for-supplied-glyphs
  ([model glyphs make-delete-fn make-insert-fn make-update-fn]
    )
  ([model glyphs] (create-update-for-supplied-glyphs model glyphs make-delete-dflt make-insert-dflt make-update-dflt)))

(defn create-update-for-key-press
  ([model key make-delete-fn make-update-fn]
   (let [delete (condp = key
                  KeyEvent/VK_BACK_SPACE nil
                  KeyEvent/VK_DELETE nil
                  nil)]
     (Update. delete nil)))
  ([model key] (create-update-for-key-press model key make-delete-dflt make-update-dflt)))

(defn process-update [model update]
  (let [delete (:delete model)
        insert (:indest model)
        delete-processed (if delete nil model)  ;TODO kill-glyphs but independently from local caret/mark
        ]
    (if insert
      nil
      delete-processed)))