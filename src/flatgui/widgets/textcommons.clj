; Copyright (c) 2017 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Text Field widget"
      :author "Denys Lebediev"}
  flatgui.widgets.textcommons
  (:require [flatgui.widgets.component]
            [flatgui.widgets.scrollpanel]
            [flatgui.util.matrix :as m]))

(defn deccaretpos [c]
  (if (> c 0) (- c 1) 0))

(defn inccaretpos [c len]
  (if (< c len) (+ c 1) len))

(defn keep-in-range [mx cs content-size]
  (let [x (m/mx-x mx)
        y (m/mx-y mx)]
    (m/translation
      (if (neg? x) (max x (- (m/x cs) (m/x content-size))) x)
      (if (neg? y) (max y (- (m/y cs) (m/y content-size))) y))))