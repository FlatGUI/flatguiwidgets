; Copyright (c) 2017 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:author "Denys Lebediev"}
  flatgui.widgets.textcommons
  (:require [flatgui.widgets.component]
            [flatgui.widgets.scrollpanel]
            [flatgui.util.matrix :as m]
            [flatgui.inputchannels.keyboard :as keyboard])
  (:import (java.awt.event KeyEvent)))

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

(defn textfield-dflt-text-suplier [component]
  (if (or
        ;; temp fix: it kills apostrophe because it's 0x27, same as VK_RIGHT
        (and
          (keyboard/key-typed? component)
          (not
            (#{KeyEvent/VK_BACK_SPACE KeyEvent/VK_DELETE KeyEvent/VK_LEFT
               KeyEvent/VK_HOME KeyEvent/VK_END KeyEvent/VK_UP KeyEvent/VK_DOWN
               KeyEvent/VK_PAGE_UP KeyEvent/VK_PAGE_DOWN}
              (keyboard/get-key component))))
        (not
          (#{KeyEvent/VK_BACK_SPACE KeyEvent/VK_DELETE KeyEvent/VK_LEFT KeyEvent/VK_RIGHT
             KeyEvent/VK_HOME KeyEvent/VK_END KeyEvent/VK_UP KeyEvent/VK_DOWN
             KeyEvent/VK_PAGE_UP KeyEvent/VK_PAGE_DOWN}
            (keyboard/get-key component))))
    (keyboard/get-key-str component)
    ""))