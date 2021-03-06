; Copyright (c) 2015 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Panel widget"
      :author "Denys Lebediev"}
  flatgui.widgets.panel
  (:require [flatgui.base :as fg]
            [flatgui.widgets.component]))

;; For now, inherits :skin-key from component
(fg/defwidget "panel"
  {:consumes? (fn [_] false)}
  flatgui.widgets.component/component)