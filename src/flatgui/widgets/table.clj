; Copyright (c) 2015 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Table widget"
      :author "Denys Lebediev"}
  flatgui.widgets.table
  (:require [flatgui.base :as fg]
            [flatgui.widgets.panel]
            [flatgui.widgets.scrollpanel]
            [flatgui.widgets.table.contentpane]))


(fg/defwidget "table"
  {:header-ids nil
   :header-aliases nil

   ;; TODO Problem is that it requires :value-provider (NPE is thrown otherwise) even if cells have custom :text evolver
   :value-provider nil

   ;;  - TODO -
   ;; Implement focus for table cells and make this false.
   ;; It looks like have to combine "anchor" and "focus" concepts.
   ;; This way natural focus order will be also the same as anchor traversal order usually is.
   :focusable true

   :background :prime-1
   :children {:header flatgui.widgets.panel/panel
              :content-pane flatgui.widgets.table.contentpane/tablecontentpane}}
  flatgui.widgets.scrollpanel/scrollpanel)