; Copyright (c) 2015 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Base types for menu widget"
      :author "Denys Lebediev"}
  flatgui.widgets.abstractmenu
  (:require [flatgui.base :as fg]
            [flatgui.inputchannels.mouse :as mouse]
            [flatgui.widgets.component]
            [flatgui.widgets.table]
            [flatgui.widgets.table.contentpane]
            [flatgui.widgets.table.header]
            [flatgui.widgets.menu.menucell]))

(fg/defwidget "menucontentpane"
  {:default-cell-component flatgui.widgets.menu.menucell/menucell
   :selection-mode :single
   :mouse-triggers-selection? (fn [component] (mouse/mouse-entered? component))}
  flatgui.widgets.table.contentpane/tablecontentpane)

(fg/defwidget "menuheader"
  {:visible :false
   :default-height 0}
  flatgui.widgets.table.header/tableheader)

(fg/defwidget "abstractmenu"
  {:popup true
   :evolvers {:z-position flatgui.widgets.component/z-position-evolver }
   :children {:header (fg/defcomponent menuheader :header {})
              :content-pane (fg/defcomponent menucontentpane :content-pane {})}}
  flatgui.widgets.table/table)