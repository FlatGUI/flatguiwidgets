; Copyright (c) 2017 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Table header corner fixed pane"
      :author "Denys Lebediev"}
 flatgui.widgets.table2.cell
  (:require [flatgui.base :as fg]
            [flatgui.widgets.component :as component]
            [flatgui.util.matrix :as m]))

(fg/defevolverfn :model-coord
  (let [screen->model (get-property [:this] :screen->model)]
    (screen->model (get-property [:this] :screen-coord))))

(fg/defevolverfn :clip-size
  (if (= [] (get-reason))
    (let [mc (get-property [:this] :model-coord)
          sizes (get-property [] :header-model-size)]
      (apply m/defmxcol (concat
                          (mapv (fn [d] (get-in sizes [d (nth mc d)])) (range (count mc)))
                          [0 1]))) ;Concat with [z 1] (where z==0) is done specifically because coord is 2-dimentional
    old-clip-size))

(fg/defevolverfn :position-matrix
  (if (= [] (get-reason))
    (let [mc (get-property [:this] :model-coord)
          positions (get-property [] :header-model-pos)]
      ;;This works with two-argument version of m/translation
      (apply m/translation (mapv (fn [d] (get-in positions [d (nth mc d)])) (range (count mc)))))
    old-position-matrix))

(fg/defwidget "cell"
  {

   ;; Actually on screen, regardless of screen coord on the scrollable pane, and therefore regardles of how many
   ;; rows/columns there are. These coords are limited by physical screen size
   :physical-screen-coord [0 0]
   ;; Screen coord on the surface of the scrollable panel ("virtual" screen). These values are potentially unlimited
   :screen-coord [0 0]
   ;; Model coords which are different from screen coords in case sorting/filtering/etc applied ; TODO use table function to translate
   :model-coord [0 0]
   :evolvers {;:clip-size clip-size-evolver
              ;:position-matrix position-matrix-evolver
              }}
  component/component)
