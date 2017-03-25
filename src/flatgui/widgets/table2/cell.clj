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
            [flatgui.util.matrix :as m]
            [flatgui.util.rectmath :as r]))

(def not-in-use-screen-coord [-1 -1])

(fg/defevolverfn :model-coord
  (let [screen->model (get-property [] :screen->model)]
    (screen->model (get-property [:this] :screen-coord))))

(fg/defevolverfn :clip-size
  (let [mc (get-property [:this] :model-coord)]
    (if (not= mc not-in-use-screen-coord)
      (apply m/defmxcol (concat
                          (mapv (fn [d] (get-in (get-property [] :header-model-size) [d (nth mc d)])) (range (count mc)))
                          ;Concat with [z 1] (where z==0) is done specifically because coord is 2-dimentional
                          [0 1]))
      old-clip-size)))

(fg/defevolverfn :position-matrix
  (let [mc (get-property [:this] :model-coord)]
    (if (not= mc not-in-use-screen-coord)
      (let [positions (get-property [] :header-model-pos)]
        ;;This works with two-argument version of m/translation
        (apply m/translation (mapv (fn [d] (get-in positions [d (nth mc d)])) (range (count mc)))))
      old-position-matrix)))

(fg/defevolverfn :screen-coord
  (let [this-id (:id component)]
    (if-let [sc (this-id (:cell-id->screen-coord (get-property [] :in-use-model)))] sc old-screen-coord)))

(fg/defwidget "cell"
  {:clip-size (m/defpoint 0 0)
   :position-matrix m/identity-matrix
   ;; Screen coord on the surface of the scrollable panel ("virtual" screen). These values are potentially unlimited
   :screen-coord not-in-use-screen-coord
   ;; Model coords which are different from screen coords in case sorting/filtering/etc applied
   :model-coord not-in-use-screen-coord
   :evolvers {:clip-size clip-size-evolver
              :position-matrix position-matrix-evolver
              :model-coord model-coord-evolver
              :screen-coord screen-coord-evolver}}
  component/component)
