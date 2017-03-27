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

(def not-in-use-coord [-1 -1])
(def not-in-use-point (m/defpoint -1 -1))
(def not-in-use-matrix (m/translation -1 -1))

(fg/defaccessorfn calc-model-coord [component sc]
  (if (not= sc not-in-use-coord)
    (let [screen->model (get-property [] :screen->model)]
      (screen->model sc))
    not-in-use-coord))

(fg/defaccessorfn calc-clip-size [component mc]
  (if (not= mc not-in-use-coord)
    (apply m/defmxcol (concat
                        (mapv (fn [d] (get-in (get-property [] :header-model-size) [d (nth mc d)])) (range (count mc)))
                        ;Concat with [z 1] (where z==0) is done specifically because coord is 2-dimentional
                        [0 1]))
    not-in-use-point))

(fg/defaccessorfn calc-position-matrix [component mc]
  (if (not= mc not-in-use-coord)
    (let [positions (get-property [] :header-model-pos)]
      ;;This works with two-argument version of m/translation
      (apply m/translation (mapv (fn [d] (get-in positions [d (nth mc d)])) (range (count mc)))))
    not-in-use-matrix))

(fg/defaccessorfn get-screen-coord [component]
  (let [this-id (:id component)]
    (if-let [sc (this-id (:cell-id->screen-coord (get-property [] :in-use-model)))]
      sc
      not-in-use-coord)))

(fg/defevolverfn :atomic-state
  (let [sc (get-screen-coord component)
        mc (calc-model-coord component sc)
        cs (calc-clip-size component mc)
        pm (calc-position-matrix component mc)]
    {:clip-size       cs
     :position-matrix pm
     :screen-coord    sc
     :model-coord     mc}))

(fg/defevolverfn :clip-size (:clip-size (get-property [:this] :atomic-state)))

(fg/defevolverfn :position-matrix (:position-matrix (get-property [:this] :atomic-state)))

(fg/defevolverfn :screen-coord (:screen-coord (get-property [:this] :atomic-state)))

(fg/defevolverfn :model-coord (:model-coord (get-property [:this] :atomic-state)))

(fg/defevolverfn :visible
  (and
    (component/visible-evolver component)
    (not= not-in-use-coord (get-property [:this] :model-coord))))

(def initial-atomic-state {:clip-size       not-in-use-point
                           :position-matrix not-in-use-matrix
                           :screen-coord    not-in-use-coord
                           :model-coord     not-in-use-coord})

(fg/defwidget "cell"
  {:clip-size       not-in-use-point
   :position-matrix not-in-use-matrix
   ;; Screen coord on the surface of the scrollable panel ("virtual" screen). These values are potentially unlimited
   :screen-coord    not-in-use-coord
   ;; Model coords which are different from screen coords in case sorting/filtering/etc applied
   :model-coord     not-in-use-coord
   :atomic-state    initial-atomic-state
   :evolvers        {:visible visible-evolver
                     :atomic-state atomic-state-evolver
                     :clip-size clip-size-evolver
                     :position-matrix position-matrix-evolver
                     :model-coord model-coord-evolver
                     :screen-coord screen-coord-evolver}}
  component/component)
