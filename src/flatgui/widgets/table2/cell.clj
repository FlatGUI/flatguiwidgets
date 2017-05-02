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
            [flatgui.focus :as focus]
            [flatgui.layout :as layout]
            [flatgui.util.matrix :as m]
            [flatgui.inputchannels.mouse :as mouse]))

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
                        (mapv (fn [d] (get-in (:sizes (get-property [] :header-model-loc)) [d (nth mc d)])) (range (count mc)))
                        ;Concat with [z 1] (where z==0) is done specifically because coord is 2-dimentional
                        [0 1]))
    not-in-use-point))

(fg/defaccessorfn calc-position-matrix [component mc]
  (if (not= mc not-in-use-coord)
    (let [hml (get-property [] :header-model-loc)
          positions (if-let [ordered (:ordered-positions hml)] ordered (:positions hml))]
      ;;This works with two-argument version of m/translation
      (apply m/translation (mapv (fn [d] (get-in positions [d (nth mc d)])) (range (count mc)))))
    not-in-use-matrix))

(fg/defaccessorfn get-screen-coord [component]
  (let [this-id (:id component)]
    (if-let [sc (this-id (:cell-id->screen-coord (get-property [] :in-use-model)))]
      sc
      not-in-use-coord)))

(fg/defaccessorfn selected? [component old-atomic-state mc]
  (if (not= mc not-in-use-coord)
    (cond
      (and (mouse/left-mouse-button? component) (mouse/mouse-pressed? component))
      (not (:selected old-atomic-state))
      :else
      (let [selection (get-property [] :selection)
            ;; d 1 means rows
            d 1]
        ;; Multiple selection would be
        ;;(some (fn [e] (= e (nth mc d))) (nth selection d))
        ;; This is single selection
        (let [s-d (nth selection d)]
          ;(and (= 1 (count s-d)) (= (nth mc d) (first s-d)))
          (= (nth mc d) (first s-d))
          )))
    false))

(fg/defevolverfn :atomic-state
  (let [sc (get-screen-coord component)
        mc (calc-model-coord component sc)
        cs (calc-clip-size component mc)
        pm (calc-position-matrix component mc)
        sel (selected? component old-atomic-state mc)]
    {:clip-size       cs
     :position-matrix pm
     :screen-coord    sc
     :model-coord     mc
     :selected        sel}))

(fg/defevolverfn :clip-size (:clip-size (get-property [:this] :atomic-state)))

(fg/defevolverfn :position-matrix (:position-matrix (get-property [:this] :atomic-state)))

(fg/defevolverfn :screen-coord (:screen-coord (get-property [:this] :atomic-state)))

(fg/defevolverfn :model-coord (:model-coord (get-property [:this] :atomic-state)))

(fg/defevolverfn :visible
  (and
    (component/visible-evolver component)
    (not= not-in-use-coord (get-property [:this] :model-coord))))

;(fg/defevolverfn :selection-trigger
;  (and (mouse/left-mouse-button? component) (mouse/mouse-pressed? component)))

(def initial-atomic-state {:clip-size       not-in-use-point
                           :position-matrix not-in-use-matrix
                           :screen-coord    not-in-use-coord
                           :model-coord     not-in-use-coord})

;; Note: inherited from componentbase and not from component for performance reasons
(fg/defwidget "cell"
  {:clip-size       not-in-use-point
   :position-matrix not-in-use-matrix
   ;; Screen coord on the surface of the scrollable panel ("virtual" screen). These values are potentially unlimited
   :screen-coord    not-in-use-coord
   ;; Model coords which are different from screen coords in case sorting/filtering/etc applied
   :model-coord     not-in-use-coord
   :atomic-state    initial-atomic-state
   ;;; Below properties are what component has in addition to componentbase
   :h-margin 0.0625
   :v-margin 0.0625
   :icon-to-text-pos :left
   :exterior-top 0
   :exterior-left 0
   :exterior-bottom 0
   :exterior-right 0
   :has-mouse false
   :accepts-focus? false
   :focus-traversal-order nil
   :focus-state focus/clean-state
   :layout nil
   :coord-map nil
   :evolvers        {:visible visible-evolver
                     :atomic-state atomic-state-evolver
                     :clip-size clip-size-evolver
                     :position-matrix position-matrix-evolver
                     :model-coord model-coord-evolver
                     :screen-coord screen-coord-evolver
                     ;:selection-trigger selection-trigger-evolver

                     ;; Below is everything component has except
                     ;;  - :focus-traversal-order which is slow. Focus management basically works with it but just without good order -
                     ;;     this is good enough for most table cell use cases
                     ;;  - layout's :clip-size which makes no sence since parent (a table) does not use layout for its cells
                     :enabled component/enabled-evolver
                     :has-mouse component/has-mouse-evolver
                     :content-size component/default-content-size-evolver

                     :accepts-focus? focus/simple-accepts-focus-evolver
                     :focus-state focus/focus-state-evolver
                     :focus-traversal-order nil

                     :children-z-order nil

                     :coord-map layout/coord-map-evolver

                     :preferred-size nil
                     }}
  component/componentbase)
