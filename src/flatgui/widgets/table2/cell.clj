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
  (let [screen->model (get-property [] :screen->model)
        _ (if (= (:id component) :cell-15-9)
            (println "=================================== CELL MC!!!!!!!!! " (screen->model (get-property [:this] :screen-coord)) ))]
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
      (let [                                              ;_ (println "CELL " (:id component) "MC" mc "SC" (get-property [:this] :screen-coord))
            positions (get-property [] :header-model-pos)
            _ (if (= :cell-15-9 (:id component))
                (println "CELL " (:id component) "MC" mc "SC" (get-property [:this] :screen-coord)
                         "T" (apply m/translation (mapv (fn [d] (get-in positions [d (nth mc d)])) (range (count mc))))
                         ) )]
        ;;This works with two-argument version of m/translation
        (apply m/translation (mapv (fn [d] (get-in positions [d (nth mc d)])) (range (count mc)))))
      old-position-matrix)))

;(fg/defevolverfn :in-use
;  ;; 2-dimentional version
;  (let [                                                    ;_ (println "CELL IN-USE called for " (:id component) " for reason " (get-reason))
;        svpm (get-property [] :viewport-matrix)
;        scs (get-property [] :clip-size)
;        viewport-rect-x1 (m/mx-x svpm)
;        viewport-rect-y1 (m/mx-y svpm)
;        viewport-rect-x2 (+ viewport-rect-x1 (m/x scs))
;        viewport-rect-y2 (+ viewport-rect-y1 (m/y scs))
;        cpm (get-property [:this] :position-matrix)
;        ccs (get-property [:this] :clip-size)
;        ;_ (println "cpm" cpm "ccs" ccs)
;        cell-rect-x1 (m/mx-x cpm)
;        cell-rect-y1 (m/mx-y cpm)
;        cell-rect-x2 (+ cell-rect-x1 (m/x ccs))
;        cell-rect-y2 (+ cell-rect-y1 (m/y ccs))]
;    (r/intersect?
;      viewport-rect-x1 viewport-rect-y1 viewport-rect-x2 viewport-rect-y2
;      cell-rect-x1 cell-rect-y1 cell-rect-x2 cell-rect-y2)))

;(fg/defevolverfn :screen-coord
;  (let [in-use (get-property [:this] :in-use)
;        ;_ (println "--" (:id component) " -- I'm in use now: " in-use " old coord is " old-screen-coord)
;        ]
;    (cond
;      (and in-use (= old-screen-coord not-in-use-screen-coord))
;      (let [vacant-coords (:vacant-screen-coords (get-property [] :in-use-model))
;            this-id (:id component)
;            new-coord (this-id vacant-coords)]
;        (if new-coord
;          (let [_ (println "vacant-coords=" vacant-coords)
;                _ (println "--" this-id " -- I'm in use now with coord " new-coord)]
;             new-coord)
;          old-screen-coord))
;
;      (not in-use)
;      not-in-use-screen-coord
;
;      :else
;      old-screen-coord)))

(fg/defevolverfn :screen-coord
  (let [this-id (:id component)
        _ (if (= this-id :cell-15-9)
            (println "=================================== CELL SC!!!!!!!!! " this-id (this-id (:cell-id->screen-coord (get-property [] :in-use-model)))))
        ]
    (if-let [sc (this-id (:cell-id->screen-coord (get-property [] :in-use-model)))] sc old-screen-coord)))

(fg/defwidget "cell"
  {
   ;;; Actually on screen, regardless of screen coord on the scrollable pane, and therefore regardles of how many
   ;;; rows/columns there are. These coords are limited by physical screen size
   ;:physical-screen-coord [0 0]

   ;;; true if cell is located somewhere within visible area
   ;:in-use nil

   :clip-size (m/defpoint 0 0)
   :position-matrix m/identity-matrix

   ;; Screen coord on the surface of the scrollable panel ("virtual" screen). These values are potentially unlimited
   :screen-coord not-in-use-screen-coord
   ;; Model coords which are different from screen coords in case sorting/filtering/etc applied
   :model-coord not-in-use-screen-coord
   :evolvers {:clip-size clip-size-evolver
              :position-matrix position-matrix-evolver
              ;:in-use in-use-evolver
              :model-coord model-coord-evolver
              :screen-coord screen-coord-evolver
              }}
  component/component)
