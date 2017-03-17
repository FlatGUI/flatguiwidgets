; Copyright (c) 2017 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Table widget"
      :author "Denys Lebediev"}
flatgui.widgets.table2.table
  (:require [flatgui.base :as fg]
            [flatgui.widgets.panel]
            [flatgui.widgets.scrollpanel :as scrollpanel]
    ;[flatgui.widgets.table2.contentpane :as contentpane]
            [flatgui.widgets.table2.cell :as cell]
            [flatgui.util.matrix :as m]))

;;; Given
;;; - col & row header positions and sizes
;;; - scrollable vieport translation
;;; calculate the map showing which screen coords are taken by phycisal screen areas.
;;; Assign some child cells, hide those that are not needed to cover physical screen.
;;; As a result, each child cell will have :screen-coord assigned. This algo will use
;;; cells' :physical-screen-coord to iterate and cover all partially and fully visible
;;; screen cells (meaning virtual screen where screen means on top of a scrollable
;;; panel, not physically).
;;;
;;; No data size in table may depend on col/row count as k*N+b ?



;(def dim-rows 0)
;(def dim-cols 1)

(defn gen-cell-id [& dim] (keyword (str "cell" (apply str (map #(str "-" %) dim)))))

(defn all-coords-2d [pss]
  (let [row-cnt (first pss)
        col-cnt (second pss)]
    (mapcat (fn [r] (map (fn [c] [r c]) (range col-cnt))) (range row-cnt))))

(fg/defevolverfn :children
  (let [;screen->model (get-property [:this] :screen->model) TODO this is not here, this will be needed in each cell to determine virtual screen coord
        ]
    (into {} (map (fn [coord]
                    (let [cid (apply gen-cell-id coord)
                          c (cid old-children)]
                      [cid (if c c (fg/defcomponent
                                     cell/cell
                                     cid
                                     {;:physical-screen-coord coord
                                      }))]))
                  (all-coords-2d (get-property [:this] :physical-screen-size))))))

(fg/defevolverfn :physical-screen-size
  (let [clip-size (get-property [:this] :clip-size)]
    [(int (Math/ceil (double (/ (m/y clip-size) (get-property [:this] :min-cell-h)))))
     (int (Math/ceil (double (/ (m/x clip-size) (get-property [:this] :min-cell-w)))))]))

(fg/defevolverfn :header-model-pos
  (if-let [cell-id (second (get-reason))]
    (let [pm (get-property [:this cell-id] :position-matrix)
          pmt (dec (count pm))
          model-coord (get-property [:this cell-id] :model-coord)]
      (loop [d 0
             positions old-header-model-pos]
        (if (< d (count model-coord))
          (recur
            (inc d)
            (assoc-in positions [d (nth model-coord d)] (m/mx-get pm d pmt)))
          positions)))
    old-header-model-pos))

(fg/defevolverfn :header-model-size
  (if-let [cell-id (second (get-reason))]
    (let [cs (get-property [:this cell-id] :clip-size)
          model-coord (get-property [:this cell-id] :model-coord)]
      (loop [d 0
             sizes old-header-model-size]
        (if (< d (count model-coord))
          (recur
            (inc d)
            (assoc-in sizes [d (nth model-coord d)] (m/mx-get cs d 0)))
          sizes)))
    old-header-model-size))

;(fg/defevolverfn :in-use-model
;  (cond
;    (= (get-reason) [:this])
;
;    (= (count (get-reason)) 2)
;    (let [cell-id (second (get-reason))
;             in-use (get-property [:this cell-id] :in-use)]
;      (if in-use
;        {}))
;
;    :else
;    old-in-use-model))

(fg/defevolverfn :not-in-use
  (if-let [cell-id (second (get-reason))]
    (let [in-use (get-property [:this cell-id] :in-use)]
      (if in-use
        (disj old-not-in-use cell-id)
        (conj old-not-in-use cell-id)))
    old-not-in-use))

(fg/defaccessorfn dummy-value-provider [component model-row model-col]
  (str (get-property [:this] :id) "-" model-row "-" model-col))

(fg/defwidget "table"
  {:header-line-count [1 0]                             ; By default, 1 header row and 0 header columns
   :header-model-pos [[0] [0]]                          ; By default, 1 cell (1 row header) starting at 0,0 of size 1,1
   :header-model-size [[1] [1]]
   :model-size [1 1]                                    ; By default, 1x1 (1 row header cell)
   :physical-screen-size [1 1]                          ; Determined by cell minimum size (a constant) and table clip size
   :min-cell-w 0.25
   :min-cell-h 0.25

   :not-in-use #{}

   ;:in-use-model {:scr-area [[0 0] [1 1]]               ; visible area in terms of screen coords
   ;               :bench {}}                            ; cell ids (and possibly screen coords assigned) awaiting to be put in use

   :screen->model identity                              ; This coord vector translation fn may take into account sorting/filtering etc.
   :value-provider dummy-value-provider
   ;:children {:content-pane contentpane/tablecontentpane}
   :evolvers {:physical-screen-size physical-screen-size-evolver ; may be turned off for better performance (but :physical-screen-size would need to be enough)
              :children children-evolver                ; maintains enough child cells to always cover :physical-screen-size area
              :header-model-pos header-model-pos-evolver
              :header-model-size header-model-size-evolver
              :not-in-use not-in-use-evolver
              }}
  scrollpanel/scrollpanel)