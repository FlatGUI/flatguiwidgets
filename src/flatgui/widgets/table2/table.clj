; Copyright (c) 2017 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc    "Table widget"
      :author "Denys Lebediev"}
flatgui.widgets.table2.table
  (:require [flatgui.base :as fg]
            [flatgui.widgets.panel]
            [flatgui.widgets.scrollpanel :as scrollpanel]
    ;[flatgui.widgets.table2.contentpane :as contentpane]
            [flatgui.widgets.table2.cell :as cell]
            [flatgui.util.matrix :as m]
            [flatgui.util.vecmath :as v]
            [flatgui.util.rectmath :as r])
  (:import (java.util Collections)))

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

(defn edge-search [range-size start pred]
  (let [_ (println "    --edge-search" range-size start pred)]
    (loop [dir-dist [(if (< start (dec range-size)) 1 -1) 1]
           i start]
      (if (and
            (and (>= i 0) (< i range-size))
            (not (pred i)))
        (recur
          (let [dir (first dir-dist)
                dist (second dir-dist)
                next-dist (inc dist)]
            (if (= 1 dir)
              (if (>= (+ start (* -1 dist)) 0)
                [-1 dist]
                [1 next-dist])
              (if (< (+ start (* 1 next-dist)) range-size)
                [1 next-dist]
                [-1 next-dist])))
          (+ start (apply * dir-dist)))
        i))
    ))

(fg/defevolverfn :in-use-model
  (cond
    (= (get-reason) [:this])
    (let [header-model-pos (get-property [:this] :header-model-pos)
          header-model-size (get-property [:this] :header-model-size)
          vpm (get-property [:this] :viewport-matrix)
          cs (get-property [:this] :clip-size)
          viewport-begin (mapv (fn [a] (* -1 a)) (v/mxtransf->vec vpm 2)) ;2-dimensional
          viewport-end (v/-mxtransf+point->vec vpm cs 2) ;2-dimensional
          viewport-begin-shift (v/vec-comparenum (:viewport-begin old-in-use-model) viewport-begin)
          viewport-end-shift (v/vec-comparenum (:viewport-end old-in-use-model) viewport-end)

          _ (println
              "---------------------------:in-use-model\n"
              "vpm" vpm "\n"
              "cs" cs "\n"
              "header-model-pos" header-model-pos "\n"
              "header-model-size" header-model-size "\n"
              "old viewport-begin" (:viewport-begin old-in-use-model) "\n"
              "old viewport-end" (:viewport-end old-in-use-model) "\n"
              "viewport-begin" viewport-begin "\n"
              "viewport-end" viewport-end "\n"
              "screen-area" (:screen-area old-in-use-model)
              ;"\n"
              ;"viewport-begin-shift" viewport-begin-shift "\n"
              ;"viewport-end-shift" viewport-end-shift
              )
          search-fn (fn [d start for-begin]
                      (let [;rng (vec (range (count (nth header-model-pos d))))
                            header-model-pos-d (nth header-model-pos d)
                            header-model-size-d (nth header-model-size d)
                            dim-range-size (count header-model-pos-d)
                            visible-screen-coord? (fn [coord]
                                                    (let [r (if (and (and (>= coord 0) (< coord dim-range-size)))
                                                              (let [screen-point-from (nth header-model-pos-d coord)
                                                                    screen-point-to (+
                                                                                      (nth header-model-pos-d coord)
                                                                                      (nth header-model-size-d coord))]
                                                                (r/line&
                                                                  (nth viewport-begin d) (nth viewport-end d)
                                                                  screen-point-from screen-point-to)))
                                                          _ (if (not for-begin)
                                                              (println "IN DIM" d
                                                                       "coord" coord (if (and (>= coord 0) (< coord dim-range-size))
                                                                                       [(nth header-model-pos-d coord)
                                                                                        (+
                                                                                          (nth header-model-pos-d coord)
                                                                                          (nth header-model-size-d coord))]
                                                                                       "<out of range>")
                                                                       "visibility is" r))]
                                                      r))
                            viewport-begin-pred (fn [coord] (and
                                                              (not (visible-screen-coord? (dec coord)))
                                                              (visible-screen-coord? coord)))
                            viewport-end-pred (fn [coord] (and
                                                            (visible-screen-coord? coord)
                                                            (not (visible-screen-coord? (inc coord)))))
                            search-result (edge-search dim-range-size start (if for-begin viewport-begin-pred viewport-end-pred))]
                        (cond
                          (< search-result 0) 0
                          (>= search-result dim-range-size) (dec search-result)
                          :else search-result)))

          old-screen-area (:screen-area old-in-use-model)
          ;new-screen-area [(mapv #(search-fn % (first old-screen-area) viewport-begin-shift) (range (count viewport-begin)))
          ;                 (mapv #(search-fn % (second old-screen-area) viewport-end-shift) (range (count viewport-end)))]
          _ (println "CALLING SEARCH---------->")
          dimensions (range (count viewport-begin))
          new-screen-area [(mapv #(search-fn % (nth (first old-screen-area) %) true) dimensions)
                           (mapv #(search-fn % (nth (second old-screen-area) %) false) dimensions)]
          _ (println "-----> new-screen-area = " new-screen-area)

          ;search-fn (fn [d v v-shift]
          ;            (let [src-coord (nth v d)
          ;                  step (nth v-shift d)]
          ;              (let [_ (println
          ;                        "    ---------------------------search-fn\n"
          ;                        "    d" d "\n"
          ;                        "    v" v "\n"
          ;                        "    v-shift" v-shift)
          ;                    inbound? (fn [coord d] (and
          ;                                             (>= coord 0)
          ;                                             (< coord (count (nth header-model-pos d)))))
          ;                    visible-screen-point? (fn [dimc d] (and
          ;                                                         (>= dimc (nth viewport-begin d))
          ;                                                         (< dimc (nth viewport-end d))))
          ;                    visible-screen-coord? (fn [coord d]
          ;                                            (or
          ;                                              (visible-screen-point? (nth (nth header-model-pos d) coord) d)
          ;                                              (visible-screen-point? (+
          ;                                                                       (nth (nth header-model-pos d) coord)
          ;                                                                       (nth (nth header-model-size d) coord)) d)))
          ; ]
          ;                (loop [coord src-coord]
          ;                  (if (and (inbound? coord d) (not (visible-screen-coord? coord d)))
          ;                    (recur
          ;                      (+ coord step))
          ;                    coord)))))
          ;old-screen-area (:screen-area old-in-use-model)
          ;new-screen-area [(mapv #(search-fn % (first old-screen-area) viewport-begin-shift) (range (count viewport-begin)))
          ;                 (mapv #(search-fn % (second old-screen-area) viewport-end-shift) (range (count viewport-end)))]
          ]
      {:viewport-begin viewport-begin
       :viewport-end viewport-end
       :screen-area new-screen-area

       ; TODO Use rect-, then see below
       ;(def r [[1 3] [4 6]])
       ;=> #'user/r
       ;(mapv (fn [d] (range (nth (first r) d) (nth (second r) d))) (range (count (first r))))
       ;=> [(1 2 3) (3 4 5)]
       :vacant-screen-coords #{}
       })

    (= (count (get-reason)) 2)
    (let [cell-id (second (get-reason))
          in-use (get-property [:this cell-id] :in-use)]
      (if in-use
        (assoc
          old-in-use-model
          :vacant-screen-coords
          (disj (:vacant-screen-coords old-in-use-model) (get-property [:this cell-id] :screen-coord)))
        old-in-use-model))

    :else
    old-in-use-model))

(fg/defevolverfn :not-in-use
  (if-let [cell-id (second (get-reason))]
    (let [in-use (get-property [:this cell-id] :in-use)]
      (if in-use
        (disj old-not-in-use cell-id)
        (conj old-not-in-use cell-id)))
    old-not-in-use))

;(fg/defevolverfn :screen-area
; ;; binary search starting from last screen-area coords, checking if a coord of another screen cell is within visible area?
;  )
;
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
   ;   :screen-area [[0 0] [1 1]]
   :vacant-screen-coords #{}
   ;:bench {}

   :in-use-model {:viewport-begin [0 0]
                  :viewport-end [1 1]
                  :screen-area [[0 0] [1 1]]
                  :vacant-screen-coords #{}}

   ;:in-use-model {:scr-area [[0 0] [1 1]]                ; visible area in terms of screen coords
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