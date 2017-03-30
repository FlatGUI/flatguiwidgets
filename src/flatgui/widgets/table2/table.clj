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
            [flatgui.widgets.component :as component]
            [flatgui.widgets.table2.cell :as cell]
            [flatgui.focus :as focus]
            [flatgui.layout :as layout]
            [flatgui.util.matrix :as m]
            [flatgui.util.vecmath :as v]
            [flatgui.util.rectmath :as r]))

(defn gen-cell-id [& dim] (keyword (str "cell" (apply str (map #(str "-" %) dim)))))

(defn all-coords-2d [pss]
  (let [row-cnt (first pss)
        col-cnt (second pss)]
    (mapcat (fn [r] (map (fn [c] [r c]) (range col-cnt))) (range row-cnt))))

(fg/defevolverfn :children
  (if (not (nil? (get-reason)))
    (let [pss (get-property [:this] :physical-screen-size)
          child-count (count (get-property [:this] :children))
          needed-count (* (first pss) (second pss))]
      (if (< child-count needed-count)
        (merge
          (into {} (map (fn [coord]
                          (let [cid (apply gen-cell-id coord)
                                c (cid old-children)]
                            [cid (if c c (fg/defcomponent
                                           (get-property [:this] :cell-prototype)
                                           cid
                                           {;:physical-screen-coord coord
                                            }))]))
                        ;; Multiply by margin to allow more children in advance and avoid adding
                        ;; new children (expensive operation) too often
                        (let [margin (if-let [m (get-property [:this] :child-count-dim-margin)] m 2)]
                          (all-coords-2d (mapv #(* margin %) pss)))))
          old-children)
        old-children))
    old-children))

(fg/defevolverfn :physical-screen-size
  (let [clip-size (get-property [:this] :clip-size)]
    ;; inc because for example screen size of 1.2 may actually occupy 3 cells of 1 (one partially for 0.1, one fully, and one last for 0.1)
    [(int (inc (Math/ceil (double (/ (m/y clip-size) (get-property [:this] :avg-min-cell-h))))))
     (int (inc (Math/ceil (double (/ (m/x clip-size) (get-property [:this] :avg-min-cell-w))))))]))

(fg/defevolverfn :header-model-loc
 (if-let [cell-id (second (get-reason))]
   (let [as (get-property [:this cell-id] :atomic-state)
         model-coord (:model-coord as)
         cs (:clip-size as)
         pm (:position-matrix as)
         pmt (dec (count pm))]
     (if (not= model-coord cell/not-in-use-coord)
       (loop [d 0
              sizes (:sizes old-header-model-loc)
              positions (:positions old-header-model-loc)]
         (if (< d (count model-coord))
           (recur
             (inc d)
             (assoc-in sizes [d (nth model-coord d)] (m/mx-get cs d 0))
             (assoc-in positions [d (nth model-coord d)] (m/mx-get pm d pmt)))
           {:positions positions
            :sizes sizes}))
       old-header-model-loc))
   old-header-model-loc))

(fg/defevolverfn shift-header-model-loc-evolver :header-model-loc
  (if-let [cell-id (second (get-reason))]
    (let [as (get-property [:this cell-id] :atomic-state)
          model-coord (:model-coord as)]
      (if (not= model-coord cell/not-in-use-coord)
        (let [new-header-model-loc (header-model-loc-evolver component)]
          (loop [d 0
                 positions (:positions new-header-model-loc)]
            (if (< d (count model-coord))
              (recur
                (inc d)
                (let [dim-coord (nth model-coord d)
                      shift (-
                              (get-in (:sizes new-header-model-loc) [d dim-coord])
                              (get-in (:sizes old-header-model-loc) [d dim-coord]))]
                  (if (not= 0 shift)
                    (loop [i (inc dim-coord)
                           p positions]
                      (if (< i (count (nth p d)))
                        (recur
                          (inc i)
                          (update-in p [d i] + shift))
                        p))
                    positions)))
              (assoc new-header-model-loc :positions positions))))
        old-header-model-loc))
    old-header-model-loc))

(fg/defevolverfn :content-size
  (let [header-model-pos (:positions (get-property [:this] :header-model-loc))
        header-model-size (:sizes (get-property [:this] :header-model-loc))
        last-col (dec (count (first header-model-pos)))
        last-row (dec (count (second header-model-pos)))]
    (m/defpoint
      (+ (nth (first header-model-pos) last-col) (nth (first header-model-size) last-col))
      (+ (nth (second header-model-pos) last-row) (nth (second header-model-size) last-row)))))

(defn edge-search [range-size start pred]
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
      i)))

;; Accepts inclusive coords as parameters
(defn combine-ranges [s1 s2] (mapcat (fn [e1] (map (fn [e2] [e1 e2]) (range (first s2) (inc (second s2))))) (range (first s1) (inc (second s1)))))

;; Decrementing x+w and y+h to prepare for inclusive combine-ranges
(defn rects->coords [rects] (mapcat (fn [vr] (combine-ranges [(:x vr) (dec (+ (:x vr) (:w vr)))] [(:y vr) (dec (+ (:y vr) (:h vr)))])) rects))

(fg/defaccessorfn enough-cells? [component]
  (let [pss (get-property [:this] :physical-screen-size)
        child-count (count (get-property [:this] :children))
        needed-count (* (first pss) (second pss))]
    (>= child-count needed-count)))

;;; TODO This means :header-model-pos and :header-model-size should be combined in one property
;(fg/defaccessorfn dimension-headers-consistent? [component]
;  (let [header-model-pos (get-property [:this] :header-model-pos)
;        header-model-size (get-property [:this] :header-model-size)]
;    (= (map count header-model-pos) (map count header-model-size))))

(fg/defevolverfn :in-use-model
  (if (enough-cells? component);(and (enough-cells? component) (dimension-headers-consistent? component))
    (let [cs (get-property [:this] :clip-size)
          header-model-pos (:positions (get-property [:this] :header-model-loc))
          header-model-size (:sizes (get-property [:this] :header-model-loc))
          vpm (get-property [:this] :viewport-matrix)
          viewport-begin (mapv (fn [a] (* -1 a)) (v/mxtransf->vec vpm 2)) ;2-dimensional
          viewport-end (v/-mxtransf+point->vec vpm cs 2) ;2-dimensional
          search-fn (fn [d start for-begin]
                      (let [header-model-pos-d (nth header-model-pos d)
                            header-model-size-d (nth header-model-size d)
                            dim-range-size (count header-model-pos-d)
                            visible-screen-coord? (fn [coord]
                                                    (if (and (and (>= coord 0) (< coord dim-range-size)))
                                                      (let [screen-point-from (nth header-model-pos-d coord)
                                                            screen-point-to (+
                                                                              (nth header-model-pos-d coord)
                                                                              (nth header-model-size-d coord))]
                                                        (r/line&
                                                          (nth viewport-begin d) (nth viewport-end d)
                                                          screen-point-from screen-point-to))))
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
          dimensions (range (count viewport-begin))
          new-screen-area [(mapv #(search-fn % (nth (first old-screen-area) %) true) dimensions)
                           (mapv #(search-fn % (nth (second old-screen-area) %) false) dimensions)]

          nx1 (first (first new-screen-area))
          ny1 (second (first new-screen-area))
          nx2 (inc (first (second new-screen-area)))        ; Incrementing ..2 coords because it's inclusive
          ny2 (inc (second (second new-screen-area)))
          ox1 (first (first old-screen-area))
          oy1 (second (first old-screen-area))
          ox2 (inc (first (second old-screen-area)))
          oy2 (inc (second (second old-screen-area)))
          new-screen-rect {:x nx1 :y ny1 :w (- nx2 nx1) :h (- ny2 ny1)}
          old-screen-rect {:x ox1 :y oy1 :w (- ox2 ox1) :h (- oy2 oy1)}
          ;_ (println "old new screen rect" old-screen-rect new-screen-rect)
          vacant-rects (r/rect- new-screen-rect old-screen-rect)
          vacant-coords (rects->coords vacant-rects)
          ;_ (println "vacant-coords" vacant-coords)
          ;; TODO the below looks like extremely heavy and complex computation
          to-be-free-rects (r/rect- old-screen-rect new-screen-rect)
          to-be-free-coords (rects->coords to-be-free-rects)
          ;_ (println "to-be-free-coords" to-be-free-coords)
          old-cell-id->screen-coord (:cell-id->screen-coord old-in-use-model)
          to-be-free-cell-ids (if to-be-free-coords
                                (filter
                                  (fn [e] (not (nil? e)))   ;TODO ?????
                                  (map #(get (:screen-coord->cell-id old-in-use-model) %) to-be-free-coords))
                                (list))
          all-unused-cell-ids (distinct (concat to-be-free-cell-ids (map (fn [[k _v]] k) (filter
                                                                                           (fn [[k _v]] (not (k old-cell-id->screen-coord)))
                                                                                           (get-property [:this] :children)))))
          new-occupants  (if vacant-coords
                           (loop [vcs vacant-coords
                                  cids all-unused-cell-ids
                                  noc {}]
                             (if (not (empty? vcs))
                               (recur
                                 (next vcs)
                                 (next cids)
                                 (assoc noc (first cids) (first vcs)))
                               noc))
                           {})
          to-be-free-cell-ids (filter #(not (% new-occupants)) to-be-free-cell-ids)
          cell-id->screen-coord (apply dissoc (merge old-cell-id->screen-coord new-occupants) to-be-free-cell-ids)
          screen-coord->cell-id (into {} (map (fn [[k v]] [v k]) cell-id->screen-coord))]
      :in-use-model {:viewport-begin viewport-begin
                     :viewport-end viewport-end
                     :screen-area new-screen-area
                     :screen-coord->cell-id screen-coord->cell-id
                     :cell-id->screen-coord cell-id->screen-coord})
    old-in-use-model))

(fg/defaccessorfn dummy-value-provider [component model-row model-col]
  (str (get-property [:this] :id) "-" model-row "-" model-col))

(def initial-in-use-model {:viewport-begin [0 0]
                           :viewport-end [0 0]
                           :screen-area [[0 0] [0 0]]
                           :screen-coord->cell-id {[0 0] :cell-0-0}
                           :cell-id->screen-coord {:cell-0-0 [0 0]}})

(fg/defwidget "table"
  {:header-line-count [1 0]                             ; By default, 1 header row and 0 header columns
   :header-model-loc {:positions [[0] [0]]       ; By default, 1 cell (1 row header) starting at 0,0 of size 1,1
                      :sizes [[1] [1]]}
   :physical-screen-size [1 1]                          ; Determined by cell minimum size (a constant) and table clip size
   :avg-min-cell-w 0.75
   :avg-min-cell-h 0.375
   :child-count-dim-margin 2
   :in-use-model initial-in-use-model
   :screen->model identity                              ; This coord vector translation fn may take into account sorting/filtering etc.
   :value-provider dummy-value-provider
   :cell-prototype cell/cell
   ;; Features of component
   :has-mouse false
   :accepts-focus? false
   :focus-traversal-order nil
   :focus-state focus/clean-state
   :layout nil
   :coord-map nil
   :evolvers {:physical-screen-size physical-screen-size-evolver ; may be turned off for better performance (but :physical-screen-size would need to be enough)
              :children children-evolver                ; maintains enough child cells to always cover :physical-screen-size area
              :content-size content-size-evolver
              :header-model-loc header-model-loc-evolver
              :in-use-model in-use-model-evolver

              ;; Features of component that make sense here
              :visible component/visible-evolver
              :enabled component/enabled-evolver
              :has-mouse nil

              :accepts-focus? focus/simple-accepts-focus-evolver
              :focus-state focus/focus-state-evolver
              :focus-traversal-order nil

              :coord-map nil

              :preferred-size nil

              :clip-size layout/clip-size-evolver
              :position-matrix layout/position-matrix-evolver}}
  component/componentbase)