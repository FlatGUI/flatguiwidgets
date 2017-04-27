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
            [flatgui.widgets.table2.sorting :as sorting]
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

(defn compute-positions-d [sizes-d order-d]
  (let [cnt-d (count sizes-d)]
    (loop [sp (make-array Double cnt-d)
           pos 0
           i 0]
      (if (< i cnt-d)
        (let [model-i (if order-d (nth order-d i) i)]
          (recur
            (do (aset sp model-i (Double/valueOf (double pos))) sp)
            (+ pos (nth sizes-d model-i))
            (inc i)))
        (vec sp)))))

(defn compute-positions [sizes positions order]
  (mapv
    (fn [d]
      (if-let [order-d (nth order d)]
        (compute-positions-d (nth sizes d) order-d)
        (nth positions d)))
    (range (count sizes))))

(fg/defaccessorfn support-order [component old-header-model-loc header-model-loc do-sort]
  (if-let [old-order (:order old-header-model-loc)]
    (let [sizes (:sizes header-model-loc)
          positions (:positions header-model-loc)
          new-order (if do-sort (sorting/tablesort component old-order (mapv count sizes)) old-order)]
      (if (some #(not (nil? %)) new-order)
        (let [ordered-positions (compute-positions sizes positions new-order)]
          (assoc
            header-model-loc
            :order new-order
            :ordered-positions ordered-positions))
        (dissoc header-model-loc :order :ordered-positions)))
    header-model-loc))

(fg/defaccessorfn support-fit-to-size [component old-header-model-loc header-model-loc]
  (if-let [fit-dim-to-size (:fit-dim-to-size old-header-model-loc)]
    (assoc header-model-loc :fit-dim-to-size fit-dim-to-size)
    header-model-loc))

(fg/defaccessorfn retain-reatures [component old-header-model-loc header-model-loc do-sort]
  (let [hml-order (support-order component old-header-model-loc header-model-loc do-sort)
        hml-fit (support-fit-to-size component old-header-model-loc hml-order)]
    hml-fit))

(fg/defevolverfn :header-model-loc
 (if-let [cell-id (second (get-reason))]
   (let [no-order? (fn [d] (nil? (nth (:order old-header-model-loc) d)))
         as (get-property [:this cell-id] :atomic-state)
         ;_ (println "HML because of cell " cell-id as)
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
             (if (< (nth model-coord d) (count (nth sizes d)))
               (assoc-in sizes [d (nth model-coord d)] (m/mx-get cs d 0))
               sizes)
             (if (no-order? d)
               (if (< (nth model-coord d) (count (nth positions d)))
                 (assoc-in positions [d (nth model-coord d)] (m/mx-get pm d pmt))
                 positions)
               (assoc positions d (compute-positions-d (nth sizes d) nil))))
           ;; Do not resort if processing for a cell reason
           (let [;_ (println "HML" old-header-model-loc "-> pos" positions "sizes" sizes "->" (retain-reatures component old-header-model-loc {:positions positions :sizes sizes} false))
                 ]
             (retain-reatures component old-header-model-loc {:positions positions :sizes sizes} false))
           ))
       old-header-model-loc))
   (retain-reatures component old-header-model-loc old-header-model-loc true)))

(fg/defaccessorfn process-container-resize [component header-model-loc]
  (let [fit-dim-to-size (:fit-dim-to-size header-model-loc)
        positions (:positions header-model-loc)
        sizes (:sizes header-model-loc)
        container-size (get-property [:this] :clip-size)
        dims (count positions)]
    (loop [d 0
           p positions
           s sizes]
      (if (< d dims)
        (let [count-d (count (nth p d))]
          (if (pos? count-d)
            (let [;_ (println "p" p "s" s "container-size" container-size)
                  last-i (dec (count (nth p d)))
                  old-d-size (+ (get-in p [d last-i]) (+ (get-in s [d last-i])))
                  new-d-size (m/mx-get container-size d 0)
                  d-diff (- new-d-size old-d-size)
                  ;_ (println "---------------------d=" d " ===> d-diff = " d-diff "old-d-size" old-d-size "new-d-size" new-d-size)
                  ]
              (if (not= d-diff 0)
                (let [d-fit (nth fit-dim-to-size d)
                      sizes-d (nth sizes d)
                      total-size (apply + sizes-d)
                      d-weight (if (not= total-size 0) (mapv #(/ % total-size) sizes-d) 0)
                      size-adj (mapv #(* % d-diff) d-weight)
                      pos-adj (loop [i 0
                                     tot-adj 0
                                     pa []]
                                (if (< i (count sizes-d))
                                  (recur
                                    (inc i)
                                    (+ tot-adj (nth size-adj i))
                                    (assoc pa i tot-adj))
                                  pa))
                      ;_ (println "d=" d "size-adj" size-adj "pos-adj" pos-adj)
                      ]
                  (recur
                    (inc d)
                    (if d-fit
                      (update p d (fn [pos-d] (mapv (fn [i] (+ (nth pos-d i) (nth pos-adj i))) (range count-d))))
                      p)
                    (if d-fit
                      (update s d (fn [size-d] (mapv (fn [i] (+ (nth size-d i) (nth size-adj i))) (range count-d))))
                      s)))
                (recur (inc d) p s)))
            (recur (inc d) p s)))
        (let [;_ (println "ADJ" header-model-loc "-> pos" p "sizes" s )
              ]

          (assoc header-model-loc :positions p :sizes s))
        ))))

(fg/defevolverfn shift-header-model-loc-evolver :header-model-loc
  (if-let [cell-id (second (get-reason))]
    (let [as (get-property [:this cell-id] :atomic-state)
          model-coord (:model-coord as)]
      (if (not= model-coord cell/not-in-use-coord)
        (let [new-header-model-loc (header-model-loc-evolver component)
              ;_ (println "S-HML" old-header-model-loc "->" new-header-model-loc)
              fit-dim-to-size (:fit-dim-to-size new-header-model-loc)]
          (loop [d 0
                 positions (:positions new-header-model-loc)
                 sizes (:sizes new-header-model-loc)]
            (if (< d (count model-coord))
              (let [dim-coord (nth model-coord d)
                    d-fit (nth fit-dim-to-size d)
                    shift (if (and
                                (< dim-coord (count (nth (:sizes new-header-model-loc) d)))
                                (< dim-coord (count (nth (:sizes old-header-model-loc) d))))
                            (-
                              (get-in (:sizes new-header-model-loc) [d dim-coord])
                              (get-in (:sizes old-header-model-loc) [d dim-coord]))
                            0)
                    ;_ (println "==shift = " shift)
                    total-remaining-size (if d-fit
                                           (loop [i (inc dim-coord) s 0]
                                             (if (< i (count (nth sizes d))) (recur (inc i) (+ s (get-in sizes [d i]))) s)))]
                (recur
                  (inc d)
                  ;; positions
                  (let []
                    (if (not= 0 shift)
                      (loop [i (inc dim-coord)
                             deduct 0
                             p positions]
                        (if (< i (count (nth p d)))
                          (recur
                            (inc i)
                            (if d-fit (- deduct (* shift (/ (get-in sizes [d i]) total-remaining-size))) deduct)
                            (let [_ (println "Processing d=" d "i=" i "shift=" shift "deduct=" deduct)]
                              (update-in p [d i] + shift deduct)))
                          p))
                      positions))
                  ;; sizes
                  (if d-fit
                    (loop [i (inc dim-coord)
                           s sizes]
                      (if (< i (count (nth s d)))
                        (recur
                          (inc i)
                          (update-in s [d i] (fn [si] (- si (* shift (/ si total-remaining-size))))))
                        s))
                    sizes)))
              (let [;_ (println "SHML>" old-header-model-loc "->" new-header-model-loc "->" (assoc new-header-model-loc :positions positions :sizes sizes))
                    ]
                (assoc new-header-model-loc :positions positions :sizes sizes)))))
        (header-model-loc-evolver component)))
    (process-container-resize component (header-model-loc-evolver component))))

;; TODO present macros make it impossible to combine evolvers, for example non-shifting + cmd
;;
(fg/defevolverfn shift-cmd-header-model-loc-evolver :header-model-loc
  (if (map? (get-reason))
    (condp = (:cmd (get-reason))
      :add (let [r (get-reason)
                 d (:d r)
                 sizes (:sizes old-header-model-loc)
                 positions (:positions old-header-model-loc)
                 new-hml {:positions (update positions d (fn [d-pos] (conj d-pos (:pos r))))
                          :sizes (update sizes d (fn [d-sizes] (conj d-sizes (:size r))))
                          :order (if-let [order (:order old-header-model-loc)] (update order d (fn [d-ord] (conj d-ord (count d-ord)))))}]
             (retain-reatures component new-hml new-hml true))
      old-header-model-loc)
    (shift-header-model-loc-evolver component)))

(fg/defevolverfn :content-size
  (let [header-model-pos (:positions (get-property [:this] :header-model-loc))
        header-model-size (:sizes (get-property [:this] :header-model-loc))
        last-col (dec (count (first header-model-pos)))
        last-row (dec (count (second header-model-pos)))]
    (m/defpoint
      (if (>= last-col 0) (+ (nth (first header-model-pos) last-col) (nth (first header-model-size) last-col)) 0)
      (if (>= last-row 0) (+ (nth (second header-model-pos) last-row) (nth (second header-model-size) last-row)) 0))))

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

(fg/defaccessorfn dimensions-non-empty? [component]
  (let [header-model-pos (:positions (get-property [:this] :header-model-loc))]
    (not (some empty? header-model-pos))))

(fg/defevolverfn :in-use-model
  (if (and
        (enough-cells? component)
        (dimensions-non-empty? component))
    (let [cs (get-property [:this] :clip-size)
          header-model-pos (:positions (get-property [:this] :header-model-loc))
          header-model-size (:sizes (get-property [:this] :header-model-loc))
          vpm (get-property [:this] :viewport-matrix)
          viewport-begin (mapv (fn [a] (* -1 a)) (v/mxtransf->vec vpm 2)) ;2-dimensional
          viewport-end (v/-mxtransf+point->vec vpm cs 2) ;2-dimensional
          screen->model (get-property [:this] :screen->model)
          dimensions (range (count viewport-begin))
          empty-coord (mapv (fn [_] 0) dimensions)
          search-fn (fn [d start for-begin]
                      (let [header-model-pos-d (nth header-model-pos d)
                            header-model-size-d (nth header-model-size d)
                            dim-range-size (count header-model-pos-d)
                            visible-screen-coord? (fn [scr-coord]
                                                    (let [  ;_ (println "SC" (assoc empty-coord d scr-coord) "MC" (screen->model (assoc empty-coord d scr-coord)) "d" d)
                                                          model-coord (nth (screen->model (assoc empty-coord d scr-coord)) d)]
                                                      (if (and (and (>= model-coord 0) (< model-coord dim-range-size)))
                                                        (let [screen-point-from (nth header-model-pos-d model-coord)
                                                              screen-point-to (+
                                                                                (nth header-model-pos-d model-coord)
                                                                                (nth header-model-size-d model-coord))]
                                                          (r/line&
                                                            (nth viewport-begin d) (nth viewport-end d)
                                                            screen-point-from screen-point-to)))))
                            viewport-begin-pred (fn [scr-coord] (and
                                                              (not (visible-screen-coord? (dec scr-coord)))
                                                              (visible-screen-coord? scr-coord)))
                            viewport-end-pred (fn [scr-coord] (and
                                                            (visible-screen-coord? scr-coord)
                                                            (not (visible-screen-coord? (inc scr-coord)))))
                            search-result (edge-search dim-range-size start (if for-begin viewport-begin-pred viewport-end-pred))]
                        (cond
                          (< search-result 0) 0
                          (>= search-result dim-range-size) (max (dec search-result) 0)
                          :else search-result)))

          old-screen-area (:screen-area old-in-use-model)
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
                                 (let [coord (first vcs)
                                       fits-in-model (not (some (fn [d] (> (nth coord d) (count (nth header-model-pos d)))) dimensions))]
                                   (if fits-in-model
                                     (assoc noc (first cids) coord)
                                     noc)))
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

(def empty-in-use-model {:viewport-begin [0 0]
                         :viewport-end [-1 -1]
                         :screen-area [[0 0] [-1 -1]]
                         :screen-coord->cell-id {}
                         :cell-id->screen-coord {}})

;; TODO  size distribution for dimension. If defined for columns for example
;; TODO  then total table w is fit to clip w, columns are resized proportionally

(fg/defwidget "table"
  {;:header-line-count [1 0] not implemented yet                             ; By default, 1 header row and 0 header columns
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