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
            [flatgui.util.rectmath :as r]
            [flatgui.util.vectorutil :as vu]
            [flatgui.inputchannels.mouse :as mouse]))

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

(def cell-id-prefix "cell")

(defn gen-cell-id [& dim] (keyword (str cell-id-prefix (apply str (map #(str "-" %) dim)))))

(defn all-coords-2d [pss]
  (let [row-cnt (vu/firstv pss)
        col-cnt (vu/secondv pss)]
    (mapcat (fn [r] (map (fn [c] [r c]) (range col-cnt))) (range row-cnt))))

(fg/defaccessorfn gen-children-regular [component pss old-children]
  (let [child-count (count (get-property [:this] :children))
        needed-count (* (vu/firstv pss) (vu/secondv pss))
        cell-prototype (get-property [:this] :cell-prototype)]
    (if (< child-count needed-count)
      (merge
        (into {} (map (fn [coord]
                        (let [cid (apply gen-cell-id coord)
                              c (cid old-children)]
                          [cid (if c c (fg/defcomponent cell-prototype cid {}))]))
                      ;; Multiply by margin to allow more children in advance and avoid adding
                      ;; new children (expensive operation) too often
                      (let [margin (if-let [m (get-property [:this] :child-count-dim-margin)] m 2)]
                        (all-coords-2d (mapv #(* margin %) pss)))))
        old-children)
      old-children)))

(fg/defaccessorfn get-cell-prototype-by-col [component mc-col model-column->cell-prototype]
  (if-let [prot (nth model-column->cell-prototype mc-col)] prot (get-property [:this] :cell-prototype)))

(fg/defaccessorfn gen-children-by-columns [component pss model-column->cell-prototype old-children]
  (let [col-count (count model-column->cell-prototype)
        existing-rows (/ (count (get-property [:this] :children)) col-count)
        needed-rows (vu/secondv pss)
        screen->model (get-property [:this] :screen->model)]
    (if (< existing-rows needed-rows)
      (merge
        (into {} (map (fn [coord]
                        (let [mc-col (vu/firstv (screen->model coord))
                              cid (apply gen-cell-id coord)
                              c (cid old-children)]
                          [cid (if c c (fg/defcomponent
                                         (get-cell-prototype-by-col component mc-col model-column->cell-prototype)
                                         cid {}))]))
                      ;; Multiply by margin to allow more children in advance and avoid adding
                      ;; new children (expensive operation) too often
                      (let [margin (if-let [m (get-property [:this] :child-count-dim-margin)] m 2)]
                        (all-coords-2d [(vu/firstv pss) (* needed-rows margin)]))))
        old-children)
      old-children)))

(fg/defevolverfn :children
  (if (not (nil? (get-reason)))
    (let [pss (get-property [:this] :physical-screen-size)]
      (if-let [model-column->cell-prototype (get-property [:this] :model-column->cell-prototype)]
        (gen-children-by-columns component pss model-column->cell-prototype old-children)
        (gen-children-regular component pss old-children)))
    old-children))

(fg/defevolverfn :physical-screen-size
  (let [clip-size (get-property [:this] :clip-size)
        model-column->cell-prototype (get-property [:this] :model-column->cell-prototype)]
    ;; inc because for example screen size of 1.2 may actually occupy 3 cells of 1 (one partially for 0.1, one fully, and one last for 0.1)
    [(if model-column->cell-prototype
       (count model-column->cell-prototype)
       (int (inc (Math/ceil (double (/ (m/x clip-size) (get-property [:this] :avg-min-cell-w)))))))
     (int (inc (Math/ceil (double (/ (m/y clip-size) (get-property [:this] :avg-min-cell-h))))))]))

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
    (assoc
      (if-let [dim-const-size (:fit-dim-const-size old-header-model-loc)] (assoc header-model-loc :fit-dim-const-size dim-const-size) header-model-loc)
      :fit-dim-to-size
      fit-dim-to-size)
    header-model-loc))

(fg/defaccessorfn retain-reatures [component old-header-model-loc header-model-loc do-sort]
  (let [hml-order (support-order component old-header-model-loc header-model-loc do-sort)
        hml-fit (support-fit-to-size component old-header-model-loc hml-order)]
    hml-fit))

(fg/defevolverfn :header-model-loc
 (if-let [cell-id (vu/secondv (get-reason))]
   (let [no-order? (fn [d] (nil? (nth (:order old-header-model-loc) d)))
         as (get-property [:this cell-id] :atomic-state)
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
           (retain-reatures component old-header-model-loc {:positions positions :sizes sizes} false)))
       old-header-model-loc))
   (retain-reatures component old-header-model-loc old-header-model-loc true)))

(fg/defaccessorfn process-container-resize [component header-model-loc]
  (let [fit-dim-to-size (:fit-dim-to-size header-model-loc)
        fit-dim-const-size (:fit-dim-const-size header-model-loc) ;;TODO weights (0..1) rather than true/false
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
            (let [last-i (dec (count (nth p d)))
                  old-d-size (+ (get-in p [d last-i]) (+ (get-in s [d last-i])))
                  new-d-size (m/mx-get container-size d 0)
                  d-diff (- new-d-size old-d-size)]
              (if (not= d-diff 0)
                (if (nth fit-dim-to-size d)
                  (let [d-fit-const (nth fit-dim-const-size d)
                        sizes-d (nth sizes d)
                        count-sizes-d (count sizes-d)
                        range-sizes-d (range count-sizes-d)
                        sizes-d-fn (fn [%] (if (nth d-fit-const %) 0 (nth sizes-d %)))
                        total-size (apply + (map sizes-d-fn range-sizes-d))
                        d-weight (mapv (fn [%] (if (not= (double total-size) 0.0) (/ (sizes-d-fn %) total-size) 0)) range-sizes-d)
                        size-adj (mapv #(* % d-diff) d-weight)
                        pos-adj (loop [i 0
                                       tot-adj 0
                                       pa []]
                                  (if (< i count-sizes-d)
                                    (recur
                                      (inc i)
                                      (+ tot-adj (nth size-adj i))
                                      (assoc pa i tot-adj))
                                    pa))]
                    (recur
                      (inc d)
                      (update p d (fn [pos-d] (mapv (fn [i] (+ (nth pos-d i) (nth pos-adj i))) (range count-d))))
                      (update s d (fn [size-d] (mapv (fn [i] (+ (nth size-d i) (nth size-adj i))) (range count-d))))))
                  (recur
                    (inc d)
                    p
                    s))
                (recur (inc d) p s)))
            (recur (inc d) p s)))
        (assoc header-model-loc :positions p :sizes s)))))

;; NOTE: This implementation does not support the case when const-size column
;;       goes after resized column in :fit-dim-to-size mode
(fg/defevolverfn shift-header-model-loc-evolver :header-model-loc
  (if-let [cell-id (vu/secondv (get-reason))]
    (let [as (get-property [:this cell-id] :atomic-state)
          model-coord (:model-coord as)]
      (if (not= model-coord cell/not-in-use-coord)
        (let [new-header-model-loc (header-model-loc-evolver component)
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
                            (update-in p [d i] + shift deduct))
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
              (assoc new-header-model-loc :positions positions :sizes sizes))))
        (header-model-loc-evolver component)))
    (process-container-resize component (header-model-loc-evolver component))))

(fg/defaccessorfn modify-hml [component r old-header-model-loc pos-d-fn sizes-d-fn ord-d-fn]
  (let [r (get-reason)
        d (:d r)
        sizes (:sizes old-header-model-loc)
        positions (:positions old-header-model-loc)
        new-hml {:positions (update positions d pos-d-fn)
                 :sizes (update sizes d sizes-d-fn)
                 :order (if-let [order (:order old-header-model-loc)] (update order d ord-d-fn))
                 :fit-dim-to-size (if-let [fit (:fit-dim-to-size old-header-model-loc)] fit)
                 :fit-dim-const-size (if-let [fit (:fit-dim-const-size old-header-model-loc)] fit)}]
    (retain-reatures component new-hml new-hml true)))

;; TODO present macros make it impossible to combine evolvers, for example non-shifting + cmd
;;
(fg/defevolverfn shift-cmd-header-model-loc-evolver :header-model-loc
  (let [r (get-reason)]
    (if (map? r)
      (condp = (:cmd (get-reason))
        :add (modify-hml component r old-header-model-loc
                         (fn [d-pos] (conj d-pos (:pos r))) (fn [d-sizes] (conj d-sizes (:size r))) (fn [d-ord] (conj d-ord (count d-ord))))
        :add-bulk (modify-hml component r old-header-model-loc
                         (fn [d-pos] (vec (concat d-pos (:pos r)))) (fn [d-sizes] (vec (concat d-sizes (:size r)))) (fn [d-ord] (vec (concat d-ord (count d-ord)))))
        :clear (modify-hml component r old-header-model-loc
                           (fn [_d-pos] []) (fn [_d-sizes] []) (fn [_d-ord] nil))
        old-header-model-loc)
      (shift-header-model-loc-evolver component))))

(fg/defevolverfn :content-size
  (let [header-model-pos (:positions (get-property [:this] :header-model-loc))
        header-model-size (:sizes (get-property [:this] :header-model-loc))
        last-col (dec (count (vu/firstv header-model-pos)))
        last-row (dec (count (vu/secondv header-model-pos)))]
    (m/defpoint
      (if (>= last-col 0) (+ (nth (vu/firstv header-model-pos) last-col) (nth (vu/firstv header-model-size) last-col)) 0)
      (if (>= last-row 0) (+ (nth (vu/secondv header-model-pos) last-row) (nth (vu/secondv header-model-size) last-row)) 0))))

(defn edge-search [range-size start pred]
  (loop [dir-dist [(if (< start (dec range-size)) 1 -1) 1]
         i start]
    (if (and
          (and (>= i 0) (< i range-size))
          (not (pred i)))
      (recur
        (let [dir (vu/firstv dir-dist)
              dist (vu/secondv dir-dist)
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
(defn combine-ranges [s1 s2] (mapcat (fn [e1] (map (fn [e2] [e1 e2]) (range (vu/firstv s2) (inc (vu/secondv s2))))) (range (vu/firstv s1) (inc (vu/secondv s1)))))

;; Decrementing x+w and y+h to prepare for inclusive combine-ranges
(defn rects->coords [rects] (mapcat (fn [vr] (combine-ranges [(:x vr) (dec (+ (:x vr) (:w vr)))] [(:y vr) (dec (+ (:y vr) (:h vr)))])) rects))

(fg/defaccessorfn enough-cells? [component]
  (let [pss (get-property [:this] :physical-screen-size)
        child-count (count (get-property [:this] :children))
        needed-count (* (vu/firstv pss) (vu/secondv pss))]
    (>= child-count needed-count)))

(fg/defaccessorfn dimensions-non-empty? [component]
  (let [header-model-pos (:positions (get-property [:this] :header-model-loc))]
    (not (some empty? header-model-pos))))

(defn- fits-in-model [coord header-model-pos dimensions]
  (not (some (fn [d] (> (nth coord d) (count (nth header-model-pos d)))) dimensions)))

(defn occupy-cells-regular [vacant-coords all-unused-cell-ids header-model-pos dimensions]
  (loop [vcs vacant-coords
         cids all-unused-cell-ids
         noc {}]
    (if (not (empty? vcs))
      (recur
        (next vcs)
        (next cids)
        (let [coord (first vcs)]
          (if (fits-in-model coord header-model-pos dimensions)
            (assoc noc (first cids) coord)
            noc)))
      noc)))

(fg/defaccessorfn find-first-suitable [unused-cids cell-prototype]
  (if-let [cid (some
                 (fn [id] (if (= (get-property [:this id] :widget-type) (:widget-type cell-prototype)) id))
                 unused-cids)]
    cid
    (throw
      (IllegalStateException.
        (str
          "Insufficient amount of cells generated, cannot find free cell of type "
          (:widget-type cell-prototype))))))

(fg/defaccessorfn occupy-cells-by-columns [component model-column->cell-prototype vacant-coords all-unused-cell-ids header-model-pos dimensions]
  (let [screen->model (get-property [:this] :screen->model)]
    (loop [vcs vacant-coords
           unused-cids (set all-unused-cell-ids)
           noc {}]
      (if (not (empty? vcs))
        (let [coord (first vcs)
              mc-col (vu/firstv (screen->model coord))
              cell-prototype (get-cell-prototype-by-col component mc-col model-column->cell-prototype)
              cid (find-first-suitable unused-cids cell-prototype)]
          (recur
            (next vcs)
            (disj unused-cids cid)
            (if (fits-in-model coord header-model-pos dimensions)
              (assoc noc cid coord)
              noc)))
        noc))))

(fg/defevolverfn :in-use-model
  (let [dim-non-empty (dimensions-non-empty? component)]
    (if (and
          (enough-cells? component)
          dim-non-empty)
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
            new-screen-area [(mapv #(search-fn % (nth (vu/firstv old-screen-area) %) true) dimensions)
                             (mapv #(search-fn % (nth (vu/secondv old-screen-area) %) false) dimensions)]

            nx1 (vu/firstv (vu/firstv new-screen-area))
            ny1 (vu/secondv (vu/firstv new-screen-area))
            nx2 (inc (vu/firstv (vu/secondv new-screen-area)))        ; Incrementing ..2 coords because it's inclusive
            ny2 (inc (vu/secondv (vu/secondv new-screen-area)))
            ox1 (vu/firstv (vu/firstv old-screen-area))
            oy1 (vu/secondv (vu/firstv old-screen-area))
            ox2 (inc (vu/firstv (vu/secondv old-screen-area)))
            oy2 (inc (vu/secondv (vu/secondv old-screen-area)))
            new-screen-rect {:x nx1 :y ny1 :w (- nx2 nx1) :h (- ny2 ny1)}
            old-screen-rect {:x ox1 :y oy1 :w (- ox2 ox1) :h (- oy2 oy1)}
            ;_ (println "old new screen rect" old-screen-rect new-screen-rect)
            vacant-rects (r/rect- new-screen-rect old-screen-rect)
            vacant-coords (rects->coords vacant-rects)
            ;_ (println "vacant-coords" vacant-coords)
            ;; TODO the below looks like extremely heavy and complex computation
            to-be-free-rects (r/rect- old-screen-rect new-screen-rect)
            to-be-free-coords (rects->coords to-be-free-rects)
            ;_ (println "old->new scr rect" old-screen-rect new-screen-rect)
            ;_ (println "to-be-free-rects" to-be-free-rects "to-be-free-coords" to-be-free-coords)
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
                             (if-let [mc->prot (get-property [:this] :model-column->cell-prototype)]
                               (occupy-cells-by-columns component mc->prot vacant-coords all-unused-cell-ids header-model-pos dimensions)
                               (occupy-cells-regular vacant-coords all-unused-cell-ids header-model-pos dimensions))
                             {})
            to-be-free-cell-ids (filter #(not (% new-occupants)) to-be-free-cell-ids)
            cell-id->screen-coord (apply dissoc (merge old-cell-id->screen-coord new-occupants) to-be-free-cell-ids)
            screen-coord->cell-id (into {} (map (fn [[k v]] [v k]) cell-id->screen-coord))]
        :in-use-model {:viewport-begin viewport-begin
                       :viewport-end viewport-end
                       :screen-area new-screen-area
                       :screen-coord->cell-id screen-coord->cell-id
                       :cell-id->screen-coord cell-id->screen-coord})
      (if dim-non-empty old-in-use-model empty-in-use-model))))

;; Cell-by-cell selection: each cell catches mouse event to determine selection
;; Caution: selection id not kept up to date with :header-model-loc and may
;;          become outdated for example after rows removed
(fg/defevolverfn cbc-selection :selection
  (if-let [cell-id (vu/secondv (get-reason))]
    (let [as (get-property [:this cell-id] :atomic-state)
          mc (:model-coord as)]
      (if (not= mc cell/not-in-use-coord)
        (let [selected (:selected as)]
          (loop [d 0
                 s old-selection]
            (if (< d (count mc))
              (recur
                (inc d)
                (let [d-coord (nth mc d)]
                  (update s d (fn [sel-d]
                                (let [without-d-coord (remove (fn [e] (= e d-coord)) sel-d)]
                                  (if selected (conj without-d-coord d-coord) without-d-coord))))))
              s)))
        old-selection))
    old-selection))

;; This is for direct-selection mode which means table catches mouse events to determine clicked row
;; TODO why is this slower than cbc-selection ?
(fg/defevolverfn direct-selection :selection
 (if (and (mouse/left-mouse-button? component) (mouse/mouse-pressed? component))
   (let [y (mouse/get-mouse-rel-y component)
         d 1 ; d 1 means row
         positions-d (nth (:positions (get-property [:this] :header-model-loc)) d)
         count-pos-d (count positions-d)
         d-coord (loop [c 0]
                   (if (and (< c count-pos-d) (> y (nth positions-d c)))
                     (recur (inc c))
                     (dec c)))]
     (if (< d-coord count-pos-d)
       (let [sel-d (nth old-selection d)
             was-selected (some (fn [e] (= d-coord e)) sel-d)]
         (if was-selected
           ;; This models single selection
           (assoc old-selection d nil)
           (assoc old-selection d (list d-coord))))
       old-selection))
   old-selection))

(fg/defaccessorfn dummy-value-provider [component model-row model-col]
  (str (get-property [:this] :id) "-" model-row "-" model-col))


(fg/defwidget "table"
  {;:header-line-count [1 0] not implemented yet                             ; By default, 1 header row and 0 header columns
   :header-model-loc {:positions [[0] [0]]       ; By default, 1 cell (1 row header) starting at 0,0 of size 1,1
                      :sizes [[1] [1]]}
   :physical-screen-size [1 1]                          ; Determined by cell minimum size (a constant) and table clip size
   :avg-min-cell-w 0.75
   :avg-min-cell-h 0.375
   :child-count-dim-margin 2
   :model-column->cell-prototype nil ; If defined then vector, each element - column cell prototype or nil to use :cell-prototype
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