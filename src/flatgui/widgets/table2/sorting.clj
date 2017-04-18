; Copyright (c) 2017 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc    "Sorting support for table"
      :author "Denys Lebediev"}
flatgui.widgets.table2.sorting
  (:require [flatgui.base :as fg]
            [flatgui.util.vecmath :as vecmath])
  (:import (java.util Comparator Arrays)))

(defn create-comparator [value-provider coord d-index mode]
  (proxy [Comparator] []
    (compare [d1 d2]
      (let [o1 (value-provider (assoc coord d-index d1))
            o2 (value-provider (assoc coord d-index d2))]
        (if (and (nil? o1) (nil? o2))
          0
          (let [o1n (if (nil? o1) (if (number? o2) Double/NaN "") o1)
                o2n (if (nil? o2) (if (number? o1) Double/NaN "") o2)]
            (if (= mode :desc) (.compareTo o2n o1n) (.compareTo o1n o2n))))))))

(defn sort-dim [order-arr comparators dvps key-ind range-start range-end]
  (let [comparator (nth comparators key-ind)
        _ (Arrays/sort order-arr range-start range-end comparator)
        sorted order-arr
        subranges (vecmath/find-subranges (mapv (nth dvps key-ind) (vec sorted)))
        next-key-ind (inc key-ind)]
    (if (< next-key-ind (count comparators))
      (loop [o sorted
             i 0]
        (if (< i (count subranges))
          (recur
            (let [sr (nth subranges i)] (sort-dim order-arr comparators dvps (inc key-ind) (nth sr 0) (+ (nth sr 0) (nth sr 1))))
            (inc i))
          o))
      sorted)))

(fg/defaccessorfn tablesort [component order dim-counts]
  (if (get-property [:this] :resort?)
    (let [keys (get-property [:this] :keys)
          vp (get-property [:this] :value-provider)]
      (mapv
        (fn [d] (if-let [keys-d (nth keys d)]
                  (let [order-d (if-let [o (nth order d)] o (range (nth dim-counts d)))
                        ;; This is good only for 2-dim sorting by columns
                        comparators (mapv (fn [k] (create-comparator vp [(nth k 0) nil] 1 (nth k 1))) keys-d)
                        dvps (mapv (fn [k] (fn [dimcoord] (vp [(nth k 0) dimcoord]))) keys-d)]
                    (vec (sort-dim (to-array order-d) comparators dvps 0 0 (count order-d))))))
        (range (count order))))
    order))

(fg/defevolverfn resort? true)

(fg/defevolverfn :screen->model
  (if-let [order (:order (get-property [:this] :header-model-loc))]
    (fn [sc]
      (mapv
        (fn [d] (let [order-d (nth order d)
                      d-coord (nth sc d)]
                  (if (and order-d (>= d-coord 0) (< d-coord (count order-d)))
                    (nth order-d d-coord)
                    d-coord)))
        (range 0 (count sc))))
    identity))

(def sorting
  {:header-model-loc {:order [[0] [0]]}
   :keys [[] []] ; Model coords by which sorting should be performed
   :value-provider (fn [model-coord] (throw (UnsupportedOperationException. (str "Not implemented " model-coord))))
   :resort? false
   :evolvers {:resort? resort?-evolver
              :screen->model screen->model-evolver}})