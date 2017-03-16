; Copyright (c) 2017 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns flatgui.widgets.table2.table-test
  (:require [flatgui.base :as fg]
            [flatgui.widgets.table2.table :as table]
            [clojure.test :as test]
            [flatgui.test :as fgtest]
            [flatgui.util.matrix :as m])
  (:import (flatgui.core.engine ClojureContainerParser Container IResultCollector)))


(test/deftest all-coords-2d-test
  (test/is (= #{[0 0] [0 1] [0 2] [1 0] [1 1] [1 2]} (set (table/all-coords-2d [2 3])))))

(test/deftest add-remove-children-test
  (let [_ (fg/defevolverfn :physical-screen-size (if (get-reason) [(:r (get-reason)) (:c (get-reason))] old-physical-screen-size))
        container (fg/defroot
                    {:id :main
                     :physical-screen-size [0 0]
                     :children {}
                     :evolvers {:physical-screen-size physical-screen-size-evolver
                                :children table/children-evolver}})
        results (atom {})
        added-uids (atom #{})
        removed-uids (atom #{})
        result-collector (proxy [IResultCollector] []
                           (appendResult [_parentComponentUid, _path, node, newValue]
                             (swap! results (fn [r] (if (and (= 0 (.getComponentUid node)) (= :children (.getPropertyId node)))
                                                      newValue
                                                      r))))
                           (componentAdded [_parentComponentUid componentUid] (swap! added-uids (fn [a] (conj a componentUid))))
                           (componentRemoved [componentUid] (swap! removed-uids (fn [r] (conj r componentUid))))
                           (postProcessAfterEvolveCycle [_a _m]))
        container-engine (Container.
                           (ClojureContainerParser.)
                           result-collector
                           container)
        _ (.evolve container-engine [:main] {:r 3 :c 3})
        res1 @results
        _ (.evolve container-engine [:main] {:r 2 :c 3})
        res2 @results]
    (test/is (= 9 (count res1)))
    (test/is (= 6 (count res2)))
    (test/is (=
               #{:cell-0-0 :cell-0-1 :cell-0-2 :cell-1-0 :cell-1-1 :cell-1-2 :cell-2-0 :cell-2-1 :cell-2-2}
               (set (map (fn [[k v]] (if (= k (:id v)) k)) res1))))
    (test/is (=
               #{:cell-0-0 :cell-0-1 :cell-0-2 :cell-1-0 :cell-1-1 :cell-1-2}
               (set (map (fn [[k v]] (if (= k (:id v)) k)) res2))))
    (test/is (=
               #{[0 0] [0 1] [0 2] [1 0] [1 1] [1 2]}
               (set (map (fn [[_k v]] (:physical-screen-coord v)) res2))))
    (test/is (= '(0 1 2 3 4 5 6 7 8 9) (sort @added-uids)))
    (test/is (= 3 (count @removed-uids)))))

(test/deftest physical-screen-size-test
  (let [container (fg/defroot
                    {:id        :main
                     :clip-size (m/defpoint 10 5)
                     :min-cell-h 0.9
                     :min-cell-w 3
                     :evolvers {:physical-screen-size table/physical-screen-size-evolver}
                     :children  {}})
        res (fgtest/evolve container :physical-screen-size nil)]
    (test/is (= [6 4] res))))

(test/deftest header-model-pos-size-test
  (let [results (atom {})
        result-collector (proxy [IResultCollector] []
                           (appendResult [_parentComponentUid, _path, node, newValue]
                             (swap! results (fn [r] (assoc r (.getPropertyId node) newValue))))
                           (componentAdded [_parentComponentUid _componentUid])
                           (componentRemoved [_componentUid])
                           (postProcessAfterEvolveCycle [_a _m]))
        init-w 2
        init-h 1
        init-cs (m/defpoint init-w init-h)
        init-pm-0 (mapv #(m/translation (* init-w %) 0) (range 3))
        init-pm-1 (mapv #(m/translation (* init-w %) init-h) (range 3))
        _ (fg/defevolverfn :position-matrix (if-let [pm (:position-matrix (get-reason))] pm old-position-matrix))
        _ (fg/defevolverfn :clip-size (if-let [cs (:clip-size (get-reason))] cs old-clip-size))
        cell-evolvers {:position-matrix position-matrix-evolver :clip-size clip-size-evolver}
        container (fg/defroot
                    {:id        :main
                     :screen->model identity
                     :header-model-pos-size {:positions [[0 2 4] [0 1]]
                                             :sizes [[2 2 2] [1 1]]}
                     :evolvers {:header-model-pos-size table/header-model-pos-size-evolver}
                     :children  {:cell-0-0 {:id :cell-0-0 :screen-coord [0 0] :clip-size init-cs :position-matrix (nth init-pm-0 0) :evolvers cell-evolvers}
                                 :cell-0-1 {:id :cell-0-1 :screen-coord [1 0] :clip-size init-cs :position-matrix (nth init-pm-0 1) :evolvers cell-evolvers}
                                 :cell-0-2 {:id :cell-0-2 :screen-coord [2 0] :clip-size init-cs :position-matrix (nth init-pm-0 2) :evolvers cell-evolvers}
                                 :cell-1-0 {:id :cell-1-0 :screen-coord [0 1] :clip-size init-cs :position-matrix (nth init-pm-1 0) :evolvers cell-evolvers}
                                 :cell-1-1 {:id :cell-1-1 :screen-coord [1 1] :clip-size init-cs :position-matrix (nth init-pm-1 1) :evolvers cell-evolvers}
                                 :cell-1-2 {:id :cell-1-2 :screen-coord [2 1] :clip-size init-cs :position-matrix (nth init-pm-1 2) :evolvers cell-evolvers}}})
        container-engine (Container.
                           (ClojureContainerParser.)
                           result-collector
                           container)
        _ (.evolve container-engine [:main :cell-0-1] {:clip-size (m/defpoint 3 1)})
        res0 (:header-model-pos-size @results)
        _ (.evolve container-engine [:main :cell-0-1] {:clip-size (m/defpoint 3 2)})
        res1 (:header-model-pos-size @results)
        _ (.evolve container-engine [:main :cell-0-1] {:clip-size (m/defpoint 4 3)})
        res2 (:header-model-pos-size @results)
        _ (.evolve container-engine [:main :cell-0-2] {:position-matrix (m/translation 6 0) :clip-size (m/defpoint 3 2)})
        res3 (:header-model-pos-size @results)
        _ (.evolve container-engine [:main :cell-0-2] {:position-matrix (m/translation 7 0)})
        res4 (:header-model-pos-size @results)]
    (test/is (= {:positions [[0 2 4] [0 1]]
                 :sizes [[2 3 2] [1 1]]} res0))
    (test/is (= {:positions [[0 2 4] [0 1]]
                 :sizes [[2 3 2] [2 1]]} res1))
    (test/is (= {:positions [[0 2 4] [0 1]]
                 :sizes [[2 4 2] [3 1]]} res2))
    (test/is (= {:positions [[0 2 6] [0 1]]
                 :sizes [[2 4 3] [2 1]]} res3))
    (test/is (= {:positions [[0 2 7] [0 1]]
                 :sizes [[2 4 3] [2 1]]} res4))))


;;;
;;; Sorting
;;;

