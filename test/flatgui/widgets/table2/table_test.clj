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
            [flatgui.widgets.table2.cell :as cell]
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
                     :clip-size (m/defpoint 10 5)
                     :screen->model identity
                     :viewport-matrix m/identity-matrix
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
    ;(test/is (=
    ;           #{[0 0] [0 1] [0 2] [1 0] [1 1] [1 2]}
    ;           (set (map (fn [[_k v]] (:physical-screen-coord v)) res2))))
    (test/is (= '(0 1 2 3 4 5 6 7 8 9) (sort @added-uids)))
    (test/is (= 3 (count @removed-uids)))))

(test/deftest physical-screen-size-test
  (let [container (fg/defroot
                    {:id        :main
                     :clip-size (m/defpoint 10 5)
                     :min-cell-h 0.9
                     :min-cell-w 3
                     :min-physical-cells [1 1]
                     :evolvers {:physical-screen-size table/physical-screen-size-evolver}
                     :children  {}})
        res (fgtest/evolve container :physical-screen-size nil)]
    (test/is (= [7 5] res))))

(test/deftest header-model-pos-size-test
  (let [results (atom {})
        cell-pms (atom {})
        cell-cs (atom {})
        result-collector (proxy [IResultCollector] []
                           (appendResult [_parentComponentUid, path, node, newValue]
                             (condp = (.getPropertyId node)
                               :position-matrix (swap! cell-pms (fn [r] (assoc r (last path) newValue)))
                               :clip-size (swap! cell-cs (fn [r] (assoc r (last path) newValue)))
                               (swap! results (fn [r] (assoc r (.getPropertyId node) newValue)))))
                           (componentAdded [_parentComponentUid _componentUid])
                           (componentRemoved [_componentUid])
                           (postProcessAfterEvolveCycle [_a _m]))
        init-w 2
        init-h 1
        init-cs (m/defpoint init-w init-h)
        init-pm-0 (mapv #(m/translation (* init-w %) 0) (range 3))
        init-pm-1 (mapv #(m/translation (* init-w %) init-h) (range 3))
        _ (fg/defevolverfn :position-matrix (if-let [pm (:position-matrix (get-reason))] pm (cell/position-matrix-evolver component)))
        _ (fg/defevolverfn :clip-size (if-let [cs (:clip-size (get-reason))] cs (cell/clip-size-evolver component)))
        cell-evolvers {:position-matrix position-matrix-evolver :clip-size clip-size-evolver}
        container (fg/defroot
                    {:id        :main
                     :screen->model identity
                     :header-model-pos [[0 2 4] [0 1]]
                     :header-model-size [[2 2 2] [1 1]]
                     :evolvers {:header-model-pos table/header-model-pos-evolver
                                :header-model-size table/header-model-size-evolver}
                     :children  {:cell-0-0 {:id :cell-0-0 :model-coord [0 0] :clip-size init-cs :position-matrix (nth init-pm-0 0) :evolvers cell-evolvers}
                                 :cell-1-0 {:id :cell-1-0 :model-coord [1 0] :clip-size init-cs :position-matrix (nth init-pm-0 1) :evolvers cell-evolvers}
                                 :cell-2-0 {:id :cell-2-0 :model-coord [2 0] :clip-size init-cs :position-matrix (nth init-pm-0 2) :evolvers cell-evolvers}
                                 :cell-0-1 {:id :cell-0-1 :model-coord [0 1] :clip-size init-cs :position-matrix (nth init-pm-1 0) :evolvers cell-evolvers}
                                 :cell-1-1 {:id :cell-1-1 :model-coord [1 1] :clip-size init-cs :position-matrix (nth init-pm-1 1) :evolvers cell-evolvers}
                                 :cell-2-1 {:id :cell-2-1 :model-coord [2 1] :clip-size init-cs :position-matrix (nth init-pm-1 2) :evolvers cell-evolvers}}})
        container-engine (Container.
                           (ClojureContainerParser.)
                           result-collector
                           container)
        retain (fn [m & keys] (into {} (map (fn [k] [k (k m)]) keys)))
        get-result (fn [] (retain @results :header-model-pos :header-model-size))
        _ (.evolve container-engine [:main :cell-1-0] {:clip-size (m/defpoint 3 1)})
        res0 (get-result)
        _ (.evolve container-engine [:main :cell-1-0] {:clip-size (m/defpoint 3 2)})
        res1 (get-result)
        _ (.evolve container-engine [:main :cell-1-0] {:clip-size (m/defpoint 4 3)})
        res2 (get-result)
        _ (.evolve container-engine [:main :cell-2-0] {:position-matrix (m/translation 6 0) :clip-size (m/defpoint 3 2)})
        res3 (get-result)
        _ (.evolve container-engine [:main :cell-2-0] {:position-matrix (m/translation 7 0)})
        res4 (get-result)
        _ (.evolve container-engine [:main :cell-0-1] {:position-matrix (m/translation 0 2)})
        res5 (get-result)]
    (test/is (= {:header-model-pos [[0 2 4] [0 1]]
                 :header-model-size [[2 3 2] [1 1]]}
                res0))
    (test/is (= {:header-model-pos [[0 2 4] [0 1]]
                 :header-model-size [[2 3 2] [2 1]]}
                res1))
    (test/is (= {:header-model-pos [[0 2 4] [0 1]]
                 :header-model-size [[2 4 2] [3 1]]}
                res2))
    (test/is (= {:header-model-pos [[0 2 6] [0 1]]
                 :header-model-size [[2 4 3] [2 1]]}
                res3))
    (test/is (= {:header-model-pos [[0 2 7] [0 1]]
                 :header-model-size [[2 4 3] [2 1]]}
                res4))
    (test/is (= {:header-model-pos [[0 2 7] [0 2]]
                 :header-model-size [[2 4 3] [2 1]]}
                res5))
    (test/is (= (m/translation 0 0) (:cell-0-0 @cell-pms)))
    (test/is (= (m/translation 2 0) (:cell-1-0 @cell-pms)))
    (test/is (= (m/translation 7 0) (:cell-2-0 @cell-pms)))
    (test/is (= (m/translation 0 2) (:cell-0-1 @cell-pms)))
    (test/is (= (m/translation 2 2) (:cell-1-1 @cell-pms)))
    (test/is (= (m/translation 7 2) (:cell-2-1 @cell-pms)))
    (test/is (= {:cell-0-0 (m/defpoint 2 2)
                 :cell-1-0 (m/defpoint 4 2)
                 :cell-2-0 (m/defpoint 3 2)
                 :cell-0-1 (m/defpoint 2 1)
                 :cell-1-1 (m/defpoint 4 1)
                 :cell-2-1 (m/defpoint 3 1)}
                @cell-cs))))

(test/deftest edge-search-test1
  (let [visit-track (atom [])
        pred (fn [i] (do (swap! visit-track (fn [s] (conj s i))) (= i 9)))
        search-result (table/edge-search 10 3 pred)]
    (test/is (= 9 search-result))
    (test/is (= [3 4 2 5 1 6 0 7 8 9] @visit-track))))

(test/deftest edge-search-test2
  (let [visit-track (atom [])
        pred (fn [i] (do (swap! visit-track (fn [s] (conj s i))) (= i 0)))
        search-result (table/edge-search 10 8 pred)]
    (test/is (= 0 search-result))
    (test/is (= [8 9 7 6 5 4 3 2 1 0] @visit-track))))

(test/deftest edge-search-test3
  (let [visit-track (atom [])
        pred (fn [i] (do (swap! visit-track (fn [s] (conj s i))) (= i 9999))) ;Will never be found
        search-result (table/edge-search 3 1 pred)]
    (test/is (= -1 search-result))
    (test/is (= [1 2 0] @visit-track))))

(test/deftest edge-search-test4
  (let [visit-track (atom [])
        pred (fn [i] (do (swap! visit-track (fn [s] (conj s i))) (= i 9999))) ;Will never be found
        search-result (table/edge-search 3 0 pred)]
    (test/is (= 3 search-result))
    (test/is (= [0 1 2] @visit-track))))

(test/deftest edge-search-test5
  (let [visit-track (atom [])
        pred (fn [i] (do (swap! visit-track (fn [s] (conj s i))) (= i 3)))
        search-result (table/edge-search 5 4 pred)]
    (test/is (= 3 search-result))
    (test/is (= [4 3] @visit-track))))

(test/deftest combine-ranges-test
  (test/is (= (list [1 3] [1 4] [2 3] [2 4]) (table/combine-ranges [1 2] [3 4])))
  (test/is (= (list [1 5] [1 6] [1 7] [2 5] [2 6] [2 7] [3 5] [3 6] [3 7] [4 5] [4 6] [4 7]) (table/combine-ranges [1 4] [5 7]))))

(test/deftest in-use-model-test
  (let [init-header-model-pos  [[0 2 3 4 6 8]
                                [0 1 3 4 7]]
        init-header-model-size [[2 1 1 2 2 2]
                                [1 2 1 3 3]]
        init-viewport-matrix (m/translation -2.5 -2.5)
        init-clip-size (m/defpoint 2.0 2.0)
        step-1 {:header-model-pos init-header-model-pos
                :header-model-size init-header-model-size
                :viewport-matrix init-viewport-matrix
                :clip-size init-clip-size}
        step-2 (assoc step-1
                 :viewport-matrix (m/translation -3.5 -4.5)
                 :clip-size (m/defpoint 4.0 3.0))
        step-3 (assoc step-2
                 :header-model-pos [[0 2 3 4 6 7]
                                    [0 1 3 4 8]]
                 :header-model-size [[2 1 1 2 1 3]
                                     [1 2 1 4 2]])
        step-4 (assoc step-3
                 :viewport-matrix (m/translation -8.5 -1.25)
                 :clip-size (m/defpoint 1.0 0.5))
        step-5 (assoc step-4
                 :viewport-matrix (m/translation 0 0)
                 :clip-size (m/defpoint 15 12))
        _ (fg/defevolverfn :header-model-pos (if (get-reason)
                                               (:header-model-pos (get-reason))
                                               old-header-model-pos))
        _ (fg/defevolverfn :header-model-size (if (get-reason)
                                                (:header-model-size (get-reason))
                                                old-header-model-size))
        _ (fg/defevolverfn :viewport-matrix (if (get-reason) (:viewport-matrix (get-reason)) old-viewport-matrix))
        _ (fg/defevolverfn :clip-size (if (get-reason) (:clip-size (get-reason)) old-clip-size))
        container (fg/defroot
                    {:id :main
                     :physical-screen-size [1 1]
                     :min-cell-w 1
                     :min-cell-h 1
                     :screen->model identity
                     :header-model-pos [[0] [0]]
                     :header-model-size [[1] [1]]
                     :viewport-matrix m/identity-matrix
                     :clip-size (m/defpoint 1 1)
                     :not-in-use #{}
                     :in-use-model {:viewport-begin [0 0]
                                    :viewport-end [1 1]
                                    :screen-area [[0 0] [0 0]]
                                    :vacant-screen-coords {}}
                     :cell-prototype cell/cell
                     :children {}
                     :evolvers {:physical-screen-size table/physical-screen-size-evolver
                                :header-model-pos header-model-pos-evolver
                                :header-model-size header-model-size-evolver
                                :viewport-matrix viewport-matrix-evolver
                                :clip-size clip-size-evolver
                                ;:not-in-use table/not-in-use-evolver
                                :in-use-model table/in-use-model-evolver
                                :children table/children-evolver}})
        results (atom {})
        cells-state (atom {})
        result-collector (proxy [IResultCollector] []
                           (appendResult [_parentComponentUid, path, node, newValue]
                             (cond
                               (= :in-use-model (.getPropertyId node)) (reset! results newValue)
                               (= 2 (count path)) (swap! cells-state (fn [a] (assoc-in a [(second path) (.getPropertyId node)] newValue)))))
                           (componentAdded [_parentComponentUid _componentUid])
                           (componentRemoved [_componentUid])
                           (postProcessAfterEvolveCycle [_a _m]))
        container-engine (Container.
                           (ClojureContainerParser.)
                           result-collector
                           container)
        _ (.evolve container-engine [:main] step-1)
        step1-result @results
        step1-cell-state @cells-state
        _ (.evolve container-engine [:main] step-2)
        step2-result @results
        step2-cell-state @cells-state
        _ (.evolve container-engine [:main] step-3)
        step3-result @results
        step3-cell-state @cells-state
        _ (.evolve container-engine [:main] step-4)
        step4-result @results
        step4-cell-state @cells-state
        _ (.evolve container-engine [:main] step-5)
        step5-result @results
        step5-cell-state @cells-state
        verify-cell (fn [scrx scry tx ty csx csy step-cell-state step-result step]
                      (let [cell-id (get (:screen-coord->cell-id step-result) [scrx scry])]
                        (test/is (= (m/translation tx ty) (get-in step-cell-state [cell-id :position-matrix])) (str "Cell " [scrx scry] " translation matrix failed on step " step))
                        (test/is (= (m/defpoint csx csy) (get-in step-cell-state [cell-id :clip-size])) (str "Cell " [scrx scry] " clip size failed on step " step))))]
    (test/is (= [2.5 2.5] (:viewport-begin step1-result)))
    (test/is (= [4.5 4.5] (:viewport-end step1-result)))
    (test/is (= [[1 1] [3 3]] (:screen-area step1-result)))
    (test/is (= (set (list [1 1] [2 1] [3 1] [1 2] [2 2] [3 2] [1 3] [2 3] [3 3])) (set (map (fn [[k _v]] k) (:screen-coord->cell-id step1-result)))))
    (test/is (nil?  (some (complement nil?) (map (fn [[k v]]   (if (= k (v (:cell-id->screen-coord step1-result))) nil [k v])) (:screen-coord->cell-id step1-result)))))
    (verify-cell 1 1 2 1 1 2 step1-cell-state step1-result 1)
    (verify-cell 2 1 3 1 1 2 step1-cell-state step1-result 1)
    (verify-cell 3 1 4 1 2 2 step1-cell-state step1-result 1)
    (verify-cell 1 2 2 3 1 1 step1-cell-state step1-result 1)
    (verify-cell 2 2 3 3 1 1 step1-cell-state step1-result 1)
    (verify-cell 3 2 4 3 2 1 step1-cell-state step1-result 1)
    (verify-cell 1 3 2 4 1 3 step1-cell-state step1-result 1)
    (verify-cell 2 3 3 4 1 3 step1-cell-state step1-result 1)
    (verify-cell 3 3 4 4 2 3 step1-cell-state step1-result 1)

    (test/is (= [3.5 4.5] (:viewport-begin step2-result)))
    (test/is (= [7.5 7.5] (:viewport-end step2-result)))
    (test/is (= [[2 3] [4 4]] (:screen-area step2-result)))
    (test/is (= (set (table/combine-ranges [2 4] [3 4])) (set (map (fn [[k _v]] k) (:screen-coord->cell-id step2-result)))))
    (verify-cell 2 3 3 4 1 3 step2-cell-state step2-result 2)
    (verify-cell 3 3 4 4 2 3 step2-cell-state step2-result 2)
    (verify-cell 4 3 6 4 2 3 step2-cell-state step2-result 2)
    (verify-cell 2 4 3 7 1 3 step2-cell-state step2-result 2)
    (verify-cell 3 4 4 7 2 3 step2-cell-state step2-result 2)
    (verify-cell 4 4 6 7 2 3 step2-cell-state step2-result 2)

    (test/is (= [3.5 4.5] (:viewport-begin step3-result)))
    (test/is (= [7.5 7.5] (:viewport-end step3-result)))
    (test/is (= [[2 3] [5 3]] (:screen-area step3-result)))
    (test/is (= (set (table/combine-ranges [2 5] [3 3])) (set (map (fn [[k _v]] k) (:screen-coord->cell-id step3-result)))))
    (verify-cell 2 3 3 4 1 4 step3-cell-state step3-result 3)
    (verify-cell 3 3 4 4 2 4 step3-cell-state step3-result 3)
    (verify-cell 4 3 6 4 1 4 step3-cell-state step3-result 3)
    (verify-cell 5 3 7 4 3 4 step3-cell-state step3-result 3)

    (test/is (= [8.5 1.25] (:viewport-begin step4-result)))
    (test/is (= [9.5 1.75] (:viewport-end step4-result)))
    (test/is (= [[5 1] [5 1]] (:screen-area step4-result)))
    (test/is (= #{[5 1]} (set (map (fn [[k _v]] k) (:screen-coord->cell-id step4-result)))))
    (verify-cell 5 1 7 1 3 2 step4-cell-state step4-result 4)

    (test/is (= [0 0] (:viewport-begin step5-result)))
    (test/is (= [15 12] (:viewport-end step5-result)))
    (test/is (= [[0 0] [5 4]] (:screen-area step5-result)))
    (test/is (= (set (table/combine-ranges [0 5] [0 4])) (set (map (fn [[k _v]] k) (:screen-coord->cell-id step5-result)))))
    (verify-cell 0 0 0 0 2 1 step5-cell-state step5-result 5)
    (verify-cell 1 0 2 0 1 1 step5-cell-state step5-result 5)
    (verify-cell 2 0 3 0 1 1 step5-cell-state step5-result 5)
    (verify-cell 3 0 4 0 2 1 step5-cell-state step5-result 5)
    (verify-cell 4 0 6 0 1 1 step5-cell-state step5-result 5)
    (verify-cell 5 0 7 0 3 1 step5-cell-state step5-result 5)
    (verify-cell 0 1 0 1 2 2 step5-cell-state step5-result 5)
    (verify-cell 1 1 2 1 1 2 step5-cell-state step5-result 5)
    (verify-cell 2 1 3 1 1 2 step5-cell-state step5-result 5)
    (verify-cell 3 1 4 1 2 2 step5-cell-state step5-result 5)
    (verify-cell 4 1 6 1 1 2 step5-cell-state step5-result 5)
    (verify-cell 5 1 7 1 3 2 step5-cell-state step5-result 5)
    (verify-cell 0 2 0 3 2 1 step5-cell-state step5-result 5)
    (verify-cell 1 2 2 3 1 1 step5-cell-state step5-result 5)
    (verify-cell 2 2 3 3 1 1 step5-cell-state step5-result 5)
    (verify-cell 3 2 4 3 2 1 step5-cell-state step5-result 5)
    (verify-cell 4 2 6 3 1 1 step5-cell-state step5-result 5)
    (verify-cell 5 2 7 3 3 1 step5-cell-state step5-result 5)
    (verify-cell 0 3 0 4 2 4 step5-cell-state step5-result 5)
    (verify-cell 1 3 2 4 1 4 step5-cell-state step5-result 5)
    (verify-cell 2 3 3 4 1 4 step5-cell-state step5-result 5)
    (verify-cell 3 3 4 4 2 4 step5-cell-state step5-result 5)
    (verify-cell 4 3 6 4 1 4 step5-cell-state step5-result 5)
    (verify-cell 5 3 7 4 3 4 step5-cell-state step5-result 5)
    (verify-cell 0 4 0 8 2 2 step5-cell-state step5-result 5)
    (verify-cell 1 4 2 8 1 2 step5-cell-state step5-result 5)
    (verify-cell 2 4 3 8 1 2 step5-cell-state step5-result 5)
    (verify-cell 3 4 4 8 2 2 step5-cell-state step5-result 5)
    (verify-cell 4 4 6 8 1 2 step5-cell-state step5-result 5)
    (verify-cell 5 4 7 8 3 2 step5-cell-state step5-result 5)))

;;;
;;; Sorting
;;;
