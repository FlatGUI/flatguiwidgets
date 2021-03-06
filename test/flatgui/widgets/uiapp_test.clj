; Copyright (c) 2016 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns flatgui.widgets.uiapp-test
  (:require [clojure.test :as test]
            [flatgui.base :as core]
            [flatgui.util.matrix :as m]
            [flatgui.widgets.panel :as panel]
            [flatgui.widgets.window :as window]
            [flatgui.widgets.checkbox :as checkbox]
            [flatgui.widgets.label :as label])
  (:import (java.awt.event MouseEvent)
           (flatgui.core.awt FGAWTInteropUtil)
           (flatgui.core.engine.ui FGAppContainer)
           (java.awt Container)))

(test/deftest uiapp-test
  (let [checkbox-not-pressed-text "aa123"
        checkbox-pressed-text "bb456"
        _ (core/defevolverfn :text
                             (if (get-property [:sw-text] :pressed)
                               checkbox-pressed-text
                               checkbox-not-pressed-text))
        t-win (core/defcomponent
                window/window
                :hello
                {:clip-size (m/defpoint 3 1.5)
                 :position-matrix (m/translation 1 1)
                 :text "Hello World Example"}

                (core/defcomponent
                  checkbox/checkbox
                  :sw-text
                  {:clip-size (m/defpoint 1.75 0.25 0)
                   :text "test"
                   :position-matrix (m/translation 1 1)})

                (core/defcomponent
                  label/label
                  :txt
                  {:text checkbox-not-pressed-text
                   :clip-size (m/defpoint 2.25 0.25 0)
                   :position-matrix (m/translation 2.5 0.75)
                   :evolvers {:text text-evolver}}))
        container (core/defroot (core/defcomponent panel/panel :main {:clip-size (m/defpoint 10 10)} t-win))
        ui-app (FGAppContainer. "c1" container (FGAWTInteropUtil. 64))
        _ (.initialize ui-app)
        dummy-source (Container.)
        txt-uid (.getComponentUid ui-app [:main :hello :txt])
        container-accessor (.getContainerAccessor ui-app)
        look-before-click (.get (.getComponent container-accessor txt-uid) :look-vec)
        _ (.get (.evolve ui-app (MouseEvent. dummy-source MouseEvent/MOUSE_PRESSED 0 MouseEvent/BUTTON1_DOWN_MASK 129 129 129 129 1 false MouseEvent/BUTTON1)))
        _ (.get (.evolve ui-app (MouseEvent. dummy-source MouseEvent/MOUSE_RELEASED 0 MouseEvent/BUTTON1_DOWN_MASK 129 129 129 129 1 false MouseEvent/BUTTON1)))
        _ (.get (.evolve ui-app (MouseEvent. dummy-source MouseEvent/MOUSE_CLICKED 0 MouseEvent/BUTTON1_DOWN_MASK 129 129 129 129 1 false MouseEvent/BUTTON1)))
        look-after-click (.get (.getComponent container-accessor txt-uid) :look-vec)]
    (test/is (some #(= checkbox-not-pressed-text %) (second look-before-click)))
    (test/is (some #(= checkbox-pressed-text %) (second look-after-click)))))