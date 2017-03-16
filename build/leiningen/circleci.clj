(ns leiningen.circleci
  (:require [leiningen
             [deploy :as deploy]]))


(defn circleci [project & args]
  (let [branch (System/getenv "CIRCLE_BRANCH")]
    (condp re-find branch
      #"master" (deploy/deploy project "clojars"))))