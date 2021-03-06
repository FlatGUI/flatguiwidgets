; Copyright (c) 2015 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(def flatgui-version "0.2.2-SNAPSHOT")


(defproject org.flatgui/flatguiwidgets flatgui-version
  :description "FlatGUI widget library"
  :url "http://flatgui.org"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.flatgui/flatguicore ~flatgui-version]
                 [org.flatgui/flatguithemes ~flatgui-version]
                 [org.flatgui/flatguiskins ~flatgui-version]]
  :deploy-repositories {"releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/" :creds :gpg}
                        "snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/" :creds :gpg}
                        "clojars" {:url "https://clojars.org/repo/"
                                   :username :env/clojars_user
                                   :password :env/clojars_password}}
  :omit-source true
  :aot :all

  :scm {:url "https://github.com/flatgui/flatgui/"}
  :pom-addition [:developers [:developer
                              [:name "Denys Lebediev"]
                              [:email "denis@flatgui.org"]
                              [:timezone "+2"]]])
