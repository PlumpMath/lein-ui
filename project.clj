(defproject lein-ui "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.nrepl "0.2.5"]
                 [leiningen "2.5.0"]
                 [com.cemerick/piggieback "0.1.3"]
                 [http-kit "2.1.16"]
                 [org.clojure/clojurescript "0.0-2356"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/core.match "0.2.1"]
                 [cheshire "5.3.1"]                 
                 [om "0.7.3"]
                 [bond "0.2.5"]
                 [ring/ring-devel "1.1.8"]
                 [ring/ring-core "1.1.8"]
                 [compojure "1.1.9"]
                 [org.webjars/react "0.11.1"]
                 [om "0.7.1"]
                 [cljs-http "0.1.14"]
                 [lein-cljsbuild "1.0.3"]
                 [lein-figwheel "0.1.4-SNAPSHOT"]
                 [figwheel "0.1.4-SNAPSHOT"]
                 [sablono "0.2.22"]]

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src-cljs/"]
                :compiler {:output-to "resources/public/js/main.js"
                           :output-dir "resources/public/js/out"
                           :optimizations :none
                           :source-map true}}]}

  :plugins [[lein-cljsbuild "1.0.3"]
            [lein-figwheel "0.1.4-SNAPSHOT"]]

  :main lein-ui.core

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]})
