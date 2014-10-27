(ns lein-ui.core
    (:require [clojure.tools.nrepl :as nrepl]
              [lein-ui.http :as http]
              [lein-ui.nrepl :as ui-repl]
              [lein-ui.project :as project])
    (:import (com.hypirion.io Pipe ClosingPipe)))



(def self-repl {:host "localhost"
                :port 7888})


(defonce server (atom nil))
(defonce nrepl-server (atom nil))
(defonce web-server (atom nil))


(defn bootstrap-self []
  (project/load-project! ".")
  (println (project/get-project*))
  (swap! (-> (project/get-project*) project/run-state) assoc :repl self-repl)
  (project/start-figwheel!))

(defn -main []
  (reset! nrepl-server
          (ui-repl/lein-ui-nrepl :port (:port self-repl)))
  (reset! ui-repl/connection (apply nrepl/connect (apply concat (seq self-repl))))
  (reset! web-server (http/start-server server))
  (bootstrap-self)
  nil)
