(ns lein-ui.core
    (:require [clojure.core.async :refer [go-loop <! timeout]]
              [clojure.java.io :as io]
              [clojure.tools.nrepl :as nrepl]
              [lein-ui.http :as http]
              [lein-ui.nrepl :as ui-repl]
              [lein-ui.util :refer [pprint-str]]
              [leiningen.core.project :as project]
              [leiningen.figwheel :as figwheel]
              [leiningen.repl :as repl]
              [org.httpkit.server :as server]
              [ring.middleware.reload :as reload]
              [compojure.handler :refer [site]]
              [compojure.core :as compojure :refer [defroutes GET POST DELETE]]
              [compojure.route :as route])
      (:import (com.hypirion.io Pipe ClosingPipe)))



;;; Projects

(defonce project (atom nil))

(defn get-project* []
  (if-let [project @project]
    project
    (throw (ex-info "Project doesn't exist" {:name name}))))

(defn new-project [project-map raw-project-map]
  (assoc project-map
    ::run-state (atom {})
    ::raw raw-project-map))

(defn load-project! [root]
  (let [path (->>  (io/file root "project.clj")
                      (.getAbsolutePath))
        project-map (project/read path)
        raw-project-map (project/read-raw path)
        ui-project (new-project project-map raw-project-map)
        name (-> ui-project :name)
        root (-> ui-project :root)]
    (reset! project ui-project)
    name))


(def self-repl {:host "localhost"
                :port 7888})


;; TODO remove this for nrepl.client-manager
(defn repl-eval! [code]
  (let [{:keys [host port]} (-> (get-project*) ::run-state deref :repl)]
    (with-open [conn (nrepl/connect :host host
                                    :port port)]
      (-> (nrepl/client conn 1000)
          (nrepl/message {:op "eval" :code code})
          doall))))


(defn figwheel-process []
  (let [out-writer (java.io.StringWriter.)]
    {:thread (.start
              (Thread. (bound-fn []
                         (binding [*out* out-writer]
                           (figwheel/figwheel (get-project*))))))
     :out-writer out-writer}))

(defn start-figwheel! []
  (swap! (-> (get-project*) ::run-state)
         assoc :figwheel (figwheel-process)))

;;; Util
(defn base-api-url []
  (str "http://" "localhost" ":8000/api/project"))

(defn url-for [path]
  (str (base-api-url) path))


;;; API

(defn get-repl-data []
  (let [process (-> (get-project*) ::run-state deref :repl)
        url (url-for "/repl")]
    (merge
     {:url url}
     (if process
       {:state :started
        :host (:host process)
        :port (:port process)
        :stop-url (url-for "/repl/stop")
        :eval-url (url-for "/repl/eval")}
       {:state :stopped
        :start-url (url-for "/repl/start")}))))



;; TODO think about this
(def non-data-keys
  "These are keys of a leiningen project map which contain non-values
  or values not serializable or readable by the base EDN spec"
  #{:uberjar-merge-with
    :uberjar-exclusions
    :jar-exclusions
    :checkout-deps-shares

    ;; also remove our data
    ;; TODO move our state out of the project map?
    ::run-state
    ::raw})

(defn get-readable-project []
  (apply dissoc (get-project*)
         non-data-keys))

(defn get-readable-raw-project []
  (apply dissoc (::raw (get-project*))
         non-data-keys))

(defn project-summary []
  (let [project (get-project*)]
    {:url (base-api-url)
     :name (:name project)
     :version (:version project)
     :root (:root project)}))

(defn bootstrap-self []
  (load-project! ".")
  (swap! (-> (get-project*) ::run-state) assoc :repl self-repl)
  (start-figwheel!))


;;; Server

(defonce figwheel-receivers (atom {}))
(defn figwheel-broadcaster []
  (go-loop []
    (<! (timeout 500))
    (doseq [[project subscribers] @figwheel-receivers]
      (let [output (-> (get-project*)
                       ::run-state
                       deref
                       :figwheel
                       :out-writer
                       str)]
        (server/send! subscribers (pprint-str {:type :set
                                               :args [[:projects :map project :figwheel] output]}))))
    (recur)))

(defroutes all-routes
  (GET "/websocket" {:as request}
       (server/with-channel request channel
         (swap! figwheel-receivers assoc "lein-ui" channel)
         (server/on-close channel (fn [status] (swap! figwheel-receivers dissoc "lein-ui" channel)))))
  (GET "/api/project" []
       (let [project (get-project*)
             run-state @(project ::run-state)]
         {:body (pprint-str
                 (merge (project-summary)
                        ;; TODO just include these in the summary?
                        {:map-url (url-for "/map")
                         :raw-map-url (url-for "/raw-map")
                         :repl (get-repl-data)}))
          :status 200}))
  (GET "/api/project/map" []
       {:body (pprint-str (get-readable-project))
        :status 200})
  (GET "/api/project/raw-map" []
       {:body (pprint-str (get-readable-raw-project))
        :status 200})
  (POST "/api/project/repl/eval" {{:keys [code]} :params}
        {:body (pprint-str (repl-eval! code))
         :status 200}))

(defonce server (atom nil))

(defn wrap-continue-session [h]
  (fn [req]
    (let [resp (h req)]
      (if (resp :session)
        resp
        (assoc resp :session (req :session))))))

(defn start-server [& args] ;; entry point, lein run will pick up and start from here
  (when @server
    (throw (ex-info "Server running!" {})))
  (let [handler (-> (compojure/routes #'all-routes
                                       #'http/user-routes
                                       (route/resources "/")
                                       (route/not-found "<p>Page not found.</p>"))
                    wrap-continue-session
                    http/wrap-user
                    site
                    reload/wrap-reload)]
    (reset! server (server/run-server handler {:port 8000}))))

(defn stop-server []
  (@server)
  (reset! server nil))

(defn reset-server []
  (stop-server)
  (start-server))

(defonce nrepl-server (atom nil))
(defonce web-server (atom nil))
(defn -main []
  (reset! nrepl-server
          (ui-repl/lein-ui-nrepl :port (:port self-repl)))
  (reset! ui-repl/connection (apply nrepl/connect (apply concat (seq self-repl))))
  (reset! web-server (start-server))
  (bootstrap-self)
  nil)
