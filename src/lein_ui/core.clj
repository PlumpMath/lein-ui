(ns lein-ui.core
    (:require [clojure.core.async :refer [go-loop <! timeout]]
              [clojure.java.io :as io]
              [clojure.tools.nrepl :as nrepl]
              [lein-ui.nrepl :as ui-repl]
              [lein-ui.util :refer [pprint-str]]
              [leiningen.core.project :as project]
              [leiningen.figwheel :as figwheel]
              [leiningen.repl :as repl]
              [org.httpkit.server :as server]
              [ring.middleware.reload :as reload]
              [compojure.handler :refer [site]]
              [compojure.core :refer [defroutes GET POST DELETE]]
              [compojure.route :as route])
      (:import (com.hypirion.io Pipe ClosingPipe)))



;;; Projects

(defonce projects (atom {}))

(defn new-project [project-map raw-project-map]
  (assoc project-map
    ::run-state (atom {})
    ::raw raw-project-map))

(defn get-project* [name]
  (if-let [project (@projects name)]
    project
    (throw (ex-info "Project doesn't exist" {:name name}))))

(defn load-project! [root]
  (let [path (->>  (io/file root "project.clj")
                      (.getAbsolutePath))
        project (project/read path)
        raw-project (project/read-raw path)
        ui-project (new-project project raw-project)
        name (-> ui-project :name)
        root (-> ui-project :root)]
       (swap! projects (fn [ps]
                         (when (ps name)
                           (throw (ex-info (str "Project already loaded")
                                           {:root root})))
                         (assoc ps name
                                ui-project)))
       name))


(defn unload-project! [name]
  (swap! projects dissoc name))

(defn reload-project! [name]
  (let [root (-> (get-project* name) :root)]
    (unload-project! name)
    (load-project! root)))

;; adapated from leiningen.core.eval
(defn- overridden-env
  "Returns an overridden version of the current environment as an Array of
  Strings of the form name=val, suitable for passing to Runtime#exec."
  [env]
  (->> env
       (filter val)
       (map #(str (name (key %)) "=" (val %)))
       (into-array String)))

;; adapated from leiningen.core.eval
(defn sh
  [cmd & {:keys [dir]}]
  (let [env (overridden-env (System/getenv))
        proc (.exec (Runtime/getRuntime) (into-array cmd) env
                    (io/file dir))]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn [] (.destroy proc))))
    ;; TODO: wrap setup in try/finally and close streams
    (let [out (io/reader (.getInputStream proc))
          err (io/reader (.getErrorStream proc))
          out-writer (java.io.StringWriter.)
          err-writer (java.io.StringWriter.)
          pump-out (doto (Pipe. out out-writer) .start)
          pump-err (doto (Pipe. err err-writer) .start)]
      {:process proc
       :pump-out pump-out
       :pump-err pump-err
       :out-writer out-writer
       :err-writer err-writer})))


(defonce free-port (atom 4000))
(defn next-free-port []
  (swap! free-port inc))

(defn start-repl! [name]
  (let [project (get-project* name)]
    (when (-> project ::run-state deref :repl)
      (throw (ex-info "Project already running a repl!" {:name name})))

    (let [env (overridden-env (System/getenv))
          host (repl/repl-host project)
          port (let [project-port (repl/repl-port project)]
                 (if (= project-port 0)
                   (next-free-port)
                   project-port))
          repl-process (sh ["lein"
                            "repl"
                            ":headless"
                            ":host"
                            host
                            ":port"
                            (str port)]
                           :dir ".")]
      (swap! (::run-state project) assoc :repl
             (assoc repl-process
               :host host
               :port port))
      nil)))


(defn reset-repl! [name]
  (reset! (-> name get-project* ::run-state)
          nil))

(defn stop-repl! [name]
  (throw (ex-info "Not supported" {:error :go-away}))
  (let [project (get-project* name)]
    (if-let [repl (-> project ::run-state deref :repl)]
      (do (.destroy (:process repl))
          (swap! (::run-state project) dissoc :repl)
          nil)
      (throw (ex-info "repl not running" {:name name})))))

(def self-repl {:host "localhost"
                :port 7888})


(defn repl-eval! [name code]
  (let [{:keys [host port]} (-> name get-project* ::run-state deref :repl)]
    (with-open [conn (nrepl/connect :host host
                                    :port port)]
      (-> (nrepl/client conn 1000)
          (nrepl/message {:op "eval" :code code})
          doall))))


(defn figwheel-process [project]
  (let [out-writer (java.io.StringWriter.)]
    {:thread (.start
              (Thread. (bound-fn []
                         (binding [*out* out-writer]
                           (figwheel/figwheel project)))))
     :out-writer out-writer}))

(defn start-figwheel! [name]
  (let [project (get-project* "lein-ui")]
    (swap! (-> project ::run-state)
           assoc :figwheel (figwheel-process project))))

;;; Util
(defn base-url []
  (str "http://" "localhost" ":8000/"))

(defn url-for [path]
  (str (base-url) path))


;;; API
(defn url-for-project [name]
  (url-for (str "api/projects/" name)))


(defn get-repl-data [name]
  (let [process (-> name get-project* ::run-state deref :repl)
        url (str (url-for-project name) "/repl")]
    (merge
     {:url url}
     (if process
       {:state :started
        :host (:host process)
        :port (:port process)
        :stop-url (str (url-for-project name) "/repl/stop")
        :eval-url (str (url-for-project name) "/repl/eval")}
       {:state :stopped
        :start-url (str (url-for-project name) "/repl/start")}))))



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

(defn get-readable-project [project-name]
  (apply dissoc (get-project* project-name)
         non-data-keys))

(defn get-readable-raw-project [project-name]
  (apply dissoc (::raw (get-project* project-name))
         non-data-keys))

(defn project-summary [project]
  {:url (url-for-project (:name project))
   :name (:name project)
   :version (:version project)
   :root (:root project)})

(defn get-projects []
  {:projects (for [[name project] @projects]
               (project-summary project))})

(defn add-project [project-root]
  (load-project! project-root))


(defn bootstrap-self []
  (load-project! ".")
  (swap! (-> "lein-ui" (get-project*) ::run-state)
         assoc :repl self-repl
               :figwheel (figwheel-process (get-project* "lein-ui"))))


;;; Server

(defonce figwheel-receivers (atom {}))
(defn figwheel-broadcaster []
  (go-loop []
    (<! (timeout 500))
    (doseq [[project subscribers] @figwheel-receivers]
      (let [output (-> (get-project* project)
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
  (GET "/api/projects" []
       {:body (pprint-str (get-projects))
        :status 200})
  (GET "/api/projects/:project-name" [project-name]
       (let [project (get-project* project-name)
             run-state @(project ::run-state)]
         {:body (pprint-str
                 (merge (project-summary project)
                        ;; TODO just include these in the summary?
                        {:map-url (str (url-for-project project-name)
                                                "/map")
                         :raw-map-url (str (url-for-project project-name)
                                           "/raw-map")
                         :repl (get-repl-data project-name)}))
          :status 200}))
  (GET "/api/projects/:project-name/map" [project-name]
       {:body (pprint-str (get-readable-project project-name))
        :status 200})
  (GET "/api/projects/:project-name/raw-map" [project-name]
       {:body (pprint-str (get-readable-raw-project project-name))
        :status 200})
  (POST "/api/projects/:project-name/repl/start" [project-name]
        (start-repl! project-name)
        {:body (pprint-str (get-repl-data project-name))
         :status 201})
  (POST "/api/projects/:project-name/repl/eval" {{:keys [project-name
                                                         code]} :params}
        {:body (pprint-str (repl-eval! project-name code))
         :status 200})
  (POST "/api/projects" [root]
        (let [name (load-project! root)]
          {:status 201
           :body (pprint-str (project-summary (get-project* name)))}))
  (DELETE "/api/projects/:project-name" [project-name]
          (unload-project! project-name)
          {:status 204})
  (route/resources "/")
  (route/not-found "<p>Page not found.</p>"))

(defonce server (atom nil))

(defn start-server [& args] ;; entry point, lein run will pick up and start from here
  (when @server
    (throw (ex-info "Server running!" {})))
  (let [handler (reload/wrap-reload (site #'all-routes))]
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
  (reset! web-server (start-server))
  (bootstrap-self)
  nil)
