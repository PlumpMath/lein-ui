(ns lein-ui.project
  (:require [clojure.java.io :as io]
            [clojure.tools.nrepl :as nrepl]
            [leiningen.core.project :as project]
            [leiningen.figwheel :as figwheel]
            [leiningen.repl :as repl]
            [lein-ui.http.route :refer [url-for]]))

(defonce project (atom nil))

(def run-state ::run-state)

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


;;; API

(defn start-figwheel! []
  (swap! (-> (get-project*) ::run-state)
         assoc :figwheel (figwheel-process)))


(defn get-repl-data []
  (let [process (-> (get-project*) ::run-state deref :repl)
        url (url-for [:project :repl])]
    (merge
     {:url url}
     (if process
       {:state :started
        :host (:host process)
        :port (:port process)
        :eval-url (url-for [:project :repl :eval])}
       {:state :stopped}))))



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
    {:url (url-for [:project])
     :name (:name project)
     :version (:version project)
     :root (:root project)}))
