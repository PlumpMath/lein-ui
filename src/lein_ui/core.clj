(ns lein-ui.core
    (:require [clojure.java.io :as io]
              [leiningen.core.project :as project]
              [org.httpkit.server :as server]
              [ring.middleware.reload :as reload]
              [compojure.handler :refer [site]]
              [compojure.core :refer [defroutes GET POST DELETE]]
              [compojure.route :as route]))

;;; Projects

(defonce projects (atom {}))

(defrecord Project [project state])
(defn new-project [project-map]
  (->Project project-map (atom {})))

(defn load-project! [root]
  (let [project (->>  (io/file root "project.clj")
                      (.getAbsolutePath)
                      (project/read-raw))]    
       (swap! projects (fn [ps p]
                         (when (ps (:name p))
                           (throw (ex-info (str "Project already loaded")
                                           {:root (:root p)})))
                         (assoc ps (:name p)
                                (new-project p)))
              project)
       (:name project)))


(defn unload-project! [name]
  (swap! projects dissoc name))

(defn reload-project! [name]
  (let [root (-> @projects name :root)]
    (unload-project! name)
    (load-project! root)))


;;; Util
(defn base-url []
  (str "http://" "localhost" ":8001/"))

(defn url-for [path]
  (str (base-url) path))

(defn pprint-str [o]
  (let [w (java.io.StringWriter.)]
    (pprint o w)
    (.toString w)))


;;; API
(defn url-for-project [name]
  (url-for (str "api/projects/" name)))

(defn project-short [project]
  {:url (url-for-project (:name project))
   :name (:name project)
   :version (:version project)
   :root (:root project)})

(defn get-projects []
  {:projects (for [[name {:keys [project]}] @projects]
               (project-short project))})

(defn get-project [project-name]
  (-> @projects
      (get project-name)
      :project))

(defn add-project [project-root]
  (load-project! project-root))




;;; Server

(defroutes all-routes
  (GET "/api/projects" []
       {:body (pprint-str (get-projects))
        :status 200})
  (GET "/api/projects/:project-name" [project-name]
       {:body (pprint-str (get-project project-name))
        :status 200})
  (POST "/api/projects" [root]
        (let [name (load-project! root)]
          {:status 201
           :body (pprint-str (project-short (:project (@projects name))))}))
  (DELETE "/api/projects/:project-name" [project-name]
          (unload-project! project-name)
          {:status 204})
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



