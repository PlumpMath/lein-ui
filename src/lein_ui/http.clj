(ns lein-ui.http
  (:require [compojure.core :refer [defroutes GET POST DELETE]]
            [clojure.java.io :as io]
            [lein-ui.http.route :refer [url-for]]
            [lein-ui.nrepl.http :as nrepl-http]
            [lein-ui.project :as project]
            [lein-ui.util :refer [pprint-str]]
            [org.httpkit.server :as server]
            [ring.middleware.reload :as reload]
            [compojure.handler :refer [site]]
            [compojure.core :as compojure :refer [defroutes GET POST DELETE]]
            [compojure.route :as route]))

(defn find-user [request]
  (if-let [user (-> request :cookies (get "username") :value)]
    user))

(defroutes user-routes
  (GET "/api/user" {:as request}
       (if-let [user (:user request)]
         {:status 200 :body (pprint-str {:username user})}
         {:status 404 :body (pprint-str {:error "no user set"})}))
  (POST "/api/user" {{:keys [username]} :params}
        (let [username (str username)]
          (if (or (empty? username))
            {:status 400 :body (pprint-str {:error "empty username"})}
            {:status 201 :cookies {"username" {:value username
                                               :path "/"
                                               :expires "2050-01-01T00:00:00.000Z"}}
             :body (pprint-str {:username username})})))
  (DELETE "/api/user" {:as request}
          (if (:user request)
            {:status 200 :cookies {:username {:value ""
                                              :expires "2000-01-01T00:00:00.000Z"}}}
            {:status 404 :body (pprint-str {:error "no user set"})})))

(defn wrap-user [h]
  (fn [request]
    (h (assoc request :user (find-user request)))))

(defroutes websocket-route
  (GET "/websocket" {:as request}
       (nrepl-http/nrepl-socket-handler request)))

(defroutes project-routes
  (GET (url-for [:project]) []
       (let [project (project/get-project*)
             run-state @(project project/run-state)]
         {:body (pprint-str
                 (merge (project/project-summary)
                        ;; TODO just include these in the summary?
                        {:map-url (url-for [:project :map])
                         :raw-map-url (url-for [:project :raw-map])
                         :repl (project/get-repl-data)}))
          :status 200}))
  (GET (url-for [:project :map]) []
       {:body (pprint-str (project/get-readable-project))
        :status 200})
  (GET (url-for [:project :raw-map]) []
       {:body (pprint-str (project/get-readable-raw-project))
        :status 200})
  (GET (url-for [:project :repl]) []
       {:status 200
        :body (pprint-str (project/get-repl-data))})
  (POST (url-for [:project :repl :eval]) {{:keys [code]} :params :as request}
        ;; (client-manager/send-message! {:op "eval" :code code})
        {:body (pprint-str (project/repl-eval! code))
         :status 200}))


(defn wrap-continue-session [h]
  (fn [req]
    (let [resp (h req)]
      (if (resp :session)
        resp
        (assoc resp :session (req :session))))))

(defn start-server [server & args] ;; entry point, lein run will pick up and start from here
  (when @server
    (throw (ex-info "Server running!" {})))
  (let [handler (-> (compojure/routes #'project-routes
                                      #'user-routes
                                      #'websocket-route
                                      (route/resources "/")
                                      (route/not-found "<p>Page not found.</p>"))
                    wrap-continue-session
                    wrap-user
                    site
                    reload/wrap-reload)]
    (reset! server (server/run-server handler {:port 8000}))))

(defn stop-server [server]
  (@server)
  (reset! server nil))

(defn reset-server [server]
  (stop-server)
  (start-server))
