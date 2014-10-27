(ns lein-ui.http
  (:require [compojure.core :refer [defroutes GET POST DELETE]]
            [lein-ui.util :refer [pprint-str]]))

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
