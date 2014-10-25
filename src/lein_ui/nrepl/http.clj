(ns lein-ui.nrepl.http
  (:require [org.httpkit.server :as server]
            [clojure.tools.nrepl :as nrepl]
            [lein-ui.core :as ui]
            [lein-ui.nrepl :as ui-nrepl]
            [lein-ui.nrepl.client-manager :as client-manager]
            [lein-ui.util :refer [pprint-str]]))


;; TODO figure out users
;; TODO register watcher for messages

(defmulti handle-message (fn [event & args] event))

(defmethod handle-message [:ws :connect] [_ session channel])

(defmethod handle-message [:ws :receive] [_ session msg]
  ;; (validate-nrepl-msg! msg :or send-error :to session)
  ;; (some-> (validate-nrepl-msg! msg) (send-error session))
  (let [session (client-manager/load-repl-session session (:subject msg))]
    (client-manager/send-message! session (:subject msg) (:body msg))))

(defmethod handle-message [:ws :close] [_ session status]
  (println "Session" session "disconnected with" status))

(defn nrepl-socket-handler [request]
  (let [user :current-user]
    (server/with-channel request channel
      (server/on-receive channel (fn [msg]
                                   (try
                                     (handle-message msg user (request :lein-ui/app-state))
                                     (catch Throwable t
                                       (println t)
                                       (server/send! channel (pprint-str
                                                              {:type :error
                                                               :exception (str t)}))))))
      (server/on-close channel (fn [status]
                                 ;; (schedule (-> 10 minutes) #(cleanup user)
                                 (println "User" user "disconnected"))))))
