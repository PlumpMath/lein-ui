(ns lein-ui.nrepl.http
  (:require [org.httpkit.server :as server]
            [clojure.tools.nrepl :as nrepl]
            [clojure.tools.nrepl.misc :refer [uuid]]
            [lein-ui.nrepl :as ui-nrepl]
            [lein-ui.nrepl.client-manager :as client-manager]
            [lein-ui.util :refer [pprint-str]]))


;; TODO figure out users
;; TODO register watcher for messages

(defmulti handle-message (fn [event & args] event))

(defmethod handle-message [:ws :connect] [_ session channel]
  (let [id (session :ws-session-id)]
    (println (:user session) "connected on websocket id" id)
    (ui-nrepl/subscribe! id
                         (fn [msg]
                           (server/send! channel (pprint-str {:type :nrepl/message
                                                              :body (dissoc msg :transport)}))))))

(defmethod handle-message [:ws :receive] [_ session msg]
  (client-manager/send-message! session (:body msg)))

(defmethod handle-message [:ws :close] [_ session status]
  (println "Session" session "disconnected with" status)
  (ui-nrepl/unsubscribe! (session :ws-session-id)))

(defn nrepl-socket-handler [request]
  {:pre [(contains? request :user)]}
  (let [ws-session-id (uuid)
        session (assoc request :ws-session-id ws-session-id)]
    (server/with-channel session channel
      (handle-message [:ws :connect] session channel)

      (server/on-receive channel (partial handle-message [:ws :receive] session))

      (server/on-close channel (partial handle-message [:ws :close] session)))))
