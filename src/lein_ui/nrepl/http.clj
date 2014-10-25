(ns lein-ui.nrepl.http
  (:require [org.httpkit.server :as server]
            [clojure.tools.nrepl :as nrepl]
            [lein-ui.core :as ui]
            [lein-ui.nrepl :as ui-nrepl]
            [lein-ui.nrepl.client-manager :as client-manager]
            [lein-ui.util :refer [pprint-str]]))


;; TODO figure out users
;; TODO register watcher for messages




;; {:op "clone"} -async> {:new-session :id}


;; I'd like to be able to setup a session for a project & user without
;; necessarily having a channel available

;; The top-level ensure-user-session can block... for now.


;; {send the create session message}

;; block / async handler

;; receive the create session response
   ;; if it's bad, {log & notify} [callback]

;; {set the session in the state} [set-fn :: SessionId -> Boolean]
   ;; if it got beat, close, {log, & notify} [callback]

;; {log & notify} [callback]



(defmulti handle-message (fn [event & args] event))

(defmethod handle-message [:ws :receive] [_ session msg]
  ;; (validate-nrepl-msg! msg :or send-error :to session)
  ;; (some-> (validate-nrepl-msg! msg) (send-error session))
  (let [session (client-manager/load-repl-session session (:subject msg))]
    (client-manager/send-message! session (:subject msg) (:body msg))
    ;; (send-nrepl-message session! session msg)
    ))

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



;; Can multiple agents can read the client response stream without
;; interference from each other?

;; No! Need a single agent reading the response stream and interested
;; parties must subscribe - hence callbacks.

;; lein-ui.nrepl.handler> (def a-connection (nrepl/connect :port 7888))

;; lein-ui.nrepl.handler> (def some-responses (nrepl/response-seq a-connection 1000))

;; lein-ui.nrepl.handler> (nrepl/message (nrepl/client a-connection 1000) {:op "eval" :code ":hello"})
;; ({:id "5f1f304b-d13f-4ee5-a416-a629e721f3db", :ns "user", :session "3a878c88-e11e-46ab-9e67-2bf7b9644111", :value ":hello"} {:id "5f1f304b-d13f-4ee5-a416-a629e721f3db", :session "3a878c88-e11e-46ab-9e67-2bf7b9644111", :status ["done"]})

;; lein-ui.nrepl.handler> some-responses
;; ()



;; lein-ui.nrepl.handler> (def mmhmm (nrepl/message (nrepl/client a-connection 1000) {:op "eval" :code ":hello"}))

;; lein-ui.nrepl.handler> (def some-responses (nrepl/response-seq a-connection 1000))

;; lein-ui.nrepl.handler> some-responses
;; ({:id "c614d72b-1472-4607-a648-528cfab5883b", :ns "user", :session "2eb5fe14-7f3d-4211-9540-ff6d81069562", :value ":hello"} {:id "c614d72b-1472-4607-a648-528cfab5883b", :session "2eb5fe14-7f3d-4211-9540-ff6d81069562", :status ["done"]})

;; lein-ui.nrepl.handler> mmhmm
;; ()
