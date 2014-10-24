(ns lein-ui.nrepl.handler
  (:require [org.httpkit.server :as server]
            [clojure.tools.nrepl :as nrepl]
            [lein-ui.core :as ui]
            [lein-ui.nrepl :as ui-nrepl]
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


(defonce repl-channels (atom {:connections {}
                              :sessions {}}))

(defn set-nil-path!
  "Attempts to set value at path in the deref'd state.  set-nil-path!
  ensures that (get-in @state path) is nil before and during setting.
  It will return true if `value` is set in state or false if it is not"
  [state path value]
  (let [now @state]
    (if (get-in @state path)
      false
      (let [new (assoc-in now path value)
            set? (compare-and-set! state now new)]
        (if set?
          true
          (recur state path value))))))

(defn ensure-project-connection [project]
  (if-let [conn (-> @repl-channels :connections (get project))]
    conn
    (let [conn (apply nrepl/connect (apply concat (ui/get-repl-data project)))]
      (if (set-nil-path! repl-channels [:connections project] conn)
        conn
        (do
          (.close conn)
          (recur project))))))

(defn set-user-session! [state project user session-id]
  (set-nil-path! state [:sessions project user] session-id))


(defn new-session-response-callback [project user callback]
  (fn [{:keys [id status new-session] :as msg}]
    (ui-nrepl/remove-handler-for-id! id)

    (when-not (contains? (set status) "done")
      (println (format (str "Response to create session"
                            "for %s on project % with id %s"
                            "did not contain a session id")
                       user project new-session))
      (throw (ex-info "Bad message in response to new session" msg)))

    (println (format "Session created for %s on %s: %s"
                     user project new-session))

    (when-not (set-user-session! repl-channels project user new-session)
      (do
        (nrepl/message (get-in @repl-channels [:connections project])
                       {:op "close" :session new-session})
        (println
         (format "Duplicate session created for %s on %s, closing %s"
                 user project new-session))))

    (callback :success (get-in @repl-channels [:sessions user project]))))



(defn find-repl-session [user project]
  (ensure-project-connection project)
  (ensure-user-session project user)
  (get-in @repl-channels [user project])))

(defmulti handle-message (fn [msg user] (msg :type)))

(defmethod handle-message :nrepl/command [msg user app-state]
  (let [session-id (find-repl-session user (msg :subject))]
    (nrepl/message (assoc (msg :nrepl/message)
                     :session session-id))))

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
