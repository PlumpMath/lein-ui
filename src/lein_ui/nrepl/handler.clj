(ns lein-ui.nrepl.handler
  (:require [org.httpkit.server :as server]
            [clojure.tools.nrepl :as nrepl]
            [lein-ui.core :as ui]
            [lein-ui.util :refer [pprint-str]]))


;; TODO figure out users
;; TODO register watcher for messages



(defn repl-eval! [host port message]
  (with-open [conn (nrepl/connect :host host
                                  :port port)]
  (-> (nrepl/client conn 2000)
      (nrepl/message message)
      doall)))

(defonce repl-channels (atom {:connections {}
                              :sessions {}}))

(defn ensure-project-connection
  ([project]
     (if-let [conn (-> @repl-channels :connections (get project))]
       conn
       (ensure-project-connection project (apply nrepl/connect
                                                 (apply concat (ui/get-repl-data project))))))
  ([project conn]
     (let [channels @repl-channels]
       (if (compare-and-set! repl-channels
                             channels
                             (assoc-in channels [:connections project] conn))
         conn
         (if-let [conn* (get-in @repl-channels [:connections project])]
           ;; a competing connection was inserted, close ours
           (do (.close conn) conn*)
           ;; some other connection or session was created or destroyed
           (recur project conn))))))

;; Experiment and take notes on "multiple agents can read the client
;; response stream without interference from each other"




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

(fn []
  (expected-message? msg callback)

  (ui-nrepl/remove-handler-for-id (msg :id))

  (if (set-fn (msg :new-session))
    (callback :success (msg :new-session))
    (close-session (msg :new-session) callback)))

(fn [project user conn channel msg]
  ;; put the new session in our state, cleaning up if there's a
  ;; race. let the user know what's up and log pertinent information
  (if (contains? (-> msg :status set)
                 "done")
    (do ;;(ui-nrepl/remove-handler-for-id (msg :id))
        (if-let [session-id (msg :new-session)]
          (do (swap! repl-channels update-in [:sessions user]
                     (fn [user-sessions]
                       (if (user-sessions project)
                         ;; inside a swap here - normally we don't cause
                         ;; side effects because it can be run more than
                         ;; once but we throw after performing the side
                         ;; effects here so they won't happen again
                         ;; since swap! will fail.  Not concerned about
                         ;; performance of competing transactions
                         ;; (assume low volume)
                         (do
                           (nrepl/message conn {:op "close"
                                                :session session-id})
                           (println
                            (format "Duplicate session created for %s on %s"
                                    user project))
                           (server/send! channel (pprint-str
                                                  {:type :error
                                                   :subject project
                                                   :error :duplicate-session}))
                           (throw (ex-info "Duplicate session" {:project project :user user}))))
                       (assoc user-sessions project session-id)))
              (println (format "Session created for %s on %s: %s"
                               user project session-id))
              (server/send! channel (pprint-str {:type :nrepl/session
                                                 :subject project
                                                 :status :created})))
          (do
            (println (format (str "Response to create session"
                                  "for %s on project % with id %s"
                                  "did not contain a session id")
                             user project (msg :id)))
            (server/send! channel (pprint-str
                                   {:type :error
                                    :subject project
                                    :error :unknown-session-error})))))
    (println "Unexpected response" msg)))



(defn ensure-user-session
  ([conn project user]
     (if-let [session (-> @repl-channels (get-in [:sessions user project]))]
       session
       (ensure-user-session conn project user
                            (apply nrepl/connect
                                   (apply concat (ui/get-repl-data project))))))
  ([conn project user session]
     (let [channels @repl-channels]
       (if (compare-and-set! repl-channels
                             channels
                             (assoc-in channels [:connections project] conn))
         conn
         (if-let [conn* (get-in @repl-channels [:connections project])]
           ;; a competing connection was inserted, close ours
           (do (.close conn) conn*)
           ;; some other connection or session was created or destroyed
           (recur conn project user session))))))

(defn find-repl-session [user project]
  (let [channels @repl-channels
        project-connection (get-in channels [:connections project])
        user-session (get-in channels [:sessions user])]
    (if-not project-connection
      )
    ))

(defmulti handle-message (fn [msg user] (msg :type)))

(defmethod handle-message :nrepl/command [msg user app-state]
  (let [conn (find-repl-connection user (msg :subject))]
    (repl-eval! host port (msg  :message))))

(defn nrepl-socket-handler [request]
  (let [user :current-user]
    (server/with-channel request channel
      (on-receive channel (fn [msg]
                            (try
                              (handle-message msg user (request :lein-ui/app-state))
                              (catch Throwable t
                                (println t)
                                (server/send! channel (pprint-str
                                                       {:type :error
                                                        :exception (str t)}))))))
      (on-close channel (fn [status]
                          ;; (release-repl-connections user)
                          (cleanup user))))))
