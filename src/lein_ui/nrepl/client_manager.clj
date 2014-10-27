(ns lein-ui.nrepl.client-manager
  (:require [clojure.tools.nrepl :as nrepl]
            [clojure.tools.nrepl.misc :refer [uuid]]
            [lein-ui.nrepl :as ui-nrepl]))


(defonce repl-sessions (atom {}))

(declare send-message!)

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

(defn set-user-session! [state user session-id]
  (set-nil-path! state [user] {:user user
                               :id session-id}))


(defn new-session-response-handler [session callback]
  (bound-fn [{:keys [id status new-session] :as msg}]
    ;; our handler is also called for all messages with :id id,
    ;; i.e. including the message we send.  ignore it by checking for
    ;; :op, a key present in nrepl requests
    (if-not (contains? msg :op)
      (try
        (ui-nrepl/remove-handler-for-id! id)

        (when-not (contains? (set status) :done)
          (println (format (str "Response to create session"
                                " for %s on with id %s"
                                " was not marked as done")
                           (:user session) id))
          (callback :failure [:bad-message msg])
          (throw (ex-info "Bad message in response to new session" msg)))

        (println (format "Session created for %ss: %s"
                         (:user session) new-session))

        (when-not (set-user-session! repl-sessions (:user session) new-session)
          (do
            (send-message! session {:op "close" :session new-session})
            (println
             (format "Duplicate session created for %s, closing %s"
                     (:user session) new-session))))

        (callback :success (get-in @repl-sessions [:sessions (:user session)]))
        (catch Exception e
          (.printStackTrace e)
          (callback :exception e))))))

(defn ensure-user-session [session]
  (if-let [session (get @repl-sessions (:user session))]
    session
    (let [id (uuid)
          done (promise)]
      (ui-nrepl/set-handler-for-id!
       id
       (new-session-response-handler session (fn [& args] (deliver done args))))
      (ui-nrepl/send-message! {:id id :op "clone"})
      (let [[status data] @done]
        (when (= status :failure)
          (throw (ex-info "Failed to create user session" {:error (first data)
                                                           :body (rest data)})))
        (when (= status :exception)
          (throw data)))
      (get @repl-sessions (:user session)))))

(defn message [session msg]
  (if-let [session-id (when (:user session)
                     (-> (ensure-user-session session) :id))]
    (ui-nrepl/message (assoc msg :session session-id))
    (throw (ex-info "No nrepl session" {:session session})))
  )

(defn send-message! [session msg]
  (if-let [session-id (when (:user session)
                     (-> (ensure-user-session session) :id))]
    (ui-nrepl/send-message! (assoc msg
                              :session session-id
                              :user (:user session)))
    (throw (ex-info "No nrepl session" {:session session}))))
