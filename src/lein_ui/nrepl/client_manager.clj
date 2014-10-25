(ns lein-ui.nrepl.client-manager
  (:require [clojure.tools.nrepl :as nrepl]
            [clojure.tools.nrepl.misc :refer [uuid]]
            [lein-ui.core :as ui]
            [lein-ui.nrepl :as ui-nrepl]))


(defonce repl-channels (atom {:connections {}
                              :sessions {}}))

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


(defn new-session-response-handler [session project callback]
  (bound-fn [{:keys [id status new-session] :as msg}]
    (if-not (contains? msg :op)
        (try
          (println "got here.. that's good" msg)
          (ui-nrepl/remove-handler-for-id! id)

          (when-not (contains? (set status) :done)
            (println (format (str "Response to create session"
                                  " for %s on project %s with id %s"
                                  " was not marked as done")
                             (:user session) project id))
            (callback :failure [:bad-message msg])
            (throw (ex-info "Bad message in response to new session" msg)))

          (println (format "Session created for %s on %s: %s"
                           (:user session) project new-session))

          (when-not (set-user-session! repl-channels project (:user session) new-session)
            (do
              (send-message! session project {:op "close" :session new-session})
              (println
               (format "Duplicate session created for %s on %s, closing %s"
                       (:user session) project new-session))))

          (callback :success (get-in @repl-channels [:sessions (:user session) project]))
          (catch Exception e
            (.printStackTrace e)
            (callback :exception e))))))

(defn ensure-user-session [session project]
  (when-not (get-in @repl-channels [(:user session) project])
    (let [id (uuid)
          done (promise)]
      (println id)
      (ui-nrepl/set-handler-for-id!
       id
       (new-session-response-handler session project (fn [& args] (deliver done args))))
      ;; global project client
      (send-message! session project {:id id :op "clone"})
      (let [[status data] @done]
        (when (= status :failure)
          (throw (ex-info "Failed to create user session" {:error (first data)
                                                           :body (rest data)})))
        (when (= status :exception)
          (throw data)))
      (get-in @repl-channels [:sessions (:user session) project]))))


(defn make-repl-client [session project]
  (let [conn   (ensure-project-connection  project)
        client (nrepl/client conn 1000) ;; arbitrary timeout.. we don't read
        session-id (get-in session [:nrepl :session-id])]
    (if session-id
      (nrepl/client-session client :session session-id)
      client)))

(defn send-message! [session project msg]
  (let [client (make-repl-client session project)]
    (nrepl/message client msg)
    nil))

(defn load-repl-session [session project]
  (if-let [session-id (get-in @repl-channels [:sessions (:user session) project])]
    (assoc-in session [:nrepl :session-id] session-id)
    (throw (ex-data "nrepl session not found" {:user (:user session) :project project}))))
