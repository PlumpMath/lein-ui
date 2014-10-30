(ns lein-ui.app
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [figwheel.client :as fw :include-macros true]
            [goog.net.cookies :as cks]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! put! chan timeout >!]]
            [cljs.reader :as edn]
            [sablono.core :as html :refer-macros [html]]))

(enable-console-print!)

(defonce app-state (atom {:project nil
                          :user {:status :loading}}))

(def ENTER-KEY 13)

(defn edn-body [response]
  (-> response :body str edn/read-string))

(defn api-call [method url & body]
  (go (edn-body
       (<! (http/request (merge
                          {:method method
                           :url url}
                          (when body {:form-params body})))))))

(defonce websocket (atom nil))

(defn open-websocket! []
  (let [socket (js/WebSocket. "ws://localhost:8000/websocket")
        socket-chan (chan)]
    (go
      (doto socket
        (aset "onopen" (fn [e]
                         (.log js/console "ws open" e)))
        (aset "onclose" (fn [e]
                          (.log js/console "ws close" e)))
        (aset "onmessage" (fn [e]
                            (.log js/console "ws message" e)
                            (put! socket-chan e)))
        (aset "onerror" (fn [e]
                          (.log js/console "ws error" e))))
      (loop []
        (let [ws-msg (<! socket-chan)
              msg-data (try (edn/read-string (.-data ws-msg))
                            (catch :default e
                              (.log js/console "error reading" ws-msg)
                              nil))]
          (when msg-data
            (handle-ws-message (:type msg-data) msg-data)))
        (recur)))

    (reset! websocket socket)))


(defn close-websocket! []
  (let [socket @websocket]
    (.close socket)
    (reset! websocket nil)))


(defn get-edn-url [result-chan url]
  (go
    (let [result (<! (api-call :get url))]
      (put! result-chan result))))

(defn logged-in? [user]
  (= :logged-in (-> user :status)))

(defn loading? [user]
  (= :loading (-> user :status)))

(defn login! [button username]
  (set! (.-disabled button) true)
  (swap! app-state assoc-in [:user :status] :loading)
  (go
    (let [result (<! (api-call :post "http://localhost:8000/api/user" [:username username]))]
      (if (contains? result :error)
        (do (println "Couldn't log in: " result)
            (swap! app-state assoc :user {:status :logged-out}))
        (do
          (swap! app-state assoc :user (assoc result
                                         :status :logged-in))))))
  (open-websocket!))

(defn logout! []
  (close-websocket!)
  (swap! app-state assoc :user {:status :logged-out})
  (go (<! (api-call :delete "http://localhost:8000/api/user"))
      (cks/remove "username")))

(defn when-enter-key [f & args]
  (fn [e]
    (when (= ENTER-KEY
             (.-keyCode e))
      (apply f e args))))

(defn user-widget [user owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [username-input]}]
      (html
       [:div
        (cond
         (logged-in? user) [:div [:h3 "Welcome, " [:span (:username user)]]
                            [:button {:onClick (fn [_] (logout!))} "Logout"]]
         (loading? user) [:span "Loading user.."]
         :else           [:span "Username"
                          [:input
                           {:type "text"
                            :value username-input
                            :onKeyPress (when-enter-key
                                         (fn [e]
                                           (login! (.-target e) username-input)
                                           (om/set-state! owner :username-input "")))
                            :onChange (fn [e]
                                        (om/set-state! owner
                                                       :username-input
                                                       (.-value (.-target e))))}]
                          [:button
                           {:onClick (fn [e]
                                       (when-not (empty? username-input)
                                         (login! (.-target e) username-input)
                                         (om/set-state! owner :username-input "")))}
                           "Login"]])]))))

(defn repl-widget [repl owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [project-controller eval-input]}]
      (let [{:keys [state url]} repl]
        (condp = state
          :stopped (html [:div
                          "repl not started.. this is unexpected"])
          :started (html [:div
                          [:input {:type "text"
                                   :value eval-input
                                   :onKeyPress (when-enter-key (fn [_]
                                                                 (put! project-controller
                                                                       {:msg :eval
                                                                        :args [eval-input]})
                                                                 (om/set-state! :eval-input "")))
                                   :onChange (fn [e]
                                               (om/set-state! owner
                                                              :eval-input (.-value (.-target e))))
                                   :class "repl-code"}]
                          [:button {:onClick
                                    (fn [_]
                                      (put! project-controller
                                            {:msg :eval
                                             :args [eval-input]})
                                      (om/set-state! :eval-input ""))}
                           "Eval!"]
                          [:ul {:class "eval-results"}
                           (for [id (rseq (-> (om/value repl) :history :order))]
                             (let [entry (->  repl :history :entries (get id))]
                               [:li {:key id
                                     :class (str "eval-state-" (name (:state entry))
                                                 (when (:error entry)
                                                   " eval-state-error"))}
                                [:button {:class "toggle-button"
                                          :onClick (fn []
                                                     (om/transact! repl
                                                                   (fn [r]
                                                                     (update-in r [:history :entries id :hide] not))))}]
                                [:div {:class (str "eval-contents"
                                                   (when (:hide entry)
                                                     " hidden"))}
                                 (when-let [user (:user entry)]
                                   [:div user])
                                 [:div {:class "eval-code"}
                                  (:code entry)]
                                 (when-let [value (:value entry)]
                                   [:div {:class "eval-value"}
                                    value])
                                 (when-let [out (:out entry)]
                                   [:div {:class "eval-out"} out])
                                 (when-let [error (:error entry)]
                                   [:div {:class "eval-error"}
                                    error])
                                 (when-let [exception (:exception entry)]
                                   [:div {:class "eval-exception"}
                                    exception])]]))]]))))))

(defn app-view [project owner]
  (reify
    om/IRender
    (render [_]
      ;; TODO rename css active-project-view
      (html [:div {:class "active-project-view"}
             [:div
              [:h1 (:name project)]
              (get-in project [:raw-map :description])]
             [:div
              [:h3 "REPL"]
              (when-let [repl (project :repl)]
                (om/build repl-widget (:repl project)
                          {:init-state {:project-controller (-> project :controller :in)}}))]
             [:div
              [:h3 "Figwheel"]
              (when-let [figwheel (project :figwheel)]
                [:pre figwheel]
                )
              ]
             [:div
              [:h3 "Dependencies"]
              [:ol
               (for [[name version] (get-in project [:raw-map :dependencies])]
                 [:li {:key name} (str name " " version)])]]]))))

(def set-project (chan))
(defn app [data owner]
  (reify
    om/IRender
    (render [this]
      (html
       [:div {:id "app-container"}
        [:div {:style #js {"background-color" "grey"
                           "padding" "5px"
                           "margin" "5px"
                           "width" "100%"}}
         [:button {:onMouseDown (fn [_]
                                  (println (pr-str @app-state)))}
          "Print State"]]
        (om/build user-widget (:user data))
        (when-let [project (:project data)]
          (if (logged-in? (:user data))
            (om/build app-view project)))]))))

(om/root
 app
 app-state
 {:target (. js/document (getElementById "my-app"))})


(defmulti handle-project-message (fn [{:keys [msg]}] msg))

(defmethod handle-project-message :eval [{:keys [args]}]
  (let [code (first args)]
    (go (api-call :post (-> @app-state :project :repl :eval-url)
                  [:code code]))))

(defn ensure-project-controller-process []
  (if-let [controller (-> @app-state :project :controller)]
    controller
    (let [in (chan)]

      {:in in
       :process (go
                  (swap! app-state assoc-in [:project :repl :history] {:order []
                                                                       :entries (sorted-map-by >)})
                  (get-edn-url in (-> @app-state :project :raw-map-url))
                  (let [raw-map (<! in)]
                    (swap! app-state assoc-in [:project :raw-map] raw-map))

                  (loop []
                    (let [msg (<! in)]
                      (handle-project-message msg))
                    (recur)))})))

(defmulti handle-ws-message (fn [type args] type))

(defmethod handle-ws-message :default [type args]
  (println "unhandled ws message" type args))

(defmethod handle-ws-message :set [_ [path value]]
  (swap! app-state assoc-in path value))

(defmethod handle-ws-message :nrepl/message [_ {:keys [body]}]
  (let [id (body :id)
        state (get-in @app-state [:project :repl :history :entries id])
        state' (-> (merge {:messages []}
                          state
                          (cond
                           (contains? body :op) {:state :waiting :op (body :op)
                                                 :code (body :code)
                                                 :user (body :user)}
                           (contains? (-> body :status set) :done) {:state :done}
                           (contains? body :ex) {:exception (:ex body)}
                           (contains? body :value) {:value (:value body)}
                           (contains? body :err) {:error (:err body)}
                           (contains? body :out) {:out (str (state :out) (body :out))}))
                   (update-in [:messages] conj body))]

    (swap! app-state assoc-in [:project :repl :history :entries id] state')
    (when (and (not state)
               ;; only show :op "eval" requests until we figure out
               ;; how to show more
               (state' :code))
      (swap! app-state update-in [:project :repl :history :order] conj id))))


(defonce load-initial-state
  (go

    (let [result (<! (http/get "http://localhost:8000/api/project"))]
      (swap! app-state assoc :project
             (edn-body result))
      (swap! app-state assoc-in [:project :controller]
             (ensure-project-controller-process)))
    (let [result (edn-body (<! (http/get "http://localhost:8000/api/user")))]
      (if (contains? result :error)
        (swap! app-state assoc :user {:status :logged-out})
        (do (swap! app-state assoc :user (assoc result
                                           :status :logged-in))
            (open-websocket!))))))

(defn connect []
  (fw/watch-and-reload
   :websocket-url "ws://localhost:3449/figwheel-ws"
   :jsload-callback (fn [] (print "reloaded"))))
(connect)

;;; Goal
;;
;; Multiple clients can connect to a server and submit nrepl commands.
;;
;; An "nREPL" log contains data for both the requests made to nrepl
;; by all clients and their responses.

;; The nREPL log can be replicated on each client.

;; Users can see who sent each nrepl command

;; Solutions
;;
;; Connect with a Websocket.
;;
;; There is a protocol for submitting commands to nREPL and reporting
;; stream state and/or request a syncrhonization.

;; An nREPL middle captures requests and responses and appends them to
;; a log.  The order that they are appended is the 'server order'
;; and this is the log clients sync.

;; Commands to nREPL are submitted directly to nREPL.  Until the nREPL
;; middleware logs the command, it is not part of the global state.

;; (A second log exists for incoming commands.  There exists enough
;; information to merge the nREPL log and the command log).

;; Clients can ask for all entries from an offest (inclusive) and send
;; a subscribe message with a current position.  The server will then
;; send all message from current position and continue streaming
;; messages as they arrive.
