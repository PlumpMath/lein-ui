(ns lein-ui.app
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [figwheel.client :as fw :include-macros true]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! put! chan timeout >!]]
            [cljs.reader :as edn]
            [sablono.core :as html :refer-macros [html]]))

(enable-console-print!)

(defonce app-state (atom {:project nil
                          :user {:status :loading}}))

(defn edn-body [response]
  (-> response :body str edn/read-string))

(defn api-call [method url & body]
  (go (edn-body
       (<! (http/request (merge
                          {:method method
                           :url url}
                          (when body {:form-params body})))))))

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
        (println "Couldn't log in: " result)
        (swap! app-state assoc :user {:status :logged-out})
        (do
          (swap! app-state assoc :user (assoc result
                                         :status :logged-in)))))))

(defn logout! []
  (swap! app-state assoc :user {:status :logged-out})
  (go (api-call :delete "http://localhost:8000/api/user")))

(defn user-widget [user owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (html
       [:div
        (cond
         (logged-in? user) [:div [:h3 "Welcome, " [:span (:username user)]]
                            [:button {:onClick (fn [_] (logout!))} "Logout"]
                            ]
         (loading? user) [:span "Loading user.."]
         :else           [:span "Username" [:input {:type "text"
                                                    :onChange (fn [e]
                                                                (om/set-state! owner
                                                                            :username-input
                                                                            (.-value (.-target e))))}]
                          [:button {:onClick (fn [e]

                                               (login! (.-target e) (state :username-input)))} "Login"]])]))))

(defn repl-widget [repl owner]
  (reify
    om/IDidMount
    (did-mount [this]
      (om/set-state! owner :eval-input
                     (-> (om/get-node owner)
                         (.querySelector ".repl-code"))))

    om/IRenderState
    (render-state [_ {:keys [project-controller eval-input]}]
      (let [{:keys [state url]} repl]
        (condp = state
          :stopped (html [:div
                          "repl not started.. this is unexpected"])
          :started (html [:div
                          [:input {:type "text"
                                   :class "repl-code"}]
                          [:button {:onClick
                                    (fn [_]
                                      (put! project-controller
                                            {:msg :eval
                                             :args [(.-value eval-input)]})
                                      (set! (.-value eval-input) ""))}
                           "Eval!"]
                          [:ul {:class "eval-results"}
                           (for [[index eval-request] (vec (-> repl :history :entries))]
                             [:li {:key index
                                   :class (str "eval-state-" (name (:state eval-request))
                                               (when (:error eval-request)
                                                 " eval-state-error"))}
                              [:button {:class "toggle-button"
                                        :onClick (fn []
                                                   (om/transact! repl
                                                                 (fn [r]
                                                                   (update-in r [:history :entries index :hide] not))))}]
                              [:div {:class (str "eval-contents"
                                                 (when (:hide eval-request)
                                                   " hidden"))}
                               [:div {:class "eval-code"}
                                (:code eval-request)]
                               (when-let [value (:value eval-request)]
                                 [:div {:class "eval-value"}
                                  value])
                               (when-let [out (:out eval-request)]
                                 [:div {:class "eval-out"} out])
                               (when-let [error (:error eval-request)]
                                 [:div {:class "eval-error"}
                                  error])
                               (when-let [exception (:exception eval-request)]
                                 [:div {:class "eval-exception"}
                                  exception])]])]]))))))

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
    (swap! app-state
           update-in [:project :repl :history]
           (fn [{:keys [index entries] :as history}]
             {:entries (assoc entries index {:state :waiting
                                             :code code})
              :index (inc index)}))
    (let [index (-> @app-state :project :repl :history :index dec)]
      (go (let [result (<! (api-call :post (-> @app-state :project :repl :eval-url)
                                     [:code code]))
                final-state (reduce (fn [s val]
                                      (cond
                                       (= (:status val) ["done"]) (assoc s :state :done)
                                       (:ex val) (assoc s :exception (:ex val))
                                       (:err val) (assoc s :error (:err val))
                                       (:value val) (assoc s :value (:value val))
                                       (:out val)   (update-in s [:out] str (:out val))))
                                    {:state :no-response}
                                    result)]
            (swap! app-state update-in [:project :repl :history :entries index]
                   merge final-state))))))

(defn ensure-project-controller-process []
  (if-let [controller (-> @app-state :project :controller)]
    controller
    (let [in (chan)]

      {:in in
       :process (go
                  (swap! app-state assoc-in [:project :repl :history] {:index 0
                                                                       :entries (sorted-map-by >)})
                  (println "getting project map from " (-> @app-state :project :raw-map-url))
                  (get-edn-url in (-> @app-state :project :raw-map-url))
                  (println "waiting for response")
                  (let [raw-map (<! in)]
                    (println "got response")
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

(defn websocket-process []
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
            (handle-ws-message (:type msg-data) (:args msg-data))))
        (recur)))))

(defonce run-processes
  (do
    (websocket-process)
    nil))

(defonce load-initial-state
  (go

    (let [result (<! (http/get "http://localhost:8000/api/project"))]
      (swap! app-state assoc :project
             (edn-body result))
      (println @app-state)
      (swap! app-state assoc-in [:project :controller]
             (ensure-project-controller-process)))
    (let [result (edn-body (<! (http/get "http://localhost:8000/api/user")))]
      (if (contains? result :error)
        (swap! app-state assoc :user {:status :logged-out})
        (swap! app-state assoc :user (assoc result
                                       :status :logged-in))))))

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
