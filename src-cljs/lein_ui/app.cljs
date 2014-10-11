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

(defonce app-state (atom {:projects {:active nil
                                     :map {}}}))

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

(defn active-project [projects]
  (when-let [active-key (projects :active)]
    (get-in projects [:map active-key])))

(defn app-list [projects owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [set-project]}]
      (html
       [:div {:class "project-list"}
        [:h3 "Projects"]
        (for [[name project] (projects :map)]
          [:a {:key name
               :href "#"
               :class (if (= (projects :active)
                             name)
                        "active-project-link"
                        "")
               :onClick (fn [_]
                          (put! set-project project))}
           (project :name)])]))))


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
                          [:button {:onClick
                                    (fn [_]
                                      (put! project-controller {:msg :start-repl
                                                                :args nil}))}
                           "start repl"]])
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
                          [:ul
                           (for [[index item] (vec (-> repl :history :entries))]
                             [:li {:key index} (:code item)])
                           ]]))))))

(defn app-view [project owner]
  (reify
    om/IRender
    (render [_]
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

        (om/build app-list (:projects data) {:init-state {:set-project set-project}})
        (when-let [project (active-project (:projects data))]
          (om/build app-view project))]))))

(om/root
 app
 app-state
 {:target (. js/document (getElementById "my-app"))})

(defmulti handle-project-message (fn [{:keys [msg]} project] msg))

(defmethod handle-project-message :start-repl [_ project]
  (go (let [repl (<! (api-call :post (-> @project :repl :url)))]
        (om/transact! project (fn [p]
                                (update-in p [:repl] merge repl))))))

(defmethod handle-project-message :eval [{:keys [args]} project]
  (let [code (first args)]
    (om/transact! project
                  (fn [p]
                    (update-in p [:repl :history]
                               (fn [{:keys [index entries] :as history}]
                                 {:entries (assoc entries index {:state :waiting
                                                                 :code code})
                                  :index (inc index)}))))
    (let [index (-> @project :repl :history :index dec)]
      (go (let [result (<! (api-call :post (-> @project :repl :eval-url)
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
            (om/transact! project
                          (fn [p]
                            (update-in p [:repl :history :entries index] merge final-state))))))))

(defn ensure-project-controller-process [project]
  (if-let [controller (@project :controller)]
    controller
    (let [in (chan)]
      {:in in
       :process (go
                  (get-edn-url in (@project :url))
                  (let [summary (<! in)]
                    (om/transact! project (fn [p]
                                            (-> p
                                                (merge summary)
                                                (assoc-in [:repl :history] {:index 0 :entries (sorted-map-by >)})))))

                  (get-edn-url in (@project :raw-map-url))
                  (let [raw-map (<! in)]
                    (om/transact! project #(assoc % :raw-map raw-map)))

                  (loop []
                    (let [msg (<! in)]
                      (handle-project-message msg project))
                    (recur)))})))

(defn set-project-process []
  (go
    (loop []
      (let [project (<! set-project)]
        (swap! app-state (fn [state]
                           (-> state
                               (assoc-in [:projects :active] (@project :name))
                               (assoc-in [:projects :map (:name @project) :controller] (ensure-project-controller-process project))))))
      (recur))))


(defonce run-processes
  (do
    (set-project-process)
    nil))

(defonce load-initial-state
  (go
    (let [result (<! (http/get "http://localhost:8000/api/projects"))]
      (swap! app-state assoc-in [:projects :map]
             (reduce (fn [m p]
                       (assoc m (p :name) p))
                     {}
                     (-> result :body str edn/read-string :projects))))))

 ;; optional callback

(defn connect []

  (fw/watch-and-reload
   :websocket-url "ws://localhost:3449/figwheel-ws"
   :jsload-callback (fn [] (print "reloaded"))) ;; optional callback
  )
(connect)
