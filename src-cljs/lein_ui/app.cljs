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

(defonce app-state (atom {:current-project {}
                          :projects []}))

(go
  (let [result (<! (http/get "http://localhost:8000/api/projects"))]
    (swap! app-state assoc :projects (-> result :body str edn/read-string :projects))))


(defn fetch-project [result-chan name key url]
  (go
    (let [result (<! (http/get url))]
      (put! result-chan [name key (-> result :body str edn/read-string)]))))

(defn app-list [data owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [set-project active-project]}]
      (html
       [:div {:class "project-list"}
        [:button {:onMouseDown (fn [_]
                                 (println (pr-str @app-state)))}
         "Print State"]
        [:h3 "Projects"]
        (for [project data]
          [:a {:key (project :name)
               :href "#"
               :class (if (= (:name project) active-project)
                        "active-project-link"
                        "")
               :onClick (fn [_]
                               (put! set-project project))}
           (project :name)])]))))

(defn app-view [project owner]
  (reify

    om/IRenderState
    (render-state [_ {:keys [project-data]}]
      (html [:div {:class "active-project-view"}
             (project :name)]))))

(defn app [data owner]
  (reify

    om/IInitState
    (init-state [_]
      {:set-project (chan)})

    om/IWillMount
    (will-mount [_]
      (let [set-project (om/get-state owner :set-project)]
        (go (loop []
              (let [project (<! set-project)]
                (om/transact! data :current-project project))
              (recur)))))

    om/IRenderState
    (render-state [this state]
      (html
       [:div {:id "app-container"}
        (om/build app-list (:projects data) {:init-state {:set-project (:set-project state)}
                                             :state {:active-project (get-in data [:current-project :name])}})
        (om/build app-view (:current-project data))]))))

(om/root
 app
 app-state
 {:target (. js/document (getElementById "my-app"))})

 ;; optional callback

(defn connect []

  (fw/watch-and-reload
   :websocket-url "ws://localhost:3449/figwheel-ws"
   :jsload-callback (fn [] (print "reloaded"))) ;; optional callback
  )
(connect)




