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


(defn fetch-project [result-chan name key url]
  (go
    (let [result (<! (http/get url))]
      (put! result-chan [name key (-> result :body str edn/read-string)]))))

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

(defn app-view [project owner]
  (reify
    om/IRender
    (render [_]
      (html [:div {:class "active-project-view"}
             [:div (:name project)]
             [:div (get-in project [:raw-map :description])]
             [:ol
              (for [[name version] (get-in project [:raw-map :dependencies])]
                [:li (str name " " version)])]]))))

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
        (om/build app-view (active-project (:projects data)))]))))

(om/root
 app
 app-state
 {:target (. js/document (getElementById "my-app"))})

(defn ensure-project-controller-process [project]
  (if-let [controller (@project :controller)]
    controller
    (let [in (chan)]
      {:in in
       :process (go
                  (fetch-project in "" :summary (@project :url))
                  (let [[_ _ summary] (<! in)]
                    (om/transact! project #(merge % summary)))

                  (fetch-project in "" :raw-map (@project :raw-map-url))
                  (let [[_ _ raw-map] (<! in)]
                    (om/transact! project #(assoc % :raw-map raw-map))))})))

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
