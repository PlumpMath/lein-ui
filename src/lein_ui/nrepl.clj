(ns lein-ui.nrepl
  (:require [clojure.tools.nrepl.server :as server]
            [clojure.tools.nrepl :as nrepl]
            [clojure.tools.nrepl.middleware :as middle]
            [clojure.tools.nrepl.transport :as t]

            [cider.nrepl.middleware.apropos]
            [cider.nrepl.middleware.classpath]
            [cider.nrepl.middleware.complete]
            [cider.nrepl.middleware.info]
            [cider.nrepl.middleware.inspect]
            [cider.nrepl.middleware.macroexpand]
            [cider.nrepl.middleware.ns]
            [cider.nrepl.middleware.resource]
            [cider.nrepl.middleware.stacktrace]
            [cider.nrepl.middleware.test]
            [cider.nrepl.middleware.trace]
            [cider.nrepl.middleware.undef]))


(defonce items (atom {:position 0
                      :latest []}))

(defonce error (atom nil))

(defonce cider-handlers [#'cider.nrepl.middleware.apropos/wrap-apropos
                         #'cider.nrepl.middleware.classpath/wrap-classpath
                         #'cider.nrepl.middleware.complete/wrap-complete
                         #'cider.nrepl.middleware.info/wrap-info
                         #'cider.nrepl.middleware.inspect/wrap-inspect
                         #'cider.nrepl.middleware.macroexpand/wrap-macroexpand
                         #'cider.nrepl.middleware.ns/wrap-ns
                         #'cider.nrepl.middleware.resource/wrap-resource
                         #'cider.nrepl.middleware.stacktrace/wrap-stacktrace
                         #'cider.nrepl.middleware.test/wrap-test
                         #'cider.nrepl.middleware.trace/wrap-trace
                         #'cider.nrepl.middleware.undef/wrap-undef
                         ])

(defn update-log [items msg]
  (try
    (swap! items (fn [{:keys [position latest]}]
                   {:position (inc position)
                    :latest ((comp vec (partial take 500) (partial conj latest))
                             msg)}))
    (catch Exception e
      (reset! error e))))

(deftype LogTransport [items inner]
  t/Transport
  (send [this msg]
    (update-log items msg)
    (t/send inner msg))
  (recv [this msg]
    (t/recv inner msg)))

(defn log-middleware [items handler]
  (fn [msg]    
    (update-log items msg)
    (handler (update-in msg [:transport] #(LogTransport. items %)))))

(defn partial-middleware [middleware-var & args]
  (with-meta (apply partial middleware-var args)
    (meta middleware-var)))

(middle/set-descriptor! #'log-middleware
                        {:requires (constantly true)
                         :expects #{}})

(defn lein-ui-handler [middlewares]
  (apply server/default-handler (concat [(partial-middleware log-middleware items)] cider-handlers middlewares)))

(defn lein-ui-nrepl [& {:keys [] :as opts}]
  (apply server/start-server
         (apply concat
                (seq (merge {:handler (lein-ui-handler [])}
                            opts)))))
(defn get-messages [items from]
  (let [{:keys [position latest]} @items
        n (count latest)
        drop-n (max (- position n)
                    from)]
    {:from (if (<= drop-n 0) (- position n) from)
     :messages (drop drop-n latest)}))
