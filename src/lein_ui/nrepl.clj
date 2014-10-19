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


(def items (atom {:requests []
                  :responses []}) )



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


(deftype LogTransport [inner]
  t/Transport
  (send [this msg]
    (swap! items update-in [:responses] conj msg)
    (t/send inner msg))
  (recv [this msg]
    (t/recv inner msg)))

(defn log-middleware [handler]
  (fn [message]
    (swap! items update-in [:requests] conj message)
    (handler (update-in message [:transport] #(LogTransport. %)))))


(middle/set-descriptor! #'log-middleware
                        {:requires (constantly true)
                         :expects #{}})

(defn lein-ui-handler [middlewares]
  (apply server/default-handler (concat [log-middleware] cider-handlers middlewares)))

(defn lein-ui-nrepl [& {:keys [] :as opts}]
  (apply server/start-server
         (apply concat
                (seq (merge {:handler (lein-ui-handler [])}
                            opts)))))
