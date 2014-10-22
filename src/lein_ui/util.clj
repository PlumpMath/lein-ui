(ns lein-ui.util
  (:require [clojure.pprint :as pprint]))

(defn pprint-str [o]
  (let [w (java.io.StringWriter.)]
    (pprint/pprint o w)
    (.toString w)))
