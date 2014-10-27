(ns lein-ui.http.route)

(def entities {[:project] "/project"
               [:project :map] "/project/map"
               [:project :raw-map] "/project/raw-map"
               [:project :repl] "/project/repl"
               [:project :repl :eval] "/project/repl/eval"})

(defn url-for [entity]
  (str "http://localhost:8000/api"
       (or (entities entity)
           (throw (ex-info "No url for entity" entity)))))
