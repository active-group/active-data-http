(ns hooks.active-data-http
  (:require [clj-kondo.hooks-api :as api]))

;; test via
;; (require '[clj-kondo.hooks-api :as api])
;; (load-file "resources/clj-kondo.exports/de.active-group/active.data.http/hooks/active_data_http.clj")
;; (hooks.active-data-http/defn-rpc {:node (api/parse-string "(defn-rpc call context \"docstring\" :- result [arg :- arg-t])")})

(defn- analyze-params [children]
  (loop [children children
         params []
         refs []]
    (cond
      (empty? children)
      [params refs]

      (and (api/keyword-node? (second children)) (= :- (:k (second children))))
      (recur (drop 3 children)
             (conj params (first children))
             (conj refs (second (rest children))))

      :else
      (recur (rest children)
             params
             refs))))

(defn- analyze-defn-rpc-args [all-args]
  (let [name (first all-args)
        context (second all-args)]
    (loop [args (drop 2 all-args)
           params nil
           refs [context]]
      (cond
        (empty? args)
        [name params refs]

        (api/string-node? (first args))
        ;; ignore docstring
        (recur (rest args)
               params
               refs)

        (and (api/keyword-node? (first args)) (= :- (:k (first args))))
        (recur (drop 2 args)
               params
               (conj refs (second args)))

        (api/vector-node? (first args))
        (let [[params references] (analyze-params (:children (first args)))]
          (recur (rest args)
                 params
                 (concat refs references)))

        :else
        (recur (rest args)
               params
               refs)))))

(defn defn-rpc [expr]
  (-> expr
      (update :node
              (fn [node]
                (if (api/list-node? node)
                  (let [args (rest (:children node)) ;; drop defn-rpc
                        [name params references] (analyze-defn-rpc-args args)
                        plain-defn (api/list-node (list (api/token-node 'defn)
                                                        name
                                                        (api/vector-node params)
                                                        ;; as the body of the defn, reference the args.
                                                        (api/vector-node params)))]
                    (api/list-node (list* (api/token-node 'do)
                                          plain-defn
                                          references)))
                  node)))))
