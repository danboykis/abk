(ns abk.dag
  (:require [clojure.set :as s]
            [clojure.set :as set]))

(defn- c-edges [m]
  (into #{} (comp (map :abk.core/deps) cat) (vals m)))

(defn- incoming-edges [m]
  (let [ik (c-edges m)]
    (into (list) (remove ik) (keys m))))

(defn- topo-sort [graph]
  (loop [m  graph
         s  (incoming-edges m)
         sorted []]
    (if-not (empty? s)
      (let [[head-node & tail-nodes] s
            node-edges (-> m (get head-node) :abk.core/deps)
            smaller-m  (dissoc m head-node)
            edges      (c-edges smaller-m)]
        (recur
          smaller-m
          (reduce (fn [accum e]
                    (if-not (contains? edges e)
                      (cons e accum)
                      accum))
                  tail-nodes
                  node-edges)
          (conj sorted head-node)))
      sorted)))

(defn graph-sort [g]
  (let [sorted (topo-sort g)]
    (if (< (count sorted) (count g))
      (throw (ex-info "graph contains cycles" {:graph g}))
      (reverse sorted))))
