(ns abk.dag
  (:require [clojure.set :as s]))

(defn- to-map [graph]
  (loop [m {} [a b & tail] graph]
    (cond (and (keyword? a) (coll? b))    (recur (conj m [a (set b)]) tail)
          (and (keyword? a) (keyword? b)) (recur (conj m [a #{}]) (cons b tail))
          (and (keyword? a) (nil? b))     (conj m [a #{}])
          (nil? a) m)))

(defn- incoming-edges [m]
  (let [ik (set (apply concat (vals m)))]
    (into (list) (remove ik) (keys m))))

(defn- c-edges [m]
  (reduce (fn [accum s] (s/union accum s)) #{} (vals m)))

(defn- topo-sort [graph]
  (loop [m  (to-map graph)
         s  (incoming-edges m)
         sorted []]
    (if-not (empty? s)
      (let [[head-node & tail-nodes] s
            node-edges (get m head-node)
            smaller-m  (dissoc m head-node)
            edges      (c-edges smaller-m)]
        (recur
          smaller-m
          (reduce (fn [accum e] (if-not (contains? edges e) (cons e accum) accum)) tail-nodes node-edges)
          (conj sorted head-node)))
      sorted)))

(defn graph-sort [g]
  (let [sorted (topo-sort g)]
    (if (< (count sorted) (count (filter keyword? g)))
      (throw (ex-info "graph contains cycles" {:graph g}))
      (reverse sorted))))
