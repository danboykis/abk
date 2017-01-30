(ns abk.core
  (:require [abk.dag :refer [graph-sort]]
            [clojure.set :as cs]
            [clojure.spec :as s]))

(s/def ::state-map (s/map-of keyword? (s/and vector? #(= 3 (count %)) #(and (fn? (nth % 0))
                                                                            (fn? (nth % 1))
                                                                            (keyword? (nth % 2))))))

(s/def ::dep-graph (s/coll-of #(or (keyword? %) (vector? %))))

(defn- check-args [state-map dep-graph]
  (when-not (s/valid? ::state-map state-map)
    (throw (ex-info "not a valid state-map" (s/explain-data ::state-map state-map))))
  (when-not (s/valid? ::dep-graph dep-graph)
    (throw (ex-info "invalid graph structure" (s/explain-data ::dep-graph dep-graph)))))

(defn- add-started [s dep]
  (update s ::started (fnil conj []) dep))

(defn- remove-started [s dep]
  (let [new-s (update s ::started #(vec (remove #{dep} %)))]
    (cond-> new-s
            (empty? (::started new-s)) (dissoc new-s ::started))))



(defn init! [s state-map dep-graph]
  (check-args state-map dep-graph)
  (let [deps (graph-sort dep-graph)]
    (assert (cs/subset? (set deps) (set (keys state-map))) "state-map doesn't have all the necessary states present in dep-graph")
    (loop [ss s
           [head-dep & tails-deps] deps]
      (if-not (nil? head-dep)
        (let [[start! _ k](get state-map head-dep)]
          (recur (-> (assoc ss k (start! ss))
                     (add-started head-dep))
                 tails-deps))
        ss))))

(defn stop! [s state-map dep-graph]
  (check-args state-map dep-graph)
  (let [deps (reverse (graph-sort dep-graph))]
    (assert (cs/subset? (set deps) (set (keys state-map))) "state-map doesn't have all the necessary states present in dep-graph")
    (loop [ss s
           [head-dep & tails-deps] deps]
      (if-not (nil? head-dep)
        (let [[_ stop! k](get state-map head-dep)]
          (recur (do (stop! (get ss k))
                     (-> (dissoc ss k)
                         (remove-started head-dep)))
                 tails-deps))
        ss))))
