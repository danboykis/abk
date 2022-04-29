(ns abk.dag
  (:import [clojure.lang PersistentQueue]))

(defn- adj-edges [graph v]
  (into #{} (comp (filter (fn [[_ nodes]] (contains? nodes v)))
                  (map first))
        (dissoc graph v)))

(defn- indegree [graph v]
  (count (get graph v #{})))

(defn- -t-sort [graph]
  "See: https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm"
  (loop [l []
         s (into PersistentQueue/EMPTY (filter #(zero? (indegree graph %)) (keys graph)))
         g graph]
    (if (empty? s)
      (if (every? empty? (vals g))
        l
        (throw (ex-info "graph has cycles" g)))
      (let [n (peek s)
            [g' s'] (reduce (fn [[g s] m]
                              (let [g' (update g m disj n)]
                                (if (empty? (get g' m))
                                  [g' (conj s m)]
                                  [g' s])))
                            [g s]
                            (adj-edges g n))]
        (recur (conj l n) (pop s') g')))))

(defprotocol Graph
  (add [this node deps])
  (t-sort [this]))

(deftype GraphImpl [g]
  Object
  (toString [_] (let [sb (StringBuilder.)]
                  (doseq [[k v] g]
                    (.append sb k)
                    (.append sb " -> ")
                    (.append sb (clojure.string/join " " v))
                    (.append sb \n))))
  Graph
  (t-sort [_] (reverse (-t-sort g)))
  (add [_ node deps]
    (GraphImpl. (assoc g node (set deps)))))

(defn topo-sort [g]
  (let [ks (set (keys g))
        g' (into {} (map (fn [[k {:abk.core/keys [deps] :as v}]]
                           (if (= :abk.core/everything deps)
                             [k (assoc v :abk.core/deps (disj ks k))]
                             [k v])))
                 g)]
    (t-sort (reduce-kv (fn [gi k v] (add gi k (:abk.core/deps v)))
                          (GraphImpl. {})
                          g'))))
