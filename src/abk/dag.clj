(ns abk.dag
  (:import [java.util ArrayDeque]))

(defn- visit [sorted g-ref node]
  (let [node-v (get @g-ref node)]
    (cond (= ::perm (::mark node-v)) sorted
          (= ::temp (::mark node-v))
          (throw (ex-info "invalid dag" {node (dissoc node-v ::mark)}))
          :else
          (do
            (vswap! g-ref assoc-in [node ::mark] ::temp)
            (doseq [dep-node (get node-v :abk.core/deps)]
                (visit sorted g-ref dep-node))
            (vswap! g-ref assoc-in [node ::mark] ::perm)
            (.addFirst sorted node)))))

(defn- transform [g]
  (let [ks (set (keys g))]
    (into {} (map (fn [[k {:abk.core/keys [deps] :as v}]]
                    (if (= :abk.core/everything deps)
                      [k (assoc v :abk.core/deps (disj ks k))]
                      [k v])))
          g)))

(defn graph-sort [g]
  (let [g-ref (volatile! (transform g))
        sorted (ArrayDeque. (count g))]
    (doseq [[k v] g]
      (when-not (::mark v)
        (visit sorted g-ref k)))
    (vec sorted)))

