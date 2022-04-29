(ns abk.dag-test
  (:require [clojure.test :refer :all]
            [abk.dag :refer :all]
            [clojure.set :as s]))

(def graph {:foobar {}
            :5  {:abk.core/deps [:11]}
            :2  {}
            :7  {:abk.core/deps [:11 :8]}
            :11 {:abk.core/deps [:2 :9 :10]}
            :8  {:abk.core/deps [:9]}
            :3  {:abk.core/deps [:8 :10]}
            :baz {}
            :cool {}
            :9  {}
            :10 {}})

(def easy-dag {:1 {:abk.core/deps [:2]}
               :2 {:abk.core/deps [:3]}
               :3 {:abk.core/deps [:4]}
               :4 {:abk.core/deps []}})

(defn test-sorted [g sorted]
  (println sorted)
  (loop [[head & tail] sorted]
    (if (nil? head)
      ::sorted
      (if (every? #(< (.indexOf sorted head ) (.indexOf sorted %)) (-> (get g head) :abk.core/deps))
        (recur tail)
        (throw (ex-info (str "sorted: " sorted ", started: " head "\nbad sort at: " head) g))))))

(deftest sort-dag
  (testing "sort dag"
    (test-sorted graph (topo-sort graph))))

(deftest sort-easy
  (testing "sort easy dag"
    (is (= (topo-sort easy-dag) [:1 :2 :3 :4]))))
