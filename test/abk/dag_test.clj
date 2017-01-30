(ns abk.dag-test
  (:require [clojure.test :refer :all]
            [abk.dag :refer :all]))

(def graph [:5  [:11]
            :7  [:11 :8]
            :11 [:2 :9 :10]
            :8  [:9]
            :3  [:8 :10]
            :2 :9 :10])

(def easy-dag [:1 [:2] :2 [:3] :3 [:4]])

(deftest sort-dag
  (testing "sort dag"
    (is (= (graph-sort graph) (reverse [:3 :7 :8 :5 :11 :2 :9 :10])))))

(deftest sort-easy
  (testing "sort easy dag"
    (is (= (graph-sort easy-dag) (reverse [:1 :2 :3 :4])))))
