(ns hive-vessel.dag-routes-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-vessel.layout.dag :as dag]
            [hive-vessel.test-helper.dag-path :as path]))

(def example-nodes (mapv (fn [[id label]] {:id id :label label})
                         [["a" "parse"] ["b" "lower"] ["c" "layout"]
                          ["d" "emit"] ["e" "cache"]]))
(def example-edges [["a" "b"] ["a" "c"] ["b" "d"] ["c" "d"]
                    ["a" "d"] ["d" "a"] ["e" "d"]])

(defn snapshot [nodes edges width]
  (mapv (comp str/trimr path/text) (dag/rows nodes edges width)))

(def example-golden
  ["[parse]  [cache]"
   "   ▲        ┬"
   "   │        └────────────┐"
   "   ├─────────┐           │"
   "   ├─────────┼─────┐     │"
   "   ├─────────┼─────┼──┐  │"
   "   ▼         ▼     │  │  │"
   "[lower]  [layout]  │  │  │"
   "   ┬         ┬     │  │  │"
   "   │         │     │  │  │"
   "   ├─────────┘     │  │  │"
   "   ├───────────────┘  │  │"
   "   ├──────────────────┘  │"
   "   ├─────────────────────┘"
   "   ▼"
   "[emit]"])

(deftest dag-goldens
  (testing "full labels at both widths and a visible feedback head"
    (doseq [w [80 30]]
      (is (= example-golden (snapshot example-nodes example-edges w)))
      (is (= 7 (count (mapcat :edges (dag/rows example-nodes example-edges w)))))
      (is (= 1 (count (filter :reversed? (mapcat :edges (dag/rows example-nodes example-edges w))))))))
  (testing "diamond"
    (is (= ["[a]" " ┬" " │" " ├────┐" " ▼    ▼" "[b]  [c]"
            " ┬    ┬" " │    │" " ├────┘" " ▼" "[d]"]
           (snapshot (mapv #(hash-map :id %) ["a" "b" "c" "d"])
                     [["a" "b"] ["a" "c"] ["b" "d"] ["c" "d"]] 40))))
  (testing "chain"
    (is (= ["[a]" " ┬" " │" " ▼" "[b]" " ┬" " │" " ▼" "[c]"]
           (snapshot (mapv #(hash-map :id %) ["a" "b" "c"])
                     [["a" "b"] ["b" "c"]] 40))))
  (testing "six-way fan-out packs distinct tracks at width 40"
    (is (= ["[0]" " ┬" " │" " ├────┐" " ├────┼────┐"
            " ├────┼────┼────┐" " ├────┼────┼────┼────┐"
            " ├────┼────┼────┼────┼────┐"
            " ▼    ▼    ▼    ▼    ▼    ▼"
            "[1]  [2]  [3]  [4]  [5]  [6]"]
           (snapshot (mapv #(hash-map :id (str %)) (range 7))
                     (mapv #(vector "0" (str %)) (range 1 7)) 40)))))

(deftest labels-only-shrink-on-overflow
  (let [nodes [{:id "a" :label "abcdefghijkl"}
               {:id "b" :label "short"} {:id "c" :label "t"}]
        edges [["a" "c"] ["b" "c"]]]
    (is (str/includes? (first (snapshot nodes edges 30)) "[abcdefghijkl]"))
    (is (= "[abcdefg]  [short]" (first (snapshot nodes edges 18))))
    (is (every? #(<= (count %) 18) (snapshot nodes edges 18)))))

(def generated-acyclic-graph
  (gen/let [n (gen/choose 2 8)
            pairs (gen/vector (gen/tuple (gen/choose 0 7) (gen/choose 0 7)) 0 18)]
    (let [nodes (mapv #(hash-map :id (str %) :label (str "node" %)) (range n))
          edges (->> pairs
                     (map (fn [[a b]] [(mod a n) (mod b n)]))
                     (remove (fn [[a b]] (= a b)))
                     (map (fn [[a b]] [(str a) (str b)])) distinct vec)]
      [nodes edges])))

(defspec each-edge-is-traceable-in-the-rendered-grid 100
  (prop/for-all [[nodes edges] generated-acyclic-graph]
    (let [rows (dag/rows nodes edges 80)
          arcs (:arcs (dag/layout nodes edges))]
      (every? (fn [{:keys [from to reversed?]}]
                (path/trace? rows from to reversed?)) arcs))))
