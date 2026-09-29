(ns hive-vessel.span-lines-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-vessel.doc :as doc]
            [hive-vessel.layout.cells :as cells]
            [hive-vessel.layout.dag :as dag]
            [hive-vessel.dialect.json :as json]))

(defn content [line] (apply str (map :text (:spans line))))

(deftest block-examples
  (testing "table aligns display cells and retains headers"
    (let [lines (doc/render-span-lines (doc/doc "T" (doc/table ["name" "value"] [["a" "1"] ["long" "2"]])) 30)]
      (is (= 3 (count (filter #(re-find #"│" (content %)) lines))))))
  (testing "tree branches and cursor ids"
    (let [lines (doc/render-span-lines (doc/doc "T" (doc/tree [{:id "a" :label "A" :children [{:id "b" :label "B"}]}])) 20)]
      (is (some #(= "b" (:id %)) lines))
      (is (some #(re-find #"└─" (content %)) lines))))
  (testing "diff hunk and additions have distinct faces"
    (let [lines (doc/render-span-lines (doc/doc "T" (doc/diff "@@ -1 +1 @@\n-old\n+new")) 20)]
      (is (some #(= :hunk (:face %)) lines))
      (is (some #(= :added (:face %)) lines))))
  (testing "fields carry distinct key and value faces"
    (is (= [:heading :muted :plain]
           (mapv :face (:spans (last (doc/render-span-lines (doc/doc "T" (doc/fields [["k" "v"]])) 20)))))))
  (testing "sparkline and gauge glyphs"
    (is (re-find #"█" (content (last (doc/render-span-lines (doc/doc "T" (doc/gauge 5 10)) 20)))))
    (is (re-find #"█" (content (last (doc/render-span-lines (doc/doc "T" (doc/sparkline [0 10])) 20))))))
  (testing "longest-path layering and deterministic crossing sweep"
    (let [nodes [{:id "a"} {:id "b"} {:id "c"}]
          edges [["a" "c"] ["b" "c"]]]
      (is (= [["a" "b"] ["c"]] (dag/layers nodes edges)))
      (is (some #(re-find #"│" (content %))
                (doc/render-span-lines (doc/doc "T" (doc/dag nodes edges)) 30))))))

(deftest json-feature-negotiation
  (let [op {:op :ui/show-panel :panel/id "p" :doc (doc/doc "T" (doc/para "body"))}]
    (is (contains? (first (get (json/show-panel-message op {:vessel/features #{:spans}}) "lines")) "spans"))
    (is (contains? (first (get (json/show-panel-message op {:vessel/features #{}}) "lines")) "text"))))

(def generated-doc
  (gen/let [title gen/string-alphanumeric
            words (gen/vector gen/string-alphanumeric 0 8)
            nums (gen/vector (gen/choose 0 100) 0 16)]
    (doc/doc title (doc/table ["name" "value"] (mapv #(vector % title) words))
             (doc/tree (mapv (fn [x] {:id x :label x}) words))
             (doc/diff (str/join "\n" (map #(str "+" %) words)))
             (doc/fields (mapv #(vector % title) words))
             (doc/sparkline nums) (doc/gauge (count words) 8)
             (doc/dag (mapv (fn [i x] {:id i :label x}) (range) words)
                      (mapv vector (range) (range 1 (count words)))))))

(defspec span-lines-are-width-bounded-and-deterministic 100
  (prop/for-all [d generated-doc width (gen/choose 0 100)]
    (let [a (doc/render-span-lines d width)]
      (and (= a (doc/render-span-lines d width))
           (every? #(<= (cells/display-width (content %)) width) a)))))
