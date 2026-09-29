(ns hive-vessel.layout.dag
  "Pure, deterministic layered DAG layout. Cycles are broken by the stable node order;
   the resulting forward graph is layered by longest path and swept by barycenter."
  (:require [clojure.string :as str]
            [hive-vessel.layout.cells :as cells]))

(defn layers
  "Return ordered layers of node ids. edges are [from to] pairs; backwards
   edges (including cycles) are omitted from the layout, not from the input."
  [nodes edges]
  (let [ids (vec (distinct (map :id nodes)))
        index (zipmap ids (range))
        forward (filter (fn [[a b]] (and (contains? index a) (contains? index b)
                                         (< (index a) (index b)))) edges)
        depths (reduce (fn [ds id]
                         (assoc ds id (reduce max 0 (map #(inc (get ds (first %) 0))
                                                        (filter (fn [[_ b]] (= b id)) forward)))))
                       {} ids)
        grouped (->> ids (group-by depths) (sort-by key) (mapv (comp vec val)))]
    ;; A pair of alternating barycenter sweeps reduces crossings without
    ;; introducing nondeterministic ties (original position wins ties).
    (reduce (fn [ls direction]
              (reduce (fn [acc i]
                        (let [adjacent (nth acc (+ i direction) [])
                              positions (zipmap adjacent (range))
                              neighbors (fn [id] (keep (fn [[a b]]
                                                          (when (= id (if (= direction -1) b a))
                                                            (get positions (if (= direction -1) a b))))
                                                        forward))
                              ordered (->> (nth acc i)
                                           (map-indexed (fn [j id]
                                                          (let [ps (neighbors id)]
                                                            [id (if (seq ps) (/ (reduce + ps) (count ps)) j) j])))
                                           (sort-by (juxt second #(nth % 2))) (mapv first))]
                          (assoc acc i ordered)))
                      ls (if (= direction -1) (range 1 (count ls))
                             (range (- (count ls) 2) -1 -1))))
            grouped [-1 1])))

(defn rows
  "A width-bounded graph of labeled boxes, with box-drawing forward edges.
   Node labels are supplied already clipped to max-label columns."
  [nodes edges max-label]
  (let [by-id (into {} (map (juxt :id identity) nodes))
        ls (layers nodes edges)
        clip #(cells/clip (str %) max-label)]
    (vec (mapcat (fn [i layer]
                   (let [prev (set (nth ls (dec i) []))]
                     (mapcat (fn [id]
                               (let [incoming (->> edges (filter #(and (= (second %) id)
                                                                      (contains? prev (first %))))
                                                   (map first) (sort-by str))
                                     label (clip (or (:label (by-id id)) id))]
                                 (cond-> [] (seq incoming) (conj (str "  │ " (str/join ", " (map (comp clip str) incoming)) " ↓"))
                                         true (conj (str "  └─[" label "]")))))
                             layer)))
                 (range (count ls)) ls))))
