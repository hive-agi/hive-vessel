(ns hive-vessel.layout.dag
  "Pure deterministic Sugiyama layout: orient feedback arcs, layer, order,
   insert dummy waypoints, then lower to character-cell span rows."
  (:require [hive-vessel.layout.cells :as cells]))

(defn oriented-edges
  "DFS feedback-arc orientation. Only gray-target (back) edges are reversed.
   :from/:to are layout direction; :source/:target preserve the input edge."
  [nodes edges]
  (let [ids (vec (distinct (map :id nodes)))
        valid (set ids)
        es (vec (filter (fn [[a b]] (and (valid a) (valid b))) edges))
        outgoing (group-by first es)
        state (atom {})
        reversed (atom #{})]
    (letfn [(visit [id]
              (swap! state assoc id :gray)
              (doseq [[_ target :as edge] (get outgoing id)]
                (case (get @state target)
                  :gray (swap! reversed conj edge)
                  :black nil
                  (visit target)))
              (swap! state assoc id :black))]
      (doseq [id ids] (when-not (@state id) (visit id)))
      (mapv (fn [[a b :as edge]]
              (let [back? (or (= a b) (@reversed edge))]
                {:source a :target b :from (if back? b a) :to (if back? a b)
                 :reversed? (boolean back?)})) es))))

(defn- ordered-layers [ids arcs]
  (let [order (zipmap ids (range))
        out (group-by :from arcs)
        indegree (reduce (fn [m {:keys [from to]}]
                           (if (= from to) m (update m to inc)))
                         (zipmap ids (repeat 0)) arcs)
        [topo _] (loop [pending (set ids) deg indegree result []]
                   (if (empty? pending) [result deg]
                       (let [ready (first (sort-by order (filter #(zero? (deg %)) pending)))
                             ;; In antiparallel pairs an orientation may still cycle.
                             ;; Stable rescue ensures termination; layout-edge orientation
                             ;; is finalized below against the resulting topological order.
                             id (or ready (first (sort-by order pending)))]
                         (recur (disj pending id)
                                (reduce (fn [d {:keys [to]}] (update d to dec)) deg (get out id))
                                (conj result id)))))
        rank (zipmap topo (range))
        forward (mapv (fn [arc]
                        (if (or (= (:from arc) (:to arc))
                                (< (rank (:from arc)) (rank (:to arc))))
                          arc
                          (assoc arc :from (:to arc) :to (:from arc)
                                 :reversed? (not (:reversed? arc))))) arcs)
        depths (reduce (fn [ds id]
                         (assoc ds id (reduce max 0
                                              (for [{:keys [from to]} forward
                                                    :when (and (= to id) (not= from to))]
                                                (inc (get ds from 0)))))) {} topo)
        grouped (->> ids (group-by depths) (sort-by key) (mapv (comp vec val)))]
    {:arcs forward
     :layers (reduce (fn [ls direction]
                       (reduce (fn [acc i]
                                 (let [adjacent (nth acc (+ i direction))
                                       positions (zipmap adjacent (range))
                                       neighbors (fn [id]
                                                   (keep (fn [{:keys [from to]}]
                                                           (when (= id (if (= direction -1) to from))
                                                             (get positions (if (= direction -1) from to)))) forward))]
                                   (assoc acc i (->> (nth acc i)
                                                     (map-indexed (fn [j id]
                                                                    (let [ps (neighbors id)]
                                                                      [id (if (seq ps) (/ (reduce + ps) (count ps)) j) j])))
                                                     (sort-by (juxt second #(nth % 2))) (mapv first)))))
                               ls (if (= direction -1) (range 1 (count ls))
                                      (range (- (count ls) 2) -1 -1))))
                     grouped [-1 1])}))

(defn layout [nodes edges]
  (ordered-layers (vec (distinct (map :id nodes))) (oriented-edges nodes edges)))

(defn layers [nodes edges] (:layers (layout nodes edges)))

(defn- text-row [text face & {:as metadata}]
  (merge {:face face :spans [{:text text :face face}]} metadata))

(defn- vertical [nodes arcs width]
  (let [by-id (into {} (map (juxt :id identity) nodes))
        ids (mapcat identity (:layers (ordered-layers (vec (distinct (map :id nodes))) arcs)))]
    (mapv (fn [id]
            (text-row (cells/clip (str "  └─[" (cells/clip (str (or (:label (by-id id)) id)) (max 0 (- width 6))) "]"
                                   (when (some #(and (= id (:to %)) (:reversed? %)) arcs) " ↶")) width)
                      :heading :id (str id)
                      :edges (vec (filter #(= id (:to %)) arcs)))) ids)))

(defn- segments [ls arcs]
  (let [depth (into {} (mapcat (fn [i layer] (map (fn [id] [id i]) layer)) (range) ls))]
    (vec (mapcat (fn [arc-index {:keys [from to] :as arc}]
                   (if (= from to) []
                       (let [a (depth from) b (depth to)]
                         (mapv (fn [i] {:layer i :from (if (= i a) from [:dummy arc-index i])
                                        :to (if (= (inc i) b) to [:dummy arc-index (inc i)])
                                        :arc arc}) (range a b))))) (range) arcs))))

(defn- slots [ls segments]
  (mapv (fn [i layer]
          (let [dummies (->> segments (filter #(= (inc (:layer %)) i)) (map :to)
                             (filter vector?) distinct)
                positions (zipmap layer (range))]
            (->> (concat layer dummies)
                 (sort-by (fn [id]
                            (if (vector? id)
                              (let [incoming (first (filter #(= id (:to %)) segments))]
                                [(get positions (:from incoming) (count layer)) 1 (pr-str id)])
                              [(positions id) 0 ""]))) vec))) (range (count ls)) ls))

(defn- junction [old new]
  (if (= old \space) new
      (if (= old new) old
          (cond
            (= new \▼) \▼
            (= old \▼) \▼
            (and (#{\─ \┌ \┐ \┬ \┴ \┼} old) (#{\│ \┌ \┐ \└ \┘ \┬ \┴ \┼} new)) \┼
            (and (#{\─ \┌ \┐ \┬ \┴ \┼} new) (#{\│ \┌ \┐ \└ \┘ \┬ \┴ \┼} old)) \┼
            :else \┼))))

(defn- paint [canvas x glyph]
  (if (<= 0 x (dec (count canvas))) (update canvas x junction glyph) canvas))

(defn- gap-rows [positions segments width]
  (let [top (vec (repeat width \space))
        mid top
        bottom top
        [top mid bottom] (reduce (fn [[t m b] {:keys [from to]}]
                                   (let [a (positions from) z (positions to)
                                         m (reduce #(paint %1 %2 \─) m (range (inc (min a z)) (max a z)))
                                         m (paint (paint m a (if (< a z) \└ \┘)) z
                                                  (if (< a z) \┐ \┌))]
                                     [(paint t a \│) m (paint b z \▼)]))
                                 [top mid bottom] segments)
        top (reduce (fn [canvas {:keys [from arc]}]
                      (if (:reversed? arc) (paint canvas (positions from) \↶) canvas)) top segments)]
    (mapv #(text-row (apply str %) :muted) [top mid bottom])))

(defn- node-row [layer by-id label-size positions width]
  (let [canvas (vec (repeat width \space))
        cells-map (into {} (keep (fn [id]
                                   (when-not (vector? id)
                                     (let [label (cells/clip (str (or (:label (by-id id)) id)) label-size)
                                           x (positions id)]
                                       [id [(- x 1) (+ x (cells/display-width label) 1)]]))) layer))
        canvas (reduce (fn [c id]
                         (if (vector? id) (paint c (positions id) \│)
                             (let [[start _] (cells-map id)
                                   label (cells/clip (str (or (:label (by-id id)) id)) label-size)
                                   box (str "[" label "]")]
                               (reduce (fn [c [i ch]] (assoc c (+ start i) ch))
                                       c (map-indexed vector box))))) canvas layer)
        spans (loop [i 0 result []]
                (if (>= i width) result
                    (let [id (first (filter (fn [[_ [a b]]] (<= a i b)) cells-map))
                          face (if id :heading :muted)
                          end (or (first (filter (fn [j]
                                                   (not= face (if (some (fn [[_ [a b]]] (<= a j b)) cells-map)
                                                                 :heading :muted)))
                                                 (range (inc i) width))) width)]
                      (recur end (conj result {:text (apply str (subvec canvas i end)) :face face})))))]
    (cond-> {:face :plain :spans spans}
      (= 1 (count cells-map)) (assoc :id (str (ffirst cells-map)))
      (> (count cells-map) 1) (assoc :cells (into {} (map (fn [[id range]] [(str id) range]) cells-map))))))

(defn rows
  "Width-bounded span rows. :edges on gap rows preserve original edge identity
   and reversal even when several edges share a drawn junction."
  [nodes edges width]
  (let [{:keys [layers arcs]} (layout nodes edges)
        width (max 0 width)
        by-id (into {} (map (juxt :id identity) nodes))
        segments (segments layers arcs)
        slot-layers (slots layers segments)
        max-slots (reduce max 0 (map count slot-layers))
        ;; Shrink all labels uniformly before falling back; 3 columns per box
        ;; plus two columns of separation (also for dummy waypoints).
        label-size (min 12 (max 1 (quot (+ (- width (* 3 max-slots)) 2) (max 1 (count nodes)))))
        needed (apply max 0 (map (fn [layer]
                                   (+ (reduce + (map #(if (vector? %) 1 (+ label-size 2)) layer))
                                      (* 2 (max 0 (dec (count layer)))))) slot-layers))]
    (if (> needed width)
      (vertical nodes arcs width)
      (let [positions (mapv (fn [layer]
                              (loop [ids layer x 0 result {}]
                                (if-let [id (first ids)]
                                  (let [size (if (vector? id) 1
                                                 (+ 2 (cells/display-width
                                                       (cells/clip (str (or (:label (by-id id)) id)) label-size))))]
                                    (recur (next ids) (+ x size 2) (assoc result id (if (vector? id) x (inc x)))))
                                  result))) slot-layers)]
        (vec (mapcat (fn [i layer]
                       (let [node (node-row layer by-id label-size (positions i) width)
                             loops (filter #(and (= (:from %) (:to %)) (some #{(:from %)} layer)) arcs)
                             node (if (seq loops) (assoc node :edges (vec loops)) node)]
                         (if (= i (dec (count slot-layers))) [node]
                             (let [segs (filter #(= i (:layer %)) segments)
                                   gap (gap-rows (merge (positions i) (positions (inc i))) segs width)]
                               (into [node] (map-indexed (fn [j row]
                                                           (if (zero? j) (assoc row :edges (mapv :arc (filter #(= (:from %) (get-in % [:arc :from])) segs))) row)) gap))))))
                     (range (count slot-layers)) slot-layers))))))
