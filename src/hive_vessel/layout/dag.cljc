(ns hive-vessel.layout.dag
  "Pure deterministic Sugiyama layout: orient feedback arcs, layer, order,
   insert dummy waypoints, then lower to character-cell span rows.
   Every edge owns an orthogonal track in its layer gap (greedy interval
   packing, deterministic); junctions only where a vertical genuinely
   crosses or tees into a horizontal. Feedback arcs are drawn back to their
   original target and marked with a ▲ head plus :reversed metadata."
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
                                        :span 1 :arc arc}) (range a b))))) (range) arcs))))

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

;;; Orthogonal routes are computed as geometry first and then projected to cells.
;;; Direction sets preserve true crossings instead of blindly merging buses.

(def ^:private glyph-directions
  {\│ #{:up :down} \─ #{:left :right}
   \┌ #{:right :down} \┐ #{:left :down}
   \└ #{:up :right} \┘ #{:up :left}
   \┬ #{:left :right :down} \┴ #{:left :right :up}
   \├ #{:up :right :down} \┤ #{:up :left :down}
   \┼ #{:up :down :left :right}})
(def ^:private directions-glyph (into {} (map (fn [[glyph dirs]] [dirs glyph]) glyph-directions)))

(defn- junction [old new]
  (cond
    (= old \space) new
    (= new \space) old
    (#{\▲ \▼} new) new
    (#{\▲ \▼} old) old
    :else (get directions-glyph
               (into (get glyph-directions old #{}) (get glyph-directions new #{})) \┼)))

(defn- paint [canvas x glyph]
  (if (<= 0 x (dec (count canvas))) (update canvas x junction glyph) canvas))

(defn- assign-tracks
  "First-fit interval packing in input order. Closed intervals must not touch:
   different edges sharing a port retain separate tracks."
  [segments]
  (reduce (fn [tracks {:keys [a z] :as seg}]
            (let [lo (min a z) hi (max a z)
                  free? (fn [track]
                          (every? (fn [{x :a y :z}]
                                    (or (< (max x y) lo) (< hi (min x y)))) track))
                  i (first (filter #(free? (nth tracks %)) (range (count tracks))))]
              (if (some? i) (update tracks i conj seg) (conj tracks [seg]))))
          [] segments))

(defn- draw-segment
  "Route a segment from its upper port, across one track, to its lower port.
   Each track has its own row; verticals cross other tracks only at actual
   intersections. The final arrow is at the ORIGINAL target for feedback arcs."
  [grid track count-tracks {:keys [a z arc from to]}]
  (let [last-row (inc count-tracks)
        ;; A dummy has no box: it must connect to the previous gap through │.
        grid (update grid 0 paint a (if (vector? from) \│ \┬))
        grid (reduce (fn [g row] (update g row paint a \│)) grid (range 1 (inc track)))
        row (inc track)
        grid (if (= a z)
               (update grid row paint a \│)
               (let [grid (update grid row paint a (if (< a z) \└ \┘))
                     grid (reduce (fn [g x] (update g row paint x \─))
                                  grid (range (inc (min a z)) (max a z)))]
                 (update grid row paint z (if (< a z) \┐ \┌))))
        grid (reduce (fn [g r] (update g r paint z \│)) grid (range (inc row) last-row))]
    (if (and (:reversed? arc) (= from (:target arc)))
      (assoc-in (update grid last-row paint z (if (vector? to) \│ \▼)) [0 a] \▲)
      (update grid last-row paint z (if (vector? to) \│ \▼)))))

(defn- gap-rows [positions segments width]
  (let [segments (mapv (fn [{:keys [from to] :as seg}]
                         (assoc seg :a (positions from) :z (positions to))) segments)
        tracks (assign-tracks segments)
        blank (vec (repeat width \space))
        grid (vec (repeat (+ 2 (count tracks)) blank))
        grid (reduce (fn [g [i track]]
                       (reduce (fn [g seg] (draw-segment g i (count tracks) seg)) g track))
                     grid (map-indexed vector tracks))]
    (mapv #(text-row (apply str %) :muted) grid)))

(defn- node-row [layer label-of positions width]
  (let [canvas (vec (repeat width \space))
        cells-map (into {} (keep (fn [id]
                                   (when-not (vector? id)
                                     (let [label (label-of id)
                                           x (positions id)]
                                       [id [(- x (quot (+ (cells/display-width label) 2) 2)) (+ (- x (quot (+ (cells/display-width label) 2) 2)) (cells/display-width label) 2)]]))) layer))
        canvas (reduce (fn [c id]
                         (if (vector? id) (paint c (positions id) \│)
                             (let [[start _] (cells-map id)
                                   box (str "[" (label-of id) "]")]
                               (reduce (fn [c [i ch]] (assoc c (+ start i) ch))
                                       c (map-indexed vector box))))) canvas layer)
        spans (loop [i 0 result []]
                (if (>= i width) result
                    (let [id (first (filter (fn [[_ [a b]]] (<= a i (dec b))) cells-map))
                          face (if id :heading :muted)
                          end (or (first (filter (fn [j]
                                                   (not= face (if (some (fn [[_ [a b]]] (<= a j (dec b))) cells-map)
                                                                 :heading :muted)))
                                                 (range (inc i) width))) width)]
                      (recur end (conj result {:text (apply str (subvec canvas i end)) :face face})))))]
    (cond-> {:face :plain :spans spans}
      (= 1 (count cells-map)) (assoc :id (str (ffirst cells-map)))
      (> (count cells-map) 1) (assoc :cells (into {} (map (fn [[id range]] [(str id) range]) cells-map))))))

(defn- box-width [id label-of]
  (if (vector? id) 1
      (+ 2 (cells/display-width (label-of id)))))

(defn- layer-needed
  "Display cells a layer occupies at the given label size: box widths plus
   two columns of separation (dummy waypoints occupy a single slot)."
  [layer label-of]
  (+ (reduce + (map #(box-width % label-of) layer))
     (* 2 (max 0 (dec (count layer))))))

(defn- fit-label-size
  "Shrink only overflowing layers, widest label first. Remove the whole
   excess in one clip instead of one character per pass (large labels occur
   in generated documents). Retain the vertical fallback when even one-cell
   labels cannot fit."
  [by-id slot-layers width]
  (let [real (distinct (remove vector? (mapcat identity slot-layers)))
        full (into {} (map (juxt identity #(str (or (:label (by-id %)) %))) real))]
    (reduce (fn [sizes layer]
              (loop [sizes sizes]
                (let [label-of #(get sizes %)
                      excess (- (layer-needed layer label-of) width)
                      candidates (sort-by (fn [id] [(- (cells/display-width (label-of id))) (str id)])
                                          (remove vector? layer))
                      target (first (filter #(> (cells/display-width (label-of %)) 1) candidates))]
                  (if (or (<= excess 0) (nil? target))
                    sizes
                    (let [old (cells/display-width (label-of target))
                          clipped (cells/clip (label-of target) (max 1 (- old excess)))]
                      (if (= clipped (label-of target)) sizes
                          (recur (assoc sizes target clipped))))))))
            full slot-layers)))

(defn rows
  "Width-bounded span rows. Every gap between layers gets one row per edge
   track plus a head row and a tail row; each input edge is traceable as an
   orthogonal path from its source box to its target box (▲ marks the
   original target of a reversed feedback arc)."
  [nodes edges width]
  (let [{:keys [layers arcs]} (layout nodes edges)
        width (max 0 width)
        by-id (into {} (map (juxt :id identity) nodes))
        segments (segments layers arcs)
        slot-layers (slots layers segments)
        sizes (fit-label-size by-id slot-layers width)
        fits? (every? #(<= (layer-needed % (fn [id] (get sizes id))) width) slot-layers)]
    (if-not fits?
      (vertical nodes arcs width)
      (let [positions (mapv (fn [layer]
                              (loop [ids layer x 0 result {}]
                                (if-let [id (first ids)]
                                  (recur (next ids) (+ x (box-width id (fn [_] (get sizes id))) 2)
                                         (assoc result id (if (vector? id) x (+ x (quot (box-width id (fn [_] (get sizes id))) 2)))))
                                  result)))
                            slot-layers)]
        (vec (mapcat (fn [i layer]
                       (let [node (node-row layer (fn [id] (get sizes id)) (positions i) width)
                             loops (filter #(and (= (:from %) (:to %)) (some #{(:from %)} layer)) arcs)
                             node (if (seq loops) (assoc node :edges (vec loops)) node)]
                         (if (= i (dec (count slot-layers))) [node]
                             (let [segs (filter #(= i (:layer %)) segments)
                                   gap (gap-rows (merge (positions i) (positions (inc i))) segs width)]
                               (into [node] (map-indexed (fn [j row]
                                                           (if (zero? j) (assoc row :edges (mapv :arc (remove #(vector? (:from %)) segs))) row))
                                                         gap))))))
                     (range (count slot-layers)) slot-layers))))))
