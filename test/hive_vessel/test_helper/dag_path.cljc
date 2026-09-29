(ns hive-vessel.test-helper.dag-path
  "Pure connectivity tracer over the rendered character grid. Box glyphs are
   endpoints, never transit cells; turns must agree on both sides of a cell."
  (:require [clojure.string :as str]))

(def ports
  {\│ #{:up :down} \─ #{:left :right}
   \┌ #{:right :down} \┐ #{:left :down}
   \└ #{:up :right} \┘ #{:up :left}
   \┬ #{:left :right :down} \┴ #{:left :right :up}
   \├ #{:up :right :down} \┤ #{:up :left :down}
   \┼ #{:up :down :left :right}
   \▼ #{:up :down} \▲ #{:up :down}})

(defn text [row] (apply str (map :text (:spans row))))

(defn box-centers [rows]
  (into {}
        (mapcat (fn [[y row]]
                  (if-let [cells (:cells row)]
                    (map (fn [[id [a b]]] [id [y (+ a (quot (- b a) 2))]]) cells)
                    (when-let [id (:id row)]
                      (let [s (text row)
                            a (str/index-of s "[") b (str/index-of s "]")]
                        (when (and a b) [[id [y (+ a (quot (inc (- b a)) 2))]]])))))
                (map-indexed vector rows))))

(def opposite {:up :down :down :up :left :right :right :left})
(def delta {:up [-1 0] :down [1 0] :left [0 -1] :right [0 1]})

(defn trace?
  "True iff an orthogonally connected path joins the specified box centers.
   Feedback arcs are routed from layout :from down to :to, marked ▲ at their
   original target; the direction choice is supplied by the caller."
  [rows from to reversed?]
  (let [grid (mapv (comp vec text) rows)
        centers (box-centers rows)
        [sy sx] (centers from) [ty tx] (centers to)
        start [(inc sy) sx] goal [(dec ty) tx]
        glyph (fn [[y x]] (get-in grid [y x] \space))
        step (fn [[y x] dir] (mapv + [y x] (delta dir)))
        start-glyph (glyph start)]
    (and sy ty (< sy ty)
         (if reversed? (= \▲ start-glyph) (boolean ((get ports start-glyph #{}) :down)))
         (#{\▼ \▲} (glyph goal))
         (loop [queue [[start :down]] index 0 seen #{}]
           (if (>= index (count queue)) false
               (let [[p incoming] (queue index)]
                 (if (= p goal) true
                     (if (seen [p incoming]) (recur queue (inc index) seen)
                         (let [outgoing (if (= \┼ (glyph p))
                                          ;; A crossing never lets a path turn. Only a
                                          ;; genuine tee can branch into another edge.
                                          [incoming]
                                          (disj (get ports (glyph p) #{}) (opposite incoming)))
                               nexts (for [dir outgoing
                                           :let [q (step p dir)]
                                           :when ((get ports (glyph q) #{}) (opposite dir))]
                                       [q dir])]
                           (recur (into queue nexts) (inc index)
                                  (conj seen [p incoming])))))))))))
