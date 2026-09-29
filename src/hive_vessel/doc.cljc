(ns hive-vessel.doc
  "Vessel-neutral documents: block constructors and the single line renderer.

   An addon describes WHAT to show as a Doc; `render-lines` projects it to
   face-tagged lines once, and every character-cell dialect (Emacs buffer, Vim
   buffer, terminal) paints those lines. Rich dialects (:json) receive the Doc
   itself alongside the lines."
  (:require [clojure.string :as str]
            [hive-vessel.schema :as s]
            [hive-vessel.layout.cells :as cells]
            [hive-vessel.layout.dag :as dag-layout]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Constructors
;; =============================================================================

(defn doc [title & blocks] {:doc/title title :doc/blocks (vec (remove nil? blocks))})

(defn heading
  ([text] {:block/type :heading :text text})
  ([text level] {:block/type :heading :text text :level level}))

(defn para
  ([text] {:block/type :para :text text})
  ([text tone] {:block/type :para :text text :tone tone}))

(defn fields [pairs] {:block/type :fields :fields (mapv (fn [[k v]] [(str k) (str v)]) pairs)})

(defn items [xs] {:block/type :list :items (mapv str xs)})

(defn code
  ([text] {:block/type :code :text text})
  ([text lang] {:block/type :code :text text :lang lang}))

(defn diff [unified-text] {:block/type :diff :text unified-text})

(defn link
  ([file] {:block/type :link :text file :file file})
  ([text file line] (cond-> {:block/type :link :text text :file file}
                      line (assoc :line line))))

;; =============================================================================
;; Diff classification
;; =============================================================================

(defn split-lines
  "Split on any line break, keeping empty lines; a trailing break adds no line."
  [text]
  (if (= "" text) [] (str/split text #"\r\n|\r|\n")))

(defn diff-line-kind
  [line]
  (cond
    (str/starts-with? line "@@") :hunk
    (or (str/starts-with? line "+++") (str/starts-with? line "---")) :hunk
    (str/starts-with? line "+") :added
    (str/starts-with? line "-") :removed
    :else :context))

(defn diff-lines
  "The DiffLine vector of a diff block: its explicit :lines, else its :text
   classified line by line."
  [{:keys [lines text]}]
  (or lines
      (mapv (fn [l] {:line/kind (diff-line-kind l) :line/text l})
            (split-lines (or text "")))))

;; =============================================================================
;; Rendering
;; =============================================================================

(def tone->face {:plain :plain :muted :muted :info :info
                 :success :success :warn :warn :error :error})

(def diff-kind->face {:context :plain :added :added :removed :removed :hunk :hunk})

(defn- lines-of [text face]
  (let [ls (split-lines text)]
    (mapv (fn [l] {:text l :face face}) (if (empty? ls) [""] ls))))

(defmulti block-lines
  "Rendered lines of one block. Open: a new block type is a new method."
  :block/type)

(defmethod block-lines :heading [{:keys [text]}] (lines-of text :heading))

(defmethod block-lines :para [{:keys [text tone]}]
  (lines-of text (tone->face (or tone :plain))))

(defmethod block-lines :fields [{:keys [fields]}]
  (let [width (reduce max 0 (map (comp count first) fields))]
    (into []
          (mapcat (fn [[k v]]
                    (let [pad (apply str (repeat (- width (count k)) " "))
                          [first-line & more] (split-lines v)]
                      (into [{:text (str k pad " : " (or first-line "")) :face :plain}]
                            (map (fn [l] {:text (str (apply str (repeat (+ width 3) " ")) l)
                                          :face :plain}))
                            more))))
          fields)))

(defmethod block-lines :list [{:keys [items]}]
  (into [] (mapcat (fn [item]
                     (let [[l & more] (split-lines item)]
                       (into [{:text (str "  - " (or l "")) :face :plain}]
                             (map (fn [x] {:text (str "    " x) :face :plain}))
                             more))))
        items))

(defmethod block-lines :code [{:keys [text]}]
  (mapv (fn [l] {:text (str "    " l) :face :code}) (let [ls (split-lines text)] (if (empty? ls) [""] ls))))

(defmethod block-lines :diff [block]
  (mapv (fn [{:line/keys [kind text]}]
          ;; A DiffLine's text is one line by contract; flatten defensively.
          {:text (str/replace text #"\r\n|\r|\n" " ") :face (diff-kind->face kind)})
        (diff-lines block)))

(defmethod block-lines :link [{:keys [text file line]}]
  (mapv (fn [l] (cond-> {:text l :face :link :file file}
                  line (assoc :line line)))
        (let [ls (split-lines text)] (if (empty? ls) [""] ls))))

(defmethod block-lines :default [block]
  [{:text (pr-str block) :face :muted}])

(defn render-lines
  "Project DOC to face-tagged lines: the title, then each block separated by a
   blank line."
  [{:doc/keys [title blocks]}]
  (into (lines-of title :title)
        (mapcat (fn [block] (into [{:text "" :face :plain}] (block-lines block))))
        blocks))

(m/=> render-lines [:=> [:cat s/Doc] [:vector s/RenderedLine]])

(defn plain-text
  "The rendered lines of DOC joined by newlines."
  [doc]
  (str/join "\n" (map :text (render-lines doc))))

;; Rich span lowering is independent of dialects. Block methods are extension points;
;; width clipping happens once after lowering, preserving face boundaries.
(defn table [columns rows] {:block/type :table :columns (vec columns) :rows (mapv vec rows)})
(defn tree [roots] {:block/type :tree :nodes (vec roots)})
(defn sparkline [values] {:block/type :sparkline :values (vec values)})
(defn gauge [value maximum] {:block/type :gauge :value value :max maximum})
(defn dag [nodes edges] {:block/type :dag :nodes (vec nodes) :edges (vec edges)})

(defn- span [text face] {:text (str text) :face face})
(defn- row [face & spans] {:face face :spans (vec spans)})

(defmulti block-span-lines
  "Open block-to-span-line lowering. Methods receive [block width]."
  (fn [block _width] (:block/type block)))

(defmethod block-span-lines :table [{:keys [columns rows]} width]
  (let [columns (mapv str columns)
        rows (mapv #(mapv str %) rows)
        n (count columns)
        available (max 1 (- width (* 3 (max 0 (dec n)))))
        natural (mapv (fn [i] (reduce max 0 (map cells/display-width
                                               (cons (nth columns i) (map #(get % i "") rows))))) (range n))
        sizes (loop [sizes (vec (repeat n 0)) left available]
                (if (or (zero? left) (empty? sizes) (every? true? (map >= sizes natural))) sizes
                    (let [i (first (sort-by (fn [j] [(if (< (sizes j) (natural j)) (sizes j) 1000000) j]) (range n)))]
                      (recur (update sizes i inc) (dec left)))))
        format-row (fn [xs face]
                     (row face (span (str/join " │ " (map-indexed
                                                     (fn [i x] (cells/pad (cells/clip x (sizes i)) (sizes i))) xs)) face)))]
    (if (zero? n) []
        (into [(format-row columns :heading)] (map #(format-row (mapv (fn [i] (get % i "")) (range n)) :plain) rows)))))

(defmethod block-span-lines :tree [{:keys [nodes]} _]
  (letfn [(walk [siblings prefix]
            (mapcat (fn [i {:keys [label id children]}]
                      (let [last? (= i (dec (count siblings)))
                            lead (str prefix (if last? "└─ " "├─ "))]
                        (cons (cond-> (row :plain (span lead :muted) (span (or label id "") :plain))
                                id (assoc :id (str id)))
                              (walk (vec children) (str prefix (if last? "   " "│  "))))))
                    (range (count siblings)) siblings))]
    (vec (walk (vec nodes) ""))))

(defmethod block-span-lines :diff [block _]
  (mapv (fn [{:line/keys [kind text] :as line}]
          (cond-> (row (diff-kind->face kind) (span (str/replace (or text "") #"\r\n|\r|\n" " ")
                                                     (diff-kind->face kind)))
            (:id line) (assoc :id (str (:id line))))) (diff-lines block)))

(defmethod block-span-lines :fields [{:keys [fields]} _]
  (mapv (fn [[k v]] (row :plain (span (str k) :heading) (span " : " :muted) (span (str v) :plain))) fields))

(defmethod block-span-lines :sparkline [{:keys [values]} _]
  (let [levels ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"]
        hi (reduce max 0 (filter number? values))
        lo (reduce min 0 (filter number? values))
        extent (- hi lo)]
    [(row :info (span (apply str (map (fn [v] (nth levels (if (and (number? v) (pos? extent))
                                                         (min 7 (int (* 7 (/ (- v lo) extent)))) 0))) values)) :info))]))

(defmethod block-span-lines :gauge [{:keys [value] maximum :max} width]
  (let [n (max 0 (min 40 (- width 8)))
        filled (if (and (number? maximum) (pos? maximum) (number? value))
                 (int (* n (min 1 (max 0 (/ value maximum))))) 0)]
    [(row :info (span (str "[" (apply str (repeat filled "█"))
                            (apply str (repeat (- n filled) "░")) "]") :info))]))

(defmethod block-span-lines :dag [{:keys [nodes edges]} width]
  (mapv #(row :plain (span % :plain)) (dag-layout/rows nodes edges (max 1 (- width 8)))))

(defmethod block-span-lines :default [block _]
  (mapv (fn [{:keys [text face]}] (row face (span text face))) (block-lines block)))

(defn- bound-row [line width]
  (let [[spans _] (reduce (fn [[out remaining] {:keys [text face]}]
                            (let [part (cells/clip (str/replace (str text) #"\r\n|\r|\n" " ") remaining)]
                              [(conj out (span part face)) (- remaining (cells/display-width part))]))
                          [[] (max 0 width)] (:spans line))]
    (assoc line :spans spans)))

(defn render-span-lines
  "Render a Doc to width-bounded addressable {:face :id? :spans} rows.
   Width is display cells, not UTF-16 length; absent width defaults to 80."
  ([document] (render-span-lines document 80))
  ([{:doc/keys [title blocks]} width]
   (mapv #(bound-row % width)
         (into [(row :title (span title :title))]
               (mapcat (fn [block]
                         (let [id (:id block)]
                           (cons (row :plain (span "" :plain))
                                 (map-indexed (fn [i line]
                                                (if (and id (not (:id line)))
                                                  (assoc line :id (str id ":" i)) line))
                                              (block-span-lines block width))))) blocks)))))

(defn flatten-span-lines
  "Convert rich rows back to legacy face-tagged text lines."
  [lines]
  (mapv (fn [{:keys [face spans]}] {:face face :text (apply str (map :text spans))}) lines))

;; Text vessels keep using render-lines. Their new blocks flatten the same
;; span lowering rather than acquiring a second layout implementation.
(doseq [kind [:table :tree :sparkline :gauge :dag]]
  (defmethod block-lines kind [block]
    (flatten-span-lines (block-span-lines block 80))))
