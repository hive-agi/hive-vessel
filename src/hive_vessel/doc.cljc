(ns hive-vessel.doc
  "Vessel-neutral documents: block constructors and the single line renderer.

   An addon describes WHAT to show as a Doc; `render-lines` projects it to
   face-tagged lines once, and every character-cell dialect (Emacs buffer, Vim
   buffer, terminal) paints those lines. Rich dialects (:json) receive the Doc
   itself alongside the lines."
  (:require [clojure.string :as str]
            [hive-vessel.schema :as s]
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
