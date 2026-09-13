(ns hive-vessel.editor-wire.pending
  "Pure correlation table: id -> {:op :deadline}. Every entry leaves exactly once,
   by settle, expire or drain.")

;; SPDX-License-Identifier: MIT

(defn add
  "Pending table with ID registered for OP until DEADLINE."
  [pending id op deadline]
  (assoc pending id {:op op :deadline deadline}))

(defn settle
  "[pending' entry-or-nil] removing ID."
  [pending id]
  [(dissoc pending id) (get pending id)])

(defn expired-ids
  "Ids whose deadline is at or before NOW, ascending by deadline."
  [pending now]
  (->> pending
       (filter (fn [[_ {:keys [deadline]}]] (<= deadline now)))
       (sort-by (fn [[id {:keys [deadline]}]] [deadline id]))
       (mapv first)))

(defn expire
  "[pending' expired-ids] removing entries due at NOW."
  [pending now]
  (let [ids (expired-ids pending now)]
    [(apply dissoc pending ids) ids]))

(defn drain
  "[empty-table all-ids] removing every entry."
  [pending]
  [{} (vec (sort (keys pending)))])
