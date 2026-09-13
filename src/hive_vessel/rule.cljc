(ns hive-vessel.rule
  "Translator registry and selection.

   The registry is an immutable value; a host that wants a live one wraps it
   in an atom. Selection is an ordered rule chain: among translators for an op
   whose guard matches the target, higher `:translator/priority` wins, then
   the more specific guard, then earlier registration. A new vessel, dialect
   or addon behaviour is a new translator -- nothing here changes."
  (:require [clojure.set :as set]
            [hive-vessel.schema :as s]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(defn empty-registry [] {:registry/translators []})

(defn register
  "REGISTRY with TRANSLATOR added. A translator with the same
   :translator/id is replaced in place, so re-registering on reload is
   idempotent. Throws on an invalid translator."
  [registry translator]
  (when-not (m/validate s/Translator translator)
    (throw (ex-info "invalid translator"
                    {:translator/id (:translator/id translator)
                     :explain (m/explain s/Translator translator)})))
  (let [id (:translator/id translator)
        ts (:registry/translators registry)
        i  (first (keep-indexed (fn [i t] (when (= id (:translator/id t)) i)) ts))]
    (assoc registry :registry/translators
           (if i (assoc ts i translator) (conj ts translator)))))

(defn register-all
  [registry translators]
  (reduce register registry translators))

(defn unregister
  [registry translator-id]
  (update registry :registry/translators
          (fn [ts] (into [] (remove #(= translator-id (:translator/id %))) ts))))

(defn registry-of
  "A registry holding every translator in TRANSLATOR-COLLS, in order."
  [& translator-colls]
  (reduce register-all (empty-registry) translator-colls))

(defn matches?
  "True iff guard WHEN admits TARGET."
  [when target]
  (and (or (not (contains? when :vessel/id))
           (= (:vessel/id when) (:vessel/id target)))
       (or (not (contains? when :vessel/dialect))
           (= (:vessel/dialect when) (:vessel/dialect target)))
       (set/subset? (or (:vessel/features when) #{})
                    (or (:vessel/features target) #{}))))

(m/=> matches? [:=> [:cat s/When s/Target] :boolean])

(defn specificity
  "How narrowly guard WHEN picks targets: a vessel id outranks a dialect,
   which outranks any number of features."
  [when]
  (+ (if (contains? when :vessel/id) 10000 0)
     (if (contains? when :vessel/dialect) 1000 0)
     (count (:vessel/features when))))

(m/=> specificity [:=> [:cat s/When] [:int {:min 0}]])

(defn candidates
  "Translators for OP-TYPE that apply to TARGET, best first."
  [registry op-type target]
  (->> (:registry/translators registry)
       (map-indexed vector)
       (filter (fn [[_ t]] (and (= op-type (:translator/op t))
                                (matches? (or (:translator/when t) {}) target))))
       (sort-by (fn [[i t]] [(- (or (:translator/priority t) 0))
                             (- (specificity (or (:translator/when t) {})))
                             i]))
       (mapv second)))

(defn op-types
  "Every op type REGISTRY can rewrite for TARGET."
  [registry target]
  (into (sorted-set)
        (comp (filter #(matches? (or (:translator/when %) {}) target))
              (map :translator/op))
        (:registry/translators registry)))
