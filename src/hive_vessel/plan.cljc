(ns hive-vessel.plan
  "Compile ops for a target: rewrite through translators until only
   `:vessel/native` ops in the target's dialect remain.

   Pure. An addon intent lowers to standard primitives (or dialect calls), a
   vessel's dialect lowers those to natives; the same fold handles every
   level. When a translator throws, returns junk, or produces something that
   cannot itself be lowered, the next candidate is tried, so a specific
   translator degrades to the generic one instead of failing the op.

   This fall-through covers TRANSLATION only. Executing a compiled plan is
   hive-vessel.dispatch/dispatch!, where the first executor throw stops the
   batch and is reported loudly as :execute-threw."
  (:require [hive-vessel.rule :as rule]
            [hive-vessel.schema :as s]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def max-depth
  "Rewrite depth beyond which an op is refused."
  16)

(defn- fail
  ([reason op] {:error {:failure/reason reason :failure/op op}})
  ([reason op detail] {:error {:failure/reason reason :failure/op op :failure/detail detail}}))

(defn- ok [ops trace] {:ok {:plan/ops ops :plan/trace trace}})

(defn- as-ops
  "Normalize a translator's output to a vector of ops, or nil when it is not
   an op or a sequence of ops."
  [out]
  (cond
    (and (map? out) (m/validate s/Op out)) [out]
    (and (sequential? out) (every? #(and (map? %) (m/validate s/Op %)) out)) (vec out)
    :else nil))

(declare compile-ops*)

(defn- attempt
  [registry target op depth stack t]
  (let [id (:translator/id t)
        accepts (:translator/accepts t)]
    (if (and accepts (not (m/validate accepts op)))
      (fail :invalid-op op {:translator/id id :explain (m/explain accepts op)})
      (let [out (try {:value ((:translator/translate t) op target)}
                     (catch #?(:clj Throwable :cljs :default) e
                       {:thrown (or (ex-message e) (str e))}))]
        (if (contains? out :thrown)
          (fail :translator-threw op {:translator/id id :message (:thrown out)})
          (if-let [ops (as-ops (:value out))]
            (let [r (compile-ops* registry target ops (inc depth) (conj stack [(:op op) id]))]
              (if-let [p (:ok r)]
                (ok (:plan/ops p)
                    (into [{:op (:op op) :translator/id id :depth depth}] (:plan/trace p)))
                r))
            (fail :invalid-output op {:translator/id id})))))))

(defn- compile-op*
  [registry target op depth stack]
  (let [op-type (:op op)
        primitive (get s/primitive-schemas op-type)]
    (cond
      (not (and (map? op) (m/validate s/Op op)))
      (fail :invalid-op op)

      (> depth max-depth)
      (fail :depth-exceeded op {:max-depth max-depth})

      (= :vessel/native op-type)
      (cond
        (not (m/validate s/Native op)) (fail :invalid-op op (m/explain s/Native op))
        (= (:native/dialect op) (:vessel/dialect target)) (ok [op] [])
        :else (fail :dialect-mismatch op {:expected (:vessel/dialect target)
                                          :actual (:native/dialect op)}))

      (and primitive (not (m/validate primitive op)))
      (fail :invalid-op op (m/explain primitive op))

      :else
      (let [cands (rule/candidates registry op-type target)]
        (if (empty? cands)
          (fail :unsupported op {:vessel/id (:vessel/id target)
                                 :vessel/dialect (:vessel/dialect target)})
          (loop [[t & more] cands
                 attempts []]
            (if (nil? t)
              {:error {:failure/reason :unsupported
                       :failure/op op
                       :failure/attempts attempts}}
              (let [r (if (some #{[op-type (:translator/id t)]} stack)
                        (fail :cycle op {:translator/id (:translator/id t)})
                        (attempt registry target op depth stack t))]
                (if (:ok r)
                  r
                  (recur more (conj attempts (:error r))))))))))))

(defn- compile-ops*
  [registry target ops depth stack]
  (reduce (fn [acc op]
            (let [r (compile-op* registry target op depth stack)]
              (if-let [p (:ok r)]
                (-> acc
                    (update-in [:ok :plan/ops] into (:plan/ops p))
                    (update-in [:ok :plan/trace] into (:plan/trace p)))
                (reduced r))))
          (ok [] [])
          ops))

(defn plan
  "Compile OP-OR-OPS (one op or a sequence) for TARGET against REGISTRY.
   Returns {:ok {:plan/ops [native ...] :plan/trace [...]}} or
   {:error failure}."
  [registry target op-or-ops]
  (cond
    (not (m/validate s/Target target))
    (fail :invalid-op nil {:target (m/explain s/Target target)})

    (map? op-or-ops)
    (compile-ops* registry target [op-or-ops] 0 [])

    (sequential? op-or-ops)
    (compile-ops* registry target (vec op-or-ops) 0 [])

    :else (fail :invalid-op op-or-ops)))

(m/=> plan [:=> [:cat s/Registry :map :any] s/Compiled])

(defn supports?
  "True iff OP can be compiled for TARGET."
  [registry target op]
  (contains? (plan registry target op) :ok))
