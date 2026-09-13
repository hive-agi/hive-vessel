(ns hive-vessel.dispatch
  "Boundary: compile ops and hand each native op to the target's injected
   executor, `(:vessel/execute! target)` -- emacsclient, a Vim channel, a
   VS Code JSON-lines pipe, a web socket. The executor is the only effect."
  (:require [hive-vessel.plan :as plan]))

;; SPDX-License-Identifier: MIT

(defn- registry-value [registry]
  (if (instance? #?(:clj clojure.lang.IDeref :cljs IDeref) registry) @registry registry))

(defn dispatch!
  "Compile OP-OR-OPS for TARGET and execute every native op in order.
   Returns {:ok plan-with-:plan/results} or {:error failure}; an executor that
   throws stops the run and reports how many ops completed. REGISTRY may be a
   value or an atom holding one."
  [registry target op-or-ops]
  (let [compiled (plan/plan (registry-value registry) target op-or-ops)]
    (if-let [p (:ok compiled)]
      (if-let [execute! (:vessel/execute! target)]
        (loop [[op & more] (:plan/ops p)
               results []]
          (if (nil? op)
            {:ok (assoc p :plan/results results)}
            (let [r (try {:value (execute! op)}
                         (catch #?(:clj Throwable :cljs :default) e
                           {:thrown (or (ex-message e) (str e))}))]
              (if (contains? r :thrown)
                {:error {:failure/reason :execute-threw
                         :failure/op op
                         :failure/detail {:message (:thrown r)
                                          :completed (count results)
                                          :results results}}}
                (recur more (conj results (:value r)))))))
        {:error {:failure/reason :no-executor
                 :failure/detail {:vessel/id (:vessel/id target)}}})
      compiled)))

(defn sink
  "A one-argument fn dispatching each op it receives to TARGET. Suits any
   consumer that takes a delivery fn (e.g. an addon presenter)."
  ([registry target] (sink registry target identity))
  ([registry target ->op]
   (fn [x] (dispatch! registry target (->op x)))))

(defn broadcast!
  "Dispatch OP-OR-OPS to every target in TARGETS. Returns
   {vessel-id result}; one vessel's failure does not stop the others."
  [registry targets op-or-ops]
  (into {} (map (fn [t] [(:vessel/id t) (dispatch! registry t op-or-ops)])) targets))
