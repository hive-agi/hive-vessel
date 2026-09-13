(ns hive-vessel.dispatch
  "Boundary: compile ops and hand each native op to the target's injected
   executor, `(:vessel/execute! target)` -- emacsclient, a Vim channel, a
   VS Code JSON-lines pipe, a web socket. The executor is the only effect."
  (:require [hive-vessel.plan :as plan]))

;; SPDX-License-Identifier: MIT

(defn- registry-value
  "REGISTRY itself, or its value when it is something dereferenceable (an
   atom holding a live registry). In ClojureScript IDeref is a protocol, so
   the test is satisfies?, not instance?."
  [registry]
  (if #?(:clj (instance? clojure.lang.IDeref registry)
         :cljs (satisfies? IDeref registry))
    @registry
    registry))

(defn dispatch!
  "Compile OP-OR-OPS for TARGET and execute every native op in order.
   Returns {:ok plan-with-:plan/results} or {:error failure}. REGISTRY may be
   a value or an atom holding one.

   Failure semantics, which differ by phase:
   - TRANSLATION failures fall through to the next candidate translator, so a
     vessel-specific translator degrades to the generic one.
   - EXECUTION failures do NOT. The first executor throw stops the batch and
     returns {:error {:failure/reason :execute-threw ... :failure/detail
     {:completed n}}}: no re-route to another vessel, no silent drop.
   - A batch is not a transaction. Order ops so a partial run is safe and
     treat :completed as the resume point."
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
