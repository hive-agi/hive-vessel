(ns hive-vessel.test-util
  "Oracles shared by the dialect and parity tests."
  (:require [clojure.string :as str]
            [hive-vessel.core :as v]))

;; SPDX-License-Identifier: MIT

(defn ordered-in?
  "True iff every literal in LITERALS occurs in CODE, each after the previous."
  [code literals]
  (loop [from 0 [l & more] literals]
    (if (nil? l)
      true
      (let [i (str/index-of code l from)]
        (and i (recur (+ i (count l)) more))))))

(defn lowered-payload
  "The single native payload OP lowers to on the reference target VESSEL-ID,
   through the standard registry and the real plan."
  [vessel-id op]
  (get-in (v/plan (v/standard-registry) (get v/reference-targets vessel-id) op)
          [:ok :plan/ops 0 :native/payload]))