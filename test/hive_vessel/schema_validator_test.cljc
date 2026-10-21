(ns hive-vessel.schema-validator-test
  "The precompiled validators agree with malli on the schema forms they
   replace, and a schema is compiled once."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-vessel.schema :as s]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def samples
  [{:op :ui/notify :message "hi"}
   {:op :vessel/native :native/dialect :vim-channel :native/payload ["call" "f" []]}
   {:op "not-qualified"}
   {}
   {:vessel/id :vim :vessel/dialect :vim-channel}
   {:vessel/id :vim}])

(deftest precompiled-validators-agree-with-malli
  (doseq [v samples]
    (testing (pr-str v)
      (is (= (m/validate s/Op v) (s/valid-op? v)))
      (is (= (m/validate s/Native v) (s/valid-native? v)))
      (is (= (m/validate s/Target v) (s/valid-target? v)))
      (is (= (m/validate s/Op v) (s/valid? s/Op v))))))

(deftest a-schema-is-compiled-once
  (is (identical? (s/validator s/Op) (s/validator s/Op)))
  (let [form [:map [:x :int]]]
    (is (identical? (s/validator form) (s/validator [:map [:x :int]]))
        "an equal schema form reuses the compiled validator")
    (is ((s/validator form) {:x 1}))
    (is (not ((s/validator form) {:x "1"})))))
