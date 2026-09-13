(ns hive-vessel.rule-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-vessel.rule :as rule]
            [hive-vessel.schema :as s]
            [malli.generator :as mg]))

;; SPDX-License-Identifier: MIT

(defspec a-target-matches-its-own-guard 100
  (prop/for-all [t (mg/generator s/Target)]
    (rule/matches? (select-keys t [:vessel/id :vessel/dialect :vessel/features]) t)))

(defspec the-empty-guard-matches-everything 100
  (prop/for-all [t (mg/generator s/Target)]
    (rule/matches? {} t)))

(defspec a-feature-the-target-lacks-never-matches 100
  (prop/for-all [t (mg/generator s/Target)]
    (not (rule/matches? {:vessel/features #{::absent-feature}} t))))

(defn- tr [id op & {:as more}]
  (merge {:translator/id id :translator/op op :translator/translate (fn [_ _] nil)} more))

(def emacs {:vessel/id :emacs :vessel/dialect :elisp :vessel/features #{:x/flow}})

(deftest candidate-order-is-priority-then-specificity-then-registration
  (let [reg (rule/registry-of
             [(tr :t/generic :a/op)
              (tr :t/dialect :a/op :translator/when {:vessel/dialect :elisp})
              (tr :t/vessel :a/op :translator/when {:vessel/id :emacs})
              (tr :t/feature :a/op :translator/when {:vessel/dialect :elisp :vessel/features #{:x/flow}})
              (tr :t/generic-2 :a/op)
              (tr :t/urgent :a/op :translator/priority 5)
              (tr :t/other-vessel :a/op :translator/when {:vessel/id :vim})
              (tr :t/other-op :b/op)])]
    (is (= [:t/urgent :t/vessel :t/feature :t/dialect :t/generic :t/generic-2]
           (mapv :translator/id (rule/candidates reg :a/op emacs))))))

(deftest register-replaces-by-id-in-place
  (let [reg (-> (rule/empty-registry)
                (rule/register (tr :t/one :a/op))
                (rule/register (tr :t/two :a/op))
                (rule/register (tr :t/one :c/op)))]
    (is (= [[:t/one :c/op] [:t/two :a/op]]
           (mapv (juxt :translator/id :translator/op) (:registry/translators reg))))))

(deftest register-refuses-an-invalid-translator
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo)
               (rule/register (rule/empty-registry) {:translator/id :t/x :translator/op :a/op}))))

(deftest unregister-removes-by-id
  (is (empty? (:registry/translators
               (rule/unregister (rule/registry-of [(tr :t/x :a/op)]) :t/x)))))

(deftest op-types-lists-what-a-target-can-rewrite
  (is (= #{:a/op :b/op}
         (rule/op-types (rule/registry-of [(tr :t/a :a/op)
                                           (tr :t/b :b/op :translator/when {:vessel/id :emacs})
                                           (tr :t/c :c/op :translator/when {:vessel/id :vim})])
                        emacs))))
