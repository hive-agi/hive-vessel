(ns hive-vessel.plan-test
  "The compile fold: the standard vocabulary reaches every reference vessel,
   and each extension seam (addon intent, dialect call, new dialect, override)
   works by registration alone."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-vessel.core :as v]
            [hive-vessel.plan :as plan]
            [hive-vessel.rule :as rule]
            [hive-vessel.schema :as s]
            [malli.core :as m]
            [malli.generator :as mg]))

;; SPDX-License-Identifier: MIT

(def standard (v/standard-registry))

(defspec every-primitive-lowers-for-every-reference-vessel 150
  (prop/for-all [op (mg/generator s/Primitive)
                 target (gen/elements (vals v/reference-targets))]
    (let [{:keys [ok error]} (plan/plan standard target op)]
      (and (nil? error)
           (m/validate s/Plan ok)
           (seq (:plan/ops ok))
           (every? #(= (:vessel/dialect target) (:native/dialect %)) (:plan/ops ok))
           (= [(:op op)] (mapv :op (:plan/trace ok)))))))

(defspec batches-concatenate-in-order 50
  (prop/for-all [ops (gen/vector (mg/generator s/Primitive) 0 5)]
    (let [target (:tmux v/reference-targets)
          one-by-one (mapcat #(get-in (plan/plan standard target %) [:ok :plan/ops]) ops)]
      (= (vec one-by-one) (get-in (plan/plan standard target ops) [:ok :plan/ops])))))

(def emacs (:emacs v/reference-targets))
(def vim (:vim v/reference-targets))

(defn- reason [r] (get-in r [:error :failure/reason]))

(deftest an-addon-intent-lowers-through-primitives-on-every-vessel
  (let [reg (v/standard-registry
             [{:translator/id :demo/show
               :translator/op :demo/frame
               :translator/translate (fn [{:keys [n]} _]
                                       {:op :ui/notify :message (str "frame " n)})}])]
    (doseq [t (vals v/reference-targets)]
      (let [r (plan/plan reg t {:op :demo/frame :n 7})]
        (is (:ok r) (str (:vessel/id t)))
        (is (= [:demo/frame :ui/notify] (mapv :op (get-in r [:ok :plan/trace]))))))))

(deftest a-feature-guarded-translator-wins-only-where-advertised
  (let [reg (v/standard-registry
             [{:translator/id :demo/generic
               :translator/op :demo/frame
               :translator/translate (fn [_ _] {:op :ui/notify :message "generic"})}
              {:translator/id :demo/native-emacs
               :translator/op :demo/frame
               :translator/when {:vessel/dialect :elisp :vessel/features #{:demo/el}}
               :translator/translate (fn [_ _] {:op :elisp/call :fn "demo-show" :args [{:a 1}]})}])]
    (is (= "(demo-show '((a . 1)))"
           (get-in (plan/plan reg (assoc emacs :vessel/features #{:demo/el}) {:op :demo/frame})
                   [:ok :plan/ops 0 :native/payload])))
    (is (= [:demo/generic :hive-vessel.elisp/notify]
           (mapv :translator/id (get-in (plan/plan reg emacs {:op :demo/frame}) [:ok :plan/trace]))))))

(deftest a-new-dialect-is-one-registration
  (let [target {:vessel/id :kakoune :vessel/dialect :kak}
        reg (v/standard-registry
             [{:translator/id :kak/notify
               :translator/op :ui/notify
               :translator/when {:vessel/dialect :kak}
               :translator/translate (fn [{:keys [message]} _]
                                       {:op :vessel/native :native/dialect :kak
                                        :native/payload (str "echo " message)})}])]
    (is (= "echo hi" (get-in (plan/plan reg target {:op :ui/notify :message "hi"})
                             [:ok :plan/ops 0 :native/payload])))
    (is (= :unsupported (reason (plan/plan reg target {:op :ui/close-panel :panel/id "p"}))))))

(deftest a-vessel-can-override-a-standard-lowering
  (let [reg (v/standard-registry
             [{:translator/id :my-emacs/notify
               :translator/op :ui/notify
               :translator/when {:vessel/id :emacs}
               :translator/translate (fn [_ _] {:op :vessel/native :native/dialect :elisp
                                                :native/payload "(alert)"})}])]
    (is (= "(alert)" (get-in (plan/plan reg emacs {:op :ui/notify :message "x"})
                             [:ok :plan/ops 0 :native/payload])))))

(deftest failing-translators-degrade-to-the-next-candidate
  (let [base {:translator/op :demo/frame}
        generic (assoc base :translator/id :demo/generic
                       :translator/translate (fn [_ _] {:op :ui/notify :message "fallback"}))]
    (doseq [[label bad] {:throws (fn [_ _] (throw (ex-info "boom" {})))
                         :junk (fn [_ _] 42)
                         :unlowerable (fn [_ _] {:op :nowhere/op})
                         :wrong-dialect (fn [_ _] {:op :vessel/native :native/dialect :json
                                                   :native/payload {}})}]
      (let [reg (v/standard-registry
                 [generic (assoc base :translator/id :demo/bad :translator/priority 1
                                 :translator/translate bad)])
            r (plan/plan reg emacs {:op :demo/frame})]
        (is (= [:demo/generic :hive-vessel.elisp/notify]
               (mapv :translator/id (get-in r [:ok :plan/trace])))
            (name label))))))

(deftest exhausted-candidates-report-every-attempt
  (let [reg (v/standard-registry
             [{:translator/id :demo/a :translator/op :demo/frame
               :translator/translate (fn [_ _] (throw (ex-info "a" {})))}
              {:translator/id :demo/b :translator/op :demo/frame
               :translator/translate (fn [_ _] :nope)}])
        err (:error (plan/plan reg emacs {:op :demo/frame}))]
    (is (= :unsupported (:failure/reason err)))
    (is (= [:translator-threw :invalid-output] (mapv :failure/reason (:failure/attempts err))))))

(deftest cycles-and-runaway-depth-are-refused
  (testing "a translator that rewrites an op into itself"
    (let [reg (v/standard-registry
               [{:translator/id :demo/loop :translator/op :demo/frame
                 :translator/translate (fn [op _] op)}])]
      (is (= [:cycle] (->> (plan/plan reg emacs {:op :demo/frame})
                           :error :failure/attempts
                           (mapcat :failure/attempts)
                           (map :failure/reason)
                           distinct)))))
  (testing "an unbounded chain of distinct ops"
    (let [reg (v/standard-registry
               [{:translator/id :demo/deeper :translator/op :demo/frame
                 :translator/translate (fn [op _] (update op :n (fnil inc 0)))}])
          r (plan/plan reg emacs {:op :demo/frame})]
      (is (= :unsupported (reason r))))))

(deftest accepts-guards-the-translator-input
  (let [reg (v/standard-registry
             [{:translator/id :demo/strict :translator/op :demo/frame
               :translator/accepts [:map [:n :int]]
               :translator/translate (fn [{:keys [n]} _] {:op :ui/notify :message (str n)})}])]
    (is (:ok (plan/plan reg emacs {:op :demo/frame :n 1})))
    (is (= [:invalid-op] (mapv :failure/reason
                               (get-in (plan/plan reg emacs {:op :demo/frame :n "x"})
                                       [:error :failure/attempts]))))))

(deftest malformed-input-is-refused-up-front
  (is (= :invalid-op (reason (plan/plan standard emacs {:op :ui/notify}))))
  (is (= :invalid-op (reason (plan/plan standard emacs {:no-op true}))))
  (is (= :invalid-op (reason (plan/plan standard {:vessel/id :emacs} {:op :ui/notify :message "x"}))))
  (is (= :dialect-mismatch
         (reason (plan/plan standard vim {:op :vessel/native :native/dialect :elisp :native/payload "x"})))))

(deftest dialect-calls-quote-their-arguments
  (is (= "(my-fn '((frame/phase . \"apply\") (paths . (\"a b\"))) '42)"
         (get-in (plan/plan standard emacs {:op :elisp/call :fn "my-fn"
                                            :args [{:frame/phase :apply :paths ["a b"]} 42]})
                 [:ok :plan/ops 0 :native/payload])))
  (is (= ["call" "Mine#show" [{"k" "v"}]]
         (get-in (plan/plan standard vim {:op :vim/call :fn "Mine#show" :args [{:k :v}]})
                 [:ok :plan/ops 0 :native/payload]))))
