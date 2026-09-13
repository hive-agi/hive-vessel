(ns hive-vessel.dispatch-test
  "The boundary against injected executors: a recording decorator, a failing
   one, and none."
  (:require [clojure.test :refer [deftest is]]
            [hive-vessel.core :as v]))

;; SPDX-License-Identifier: MIT

(defn- recording-target [base]
  (let [log (atom [])]
    [(assoc base :vessel/execute! (fn [op] (swap! log conj op) (count @log))) log]))

(def notify {:op :ui/notify :message "hi"})

(deftest dispatch-executes-every-native-op-in-order
  (let [[target log] (recording-target (:tmux v/reference-targets))
        r (v/dispatch! (v/standard-registry) target [notify {:op :ui/open-file :file "a" :line 2}])]
    (is (= [1 2] (get-in r [:ok :plan/results])))
    (is (= [["[info] hi"] ["open a:2"]] (mapv (comp :text/lines :native/payload) @log)))))

(deftest a-compile-failure-executes-nothing
  (let [[target log] (recording-target (:tmux v/reference-targets))
        r (v/dispatch! (v/standard-registry) target [notify {:op :demo/unknown}])]
    (is (= :unsupported (get-in r [:error :failure/reason])))
    (is (empty? @log))))

(deftest a-throwing-executor-reports-progress
  (let [n (atom 0)
        target (assoc (:tmux v/reference-targets)
                      :vessel/execute! (fn [_] (when (= 2 (swap! n inc)) (throw (ex-info "down" {}))) :ok))
        err (:error (v/dispatch! (v/standard-registry) target [notify notify notify]))]
    (is (= :execute-threw (:failure/reason err)))
    (is (= 1 (get-in err [:failure/detail :completed])))))

(deftest no-executor-is-a-failure-not-a-silent-success
  (is (= :no-executor
         (get-in (v/dispatch! (v/standard-registry) (:vim v/reference-targets) notify)
                 [:error :failure/reason]))))

(deftest sink-reads-a-live-registry
  (let [reg (atom (v/standard-registry))
        [target log] (recording-target (:tmux v/reference-targets))
        deliver! (v/sink reg target (fn [n] {:op :demo/tick :n n}))]
    (is (= :unsupported (get-in (deliver! 1) [:error :failure/reason])))
    (swap! reg v/register {:translator/id :demo/tick :translator/op :demo/tick
                           :translator/translate (fn [{:keys [n]} _] {:op :ui/notify :message (str "tick " n)})})
    (is (:ok (deliver! 2)))
    (is (= [["[info] tick 2"]] (mapv (comp :text/lines :native/payload) @log)))))

(deftest broadcast-isolates-vessels
  (let [[tmux log] (recording-target (:tmux v/reference-targets))
        r (v/broadcast! (v/standard-registry) [tmux (:vim v/reference-targets)] notify)]
    (is (:ok (:tmux r)))
    (is (= :no-executor (get-in r [:vim :error :failure/reason])))
    (is (= 1 (count @log)))))

(deftest envelopes-normalize-to-ops
  (is (= notify (v/envelope->op notify)))
  (is (= {:op :demo/frame :payload {:n 1}}
         (v/envelope->op {:type :demo/frame :event-name "x" :payload {:n 1}})))
  (is (= {:type "unqualified"} (v/envelope->op {:type "unqualified"}))))

(deftest hooks-contribute-translators
  (let [hooks {v/hook-key (fn [] [{:translator/id :demo/x :translator/op :demo/x
                                   :translator/translate (fn [_ _] notify)}])}
        reg (v/registry-from-hooks [hooks {} {v/hook-key :not-a-collection}])]
    (is (v/supports? reg (:web v/reference-targets) {:op :demo/x}))))
