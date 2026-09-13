(ns hive-vessel.editor-wire.session-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-vessel.editor-wire.codec :as codec]
            [hive-vessel.editor-wire.ops :as ops]
            [hive-vessel.editor-wire.pending :as pending]
            [hive-vessel.editor-wire.schema :as s]
            [hive-vessel.editor-wire.session :as ses]))

;; SPDX-License-Identifier: MIT

(def token "0123456789abcdef")

(defn- hello-frame
  ([] (hello-frame token))
  ([tok] [1 (codec/hello {:token tok :editor "vim"})]))

(defn- ready []
  (:state (ses/step (ses/init "S" token) [:frame (hello-frame)] 0)))

(defn- effects-of [kind step]
  (filter #(= kind (first %)) (:effects step)))

(deftest handshake
  (testing "a valid hello readies the session and answers with the session id"
    (let [r (ses/step (ses/init "S" token) [:frame (hello-frame)] 0)]
      (is (= :ready (get-in r [:state :status])))
      (is (= [[:send [1 (ops/ok {"session" "S" "wire" 1})]]
              [:hello (codec/hello {:token token :editor "vim"})]]
             (:effects r)))))
  (testing "a wrong token is denied and closes"
    (let [r (ses/step (ses/init "S" token) [:frame (hello-frame "0123456789abcdeX")] 0)]
      (is (= :closed (get-in r [:state :status])))
      (is (= [[:send [1 (ops/err "auth/denied")]] [:close "auth/denied"]] (:effects r)))))
  (testing "a request before hello is denied by id, then closes"
    (let [r (ses/step (ses/init "S" token) [:frame [2 (codec/event "focus")]] 0)]
      (is (= :closed (get-in r [:state :status])))
      (is (= [[:send [2 (ops/err "auth/denied")]] [:close "auth/denied"]] (:effects r)))))
  (testing "a non-request frame before hello closes without a reply"
    (doseq [frame [[-1 (ops/ok 1)] ["junk"]]]
      (let [r (ses/step (ses/init "S" token) [:frame frame] 0)]
        (is (= :closed (get-in r [:state :status])) (pr-str frame))
        (is (= [[:close "auth/denied"]] (:effects r)) (pr-str frame)))))
  (testing "a call before hello resolves not-connected"
    (let [r (ses/step (ses/init "S" token) [:call "editor-status" {}] 0)]
      (is (= [[:resolve -1 (ops/err "wire/not-connected")]] (:effects r))))))

(deftest token-equality
  (is (ses/token= "abc" "abc"))
  (is (not (ses/token= "abc" "abd")))
  (is (not (ses/token= "abc" "abcd")))
  (is (not (ses/token= nil "abc"))))

(deftest events-are-acknowledged-and-emitted
  (let [r (ses/step (ready) [:frame [5 (codec/event "focus")]] 0)]
    (is (= [[:send [5 (ops/ok nil)]] [:event (codec/event "focus")]] (:effects r))))
  (let [r (ses/step (ready) [:frame [6 {"type" "event" "event" "nope"}]] 0)]
    (is (= [[:send [6 (ops/err "wire/invalid-frame")]]] (:effects r)))))

(deftest unknown-ops-never-reach-the-wire
  (let [r (ses/step (ready) [:call "rm-rf" {}] 0)]
    (is (empty? (effects-of :send r)))
    (is (= "wire/unknown-op" (ops/error-code (nth (first (:effects r)) 2))))))

(deftest late-replies-are-dropped
  (let [r1 (ses/step (ready) [:call "editor-status" {}] 0)
        r2 (ses/step (:state r1) [:tick] 10000)
        r3 (ses/step (:state r2) [:frame [(:call-id r1) (ops/ok "late")]] 10001)]
    (is (= [[:resolve (:call-id r1) (ops/err "wire/timeout")]] (:effects r2)))
    (is (= [] (:effects r3)))))

(deftest pending-table
  (let [t (-> {} (pending/add -1 "a" 10) (pending/add -2 "b" 5) (pending/add -3 "c" 50))]
    (is (= [-2 -1] (pending/expired-ids t 10)))
    (is (= [{-3 {:op "c" :deadline 50}} [-2 -1]] (pending/expire t 10)))
    (is (= [(dissoc t -1) {:op "a" :deadline 10}] (pending/settle t -1)))
    (is (= [{} [-3 -2 -1]] (pending/drain t)))))

(def gen-input
  (gen/frequency
   [[5 (gen/let [op (gen/elements (sort ops/all-ops))]
         [:call op {}])]
    [4 (gen/let [pick gen/nat ok gen/boolean]
         [:reply pick ok])]
    [2 (gen/let [dt (gen/choose 0 8000)] [:tick dt])]
    [1 (gen/return [:event])]
    [1 (gen/return [:junk])]]))

(defn- run-script
  "Drive a ready session through SCRIPT, then close it. Returns every step."
  [script]
  (loop [state (ready) now 0 calls [] steps [] [input & more] script]
    (if (nil? input)
      (conj steps (ses/step state [:closed] now))
      (let [[kind a b] input
            [now step] (case kind
                         :call [now (ses/step state [:call a b] now)]
                         :reply [now (let [ids (vec (keys (:pending state)))]
                                       (if (seq ids)
                                         (ses/step state [:frame [(nth ids (mod a (count ids)))
                                                                  (if b (ops/ok 1) (ops/err "op/failed"))]] now)
                                         (ses/step state [:frame [(- -1000 a) (ops/ok 1)]] now)))]
                         :tick [(+ now a) (ses/step state [:tick] (+ now a))]
                         :event [now (ses/step state [:frame [9 (codec/event "focus")]] now)]
                         :junk [now (ses/step state [:frame ["junk"]] now)])]
        (recur (:state step) now (cond-> calls (:call-id step) (conj (:call-id step)))
               (conj steps step) more)))))

(defspec every-call-resolves-exactly-once 300
  (prop/for-all [script (gen/vector gen-input 0 40)]
    (let [steps (run-script script)
          call-ids (keep :call-id steps)
          resolved (for [st steps [k id] (:effects st) :when (= k :resolve)] id)]
      (and (= (count call-ids) (count (set call-ids)))
           (= (sort call-ids) (sort resolved))
           (empty? (get-in (last steps) [:state :pending]))))))

(defspec every-step-conforms-to-the-step-schema 200
  (prop/for-all [script (gen/vector gen-input 0 30)]
    (every? #(s/validate s/Step %) (run-script script))))
