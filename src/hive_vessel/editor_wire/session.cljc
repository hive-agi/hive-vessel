(ns hive-vessel.editor-wire.session
  "Server-role session state machine of hive editor wire v1. Pure.

   (step state input now) -> {:state state' :effects [effect ...]}
   Inputs:  [:frame frame] [:call op params] [:tick] [:closed]
   Effects: [:send frame] [:resolve call-id result] [:close code]
            [:hello hello-msg] [:event event-msg]
   A [:call ...] step also carries :call-id."
  (:require [hive-vessel.editor-wire.codec :as codec]
            [hive-vessel.editor-wire.ops :as ops]
            [hive-vessel.editor-wire.pending :as pending]))

;; SPDX-License-Identifier: MIT

(defn init
  "Fresh session awaiting hello, authenticated by TOKEN."
  [session-id token]
  {:status :awaiting-hello
   :session-id session-id
   :token token
   :next-call 1
   :pending {}
   :hello nil})

(defn- char-code [c]
  #?(:clj (int c) :cljs (.charCodeAt c 0)))

(defn token=
  "Constant-time string equality."
  [a b]
  (and (string? a) (string? b)
       (= (count a) (count b))
       (zero? (reduce bit-or 0 (map (fn [x y] (bit-xor (char-code x) (char-code y))) a b)))))

(defn valid-hello?
  "True when MSG is a wire v1 hello carrying TOKEN."
  [msg token]
  (and (= "hello" (get msg "type"))
       (= ops/wire-version (get msg "wire"))
       (let [editor (get msg "editor")] (and (string? editor) (pos? (count editor))))
       (token= (get msg "token") token)))

(defn- emit [state effects] {:state state :effects (vec effects)})

(defn- close-all
  [state code]
  (let [[table ids] (pending/drain (:pending state))]
    (emit (assoc state :status :closed :pending table)
          (map (fn [id] [:resolve id (ops/err code)]) ids))))

(defn- on-hello-frame
  [state frame]
  (let [id (codec/frame-id frame)
        msg (codec/message frame)]
    (if (and (= :client-request (codec/classify frame)) (valid-hello? msg (:token state)))
      (emit (assoc state :status :ready :hello msg)
            [[:send (codec/reply id (ops/ok {"session" (:session-id state)
                                             "wire" ops/wire-version}))]
             [:hello msg]])
      (let [closed (close-all state "auth/denied")]
        (update closed :effects
                (fn [effects]
                  (cond-> []
                    (pos-int? id) (conj [:send (codec/reply id (ops/err "auth/denied"))])
                    true (into effects)
                    true (conj [:close "auth/denied"]))))))))

(defn- on-client-request
  [state frame]
  (let [id (codec/frame-id frame)
        msg (codec/message frame)]
    (if (and (= "event" (get msg "type")) (contains? ops/event-names (get msg "event")))
      (emit state [[:send (codec/reply id (ops/ok nil))] [:event msg]])
      (emit state [[:send (codec/reply id (ops/err "wire/invalid-frame"))]]))))

(defn- on-client-reply
  [state frame]
  (let [[table entry] (pending/settle (:pending state) (codec/frame-id frame))]
    (if entry
      (emit (assoc state :pending table)
            [[:resolve (codec/frame-id frame) (codec/message frame)]])
      (emit state []))))

(defn- native-reply?
  "True when FRAME answers a pending native command of STATE: [id value] with
   id registered under vessel/native. The value is the editor's raw reply,
   not a Result, so it is checked before any frame classification."
  [state frame]
  (and (vector? frame) (= 2 (count frame))
       (= ops/native-op (get-in state [:pending (first frame) :op]))))

(defn- on-native-reply
  [state frame]
  (let [[id value] frame
        [table _] (pending/settle (:pending state) id)]
    (emit (assoc state :pending table) [[:resolve id (ops/ok value)]])))

(defn- on-ready-frame
  [state frame]
  (if (native-reply? state frame)
    (on-native-reply state frame)
    (case (codec/classify frame)
      :client-request (on-client-request state frame)
      :client-reply (on-client-reply state frame)
      (emit state []))))

(defn- on-frame
  [state frame]
  (case (:status state)
    :awaiting-hello (on-hello-frame state frame)
    :ready (on-ready-frame state frame)
    :closed (emit state [])))

(defn- on-call
  [state op params now]
  (let [call-id (- (:next-call state))]
    (cond
      (not= :ready (:status state))
      (assoc (emit state [[:resolve call-id (ops/err "wire/not-connected")]])
             :call-id call-id)

      (not (ops/op? op))
      (assoc (emit (update state :next-call inc)
                   [[:resolve call-id (ops/err "wire/unknown-op" (str op))]])
             :call-id call-id)

      :else
      (assoc (emit (-> state
                       (update :next-call inc)
                       (update :pending pending/add call-id op
                               (+ now (ops/timeout-ms op params))))
                   [[:send (codec/call call-id op params)]])
             :call-id call-id))))

(defn- on-native
  "Send native channel command PAYLOAD on behalf of hive-vessel. A replying
   command (call, expr) goes out with a fresh call id and waits TIMEOUT ms
   under the pending table; ex, normal and redraw never reply, so they are
   sent as-is and resolved at once with ok nil."
  [state payload timeout now]
  (let [call-id (- (:next-call state))]
    (cond
      (not= :ready (:status state))
      (assoc (emit state [[:resolve call-id (ops/err "wire/not-connected")]])
             :call-id call-id)

      (not (codec/native? payload))
      (assoc (emit (update state :next-call inc)
                   [[:resolve call-id (ops/err "wire/invalid-frame" (pr-str payload))]])
             :call-id call-id)

      (codec/replying-native? payload)
      (assoc (emit (-> state
                       (update :next-call inc)
                       (update :pending pending/add call-id ops/native-op (+ now timeout)))
                   [[:send (codec/native-call call-id payload)]])
             :call-id call-id)

      :else
      (assoc (emit (update state :next-call inc)
                   [[:send payload] [:resolve call-id (ops/ok nil)]])
             :call-id call-id))))

(defn- on-tick
  [state now]
  (let [[table ids] (pending/expire (:pending state) now)]
    (emit (assoc state :pending table)
          (map (fn [id] [:resolve id (ops/err "wire/timeout")]) ids))))

(defn step
  "Advance STATE by INPUT at time NOW (ms)."
  [state input now]
  (case (first input)
    :frame (on-frame state (second input))
    :call (on-call state (nth input 1) (nth input 2 {}) now)
    :native (on-native state (nth input 1) (nth input 2 ops/default-timeout-ms) now)
    :tick (on-tick state now)
    :closed (if (= :closed (:status state))
              (emit state [])
              (close-all state "wire/closed"))))
