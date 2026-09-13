(ns hive-vessel.editor-wire.codec
  "Frame classification and construction for hive editor wire v1.

   Operates on JSON-parsed data (vectors, string-keyed maps). JSON text
   encoding belongs to the boundary of each runtime. Malli-free."
  (:require [hive-vessel.editor-wire.ops :as ops]))

;; SPDX-License-Identifier: MIT

(def call-fn-name "HiveOp")

(defn- int-id? [x] (and (integer? x) (not (zero? x))))

(defn- result-shape? [x] (and (map? x) (boolean? (get x "ok"))))

(defn- message-shape? [x] (and (map? x) (string? (get x "type"))))

(defn classify
  "Frame kind: :client-request :client-reply :server-call :server-reply
   :server-native or :invalid. A :server-native is a native channel command
   (call or expr) carrying a negative id whose function is not HiveOp."
  [frame]
  (if-not (vector? frame)
    :invalid
    (let [[a b c d] frame]
      (cond
        (and (= 4 (count frame)) (= "call" a) (= call-fn-name b)
             (vector? c) (= 2 (count c)) (string? (first c)) (map? (second c))
             (int-id? d) (neg? d))
        :server-call

        (and (contains? ops/replying-native-commands a)
             (int-id? (peek frame)) (neg? (peek frame))
             (or (and (= "call" a) (= 4 (count frame)) (string? b) (vector? c))
                 (and (= "expr" a) (= 3 (count frame)) (string? b))))
        :server-native

        (not= 2 (count frame)) :invalid
        (not (int-id? a)) :invalid
        (and (pos? a) (message-shape? b)) :client-request
        (and (pos? a) (result-shape? b)) :server-reply
        (and (neg? a) (result-shape? b)) :client-reply
        :else :invalid))))

(defn frame-id
  "The id of a classified frame, nil for :invalid."
  [frame]
  (case (classify frame)
    :server-call (nth frame 3)
    :server-native (peek frame)
    :invalid nil
    (first frame)))

(defn call
  "Server -> client call frame."
  [call-id op params]
  ["call" call-fn-name [op (or params {})] call-id])

(defn call-op [frame] (first (nth frame 2)))

(defn call-params [frame] (second (nth frame 2)))

(defn native?
  "True when PAYLOAD is a Vim channel command: a vector headed by a command
   name (call, expr, ex, normal, redraw), as hive-vessel's :vim-channel
   dialect emits."
  [payload]
  (and (vector? payload) (string? (first payload))
       (case (first payload)
         "call" (and (= 3 (count payload)) (string? (second payload)) (vector? (nth payload 2)))
         "expr" (and (= 2 (count payload)) (string? (second payload)))
         ("ex" "normal") (and (= 2 (count payload)) (string? (second payload)))
         "redraw" (<= 1 (count payload) 2)
         false)))

(defn replying-native?
  "True when native PAYLOAD answers once given an id."
  [payload]
  (contains? ops/replying-native-commands (first payload)))

(defn native-call
  "Server -> client native frame: PAYLOAD with CALL-ID appended, so the
   editor's reply [call-id value] correlates like any other call."
  [call-id payload]
  (conj (vec payload) call-id))

(defn native-payload
  "The payload of a :server-native frame, id stripped."
  [frame]
  (pop frame))

(defn reply
  "Reply frame answering id with result."
  [id result]
  [id result])

(defn request
  "Client -> server request frame."
  [id message]
  [id message])

(defn hello
  "Hello message. :wire defaults to the current wire version."
  [{:keys [token editor instance capabilities cwd wire]}]
  (cond-> {"type" "hello" "wire" (or wire ops/wire-version) "token" token "editor" editor}
    instance (assoc "instance" instance)
    capabilities (assoc "capabilities" (vec capabilities))
    cwd (assoc "cwd" cwd)))

(defn event
  "Event message."
  ([event-name] {"type" "event" "event" event-name})
  ([event-name data] {"type" "event" "event" event-name "data" (or data {})}))

(defn message [frame] (second frame))

(defn message-type [frame] (get (second frame) "type"))
