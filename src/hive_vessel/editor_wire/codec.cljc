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
  "Frame kind: :client-request :client-reply :server-call :server-reply or :invalid."
  [frame]
  (if-not (vector? frame)
    :invalid
    (let [[a b c d] frame]
      (cond
        (and (= 4 (count frame)) (= "call" a) (= call-fn-name b)
             (vector? c) (= 2 (count c)) (string? (first c)) (map? (second c))
             (int-id? d) (neg? d))
        :server-call

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
    :invalid nil
    (first frame)))

(defn call
  "Server -> client call frame."
  [call-id op params]
  ["call" call-fn-name [op (or params {})] call-id])

(defn call-op [frame] (first (nth frame 2)))

(defn call-params [frame] (second (nth frame 2)))

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
