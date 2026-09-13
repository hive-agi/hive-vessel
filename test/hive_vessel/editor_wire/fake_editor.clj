(ns hive-vessel.editor-wire.fake-editor
  "In-memory editor peer for adapter tests: a transport that answers server calls
   through a handler on another thread, as a real editor would."
  (:require [hive-vessel.editor-wire.codec :as codec]
            [hive-vessel.editor-wire.hub :as hub]
            [hive-vessel.editor-wire.transport :as transport]))

;; SPDX-License-Identifier: MIT

(defrecord FakeEditor [hub* sid* handler native sent calls closed?]
  transport/ITransport
  (send-frame! [_ frame]
    (swap! sent conj frame)
    (case (codec/classify frame)
      :server-call
      (let [op (codec/call-op frame)
            params (codec/call-params frame)
            id (codec/frame-id frame)]
        (swap! calls conj [op params])
        (future
          (let [result (handler op params)]
            (when-not (= ::silent result)
              (hub/receive! @hub* @sid* [id result])))))

      :server-native
      (let [payload (codec/native-payload frame)
            id (codec/frame-id frame)]
        (swap! calls conj [:native payload])
        (future
          (let [value (native payload)]
            (when-not (= ::silent value)
              (hub/receive! @hub* @sid* [id value])))))

      (when (codec/native? frame)
        (swap! calls conj [:native frame])))
    nil)
  (close! [_]
    (when (compare-and-set! closed? false true)
      (hub/disconnect! @hub* @sid*))
    nil))

(defn connect!
  "Connect a fake editor answering HiveOp calls with HANDLER (fn [op params]
   -> Result or ::silent) and native channel commands with :native (fn
   [payload] -> value or ::silent; default echoes the payload). Sends hello
   with TOKEN. Returns the fake."
  ([h handler] (connect! h handler {}))
  ([h handler {:keys [token editor native] :or {editor "fake" native identity}}]
   (let [fake (->FakeEditor (atom h) (atom nil) handler native (atom []) (atom []) (atom false))
         sid (hub/connect! h fake)]
     (reset! (:sid* fake) sid)
     (hub/receive! h sid [1 (codec/hello {:token (or token (:token h)) :editor editor})])
     fake)))

(defn sid [fake] @(:sid* fake))

(defn calls [fake] @(:calls fake))
