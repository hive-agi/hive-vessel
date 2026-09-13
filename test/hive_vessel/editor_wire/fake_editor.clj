(ns hive-vessel.editor-wire.fake-editor
  "In-memory editor peer for adapter tests: a transport that answers server calls
   through a handler on another thread, as a real editor would."
  (:require [hive-vessel.editor-wire.codec :as codec]
            [hive-vessel.editor-wire.hub :as hub]
            [hive-vessel.editor-wire.transport :as transport]))

;; SPDX-License-Identifier: MIT

(defrecord FakeEditor [hub* sid* handler sent calls closed?]
  transport/ITransport
  (send-frame! [_ frame]
    (swap! sent conj frame)
    (when (= :server-call (codec/classify frame))
      (let [op (codec/call-op frame)
            params (codec/call-params frame)
            id (codec/frame-id frame)]
        (swap! calls conj [op params])
        (future
          (let [result (handler op params)]
            (when-not (= ::silent result)
              (hub/receive! @hub* @sid* [id result]))))))
    nil)
  (close! [_]
    (when (compare-and-set! closed? false true)
      (hub/disconnect! @hub* @sid*))
    nil))

(defn connect!
  "Connect a fake editor answering with HANDLER (fn [op params] -> Result or ::silent).
   Sends hello with TOKEN. Returns the fake."
  ([h handler] (connect! h handler {}))
  ([h handler {:keys [token editor] :or {editor "fake"}}]
   (let [fake (->FakeEditor (atom h) (atom nil) handler (atom []) (atom []) (atom false))
         sid (hub/connect! h fake)]
     (reset! (:sid* fake) sid)
     (hub/receive! h sid [1 (codec/hello {:token (or token (:token h)) :editor editor})])
     fake)))

(defn sid [fake] @(:sid* fake))

(defn calls [fake] @(:calls fake))
