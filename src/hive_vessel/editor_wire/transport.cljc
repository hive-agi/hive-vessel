(ns hive-vessel.editor-wire.transport
  "Ports of hive editor wire v1: how frames leave a process, and how an op is
   called on the active editor.")

;; SPDX-License-Identifier: MIT

(defprotocol ITransport
  (send-frame! [this frame]
    "Write one JSON-parsed frame to the peer. Returns nil.")
  (close! [this]
    "Close the connection. Idempotent. Returns nil."))

(defprotocol ICaller
  (call! [this op params]
    "Invoke OP with string-keyed PARAMS on the active editor session.
     Blocks at most the op's bounded wait. Returns a Result envelope."))

(defrecord RecordingTransport [sent closed?]
  ITransport
  (send-frame! [_ frame] (swap! sent conj frame) nil)
  (close! [_] (reset! closed? true) nil))

(defn recording-transport
  "In-memory transport that records sent frames in :sent and closure in :closed?."
  []
  (->RecordingTransport (atom []) (atom false)))
