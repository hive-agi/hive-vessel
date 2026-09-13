(ns hive-vessel.editor-wire.executor
  "A hive-vessel :vessel/execute! over the editor-wire hub: every :vim-channel
   native op travels to the active Vim session as a channel command, and its
   reply comes back through the session's own pending table. Discovery,
   token, reconnection and many sessions are the hub's; nothing here knows a
   port. The other option for the same dialect is
   hive-vessel.executor.vim-channel, one Vim on a port the user types."
  (:require [hive-vessel.editor-wire.hub :as hub]
            [hive-vessel.editor-wire.ops :as ops]))

;; SPDX-License-Identifier: MIT

(def dialect :vim-channel)

(defn- hub-of
  "HUB-OR-SERVER: a hub, or the map server/start! returns."
  [x]
  (or (:hub x) x))

(defn execute!
  "Run native OP on the active session of HUB-OR-SERVER, waiting at most
   TIMEOUT-MS. Returns the editor's reply value (nil for a command that never
   replies). Throws ex-info when the wire fails, so hive-vessel.dispatch
   reports :execute-threw with how many ops of the batch already ran."
  [hub-or-server timeout-ms {:native/keys [dialect payload]}]
  (when-not (= :vim-channel dialect)
    (throw (ex-info "the editor-wire executor runs :vim-channel only" {:dialect dialect})))
  (let [result (hub/native! (hub-of hub-or-server) payload timeout-ms)]
    (if (ops/ok? result)
      (get result "value")
      (throw (ex-info (str "editor wire: " (ops/error-code result))
                      {:error (ops/error-code result)
                       :message (get-in result ["error" "message"])
                       :payload payload})))))

(defn executor
  "A :vessel/execute! fn over HUB-OR-SERVER."
  ([hub-or-server] (executor hub-or-server ops/default-timeout-ms))
  ([hub-or-server timeout-ms] (fn [op] (execute! hub-or-server timeout-ms op))))

(defn target
  "A hive-vessel target for the Vim sessions of HUB-OR-SERVER:
   {:vessel/id :vim :vessel/dialect :vim-channel :vessel/execute! f}.
   :features advertises capabilities to translator guards; :timeout-ms
   bounds every native reply."
  [hub-or-server & {:keys [features timeout-ms]}]
  (cond-> {:vessel/id :vim
           :vessel/dialect dialect
           :vessel/execute! (executor hub-or-server (or timeout-ms ops/default-timeout-ms))}
    features (assoc :vessel/features features)))
