(ns hive-vessel.editor-wire.terminal
  "RemoteTerminal: hive-addon ITerminalAddon (plus IAddon lifecycle) over an ICaller.
   Lings run in terminals owned by the remote editor."
  (:require [hive-addon.protocol :as addon]
            [hive-addon.terminal :as term]
            [hive-vessel.editor-wire.ops :as ops]
            [hive-vessel.editor-wire.transport :as transport]))

;; SPDX-License-Identifier: MIT

(defn- fail!
  [what id result]
  (throw (ex-info (str what " failed: " (ops/error-code result))
                  {:id id :error (ops/error-code result) :result result})))

(defn spawn-params
  "Wire params for terminal-spawn from a ling CTX, spawn OPTS and default COMMAND."
  [ctx opts command]
  (cond-> {"id" (:id ctx)
           "cmd" (vec (or (:command opts) command))}
    (:cwd ctx) (assoc "cwd" (:cwd ctx))
    (:env opts) (assoc "env" (:env opts))))

(defn context-of
  "Recorded {:cwd :project-id} for ling ID spawned through TERMINAL, or nil."
  [terminal id]
  (get @(:contexts terminal) id))

(defrecord RemoteTerminal [caller terminal-kw command contexts]
  term/ITerminalAddon
  (terminal-id [_] terminal-kw)

  (terminal-spawn! [_ ctx opts]
    (let [id (:id ctx)
          result (transport/call! caller "terminal-spawn" (spawn-params ctx opts command))]
      (when-not (ops/ok? result) (fail! "terminal-spawn" id result))
      (swap! contexts assoc id (select-keys ctx [:cwd :project-id]))
      id))

  (terminal-dispatch! [_ ctx task-opts]
    (let [id (:id ctx)
          result (transport/call! caller "terminal-dispatch"
                                  {"id" id "text" (str (:task task-opts))})]
      (when-not (ops/ok? result) (fail! "terminal-dispatch" id result))
      true))

  (terminal-status [_ ctx _ds-status]
    (let [id (:id ctx)
          result (transport/call! caller "terminal-status" {"id" id})]
      (when (ops/ok? result)
        (let [status (get-in result ["value" "status"])]
          {:slave/id id
           :slave/status (if (string? status) (keyword status) :unknown)}))))

  (terminal-kill! [_ ctx]
    (let [id (:id ctx)
          result (transport/call! caller "terminal-kill" {"id" id})]
      (if (ops/ok? result)
        (do (swap! contexts dissoc id) {:killed? true :id id})
        {:killed? false :id id :reason (keyword (ops/error-code result))})))

  (terminal-interrupt! [_ ctx]
    (let [id (:id ctx)
          result (transport/call! caller "terminal-interrupt" {"id" id})]
      (if (ops/ok? result)
        {:success? true :ling-id id}
        {:success? false :ling-id id :errors [(ops/error-code result)]})))

  addon/IAddon
  (addon-id [_] (str "hive.editor-wire.terminal." (name terminal-kw)))
  (addon-type [_] :native)
  (capabilities [_] #{:terminal})
  (initialize! [_ _config] {:success? true :errors []})
  (shutdown! [_] (reset! contexts {}) nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok :details {:terminals (count @contexts)}})
  (excluded-tools [_] #{})
  (hooks [_] {}))

(defn read-lines
  "Screen lines of ling ID, or a failed Result's error code as {:error code}."
  [terminal id]
  (let [result (transport/call! (:caller terminal) "terminal-read" {"id" id})]
    (if (ops/ok? result)
      (vec (get-in result ["value" "lines"]))
      {:error (ops/error-code result)})))

(defn remote-terminal
  "ITerminalAddon :terminal-kw running COMMAND (argv vector) in the remote editor."
  [caller terminal-kw command]
  (->RemoteTerminal caller terminal-kw (vec command) (atom {})))
