(ns hive-vessel.editor-wire.ops
  "Op vocabulary of hive editor wire v1 and the Result -> MCP projection.

   Op names are the hive-spi.editor.ports and hive-addon.terminal method names
   (bang stripped), as strings. Pure data, loads on JVM and ClojureScript."
  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT

(def wire-version 1)

(def substrate-ops
  ["editor-eval" "editor-notify" "editor-status" "editor-capabilities"])

(def buffer-ops
  ["list-buffers" "current-buffer" "buffer-info" "special-buffers"
   "switch-buffer" "find-file" "save-buffers" "goto-line" "insert-text"
   "recent-files" "project-root" "editor-context"])

(def terminal-ops
  ["terminal-spawn" "terminal-dispatch" "terminal-status" "terminal-kill"
   "terminal-interrupt" "terminal-read"])

(def surfaces
  "surface keyword -> ordered op names."
  {:editor substrate-ops
   :buffer buffer-ops
   :terminal terminal-ops})

(def op->surface
  "op name -> surface keyword."
  (into {}
        (mapcat (fn [[surface names]] (map (fn [n] [n surface]) names)))
        surfaces))

(def all-ops (set (keys op->surface)))

(defn op?
  "True when x names a wire v1 op."
  [x]
  (contains? all-ops x))

(defn method->op
  "Protocol method name (symbol, keyword or string) -> wire op name."
  [method]
  (let [n (if (string? method) method (name method))]
    (if (str/ends-with? n "!") (subs n 0 (dec (count n))) n)))

(def error-codes
  #{"wire/timeout" "wire/not-connected" "wire/unknown-op" "wire/closed"
    "wire/invalid-frame" "auth/denied" "op/failed" "op/unsupported"})

(def event-names
  #{"focus" "selection_changed" "at_mentioned" "terminal_exit"})

(def default-timeout-ms 5000)

(defn timeout-ms
  "Bounded wait for OP given PARAMS (string keys). editor-eval honours timeout_ms."
  [op params]
  (let [requested (get params "timeout_ms")]
    (if (and (= op "editor-eval") (integer? requested) (pos? requested))
      requested
      default-timeout-ms)))

(defn ok
  "Success Result envelope."
  [value]
  {"ok" true "value" value})

(defn err
  "Failure Result envelope."
  ([code] {"ok" false "error" {"code" code}})
  ([code message] {"ok" false "error" {"code" code "message" message}}))

(defn ok?
  [result]
  (true? (get result "ok")))

(defn error-code
  "The error code of a failed Result, nil for success."
  [result]
  (when-not (ok? result) (get-in result ["error" "code"])))

(defn result->mcp
  "Result envelope -> MCP response map. ENCODE turns the payload into text."
  [encode result]
  (if (ok? result)
    {:content [{:type "text" :text (encode (get result "value"))}]
     :isError false}
    {:content [{:type "text" :text (encode (get result "error"))}]
     :isError true}))
