(ns hive-vessel.editor-wire.port
  "RemoteEditorPort: hive-spi editor substrate and buffer surfaces over an ICaller.
   Every method forwards its MCP params as the wire op of the same name."
  (:require [hive-vessel.editor-wire.ops :as ops]
            [hive-vessel.editor-wire.transport :as transport]
            [hive-spi.editor.ports :as ports]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(defn string-keys
  "PARAMS with keyword keys rendered as strings. nil -> {}."
  [params]
  (into {}
        (map (fn [[k v]] [(if (keyword? k) (name k) (str k)) v]))
        params))

(defn invoke
  "Call OP on CALLER and project the Result to an MCP response map."
  [caller op params]
  (ops/result->mcp wire/write-json (transport/call! caller op (string-keys params))))

(defrecord RemoteEditorPort [caller]
  ports/IEditorPort
  (editor-eval [_ params] (invoke caller "editor-eval" params))
  (editor-notify [_ params] (invoke caller "editor-notify" params))
  (editor-status [_ params] (invoke caller "editor-status" params))
  (editor-capabilities [_ params] (invoke caller "editor-capabilities" params))

  ports/IEditorBufferPort
  (list-buffers [_ params] (invoke caller "list-buffers" params))
  (current-buffer [_ params] (invoke caller "current-buffer" params))
  (buffer-info [_ params] (invoke caller "buffer-info" params))
  (special-buffers [_ params] (invoke caller "special-buffers" params))
  (switch-buffer [_ params] (invoke caller "switch-buffer" params))
  (find-file [_ params] (invoke caller "find-file" params))
  (save-buffers [_ params] (invoke caller "save-buffers" params))
  (goto-line [_ params] (invoke caller "goto-line" params))
  (insert-text [_ params] (invoke caller "insert-text" params))
  (recent-files [_ params] (invoke caller "recent-files" params))
  (project-root [_ params] (invoke caller "project-root" params))
  (editor-context [_ params] (invoke caller "editor-context" params)))

(defn remote-editor-port
  "An editor port routing every verb through CALLER."
  [caller]
  (->RemoteEditorPort caller))
