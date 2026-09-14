(ns hive-vessel.dialect.text
  "The :text dialect: plain lines for character-cell vessels without an
   editor -- a tmux pane, a CLI, a log, a headless test sink.

   Payload: `{:text/lines [string ...]}`, plus `:text/terminal` when the lines
   are keystrokes for a named terminal, and `:text/face-lines` (the rendered
   lines with faces) for a pane that can colour them."
  (:require [hive-vessel.doc :as doc]
            [hive-vessel.schema :as s]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def dialect :text)

(defn native [payload] {:op :vessel/native :native/dialect dialect :native/payload payload})

(defn- nonempty-lines [s]
  (let [ls (doc/split-lines s)] (if (empty? ls) [""] ls)))

(def Payload
  "A :text native payload. Every op carries :text/lines; a panel op names
   its panel, a terminal op its terminal and the raw keys."
  [:map
   [:text/lines [:vector :string]]
   [:text/panel {:optional true} s/NonBlank]
   [:text/close? {:optional true} :boolean]
   [:text/face-lines {:optional true} [:vector s/RenderedLine]]
   [:text/terminal {:optional true} s/NonBlank]
   [:text/keys {:optional true} :string]])

(defn notify-payload
  "The :text payload of a :ui/notify op: one `[level] ` tagged line per
   message line, at least one."
  [{:keys [message level]}]
  {:text/lines (mapv #(str "[" (name (or level :info)) "] " %)
                     (nonempty-lines message))})

(m/=> notify-payload [:=> [:cat s/Notify] Payload])

(defn show-panel-payload
  "The :text payload of a :ui/show-panel op: the rendered lines, plain and
   face-tagged, under the panel id."
  [{:keys [doc] panel-id :panel/id}]
  (let [lines (doc/render-lines doc)]
    {:text/panel panel-id
     :text/lines (mapv :text lines)
     :text/face-lines lines}))

(m/=> show-panel-payload [:=> [:cat s/ShowPanel] Payload])

(defn close-panel-payload
  [{panel-id :panel/id}]
  {:text/panel panel-id :text/close? true :text/lines []})

(m/=> close-panel-payload [:=> [:cat s/ClosePanel] Payload])

(defn open-file-payload
  "`open FILE[:LINE[:COLUMN]]`. A column without a line reads as line 1, so
   no given field is dropped."
  [{:keys [file line column]}]
  {:text/lines [(str "open " file
                     (when (or line column)
                       (str ":" (or line 1) (when column (str ":" column)))))]})

(m/=> open-file-payload [:=> [:cat s/OpenFile] Payload])

(defn send-to-terminal-payload
  [{:keys [terminal text]}]
  {:text/terminal terminal :text/lines (doc/split-lines text) :text/keys text})

(m/=> send-to-terminal-payload [:=> [:cat s/SendToTerminal] Payload])

(def translators
  (let [w {:vessel/dialect dialect}
        t (fn [op f] {:translator/id (keyword "hive-vessel.text" (name op))
                      :translator/op op
                      :translator/when w
                      :translator/translate (fn [o _] (native (f o)))})]
    ;; Each lowering is reached through its var at call time, never captured.
    [(t :ui/notify #(notify-payload %))
     (t :ui/show-panel #(show-panel-payload %))
     (t :ui/close-panel #(close-panel-payload %))
     (t :ui/open-file #(open-file-payload %))
     (t :ui/send-to-terminal #(send-to-terminal-payload %))]))
