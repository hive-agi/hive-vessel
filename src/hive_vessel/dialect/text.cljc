(ns hive-vessel.dialect.text
  "The :text dialect: plain lines for character-cell vessels without an
   editor -- a tmux pane, a CLI, a log, a headless test sink.

   Payload: `{:text/lines [string ...]}`, plus `:text/terminal` when the lines
   are keystrokes for a named terminal, and `:text/face-lines` (the rendered
   lines with faces) for a pane that can colour them."
  (:require [hive-vessel.doc :as doc]))

;; SPDX-License-Identifier: MIT

(def dialect :text)

(defn native [payload] {:op :vessel/native :native/dialect dialect :native/payload payload})

(defn- nonempty-lines [s]
  (let [ls (doc/split-lines s)] (if (empty? ls) [""] ls)))

(def translators
  (let [w {:vessel/dialect dialect}
        t (fn [op f] {:translator/id (keyword "hive-vessel.text" (name op))
                      :translator/op op
                      :translator/when w
                      :translator/translate (fn [o _] (native (f o)))})]
    [(t :ui/notify
        (fn [{:keys [message level]}]
          {:text/lines (mapv #(str "[" (name (or level :info)) "] " %)
                             (nonempty-lines message))}))
     (t :ui/show-panel
        (fn [{:keys [doc] panel-id :panel/id}]
          (let [lines (doc/render-lines doc)]
            {:text/panel panel-id
             :text/lines (mapv :text lines)
             :text/face-lines lines})))
     (t :ui/close-panel
        (fn [{panel-id :panel/id}] {:text/panel panel-id :text/close? true :text/lines []}))
     (t :ui/open-file
        (fn [{:keys [file line column]}]
          {:text/lines [(str "open " file (when line (str ":" line (when column (str ":" column)))))]}))
     (t :ui/send-to-terminal
        (fn [{:keys [terminal text]}]
          {:text/terminal terminal :text/lines (doc/split-lines text) :text/keys text}))]))
