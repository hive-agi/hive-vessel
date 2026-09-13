(ns hive-vessel.dialect.vim
  "The :vim-channel dialect: Vim channel commands (`:help channel-commands`).

   A native payload is the JSON-able vector `[\"call\" fn args]`. The executor
   writes it to a channel Vim opened in JSON mode (appending a request id
   when it wants the reply). The functions called live in the bundled plugin,
   resources/hive-vessel/vim/autoload/hive_vessel.vim, which paints the same
   rendered lines as every other character-cell vessel.

   Dialect calls: `{:op :vim/call :fn \"MyAddon#show\" :args [...]}` and
   `{:op :vim/ex :command \"...\"}` for addons shipping their own Vim script."
  (:require [hive-vessel.doc :as doc]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(def dialect :vim-channel)

(defn native [payload] {:op :vessel/native :native/dialect dialect :native/payload payload})

(defn call [f args] (native ["call" f (wire/->json-data (vec args))]))

(defn- line->json [{:keys [text face file line]}]
  (cond-> {"text" text "face" (name face)}
    file (assoc "file" file)
    line (assoc "line" line)))

(def translators
  (let [w {:vessel/dialect dialect}
        t (fn [op f] {:translator/id (keyword "hive-vessel.vim" (name op))
                      :translator/op op
                      :translator/when w
                      :translator/translate (fn [o _] (f o))})]
    [(t :ui/notify
        (fn [{:keys [message level]}]
          (call "hive_vessel#notify" [message (name (or level :info))])))
     (t :ui/show-panel
        (fn [{:keys [doc] panel-id :panel/id}]
          (call "hive_vessel#show_panel"
                [panel-id (:doc/title doc) (mapv line->json (doc/render-lines doc))])))
     (t :ui/close-panel
        (fn [{panel-id :panel/id}] (call "hive_vessel#close_panel" [panel-id])))
     (t :ui/open-file
        (fn [{:keys [file line column]}]
          (call "hive_vessel#open_file" [file (or line 1) (or column 1)])))
     (t :ui/send-to-terminal
        (fn [{:keys [terminal text]}]
          (call "hive_vessel#send_to_terminal" [terminal text])))
     (assoc (t :vim/call (fn [{f :fn args :args}] (call f (or args []))))
            :translator/accepts [:map [:fn [:string {:min 1}]] [:args {:optional true} [:sequential :any]]])
     (assoc (t :vim/ex (fn [{:keys [command]}] (native ["ex" command])))
            :translator/accepts [:map [:command [:string {:min 1}]]])]))
