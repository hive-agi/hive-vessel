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
            [hive-vessel.wire :as wire]
            [hive-vessel.schema :as s]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def dialect :vim-channel)

(defn native [payload] {:op :vessel/native :native/dialect dialect :native/payload payload})

(def Call
  "A channel command calling a Vim function: [\"call\" fn json-args]."
  [:tuple [:= "call"] s/NonBlank [:vector :any]])

(def Ex
  "A channel command running an Ex command: [\"ex\" command]."
  [:tuple [:= "ex"] s/NonBlank])

(def Payload [:or Call Ex])

(defn call-payload [f args] ["call" f (wire/->json-data (vec args))])

(defn call [f args] (native (call-payload f args)))

(defn- line->json [{:keys [text face file line]}]
  (cond-> {"text" text "face" (name face)}
    file (assoc "file" file)
    line (assoc "line" line)))

(defn notify-payload [{:keys [message level]}]
  (call-payload "hive_vessel#notify" [message (name (or level :info))]))
(m/=> notify-payload [:=> [:cat s/Notify] Call])

(defn show-panel-payload
  "The panel id, the title and the rendered lines, each line a JSON object
   with text, face and an optional file/line to visit."
  [{:keys [doc] panel-id :panel/id}]
  (call-payload "hive_vessel#show_panel"
                [panel-id (:doc/title doc) (mapv line->json (doc/render-lines doc))]))
(m/=> show-panel-payload [:=> [:cat s/ShowPanel] Call])

(defn close-panel-payload [{panel-id :panel/id}]
  (call-payload "hive_vessel#close_panel" [panel-id]))
(m/=> close-panel-payload [:=> [:cat s/ClosePanel] Call])

(defn open-file-payload
  "Line and column default to 1: the plugin always receives all three."
  [{:keys [file line column]}]
  (call-payload "hive_vessel#open_file" [file (or line 1) (or column 1)]))
(m/=> open-file-payload [:=> [:cat s/OpenFile] Call])

(defn send-to-terminal-payload [{:keys [terminal text]}]
  (call-payload "hive_vessel#send_to_terminal" [terminal text]))
(m/=> send-to-terminal-payload [:=> [:cat s/SendToTerminal] Call])

(def VimCall
  "The :vim/call dialect call."
  [:map [:op [:= :vim/call]] [:fn s/NonBlank] [:args {:optional true} [:sequential :any]]])

(def VimEx
  "The :vim/ex dialect call."
  [:map [:op [:= :vim/ex]] [:command s/NonBlank]])

(defn call-op-payload [{f :fn args :args}] (call-payload f (or args [])))
(m/=> call-op-payload [:=> [:cat VimCall] Call])

(defn ex-payload [{:keys [command]}] ["ex" command])
(m/=> ex-payload [:=> [:cat VimEx] Ex])

(def translators
  (let [w {:vessel/dialect dialect}
        t (fn [op f] {:translator/id (keyword "hive-vessel.vim" (name op))
                      :translator/op op
                      :translator/when w
                      :translator/translate (fn [o _] (native (f o)))})]
    ;; Each lowering is reached through its var at call time, never captured.
    [(t :ui/notify #(notify-payload %))
     (t :ui/show-panel #(show-panel-payload %))
     (t :ui/close-panel #(close-panel-payload %))
     (t :ui/open-file #(open-file-payload %))
     (t :ui/send-to-terminal #(send-to-terminal-payload %))
     (assoc (t :vim/call #(call-op-payload %))
            :translator/accepts [:map [:fn [:string {:min 1}]] [:args {:optional true} [:sequential :any]]])
     (assoc (t :vim/ex #(ex-payload %))
            :translator/accepts [:map [:command [:string {:min 1}]]])]))
