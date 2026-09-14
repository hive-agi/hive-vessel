(ns hive-vessel.dialect.nvim
  "The :nvim-rpc dialect: Neovim API calls over msgpack-rpc (`:help
   msgpack-rpc`, `:help api`).

   A native payload is one API call as data, `{:nvim/method \"nvim_call_function\"
   :nvim/params [...]}`. The executor frames it as a request or a notification
   and encodes the msgpack; this dialect never touches bytes. Standard
   primitives call the same bundled autoload functions the :vim-channel
   dialect calls: resources/hive-vessel/vim/autoload/hive_vessel.vim is legacy
   Vim script and runs unchanged in Neovim, so both editors paint the same
   rendered lines.

   Dialect calls for vessels shipping their own Lua or Vim script:
   `{:op :nvim/call :fn \"F\" :args [...]}` (nvim_call_function),
   `{:op :nvim/lua :code \"return ...\" :args [...]}` (nvim_exec_lua, args
   arrive as `...`) and `{:op :nvim/command :command \"...\"}` (nvim_command)."
  (:require [hive-vessel.dialect.vim :as vim]
            [hive-vessel.doc :as doc]
            [hive-vessel.schema :as s]
            [hive-vessel.wire :as wire]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def dialect :nvim-rpc)

(defn native [payload] {:op :vessel/native :native/dialect dialect :native/payload payload})

(def Payload
  "One Neovim API call as data: the method name and its positional
   parameters, already JSON-shaped (string keys, vectors) so any msgpack
   encoder can take them as they are."
  [:map
   [:nvim/method s/NonBlank]
   [:nvim/params [:vector :any]]])

(defn api-call [method params]
  {:nvim/method method :nvim/params (wire/->json-data (vec params))})

(defn call-payload
  "nvim_call_function of F with ARGS."
  [f args]
  (api-call "nvim_call_function" [f (vec args)]))

(defn notify-payload [{:keys [message level]}]
  (call-payload "hive_vessel#notify" [message (name (or level :info))]))
(m/=> notify-payload [:=> [:cat s/Notify] Payload])

(defn show-panel-payload
  "The panel id, the title and the rendered lines, each line a JSON object
   with text, face and an optional file/line to visit."
  [{:keys [doc] panel-id :panel/id}]
  (call-payload "hive_vessel#show_panel"
                [panel-id (:doc/title doc) (mapv vim/line->json (doc/render-lines doc))]))
(m/=> show-panel-payload [:=> [:cat s/ShowPanel] Payload])

(defn close-panel-payload [{panel-id :panel/id}]
  (call-payload "hive_vessel#close_panel" [panel-id]))
(m/=> close-panel-payload [:=> [:cat s/ClosePanel] Payload])

(defn open-file-payload
  "Line and column default to 1: the autoload always receives all three."
  [{:keys [file line column]}]
  (call-payload "hive_vessel#open_file" [file (or line 1) (or column 1)]))
(m/=> open-file-payload [:=> [:cat s/OpenFile] Payload])

(defn send-to-terminal-payload [{:keys [terminal text]}]
  (call-payload "hive_vessel#send_to_terminal" [terminal text]))
(m/=> send-to-terminal-payload [:=> [:cat s/SendToTerminal] Payload])

(def NvimCall
  "The :nvim/call dialect call."
  [:map [:op [:= :nvim/call]] [:fn s/NonBlank] [:args {:optional true} [:sequential :any]]])

(def NvimLua
  "The :nvim/lua dialect call: a Lua chunk run by nvim_exec_lua."
  [:map [:op [:= :nvim/lua]] [:code s/NonBlank] [:args {:optional true} [:sequential :any]]])

(def NvimCommand
  "The :nvim/command dialect call: one Ex command run by nvim_command."
  [:map [:op [:= :nvim/command]] [:command s/NonBlank]])

(defn call-op-payload [{f :fn args :args}] (call-payload f (or args [])))
(m/=> call-op-payload [:=> [:cat NvimCall] Payload])

(defn lua-payload [{:keys [code args]}] (api-call "nvim_exec_lua" [code (vec (or args []))]))
(m/=> lua-payload [:=> [:cat NvimLua] Payload])

(defn command-payload [{:keys [command]}] (api-call "nvim_command" [command]))
(m/=> command-payload [:=> [:cat NvimCommand] Payload])

(def translators
  (let [w {:vessel/dialect dialect}
        t (fn [op f] {:translator/id (keyword "hive-vessel.nvim" (name op))
                      :translator/op op
                      :translator/when w
                      :translator/translate (fn [o _] (native (f o)))})]
    ;; Each lowering is reached through its var at call time, never captured.
    [(t :ui/notify #(notify-payload %))
     (t :ui/show-panel #(show-panel-payload %))
     (t :ui/close-panel #(close-panel-payload %))
     (t :ui/open-file #(open-file-payload %))
     (t :ui/send-to-terminal #(send-to-terminal-payload %))
     (assoc (t :nvim/call #(call-op-payload %))
            :translator/accepts [:map [:fn [:string {:min 1}]] [:args {:optional true} [:sequential :any]]])
     (assoc (t :nvim/lua #(lua-payload %))
            :translator/accepts [:map [:code [:string {:min 1}]] [:args {:optional true} [:sequential :any]]])
     (assoc (t :nvim/command #(command-payload %))
            :translator/accepts [:map [:command [:string {:min 1}]]])]))