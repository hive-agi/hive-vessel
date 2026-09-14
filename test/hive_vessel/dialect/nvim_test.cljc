(ns hive-vessel.dialect.nvim-test
  "The :nvim-rpc dialect's lowering fns: schema-synthesized from the primitive
   each lowers, related to the op by the API call's method and params, and a
   hand mutant for the dropped default, killed through the real plan."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.mutation :as mut]
            [hive-vessel.dialect.nvim :as nvim]
            [hive-vessel.doc :as d]
            [hive-vessel.schema :as s]
            [hive-vessel.test-util :as tu]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(defn- api-call?
  "True iff PAYLOAD is METHOD applied to PARAMS, compared as written JSON (the
   shape the msgpack encoder takes)."
  [method params payload]
  (and (= method (:nvim/method payload))
       (= (wire/write-json params) (wire/write-json (:nvim/params payload)))))

(defn- calls? [f args payload] (api-call? "nvim_call_function" [f args] payload))

(hst/deftrifecta-from-schema notify-payload
  hive-vessel.dialect.nvim/notify-payload
  {:in s/Notify
   :out nvim/Payload
   :rel (fn [{:keys [message level]} payload]
          (calls? "hive_vessel#notify" [message (name (or level :info))] payload))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema show-panel-payload
  hive-vessel.dialect.nvim/show-panel-payload
  {:in s/ShowPanel
   :out nvim/Payload
   :rel (fn [{:keys [doc] panel-id :panel/id} {:nvim/keys [method params]}]
          (let [[f [id title lines]] params
                rendered (d/render-lines doc)]
            (and (= "nvim_call_function" method)
                 (= "hive_vessel#show_panel" f)
                 (= panel-id id)
                 (= (:doc/title doc) title)
                 (= (mapv :text rendered) (mapv #(get % "text") lines))
                 (= (mapv (comp name :face) rendered) (mapv #(get % "face") lines)))))
   :mutation true
   :num-tests 60})

(hst/deftrifecta-from-schema close-panel-payload
  hive-vessel.dialect.nvim/close-panel-payload
  {:in s/ClosePanel
   :out nvim/Payload
   :rel (fn [{panel-id :panel/id} payload] (calls? "hive_vessel#close_panel" [panel-id] payload))
   :mutation true
   :num-tests 60})

(hst/deftrifecta-from-schema open-file-payload
  hive-vessel.dialect.nvim/open-file-payload
  {:in s/OpenFile
   :out nvim/Payload
   :rel (fn [{:keys [file line column]} payload]
          (calls? "hive_vessel#open_file" [file (or line 1) (or column 1)] payload))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema send-to-terminal-payload
  hive-vessel.dialect.nvim/send-to-terminal-payload
  {:in s/SendToTerminal
   :out nvim/Payload
   :rel (fn [{:keys [terminal text]} payload]
          (calls? "hive_vessel#send_to_terminal" [terminal text] payload))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema call-op-payload
  hive-vessel.dialect.nvim/call-op-payload
  {:in nvim/NvimCall
   :out nvim/Payload
   :rel (fn [{f :fn args :args} payload] (calls? f (vec (or args [])) payload))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema lua-payload
  hive-vessel.dialect.nvim/lua-payload
  {:in nvim/NvimLua
   :out nvim/Payload
   :rel (fn [{:keys [code args]} payload] (api-call? "nvim_exec_lua" [code (vec (or args []))] payload))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema command-payload
  hive-vessel.dialect.nvim/command-payload
  {:in nvim/NvimCommand
   :out nvim/Payload
   :rel (fn [{:keys [command]} payload] (api-call? "nvim_command" [command] payload))
   :mutation true
   :num-tests 100})

(mut/deftest-mutations open-file-always-sends-all-three-arguments
  hive-vessel.dialect.nvim/open-file-payload
  [["no-line-default" (fn [{:keys [file line column]}]
                        (nvim/call-payload "hive_vessel#open_file" [file line (or column 1)]))]
   ["no-column-default" (fn [{:keys [file line column]}]
                          (nvim/call-payload "hive_vessel#open_file" [file (or line 1) column]))]]
  (fn []
    (is (= {:nvim/method "nvim_call_function" :nvim/params ["hive_vessel#open_file" ["a.clj" 1 1]]}
           (tu/lowered-payload :neovim {:op :ui/open-file :file "a.clj"})))
    (is (= {:nvim/method "nvim_call_function" :nvim/params ["hive_vessel#open_file" ["a.clj" 3 4]]}
           (tu/lowered-payload :neovim {:op :ui/open-file :file "a.clj" :line 3 :column 4})))))

(deftest a-translator-reads-its-lowering-through-the-var
  (mut/with-mutation [hive-vessel.dialect.nvim/open-file-payload (fn [_] {:nvim/method "mutant" :nvim/params []})]
    (is (= {:nvim/method "mutant" :nvim/params []} (tu/lowered-payload :neovim {:op :ui/open-file :file "a.clj"}))))
  (is (= {:nvim/method "nvim_call_function" :nvim/params ["hive_vessel#open_file" ["a.clj" 1 1]]}
         (tu/lowered-payload :neovim {:op :ui/open-file :file "a.clj"}))))

(deftest the-nvim-and-vim-dialects-paint-the-same-call
  ;; Both call the same autoload function with the same arguments; only the
  ;; framing differs (a JSON channel command vs an API call).
  (doseq [op [{:op :ui/notify :message "hi" :level :warn}
              {:op :ui/show-panel :panel/id "p" :doc (d/doc "T" (d/para "x"))}
              {:op :ui/close-panel :panel/id "p"}
              {:op :ui/open-file :file "a.clj" :line 2}
              {:op :ui/send-to-terminal :terminal "*t*" :text "ls\n"}]]
    (let [[_ f args] (tu/lowered-payload :vim op)
          {:nvim/keys [method params]} (tu/lowered-payload :neovim op)]
      (is (= "nvim_call_function" method))
      (is (= [f args] params) (pr-str op)))))