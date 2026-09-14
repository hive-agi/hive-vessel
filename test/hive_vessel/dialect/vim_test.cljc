(ns hive-vessel.dialect.vim-test
  "The :vim-channel dialect's lowering fns: schema-synthesized from the
   primitive each lowers, related to the op by the channel call's arguments,
   and a hand mutant for the dropped default, killed through the real plan."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.mutation :as mut]
            [hive-vessel.dialect.vim :as vim]
            [hive-vessel.doc :as d]
            [hive-vessel.schema :as s]
            [hive-vessel.test-util :as tu]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(defn- calls?
  "True iff PAYLOAD calls F with ARGS. Arguments are compared as written
   JSON, the form Vim receives, so a NaN (null on the wire) compares equal
   to itself."
  [f args [tag g jargs]]
  (and (= "call" tag) (= f g) (= (wire/write-json args) (wire/write-json jargs))))

(hst/deftrifecta-from-schema notify-payload
  hive-vessel.dialect.vim/notify-payload
  {:in s/Notify
   :out vim/Call
   :rel (fn [{:keys [message level]} payload]
          (calls? "hive_vessel#notify" [message (name (or level :info))] payload))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema show-panel-payload
  hive-vessel.dialect.vim/show-panel-payload
  {:in s/ShowPanel
   :out vim/Call
   :rel (fn [{:keys [doc] panel-id :panel/id} [_ f [id title lines]]]
          (let [rendered (d/render-lines doc)]
            (and (= "hive_vessel#show_panel" f)
                 (= panel-id id)
                 (= (:doc/title doc) title)
                 (= (mapv :text rendered) (mapv #(get % "text") lines))
                 (= (mapv (comp name :face) rendered) (mapv #(get % "face") lines))
                 (= (mapv #(select-keys % [:file :line]) rendered)
                    (mapv (fn [l] (cond-> {}
                                    (contains? l "file") (assoc :file (get l "file"))
                                    (contains? l "line") (assoc :line (get l "line"))))
                          lines)))))
   :mutation true
   :num-tests 60})

(hst/deftrifecta-from-schema close-panel-payload
  hive-vessel.dialect.vim/close-panel-payload
  {:in s/ClosePanel
   :out vim/Call
   :rel (fn [{panel-id :panel/id} payload] (calls? "hive_vessel#close_panel" [panel-id] payload))
   :mutation true
   :num-tests 60})

(hst/deftrifecta-from-schema open-file-payload
  hive-vessel.dialect.vim/open-file-payload
  {:in s/OpenFile
   :out vim/Call
   :rel (fn [{:keys [file line column]} payload]
          (calls? "hive_vessel#open_file" [file (or line 1) (or column 1)] payload))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema send-to-terminal-payload
  hive-vessel.dialect.vim/send-to-terminal-payload
  {:in s/SendToTerminal
   :out vim/Call
   :rel (fn [{:keys [terminal text]} payload]
          (calls? "hive_vessel#send_to_terminal" [terminal text] payload))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema call-op-payload
  hive-vessel.dialect.vim/call-op-payload
  {:in vim/VimCall
   :out vim/Call
   :rel (fn [{f :fn args :args} payload] (calls? f (vec (or args [])) payload))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema ex-payload
  hive-vessel.dialect.vim/ex-payload
  {:in vim/VimEx
   :out vim/Ex
   :rel (fn [{:keys [command]} [tag c]] (and (= "ex" tag) (= command c)))
   :mutation true
   :num-tests 100})

(mut/deftest-mutations open-file-always-sends-all-three-arguments
  hive-vessel.dialect.vim/open-file-payload
  [["no-line-default" (fn [{:keys [file line column]}]
                        (vim/call-payload "hive_vessel#open_file" [file line (or column 1)]))]
   ["no-column-default" (fn [{:keys [file line column]}]
                          (vim/call-payload "hive_vessel#open_file" [file (or line 1) column]))]]
  (fn []
    (is (= ["call" "hive_vessel#open_file" ["a.clj" 1 1]]
           (tu/lowered-payload :vim {:op :ui/open-file :file "a.clj"})))
    (is (= ["call" "hive_vessel#open_file" ["a.clj" 3 4]]
           (tu/lowered-payload :vim {:op :ui/open-file :file "a.clj" :line 3 :column 4})))))

(deftest a-translator-reads-its-lowering-through-the-var
  (mut/with-mutation [hive-vessel.dialect.vim/open-file-payload (fn [_] ["ex" "mutant"])]
    (is (= ["ex" "mutant"] (tu/lowered-payload :vim {:op :ui/open-file :file "a.clj"}))))
  (is (= ["call" "hive_vessel#open_file" ["a.clj" 1 1]]
         (tu/lowered-payload :vim {:op :ui/open-file :file "a.clj"}))))