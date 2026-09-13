(ns hive-vessel.executor-integration-test
  "Live executors: a private Emacs daemon driven through emacsclient, and a
   headless Vim that connects back to the channel server. One op batch,
   dispatched unchanged to both."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-vessel.core :as v]
            [hive-vessel.dialect.elisp :as elisp]
            [hive-vessel.doc :as d]
            [hive-vessel.executor.emacsclient :as ec]
            [hive-vessel.executor.vim-channel :as vc]
            [hive-vessel.wire :as wire]
            [hive-vessel.editor-wire.server :as server]
            [hive-vessel.editor-wire.executor :as executor]
            [hive-vessel.editor-wire.hub :as hub]
            [hive-vessel.editor-wire.transport :as transport])
  (:import (java.io File)
           (java.util.concurrent TimeUnit)
[java.nio.file Files]))

;; SPDX-License-Identifier: MIT

(def panel-doc
  (d/doc "Carto Flow #7"
         (d/para "apply write-form" :info)
         (d/diff "-(old)\n+(new)")
         (d/link "src/a.clj" "src/a.clj" 3)))

(def ops
  [{:op :ui/show-panel :panel/id "live" :doc panel-doc}
   {:op :ui/notify :message "frame 7 applied"}])

(defn- expected-lines [] (mapv :text (d/render-lines panel-doc)))

(deftest ^:integration emacs-daemon-over-emacsclient
  (let [socket (str "hive-vessel-test-" (System/currentTimeMillis))
        daemon (.start (ProcessBuilder. ["emacs" "-Q" (str "--fg-daemon=" socket)]))
        opts {:socket-name socket}]
    (try
      (loop [tries 50]
        (when (and (pos? tries)
                   (not (try (ec/eval-elisp opts "t") (catch Exception _ nil))))
          (Thread/sleep 200)
          (recur (dec tries))))
      (let [r (v/dispatch! (v/standard-registry) (ec/target opts) ops)]
        (is (:ok r) (pr-str r))
        (is (= "\"*hive:live*\"" (first (get-in r [:ok :plan/results])))))
      (is (= "t"
             (ec/eval-elisp opts (str "(with-current-buffer \"*hive:live*\" (string= (buffer-substring-no-properties (point-min) (point-max)) "
                                      (elisp/string-literal (str (str/join "\n" (expected-lines)) "\n"))
                                      "))"))))
      (is (= "\"diff-added\""
             (ec/eval-elisp opts "(with-current-buffer \"*hive:live*\" (goto-char (point-min)) (search-forward \"+(new)\") (format \"%s\" (get-text-property (match-beginning 0) 'face)))")))
      (finally
        (try (ec/eval-elisp opts "(kill-emacs)") (catch Exception _ nil))
        (.waitFor daemon 10 TimeUnit/SECONDS)
        (.destroyForcibly daemon)))))

(defn- vim-script
  "A headless Vim session loading the bundled plugin, running CONNECT (an Ex
   line) and sleeping until hive sets g:hive_vessel_test_done."
  [connect & settings]
  (let [rtp (.getPath (io/file "resources/hive-vessel/vim"))]
    (doto (File/createTempFile "hive-vessel" ".vim") .deleteOnExit
      (spit (str "set nocompatible\n"
                 "let &rtp = " (wire/write-json rtp) " . ',' . &rtp\n"
                 "let g:hive_vessel_headless = 1\n"
                 "let g:hive_vessel_autoconnect = 0\n"
                 (str/join "" (map #(str % "\n") settings))
                 "runtime plugin/hive_vessel.vim\n"
                 connect "\n"
                 "let s:n = 0\n"
                 "while !get(g:, 'hive_vessel_test_done', 0) && s:n < 300\n"
                 "  sleep 50m\n"
                 "  let s:n += 1\n"
                 "endwhile\n"
                 "qall!\n")))))

(defn- start-vim! ^Process [^File script]
  (.start (ProcessBuilder. ["vim" "-N" "-u" "NONE" "-i" "NONE" "-n" "-es" "-S" (.getPath script)])))

(deftest ^:integration vim-connects-and-is-driven-over-its-channel
  (let [server (vc/start!)
        script (vim-script (str "HiveVesselConnect 127.0.0.1:" (:port server)))
        vim (start-vim! script)]
    (try
      (vc/await-vim! server 15000)
      (let [target (vc/target server)
            reg (v/standard-registry)
            r (v/dispatch! reg target ops)]
        (is (:ok r) (pr-str r))
        (testing "the reply to a call carries Vim's return value"
          (is (= (wire/write-json (expected-lines))
                 (vc/send-command! server ["call" "hive_vessel#panel_lines" ["live"]] 5000))))
        (testing "an addon's own Vim script is a dialect call away"
          (is (:ok (v/dispatch! reg target {:op :vim/ex :command "let g:hive_vessel_test_done = 1"})))))
      (is (.waitFor vim 20 TimeUnit/SECONDS) "vim exits once told it is done")
      (finally
        (vc/stop! server)
        (.destroyForcibly vim)))))

(deftest ^:integration vim-discovers-the-wire-and-is-driven-through-the-hub
  (let [dir (str (Files/createTempDirectory "hive-vessel-wire" (make-array java.nio.file.attribute.FileAttribute 0)))
        hello (promise)
        srv (server/start! {:editor "vim" :dir dir
                            :listener (fn [kind _sid msg] (when (= :hello kind) (deliver hello msg)))})
        script (vim-script "HiveVesselConnect"
                           (str "let g:hive_vessel_discovery_dir = " (wire/write-json dir)))
        vim (start-vim! script)]
    (try
      (is (map? (deref hello 15000 nil)) "Vim found the discovery file and said hello")
      (let [h (:hub srv)
            reg (v/standard-registry)
            target (executor/target srv)
            r (v/dispatch! reg target ops)]
        (is (:ok r) (pr-str r))
        (testing "the same session answers HiveOp calls"
          (is (= "vim" (get-in (transport/call! h "editor-status" {}) ["value" "editor"]))))
        (testing "a native reply carries Vim's value through the session's pending table"
          (is (= (expected-lines)
                 (get (hub/native! h ["call" "hive_vessel#panel_lines" ["live"]]) "value"))))
        (testing "HiveOp and natives interleave on one channel"
          (is (= ["live"] (get (hub/native! h ["expr" "keys(get(b:, 'hive_vessel_lines', {})) + ['live']"]) "value")))
          (is (= (count (expected-lines))
                 (get-in (transport/call! h "editor-eval" {"code" "len(hive_vessel#panel_lines('live'))"}) ["value"]))))
        (is (:ok (v/dispatch! reg target {:op :vim/ex :command "let g:hive_vessel_test_done = 1"}))))
      (is (.waitFor vim 20 TimeUnit/SECONDS) "vim exits once told it is done")
      (finally
        (server/stop! srv)
        (.destroyForcibly vim)))))
