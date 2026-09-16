(ns hive-vessel.executor.tmux-test
  "The tmux executor against a recording tmux port: which argv each primitive
   produces, what the panel file holds, how a failing tmux surfaces through
   dispatch!, and that a panel id can never escape the panel directory."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.mutation :as mut]
            [hive-vessel.core :as v]
            [hive-vessel.doc :as d]
            [hive-vessel.executor.tmux :as tmux]
            [hive-vessel.render.ansi :as ansi])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; SPDX-License-Identifier: MIT

(defn- temp-dir []
  (str (Files/createTempDirectory "hive-vessel-tmux" (make-array FileAttribute 0))))

(defn- recording-port
  "A tmux port that records every argv and answers like tmux would: a
   new-window prints a fresh window id; FAILING verbs exit 1."
  [failing]
  (let [log (atom [])
        n (atom 0)]
    [(fn [argv]
       (swap! log conj argv)
       (cond
         (contains? failing (first argv)) {:exit 1 :out "" :err (str "can't " (first argv))}
         (= "new-window" (first argv)) {:exit 0 :out (str "@" (swap! n inc) "\n") :err ""}
         :else {:exit 0 :out "" :err ""}))
     log]))

(defn- verbs [log] (mapv first @log))

(def doc-a (d/doc "Carto Flow #7" (d/para "apply write-form" :info) (d/link "src/a.clj" "src/a.clj" 3)))
(def doc-b (d/doc "Again" (d/para "repaint")))

(deftest a-panel-is-a-window-that-is-respawned-and-then-killed
  (let [dir (temp-dir)
        [run! log] (recording-port #{})
        target (tmux/target {:run! run! :dir dir :session "s"})
        reg (v/standard-registry)]
    (testing "first show creates the window and writes the file"
      (let [r (v/dispatch! reg target {:op :ui/show-panel :panel/id "olympus/tab-2" :doc doc-a})]
        (is (= ["@1"] (get-in r [:ok :plan/results])))
        (is (= ["new-window" "-d" "-t" "s" "-n" "hive:olympus/tab-2" "-P" "-F" "#{window_id}"]
               (butlast (last @log))))
        (is (str/starts-with? (last (last @log)) (str "cat '" dir "/olympus_tab-2.txt'")))
        (is (= (str (str/join "\n" (map :text (d/render-lines doc-a))) "\n")
               (slurp (io/file dir "olympus_tab-2.txt"))))
        (is (= {"olympus/tab-2" "@1"} (tmux/panel-windows (:vessel/execute! target))))))
    (testing "a second show respawns the same window with the new lines"
      (v/dispatch! reg target {:op :ui/show-panel :panel/id "olympus/tab-2" :doc doc-b})
      (is (= ["respawn-window" "-k" "-t" "@1"] (butlast (last @log))))
      (is (= (str (str/join "\n" (map :text (d/render-lines doc-b))) "\n")
             (slurp (io/file dir "olympus_tab-2.txt")))))
    (testing "close kills it and forgets it"
      (is (= ["@1"] (get-in (v/dispatch! reg target {:op :ui/close-panel :panel/id "olympus/tab-2"})
                            [:ok :plan/results])))
      (is (= ["kill-window" "-t" "@1"] (last @log)))
      (is (= {} (tmux/panel-windows (:vessel/execute! target))))
      (is (= [nil] (get-in (v/dispatch! reg target {:op :ui/close-panel :panel/id "olympus/tab-2"})
                           [:ok :plan/results]))
          "closing a panel that is not open is a no-op")
      (is (= ["new-window" "respawn-window" "kill-window"] (verbs log))))))

(deftest a-vanished-window-is-recreated-instead-of-respawned
  (let [[run! log] (recording-port #{"respawn-window"})
        target (tmux/target {:run! run! :dir (temp-dir)})
        reg (v/standard-registry)]
    (v/dispatch! reg target {:op :ui/show-panel :panel/id "p" :doc doc-a})
    (is (= ["@2"] (get-in (v/dispatch! reg target {:op :ui/show-panel :panel/id "p" :doc doc-b})
                          [:ok :plan/results])))
    (is (= ["new-window" "respawn-window" "new-window"] (verbs log)))
    (is (= {"p" "@2"} (tmux/panel-windows (:vessel/execute! target))))))

(deftest the-other-primitives-map-to-tmux-commands
  (let [[run! log] (recording-port #{})
        target (tmux/target {:run! run! :dir (temp-dir) :session "hive" :editor "nvim"})
        reg (v/standard-registry)]
    (is (:ok (v/dispatch! reg target [{:op :ui/notify :message "frame 7\napplied" :level :warn}
                                      {:op :ui/send-to-terminal :terminal "%3" :text "ls -la\n"}
                                      {:op :ui/send-to-terminal :terminal "%3" :text "partial"}
                                      {:op :ui/send-to-terminal :terminal "%3" :text "\n"}
                                      {:op :ui/open-file :file "/tmp/a b.clj" :line 3 :column 7}])))
    (is (= [["display-message" "-t" "hive" "[warn] frame 7 | [warn] applied"]
            ["send-keys" "-t" "%3" "-l" "ls -la"]
            ["send-keys" "-t" "%3" "Enter"]
            ["send-keys" "-t" "%3" "-l" "partial"]
            ["send-keys" "-t" "%3" "Enter"]
            ["new-window" "-t" "hive" "-P" "-F" "#{window_id}" "nvim +3 '/tmp/a b.clj'"]]
           @log))))

(deftest a-failing-tmux-stops-the-batch-loudly
  (let [[run! _] (recording-port #{"display-message"})
        target (tmux/target {:run! run! :dir (temp-dir)})
        r (v/dispatch! (v/standard-registry) target
                       [{:op :ui/show-panel :panel/id "p" :doc doc-a}
                        {:op :ui/notify :message "x"}
                        {:op :ui/close-panel :panel/id "p"}])]
    (is (= :execute-threw (get-in r [:error :failure/reason])))
    (is (= 1 (get-in r [:error :failure/detail :completed])))
    (is (str/includes? (get-in r [:error :failure/detail :message]) "display-message"))))

(deftest only-text-natives-are-accepted
  (is (thrown? clojure.lang.ExceptionInfo
               ((tmux/executor {:run! (fn [_] {:exit 0 :out "" :err ""})})
                {:op :vessel/native :native/dialect :elisp :native/payload "x"}))))

(defspec a-panel-id-never-escapes-the-panel-directory 300
  (prop/for-all [id (gen/not-empty gen/string)]
    (let [n (tmux/safe-name id)]
      (and (re-matches #"[A-Za-z0-9._-]+" n)
           (not (re-matches #"\.*" n))))))

(mut/deftest-mutations safe-name-neutralises-path-tricks
  hive-vessel.executor.tmux/safe-name
  [["identity" identity]
   ["slashes-only" (fn [s] (str/replace s "/" "_"))]
   ["dots-kept" (fn [s] (str/replace s #"[^A-Za-z0-9._-]" "_"))]]
  (fn []
    (is (= "_.." (tmux/safe-name "..")))
    (is (= ".._x" (tmux/safe-name "../x")))
    (is (= "olympus_tab-2" (tmux/safe-name "olympus/tab-2")))
    (is (= "_" (tmux/safe-name "/")))
    (is (= "a_b" (tmux/safe-name "a\\b")))))

(deftest shell-words-and-commands
  (is (= "'a b'" (tmux/sh-quote "a b")))
  (is (= "'it'\\''s'" (tmux/sh-quote "it's")))
  (is (= "cat '/p/x.txt'; exec sleep infinity" (tmux/viewer-command "/p/x.txt")))
  (is (= "vi +3 '/tmp/a b.clj'" (tmux/editor-command "vi" {:file "/tmp/a b.clj" :line 3})))
  (is (= "vi '/tmp/a.clj'" (tmux/editor-command "vi" {:file "/tmp/a.clj"}))))

(deftest colour-is-opt-in-and-reaches-the-pane-through-the-real-plan
  ;; Faces must survive the whole path: doc -> :text dialect -> :text/face-lines
  ;; -> executor -> the file the pane cats. Painting costs the model nothing
  ;; because the model never reads this file.
  (let [reg (v/standard-registry)
        pane (fn [colour?]
               (let [dir (temp-dir)
                     [run! _] (recording-port #{})
                     target (tmux/target {:run! run! :dir dir :session "s" :colour? colour?})]
                 (v/dispatch! reg target {:op :ui/show-panel :panel/id "flow" :doc doc-a})
                 (slurp (io/file dir "flow.txt"))))
        rendered (d/render-lines doc-a)
        plain (str (str/join "\n" (map :text rendered)) "\n")]
    (testing "the default executor is byte-identical to the uncoloured one"
      (is (= plain (pane false)))
      (is (zero? (ansi/span-count (pane false)))))
    (testing "colour? paints, and strips back to exactly the same text"
      (let [painted (pane true)]
        (is (= plain (ansi/strip-ansi painted)))
        ;; expected span count comes from the doc's own faces through the
        ;; palette, never from a literal that could drift with either
        (is (= (count (remove #(nil? (get ansi/face-sgr (:face %))) rendered))
               (ansi/span-count painted)))))))