(ns hive-vessel.executor.tmux-integration-test
  "The tmux executor against a real, private tmux server: a panel's pane
   shows exactly the rendered lines, a re-show repaints, a close removes the
   window, keys reach a shell pane. The subject IS tmux, hence ^:integration."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-vessel.core :as v]
            [hive-vessel.doc :as d]
            [hive-vessel.executor.tmux :as tmux])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; SPDX-License-Identifier: MIT

(def panel-doc
  (d/doc "Carto \"Flow\" #7"
         (d/para "apply write-form" :info)
         (d/diff "-(old)\n+(new ü)")
         (d/link "src/a.clj" "src/a.clj" 3)))

(def other-doc (d/doc "Again" (d/para "repaint")))

(defn- expected-lines [doc] (mapv :text (d/render-lines doc)))

(defn- pane-lines
  "What the pane of WID shows, trailing blank lines dropped."
  [opts wid]
  (let [{:keys [exit out]} (tmux/run-tmux! opts ["capture-pane" "-p" "-t" wid])]
    (is (zero? exit))
    (->> (str/split-lines out) reverse (drop-while str/blank?) reverse vec)))

(deftest ^:integration a-panel-paints-exactly-the-rendered-lines
  (let [socket (str "hive-vessel-" (System/nanoTime))
        opts {:socket-name socket :session "hive" :editor "true"
              :dir (str (Files/createTempDirectory "hive-vessel-tmux" (make-array FileAttribute 0)))}
        eventually (fn [pred] (loop [i 0] (cond (pred) true (< i 50) (do (Thread/sleep 100) (recur (inc i))) :else false)))]
    ;; A plain sh in the first window: the user's login shell may take seconds
    ;; to print a prompt, or print none a capture can see.
    (is (zero? (:exit (tmux/run-tmux! opts ["new-session" "-d" "-s" "hive" "-x" "120" "-y" "40" "sh"]))))
    (try
      (let [target (tmux/target opts)
            reg (v/standard-registry)
            show (fn [doc] (first (get-in (v/dispatch! reg target {:op :ui/show-panel :panel/id "olympus/tab-2" :doc doc})
                                          [:ok :plan/results])))
            wid (show panel-doc)]
        (is (re-matches #"@\d+" wid))
        (is (eventually #(= (expected-lines panel-doc) (pane-lines opts wid))))
        (is (= (expected-lines panel-doc) (pane-lines opts wid)))
        (testing "a second show repaints the same window"
          (is (= wid (show other-doc)))
          (is (eventually #(= (expected-lines other-doc) (pane-lines opts wid))))
          (is (= (expected-lines other-doc) (pane-lines opts wid))))
        (testing "close removes the window"
          (is (= [wid] (get-in (v/dispatch! reg target {:op :ui/close-panel :panel/id "olympus/tab-2"})
                               [:ok :plan/results])))
          (is (eventually #(not (str/includes? (:out (tmux/run-tmux! opts ["list-windows" "-t" "hive" "-F" "#{window_id}"])) wid)))))
        (testing "keys reach the shell pane and a notify is accepted"
          (let [pane (str/trim (:out (tmux/run-tmux! opts ["list-panes" "-t" "hive" "-F" "#{pane_id}"])))
                r (v/dispatch! reg target [{:op :ui/send-to-terminal :terminal pane :text "echo tmux-vessel-ok\n"}
                                           {:op :ui/notify :message "frame 7 applied"}])]
            (is (:ok r) (pr-str r))
            (is (eventually #(some (fn [l] (= "tmux-vessel-ok" (str/trim l))) (pane-lines opts pane)))
                (pr-str (pane-lines opts pane)))))
        (testing "open-file lands in a new window"
          (let [r (v/dispatch! reg target {:op :ui/open-file :file "/tmp/a.clj" :line 3})]
            (is (re-matches #"@\d+" (first (get-in r [:ok :plan/results])))))))
      (finally
        (tmux/run-tmux! opts ["kill-server"])))))