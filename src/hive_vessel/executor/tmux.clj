(ns hive-vessel.executor.tmux
  "Executor for the :text dialect on a tmux server, through the tmux CLI (no
   dependency, like the emacsclient executor).

   Panels are windows. A show-panel payload writes its lines to a file under
   a private directory and (re)spawns a window named `hive:<panel>` that
   cats the file and sleeps, so the pane shows exactly the rendered lines;
   close-panel kills that window; send-to-terminal sends the keys literally
   to the named pane; a notify goes to display-message; open-file opens the
   editor at the line in a new window. Windows are addressed by the id tmux
   returned (`@N`), never by index or name.

   tmux is reached through an injected `:run!` port, `(fn [argv] {:exit n
   :out s :err s})`, so the executor is testable without a server; the
   default runs `tmux [-L socket] argv...`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-vessel.render.ansi :as ansi])
  (:import (java.io File)
           (java.util.concurrent TimeUnit)))

;; SPDX-License-Identifier: MIT

(defn run-tmux!
  "Run `tmux ARGS` (on SOCKET-NAME's server when given). Returns
   {:exit n :out s :err s}; a non-zero exit is reported, never thrown."
  [{:keys [tmux socket-name timeout-ms] :or {tmux "tmux" timeout-ms 10000}} args]
  (let [cmd (into [tmux] (concat (when socket-name ["-L" socket-name]) args))
        p (.start (ProcessBuilder. ^java.util.List (vec cmd)))
        out (future (slurp (.getInputStream p)))
        err (future (slurp (.getErrorStream p)))]
    (if (.waitFor p timeout-ms TimeUnit/MILLISECONDS)
      {:exit (.exitValue p) :out @out :err @err}
      (do (.destroyForcibly p)
          {:exit -1 :out "" :err (str "tmux timed out after " timeout-ms " ms")}))))

;; =============================================================================
;; Pure helpers
;; =============================================================================

(defn safe-name
  "PANEL-ID as one file-name segment: every character outside [A-Za-z0-9._-]
   becomes `_`, and a name that is only dots gets a leading `_`, so an id can
   never address a path outside the panel directory."
  [panel-id]
  (let [s (str/replace panel-id #"[^A-Za-z0-9._-]" "_")]
    (if (re-matches #"\.*" s) (str "_" s) s)))

(defn sh-quote
  "S as one POSIX shell word."
  [s]
  (str "'" (str/replace s "'" "'\\''") "'"))

(defn viewer-command
  "The shell command a panel window runs: show the file, then stay open."
  [path]
  (str "cat " (sh-quote path) "; exec sleep infinity"))

(defn editor-command
  "The shell command an open-file window runs."
  [editor {:keys [file line]}]
  (str editor (when line (str " +" line)) " " (sh-quote file)))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn- ok!
  [{:keys [exit err] :as r} what]
  (if (zero? exit)
    r
    (throw (ex-info (str "tmux " what " failed: " (str/trim (or err ""))) r))))

(defn- panel-file ^File [{:keys [dir]} panel-id]
  (let [d (io/file dir)]
    (.mkdirs d)
    (io/file d (str (safe-name panel-id) ".txt"))))

(defn- show-panel! [{:keys [run! session state] :as ex} panel-id lines]
  (let [f (panel-file ex panel-id)
        _ (spit f (str (str/join "\n" lines) "\n"))
        cmd (viewer-command (.getPath f))
        wid (get-in @state [:windows panel-id])]
    (if (and wid (zero? (:exit (run! ["respawn-window" "-k" "-t" wid cmd]))))
      wid
      (let [{:keys [out]} (ok! (run! ["new-window" "-d" "-t" session "-n" (str "hive:" panel-id)
                                      "-P" "-F" "#{window_id}" cmd])
                               "new-window")
            wid (str/trim out)]
        (swap! state assoc-in [:windows panel-id] wid)
        wid))))

(defn- close-panel! [{:keys [run! state]} panel-id]
  (when-let [wid (get-in @state [:windows panel-id])]
    (swap! state update :windows dissoc panel-id)
    (run! ["kill-window" "-t" wid])
    wid))

(defn- notify! [{:keys [run! session]} lines]
  (ok! (run! ["display-message" "-t" session (str/join " | " lines)]) "display-message")
  true)

(defn- send-keys!
  "TEXT to the pane TERMINAL, literally; one trailing newline is sent as the
   Enter key, the tmux idiom that submits a line whatever the pane's line
   discipline binds LF to."
  [{:keys [run!]} terminal text]
  (let [submit? (str/ends-with? text "\n")
        body (if submit? (subs text 0 (dec (count text))) text)]
    (when (seq body)
      (ok! (run! ["send-keys" "-t" terminal "-l" body]) "send-keys"))
    (when submit?
      (ok! (run! ["send-keys" "-t" terminal "Enter"]) "send-keys"))
    true))

(defn- open-file! [{:keys [run! session editor]} location]
  (-> (run! ["new-window" "-t" session "-P" "-F" "#{window_id}" (editor-command editor location)])
      (ok! "new-window")
      :out
      str/trim))

(defn execute-text!
  "Run one :text PAYLOAD on the executor context EX. Returns the window id
   a panel or editor landed in, or true. A panel's lines are painted from
   :text/face-lines when EX paints and the payload carries them."
  [ex {:text/keys [panel close? terminal keys lines open face-lines] :as payload}]
  (cond
    close? (close-panel! ex panel)
    panel (show-panel! ex panel (if (and (:colour? ex) face-lines)
                                  (:text/lines (ansi/colourize payload true))
                                  lines))
    terminal (send-keys! ex terminal (or keys (str/join "\n" lines)))
    open (open-file! ex open)
    :else (notify! ex lines)))

(defn executor
  "A :vessel/execute! fn for :text natives on a tmux server. opts:
     :session     tmux session panels open in (default \"hive\")
     :socket-name tmux -L socket (default: the user's server)
     :tmux        binary (default \"tmux\")
     :dir         panel files (default <tmpdir>/hive-vessel-tmux)
     :editor      command for open-file (default $EDITOR, else vi)
     :colour?     paint panel lines from :text/face-lines (default false)
     :run!        the tmux port (default run-tmux! over the binary)
   The returned fn carries its window map under ::state in its metadata."
  ([] (executor {}))
  ([{:keys [session dir editor run! colour?] :as opts}]
   (let [state (atom {:windows {}})
         ex {:session (or session "hive")
             :dir (or dir (str (System/getProperty "java.io.tmpdir") "/hive-vessel-tmux"))
             :editor (or editor (System/getenv "EDITOR") "vi")
             :colour? (boolean colour?)
             :run! (or run! #(run-tmux! opts %))
             :state state}]
     (with-meta
       (fn [{:native/keys [dialect payload]}]
         (when-not (= :text dialect)
           (throw (ex-info "tmux executor runs :text only" {:dialect dialect})))
         (execute-text! ex payload))
       {::state state}))))

(defn panel-windows
  "{panel-id window-id} of the panels EXECUTOR has open."
  [executor]
  (:windows @(::state (meta executor))))

(defn target
  "A tmux target descriptor executing :text natives through the tmux CLI."
  ([] (target {}))
  ([opts]
   (cond-> {:vessel/id :tmux :vessel/dialect :text :vessel/execute! (executor opts)}
     (:features opts) (assoc :vessel/features (:features opts)))))