(ns hive-vessel.executor.emacsclient
  "Executor for the :elisp dialect through `emacsclient --eval`.

   A host with its own Emacs bridge (hive-emacs's eval port) injects that
   instead; this one needs nothing but an Emacs server on the socket."
  (:require [clojure.string :as str])
  (:import (java.util.concurrent TimeUnit)))

;; SPDX-License-Identifier: MIT

(defn eval-elisp
  "Evaluate CODE in the Emacs server. Returns the printed result; throws
   ex-info when emacsclient fails or times out.
   opts: :socket-name, :emacsclient (binary, default \"emacsclient\"),
   :timeout-ms (default 10000)."
  [{:keys [socket-name emacsclient timeout-ms] :or {emacsclient "emacsclient" timeout-ms 10000}} code]
  (let [cmd (cond-> [emacsclient]
              socket-name (conj (str "--socket-name=" socket-name))
              true (conj "--eval" code))
        p (.start (ProcessBuilder. ^java.util.List cmd))
        out (future (slurp (.getInputStream p)))
        err (future (slurp (.getErrorStream p)))]
    (when-not (.waitFor p timeout-ms TimeUnit/MILLISECONDS)
      (.destroyForcibly p)
      (throw (ex-info "emacsclient timed out" {:timeout-ms timeout-ms})))
    (if (zero? (.exitValue p))
      (str/trim-newline @out)
      (throw (ex-info "emacsclient failed" {:exit (.exitValue p) :stderr @err})))))

(defn executor
  "A :vessel/execute! fn evaluating :elisp native ops via emacsclient."
  ([] (executor {}))
  ([opts]
   (fn [{:native/keys [dialect payload]}]
     (when-not (= :elisp dialect)
       (throw (ex-info "emacsclient executor runs :elisp only" {:dialect dialect})))
     (eval-elisp opts payload))))

(defn target
  "An Emacs target descriptor executing through emacsclient."
  ([] (target {}))
  ([opts]
   (cond-> {:vessel/id :emacs :vessel/dialect :elisp :vessel/execute! (executor opts)}
     (:features opts) (assoc :vessel/features (:features opts)))))
