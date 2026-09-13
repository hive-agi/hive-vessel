(ns hive-vessel.executor.vim-channel
  "Executor for the :vim-channel dialect: a TCP server Vim connects to with
   `:HiveVesselConnect host:port` (a JSON-mode channel).

   Every native op is sent as a channel command with a negative request id;
   Vim evaluates it and answers `[id, result]`. The reply's result is returned
   as its raw JSON text -- callers that need structure decode it themselves."
  (:require [clojure.string :as str]
            [hive-vessel.wire :as wire])
  (:import (java.io BufferedReader InputStreamReader OutputStreamWriter PushbackReader)
           (java.net InetAddress ServerSocket Socket SocketTimeoutException)
           (java.nio.charset StandardCharsets)))

;; SPDX-License-Identifier: MIT

(defn- read-message
  "Read one top-level JSON array from READER (Vim does not newline-frame its
   messages). Returns the raw text, or nil at end of stream."
  [^PushbackReader reader]
  (let [sb (StringBuilder.)]
    (loop [depth 0 in-string? false escaped? false started? false]
      (let [c (.read reader)]
        (if (neg? c)
          nil
          (let [ch (char c)]
            (.append sb ch)
            (cond
              escaped? (recur depth in-string? false started?)
              (and in-string? (= ch \\)) (recur depth true true started?)
              (= ch \") (recur depth (not in-string?) false started?)
              in-string? (recur depth true false started?)
              (= ch \[) (recur (inc depth) false false true)
              (= ch \]) (if (= 1 depth)
                          (str/trim (str sb))
                          (recur (dec depth) false false started?))
              (and (not started?) (Character/isWhitespace ch)) (do (.setLength sb 0) (recur 0 false false false))
              :else (recur depth false false started?))))))))

(defn- reply-id+result [text]
  (when-let [[_ id result] (re-matches #"(?s)\[\s*(-?\d+)\s*,\s*(.*)\]" text)]
    [(parse-long id) (str/trim result)]))

(defn start!
  "Listen on PORT (0 picks a free one) at 127.0.0.1. Returns a server map;
   `(:port server)` is what Vim connects to."
  ([] (start! {}))
  ([{:keys [port] :or {port 0}}]
   (let [ss (ServerSocket. port 1 (InetAddress/getLoopbackAddress))]
     {:server-socket ss
      :port (.getLocalPort ss)
      :conn (atom nil)
      :next-id (atom 0)})))

(defn await-vim!
  "Block until Vim connects (or TIMEOUT-MS elapses). Returns the server."
  [{:keys [^ServerSocket server-socket conn] :as server} timeout-ms]
  (.setSoTimeout server-socket (int timeout-ms))
  (let [^Socket s (.accept server-socket)]
    (reset! conn {:socket s
                  :writer (OutputStreamWriter. (.getOutputStream s) StandardCharsets/UTF_8)
                  :reader (PushbackReader. (BufferedReader. (InputStreamReader. (.getInputStream s) StandardCharsets/UTF_8)))})
    server))

(defn connected? [server] (boolean @(:conn server)))

(def ^:private replying-commands
  "Channel commands Vim answers when given an id; ex, normal and redraw
   never reply."
  #{"call" "expr"})

(defn send-command!
  "Send channel PAYLOAD (a vector like [\"call\" fn args]). A call or expr
   gets a fresh negative id and waits for Vim's reply, returning the result's
   raw JSON text; any other command is fire-and-forget and returns nil."
  [{:keys [conn next-id]} payload timeout-ms]
  (let [{:keys [^Socket socket ^OutputStreamWriter writer reader]} (or @conn (throw (ex-info "vim not connected" {})))
        replies? (contains? replying-commands (first payload))
        id (when replies? (- (swap! next-id inc)))
        msg (wire/write-json (cond-> (vec payload) replies? (conj id)))]
    (locking socket
      (.setSoTimeout socket (int timeout-ms))
      (.write writer (str msg "\n"))
      (.flush writer)
      (when replies?
       (loop []
        (let [text (try (read-message reader)
                        (catch SocketTimeoutException _
                          (throw (ex-info "vim did not reply" {:id id :timeout-ms timeout-ms}))))]
          (when (nil? text) (throw (ex-info "vim channel closed" {:id id})))
          (let [[rid result] (reply-id+result text)]
            (if (= rid id) result (recur)))))))))

(defn executor
  "A :vessel/execute! fn sending :vim-channel native ops over SERVER."
  ([server] (executor server 10000))
  ([server timeout-ms]
   (fn [{:native/keys [dialect payload]}]
     (when-not (= :vim-channel dialect)
       (throw (ex-info "vim channel executor runs :vim-channel only" {:dialect dialect})))
     (send-command! server payload timeout-ms))))

(defn target
  [server & {:keys [features]}]
  (cond-> {:vessel/id :vim :vessel/dialect :vim-channel :vessel/execute! (executor server)}
    features (assoc :vessel/features features)))

(defn stop!
  [{:keys [^ServerSocket server-socket conn]}]
  (when-let [{:keys [^Socket socket]} @conn] (.close socket))
  (reset! conn nil)
  (.close server-socket))
