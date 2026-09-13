(ns hive-vessel.executor.sse
  "Executor for the :json dialect over Server-Sent Events: one loopback HTTP
   bridge any :json vessel can consume -- a DeepSeek Harness page, a VS Code
   extension host, a web harness. JDK HttpServer only, no dependency.

   Routes:
     GET  /vessel/events   SSE stream (`event: vessel`, one JSON line per op);
                           replays retained panels on connect
     POST /vessel/reply    one JSON message from the client
     GET  /vessel/health   JSON status

   Admission: a request carrying an Origin header (a browser) must come from
   an allowed origin; every request must carry `token` when the bridge was
   started with one. Non-browser clients send no Origin and are gated by the
   token alone.

   Retention: the latest `ui/show-panel` per panel id is replayed to each new
   connection and dropped on `ui/close-panel`, so a client that connects after
   hive presented something still shows it."
  (:require [clojure.string :as str]
            [hive-vessel.wire :as wire])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.io IOException OutputStream)
           (java.net InetAddress InetSocketAddress URLDecoder)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent ExecutorService Executors ScheduledExecutorService TimeUnit)))

;; SPDX-License-Identifier: MIT

(def ^:private loopback-origin
  #"https?://(127\.0\.0\.1|localhost|\[::1\])(:\d+)?")

(defn loopback-origin?
  "Default origin policy: any page served from this machine's loopback."
  [origin]
  (boolean (and origin (re-matches loopback-origin origin))))

;; =============================================================================
;; Pure helpers
;; =============================================================================

(defn sse-frame
  "One SSE event carrying MESSAGE (JSON-able data) under sequence number SEQ.
   write-json never emits a raw newline, so the data fits on one line."
  [seq message]
  (str "id: " seq "\n"
       "event: vessel\n"
       "data: " (wire/write-json message) "\n\n"))

(defn retain
  "RETAINED (panel id -> message) after MESSAGE passes."
  [retained message]
  (case (get message "op")
    "ui/show-panel" (assoc retained (get message "panel/id") message)
    "ui/close-panel" (dissoc retained (get message "panel/id"))
    retained))

(defn query-params [^String query]
  (if (str/blank? query)
    {}
    (into {}
          (keep (fn [pair]
                  (let [[k v] (str/split pair #"=" 2)]
                    (when-not (str/blank? k)
                      [(URLDecoder/decode k "UTF-8") (URLDecoder/decode (or v "") "UTF-8")]))))
          (str/split query #"&"))))

;; =============================================================================
;; HTTP plumbing
;; =============================================================================

(defn- header [^HttpExchange ex name] (.getFirst (.getRequestHeaders ex) name))

(defn- cors! [^HttpExchange ex origin]
  (when origin
    (doto (.getResponseHeaders ex)
      (.set "Access-Control-Allow-Origin" origin)
      (.set "Vary" "Origin")
      (.set "Access-Control-Allow-Methods" "GET, POST, OPTIONS")
      (.set "Access-Control-Allow-Headers" "Content-Type"))))

(defn- respond! [^HttpExchange ex status ^String content-type ^String body]
  (let [bytes (.getBytes body StandardCharsets/UTF_8)]
    (.set (.getResponseHeaders ex) "Content-Type" content-type)
    (.sendResponseHeaders ex status (if (zero? (alength bytes)) -1 (alength bytes)))
    (when (pos? (alength bytes))
      (with-open [out (.getResponseBody ex)] (.write out bytes)))
    (.close ex)))

(defn- write! [{:keys [^OutputStream out lock]} ^String text]
  (locking lock
    (.write out (.getBytes text StandardCharsets/UTF_8))
    (.flush out)))

(defn- admitted?
  "nil when EX may proceed, else the refusal status."
  [{:keys [allowed-origin? token]} ^HttpExchange ex]
  (let [origin (header ex "Origin")
        params (query-params (.getRawQuery (.getRequestURI ex)))]
    (cond
      (and origin (not (allowed-origin? origin))) 403
      (and token (not= token (get params "token"))) 401
      :else nil)))

(defn- drop-client! [state id]
  (when-let [{:keys [^HttpExchange exchange]} (get-in @state [:clients id])]
    (swap! state update :clients dissoc id)
    (try (.close exchange) (catch Throwable _ nil))))

(defn- handle-events [{:keys [state] :as bridge} ^HttpExchange ex]
  (let [origin (header ex "Origin")
        id (str (random-uuid))
        client {:id id :exchange ex :out (.getResponseBody ex) :lock (Object.)
                :origin origin}]
    (cors! ex origin)
    (doto (.getResponseHeaders ex)
      (.set "Content-Type" "text/event-stream; charset=utf-8")
      (.set "Cache-Control" "no-cache")
      (.set "X-Accel-Buffering" "no"))
    (.sendResponseHeaders ex 200 0)
    ;; Register and replay under the state lock, so a concurrent broadcast
    ;; lands either in the replay or on the live stream, never neither.
    (locking state
      (try
        (write! client "retry: 2000\n\n")
        (doseq [message (vals (:retained @state))]
          (write! client (sse-frame (:seq @state) message)))
        (swap! state assoc-in [:clients id] client)
        (catch IOException _ (.close ex))))
    (when-let [f (:on-connect bridge)] (f {:client/id id :client/origin origin}))))

(defn- handle-reply [{:keys [state on-message]} ^HttpExchange ex]
  (let [origin (header ex "Origin")
        body (slurp (.getRequestBody ex) :encoding "UTF-8")]
    (cors! ex origin)
    (swap! state update :inbox (fn [inbox] (vec (take-last 100 (conj (or inbox []) body)))))
    (when on-message (on-message body))
    (respond! ex 204 "text/plain" "")))

(defn- handle-health [{:keys [state]} ^HttpExchange ex]
  (cors! ex (header ex "Origin"))
  (let [{:keys [clients retained seq]} @state]
    (respond! ex 200 "application/json"
              (wire/write-json {"ok" true
                                "clients" (count clients)
                                "panels" (vec (sort (keys retained)))
                                "seq" seq}))))

(defn- handler [bridge f]
  (reify HttpHandler
    (handle [_ ex]
      (try
        (if-let [status (admitted? bridge ex)]
          (respond! ex status "text/plain" (if (= 403 status) "origin not allowed" "bad token"))
          (if (= "OPTIONS" (.getRequestMethod ex))
            (do (cors! ex (header ex "Origin")) (respond! ex 204 "text/plain" ""))
            (f bridge ex)))
        (catch Throwable t
          (try (respond! ex 500 "text/plain" (str (ex-message t))) (catch Throwable _ nil)))))))

(defn- method-guard [method f]
  (fn [bridge ^HttpExchange ex]
    (if (= method (.getRequestMethod ex))
      (f bridge ex)
      (respond! ex 405 "text/plain" "method not allowed"))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn start!
  "Start a bridge. opts:
     :port (default 0 = any free port), :host (default loopback),
     :token, :allowed-origin? (fn [origin] bool, default loopback pages),
     :on-message (fn [raw-json-string]), :on-connect (fn [client-info]),
     :heartbeat-ms (default 15000).
   Returns the bridge map; `(:port bridge)` is the bound port."
  ([] (start! {}))
  ([{:keys [port host token allowed-origin? on-message on-connect heartbeat-ms]
     :or {port 0 heartbeat-ms 15000}}]
   (let [addr (if host
                (InetSocketAddress. ^String host (int port))
                (InetSocketAddress. (InetAddress/getLoopbackAddress) (int port)))
         server (HttpServer/create addr 0)
         state (atom {:clients {} :retained {} :seq 0 :inbox []})
         bridge {:server server
                 :state state
                 :token token
                 :allowed-origin? (or allowed-origin? loopback-origin?)
                 :on-message on-message
                 :on-connect on-connect}
         pool (Executors/newCachedThreadPool)
         ^ScheduledExecutorService ticker (Executors/newSingleThreadScheduledExecutor)]
     (.createContext server "/vessel/events" (handler bridge (method-guard "GET" handle-events)))
     (.createContext server "/vessel/reply" (handler bridge (method-guard "POST" handle-reply)))
     (.createContext server "/vessel/health" (handler bridge (method-guard "GET" handle-health)))
     (.setExecutor server pool)
     (.start server)
     (.scheduleAtFixedRate ticker
                           (fn []
                             (doseq [[id client] (:clients @state)]
                               (try (write! client ": ping\n\n")
                                    (catch Throwable _ (drop-client! state id)))))
                           heartbeat-ms heartbeat-ms TimeUnit/MILLISECONDS)
     (assoc bridge
            :port (.getPort (.getAddress server))
            :pool pool
            :ticker ticker))))

(defn broadcast!
  "Send MESSAGE (JSON-able data, a :json-dialect payload) to every connected
   client and update retention. Returns {:delivered n :seq s}; a client whose
   stream fails is dropped."
  [{:keys [state]} message]
  (let [message (wire/->json-data message)]
    (locking state
      (let [{:keys [seq clients]} (swap! state (fn [s] (-> s
                                                           (update :seq inc)
                                                           (update :retained retain message))))
            frame (sse-frame seq message)
            delivered (reduce (fn [n [id client]]
                                (try (write! client frame) (inc n)
                                     (catch Throwable _ (drop-client! state id) n)))
                              0 clients)]
        {:delivered delivered :seq seq}))))

(defn executor
  "A :vessel/execute! fn broadcasting :json natives over BRIDGE."
  [bridge]
  (fn [{:native/keys [dialect payload]}]
    (when-not (= :json dialect)
      (throw (ex-info "sse executor runs :json only" {:dialect dialect})))
    (broadcast! bridge payload)))

(defn clients [{:keys [state]}] (count (:clients @state)))

(defn retained-panels [{:keys [state]}] (set (keys (:retained @state))))

(defn inbox [{:keys [state]}] (:inbox @state))

(defn stop!
  [{:keys [^HttpServer server state pool ticker]}]
  (doseq [id (keys (:clients @state))] (drop-client! state id))
  (.shutdownNow ^ScheduledExecutorService ticker)
  (.stop server 0)
  (.shutdownNow ^ExecutorService pool)
  nil)
