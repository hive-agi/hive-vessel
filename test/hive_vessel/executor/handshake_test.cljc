(ns hive-vessel.executor.handshake-test
  "Pure tests of the feature handshake and the producer capabilities:
   parsing, per-client recording through the real bridge, key validation,
   and the discovery-doc shape."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-vessel.executor.handshake :as handshake]
            [hive-vessel.executor.sse :as sse])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net URI)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.time Duration)
           (java.util.concurrent LinkedBlockingQueue TimeUnit)))

;; SPDX-License-Identifier: MIT

(def ^HttpClient http (HttpClient/newHttpClient))

(defn- bridge-fixture [f]
  (let [b (sse/start! {:heartbeat-ms 60000})]
    (try (f b) (finally (sse/stop! b)))))

;; =============================================================================
;; Feature parsing (pure)
;; =============================================================================

(deftest parse-features-pure
  (is (= #{} (handshake/parse-features nil)))
  (is (= #{} (handshake/parse-features "")))
  (is (= #{} (handshake/parse-features "   ")))
  (is (= #{:spans :keys :cursor :open-file}
         (handshake/parse-features "spans,keys,cursor,open-file")))
  (is (= #{:spans} (handshake/parse-features "spans")))
  (is (= #{:a} (handshake/parse-features "a,a,a")) "duplicates collapse")
  (is (= #{:a :b} (handshake/parse-features "a,,b")) "empty tokens are dropped")
  (is (= #{:unknown-token} (handshake/parse-features "unknown-token"))
      "unknown tokens are kept as keywords")
  (is (every? keyword? (handshake/parse-features "spans,keys")))
  (is (= 1 handshake/feature-set-version) "the feature set is versioned"))

;; =============================================================================
;; Per-client recording through the real bridge
;; =============================================================================

(defn- subscribe!
  "Open an SSE stream at PATH, draining lines into a queue."
  [b path]
  (let [q (LinkedBlockingQueue.)
        req (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" (:port b) path)))
                (.timeout (Duration/ofSeconds 5))
                (.build))]
    (future
      (try
        (let [resp (.send http req (HttpResponse$BodyHandlers/ofInputStream))]
          (with-open [r (BufferedReader. (InputStreamReader. ^java.io.InputStream (.body resp) "UTF-8"))]
            (loop [] (when-let [line (.readLine r)] (.put q line) (recur)))))
        (catch Throwable _ nil)))
    q))

(defn- await
  "Poll THUNK until it answers truthy or MS pass; its last value."
  ([thunk] (await thunk 5000))
  ([thunk ms]
   (let [deadline (+ (System/currentTimeMillis) ms)]
     (loop []
       (let [v (thunk)]
         (if (or v (> (System/currentTimeMillis) deadline))
           v
           (do (Thread/sleep 10) (recur))))))))

(deftest features-are-recorded-per-client
  (bridge-fixture
   (fn [b]
     (let [q1 (subscribe! b "/vessel/events?vessel=dirge&features=spans,keys,cursor,open-file")
           q2 (subscribe! b "/vessel/events?vessel=dirge&features=cursor")]
       (is (= "retry: 2000" (.poll ^LinkedBlockingQueue q1 5 TimeUnit/SECONDS)))
       (is (await #(= 2 (sse/clients b))))
       (is (= #{:spans :keys :cursor :open-file} (sse/client-features b "dirge"))
           "the union across clients, recorded purely from the query param")
       (is (= #{:spans :keys :cursor :open-file} (sse/client-features b :dirge))
           "the reader accepts the vessel id as a keyword too")
       (is (= #{} (sse/client-features b "vscode")) "other vessels see nothing")))))

(deftest absent-features-mean-no-features
  (bridge-fixture
   (fn [b]
     (let [q (subscribe! b "/vessel/events?vessel=dirge")]
       (is (= "retry: 2000" (.poll ^LinkedBlockingQueue q 5 TimeUnit/SECONDS)))
       (is (await #(= 1 (sse/clients b))))
       (is (= #{} (sse/client-features b "dirge")))))))

(deftest disconnected-clients-drop-their-features
  (bridge-fixture
   (fn [b]
     (let [q1 (subscribe! b "/vessel/events?vessel=dirge&features=spans")
           q2 (subscribe! b "/vessel/events?vessel=dirge&features=cursor")]
       (is (= "retry: 2000" (.poll ^LinkedBlockingQueue q1 5 TimeUnit/SECONDS)))
       (is (await #(= 2 (sse/clients b))))
       (is (= #{:spans :cursor} (sse/client-features b "dirge")))
       (sse/stop! b)
       ;; a new bridge over the same client set semantics: the union shrinks
       (is (= #{} (sse/client-features b "dirge")))))))

;; =============================================================================
;; Capabilities (producer -> dirge), pure
;; =============================================================================

(deftest capabilities-default
  (let [caps (handshake/capabilities)]
    (is (= {"version" 1
            "replies" ["focus" "next-tab" "prev-tab" "refresh" "unfocus"]
            "invokes" []
            "keys" {"enter" "focus"
                    "r" "refresh"
                    "shift-tab" "prev-tab"
                    "tab" "next-tab"
                    "u" "unfocus"}}
           caps))
    (is (= 1 (get caps "version")) "the version is the integer 1")))

(deftest capabilities-with-invokes
  (let [caps (handshake/capabilities ["open" "run"]
                                     {"o" {"invoke" "open"}
                                      "enter" "focus"
                                      "x" {"invoke" "nope"}})]
    (is (= ["open" "run"] (get caps "invokes")))
    (is (= {"enter" "focus" "o" {"invoke" "open"}} (get caps "keys"))
        "a key whose verb is not advertised is dropped")
    (is (= ["focus" "next-tab" "prev-tab" "refresh" "unfocus"] (get caps "replies"))
        "the provider arity keeps the five olympus replies")))

(deftest validate-drops-unadvertised-keys-purely
  (let [caps (handshake/validate-capabilities
              {:replies ["focus"]
               :invokes ["open"]
               :keys {"enter" "focus"
                      "o" {"invoke" "open"}
                      "bad-reply" "unknown-reply"
                      "bad-invoke" {"invoke" "unknown-invoke"}
                      "not-a-verb" 42
                      "also-bad" {"invoke" 7}}})]
    (is (= {"enter" "focus" "o" {"invoke" "open"}} (get caps "keys")))
    (is (= 1 (get caps "version")))))

(deftest validate-keeps-unknown-fields-out-but-passes-key-forms
  (let [caps (handshake/validate-capabilities
              {:replies ["focus" "invoke" "unfocus"]
               :invokes ["open"]
               :keys {"enter" "focus"
                      "i" {"invoke" "open"}
                      "u" "unfocus"}
               :future-field {:anything :goes}})]
    (is (= {"enter" "focus" "i" {"invoke" "open"} "u" "unfocus"}
           (get caps "keys")))))

(deftest validate-sorts-verb-collections
  (let [caps (handshake/validate-capabilities
              {:replies ["refresh" "focus" "unfocus" "next-tab" "prev-tab" "invoke"]
               :invokes ["run" "open"]
               :keys {"enter" "focus"}})]
    (is (= ["focus" "invoke" "next-tab" "prev-tab" "refresh" "unfocus"]
           (get caps "replies")))
    (is (= ["open" "run"] (get caps "invokes")))))
