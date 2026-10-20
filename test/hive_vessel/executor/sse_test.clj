(ns hive-vessel.executor.sse-test
  "The SSE bridge over real sockets: an SSE reader on java.net.http, CORS
   preflights, origin and token refusal, retention replay, replies, and the
   executor driven through dispatch!."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-vessel.core :as v]
            [hive-vessel.executor.sse :as sse]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-vessel.dialect.json :as json])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.time Duration)
           (java.util.concurrent LinkedBlockingQueue TimeUnit)))

;; SPDX-License-Identifier: MIT

(def ^:dynamic *bridge* nil)

(defn- with-bridge [opts f]
  (let [b (sse/start! (merge {:heartbeat-ms 60000} opts))]
    (try (binding [*bridge* b] (f)) (finally (sse/stop! b)))))

(use-fixtures :each (fn [f] (with-bridge {} f)))

(def ^HttpClient http (HttpClient/newHttpClient))

(defn- url [b path] (str "http://127.0.0.1:" (:port b) path))

(defn- request
  ([b method path] (request b method path {} nil))
  ([b method path headers body]
   (let [builder (-> (HttpRequest/newBuilder (URI/create (url b path)))
                     (.timeout (Duration/ofSeconds 5)))]
     (doseq [[k v] headers] (.header builder k v))
     (.method builder method (if body
                               (HttpRequest$BodyPublishers/ofString body)
                               (HttpRequest$BodyPublishers/noBody)))
     (.send http (.build builder) (HttpResponse$BodyHandlers/ofString)))))

(defn- subscribe!
  "Open an SSE stream; returns a queue receiving [:status n] then each
   [:data payload]."
  ([b] (subscribe! b "/vessel/events" {}))
  ([b path headers]
   (let [q (LinkedBlockingQueue.)
         builder (HttpRequest/newBuilder (URI/create (url b path)))]
     (doseq [[k v] headers] (.header builder k v))
     (future
       (try
         (let [resp (.send http (.build builder) (HttpResponse$BodyHandlers/ofInputStream))]
           (.put q [:status (.statusCode resp)])
           (with-open [r (BufferedReader. (InputStreamReader. ^java.io.InputStream (.body resp) "UTF-8"))]
             (loop []
               (when-let [line (.readLine r)]
                 (when (str/starts-with? line "data: ")
                   (.put q [:data (subs line 6)]))
                 (recur)))))
         (catch Throwable _ nil)))
     q)))

(defn- take! [^LinkedBlockingQueue q] (.poll q 5 TimeUnit/SECONDS))

(defn- await-clients [b n]
  (loop [i 0]
    (when (and (< i 100) (not= n (sse/clients b)))
      (Thread/sleep 20)
      (recur (inc i)))))

(deftest sse-frames-are-single-line-json
  (is (= "id: 3\nevent: vessel\ndata: {\"op\":\"ui/notify\",\"message\":\"a\\nb\"}\n\n"
         (sse/sse-frame 3 {"op" "ui/notify" "message" "a\nb"}))))

(deftest retention-follows-show-and-close
  (is (= {"p" {"op" "ui/show-panel" "panel/id" "p"}}
         (sse/retain {} {"op" "ui/show-panel" "panel/id" "p"})))
  (is (= {} (sse/retain {"p" {}} {"op" "ui/close-panel" "panel/id" "p"})))
  (is (= {"p" {}} (sse/retain {"p" {}} {"op" "ui/notify"}))))

(defn retain-sample
  "Retention after one MESSAGE, starting from panel \"q\" already retained."
  [message]
  (sse/retain {"q" {"op" "ui/show-panel" "panel/id" "q"}} message))

(deftrifecta retention-reads-both-vocabularies
  hive-vessel.executor.sse-test/retain-sample
  {:golden-path "test/golden/executor/sse-retain.edn"
   :cases {:legacy-show {"op" "ui/show-panel" "panel/id" "p"}
           :neutral-show {"op" "show" "id" "p"}
           :legacy-close {"op" "ui/close-panel" "panel/id" "q"}
           :neutral-close {"op" "close" "id" "q"}
           :other {"op" "notify" "id" "q"}}
   :gen (gen/hash-map "op" (gen/elements ["ui/show-panel" "show" "ui/close-panel" "close" "notify"])
                      "id" (gen/elements ["p" "q"]))
   :property-type :totality
   :mutations [["legacy-only" (fn [message]
                                (let [r {"q" {"op" "ui/show-panel" "panel/id" "q"}}]
                                  (case (get message "op")
                                    "ui/show-panel" (assoc r (get message "panel/id") message)
                                    "ui/close-panel" (dissoc r (get message "panel/id"))
                                    r)))]]})

(deftest loopback-origins-only-by-default
  (is (sse/loopback-origin? "http://127.0.0.1:3080"))
  (is (sse/loopback-origin? "http://localhost:5173"))
  (is (not (sse/loopback-origin? "https://evil.example")))
  (is (not (sse/loopback-origin? "http://127.0.0.1.evil.example")))
  (is (not (sse/loopback-origin? nil))))

(deftest a-connected-browser-receives-broadcasts
  (let [q (subscribe! *bridge* "/vessel/events" {"Origin" "http://127.0.0.1:3080"})]
    (is (= [:status 200] (take! q)))
    (await-clients *bridge* 1)
    (is (= {:delivered 1 :seq 1} (sse/broadcast! *bridge* {:op :ui/notify :message "hi"})))
    (is (= [:data "{\"op\":\"ui/notify\",\"message\":\"hi\"}"] (take! q)))))

(deftest a-non-browser-client-needs-no-origin
  (let [q (subscribe! *bridge*)]
    (is (= [:status 200] (take! q)))
    (await-clients *bridge* 1)
    (sse/broadcast! *bridge* {:op :ui/notify :message "node"})
    (is (= [:data "{\"op\":\"ui/notify\",\"message\":\"node\"}"] (take! q)))))

(deftest late-joiners-get-retained-panels-and-not-closed-ones
  (sse/broadcast! *bridge* {"op" "ui/show-panel" "panel/id" "a" "lines" []})
  (sse/broadcast! *bridge* {"op" "ui/show-panel" "panel/id" "b" "lines" []})
  (sse/broadcast! *bridge* {"op" "ui/close-panel" "panel/id" "a"})
  (sse/broadcast! *bridge* {"op" "ui/notify" "message" "lost"})
  (is (= #{"b"} (sse/retained-panels *bridge*)))
  (let [q (subscribe! *bridge*)]
    (is (= [:status 200] (take! q)))
    (is (= [:data "{\"op\":\"ui/show-panel\",\"panel/id\":\"b\",\"lines\":[]}"] (take! q)))
    (is (nil? (.poll ^LinkedBlockingQueue q 300 TimeUnit/MILLISECONDS)))))

(defn v2-client-sample
  "MESSAGE as a client subscribed with features=spans receives it."
  [message]
  (sse/client-message message {:features #{:spans}}))

(deftrifecta client-message-neutralizes-for-v2-clients
  hive-vessel.executor.sse-test/v2-client-sample
  {:golden-path "test/golden/executor/sse-client-message.edn"
   :cases {:legacy-show {"op" "ui/show-panel" "panel/id" "p" "lines" []
                         "doc" {"doc/title" "T"}}
           :neutral-show {"op" "show" "id" "p" "lines" []}
           :legacy-close {"op" "ui/close-panel" "panel/id" "p"}
           :legacy-notify {"op" "ui/notify" "message" "m"}
           :custom {"op" "loop/followup" "id" "x"}}
   :gen (gen/hash-map "op" (gen/elements ["ui/show-panel" "show" "ui/close-panel"
                                          "close" "ui/notify" "loop/followup"])
                      "panel/id" (gen/elements ["p" "q"]))
   :property-type :totality
   :mutations [["identity" (fn [message] message)]
               ["ops-only" (fn [message]
                             (update message "op" #(get {"ui/show-panel" "show"
                                                         "ui/close-panel" "close"} % %)))]]})

(deftest client-message-is-the-dialect-projection
  (doseq [message [{"op" "ui/show-panel" "panel/id" "p" "lines" []}
                   {"op" "ui/close-panel" "panel/id" "p"}
                   {"op" "show" "id" "p"}]
          features [#{} #{:spans} #{:spans :keys :cursor :open-file :loop}]]
    (is (= (json/neutralize message {:vessel/features features})
           (sse/client-message message {:features features})))
    (is (= (sse/client-message message {:features features})
           (sse/client-message (sse/client-message message {:features features})
                               {:features features}))
        "idempotent: a replayed neutral payload stays neutral"))
  (let [legacy {"op" "ui/show-panel" "panel/id" "p"}]
    (is (identical? legacy (sse/client-message legacy {:features #{}})))
    (is (identical? legacy (sse/client-message legacy {})))))

(deftest replayed-and-live-panels-are-shaped-alike-per-features
  (let [panel {"op" "ui/show-panel" "panel/id" "a" "lines" []}
        live-v2 (subscribe! *bridge* "/vessel/events?features=spans,keys,cursor,open-file,loop" {})
        live-v1 (subscribe! *bridge*)]
    (is (= [:status 200] (take! live-v2)))
    (is (= [:status 200] (take! live-v1)))
    (await-clients *bridge* 2)
    (sse/broadcast! *bridge* panel)
    (let [live-v2-frame (take! live-v2)
          live-v1-frame (take! live-v1)
          late-v2 (subscribe! *bridge* "/vessel/events?features=spans,keys,cursor,open-file,loop" {})
          late-v1 (subscribe! *bridge*)]
      (is (= [:status 200] (take! late-v2)))
      (is (= [:status 200] (take! late-v1)))
      (is (= [:data "{\"op\":\"show\",\"id\":\"a\",\"lines\":[]}"] live-v2-frame))
      (is (= live-v2-frame (take! late-v2)) "replay = live for a features client")
      (is (= [:data "{\"op\":\"ui/show-panel\",\"panel/id\":\"a\",\"lines\":[]}"] live-v1-frame))
      (is (= live-v1-frame (take! late-v1)) "replay = live for a featureless client"))))

(deftest the-executor-carries-dispatched-ops
  (let [q (subscribe! *bridge*)
        target {:vessel/id :web :vessel/dialect :json :vessel/execute! (sse/executor *bridge*)}]
    (is (= [:status 200] (take! q)))
    (await-clients *bridge* 1)
    (let [r (v/dispatch! (v/standard-registry) target {:op :ui/close-panel :panel/id "x"})]
      (is (= [{:delivered 1 :seq 1}] (get-in r [:ok :plan/results]))))
    (is (= [:data "{\"panel/id\":\"x\",\"op\":\"ui/close-panel\"}"] (take! q)))
    (is (thrown? clojure.lang.ExceptionInfo
                 ((sse/executor *bridge*) {:op :vessel/native :native/dialect :elisp :native/payload "x"})))))

(deftest cors-preflight-and-headers
  (let [r (request *bridge* "OPTIONS" "/vessel/reply" {"Origin" "http://127.0.0.1:3080"
                                                        "Access-Control-Request-Method" "POST"} nil)]
    (is (= 204 (.statusCode r)))
    (is (= "http://127.0.0.1:3080" (.orElse (.firstValue (.headers r) "access-control-allow-origin") nil)))))

(deftest foreign-origins-are-refused
  (is (= 403 (.statusCode (request *bridge* "GET" "/vessel/health" {"Origin" "https://evil.example"} nil)))))

(deftest replies-land-in-the-inbox-and-reach-the-callback
  (let [seen (promise)]
    (with-bridge {:on-message #(deliver seen %)}
      (fn []
        (let [r (request *bridge* "POST" "/vessel/reply" {"Origin" "http://localhost:3080"} "{\"op\":\"hello\"}")]
          (is (= 204 (.statusCode r)))
          (is (= ["{\"op\":\"hello\"}"] (sse/inbox *bridge*)))
          (is (= "{\"op\":\"hello\"}" (deref seen 1000 nil))))))))

(deftest a-token-is-required-when-configured
  (with-bridge {:token "s3cret"}
    (fn []
      (is (= 401 (.statusCode (request *bridge* "GET" "/vessel/health"))))
      (is (= 200 (.statusCode (request *bridge* "GET" "/vessel/health?token=s3cret")))))))

(deftest a-wrong-token-of-the-same-length-is-refused
  (with-bridge {:token "s3cret"}
    (fn []
      (is (= 401 (.statusCode (request *bridge* "GET" "/vessel/health?token=s3cre7"))))
      (is (= 401 (.statusCode (request *bridge* "GET" "/vessel/health?token=S3CRET"))))
      (is (= 401 (.statusCode (request *bridge* "GET" "/vessel/health?token="))))
      (is (= 200 (.statusCode (request *bridge* "GET" "/vessel/health?token=s3cret")))))))

(defspec token-matches-exactly-its-own-value 200
  (prop/for-all [expected (gen/not-empty gen/string)
                 other gen/string]
    (and (true? (sse/token-matches? expected expected))
         (= (= expected other) (sse/token-matches? expected other))
         (false? (sse/token-matches? expected nil))
         (false? (sse/token-matches? expected (str expected "x"))))))

(deftest wrong-methods-are-refused
  (is (= 405 (.statusCode (request *bridge* "POST" "/vessel/health" {} "x"))))
  (is (= 405 (.statusCode (request *bridge* "GET" "/vessel/reply")))))

(deftest health-reports-clients-and-panels
  (sse/broadcast! *bridge* {"op" "ui/show-panel" "panel/id" "z"})
  (is (= "{\"ok\":true,\"clients\":0,\"panels\":[\"z\"],\"seq\":1}"
         (.body (request *bridge* "GET" "/vessel/health")))))
