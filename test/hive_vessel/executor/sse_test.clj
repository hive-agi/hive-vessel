(ns hive-vessel.executor.sse-test
  "The SSE bridge over real sockets: an SSE reader on java.net.http, CORS
   preflights, origin and token refusal, retention replay, replies, and the
   executor driven through dispatch!."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-vessel.core :as v]
            [hive-vessel.executor.sse :as sse])
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

(deftest wrong-methods-are-refused
  (is (= 405 (.statusCode (request *bridge* "POST" "/vessel/health" {} "x"))))
  (is (= 405 (.statusCode (request *bridge* "GET" "/vessel/reply")))))

(deftest health-reports-clients-and-panels
  (sse/broadcast! *bridge* {"op" "ui/show-panel" "panel/id" "z"})
  (is (= "{\"ok\":true,\"clients\":0,\"panels\":[\"z\"],\"seq\":1}"
         (.body (request *bridge* "GET" "/vessel/health")))))
