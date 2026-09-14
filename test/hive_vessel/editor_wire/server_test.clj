(ns hive-vessel.editor-wire.server-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-vessel.editor-wire.codec :as codec]
            [hive-vessel.editor-wire.hub :as hub]
            [hive-vessel.editor-wire.ops :as ops]
            [hive-vessel.editor-wire.server :as server]
            [hive-vessel.editor-wire.transport :as transport])
  (:import (java.io BufferedReader BufferedWriter)
           (java.net Socket)
           (java.nio.file Files LinkOption)
           (java.nio.file.attribute PosixFilePermissions)))

;; SPDX-License-Identifier: MIT

(defn- temp-dir []
  (str (Files/createTempDirectory "hive-editor-wire-test" (make-array java.nio.file.attribute.FileAttribute 0))
       "/discovery"))

(defn- client [port]
  (let [s (Socket. "127.0.0.1" (int port))]
    (.setSoTimeout s 3000)
    {:socket s
     :in (BufferedReader. (io/reader (.getInputStream s) :encoding "UTF-8"))
     :out (BufferedWriter. (io/writer (.getOutputStream s) :encoding "UTF-8"))}))

(defn- send-line! [{:keys [^BufferedWriter out]} frame]
  (.write out ^String (json/write-str frame))
  (.write out "\n")
  (.flush out))

(defn- read-frame [{:keys [^BufferedReader in]}]
  (some-> (.readLine in) json/read-str))

(defn- await-sessions [h n]
  (loop [i 0]
    (when (and (< i 100) (not= n (count (hub/session-ids h))))
      (Thread/sleep 20)
      (recur (inc i)))))

(defmacro with-server [[sym opts] & body]
  `(let [~sym (server/start! ~opts)]
     (try ~@body (finally (server/stop! ~sym)))))

(deftest discovery-file-is-private-and-removed-on-stop
  (let [dir (temp-dir)]
    (with-server [srv {:editor "vim" :dir dir}]
      (let [path (.toPath (io/file (:discovery srv)))
            doc (json/read-str (slurp (:discovery srv)))]
        (is (= (server/discovery-path dir "vim") (:discovery srv)))
        (is (= {"wire" 1 "editor" "vim" "port" (:port srv) "token" (:token srv)}
               (dissoc doc "pid")))
        (is (re-matches #"[0-9a-f]{32}" (:token srv)))
        (is (= "rw-------" (PosixFilePermissions/toString
                            (Files/getPosixFilePermissions path (make-array LinkOption 0)))))
        (is (= "rwx------" (PosixFilePermissions/toString
                            (Files/getPosixFilePermissions (.getParent path) (make-array LinkOption 0)))))))
    (is (not (.exists (io/file (server/discovery-path dir "vim")))))))

(deftest a-real-socket-client-round-trips-a-call
  (with-server [srv {:editor "vim" :dir (temp-dir)}]
    (let [c (client (:port srv))]
      (send-line! c [1 (codec/hello {:token (:token srv) :editor "vim"})])
      ;; Read the reply BEFORE asking the hub for the session id: the accept
      ;; thread registers the session, and only the hello reply proves it has.
      (let [reply (read-frame c)
            sid (first (hub/session-ids (:hub srv)))]
        (is (some? sid))
        (is (= [1 (ops/ok {"session" sid "wire" 1})] reply)))
      (let [result (future (transport/call! (:hub srv) "insert-text" {"text" "héllo\n"}))
            call (read-frame c)]
        (is (= :server-call (codec/classify call)))
        (is (= ["insert-text" {"text" "héllo\n"}] (nth call 2)))
        (send-line! c [(codec/frame-id call) (ops/ok true)])
        (is (= (ops/ok true) (deref result 3000 :hung))))
      (testing "events are acknowledged"
        (send-line! c [2 (codec/event "focus")])
        (is (= [2 (ops/ok nil)] (read-frame c))))
      (testing "a malformed line from a ready session is ignored"
        (let [^BufferedWriter out (:out c)]
          (.write out "{not json\n")
          (.flush out))
        (send-line! c [3 (codec/event "focus")])
        (is (= [3 (ops/ok nil)] (read-frame c))))
      (.close ^Socket (:socket c))
      (await-sessions (:hub srv) 0)
      (is (empty? (hub/session-ids (:hub srv))))
      (is (= "wire/not-connected" (ops/error-code (transport/call! (:hub srv) "editor-status" {})))))))

(deftest a-wrong-token-is-denied-and-disconnected
  (with-server [srv {:editor "vim" :dir (temp-dir)}]
    (let [c (client (:port srv))]
      (send-line! c [1 (codec/hello {:token "00000000000000000000000000000000" :editor "vim"})])
      (is (= [1 (ops/err "auth/denied")] (read-frame c)))
      (is (nil? (read-frame c)) "the server closed the socket")
      (await-sessions (:hub srv) 0)
      (is (empty? (hub/session-ids (:hub srv)))))))

(deftest stop-closes-connected-clients
  (let [srv (server/start! {:editor "vim" :dir (temp-dir)})
        c (client (:port srv))]
    (send-line! c [1 (codec/hello {:token (:token srv) :editor "vim"})])
    (read-frame c)
    (server/stop! srv)
    (is (nil? (try (read-frame c) (catch java.net.SocketException _ nil))))))
