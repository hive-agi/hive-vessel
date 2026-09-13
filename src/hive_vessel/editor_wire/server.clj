(ns hive-vessel.editor-wire.server
  "Boundary: loopback TCP server speaking hive editor wire v1 for one editor kind,
   plus its discovery file."
  (:require [clojure.java.io :as io]
            [hive-vessel.editor-wire.hub :as hub]
            [hive-vessel.editor-wire.ops :as ops]
            [hive-vessel.editor-wire.transport :as transport]
            [hive-vessel.wire :as wire])
  (:import (java.io BufferedReader BufferedWriter IOException)
           (java.net InetAddress ServerSocket Socket SocketException)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.security SecureRandom)))

;; SPDX-License-Identifier: MIT

(defn discovery-dir
  "Directory holding discovery files, from XDG-RUNTIME-DIR (may be nil)."
  [xdg-runtime-dir]
  (str (or xdg-runtime-dir "/tmp") "/hive-editor-wire"))

(defn discovery-path
  [dir editor]
  (str dir "/" editor ".json"))

(defn discovery-doc
  "Discovery file contents."
  [{:keys [editor port token pid]}]
  {"wire" ops/wire-version "editor" editor "port" port "token" token "pid" pid})

(defn new-token
  "32 hex chars from a SecureRandom."
  []
  (let [bytes (byte-array 16)]
    (.nextBytes (SecureRandom.) bytes)
    (apply str (map #(format "%02x" (bit-and % 0xff)) bytes))))

(defn- ->path ^Path [s] (.toPath (io/file s)))

(defn- write-private!
  [path-str content]
  (let [target (->path path-str)
        dir (.getParent target)
        perms (fn [s] (into-array FileAttribute [(PosixFilePermissions/asFileAttribute
                                                   (PosixFilePermissions/fromString s))]))]
    (when-not (Files/exists dir (make-array LinkOption 0))
      (Files/createDirectories dir (perms "rwx------")))
    (let [tmp (Files/createTempFile dir ".discovery" ".tmp" (perms "rw-------"))]
      (Files/writeString tmp content StandardCharsets/UTF_8 (make-array java.nio.file.OpenOption 0))
      (Files/move tmp target (into-array java.nio.file.CopyOption
                                         [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING])))))

(defrecord SocketTransport [^Socket socket ^BufferedWriter writer]
  transport/ITransport
  (send-frame! [_ frame]
    (locking writer
      (try
        (.write writer ^String (wire/write-json frame))
        (.write writer "\n")
        (.flush writer)
        (catch IOException _ (.close socket))))
    nil)
  (close! [_]
    (try (.close socket) (catch IOException _ nil))
    nil))

(defn- parse-line
  [line]
  (try (wire/read-json line) (catch Exception _ ::invalid)))

(defn- daemon-thread!
  [name f]
  (doto (Thread. ^Runnable f ^String name)
    (.setDaemon true)
    (.start)))

(defn- serve-connection!
  [h ^Socket socket]
  (let [reader (BufferedReader. (io/reader (.getInputStream socket) :encoding "UTF-8"))
        writer (BufferedWriter. (io/writer (.getOutputStream socket) :encoding "UTF-8"))
        sid (hub/connect! h (->SocketTransport socket writer))]
    (daemon-thread!
     (str "hive-editor-wire-conn-" sid)
     (fn []
       (try
         (loop []
           (when-let [line (.readLine reader)]
             (hub/receive! h sid (parse-line line))
             (recur)))
         (catch IOException _ nil)
         (finally
           (try (.close socket) (catch IOException _ nil))
           (hub/disconnect! h sid)))))))

(defn- accept-loop!
  [h ^ServerSocket server-socket]
  (daemon-thread!
   "hive-editor-wire-accept"
   (fn []
     (loop []
       (when-let [socket (try (.accept server-socket)
                              (catch SocketException _ nil))]
         (.setTcpNoDelay socket true)
         (serve-connection! h socket)
         (recur))))))

(defn start!
  "Start a server for EDITOR (string). Options: :listener, :dir (discovery dir),
   :token. Returns a server map; :hub is the ICaller."
  [{:keys [editor listener dir token]}]
  (let [token (or token (new-token))
        server-socket (ServerSocket. 0 50 (InetAddress/getLoopbackAddress))
        port (.getLocalPort server-socket)
        dir (or dir (discovery-dir (System/getenv "XDG_RUNTIME_DIR")))
        path (discovery-path dir editor)
        h (hub/hub {:token token :listener listener})]
    (write-private! path (wire/write-json (discovery-doc {:editor editor :port port :token token
                                                          :pid (.pid (java.lang.ProcessHandle/current))})))
    (accept-loop! h server-socket)
    {:editor editor :port port :token token :discovery path
     :hub h :server-socket server-socket}))

(defn stop!
  "Stop SERVER: no new connections, every session closed, discovery file removed."
  [{:keys [^ServerSocket server-socket hub discovery]}]
  (try (.close server-socket) (catch IOException _ nil))
  (hub/close-all! hub)
  (Files/deleteIfExists (->path discovery))
  nil)
