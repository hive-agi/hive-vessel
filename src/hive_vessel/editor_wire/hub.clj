(ns hive-vessel.editor-wire.hub
  "Session driver: holds every connected session of one editor kind, steps the
   pure session machine and interprets its effects against injected transports.
   Implements ICaller over the active session."
  (:require [hive-vessel.editor-wire.ops :as ops]
            [hive-vessel.editor-wire.session :as session]
            [hive-vessel.editor-wire.transport :as transport]))

;; SPDX-License-Identifier: MIT

(def ^:private reply-grace-ms 50)

(defn- advance!
  "Step session SID under the hub lock. Returns [effects transport call-id]."
  [{:keys [lock sessions]} sid input now]
  (locking lock
    (if-let [{:keys [state conn]} (get @sessions sid)]
      (let [{:keys [state effects call-id]} (session/step state input now)]
        (swap! sessions assoc-in [sid :state] state)
        [effects conn call-id])
      [[] nil nil])))

(defn- notify! [{:keys [listener]} kind sid msg]
  (when listener (listener kind sid msg)))

(defn- interpret!
  [{:keys [waiters active] :as hub} sid conn effects]
  (doseq [[kind a b] effects]
    (case kind
      :send (transport/send-frame! conn a)
      :resolve (when-let [p (get @waiters [sid a])]
                 (swap! waiters dissoc [sid a])
                 (deliver p b))
      :close (transport/close! conn)
      :hello (do (reset! active sid) (notify! hub :hello sid a))
      :event (do (when (= "focus" (get a "event")) (reset! active sid))
                 (notify! hub :event sid a)))))

(defn- drive!
  [hub sid input]
  (let [[effects conn call-id] (advance! hub sid input ((:now-fn hub)))]
    (interpret! hub sid conn effects)
    call-id))

(defn connect!
  "Register a new connection over CONN. Returns its session id."
  [{:keys [lock sessions id-fn token]} conn]
  (let [sid (id-fn)]
    (locking lock
      (swap! sessions assoc sid {:conn conn :state (session/init sid token)}))
    sid))

(defn receive!
  "Feed one JSON-parsed FRAME read from session SID."
  [hub sid frame]
  (drive! hub sid [:frame frame])
  nil)

(defn disconnect!
  "Session SID lost its connection: resolve its pending calls and forget it.
   No-op for an unknown SID."
  [{:keys [sessions active lock] :as hub} sid]
  (when (contains? @sessions sid)
    (drive! hub sid [:closed])
    (locking lock
      (swap! sessions dissoc sid)
      (swap! active (fn [a] (if (= a sid) nil a))))
    (notify! hub :disconnect sid nil))
  nil)

(defn active-session
  "Id of the session calls route to, or nil."
  [hub]
  @(:active hub))

(defn session-ids
  [hub]
  (vec (keys @(:sessions hub))))

(defn hello-of
  "The hello message of session SID, or nil."
  [hub sid]
  (get-in @(:sessions hub) [sid :state :hello]))

(defn close-all!
  "Close every connection and resolve every pending call."
  [hub]
  (doseq [sid (session-ids hub)]
    (when-let [conn (get-in @(:sessions hub) [sid :conn])]
      (transport/close! conn))
    (disconnect! hub sid)))

(defn- await-result
  [hub sid call-id p wait-ms]
  (let [v (deref p wait-ms ::pending)]
    (if (not= ::pending v)
      v
      (do (drive! hub sid [:tick])
          (let [v (deref p 0 ::pending)]
            (swap! (:waiters hub) dissoc [sid call-id])
            (if (= ::pending v) (ops/err "wire/timeout") v))))))

(defrecord Hub [lock sessions active waiters token id-fn now-fn listener]
  transport/ICaller
  (call! [this op params]
    (if-let [sid @active]
      (let [p (promise)
            [effects conn call-id] (locking lock
                                     (let [[effects conn call-id]
                                           (advance! this sid [:call op params] (now-fn))]
                                       (when call-id (swap! waiters assoc [sid call-id] p))
                                       [effects conn call-id]))]
        (interpret! this sid conn effects)
        (await-result this sid call-id p (+ (ops/timeout-ms op params) reply-grace-ms)))
      (ops/err "wire/not-connected"))))

(defn hub
  "A hub authenticating sessions by :token. Optional :listener
   (fn [kind session-id msg]) with kind in :hello :event :disconnect,
   :id-fn and :now-fn."
  [{:keys [token listener id-fn now-fn]}]
  (map->Hub {:lock (Object.)
             :sessions (atom {})
             :active (atom nil)
             :waiters (atom {})
             :token token
             :id-fn (or id-fn #(str (random-uuid)))
             :now-fn (or now-fn #(System/currentTimeMillis))
             :listener listener}))
