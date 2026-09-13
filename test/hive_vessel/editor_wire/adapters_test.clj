(ns hive-vessel.editor-wire.adapters-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.terminal :as term]
            [hive-addon.vessel :as vessel]
            [hive-vessel.editor-wire.codec :as codec]
            [hive-vessel.editor-wire.fake-editor :as fake]
            [hive-vessel.editor-wire.hub :as hub]
            [hive-vessel.editor-wire.ops :as ops]
            [hive-vessel.editor-wire.port :as port]
            [hive-vessel.editor-wire.terminal :as terminal]
            [hive-vessel.editor-wire.transport :as transport]
            [hive-vessel.editor-wire.vessel :as rv]
            [hive-spi.editor.ports :as ports]
            [hive-spi.editor.registry :as registry]
            [hive-vessel.editor-wire.executor :as executor]
            [hive-vessel.core :as v]))

;; SPDX-License-Identifier: MIT

(def token "feedfacefeedfacefeedfacefeedface")

(defn- new-hub
  ([] (new-hub nil))
  ([listener] (hub/hub {:token token :listener listener})))

(defn- echo [op params] (ops/ok {"op" op "params" params}))

(deftest calls-need-an-authenticated-session
  (let [h (new-hub)]
    (is (= "wire/not-connected" (ops/error-code (transport/call! h "editor-status" {}))))
    (fake/connect! h echo {:token "wrong"})
    (is (= "wire/not-connected" (ops/error-code (transport/call! h "editor-status" {}))))
    (is (empty? (hub/session-ids h)) "a denied session is not kept once its reader disconnects"
        )))

(deftest round-trip-through-the-hub
  (let [h (new-hub)
        f (fake/connect! h echo)]
    (is (= (ops/ok {"op" "find-file" "params" {"file" "/tmp/x"}})
           (transport/call! h "find-file" {"file" "/tmp/x"})))
    (is (= [["find-file" {"file" "/tmp/x"}]] (fake/calls f)))
    (is (= "wire/unknown-op" (ops/error-code (transport/call! h "rm-rf" {}))))))

(deftest a-silent-editor-times-out-within-the-bounded-wait
  (let [h (new-hub)
        _ (fake/connect! h (fn [_ _] ::fake/silent))
        t0 (System/currentTimeMillis)
        r (transport/call! h "editor-eval" {"code" "1" "timeout_ms" 100})
        dt (- (System/currentTimeMillis) t0)]
    (is (= "wire/timeout" (ops/error-code r)))
    (is (< dt 1000))))

(deftest disconnect-resolves-pending-calls
  (let [h (new-hub)
        f (fake/connect! h (fn [_ _] ::fake/silent))
        p (future (transport/call! h "editor-status" {}))]
    (Thread/sleep 50)
    (hub/disconnect! h (fake/sid f))
    (is (= "wire/closed" (ops/error-code (deref p 2000 :hung))))))

(deftest focus-selects-the-active-session
  (let [seen (atom [])
        h (new-hub (fn [kind sid msg] (swap! seen conj [kind sid (get msg "event")])))
        a (fake/connect! h (fn [_ _] (ops/ok "a")))
        b (fake/connect! h (fn [_ _] (ops/ok "b")))]
    (is (= (fake/sid b) (hub/active-session h)) "the latest hello wins")
    (is (= "b" (get (transport/call! h "editor-status" {}) "value")))
    (hub/receive! h (fake/sid a) [2 (codec/event "focus")])
    (is (= "a" (get (transport/call! h "editor-status" {}) "value")))
    (is (= [[:hello (fake/sid a) nil] [:hello (fake/sid b) nil] [:event (fake/sid a) "focus"]]
           @seen))))

(def notify {:op :ui/notify :message "hi" :level :warn})

(deftest the-hub-executes-vim-channel-natives-on-the-active-session
  (let [h (new-hub)
        f (fake/connect! h echo {:native (fn [[_ f args]] (str f ":" (count args)))})
        r (v/dispatch! (v/standard-registry) (executor/target h)
                       [notify {:op :vim/ex :command "let g:done = 1"}])]
    (testing "a replying command comes back with Vim's raw value, a silent one with nil"
      (is (= ["hive_vessel#notify:2" nil] (get-in r [:ok :plan/results])) (pr-str r)))
    (testing "the wire carried the dialect's own payloads, the call with an id"
      (is (= [[:native ["call" "hive_vessel#notify" ["hi" "warn"]]]
              [:native ["ex" "let g:done = 1"]]]
             (fake/calls f)))
      (is (some #(and (= :server-native (codec/classify %)) (neg? (codec/frame-id %))) @(:sent f))))
    (testing "HiveOp calls and natives share one correlation space"
      (is (= (ops/ok {"op" "editor-status" "params" {}}) (transport/call! h "editor-status" {})))
      (is (= "hive_vessel#panel_lines:1"
             (get (hub/native! h ["call" "hive_vessel#panel_lines" ["live"]]) "value"))))))

(deftest a-wire-failure-is-a-loud-dispatch-failure
  (let [h (new-hub)]
    (testing "no session"
      (let [err (:error (v/dispatch! (v/standard-registry) (executor/target h) notify))]
        (is (= :execute-threw (:failure/reason err)))
        (is (re-find #"wire/not-connected" (get-in err [:failure/detail :message])))))
    (testing "a silent Vim times out within the target's bound and reports the batch position"
      (fake/connect! h echo {:native (fn [_] ::fake/silent)})
      (let [t0 (System/currentTimeMillis)
            err (:error (v/dispatch! (v/standard-registry) (executor/target h :timeout-ms 100)
                                     [{:op :vim/ex :command "echo 1"} notify notify]))]
        (is (= :execute-threw (:failure/reason err)))
        (is (= 1 (get-in err [:failure/detail :completed])))
        (is (re-find #"wire/timeout" (get-in err [:failure/detail :message])))
        (is (< (- (System/currentTimeMillis) t0) 2000))))
    (testing "another dialect is refused before touching the wire"
      (is (thrown? clojure.lang.ExceptionInfo
                   ((executor/executor h) {:op :vessel/native :native/dialect :elisp :native/payload "(x)"}))))))

(deftest the-executor-accepts-a-server-map
  (let [h (new-hub)]
    (fake/connect! h echo {:native (fn [_] 7)})
    (is (= 7 ((executor/executor {:hub h}) {:op :vessel/native :native/dialect :vim-channel
                                            :native/payload ["call" "f" []]})))
    (is (= #{:vessel/id :vessel/dialect :vessel/execute! :vessel/features}
           (set (keys (executor/target {:hub h} :features #{:x/y})))))))

(defn- method-fn [protocol method]
  (let [v (resolve (symbol (str (:ns (meta (:var protocol))))
                           (name method)))]
    @v))

(deftest every-port-method-forwards-its-own-op
  (let [h (new-hub)
        f (fake/connect! h echo)
        p (port/remote-editor-port h)]
    (is (satisfies? ports/IEditorPort p))
    (is (registry/supports? p :buffer))
    (doseq [protocol [ports/IEditorPort ports/IEditorBufferPort]
            method (keys (:sigs protocol))]
      (let [resp ((method-fn protocol method) p {:k 1})]
        (is (false? (:isError resp)) (name method))
        (is (= {"op" (ops/method->op method) "params" {"k" 1}}
               (json/read-str (-> resp :content first :text)))
            (name method))))
    (is (= 16 (count (fake/calls f))))))

(deftest port-failures-are-mcp-errors-not-throws
  (let [h (new-hub)
        _ (fake/connect! h (fn [_ _] (ops/err "op/failed" "E492")))
        resp (ports/editor-eval (port/remote-editor-port h) {"code" "bad("})]
    (is (true? (:isError resp)))
    (is (= {"code" "op/failed" "message" "E492"}
           (json/read-str (-> resp :content first :text))))))

(defn- terminal-editor [state]
  (fn [op params]
    (let [id (get params "id")]
      (case op
        "terminal-spawn" (if (= "boom" id)
                           (ops/err "op/failed" "E1")
                           (do (swap! state assoc id {:status "running" :typed []})
                               (ops/ok {"terminal" 7})))
        "terminal-dispatch" (do (swap! state update-in [id :typed] conj (get params "text"))
                                (ops/ok nil))
        "terminal-status" (if-let [t (get @state id)]
                            (ops/ok {"status" (:status t)})
                            (ops/err "op/failed"))
        "terminal-read" (ops/ok {"lines" (get-in @state [id :typed])})
        "terminal-kill" (if (get @state id)
                          (do (swap! state dissoc id) (ops/ok nil))
                          (ops/err "op/failed"))
        "terminal-interrupt" (ops/ok nil)
        (ops/err "op/unsupported")))))

(deftest remote-terminal-contract
  (let [h (new-hub)
        state (atom {})
        f (fake/connect! h (terminal-editor state))
        t (terminal/remote-terminal h :vim ["claude"])
        ctx {:id "ling-1" :cwd "/work" :project-id "hive"}]
    (is (term/terminal-addon? t))
    (is (= :vim (term/terminal-id t)))
    (is (= "ling-1" (term/terminal-spawn! t ctx {})))
    (is (= ["terminal-spawn" {"id" "ling-1" "cmd" ["claude"] "cwd" "/work"}] (first (fake/calls f))))
    (is (true? (term/terminal-dispatch! t ctx {:task "hello"})))
    (is (= ["hello"] (terminal/read-lines t "ling-1")))
    (is (= {:slave/id "ling-1" :slave/status :running} (term/terminal-status t ctx nil)))
    (is (= {:success? true :ling-id "ling-1"} (term/terminal-interrupt! t ctx)))
    (is (= {:cwd "/work" :project-id "hive"} (terminal/context-of t "ling-1")))
    (is (= {:killed? true :id "ling-1"} (term/terminal-kill! t ctx)))
    (is (nil? (terminal/context-of t "ling-1")))
    (is (= {:killed? false :id "ling-1" :reason :op/failed} (term/terminal-kill! t ctx)))
    (is (nil? (term/terminal-status t ctx nil)))
    (testing "a failed spawn throws ex-info carrying the wire error"
      (let [e (try (term/terminal-spawn! t {:id "boom"} {}) nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= {:id "boom" :error "op/failed"} (select-keys (ex-data e) [:id :error])))))))

(deftest remote-vessel-contract
  (let [h (new-hub)
        _ (fake/connect! h (terminal-editor (atom {})))
        p (port/remote-editor-port h)
        t (terminal/remote-terminal h :vim ["claude"])
        v (rv/remote-vessel :vim p t)]
    (is (vessel/vessel? v))
    (is (= :vim (vessel/vessel-id v)))
    (is (= #{:editor :terminal} (vessel/capabilities v)))
    (is (identical? p (vessel/addon v :editor)))
    (is (identical? t (vessel/addon v :terminal)))
    (is (nil? (vessel/addon v :grid)))
    (is (nil? (vessel/resolve-context v "ling-9")))
    (term/terminal-spawn! t {:id "ling-9" :cwd "/w"} {})
    (is (= {:cwd "/w"} (vessel/resolve-context v "ling-9")))))
