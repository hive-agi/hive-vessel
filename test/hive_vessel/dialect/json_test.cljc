(ns hive-vessel.dialect.json-test
  "The :json dialect's lowering fns: schema-synthesized from the primitive
   each lowers, related to the op by field round-trip, and a hand mutant for
   the dropped optional field, killed through the real plan."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.mutation :as mut]
            [hive-vessel.dialect.json :as json]
            [hive-vessel.doc :as d]
            [hive-vessel.schema :as s]
            [hive-vessel.core :as v]
            [hive-vessel.test-util :as tu]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(defn- fields-round-trip?
  "Every field of OP reaches MESSAGE under its string key, :op included."
  [op message]
  (and (= (wire/key-name (:op op)) (get message "op"))
       (every? (fn [[k v]] (= (wire/->json-data v) (get message (wire/key-name k))))
               (dissoc op :op))))

(hst/deftrifecta-from-schema notify-message
  hive-vessel.dialect.json/notify-message
  {:in s/Notify :out json/Payload :rel fields-round-trip? :mutation true :num-tests 100})

(hst/deftrifecta-from-schema show-panel-message
  hive-vessel.dialect.json/show-panel-message
  {:in s/ShowPanel
   :out [:and json/Payload [:map ["lines" [:vector [:map ["text" :string] ["face" :string]]]]]]
   :rel (fn [op message]
          (and (fields-round-trip? op message)
               (= (wire/->json-data (d/render-lines (:doc op))) (get message "lines"))))
   :mutation true
   :num-tests 60})

(hst/deftrifecta-from-schema close-panel-message
  hive-vessel.dialect.json/close-panel-message
  {:in s/ClosePanel :out json/Payload :rel fields-round-trip? :mutation true :num-tests 60})

(hst/deftrifecta-from-schema open-file-message
  hive-vessel.dialect.json/open-file-message
  {:in s/OpenFile :out json/Payload :rel fields-round-trip? :mutation true :num-tests 100})

(hst/deftrifecta-from-schema send-to-terminal-message
  hive-vessel.dialect.json/send-to-terminal-message
  {:in s/SendToTerminal :out json/Payload :rel fields-round-trip? :mutation true :num-tests 100})

(hst/deftrifecta-from-schema event-message
  hive-vessel.dialect.json/event-message
  {:in json/Event
   :out json/Payload
   ;; :data is :any, so it is compared as written JSON (a NaN is null on the
   ;; wire and compares equal to itself there).
   :rel (fn [{:keys [event data]} message]
          (and (= "json/event" (get message "op"))
               (= event (get message "event"))
               (= (wire/write-json data) (wire/write-json (get message "data")))))
   :mutation true
   :num-tests 100})

(defn- json-fields [op] (wire/->json-data (assoc (dissoc op :op) :op (:op op))))

(mut/deftest-mutations open-file-carries-every-optional-field
  hive-vessel.dialect.json/open-file-message
  [["drops-the-column" (fn [op] (dissoc (json-fields op) "column"))]
   ["drops-the-line" (fn [op] (dissoc (json-fields op) "line"))]]
  (fn []
    (is (= {"file" "a.clj" "line" 3 "column" 4 "op" "ui/open-file"}
           (tu/lowered-payload :vscode {:op :ui/open-file :file "a.clj" :line 3 :column 4})))
    (is (= {"file" "a.clj" "column" 4 "op" "ui/open-file"}
           (tu/lowered-payload :web {:op :ui/open-file :file "a.clj" :column 4})))))

(deftest a-translator-reads-its-lowering-through-the-var
  (mut/with-mutation [hive-vessel.dialect.json/open-file-message (fn [_] {"op" "mutant"})]
    (is (= {"op" "mutant"} (tu/lowered-payload :vscode {:op :ui/open-file :file "a.clj"}))))
  (is (= {"file" "a.clj" "op" "ui/open-file"} (tu/lowered-payload :vscode {:op :ui/open-file :file "a.clj"}))))
;; =============================================================================
;; Lens C5: neutral op names for v2 (features) clients
;; =============================================================================

(defn- features-target
  "The :json reference target plus the given feature set, as the C3 handshake
   fills it from the `features` subscribe param."
  [features]
  (assoc (get v/reference-targets :vscode) :vessel/features features))

(defn- legacy-target []
  (get v/reference-targets :web))

(def ^:private sample-doc
  {:doc/title "Lens C5" :doc/blocks [{:block/type :para :text "hi"}]})

(def ^:private legacy-payloads
  "One sample payload per neutralized op (some carry the doc/title nested)."
  [{"op" "ui/show-panel" "panel/id" "p1" "doc" {"doc/title" "Lens C5" "doc/blocks" []}}
   {"op" "ui/close-panel" "panel/id" "p1"}
   {"op" "ui/focus-tab" "panel/id" "p1"}
   {"op" "ui/append-tab" "panel/id" "p1" "doc" {"doc/title" "T"}}
   {"op" "ui/notify" "message" "m" "level" "info"}
   {"op" "ui/open-file" "file" "a.clj" "line" 3}
   {"op" "ui/send-to-terminal" "terminal" "t" "text" "x"}])

(def ^:private expected-neutral-ops
  {"ui/show-panel"        "show"
   "ui/close-panel"       "close"
   "ui/focus-tab"         "focus"
   "ui/append-tab"        "append"
   "ui/notify"            "notify"
   "ui/open-file"         "open-file"
   "ui/send-to-terminal"  "ui/send-to-terminal"})

(deftest neutralize-renames-every-op-for-v2
  (doseq [payload legacy-payloads]
    (is (= (expected-neutral-ops (get payload "op"))
           (get (json/neutralize payload (features-target #{:spans})) "op"))
        (str "op " (get payload "op")))))

(deftest neutralize-renames-fields-recursively
  (let [out (json/neutralize {"op" "ui/show-panel" "panel/id" "p1"
                              "doc" {"doc/title" "T" "doc/blocks" []}}
                             (features-target #{:v2}))]
    (is (= "p1" (get out "id")))
    (is (nil? (get out "panel/id")))
    (is (= "T" (get-in out ["doc" "title"])))))

(deftest neutralize-is-identity-for-legacy-targets
  (doseq [payload legacy-payloads
          :let [target (legacy-target)]]
    (is (identical? payload (json/neutralize payload target))))
  (doseq [features [#{} nil]]
    (doseq [payload legacy-payloads]
      (is (identical? payload (json/neutralize payload (features-target features)))))))

(deftest neutralize-is-deterministic
  (let [target (features-target #{:spans :v2})]
    (doseq [payload legacy-payloads]
      (is (= (json/neutralize payload target)
             (json/neutralize payload target)
             (json/neutralize payload target))))))

(deftest v2-client-gets-neutral-names-end-to-end
  (let [target (features-target #{:spans :v2})
        payload (fn [op] (get-in (v/plan (v/standard-registry) target op)
                                 [:ok :plan/ops 0 :native/payload]))]
    (let [out (payload {:op :ui/show-panel :panel/id "p1" :doc sample-doc})]
      (is (= "show" (get out "op")))
      (is (= "p1" (get out "id")))
      (is (= "Lens C5" (get-in out ["doc" "title"])))
      (is (some? (get out "lines"))))
    (is (= {"op" "notify" "message" "m" "level" "info"}
           (payload {:op :ui/notify :message "m" :level :info})))
    (is (= {"op" "open-file" "file" "a.clj" "line" 3 "column" 4}
           (payload {:op :ui/open-file :file "a.clj" :line 3 :column 4})))
    (is (= {"op" "close" "id" "p1"}
           (payload {:op :ui/close-panel :panel/id "p1"})))))

(deftest legacy-client-output-unchanged-end-to-end
  (is (= {"op" "ui/close-panel" "panel/id" "p1"}
         (tu/lowered-payload :web {:op :ui/close-panel :panel/id "p1"})))
  (let [payload (tu/lowered-payload :web {:op :ui/show-panel :panel/id "p1" :doc sample-doc})]
    (is (= "ui/show-panel" (get payload "op")))
    (is (= "p1" (get payload "panel/id")))
    (is (= "Lens C5" (get-in payload ["doc" "doc/title"]))))
  (is (= {"op" "ui/send-to-terminal" "terminal" "t" "text" "x"}
         (tu/lowered-payload :web {:op :ui/send-to-terminal :terminal "t" :text "x"}))))
