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