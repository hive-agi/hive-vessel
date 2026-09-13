(ns hive-vessel.dialect.json
  "The :json dialect: one JSON message per op, for vessels that render
   themselves -- a VS Code extension, a browser harness, any custom web UI.

   Payload: `{\"op\": \"ui/show-panel\", ...fields}` as JSON-able data. A
   show-panel message carries the structured doc AND its rendered lines, so a
   rich client can lay blocks out natively while a simple one paints lines.

   Dialect call: `{:op :json/event :event \"my-addon/thing\" :data {...}}`
   for addons whose client-side code handles a custom event."
  (:require [hive-vessel.doc :as doc]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(def dialect :json)

(defn native [message] {:op :vessel/native :native/dialect dialect :native/payload (wire/->json-data message)})

(defn- message [op]
  (let [base (dissoc op :op)]
    (assoc base :op (:op op))))

(def translators
  (let [w {:vessel/dialect dialect}
        t (fn [op f] {:translator/id (keyword "hive-vessel.json" (name op))
                      :translator/op op
                      :translator/when w
                      :translator/translate (fn [o _] (native (f o)))})]
    [(t :ui/notify message)
     (t :ui/show-panel (fn [o] (assoc (message o) :lines (doc/render-lines (:doc o)))))
     (t :ui/close-panel message)
     (t :ui/open-file message)
     (t :ui/send-to-terminal message)
     (assoc (t :json/event (fn [{:keys [event data]}] {:op :json/event :event event :data data}))
            :translator/accepts [:map [:event [:string {:min 1}]]])]))
