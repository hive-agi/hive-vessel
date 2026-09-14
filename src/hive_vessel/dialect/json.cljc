(ns hive-vessel.dialect.json
  "The :json dialect: one JSON message per op, for vessels that render
   themselves -- a VS Code extension, a browser harness, any custom web UI.

   Payload: `{\"op\": \"ui/show-panel\", ...fields}` as JSON-able data. A
   show-panel message carries the structured doc AND its rendered lines, so a
   rich client can lay blocks out natively while a simple one paints lines.

   Dialect call: `{:op :json/event :event \"my-addon/thing\" :data {...}}`
   for addons whose client-side code handles a custom event."
  (:require [hive-vessel.doc :as doc]
            [hive-vessel.wire :as wire]
            [hive-vessel.schema :as s]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def dialect :json)

(defn native [message] {:op :vessel/native :native/dialect dialect :native/payload (wire/->json-data message)})

(def Payload
  "A :json native payload: the op's own fields under string keys, \"op\" among
   them; a show-panel message also carries the rendered \"lines\"."
  [:map ["op" s/NonBlank]])

(defn- message
  "OP as JSON data: every field under its string key."
  [op]
  (wire/->json-data (assoc (dissoc op :op) :op (:op op))))

(defn notify-message [op] (message op))
(m/=> notify-message [:=> [:cat s/Notify] Payload])

(defn show-panel-message
  "The doc AND its rendered lines, so a rich client lays blocks out natively
   while a simple one paints lines."
  [op]
  (assoc (message op) "lines" (wire/->json-data (doc/render-lines (:doc op)))))
(m/=> show-panel-message [:=> [:cat s/ShowPanel] Payload])

(defn close-panel-message [op] (message op))
(m/=> close-panel-message [:=> [:cat s/ClosePanel] Payload])

(defn open-file-message [op] (message op))
(m/=> open-file-message [:=> [:cat s/OpenFile] Payload])

(defn send-to-terminal-message [op] (message op))
(m/=> send-to-terminal-message [:=> [:cat s/SendToTerminal] Payload])

(def Event
  "The :json/event dialect call: a custom event for client-side code."
  [:map [:op [:= :json/event]] [:event s/NonBlank] [:data {:optional true} :any]])

(defn event-message [{:keys [event data]}]
  (wire/->json-data {:op :json/event :event event :data data}))
(m/=> event-message [:=> [:cat Event] Payload])

(def translators
  (let [w {:vessel/dialect dialect}
        t (fn [op f] {:translator/id (keyword "hive-vessel.json" (name op))
                      :translator/op op
                      :translator/when w
                      :translator/translate (fn [o _] (native (f o)))})]
    ;; Each lowering is reached through its var at call time, never captured.
    [(t :ui/notify #(notify-message %))
     (t :ui/show-panel #(show-panel-message %))
     (t :ui/close-panel #(close-panel-message %))
     (t :ui/open-file #(open-file-message %))
     (t :ui/send-to-terminal #(send-to-terminal-message %))
     (assoc (t :json/event #(event-message %))
            :translator/accepts [:map [:event [:string {:min 1}]]])]))
