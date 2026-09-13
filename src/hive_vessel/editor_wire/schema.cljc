(ns hive-vessel.editor-wire.schema
  "Malli value objects of hive editor wire v1, over JSON-parsed data (string keys)."
  (:require [hive-vessel.editor-wire.ops :as ops]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def RequestId [:int {:min 1}])

(def CallId [:int {:max -1}])

(def OpName (into [:enum] (sort ops/all-ops)))

(def ErrorCode (into [:enum] (sort ops/error-codes)))

(def EventName (into [:enum] (sort ops/event-names)))

(def JsonKey :string)

(def Params [:map-of JsonKey :any])

(def OkResult
  [:map ["ok" [:= true]] ["value" :any]])

(def ErrResult
  [:map
   ["ok" [:= false]]
   ["error" [:map ["code" ErrorCode] ["message" {:optional true} :string]]]])

(def Result [:or OkResult ErrResult])

(def Hello
  [:map
   ["type" [:= "hello"]]
   ["wire" [:= ops/wire-version]]
   ["token" :string]
   ["editor" [:string {:min 1}]]
   ["instance" {:optional true} :string]
   ["capabilities" {:optional true} [:vector :string]]
   ["cwd" {:optional true} [:maybe :string]]])

(def Event
  [:map
   ["type" [:= "event"]]
   ["event" EventName]
   ["data" {:optional true} Params]])

(def ClientMessage
  [:multi {:dispatch (fn [msg] (get msg "type"))}
   ["hello" Hello]
   ["event" Event]])

(def ClientRequestFrame [:tuple RequestId ClientMessage])

(def ClientReplyFrame [:tuple CallId Result])

(def ServerCallFrame
  [:tuple [:= "call"] [:= "HiveOp"] [:tuple OpName Params] CallId])

(def NativePayload
  "A Vim channel command as hive-vessel's :vim-channel dialect emits it."
  [:or
   [:tuple [:= "call"] :string [:vector :any]]
   [:tuple [:= "expr"] :string]
   [:tuple [:enum "ex" "normal"] :string]
   [:tuple [:= "redraw"]]
   [:tuple [:= "redraw"] :string]])

(def ServerNativeFrame
  [:or
   [:tuple [:= "call"] :string [:vector :any] CallId]
   [:tuple [:= "expr"] :string CallId]])

(def ServerReplyFrame [:tuple RequestId Result])

(def FrameKind
  [:enum :client-request :client-reply :server-call :server-reply :server-native :invalid])

(def Pending
  [:map {:closed true}
   [:op [:or OpName [:= ops/native-op]]]
   [:deadline :int]])

(def SessionStatus [:enum :awaiting-hello :ready :closed])

(def SessionState
  [:map {:closed true}
   [:status SessionStatus]
   [:session-id :string]
   [:token :string]
   [:next-call :int]
   [:pending [:map-of CallId Pending]]
   [:hello [:maybe Hello]]])

(def Effect
  [:multi {:dispatch first}
   [:send [:tuple [:= :send] :any]]
   [:resolve [:tuple [:= :resolve] CallId Result]]
   [:close [:tuple [:= :close] ErrorCode]]
   [:hello [:tuple [:= :hello] Hello]]
   [:event [:tuple [:= :event] Event]]])

(def Step
  [:map {:closed true}
   [:state SessionState]
   [:effects [:vector Effect]]
   [:call-id {:optional true} CallId]])

(defn validate
  [schema value]
  (m/validate schema value))

(defn explain
  [schema value]
  (m/explain schema value))
