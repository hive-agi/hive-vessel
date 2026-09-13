(ns hive-vessel.editor-wire.codec-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-vessel.editor-wire.codec :as codec]
            [hive-vessel.editor-wire.schema :as s]
            [hive-vessel.wire :as wire]
            #?(:clj [hive-schemas.test :as hst])
            [malli.generator :as mg]))

;; SPDX-License-Identifier: MIT

(def conformance
  "Wire v1 lines exactly as they cross the socket, with their classification."
  [["[1,{\"type\":\"hello\",\"wire\":1,\"token\":\"t\",\"editor\":\"vim\"}]" :client-request]
   ["[2,{\"type\":\"event\",\"event\":\"focus\",\"data\":{}}]" :client-request]
   ["[\"call\",\"HiveOp\",[\"find-file\",{\"file\":\"/tmp/a\"}],-1]" :server-call]
   ["[-1,{\"ok\":true,\"value\":null}]" :client-reply]
   ["[-2,{\"ok\":false,\"error\":{\"code\":\"op/failed\",\"message\":\"E492\"}}]" :client-reply]
   ["[1,{\"ok\":true,\"value\":{\"session\":\"s\",\"wire\":1}}]" :server-reply]
   ["[\"call\",\"hive_vessel#notify\",[\"hi\",\"info\"],-3]" :server-native]
   ["[\"expr\",\"g:x\",-4]" :server-native]
   ["[\"call\",\"hive_vessel#notify\",[\"hi\",\"info\"],3]" :invalid]
   ["[\"ex\",\"echo 1\"]" :invalid]
   ["[0,{\"type\":\"event\",\"event\":\"focus\"}]" :invalid]
   ["[\"call\",\"HiveOp\",[\"find-file\",{}],3]" :invalid]
   ["{\"ok\":true}" :invalid]
   ["[1]" :invalid]])

(deftest conformance-vectors
  (doseq [[line kind] conformance]
    (is (= kind (codec/classify (wire/read-json line))) line)))

(defspec generated-client-requests-classify 200
  (prop/for-all [f (mg/generator s/ClientRequestFrame)]
    (= :client-request (codec/classify f))))

(defspec generated-client-replies-classify 200
  (prop/for-all [f (mg/generator s/ClientReplyFrame)]
    (= :client-reply (codec/classify f))))

(defspec generated-server-calls-classify 200
  (prop/for-all [f (mg/generator s/ServerCallFrame)]
    (= :server-call (codec/classify f))))

(defspec generated-server-replies-classify 200
  (prop/for-all [f (mg/generator s/ServerReplyFrame)]
    (= :server-reply (codec/classify f))))

#?(:clj
   (hst/deftrifecta-from-schema call-builds-server-call-frames
     hive-vessel.editor-wire.codec/call
     {:in [:cat s/CallId s/OpName s/Params]
      :out s/ServerCallFrame
      :mutation false
      :num-tests 100
      :rel (fn [[id op params] frame]
             (and (= :server-call (codec/classify frame))
                  (= id (codec/frame-id frame))
                  (= op (codec/call-op frame))
                  (= params (codec/call-params frame))))}))

(defspec generated-calls-build-server-call-frames 100
  (prop/for-all [id (mg/generator s/CallId)
                 op (mg/generator s/OpName)
                 params (mg/generator s/Params)]
    (let [frame (codec/call id op params)]
      (and (s/validate s/ServerCallFrame frame)
           (= :server-call (codec/classify frame))
           (= id (codec/frame-id frame))
           (= op (codec/call-op frame))
           (= params (codec/call-params frame))))))

(deftest call-frames-survive-json
  (let [frame (codec/call -7 "insert-text" {"text" "é\n\"quoted\""})]
    (is (= frame (wire/read-json (wire/write-json frame))))))

(defspec generated-native-payloads-frame-and-strip 200
  (prop/for-all [payload (mg/generator s/NativePayload)
                 id (mg/generator s/CallId)]
    (and (codec/native? payload)
         (if (codec/replying-native? payload)
           (let [frame (codec/native-call id payload)]
             (and (= :server-native (codec/classify frame))
                  (s/validate s/ServerNativeFrame frame)
                  (= id (codec/frame-id frame))
                  (= payload (codec/native-payload frame))))
           (= :invalid (codec/classify payload))))))

(deftest a-native-is-a-channel-command
  (is (codec/native? ["call" "hive_vessel#notify" ["hi" "info"]]))
  (is (codec/native? ["ex" "echo 1"]))
  (is (not (codec/native? ["call" "f"])) "a call needs its args")
  (is (not (codec/native? ["bogus"])))
  (is (not (codec/native? "call")))
  (is (= :server-call (codec/classify (codec/native-call -1 ["call" "HiveOp" ["find-file" {}]])))
      "a native naming HiveOp is a HiveOp call, correlated as one"))

(deftest builders-validate
  (testing "hello defaults the wire version and omits absent optionals"
    (let [h (codec/hello {:token "t" :editor "vim"})]
      (is (s/validate s/Hello h))
      (is (= #{"type" "wire" "token" "editor"} (set (keys h))))))
  (is (s/validate s/Hello (codec/hello {:token "t" :editor "vscode" :instance "i"
                                        :capabilities ["editor" "buffer"] :cwd "/w"})))
  (is (s/validate s/Event (codec/event "at_mentioned" {"filePath" "/a"})))
  (is (s/validate s/ClientRequestFrame (codec/request 3 (codec/event "focus")))))
