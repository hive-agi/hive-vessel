(ns hive-vessel.wire-test
  "hive-vessel.wire: the JSON writer and its dependency-free reader, checked
   against clojure.data.json as an independent oracle."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-vessel.wire :as wire]))

;; SPDX-License-Identifier: MIT

(def gen-scalar
  (gen/one-of [gen/string
               gen/large-integer
               (gen/double* {:infinite? false :NaN? false})
               gen/boolean
               (gen/return nil)]))

(def gen-json
  "JSON-representable data: string-keyed maps, vectors and scalars."
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of [(gen/vector inner 0 4)
                  (gen/map gen/string-alphanumeric inner {:max-elements 4})]))
   gen-scalar))

(defspec read-inverts-write 300
  (prop/for-all [x gen-json]
    (= x (wire/read-json (wire/write-json x)))))

(defspec reader-agrees-with-the-oracle 300
  (prop/for-all [x gen-json]
    (= (json/read-str (wire/write-json x))
       (wire/read-json (wire/write-json x)))))

(defspec writer-agrees-with-the-oracle 300
  (prop/for-all [x gen-json]
    (= x (json/read-str (wire/write-json x)))))

(deftest conformance
  (testing "wire frames as they cross a socket"
    (is (= [1 {"type" "hello" "wire" 1 "token" "t" "editor" "vim"}]
           (wire/read-json "[1,{\"type\":\"hello\",\"wire\":1,\"token\":\"t\",\"editor\":\"vim\"}]")))
    (is (= ["call" "HiveOp" ["find-file" {"file" "/tmp/a"}] -1]
           (wire/read-json "[\"call\",\"HiveOp\",[\"find-file\",{\"file\":\"/tmp/a\"}],-1]")))
    (is (= [-2 {"ok" false "error" {"code" "op/failed" "message" "E492"}}]
           (wire/read-json " [ -2 , { \"ok\" : false , \"error\" : { \"code\" : \"op/failed\" , \"message\" : \"E492\" } } ] "))))
  (testing "escapes, unicode and surrogate pairs"
    (is (= "é\n\"q\" \t \\ / \u2028 😀"
           (wire/read-json "\"\\u00e9\\n\\\"q\\\" \\t \\\\ \\/ \\u2028 \\ud83d\\ude00\""))))
  (testing "numbers"
    (is (= 0 (wire/read-json "0")))
    (is (= -12 (wire/read-json "-12")))
    (is (= 1.5 (wire/read-json "1.5")))
    (is (= 1.0e10 (wire/read-json "1e10")))
    (is (= 123456789012345678901234567890N (wire/read-json "123456789012345678901234567890"))))
  (testing "empty containers and literals"
    (is (= {} (wire/read-json "{}")))
    (is (= [] (wire/read-json "[ ]")))
    (is (= [true false nil] (wire/read-json "[true,false,null]")))))

(deftest malformed-input-throws
  (doseq [text ["" "   " "[1,2" "{\"a\":1" "{\"a\" 1}" "{1:2}" "[1 2]" "\"open"
                "tru" "[1,]" "{\"a\":1,}" "1 2" "\"ctl\nchar\"" "\"\\x\""]]
    (is (thrown? clojure.lang.ExceptionInfo (wire/read-json text)) (pr-str text))))
