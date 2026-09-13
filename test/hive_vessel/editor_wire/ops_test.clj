(ns hive-vessel.editor-wire.ops-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.terminal :as term]
            [hive-vessel.editor-wire.ops :as ops]
            [hive-vessel.editor-wire.schema :as s]
            [hive-schemas.test :as hst]
            [hive-spi.editor.ports :as ports]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

;; SPDX-License-Identifier: MIT

(defn- protocol-ops [protocol]
  (set (map ops/method->op (keys (:sigs protocol)))))

(deftest op-names-are-the-protocol-method-names
  (testing "substrate ops are exactly IEditorPort"
    (is (= (protocol-ops ports/IEditorPort) (set ops/substrate-ops))))
  (testing "buffer ops are exactly IEditorBufferPort"
    (is (= (protocol-ops ports/IEditorBufferPort) (set ops/buffer-ops))))
  (testing "terminal ops are ITerminalAddon minus terminal-id, plus terminal-read"
    (is (= (-> (protocol-ops term/ITerminalAddon) (disj "terminal-id") (conj "terminal-read"))
           (set ops/terminal-ops)))))

(deftest every-op-belongs-to-exactly-one-surface
  (is (= (count ops/all-ops)
         (count (mapcat val ops/surfaces))))
  (is (every? #(contains? ops/surfaces (ops/op->surface %)) ops/all-ops)))

(deftest method-names-map-to-ops
  (is (= "terminal-spawn" (ops/method->op 'terminal-spawn!)))
  (is (= "find-file" (ops/method->op :find-file)))
  (is (= "editor-eval" (ops/method->op "editor-eval"))))

(deftest timeout-policy
  (is (= 250 (ops/timeout-ms "editor-eval" {"timeout_ms" 250})))
  (is (= ops/default-timeout-ms (ops/timeout-ms "editor-eval" {"timeout_ms" -1})))
  (is (= ops/default-timeout-ms (ops/timeout-ms "find-file" {"timeout_ms" 250}))))

(defspec op?-agrees-with-schema 300
  (prop/for-all [x (gen/one-of [(gen/elements (sort ops/all-ops))
                                gen/string-alphanumeric
                                gen/any-printable-equatable])]
    (= (boolean (ops/op? x)) (s/validate s/OpName x))))

(hst/deftrifecta-from-schema ok-envelopes-are-results
  hive-vessel.editor-wire.ops/ok
  {:in [:cat [:or :string :int :boolean :nil]]
   :out s/OkResult
   :mutation false
   :num-tests 100
   :rel (fn [[v] out] (and (ops/ok? out) (= v (get out "value"))))})

(hst/deftrifecta-from-schema err-envelopes-are-results
  hive-vessel.editor-wire.ops/err
  {:in [:cat s/ErrorCode]
   :out s/ErrResult
   :mutation false
   :num-tests 100
   :rel (fn [[code] out] (= code (ops/error-code out)))})

(deftest result->mcp-projection
  (is (= {:content [{:type "text" :text "\"x\""}] :isError false}
         (ops/result->mcp json/write-str (ops/ok "x"))))
  (is (true? (:isError (ops/result->mcp json/write-str (ops/err "op/failed" "boom"))))))
