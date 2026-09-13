(ns hive-vessel.doc-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as hst]
            [hive-vessel.doc :as d]
            [hive-vessel.schema :as s]))

;; SPDX-License-Identifier: MIT

(defn- text-bearing-blocks [doc]
  (filter #(#{:heading :para :code :link} (:block/type %)) (:doc/blocks doc)))

(hst/deftrifecta-from-schema render-lines
  hive-vessel.doc/render-lines
  {:in s/Doc
   :out [:vector s/RenderedLine]
   :classify (fn [doc _] (if (empty? (:doc/blocks doc)) :title-only :with-blocks))
   :classify-domain #{:title-only :with-blocks}
   ;; malli draws an empty block vector about 1 time in 12
   :classify-floor 2
   :rel (fn [doc lines]
          (and (= :title (:face (first lines)))
               ;; every block contributes a separator plus at least one line
               ;; when it bears text
               (>= (count lines) (+ 1 (* 2 (count (text-bearing-blocks doc)))))
               ;; the title survives verbatim, modulo line breaks
               (= (str/join "\n" (d/split-lines (:doc/title doc)))
                  (str/join "\n" (map :text (take-while #(= :title (:face %)) lines))))))
   :mutation true
   :num-tests 60})

(deftest diff-line-classification
  (is (= [:hunk :hunk :hunk :removed :added :context]
         (mapv d/diff-line-kind ["--- a/x" "+++ b/x" "@@ -1 +1 @@" "-old" "+new" " same"]))))

(deftest diff-block-prefers-explicit-lines
  (is (= [{:line/kind :added :line/text "x"}]
         (d/diff-lines {:block/type :diff :text "-ignored"
                        :lines [{:line/kind :added :line/text "x"}]}))))

(deftest link-lines-carry-location
  (is (= [{:text "go" :face :link :file "a.clj" :line 4}]
         (d/block-lines (d/link "go" "a.clj" 4)))))

(deftest fields-align-keys
  (is (= ["a    : 1" "long : 2"]
         (mapv :text (d/block-lines (d/fields [["a" 1] ["long" 2]]))))))

(deftest unknown-block-type-renders-instead-of-throwing
  (is (= :muted (:face (first (d/block-lines {:block/type :chart :data [1 2]}))))))
