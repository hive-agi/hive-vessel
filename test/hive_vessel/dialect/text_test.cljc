(ns hive-vessel.dialect.text-test
  "The :text dialect's lowering fns: each is schema-synthesized (conformance +
   relation + mutation) from the primitive it lowers, and the one bug class a
   line renderer can hide -- a dropped optional field -- has a hand mutant
   that must die through the real plan."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-test.mutation :as mut]
            [hive-vessel.core :as v]
            [hive-vessel.dialect.text :as text]
            [hive-vessel.doc :as d]
            [hive-vessel.schema :as s]))

;; SPDX-License-Identifier: MIT

(hst/deftrifecta-from-schema notify-payload
  hive-vessel.dialect.text/notify-payload
  {:in s/Notify
   :out text/Payload
   :rel (fn [{:keys [message level]} {:text/keys [lines]}]
          (let [tag (str "[" (name (or level :info)) "] ")]
            (and (seq lines)
                 (every? #(str/starts-with? % tag) lines)
                 (= (str/join "\n" (d/split-lines message))
                    (str/join "\n" (map #(subs % (count tag)) lines))))))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema show-panel-payload
  hive-vessel.dialect.text/show-panel-payload
  {:in s/ShowPanel
   :out text/Payload
   :rel (fn [{:keys [doc] panel-id :panel/id} {:text/keys [panel lines face-lines]}]
          (and (= panel-id panel)
               (= (d/render-lines doc) face-lines)
               (= (mapv :text face-lines) lines)))
   :mutation true
   :num-tests 60})

(hst/deftrifecta-from-schema close-panel-payload
  hive-vessel.dialect.text/close-panel-payload
  {:in s/ClosePanel
   :out text/Payload
   :rel (fn [{panel-id :panel/id} {:text/keys [panel close? lines]}]
          (and (= panel-id panel) (true? close?) (empty? lines)))
   :mutation true
   :num-tests 60})

(defn- location-suffix [{:keys [line column]}]
  (cond
    (and line column) (str ":" line ":" column)
    line (str ":" line)
    column (str ":1:" column)
    :else ""))

(hst/deftrifecta-from-schema open-file-payload
  hive-vessel.dialect.text/open-file-payload
  {:in s/OpenFile
   :out text/Payload
   :rel (fn [{:keys [file] :as op} {:text/keys [lines open]}]
          (and (= [(str "open " file (location-suffix op))] lines)
               (= (select-keys op [:file :line :column]) open)))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema send-to-terminal-payload
  hive-vessel.dialect.text/send-to-terminal-payload
  {:in s/SendToTerminal
   :out text/Payload
   :rel (fn [{:keys [terminal text]} {:text/keys [lines keys] target :text/terminal}]
          (and (= terminal target)
               (= text keys)
               (= (d/split-lines text) lines)))
   :mutation true
   :num-tests 100})

(defn- lowered-line
  "The one text line OP lowers to on the tmux reference target, through the
   real registry."
  [op]
  (get-in (v/plan (v/standard-registry) (:tmux v/reference-targets) op)
          [:ok :plan/ops 0 :native/payload :text/lines 0]))

(mut/deftest-mutations open-file-keeps-every-optional-field
  hive-vessel.dialect.text/open-file-payload
  [["drops-the-column-without-a-line"
    (fn [{:keys [file line column]}]
      {:text/lines [(str "open " file (when line (str ":" line (when column (str ":" column)))))]})]
   ["drops-the-line"
    (fn [{:keys [file]}] {:text/lines [(str "open " file)]})]]
  (fn []
    (is (= "open a.clj:1:4" (lowered-line {:op :ui/open-file :file "a.clj" :column 4})))
    (is (= "open a.clj:3" (lowered-line {:op :ui/open-file :file "a.clj" :line 3})))
    (is (= "open a.clj:3:4" (lowered-line {:op :ui/open-file :file "a.clj" :line 3 :column 4})))
    (is (= "open a.clj" (lowered-line {:op :ui/open-file :file "a.clj"})))))

(deftest a-translator-reads-its-lowering-through-the-var
  ;; The mutation test above only has teeth if plan reaches the CURRENT var
  ;; value; a lowering captured by value at registry build time would hide
  ;; every mutant. Pin that seam explicitly.
  (mut/with-mutation [hive-vessel.dialect.text/open-file-payload (fn [_] {:text/lines ["mutant"]})]
    (is (= "mutant" (lowered-line {:op :ui/open-file :file "a.clj"}))))
  (is (= "open a.clj" (lowered-line {:op :ui/open-file :file "a.clj"}))))