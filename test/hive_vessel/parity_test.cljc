(ns hive-vessel.parity-test
  "Cross-dialect parity. What a vessel paints is decided once
   (hive-vessel.doc/render-lines); every standard dialect must carry that
   content verbatim, in order, for every standard primitive. Extractors read
   the painted content back out of each dialect's native payload; the
   properties compare them; the goldens lock the exact payloads."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.golden :refer [deftest-golden]]
            [hive-vessel.core :as v]
            [hive-vessel.dialect.elisp :as elisp]
            [hive-vessel.doc :as d]
            [hive-vessel.schema :as s]
            [malli.generator :as mg]))

;; SPDX-License-Identifier: MIT

(def standard (v/standard-registry))

(def targets (vals v/reference-targets))

(defn- payload
  "The single native payload OP lowers to on TARGET."
  [target op]
  (let [r (v/plan standard target op)]
    (is (:ok r) (pr-str (:vessel/id target) (:error r)))
    (get-in r [:ok :plan/ops 0 :native/payload])))

(defn- ordered-in?
  "True iff every literal in LITERALS occurs in CODE, each after the previous."
  [code literals]
  (loop [from 0 [l & more] literals]
    (if (nil? l)
      true
      (let [i (str/index-of code l from)]
        (and i (recur (+ i (count l)) more))))))

;; =============================================================================
;; Extractors: the painted content a dialect payload carries
;; =============================================================================

(defmulti panel-texts
  "The line texts a :ui/show-panel payload paints, or ::literal for a dialect
   whose payload is source code (checked by literal inclusion)."
  (fn [dialect _payload] dialect))

(defmethod panel-texts :text [_ p] (:text/lines p))
(defmethod panel-texts :json [_ p] (mapv #(get % "text") (get p "lines")))
(defmethod panel-texts :vim-channel [_ [_ _ [_ _ lines]]] (mapv #(get % "text") lines))
(defmethod panel-texts :elisp [_ _] ::literal)

(defmulti notify-message (fn [dialect _payload] dialect))
(defmethod notify-message :text [_ p]
  (str/join "\n" (map #(subs % (+ 2 (str/index-of % "] "))) (:text/lines p))))
(defmethod notify-message :json [_ p] (get p "message"))
(defmethod notify-message :vim-channel [_ [_ _ [message _]]] message)
(defmethod notify-message :elisp [_ _] ::literal)

(defmulti panel-id (fn [dialect _payload] dialect))
(defmethod panel-id :text [_ p] (:text/panel p))
(defmethod panel-id :json [_ p] (get p "panel/id"))
(defmethod panel-id :vim-channel [_ [_ _ [id & _]]] id)
(defmethod panel-id :elisp [_ _] ::literal)

(defmulti open-location
  "[file line column] a :ui/open-file payload targets, as strings, defaults
   applied. Strings keep the extractor host-neutral: no number parsing."
  (fn [dialect _payload] dialect))
(defmethod open-location :text [_ p]
  (let [[file line column] (str/split (subs (first (:text/lines p)) (count "open ")) #":")]
    [file (or line "1") (or column "1")]))
(defmethod open-location :json [_ p] [(get p "file") (str (get p "line" 1)) (str (get p "column" 1))])
(defmethod open-location :vim-channel [_ [_ _ [file line column]]] [file (str line) (str column)])
(defmethod open-location :elisp [_ _] ::literal)

(defmulti terminal-keys
  "[terminal text] a :ui/send-to-terminal payload sends."
  (fn [dialect _payload] dialect))
(defmethod terminal-keys :text [_ p] [(:text/terminal p) (:text/keys p)])
(defmethod terminal-keys :json [_ p] [(get p "terminal") (get p "text")])
(defmethod terminal-keys :vim-channel [_ [_ _ args]] (vec args))
(defmethod terminal-keys :elisp [_ _] ::literal)

;; =============================================================================
;; Properties: every dialect carries the same content
;; =============================================================================

(defn- same-on-every-dialect
  "True iff EXTRACT reads EXPECTED out of OP's payload on every reference
   target; an :elisp payload must contain LITERALS in order instead."
  [op extract expected literals]
  (every? (fn [{:vessel/keys [dialect] :as target}]
            (let [p (payload target op)
                  got (extract dialect p)]
              (if (= ::literal got)
                (ordered-in? p literals)
                (= expected got))))
          targets))

(def gen-doc
  "Docs of a size an editor would actually paint."
  (gen/scale #(min % 12) (mg/generator s/Doc)))

(def gen-panel-id (gen/not-empty gen/string-alphanumeric))

(defspec show-panel-paints-the-same-lines-on-every-dialect 60
  (prop/for-all [doc gen-doc
                 id gen-panel-id]
    (let [texts (mapv :text (d/render-lines doc))]
      (same-on-every-dialect {:op :ui/show-panel :panel/id id :doc doc}
                             panel-texts texts
                             (map elisp/string-literal texts)))))

(defspec show-panel-addresses-the-same-panel-on-every-dialect 60
  (prop/for-all [doc gen-doc
                 id gen-panel-id]
    (same-on-every-dialect {:op :ui/show-panel :panel/id id :doc doc}
                           panel-id id
                           [(elisp/string-literal (elisp/panel-buffer-name id))])))

(defspec close-panel-addresses-the-same-panel-on-every-dialect 60
  (prop/for-all [id gen-panel-id]
    (same-on-every-dialect {:op :ui/close-panel :panel/id id}
                           panel-id id
                           [(elisp/string-literal (elisp/panel-buffer-name id))])))

(defspec notify-carries-the-same-message-on-every-dialect 100
  (prop/for-all [op (mg/generator s/Notify)]
    (same-on-every-dialect op notify-message (:message op)
                           [(elisp/string-literal (:message op))])))

(defspec open-file-targets-the-same-location-on-every-dialect 100
  (prop/for-all [op (gen/fmap #(assoc % :file (str/replace (:file %) #"[:\s]" "_"))
                              (mg/generator s/OpenFile))]
    (let [{:keys [file line column]} op
          expected [file (str (or line 1)) (str (or column 1))]]
      (same-on-every-dialect op open-location expected
                             (cond-> [(elisp/string-literal file) "(goto-char (point-min))"]
                               line (conj (str "(forward-line " (dec line) ")"))
                               column (conj (str "(move-to-column " (dec column) ")")))))))

(defspec send-to-terminal-sends-the-same-keys-on-every-dialect 100
  (prop/for-all [op (mg/generator s/SendToTerminal)]
    (same-on-every-dialect op terminal-keys [(:terminal op) (:text op)]
                           [(elisp/string-literal (:terminal op))
                            (elisp/string-literal (:text op))])))

;; =============================================================================
;; Golden lock: the exact native payload of every primitive on every dialect
;; =============================================================================

(def sample-doc
  (d/doc "Carto \"Flow\" \\ #3"
         (d/heading "apply write-form")
         (d/para "succeeded" :success)
         (d/fields [["paths" "src/a.clj\nsrc/b.clj"] ["verify" "ok"]])
         (d/items ["one" "two\nmore"])
         (d/code "(defn f [] \"x\")" "clojure")
         (d/diff "@@ -1,2 +1,2 @@\n-(old)\n+(new ü)\n context")
         (d/link "open a" "/tmp/a.clj" 2)))

(def sample-ops
  {:show-panel {:op :ui/show-panel :panel/id "olympus/tab-2" :doc sample-doc}
   :close-panel {:op :ui/close-panel :panel/id "olympus/tab-2"}
   :notify-info {:op :ui/notify :message "frame 7 applied"}
   :notify-error {:op :ui/notify :message "boom\nsecond line" :level :error}
   :open-file {:op :ui/open-file :file "/tmp/a b.clj" :line 3 :column 7}
   :open-file-defaults {:op :ui/open-file :file "/tmp/a.clj"}
   :send-to-terminal {:op :ui/send-to-terminal :terminal "*vterm*" :text "ls -la\n"}})

(defn- lowered
  "{vessel-id {case native-ops}} for every reference vessel and sample op."
  []
  (into (sorted-map)
        (for [[id target] v/reference-targets]
          [id (into (sorted-map)
                    (for [[k op] sample-ops]
                      [k (get-in (v/plan standard target op) [:ok :plan/ops])]))])))

(deftest-golden every-primitive-on-every-dialect
  "test/golden/vessel/primitives-by-dialect.edn"
  (lowered))

(deftest the-golden-covers-every-vessel-and-every-sample
  (let [l (lowered)]
    (is (= (set (keys v/reference-targets)) (set (keys l))))
    (is (every? #(= (set (keys sample-ops)) (set (keys %))) (vals l)))
    (is (every? some? (mapcat vals (vals l))))))