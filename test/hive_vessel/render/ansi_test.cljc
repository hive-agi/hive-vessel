(ns hive-vessel.render.ansi-test
  "The Face to SGR projection. Two properties carry the namespace: painting is
   reversible (strip-ansi undoes it exactly to the sanitized text), and content
   can never open a span of its own. The palette's completeness is checked
   against the Face SCHEMA rather than against its own keys, so a face added to
   the enum cannot pass by being absent from both sides."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-schemas.test :as hst]
            [hive-test.mutation :as mut]
            [hive-vessel.dialect.text :as text]
            [hive-vessel.render.ansi :as a]
            [hive-vessel.schema :as s]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def ^:private faces (vec (m/children s/Face)))

(deftest every-face-in-the-schema-has-a-palette-entry
  ;; The universe is the enum, not (keys face-sgr): a face missing from both
  ;; would satisfy a self-derived comparison vacuously. Every shipped palette
  ;; is held to it, so adding a face breaks all of them at once rather than
  ;; leaving one silently unpainted.
  (is (seq faces))
  (doseq [[nm palette] {"face-sgr" a/face-sgr "face-sgr-256" a/face-sgr-256}]
    (is (= (set faces) (set (keys palette))) nm))
  (testing "face-sgr stays basic, so it remains the option for a 16-colour terminal"
    (is (every? #(not (str/includes? (str %) "38;5;")) (vals a/face-sgr))))
  (testing "the 256 palette is actually extended, or it is not worth its name"
    (is (every? #(str/includes? (str %) "38;5;")
                (remove nil? (vals a/face-sgr-256)))))
  (testing "the default is the wide palette: legibility wins at O(1) spans"
    (is (= a/face-sgr-256 a/*palette*))))

(deftest the-palette-is-read-per-call
  ;; A seam, not Capture-by-Var: binding must change what an EXISTING caller
  ;; paints, with no palette threaded through it.
  (let [line {:text "ok" :face :success}]
    (is (= 5.0 (a/estimated-tokens (a/paint-line line))))
    (binding [a/*palette* a/face-sgr]
      (is (= 3.0 (a/estimated-tokens (a/paint-line line))))
      (is (= "ok" (a/strip-ansi (a/paint-line line)))))
    (is (= 5.0 (a/estimated-tokens (a/paint-line line))))))

(hst/deftrifecta-from-schema paint-line
  hive-vessel.render.ansi/paint-line
  {:in s/RenderedLine
   :out :string
   :rel (fn [{:keys [text face]} painted]
          (let [sgr (get a/*palette* face)]
            (and (= (a/strip-ansi painted) (a/sanitize text))
                 (= (if sgr 1 0) (a/span-count painted))
                 (if sgr (str/ends-with? painted a/sgr-reset) true))))
   :mutation true
   :num-tests 100})

(hst/deftrifecta-from-schema colourize
  hive-vessel.render.ansi/colourize
  {:in text/Payload
   :out text/Payload
   :rel (fn [{:text/keys [face-lines] :as in} out]
          (and (= (dissoc in :text/lines) (dissoc out :text/lines))
               (= (:text/lines out)
                  (if face-lines
                    (mapv a/paint-line face-lines)
                    (mapv a/sanitize (:text/lines in))))))
   :mutation true
   :num-tests 60})

(defspec content-can-never-open-a-span 200
  ;; Arbitrary text under the :plain face (which paints nothing) must yield a
  ;; string with zero spans, whatever escapes the text itself contained.
  (prop/for-all [t gen/string]
    (zero? (a/span-count (a/paint-line {:text t :face :plain})))))

(defspec painting-never-outlives-its-line 200
  (prop/for-all [t gen/string
                 f (gen/elements faces)]
    (let [painted (a/paint-line {:text t :face f})]
      (and (not (str/includes? (a/strip-ansi painted) ""))
           (>= 1 (a/span-count painted))))))

(deftest span-cost-is-the-measured-constant
  ;; Encodes memory 20260916152640-711a6f05. Pinned on `paint` directly so it
  ;; measures the CONSTANT and not whichever palette happens to be default.
  (testing "the two prices"
    (is (= 3.0 (a/estimated-tokens (a/paint "32" "x"))))
    (is (= 5.0 (a/estimated-tokens (a/paint "38;5;208" "x"))))
    (is (= 1 (a/span-count (a/paint "32" "x"))))
    (is (zero? (a/span-count "no spans here"))))
  (testing "cost is linear in SPANS, not in characters"
    (let [short-line (a/paint "32" "x")
          long-line (a/paint "32" (apply str (repeat 500 "x")))]
      (is (= (a/estimated-tokens short-line) (a/estimated-tokens long-line)))))
  (testing "an O(1) marker set stays within a small budget on either palette"
    (doseq [palette [a/face-sgr a/face-sgr-256]]
      (binding [a/*palette* palette]
        (let [markers (map #(a/mark % (name %)) [:success :warn :error :info :muted])]
          (is (= 5 (reduce + (map a/span-count markers))))
          (is (<= (reduce + (map a/estimated-tokens markers)) 25.0)))))))

(deftest an-unknown-face-paints-nothing
  (is (= "plain" (a/mark :no-such-face "plain")))
  (is (zero? (a/span-count (a/mark :no-such-face "plain")))))

(mut/deftest-mutations painting-is-reversible-and-sealed
  hive-vessel.render.ansi/paint-line
  [["forgets-the-reset"
    (fn [{:keys [text face]}]
      (let [sgr (get a/face-sgr face)
            clean (a/sanitize text)]
        (if sgr (str a/csi sgr "m" clean) clean)))]
   ["skips-the-sanitize"
    (fn [{:keys [text face]}]
      (a/paint (get a/face-sgr face) (str text)))]]
  (fn []
    ;; kills forgets-the-reset: an opened span must be closed on its own line
    (is (str/ends-with? (a/paint-line {:text "x" :face :success}) a/sgr-reset))
    ;; kills skips-the-sanitize: an escape in CONTENT must not survive
    (is (zero? (a/span-count (a/paint-line {:text (str a/csi "31mRED") :face :plain}))))))
