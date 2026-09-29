(ns hive-vessel.executor.handshake
  "Pure half of the dirge feature handshake, and the producer capabilities.

  This namespace is the handshake bounded context's ubiquitous language:

  - Feature        a keyword a client advertises on the `features` query
                   param of its GET /vessel/events subscription.
  - Capabilities   what the producer offers the consumer: the reply verbs it
                   answers, the invoke verbs a reply may name, and a keymap
                   of dirge chord strings to either a reply verb or an
                   invoke binding.
  - Validation     capabilities are normalized as data: keys whose verb is
                   not advertised are dropped (purely), so the bridge and the
                   consumer read the same value.

  Everything here is a calculation: no IO, no state, no SSE, no files. The
  SSE bridge is the Boundary that reads `features` and records the parsed
  set; the hive-dirge host is the Boundary that supplies capabilities."

  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Feature handshake (dirge -> producer)
;; =============================================================================

(def feature-set-version
  "Version 1 of the subscription feature set. Bump only for a breaking change
   to the `features` param semantics; additive features never move it. See
   docs/panel-feed.md in hive-dirge."
  1)

(defn parse-features
  "A raw `features` query value as a set of keywords.

   \"spans,keys,cursor,open-file\" -> #{:spans :keys :cursor :open-file}.

   Unknown tokens are kept verbatim as keywords -- producers must tolerate
   features they do not know. nil or a blank string -> #{} (no features
   advertised). The value stays pure data: the bridge only collects."
  [raw]
  (if (str/blank? raw)
    #{}
    (into #{}
          (comp (remove str/blank?)
                (map #(keyword %)))
          (str/split raw #","))))

;; =============================================================================
;; Producer capabilities (producer -> dirge)
;; =============================================================================

(def capabilities-version
  "The discovery doc's capabilities.version: the only version a dirge client
   reads. Unknown fields in the doc are ignored; an absent capabilities
   block keeps the consumer's defaults."
  1)

(def default-replies
  "The five olympus reply verbs every dirge client answers."
  ["focus" "unfocus" "next-tab" "prev-tab" "refresh"])

(def default-invokes
  "No producer invoke verbs until a lens registry supplies them."
  [])

(def default-keys
  "The dirge chords bound to the default replies, in dirge chord syntax:
   lowercase, modifiers after the base key joined with '-'."
  {"enter" "focus"
   "u" "unfocus"
   "r" "refresh"
   "tab" "next-tab"
   "shift-tab" "prev-tab"})

(defn key-verb
  "The verb a keymap entry binds: a reply-verb string, an invoke binding's
   verb, or nil when the value is neither."
  [v]
  (cond
    (string? v) v
    (and (map? v) (string? (get v "invoke"))) (get v "invoke")
    :else nil))

(defn invalid-key?
  "True iff keymap entry V does not resolve to a verb advertised by
   REPLIES or INVOKES."
  [replies invokes v]
  (let [verb (key-verb v)]
    (or (nil? verb)
        (not (or (contains? replies verb)
                 (contains? invokes verb))))))

(defn validate-capabilities
  "CAPABILITIES normalized as data: every keymap entry whose verb is not
   advertised in :replies or :invokes is dropped. Pure -- the caller, not
   this fn, decides what to do about the drops (the dirge side logs a
   warning per its spec). Unknown fields are ignored, so extra data passes
   through untouched. The verb collections are returned as sorted vectors."
  [capabilities]
  (let [replies (set (:replies capabilities))
        invokes (set (:invokes capabilities))]
    {"version" capabilities-version
     "replies" (vec (sort replies))
     "invokes" (vec (sort invokes))
     "keys" (into (sorted-map)
                  (remove (fn [[_ v]] (invalid-key? replies invokes v)))
                  (:keys capabilities))}))

(defn capabilities
  "The complete discovery-doc capabilities block. INVOKES and KEYS come from
   a provider (the hive-dirge host fills them from its lens registry; the
   default is the five olympus replies and their chords, with no invokes)."
  ([] (capabilities default-invokes default-keys))
  ([invokes keys]
   (validate-capabilities
    {:version capabilities-version
     :replies default-replies
     :invokes (vec invokes)
     :keys keys})))
