(ns hive-vessel.schema
  "Malli value objects for the vessel action contract.

   Five shapes, bottom layer of the lib:

   - Op          `{:op <qualified-keyword> ...}` -- the one unit of work. An
                 addon intent (`:carto-flow/frame`), a standard primitive
                 (`:ui/show-panel`), a dialect call (`:elisp/call`) and a
                 final native op (`:vessel/native`) are all Ops.
   - Doc         a vessel-neutral document (title + blocks) that every vessel
                 knows how to paint.
   - Target      the part of a vessel descriptor that decides translation:
                 id, dialect, advertised features.
   - Translator  data: which op it rewrites, when it applies, how.
   - Compiled    `{:ok Plan}` or `{:error Failure}`.

   These schemas are the single source for the `m/=>` contracts and for the
   synthesized tests."
  (:require [malli.core :as m]))

;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Scalars
;; =============================================================================

(def NonBlank [:string {:min 1}])

(def VesselId
  "Stable vessel key: :emacs, :vim, :neovim, :vscode, :web, :tmux, ..."
  :keyword)

(def Dialect
  "The native language a vessel executes. Standard dialects: :elisp,
   :vim-channel, :json, :text. Open: a new vessel may speak a new one."
  :keyword)

(def OpType :qualified-keyword)

(def Tone [:enum :plain :muted :info :success :warn :error])

(def Level [:enum :info :warn :error])

(def Face
  "Presentation role of one rendered line. Vessels map faces to their own
   styling (Emacs faces, Vim highlight groups, CSS classes)."
  [:enum :title :heading :plain :muted :info :success :warn :error
   :added :removed :hunk :code :link])

;; =============================================================================
;; Document
;; =============================================================================

(def DiffLine
  [:map
   [:line/kind [:enum :context :added :removed :hunk]]
   [:line/text :string]])

(def Block
  [:multi {:dispatch :block/type}
   [:heading [:map
              [:block/type [:= :heading]]
              [:text :string]
              [:level {:optional true} [:int {:min 1 :max 3}]]]]
   [:para [:map
           [:block/type [:= :para]]
           [:text :string]
           [:tone {:optional true} Tone]]]
   [:fields [:map
             [:block/type [:= :fields]]
             [:fields [:vector [:tuple :string :string]]]]]
   [:list [:map
           [:block/type [:= :list]]
           [:items [:vector :string]]]]
   [:code [:map
           [:block/type [:= :code]]
           [:text :string]
           [:lang {:optional true} :string]]]
   [:diff [:map
           [:block/type [:= :diff]]
           [:text {:optional true} :string]
           [:lines {:optional true} [:vector DiffLine]]]]
   [:link [:map
           [:block/type [:= :link]]
           [:text :string]
           [:file NonBlank]
           [:line {:optional true} [:int {:min 1}]]]]])

(def Doc
  [:map
   [:doc/title :string]
   [:doc/blocks [:vector Block]]])

(def RenderedLine
  "One painted line. `:text` never contains a newline."
  [:map
   [:text [:and :string [:fn {:error/message "no newline"}
                         #(not (re-find #"[\r\n]" %))]]]
   [:face Face]
   [:file {:optional true} NonBlank]
   [:line {:optional true} [:int {:min 1}]]])

;; =============================================================================
;; Ops
;; =============================================================================

(def Op
  "Any op. Open map: an addon intent carries whatever payload it needs."
  [:map [:op OpType]])

(def Notify
  [:map
   [:op [:= :ui/notify]]
   [:message :string]
   [:level {:optional true} Level]])

(def ShowPanel
  [:map
   [:op [:= :ui/show-panel]]
   [:panel/id NonBlank]
   [:doc Doc]])

(def ClosePanel
  [:map
   [:op [:= :ui/close-panel]]
   [:panel/id NonBlank]])

(def OpenFile
  [:map
   [:op [:= :ui/open-file]]
   [:file NonBlank]
   [:line {:optional true} [:int {:min 1}]]
   [:column {:optional true} [:int {:min 1}]]])

(def SendToTerminal
  [:map
   [:op [:= :ui/send-to-terminal]]
   [:terminal NonBlank]
   [:text :string]])

(def primitive-schemas
  "The standard vocabulary. Every standard dialect lowers all of these."
  {:ui/notify          Notify
   :ui/show-panel      ShowPanel
   :ui/close-panel     ClosePanel
   :ui/open-file       OpenFile
   :ui/send-to-terminal SendToTerminal})

(def Primitive
  (into [:multi {:dispatch :op}]
        (map (fn [[k s]] [k s]))
        primitive-schemas))

(def Native
  "A fully lowered op: opaque PAYLOAD in DIALECT, ready for the vessel's
   executor."
  [:map
   [:op [:= :vessel/native]]
   [:native/dialect Dialect]
   [:native/payload :any]])

;; =============================================================================
;; Target, translators, registry
;; =============================================================================

(def Features [:set :keyword])

(def Target
  "Open map: a full vessel descriptor (with :vessel/execute!, :vessel/addon,
   ...) satisfies it."
  [:map
   [:vessel/id VesselId]
   [:vessel/dialect Dialect]
   [:vessel/features {:optional true} Features]])

(def When
  "Guard on a Target. Absent keys match anything; features must be a subset
   of what the target advertises."
  [:map
   [:vessel/id {:optional true} VesselId]
   [:vessel/dialect {:optional true} Dialect]
   [:vessel/features {:optional true} Features]])

(def Translator
  [:map
   [:translator/id OpType]
   [:translator/op OpType]
   [:translator/translate ifn?]
   [:translator/when {:optional true} When]
   [:translator/priority {:optional true} :int]
   [:translator/accepts {:optional true} :any]])

(def Registry
  [:map [:registry/translators [:vector Translator]]])

;; =============================================================================
;; Plans
;; =============================================================================

(def FailureReason
  [:enum :invalid-op :unsupported :dialect-mismatch :translator-threw
   :invalid-output :depth-exceeded :cycle :no-executor :execute-threw])

(def Failure
  [:map
   [:failure/reason FailureReason]
   [:failure/op {:optional true} :any]
   [:failure/detail {:optional true} :any]
   [:failure/attempts {:optional true} [:vector :any]]])

(def TraceStep
  [:map
   [:op OpType]
   [:translator/id OpType]
   [:depth [:int {:min 0}]]])

(def Plan
  [:map
   [:plan/ops [:vector Native]]
   [:plan/trace [:vector TraceStep]]])

(def Compiled
  [:or
   [:map [:ok Plan]]
   [:map [:error Failure]]])

(defn valid?
  "True iff VALUE conforms to SCHEMA."
  [schema value]
  (m/validate schema value))

(defn explain
  [schema value]
  (m/explain schema value))
