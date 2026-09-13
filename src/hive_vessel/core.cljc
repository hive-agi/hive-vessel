(ns hive-vessel.core
  "Facade: the standard registry, reference targets, and the addon seam.

   How the pieces meet:

     addon intent  {:op :carto-flow/frame ...}
        | addon translator (plain data the addon ships, no dependency on this lib)
     primitives    {:op :ui/show-panel ...}  or a dialect call {:op :elisp/call ...}
        | dialect translator (below, or one a vessel registers)
     native        {:op :vessel/native :native/dialect :elisp :native/payload \"(...)\"}
        | (:vessel/execute! target)
     the editor

   An addon contributes translators through its IAddon hooks under
   `hook-key`; a vessel contributes a dialect (or overrides a lowering) the
   same way. Neither edits this lib."
  (:require [hive-vessel.dialect.elisp :as elisp]
            [hive-vessel.dialect.json :as json]
            [hive-vessel.dialect.text :as text]
            [hive-vessel.dialect.vim :as vim]
            [hive-vessel.dispatch :as dispatch]
            [hive-vessel.plan :as plan]
            [hive-vessel.rule :as rule]))

;; SPDX-License-Identifier: MIT

(def standard-translators
  "Lowerings of every standard primitive for every standard dialect."
  (vec (concat elisp/translators vim/translators json/translators text/translators)))

(defn standard-registry
  "A registry with the standard dialects plus EXTRA translator collections."
  [& extra]
  (apply rule/registry-of standard-translators extra))

(def reference-targets
  "Target descriptors for the vessels the standard dialects serve. A real
   vessel adds :vessel/execute! and any :vessel/features it advertises."
  {:emacs  {:vessel/id :emacs  :vessel/dialect :elisp}
   :vim    {:vessel/id :vim    :vessel/dialect :vim-channel}
   :vscode {:vessel/id :vscode :vessel/dialect :json}
   :web    {:vessel/id :web    :vessel/dialect :json}
   :tmux   {:vessel/id :tmux   :vessel/dialect :text}})

(def hook-key
  "IAddon hook key under which an addon exposes its translators: a
   collection, or a zero-arg fn returning one."
  :vessel/translators)

(defn hook-translators
  "The translators HOOKS (an IAddon hooks map) exposes, or []."
  [hooks]
  (let [h (get hooks hook-key)]
    (vec (cond
           (fn? h) (h)
           (sequential? h) h
           :else nil))))

(defn registry-from-hooks
  "The standard registry extended with the translators of every hooks map in
   HOOKS-MAPS, in order: later addons override earlier ones by id."
  [hooks-maps]
  (apply standard-registry (map hook-translators hooks-maps)))

(defn envelope->op
  "Normalize a delivery message to an op. An op passes through; a
   presentation envelope `{:type T :payload P}` (the shape addon presenters
   deliver) becomes `{:op T :payload P}`. Anything else is returned unchanged
   and fails compilation as :invalid-op."
  [message]
  (cond
    (and (map? message) (contains? message :op)) message
    (and (map? message) (qualified-keyword? (:type message)))
    {:op (:type message) :payload (:payload message)}
    :else message))

(def register rule/register)
(def register-all rule/register-all)
(def plan plan/plan)
(def supports? plan/supports?)
(def dispatch! dispatch/dispatch!)
(def broadcast! dispatch/broadcast!)
(def sink dispatch/sink)
