(ns hive-vessel.editor-wire.vessel
  "IVessel for a remote editor kind, assembled from its editor port and terminal."
  (:require [hive-addon.vessel :as vessel]
            [hive-vessel.editor-wire.terminal :as terminal]))

;; SPDX-License-Identifier: MIT

(defn remote-vessel
  "IVessel VESSEL-KW exposing :editor PORT and :terminal TERMINAL.
   resolve-context answers for lings spawned through TERMINAL."
  [vessel-kw port term]
  (reify vessel/IVessel
    (vessel-id [_] vessel-kw)
    (capabilities [_] #{:editor :terminal})
    (resolve-context [_ agent-id]
      (when agent-id
        (when-let [ctx (terminal/context-of term agent-id)]
          (when (or (:cwd ctx) (:project-id ctx)) ctx))))
    (addon [_ capability]
      (case capability
        :editor port
        :terminal term
        nil))
    (initialize! [_ _config] nil)
    (shutdown! [_] nil)))
