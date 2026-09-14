(ns hive-vessel.renderer
  "Shared renderer adapters and lifecycle registry. No application or editor names."
  (:require [hive-spi.vessel :as port]
            [hive-vessel.core :as vessel]))

(def presentation-ops #{:ui/notify :ui/show-panel :ui/close-panel})

(defonce renderers (atom {}))

(defn deliver!
  "Use a vessel Target with presentation authority only."
  [target ops]
  (cond
    (not (and (vector? ops) (every? #(contains? presentation-ops (:op %)) ops)))
    {:error {:reason :presentation-only}}
    (nil? target) {:error {:reason :renderer-unavailable}}
    :else (vessel/dispatch! (vessel/standard-registry) target ops)))

(defn renderer
  "Adapt a dynamic Target resolver once; every package can use its IRenderer."
  [id target-fn]
  (reify port/IRenderer
    (renderer-id [_] id)
    (render! [_ ops]
      (try (deliver! (target-fn) ops)
           (catch #?(:clj Throwable :cljs :default) e
             {:error {:reason :renderer-failed :message (ex-message e)}})))))

(defn register!
  "Publish a renderer. Registry watches must only enqueue work, never render inline."
  [renderer]
  (when-not (satisfies? port/IRenderer renderer)
    (throw (ex-info "Expected IRenderer" {})))
  (swap! renderers assoc (port/renderer-id renderer) renderer)
  renderer)

(defn unregister!
  "Remove only this registration; stale shutdown cannot remove a replacement."
  [renderer]
  (swap! renderers (fn [current]
                     (let [id (port/renderer-id renderer)]
                       (if (identical? renderer (get current id))
                         (dissoc current id)
                         current))))
  nil)
