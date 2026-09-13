(ns smoke
  "Print the compiled plan of one sample op batch for every reference vessel."
  (:require [clojure.pprint :refer [pprint]]
            [hive-vessel.core :as v]
            [hive-vessel.doc :as d]))

;; SPDX-License-Identifier: MIT

(def sample-ops
  [{:op :ui/show-panel :panel/id "demo"
    :doc (d/doc "Demo \"q\"" (d/heading "H") (d/para "ok" :success)
                (d/fields [["phase" "apply"] ["cmd" "write-form"]])
                (d/diff "@@ -1 +1 @@\n-old\n+new") (d/link "src/a.clj" "src/a.clj" 3))}
   {:op :ui/notify :message "hi" :level :warn}])

(defn -main [& _]
  (let [reg (v/standard-registry)]
    (doseq [[k t] v/reference-targets]
      (println "==" k)
      (pprint (v/plan reg t sample-ops)))
    (pprint (v/plan reg (:emacs v/reference-targets) {:op :carto-flow/frame})))
  (shutdown-agents))
