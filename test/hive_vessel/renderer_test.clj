(ns hive-vessel.renderer-test
  (:require [clojure.test :refer [deftest is]]
            [hive-spi.vessel :as port]
            [hive-vessel.renderer :as renderer]))

(deftest presentation-port-refuses-execution-authority
  (let [received (atom [])
        r (renderer/renderer ::test
            (fn [] {:vessel/id ::test :vessel/dialect :text
                    :vessel/execute! #(swap! received conj %)}))]
    (is (:error (port/render! r [{:op :ui/send-to-terminal :text "unsafe"}])))
    (is (:error (port/render! r [{:op :vim/ex :command "unsafe"}])))
    (is (empty? @received))
    (is (:ok (port/render! r [{:op :ui/notify :message "hello"}])))
    (is (= 1 (count @received)))))

(deftest stale-registration-cannot-remove-replacement
  (let [old (renderer/renderer ::lifecycle (constantly nil))
        replacement (renderer/renderer ::lifecycle (constantly nil))]
    (try
      (renderer/register! old)
      (renderer/register! replacement)
      (renderer/unregister! old)
      (is (identical? replacement (get @renderer/renderers ::lifecycle)))
      (is (:error (port/render! replacement [])))
      (finally (renderer/unregister! replacement)))
    (is (not (contains? @renderer/renderers ::lifecycle)))))
