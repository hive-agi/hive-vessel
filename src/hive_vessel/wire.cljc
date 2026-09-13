(ns hive-vessel.wire
  "JSON-safe projection and a dependency-free JSON writer, shared by every
   dialect that crosses a process boundary as JSON (Vim channels, VS Code,
   web harnesses)."
  (:require [clojure.string :as str]))

;; SPDX-License-Identifier: MIT

(defn key-name
  "`:frame/phase` -> \"frame/phase\"; strings pass through; anything else is
   printed."
  [k]
  (cond
    (keyword? k) (if-let [n (namespace k)] (str n "/" (name k)) (name k))
    (string? k) k
    :else (str k)))

(defn ->json-data
  "Walk X into JSON-representable data: keywords and symbols become strings,
   map keys become strings, sets and seqs become vectors."
  [x]
  (cond
    (map? x) (into {} (map (fn [[k v]] [(key-name k) (->json-data v)])) x)
    (keyword? x) (key-name x)
    (symbol? x) (str x)
    (or (sequential? x) (set? x)) (mapv ->json-data x)
    :else x))

(defn- escape-char [c]
  (let [code #?(:clj (int c) :cljs (.charCodeAt c 0))]
    (case code
      34 "\\\""
      92 "\\\\"
      10 "\\n"
      13 "\\r"
      9 "\\t"
      8 "\\b"
      12 "\\f"
      (if (or (< code 0x20) (= code 0x2028) (= code 0x2029))
        (let [h #?(:clj (Integer/toHexString code) :cljs (.toString code 16))]
          (str "\\u" (subs (str "0000" h) (count h))))
        (str c)))))

(defn- json-string [s]
  (str "\"" (apply str (map escape-char s)) "\""))

(defn- json-number [n]
  #?(:clj (cond
            (integer? n) (str n)
            (ratio? n) (json-number (double n))
            (or (Double/isNaN (double n)) (Double/isInfinite (double n))) "null"
            :else (str/lower-case (str (double n))))
     :cljs (if (js/isFinite n) (str n) "null")))

(defn write-json
  "Serialize X (after `->json-data`) as a compact JSON string."
  [x]
  (let [x (->json-data x)]
    (cond
      (nil? x) "null"
      (true? x) "true"
      (false? x) "false"
      (string? x) (json-string x)
      (number? x) (json-number x)
      (map? x) (str "{" (str/join "," (map (fn [[k v]] (str (json-string k) ":" (write-json v))) x)) "}")
      (vector? x) (str "[" (str/join "," (map write-json x)) "]")
      :else (json-string (str x)))))
