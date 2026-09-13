(ns hive-vessel.wire.reader
  "A dependency-free JSON reader, the inverse of hive-vessel.wire/write-json
   for JSON-representable data: objects become maps with string keys, arrays
   vectors, integers longs (BigInt past the long range), other numbers
   doubles. Malformed input throws ex-info.")

;; SPDX-License-Identifier: MIT

#?(:clj
   (defn- parse-error [msg i]
     (ex-info (str "read-json: " msg) {:at i})))

#?(:clj
   (defn- skip-ws ^long [^String s ^long i]
     (loop [i i]
       (if (and (< i (.length s)) (Character/isWhitespace (.charAt s i)))
         (recur (inc i))
         i))))

#?(:clj
   (defn- parse-string
     "[string index-after-closing-quote], I at the opening quote."
     [^String s ^long i]
     (let [sb (StringBuilder.)]
       (loop [i (inc i)]
         (when (>= i (.length s)) (throw (parse-error "unterminated string" i)))
         (let [c (.charAt s i)]
           (cond
             (= c \") [(str sb) (inc i)]

             (= c \\)
             (let [e (.charAt s (inc i))]
               (case e
                 \" (do (.append sb \") (recur (+ i 2)))
                 \\ (do (.append sb \\) (recur (+ i 2)))
                 \/ (do (.append sb \/) (recur (+ i 2)))
                 \b (do (.append sb \backspace) (recur (+ i 2)))
                 \f (do (.append sb \formfeed) (recur (+ i 2)))
                 \n (do (.append sb \newline) (recur (+ i 2)))
                 \r (do (.append sb \return) (recur (+ i 2)))
                 \t (do (.append sb \tab) (recur (+ i 2)))
                 \u (do (.append sb (char (Integer/parseInt (subs s (+ i 2) (+ i 6)) 16)))
                        (recur (+ i 6)))
                 (throw (parse-error (str "bad escape \\" e) i))))

             (< (int c) 0x20) (throw (parse-error "control character in string" i))

             :else (do (.append sb c) (recur (inc i)))))))))

#?(:clj
   (defn- parse-number [^String s ^long i]
     (let [n (.length s)
           j (loop [j i]
               (if (and (< j n)
                        (let [c (.charAt s j)]
                          (or (Character/isDigit c)
                              (= c \-) (= c \+) (= c \.) (= c \e) (= c \E))))
                 (recur (inc j))
                 j))
           tok (subs s i j)]
       (when (= i j) (throw (parse-error (str "unexpected character " (.charAt s i)) i)))
       [(if (re-find #"[.eE]" tok)
          (Double/parseDouble tok)
          (try (Long/parseLong tok)
               (catch NumberFormatException _ (bigint tok))))
        j])))

#?(:clj (declare parse-value))

#?(:clj
   (defn- parse-array
     "[vector index-after-closing-bracket], I just past the opening bracket."
     [^String s ^long i]
     (let [i (skip-ws s i)]
       (if (= \] (.charAt s i))
         [[] (inc i)]
         (loop [i i acc (transient [])]
           (let [[v i] (parse-value s i)
                 acc (conj! acc v)
                 i (skip-ws s i)
                 c (.charAt s i)]
             (case c
               \, (recur (inc i) acc)
               \] [(persistent! acc) (inc i)]
               (throw (parse-error "expected , or ]" i)))))))))

#?(:clj
   (defn- parse-object
     "[map index-after-closing-brace], I just past the opening brace."
     [^String s ^long i]
     (let [i (skip-ws s i)]
       (if (= \} (.charAt s i))
         [{} (inc i)]
         (loop [i i acc (transient {})]
           (let [i (skip-ws s i)]
             (when-not (= \" (.charAt s i)) (throw (parse-error "expected a string key" i)))
             (let [[k i] (parse-string s i)
                   i (skip-ws s i)]
               (when-not (= \: (.charAt s i)) (throw (parse-error "expected :" i)))
               (let [[v i] (parse-value s (inc i))
                     acc (assoc! acc k v)
                     i (skip-ws s i)
                     c (.charAt s i)]
                 (case c
                   \, (recur (inc i) acc)
                   \} [(persistent! acc) (inc i)]
                   (throw (parse-error "expected , or }" i)))))))))))

#?(:clj
   (defn- parse-value [^String s ^long i]
     (let [i (skip-ws s i)]
       (when (>= i (.length s)) (throw (parse-error "unexpected end of input" i)))
       (let [c (.charAt s i)]
         (cond
           (= c \{) (parse-object s (inc i))
           (= c \[) (parse-array s (inc i))
           (= c \") (parse-string s i)
           (.startsWith s "true" i) [true (+ i 4)]
           (.startsWith s "false" i) [false (+ i 5)]
           (.startsWith s "null" i) [nil (+ i 4)]
           :else (parse-number s i))))))

(defn read-json
  "Parse JSON TEXT into data. Throws ex-info on malformed input, including
   trailing characters after the value."
  [text]
  #?(:clj
     (try
       (let [[v i] (parse-value text 0)
             i (skip-ws text i)]
         (when (< i (.length ^String text)) (throw (parse-error "trailing characters" i)))
         v)
       (catch StringIndexOutOfBoundsException _
         (throw (parse-error "unexpected end of input" (count text)))))
     :cljs
     (try
       (js->clj (js/JSON.parse text))
       (catch :default e
         (throw (ex-info (str "read-json: " (ex-message e)) {:at nil}))))))
