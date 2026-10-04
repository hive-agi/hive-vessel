(ns hive-vessel.layout.cells
  "Pure character-cell measurement and clipping for terminal layouts.")

(defn codepoints [text]
  #?(:clj (map #(String. (Character/toChars (int %))) (.toArray (.codePoints ^String (str text))))
     :cljs (js/Array.from (str text))))

(defn cell-width [character]
  (let [n #?(:clj (.codePointAt ^String character 0)
             :cljs (.codePointAt character 0))]
    (cond
      (or (< n 32) (<= 127 n 159)
          (<= 0x300 n 0x36f) (<= 0x1ab0 n 0x1aff)
          (<= 0xfe00 n 0xfe0f)) 0
      (or (<= 0x1100 n 0x115f) (<= 0x2e80 n 0xa4cf)
          (<= 0xac00 n 0xd7a3) (<= 0xf900 n 0xfaff)
          (<= 0xfe10 n 0xfe6f) (<= 0xff01 n 0xff60)
          (<= 0x1f300 n 0x1faff)) 2
      :else 1)))

(defn display-width [text] (reduce + (map cell-width (codepoints text))))

(defn clip [text width]
  (first (reduce (fn [[out remaining] c]
                   (if (<= (cell-width c) remaining)
                     [(str out c) (- remaining (cell-width c))]
                     (reduced [out remaining])))
                 ["" (max 0 width)] (codepoints text))))

(defn pad [text width]
  (str text (apply str (repeat (max 0 (- width (display-width text))) " "))))
