(ns hive-vessel.dialect.elisp
  "The :elisp dialect: Emacs Lisp source strings, executed by any executor
   that evaluates a form in a running Emacs (emacsclient, a CIDER bridge,
   hive-emacs's eval port).

   Standard primitives lower to self-contained forms using only built-in
   Emacs functions, so a bare Emacs can paint a panel with no package loaded.

   Dialect call: `{:op :elisp/call :fn \"flow-ingest\" :args [data ...]}` --
   the typed escape hatch for addons that ship their own Elisp. Arguments are
   quoted data (maps become alists with symbol keys), never spliced code."
  (:require [clojure.string :as str]
            [hive-vessel.doc :as doc]
            [hive-vessel.schema :as s]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def dialect :elisp)

;; =============================================================================
;; Literals
;; =============================================================================

(defn- char-code [c] #?(:clj (int c) :cljs (.charCodeAt c 0)))

(defn- hex4 [n]
  (let [h #?(:clj (Integer/toHexString n) :cljs (.toString n 16))]
    (subs (str "0000" h) (count h))))

(defn string-literal
  "S as an Elisp string literal that reads back to S."
  [s]
  (str "\""
       (apply str (map (fn [c]
                         (let [code (char-code c)]
                           (cond
                             (= c \") "\\\""
                             (= c \\) "\\\\"
                             (= code 10) "\\n"
                             (= code 9) "\\t"
                             (or (< code 0x20) (= code 0x7f)) (str "\\u" (hex4 code))
                             :else (str c))))
                       s))
       "\""))

(def ^:private symbol-safe #"[A-Za-z0-9\-_/+*=<>!$%&^~]")

(defn symbol-literal
  "NAME as an Elisp symbol literal that reads back to the symbol named NAME."
  [name]
  (cond
    (= "" name) "##"
    :else
    (let [escaped (apply str (map (fn [c]
                                    (let [c (str c)]
                                      (if (re-matches symbol-safe c) c (str "\\" c))))
                                  name))]
      (if (re-matches #"[-+]?[0-9]*\.?[0-9]+(e[-+]?[0-9]+)?|[-+]?[0-9]+\.?" escaped)
        (str "\\" escaped)
        escaped))))

(defn- key-name [k]
  (cond
    (keyword? k) (if-let [n (namespace k)] (str n "/" (name k)) (name k))
    (string? k) k
    :else (str k)))

(defn- number-literal [n]
  #?(:clj (cond
            (integer? n) (str n)
            (ratio? n) (number-literal (double n))
            (or (Double/isNaN (double n)) (Double/isInfinite (double n))) "nil"
            :else (str/lower-case (str (double n))))
     :cljs (if (js/isFinite n) (str n) "nil")))

(defn data-literal
  "X as an unquoted Elisp data literal: maps become alists keyed by symbols,
   sequences become lists, keywords become strings, false and nil become nil.
   Quote the result (`'`) at the call site."
  [x]
  (cond
    (nil? x) "nil"
    (true? x) "t"
    (false? x) "nil"
    (string? x) (string-literal x)
    (keyword? x) (string-literal (key-name x))
    (symbol? x) (string-literal (str x))
    (number? x) (number-literal x)
    (map? x) (str "(" (str/join " " (map (fn [[k v]]
                                            (str "(" (symbol-literal (key-name k)) " . " (data-literal v) ")"))
                                          x))
                  ")")
    (or (sequential? x) (set? x)) (str "(" (str/join " " (map data-literal x)) ")")
    :else (string-literal (str x))))

;; =============================================================================
;; Primitive lowering
;; =============================================================================

(defn native [code] {:op :vessel/native :native/dialect dialect :native/payload code})

(def face->elisp
  {:title "'(:inherit bold :height 1.2)"
   :heading "'bold"
   :plain nil
   :muted "'shadow"
   :info "'font-lock-function-name-face"
   :success "'success"
   :warn "'warning"
   :error "'error"
   :added "'diff-added"
   :removed "'diff-removed"
   :hunk "'diff-hunk-header"
   :code "'font-lock-constant-face"
   :link "'link"})

(defn panel-buffer-name [panel-id] (str "*hive:" panel-id "*"))

(defn- goto-form [file line column other-window?]
  (str "(progn (" (if other-window? "find-file-other-window" "find-file") " " (string-literal file) ")"
       " (goto-char (point-min))"
       (when line (str " (forward-line " (dec line) ")"))
       (when column (str " (move-to-column " (dec column) ")"))
       " (buffer-name))"))

(defn- line-form [{:keys [text face file line]}]
  (if file
    (str "(insert-text-button " (string-literal text)
         " 'follow-link t 'action (lambda (_button) " (goto-form file line nil true) "))"
         " (insert \"\\n\")")
    (if-let [f (face->elisp face)]
      (str "(insert (propertize " (string-literal text) " 'face " f ") \"\\n\")")
      (str "(insert " (string-literal text) " \"\\n\")"))))

(defn show-panel-code [{:keys [doc] panel-id :panel/id}]
  (str "(let ((buf (get-buffer-create " (string-literal (panel-buffer-name panel-id)) ")))"
       " (require 'diff-mode nil t)"
       " (with-current-buffer buf"
       " (special-mode)"
       " (let ((inhibit-read-only t))"
       " (erase-buffer) "
       (str/join " " (map line-form (doc/render-lines doc)))
       ")"
       " (goto-char (point-min)))"
       " (unless noninteractive (display-buffer buf))"
       " (buffer-name buf))"))

(defn notify-code [{:keys [message level]}]
  (case (or level :info)
    :info (str "(message \"%s\" " (string-literal message) ")")
    :warn (str "(display-warning 'hive " (string-literal message) " :warning)")
    :error (str "(display-warning 'hive " (string-literal message) " :error)")))

(defn close-panel-code [{panel-id :panel/id}]
  (str "(let ((buf (get-buffer " (string-literal (panel-buffer-name panel-id)) ")))"
       " (when buf (quit-windows-on buf t) (when (buffer-live-p buf) (kill-buffer buf)) t))"))

(defn open-file-code [{:keys [file line column]}]
  (goto-form file line column false))

(defn send-to-terminal-code [{:keys [terminal text]}]
  (str "(let* ((buf (or (get-buffer " (string-literal terminal) ") (error \"no terminal buffer %s\" " (string-literal terminal) ")))"
       " (text " (string-literal text) "))"
       " (with-current-buffer buf"
       " (cond ((and (derived-mode-p 'vterm-mode) (fboundp 'vterm-send-string)) (vterm-send-string text))"
       " ((derived-mode-p 'term-mode) (term-send-raw-string text))"
       " (t (process-send-string (get-buffer-process buf) text))))"
       " t)"))

(defn call-code [{f :fn args :args}]
  (str "(" (symbol-literal f)
       (apply str (map (fn [a] (str " '" (data-literal a))) args))
       ")"))

(def Code
  "An :elisp native payload: one self-contained form as source text."
  [:string {:min 1}])

(def Call
  "The :elisp/call dialect call: a named function applied to quoted data."
  [:map [:op [:= :elisp/call]] [:fn s/NonBlank] [:args {:optional true} [:sequential :any]]])

(m/=> notify-code [:=> [:cat s/Notify] Code])
(m/=> show-panel-code [:=> [:cat s/ShowPanel] Code])
(m/=> close-panel-code [:=> [:cat s/ClosePanel] Code])
(m/=> open-file-code [:=> [:cat s/OpenFile] Code])
(m/=> send-to-terminal-code [:=> [:cat s/SendToTerminal] Code])
(m/=> call-code [:=> [:cat Call] Code])

(def ^:private when-elisp {:vessel/dialect dialect})

(defn- lowering [op f]
  {:translator/id (keyword "hive-vessel.elisp" (name op))
   :translator/op op
   :translator/when when-elisp
   :translator/translate (fn [o _target] (native (f o)))})

(def translators
  ;; Each lowering is reached through its var at call time, never captured.
  [(lowering :ui/notify #(notify-code %))
   (lowering :ui/show-panel #(show-panel-code %))
   (lowering :ui/close-panel #(close-panel-code %))
   (lowering :ui/open-file #(open-file-code %))
   (lowering :ui/send-to-terminal #(send-to-terminal-code %))
   (assoc (lowering :elisp/call #(call-code %))
          :translator/accepts [:map [:fn [:string {:min 1}]] [:args {:optional true} [:sequential :any]]])])
