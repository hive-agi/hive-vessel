(ns hive-vessel.editor-integration-test
  "Real round-trips: the generated Elisp is evaluated by `emacs --batch`, the
   Vim channel payload is executed by headless Vim through the bundled
   plugin. The subject IS the editor integration, hence ^:integration."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-vessel.core :as v]
            [hive-vessel.dialect.elisp :as elisp]
            [hive-vessel.doc :as d]
            [hive-vessel.schema :as s]
            [hive-vessel.wire :as wire]
            [malli.generator :as mg])
  (:import (java.io File)
           (java.util.concurrent TimeUnit)))

;; SPDX-License-Identifier: MIT

(defn- sh [& args]
  (let [p (-> (ProcessBuilder. ^java.util.List (vec args))
              (.redirectErrorStream false)
              (.start))
        out (future (slurp (.getInputStream p)))
        err (future (slurp (.getErrorStream p)))]
    (when-not (.waitFor p 60 TimeUnit/SECONDS)
      (.destroyForcibly p)
      (throw (ex-info "editor timed out" {:args args})))
    {:exit (.exitValue p) :out @out :err @err}))

(defn- temp-file [suffix content]
  (let [f (File/createTempFile "hive-vessel" suffix)]
    (.deleteOnExit f)
    (spit f content)
    f))

(def sample-doc
  (d/doc "Carto \"Flow\" \\ #3"
         (d/heading "apply write-form")
         (d/para "succeeded" :success)
         (d/fields [["paths" "src/a.clj\nsrc/b.clj"] ["verify" "ok"]])
         (d/code "(defn f [] \"x\")" "clojure")
         (d/diff "@@ -1,2 +1,2 @@\n-(old)\n+(new ü)\n context")
         (d/link "open a" "/tmp/a.clj" 2)))

(defn- generated-docs [n]
  (gen/sample (mg/generator s/Doc) n))

(defn- emacs-panel-text [doc]
  (let [code (get-in (v/plan (v/standard-registry) (:emacs v/reference-targets)
                             {:op :ui/show-panel :panel/id "t" :doc doc})
                     [:ok :plan/ops 0 :native/payload])
        f (temp-file ".el" (str "(with-current-buffer " code
                                " (let ((coding-system-for-write 'utf-8))"
                                " (write-region (point-min) (point-max) (getenv \"HIVE_OUT\"))))"))
        out (File/createTempFile "hive-vessel" ".out")]
    (.deleteOnExit out)
    (let [pb (doto (ProcessBuilder. ["emacs" "--batch" "-Q" "--eval"
                                     "(setq coding-system-for-read 'utf-8)"
                                     "-l" (.getPath f)])
               (-> .environment (.put "HIVE_OUT" (.getPath out))))
          p (.start pb)
          err (future (slurp (.getErrorStream p)))]
      (.waitFor p 60 TimeUnit/SECONDS)
      {:exit (.exitValue p) :err @err :text (slurp out :encoding "UTF-8")})))

(defn- expected-buffer [doc]
  (apply str (map #(str (:text %) "\n") (d/render-lines doc))))

(deftest ^:integration emacs-paints-exactly-the-rendered-lines
  (testing "hand-picked doc with quotes, backslashes, links, unicode"
    (let [{:keys [exit err text]} (emacs-panel-text sample-doc)]
      (is (zero? exit) err)
      (is (= (expected-buffer sample-doc) text))))
  (testing "generated docs"
    (doseq [doc (generated-docs 12)]
      (let [{:keys [exit err text]} (emacs-panel-text doc)]
        (is (zero? exit) err)
        (is (= (expected-buffer doc) text) (pr-str doc))))))

(deftest ^:integration elisp-literals-read-back
  (let [strings (conj (gen/sample gen/string 40)
                      "" "\"" "\\" "a\nb\tc" (str (char 0) (char 27) (char 127)) "ünï ✓")
        ;; Each string travels twice: as an Elisp literal, and as the Elisp
        ;; literal of its write-json encoding. Emacs decodes the JSON and
        ;; the two must agree -- which checks both writers at once.
        g (temp-file ".el"
                     (str "(require 'json)"
                          "(let ((expected (list "
                          (str/join " " (map #(elisp/string-literal (wire/write-json %)) strings))
                          ")) (actual (list "
                          (str/join " " (map elisp/string-literal strings))
                          ")))"
                          " (princ (if (equal (mapcar #'json-read-from-string expected) actual) \"SAME\" \"DIFF\")))"))
        r (sh"emacs" "--batch" "-Q" "-l" (.getPath g))]
    (is (zero? (:exit r)) (:err r))
    (is (= "SAME" (:out r))))
  (testing "data literals keep keys addressable by symbol"
    (let [code (str "(princ (let ((m '" (elisp/data-literal {:frame/phase :apply :paths ["a" "b"] "1" 2 "a b" true}) "))"
                    " (list (alist-get 'frame/phase m) (alist-get 'paths m) (alist-get '\\1 m) (alist-get 'a\\ b m))))")
          r (sh"emacs" "--batch" "-Q" "--eval" code)]
      (is (zero? (:exit r)) (:err r))
      (is (= "(apply (a b) 2 t)" (:out r))))))

(def vim-rtp (.getPath (io/file "resources/hive-vessel/vim")))

(defn- vim-exec-payload
  "Execute one channel payload the way Vim's JSON channel does, then write
   panel `t`'s lines to a file."
  [payload]
  (let [in (temp-file ".json" (wire/write-json payload))
        out (File/createTempFile "hive-vessel" ".out")
        errs (File/createTempFile "hive-vessel" ".err")
        script (temp-file ".vim"
                          (str "set nocompatible\n"
                               "let &rtp = " (wire/write-json vim-rtp) " . ',' . &rtp\n"
                               "let g:hive_vessel_headless = 1\n"
                               "let p = json_decode(join(readfile(" (wire/write-json (.getPath in)) "), \"\\n\"))\n"
                               "try\n"
                               "  call call(p[1], p[2])\n"
                               "catch\n"
                               "  call writefile([v:exception, v:throwpoint], " (wire/write-json (.getPath errs)) ")\n"
                               "endtry\n"
                               "call writefile(hive_vessel#panel_lines('t'), " (wire/write-json (.getPath out)) ")\n"
                               "if v:errmsg != ''\n"
                               "  call writefile(['errmsg: ' . v:errmsg], " (wire/write-json (.getPath errs)) ", 'a')\n"
                               "endif\n"
                               "qall!\n"))
        r (sh "vim" "-N" "-u" "NONE" "-i" "NONE" "-n" "-es" "-S" (.getPath script))]
    (.deleteOnExit out)
    (.deleteOnExit errs)
    (assoc r
           :text (slurp out)
           :vim-error (not-empty (slurp errs)))))

(deftest ^:integration vim-paints-exactly-the-rendered-lines
  (doseq [doc (cons sample-doc (generated-docs 8))]
    (let [payload (get-in (v/plan (v/standard-registry) (:vim v/reference-targets)
                                  {:op :ui/show-panel :panel/id "t" :doc doc})
                          [:ok :plan/ops 0 :native/payload])
          {:keys [exit err text vim-error]} (vim-exec-payload payload)]
      (is (nil? vim-error) (str vim-error (pr-str doc)))
      (is (zero? exit) err)
      (is (= (expected-buffer doc) text) (pr-str doc)))))

(deftest ^:integration vim-panel-ids-with-a-slash-are-not-read-as-paths
  ;; Measured 2026-09-13 by the hive-olympus-vim e2e: a panel id like
  ;; olympus/tab-2 named the buffer hive://olympus/tab-2, which Vim announced
  ;; as "[New DIRECTORY]" on load. Silent -es mode hides file messages, so
  ;; this runs a terminal Vim and reads its raw output.
  (let [payload (get-in (v/plan (v/standard-registry) (:vim v/reference-targets)
                                {:op :ui/show-panel :panel/id "olympus/tab-2" :doc sample-doc})
                        [:ok :plan/ops 0 :native/payload])
        in (temp-file ".json" (wire/write-json payload))
        out (File/createTempFile "hive-vessel" ".out")
        script (temp-file ".vim"
                          (str "set nocompatible\n"
                               "let &rtp = " (wire/write-json vim-rtp) " . ',' . &rtp\n"
                               "let g:hive_vessel_headless = 1\n"
                               "let p = json_decode(join(readfile(" (wire/write-json (.getPath in)) "), \"\\n\"))\n"
                               "call call(p[1], p[2])\n"
                               "call writefile(hive_vessel#panel_lines('olympus/tab-2'), " (wire/write-json (.getPath out)) ")\n"
                               "qall!\n"))
        p (-> (ProcessBuilder. ["vim" "-N" "-u" "NONE" "-i" "NONE" "-n" "--not-a-term" "-T" "dumb" "-S" (.getPath script)])
              (.redirectErrorStream true)
              (.start))
        raw (future (slurp (.getInputStream p)))]
    (.deleteOnExit out)
    (when-not (.waitFor p 60 TimeUnit/SECONDS)
      (.destroyForcibly p)
      (throw (ex-info "vim timed out" {})))
    (is (zero? (.exitValue p)))
    (is (not (str/includes? @raw "New DIRECTORY")) @raw)
    (is (= (expected-buffer sample-doc) (slurp out)))))

(def nvim-binary
  "Neovim on PATH, else the tarball the carto-flow-nvim tests fetched."
  (let [cached (io/file (System/getProperty "user.home") ".cache" "hive-carto-flow-nvim" "nvim-0.12.5" "bin" "nvim")]
    (if (.canExecute cached) (.getPath cached) "nvim")))

(defn- nvim-exec-payload
  "Execute one :nvim-rpc payload the way an rpc peer would (the API method
   applied to its params) in a headless Neovim loading the bundled autoload,
   then write panel `t`'s lines and its extmark count to files."
  [{:nvim/keys [method params]}]
  (let [in (temp-file ".json" (wire/write-json params))
        out (File/createTempFile "hive-vessel" ".out")
        marks (File/createTempFile "hive-vessel" ".marks")
        errs (File/createTempFile "hive-vessel" ".err")
        script (temp-file ".vim"
                          (str "set nocompatible\n"
                               "let &rtp = " (wire/write-json vim-rtp) " . ',' . &rtp\n"
                               "let g:hive_vessel_headless = 1\n"
                               "let p = json_decode(join(readfile(" (wire/write-json (.getPath in)) "), \"\\n\"))\n"
                               "try\n"
                               "  call " method "(p[0], p[1])\n"
                               "catch\n"
                               "  call writefile([v:exception, v:throwpoint], " (wire/write-json (.getPath errs)) ")\n"
                               "endtry\n"
                               "call writefile(hive_vessel#panel_lines('t'), " (wire/write-json (.getPath out)) ")\n"
                               "call writefile([string(len(nvim_buf_get_extmarks(bufnr('hive://t'), nvim_create_namespace('hive_vessel'), 0, -1, {})))], " (wire/write-json (.getPath marks)) ")\n"
                               "qall!\n"))
        r (sh nvim-binary "--headless" "-u" "NONE" "-i" "NONE" "-n" "-S" (.getPath script))]
    (doseq [f [out marks errs]] (.deleteOnExit f))
    (assoc r
           :text (slurp out)
           :marks (parse-long (str/trim (slurp marks)))
           :vim-error (not-empty (slurp errs)))))

(defn- highlighted-lines
  "How many rendered lines of DOC carry a mapped face on non-empty text."
  [doc]
  (count (filter #(and (not= :plain (:face %)) (seq (:text %))) (d/render-lines doc))))

(deftest ^:integration nvim-paints-exactly-the-rendered-lines-and-highlights-them
  (doseq [doc (cons sample-doc (generated-docs 8))]
    (let [payload (get-in (v/plan (v/standard-registry) (:neovim v/reference-targets)
                                  {:op :ui/show-panel :panel/id "t" :doc doc})
                          [:ok :plan/ops 0 :native/payload])
          {:keys [exit err text marks vim-error]} (nvim-exec-payload payload)]
      (is (nil? vim-error) (str vim-error (pr-str doc)))
      (is (zero? exit) err)
      (is (= (expected-buffer doc) text) (pr-str doc))
      (is (= (highlighted-lines doc) marks) (pr-str doc)))))

(deftest ^:integration vim-decodes-write-json-losslessly
  (let [data {"s" "q\"b\\n\nt\t\u0001ü" "n" [1 -2 3.5 nil true false] "m" {"k/x" []}}
        in (temp-file ".json" (wire/write-json data))
        out (File/createTempFile "hive-vessel" ".out")
        script (temp-file ".vim"
                          (str "let v = json_decode(join(readfile(" (wire/write-json (.getPath in)) "), \"\\n\"))\n"
                               "call writefile([string(v.s == \"q\\\"b\\\\n\\nt\\t\\x01ü\"), string(v.n[2]), string(v.m)], "
                               (wire/write-json (.getPath out)) ")\n"
                               "qall!\n"))
        r (sh"vim" "-N" "-u" "NONE" "-i" "NONE" "-n" "-es" "-S" (.getPath script))]
    (is (zero? (:exit r)) (:err r))
    (is (= ["1" "3.5" "{'k/x': []}"] (str/split-lines (slurp out))))))

(deftest ^:integration vim-with-a-stale-ops-script-reports-it-instead-of-erroring
  ;; A Vim that loaded an older hive_vessel/ops.vim from another directory
  ;; keeps it (Vim refuses a second autoload definition, E1073). Sourcing the
  ;; current wire.vim over it then calls the old OnEvent with a new signature;
  ;; measured 2026-09-13 as E118 in a user's Vim. wire.vim must name the
  ;; staleness in Status() and leave v:errmsg alone.
  (let [old-rtp (.toFile (java.nio.file.Files/createTempDirectory "hive-vessel-old" (make-array java.nio.file.attribute.FileAttribute 0)))
        old-ops (io/file old-rtp "autoload" "hive_vessel" "ops.vim")
        out (File/createTempFile "hive-vessel" ".out")
        script (temp-file ".vim"
                          (str "vim9script\n"
                               "set nocompatible\n"
                               "g:hive_vessel_autoconnect = 0\n"
                               "g:hive_vessel_headless = 1\n"
                               "execute 'set runtimepath+=' .. fnameescape(" (wire/write-json (.getPath old-rtp)) ")\n"
                               "import autoload 'hive_vessel/ops.vim' as oldops\n"
                               "oldops.Surfaces()\n"
                               "execute 'set runtimepath+=' .. fnameescape(" (wire/write-json vim-rtp) ")\n"
                               "try\n"
                               "  source " vim-rtp "/autoload/hive_vessel/ops.vim\n"
                               "catch\n"
                               "endtry\n"
                               "source " vim-rtp "/autoload/hive_vessel/wire.vim\n"
                               "writefile(['error=' .. hive_vessel#wire#Status().error, 'errmsg=' .. v:errmsg], " (wire/write-json (.getPath out)) ")\n"
                               "qall!\n"))]
    (.deleteOnExit out)
    (io/make-parents old-ops)
    (spit old-ops "vim9script\nexport def OnEvent()\nenddef\nexport def Surfaces(): list<string>\n  return ['editor']\nenddef\n")
    (let [r (sh "vim" "-N" "-u" "NONE" "-i" "NONE" "-n" "-es" "-S" (.getPath script))
          [error errmsg] (str/split-lines (slurp out))]
      (is (zero? (:exit r)) (:err r))
      (is (str/starts-with? error "error=stale hive_vessel/ops.vim loaded; restart Vim") error)
      (is (str/includes? error "E118") error)
      (is (= "errmsg=" errmsg)))))
