(ns hive-vessel.render.ansi
  "Face to SGR projection for character-cell vessels: pure data, pure fns.

   A `:text` native carries `:text/face-lines` (RenderedLine: text plus face).
   Any surface that renders SGR can paint them, and `colourize` swaps a
   native's plain lines for painted ones. Content is sanitized before it is
   painted, so a line can never carry an escape of its own."
  (:require [clojure.string :as str]
            [hive-vessel.dialect.text :as text]
            [hive-vessel.schema :as s]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT

(def csi "[")

(def sgr-reset (str csi "0m"))

(def Sgr
  "SGR parameters, the part between CSI and `m`."
  [:re #"^[0-9;]+$"])

(def face-sgr
  "SGR parameters per hive-vessel face, bright 16-colour. nil paints nothing.
   Every entry stays a basic code, so a span costs the 3.0 tokens in
   `span-cost`, not the 5.0 an extended colour costs."
  {:title "1;95"
   :heading "1;96"
   :plain nil
   :muted "90"
   :info "94"
   :success "92"
   :warn "93"
   :error "1;91"
   :added "32"
   :removed "31"
   :hunk "36"
   :code "37"
   :link "4;94"})

(def face-sgr-256
  "SGR parameters per face, 256-colour. Costs the 5.0 tokens in `span-cost`
   per span, so it is for a surface the model does not read: a tmux pane, an
   editor buffer, a log. In a vessel the paint never enters a token stream at
   all, which is what makes the wider palette free there."
  {:title "1;38;5;213"
   :heading "1;38;5;117"
   :plain nil
   :muted "38;5;245"
   :info "38;5;75"
   :success "38;5;114"
   :warn "38;5;221"
   :error "1;38;5;203"
   :added "38;5;114"
   :removed "38;5;203"
   :hunk "38;5;146"
   :code "38;5;180"
   :link "4;38;5;81"})

(def ^:dynamic *palette*
  "The palette `paint-line` reads, per call. Bind it to widen or narrow the
   colour space for one surface without threading a palette through callers
   that have no opinion about it."
  face-sgr)

(def span-cost
  "Tokens one SGR span costs in Claude's tokenizer, by palette. Measured;
   see memory 20260916152640-711a6f05."
  {:basic 3.0 :extended 5.0})

(defn sanitize
  "S on one line with control characters removed, so content can neither
   break the one-line-per-record shape nor inject terminal escapes."
  [s]
  (-> (str s)
      (str/replace #"[\r\n]+" " ")
      (str/replace #"[\x00-\x1f\x7f]" "")))

(m/=> sanitize [:=> [:cat :string] :string])

(defn paint
  "S wrapped in the SGR sequence for SGR, or S unchanged when SGR is nil."
  [sgr s]
  (if sgr (str csi sgr "m" s sgr-reset) s))

(m/=> paint [:=> [:cat [:maybe Sgr] :string] :string])

(defn paint-line
  "A rendered line as terminal text: sanitized, painted by its face through
   the palette bound at call time."
  [{:keys [text face]}]
  (paint (get *palette* face) (sanitize text)))

(m/=> paint-line [:=> [:cat s/RenderedLine] :string])

(defn strip-ansi
  "S without SGR sequences."
  [s]
  (str/replace (str s) #"\x1b\[[0-9;]*m" ""))

(m/=> strip-ansi [:=> [:cat :string] :string])

(defn mark
  "TEXT painted for FACE, for a single in-band marker. An unknown face paints
   nothing rather than throwing, so a caller can pass a face this palette has
   no opinion about."
  [face text]
  (paint-line {:text text :face face}))

(m/=> mark [:=> [:cat :keyword :string] :string])

(defn colourize
  "PAYLOAD with its :text/lines painted from its :text/face-lines when it
   carries them, else with its lines sanitized. COLOUR? false sanitizes only,
   on either shape."
  ([payload] (colourize payload true))
  ([{:text/keys [face-lines] :as payload} colour?]
   (assoc payload :text/lines
          (cond
            (and colour? face-lines) (mapv paint-line face-lines)
            face-lines (mapv (comp sanitize :text) face-lines)
            :else (mapv sanitize (:text/lines payload))))))

(m/=> colourize [:function
                 [:=> [:cat text/Payload] text/Payload]
                 [:=> [:cat text/Payload :boolean] text/Payload]])

(defn span-count
  "How many SGR spans S opens. A span is one CSI sequence that sets a
   non-default style; resets are not counted, since they close a span that
   was already counted."
  [s]
  (count (filter #(not= "0" %)
                 (map second (re-seq #"\x1b\[([0-9;]*)m" (str s))))))

(m/=> span-count [:=> [:cat :string] [:int {:min 0}]])

(defn estimated-tokens
  "What the SGR in S costs a model reading it, in tokens. Extended (256
   colour) spans are priced separately because they cost more for identical
   information."
  [s]
  (let [params (map second (re-seq #"\x1b\[([0-9;]*)m" (str s)))
        live (remove #(= "0" %) params)
        extended? #(str/includes? % "38;5;")
        {ext true basic false} (group-by extended? live)]
    (+ (* (count basic) (:basic span-cost))
       (* (count ext) (:extended span-cost)))))

(m/=> estimated-tokens [:=> [:cat :string] number?])
