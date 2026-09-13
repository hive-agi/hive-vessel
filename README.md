# hive-vessel

One action vocabulary for every place hive can show something: Emacs, Vim,
VS Code, a custom web harness, a terminal. An addon says *what* should happen
once; each vessel decides *how*.

Leaf library, cljc, depends on Clojure and malli only. MIT.

## The model

```text
addon intent     {:op :carto-flow/frame :frame {...}}
   │  translator shipped by the addon (plain data, no dependency on this lib)
   ▼
primitives       {:op :ui/show-panel :panel/id "carto-flow" :doc {...}}
   │  dialect translator (standard, or one a vessel registers)
   ▼
native ops       {:op :vessel/native :native/dialect :elisp :native/payload "(...)"}
   │  (:vessel/execute! target)
   ▼
the editor
```

Everything is an **op**, a map with a qualified `:op`. A **translator** is data:

```clojure
{:translator/id        :my-addon/frame->panel
 :translator/op        :my-addon/frame
 :translator/when      {:vessel/dialect :elisp :vessel/features #{:my-addon/native-ui}} ; optional
 :translator/priority  0                                                                ; optional
 :translator/accepts   [:map [:frame map?]]                                             ; optional malli
 :translator/translate (fn [op target] [{:op :ui/show-panel ...}])}
```

`hive-vessel.plan/plan` rewrites ops until only natives in the target's dialect
remain. Candidates are tried best-first (priority, then guard specificity, then
registration order); a translator that throws, returns junk, or produces
something unlowerable falls through to the next, so a vessel-specific
translator degrades to the generic one. Cycles and runaway depth are refused.

A **target** is the translation-relevant part of a vessel descriptor:

```clojure
{:vessel/id :emacs :vessel/dialect :elisp :vessel/features #{...} :vessel/execute! f}
```

## Standard primitives

| op                    | fields                                  |
|-----------------------|-----------------------------------------|
| `:ui/notify`          | `:message`, `:level` (info/warn/error)  |
| `:ui/show-panel`      | `:panel/id`, `:doc`                     |
| `:ui/close-panel`     | `:panel/id`                             |
| `:ui/open-file`       | `:file`, `:line`, `:column`             |
| `:ui/send-to-terminal`| `:terminal`, `:text`                    |

A `:doc` is a title plus blocks: `:heading`, `:para` (with `:tone`), `:fields`,
`:list`, `:code`, `:diff` (unified text or classified lines), `:link` (file and
line). `hive-vessel.doc/render-lines` projects a doc to face-tagged lines once;
every character-cell vessel paints those same lines.

## Standard dialects

| dialect        | vessels                     | payload                                  | escape hatch       |
|----------------|-----------------------------|------------------------------------------|--------------------|
| `:elisp`       | Emacs                       | self-contained Elisp source              | `:elisp/call`      |
| `:vim-channel` | Vim 8.2+/9                  | channel command `["call" fn args]`       | `:vim/call`, `:vim/ex` |
| `:json`        | VS Code, web harnesses      | JSON-able message, doc + rendered lines  | `:json/event`      |
| `:text`        | tmux, CLI, logs             | `{:text/lines [...]}`                    | none               |

Dialect calls quote their arguments as data (maps become alists in Elisp,
JSON values in Vim), so an addon can drive its own editor code without
building source strings.

A new vessel is one translator per primitive for its dialect. Overriding a
standard lowering for one vessel is a translator guarded by `:vessel/id`.

## Executors

- `hive-vessel.executor.emacsclient`: `(target {:socket-name "..."})`
- `hive-vessel.executor.vim-channel`: `(start!)`, then in Vim
  `:HiveVesselConnect 127.0.0.1:<port>` (plugin in `resources/hive-vessel/vim`),
  `(await-vim! server ms)`, `(target server)`
  One attached Vim. For several Vim sessions, a hello handshake, reconnection
  to a restarted hive, correlated concurrent calls and inbound events, use
  `hive-vim.vessel/vessel-target` instead: a drop-in target for this same
  dialect.
- `hive-vessel.executor.sse`: the shared transport for `:json` vessels (a
  DeepSeek Harness page, a VS Code extension host, a web harness).
  `(start! {:port p :token t})`, then `(executor bridge)` as `:vessel/execute!`.
  Clients read `GET /vessel/events` (`event: vessel`, one JSON line per op),
  answer on `POST /vessel/reply`, and poll `GET /vessel/health`. A browser
  must come from an allowed Origin (loopback by default); a non-browser
  client sends no Origin and is gated by the token alone. The latest
  `ui/show-panel` per panel id is replayed to clients that connect late.

A host with its own bridge (hive-emacs's eval port, a VS Code JSON-lines pipe,
a websocket) injects its own `:vessel/execute!`.

## Addons

An addon exposes translators under the IAddon hook `:vessel/translators` (a
collection or a zero-arg fn). `hive-vessel.core/registry-from-hooks` builds the
standard registry extended with them. Because translators are data, an addon
does not need this library on its classpath to contribute them.

```clojure
(require '[hive-vessel.core :as v])

(def reg (v/standard-registry my-addon/translators))

(v/dispatch! reg emacs-target {:op :my-addon/frame :frame f})
(v/broadcast! reg [emacs-target vim-target web-target] {:op :my-addon/frame :frame f})

;; any consumer that takes a delivery fn; presenter envelopes
;; {:type T :payload P} become {:op T :payload P}
(v/sink reg vim-target v/envelope->op)
```

## Tests

```bash
clojure -M:test          # unit, property and mutation
clojure -M:integration   # real editors: emacs --batch, an Emacs daemon, headless Vim
```

The integration suite evaluates generated panels in Emacs and Vim and checks
that both paint exactly `render-lines`, reads generated Elisp and JSON literals
back, and drives a daemon and a connected Vim through the executors.
