# GUI capability matrix <!-- omit from toc -->

What bbt can do on a GUI today, on which platform, through which
toolkit: one row per vocabulary entry, one column per backend and
platform, one comment column for what the cell cannot say.

This page does not specify the vocabulary — every row links to its
feature page in [gui_features.md](gui_features.md) — nor the crates,
which are in [backends.md](backends.md). It deliberately carries no
test column: the scenarios that exercise a feature are listed under
`Verified by` on its feature page, and the feature × test coverage is
a separate matrix. Here, a cell is about the implementation.

## The cells

A cell is one token, optionally followed by ` — ` and a short note;
the note qualifies the state, it never changes it.

| Token   | Meaning |
|---------|---------|
| `yes`   | implemented by that backend on that platform |
| `?`     | implemented, platform unproven: the backend declares the platform, but no run there is recorded |
| `no`    | not implemented by that backend: the step still parses, and fails at execution with a diagnostic |
| `n/a`   | out of reach of the platform or of the toolkit |

Markdown tables carry no checkbox (GFM renders `- [x]` in list items
only), so the state is a word. A `?` is about the platform, not about
the entry: it becomes `yes` when a run on that platform is recorded,
never because the code looks right. `yes`, in turn, is a statement
about the code — whether a scenario exercises the entry is the coverage
matrix's business, not this page's.

## Backends, platforms, toolkits

The GUI surface is what the backend reads and drives; the transport is
how it reaches it. The columns of the matrix below are the pairs a
backend declares (`[available]` in its manifest): a backend that
cannot run on a platform has no column there, so no cell ever means
"this backend does not exist here".

| Backend  | GUI surface | Transport | Declared platforms | Prerequisites |
|----------|-------------|-----------|--------------------|---------------|
| `gtk4`   | GTK 4 widgets, as exposed by the toolkit | AT-SPI (`libatspi`), through the session accessibility bus | Linux (X11, Wayland) | `--backend=gtk4`; a running accessibility bus (`at-spi2-core`); a display — the CI uses Xvfb |
| `webapp` | a web page, not a widget tree: role, accessible name and content are computed on the DOM by injected scripts | W3C WebDriver: `chromedriver` steers a chromium/chrome | Linux, Windows, macOS (no `[available]` restriction) | `--backend=webapp`; `chromedriver` and a chromium/chrome binary in the `PATH`; the witness serves its page, the backend navigates to `BBT_WEBAPP_URL` |
| `adi2`   | an Adi2 application (Ada 2022, SDL3), read through the toolkit's own development bridge: the widget tree, with each node's geometry, states and flags | `Adi.MCP`: a file IPC session, one directory per launched process, a sentinel saying it is up; no driver, no bus | Linux | `--backend=adi2`; a display; the witness built in the development profile, where the bridge is compiled — any other profile builds a plain application, which the backend reports as such |

The witness applications (GTK 4, web and Adi2) satisfy
the same [widget naming contract](../Apps/ToDoMVC/ToDoMVC.md#widget-naming-contract):
the scenarios target roles and accessible names only, never positions
or pixels.

## Vocabulary matrix

Rows follow the feature numbering of
[gui_features.md](gui_features.md); the id links to the feature, which
states the semantics, the grounding and the open questions.

| Entry (feature) | gtk4 Linux | webapp Linux | webapp Windows | webapp macOS | adi2 Linux | Comment |
|-----------------|------------|--------------|----------------|--------------| --- |---------|
| [G005](gui_features.md#g005--launch-and-window-observation) `When I run <cmd>` | yes | yes | yes | ? | yes | launch, not a step on its own: any previous instance is closed first; gtk4 waits for the frame, webapp waits for the server url, then navigates |
| [G005](gui_features.md#g005--launch-and-window-observation) `a window appears` | yes | yes | yes | ? | yes | the "window" is mapped per backend: the GTK frame, the page and its document |
| [G005](gui_features.md#g005--launch-and-window-observation) `a window named <title> appears`, `there is a window named <title>` | yes | yes | yes | ? | yes | both forms are exercised by the witness stories; the witness apps expose a single window, so the "several windows" diagnostic is not exercised |
| [G010](gui_features.md#g010--text-content-of-a-window) `the window contains <text>` | yes | yes | yes | ? | yes | gtk4 reads the accessible names and `Text` interface content of the application subtree, dialogs included; webapp reads the rendered text of the page, and reads until two reads agree, since the application refreshes asynchronously |
| [G015](gui_features.md#g015--button-by-accessible-name-with-enabled-state) `there is a <role> named <name>` | yes | yes | yes | ? | yes | the role coverage itself differs, cf. [roles](#roles); a hidden widget is not found |
| [G015](gui_features.md#g015--button-by-accessible-name-with-enabled-state) `the <name> <role> is enabled / disabled` | yes | yes | yes | ? | yes | the user-visible state is asserted, never a toolkit attribute: the scenario never sees `aria-disabled`; GTK 4 never reports the AT-SPI `enabled` state, the backend reads `sensitive` |
| [G020](gui_features.md#g020--press-action-on-a-control) `I press the <name> <role>` | yes | yes | yes | ? | yes | a semantic action, not injected input: the AT-SPI `DoAction` interface on GTK 4, a click on the element on the web; the canonical verb for a button, and the synonym of `select` for an item |
| [G020](gui_features.md#g020--press-action-on-a-control) `I select the <name> listitem` | yes | yes | yes | ? | yes | the natural wording for an item — `selected` is what the user sees, the row changing colour — with the same action and the same rule as `press`; every story uses it but one, which keeps a `press` on an item to exercise the synonym |
| [G035](gui_features.md#g035--type-text-into-a-field-and-enter-it) `I type <text> into the <name> textbox` | yes | yes | yes | ? | yes | sets the accessible value with no validation side effect; webapp writes it again when the application cleared it |
| [G035](gui_features.md#g035--type-text-into-a-field-and-enter-it) `I enter <text> into the <name> textbox` | yes | yes | yes | ? | yes | the commit is a `Return` keyboard synthesis — the AT-SPI `activate` action is a silent no-op on GTK 4 — so a system-input tier action, cf. [S001](specifications.md#s001--action-provenance) |
| [G080](gui_features.md#g080--list-structure-with-content-and-emptiness-checks) `there is a <name> list` | yes | yes | yes | ? | yes | the role-word form of `there is a <role> named <name>`: same lookup |
| [G080](gui_features.md#g080--list-structure-with-content-and-emptiness-checks) `the <name> <role> is empty` | yes | yes | yes | ? | yes | the emptiness is the role's: no text for a `textbox` or a `listitem`, no item for a `list`, and no meaning at all for a `button`, which is named — but both backends implement it today as "no child under the widget", for every role, so a filled `textbox` would read empty; only the two witness lists are exercised, and the per-role scope is an open question, cf. [G080](gui_features.md#g080--list-structure-with-content-and-emptiness-checks) |
| [G080](gui_features.md#g080--list-structure-with-content-and-emptiness-checks) `the <name> <role> contains <text>`, `does not contain <text>` | yes | yes | yes | ? | yes | rendered text of the subtree; the negated form is the step polarity, cf. [D13](dev/design_discussions.md#d13-negation-as-a-step-polarity) |
| [G080](gui_features.md#g080--list-structure-with-content-and-emptiness-checks) `the <name> <role> has <n> items` | yes | yes | yes | ? | yes | the child count of the accessor: ARIA has no list size, its `aria-setsize` is carried per item |
| [G095](gui_features.md#g095--window-close-and-cross-scenario-application-lifetime) `I close the window`, `I close the <title> window` | yes | yes | yes | ? | yes | graceful close through the application's own path; gtk4 uses the AT-SPI `window.close` action by name, then a reported process-level fallback; webapp closes the page, the server outliving it by design; both forms are exercised, but no witness exposes several windows, so the ambiguity diagnostic of the plain form is not tested |

A form enters this table in the same commit as the grammar rule and the
backend that implements it. The forms the grammar does not carry yet
have no row here: [specifications.md](specifications.md#2-proposed-features)
lists them with their milestone, and each one gets its row when it is
absorbed.

## Roles

The role words a scenario may name, and how each backend answers them
— the second axis of the same capability question, since a backend
that implements a step against a role it cannot resolve supports
nothing.

| Role word | gtk4 Linux | webapp Linux | webapp Windows | webapp macOS | adi2 Linux | Comment |
|-----------|------------|--------------|----------------|--------------| --- |---------|
| `button` | yes | yes | yes | ? | yes | |
| `textbox` | yes | yes | yes | ? | yes | |
| `list` | yes | yes | yes | ? | yes | |
| `listitem` | yes | yes | yes | ? | yes | |
| `dialog` | yes | yes | yes | ? | yes | the generic `there is a <role> named <name>` form resolves the word on both, but the dedicated step waits for G025 ([proposed](proposed/G025_dialog_eventually.md)), and no witness exposes a dialog, so nothing exercises it on any platform — not tested |
| any other ARIA role | no — unknown roles never match | yes — the DOM mapping | yes — the DOM mapping | ? | yes — a role word is matched against the widget kind, by substring, so any word resolves and the wrong one finds nothing | gtk4 maps the closed MVP set on the AT-SPI role ids; webapp reads the explicit `role` attribute, else maps the native element, coarsely (`input` answers `textbox`, whatever its `type`) |

## Recorded runs

Each check writes its JUnit report and its Markdown index where the CLI
suites write theirs: `docs/tests_results/<platform>/<profile>/`, named
after the backend (`<backend>_gui_results.xml`, `<backend>_gui_results.md`),
by the witness Makefile of the backend (`make check` in
`Apps/ToDoMVC/<kind>`, reached through `make -C backends check-<kind>`).
One flat directory per platform keeps every result of a run — CLI and
GUI — in a single listing. The records are git-ignored like the CLI
ones, and a backend CI publishes them with `git add -f`, so that a
column of this page can be diffed against the last recorded run of the
same platform.

| Column | Evidence |
|--------|----------|
| gtk4 Linux | [backend-gtk4.yml](../.github/workflows/backend-gtk4.yml) runs the suite headless (Xvfb, `at-spi2-core`) on every push, through `make check` and the `--backend=gtk4` launcher, and publishes the result as `docs/tests_results/Linux/validation/gtk4_gui_results.xml` |
| adi2 Linux | no run recorded: the 0.7.0-dev changelog states the suite passes against the Adi2 witness, and `make -C backends check-adi2` reproduces it with a display and a development-profile build — no CI job runs it yet |
| webapp Linux | no run recorded: the 0.7.0-dev changelog states the suite passes against the web witness, and `make -C backends check-webapp` reproduces it with a driver and a browser in the `PATH` — no CI job runs it yet |
| webapp Windows | `docs/tests_results/Windows/validation/webapp_gui_results.xml`: 7 scenarios, every step passes, `chromedriver` 157.0.8103.78 steering Chrome 157.0.8103.79 headless, last run 2026-11-27; no CI job runs this suite, so the record is committed by hand. It is not yet stable: two runs out of about twelve failed on a single `cannot press listitem`, on different steps and with either wording (`I press` or `I select`), a fresh app as well as a row just moved to `Done`, while the same stories run alone never failed (7 runs) — the lookup is polled, so the click itself is the suspected point, and no fix is in |
| webapp macOS | nothing recorded |

## Keeping it true

- A cell changes state in the commit that changes the code or records
  the run. A `?` becomes `yes` on a recorded run of the shared suite on
  that platform, not on a reading of the code.
- The columns are the declared `(backend, platform)` pairs: adding a
  backend or a platform adds a column, and a backend that does not
  exist yet has none, so that no cell is a blanket "not applicable".
- The page is hand-maintained today, and that is its weakness: the
  runs are recorded per platform and per backend, and the page does not
  read them. The intended source of truth is the backend's own
  capability declaration — the capability report of
  [S001](specifications.md#s001--action-provenance) and G050 — with the
  columns generated from those declarations and from the recorded runs.
  A declaration contradicting a run then breaks CI instead of drifting
  the doc.
- Until then, review reads a diff of this table the way it reads a diff
  of the code: a `yes` without a run behind it is the thing to
  challenge.
