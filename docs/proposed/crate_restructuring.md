# Crate restructuring: GUI backends as separate crates in the bbt repository <!-- omit from toc -->

Source: design discussion with the project owner (2026), continuing
`gui_vocabulary.md` and `interactive_execution.md`. Facts verified
during the discussion: `alr install` capabilities (alr 2.2 help),
the Alire index `subdir` origin mechanism, the Dmitry A. Kazakov
plug-in how-to (forum.ada-lang.io, ada-lang.io Learn pages,
github.com/dmitry-kazakov/How-to-create-an-Ada-plug-in), and
github.com/ovenpasta/adi2 (own Alire manifest, not yet in the
community index).

## Contents <!-- omit from toc -->
- [Need](#need)
- [Intended outcome](#intended-outcome)
- [Scope](#scope)
- [Non-goals](#non-goals)
- [Acceptance criteria](#acceptance-criteria)
- [Specification](#specification)
  - [One repository, several crates](#one-repository-several-crates)
  - [Backend injection](#backend-injection)
  - [Launcher](#launcher)
  - [Shared suite and cross tables](#shared-suite-and-cross-tables)
  - [Migration of the GUI prototypes](#migration-of-the-gui-prototypes)
  - [Development plan](#development-plan)
- [Executable scenarios](#executable-scenarios)
  - [Scenario: a GUI step without any backend aborts with a selection hint](#scenario-a-gui-step-without-any-backend-aborts-with-a-selection-hint)
  - [Scenario: `--gui` with a missing backend executable gives an install hint](#scenario---gui-with-a-missing-backend-executable-gives-an-install-hint)
- [Verification beyond bbt](#verification-beyond-bbt)
- [Design implications](#design-implications)
- [Open questions](#open-questions)
- [Finalization target](#finalization-target)

## Need

bbt has grown. The interactive debugger embeds gnoga
(`interactive_execution.md`, accepted cost), the AT-SPI backend lives
inside the core crate and drags `libglib` with it, and the upcoming
backends (webapp driver, Adi2) would each drag a full GUI stack into
the root crate.

Alire has no optional dependency: everything in the root crate
`alire.toml` is fetched and compiled by every user, on every platform.
A CLI-only user of bbt already pays `libglib`, and would pay for every
future backend stack. `alr install bbt` must return to a light,
CLI-centric installation.

Meanwhile, the prototypes of these backends and of the witness
applications live in a separate prototypes repository. Keeping two
repositories in sync costs more than it returns: the prototypes are
transitional and must be migrated into the new crates, then retired.

## Intended outcome

One git repository, several published crates. The GUI backends live
inside the bbt repository, but outside the bbt crate:

```
bbt/                              # single git repository
├── alire.toml                    # crate "bbt": engine lib + CLI
│                                 #   + embedded gnoga debugger
│                                 #   no backend dependency anymore
├── backends/
│   ├── common/                   # crate "bbt_backends" (library)
│   ├── gtk4/                     # crate "bbt_gtk4"      (executable)
│   │                             #   dependency: libglib — Linux
│   ├── webapp/                   # crate "bbt_webapp"    (executable)
│   └── adi2/                     # crate "bbt_adi2"      (executable)
└── Apps/                         # the witness applications (test
                                  #   targets, not products, CI-bound)
    └── ToDoMVC/                  # ToDoMVC, its per-backend
        │                         #   implementations (gtk4/, ...)
        └── docs/
            └── scenarios/        # shared GUI scenario suite (data,
                                  #   not a crate)
```

For the user:

- `alr install bbt` fetches and compiles the core only; CLI behavior
  and the interactive debugger are unchanged;
- `alr install bbt_gtk4` (or `bbt_webapp`, `bbt_adi2`) pulls exactly
  one backend stack, for the kind of application under test;
- one entry point: `bbt my_scenario.md --gui=gtk4` launches the right
  backend; a GUI step without any backend gives an immediate,
  actionable error instead of a backend-specific crash.

The step grammar stays in a single place: the core. GUI vocabulary and
CLI vocabulary are registered by the same grammar engine, and the GUI
action branches of the runner call an abstract backend, exactly as
today. No grammar extension API is introduced by this proposal.

## Scope

- Split the root crate into a library project (`bbt_lib`, all engine
  sources including the gnoga debugger) and a thin executable project
  (`bbt-main`), within the single crate `bbt`, exposing the library
  to the backend crates.
- Replace the per-OS backend selection (`case Alire_Host_OS` in
  `bbt.gpr`, the `src/atspi` and `src/no_gui` source dirs, the
  `-l:libatspi` linker switches) by a backend injection: an abstract
  `Backend` interface in the core, a `Selected` class-wide access set
  at startup by whoever owns a concrete backend (the core default
  being "none", which raises `No_Backend_Available`).
- Move the AT-SPI backend sources from `src/atspi` to
  `backends/gtk4`, and `libglib` from the root manifest to the
  `bbt_gtk4` manifest.
- Add a `--gui=<kind>` option to the core CLI: a launcher that
  executes `bbt_<kind>` found in the PATH, or fails with an
  actionable message; a run-facing check also fails at analysis
  time when a document contains GUI steps but no backend is
  registered. `--backend` and `BBT_BACKEND` are replaced
  immediately, with no deprecation period (never publicly
  advertised).
- Create `backends/common` (`bbt_backends` library: polling,
  harness deadline, text normalization, diagnostics shared by
  backends; sole dependency: bbt), the per-backend executable crates,
  and the shared scenario suite; the witness applications live in
  `Apps/` (already the case for the ToDoMVC gtk4 witness, crate
  `todomvc_gtk4`).
- Publish every crate from its subdirectory (index `[origin]`
  `subdir`, as done by `aaa`/`aaa_texts` in the community index),
  versioned in lockstep during a common release procedure.
- Maintain the cross tables (step vocabulary × backends, platforms ×
  backends) at the top of the repository, fed by each backend CI
  (same spirit as `docs/tests_results/`).

## Non-goals

- No dynamic plugin loading (`dlopen`/LoadLibrary). The pattern is
  documented as an open door (Kazakov how-to; and `alr install` can
  indeed distribute shared libraries), but version consistency is
  then "on your own" per alr's own help, so the first delivery uses
  statically linked variant executables.
- No debugger split and no cross-process debug protocol: the gnoga
  debugger stays embedded in the core (decision recorded with the
  owner). `--debug` keeps working in every backend variant, since the
  debugger is part of `bbt_lib`.
- No grammar extension API: the grammar stays in one place, in the
  core. A future proposal may introduce the open rule patterns and
  the extension payload once the GUI vocabulary has stabilized.
- No witness crate (`Apps/`) in the `depends-on` of any backend
  crate: only the backend CIs pin them by path for a full run.
  `alr test` of a published backend crate runs a sanity subset
  only.
- The prototypes repository is not kept alive as a product: its
  content is migrated then archived.

## Acceptance criteria

- The existing `docs/cli_features` and `docs/examples` suites pass
  unchanged after the restructuring.
- A clean workspace `alr build` of the root crate resolves no other
  GUI stack than gnoga; `libglib` appears nowhere in the root
  `alire.toml` nor in `(alr with --solve)` of the root crate.
- `./bbt --version` output is unchanged; each `bbt_<kind> --version`
  identifies its backend.
- The scenarios of this document pass.
- Each backend crate has its own CI, running the
  `Apps/ToDoMVC/docs/features` suite against its witness application on its
  supported platforms, and updating its rows in the cross tables.
- `alr publish --skip-submit` succeeds for each new crate.

## Specification

### One repository, several crates

A git repository is not a crate. Each subdirectory above is an
independent Alire crate with its own manifest, dependencies,
CI and publication. The community index publishes from subdirectories
(`[origin]` `subdir`, e.g. `aaa` and `aaa_texts` from the same
repository), so `alr install bbt` never downloads the backends
sources: the crates limit what each user fetches, while the single
repository keeps one place for the code, one review flow and no
cross-repository synchronization.

### Backend injection

The core keeps the grammar (including GUI steps), the `Step_Data` GUI
payload fields, and the runner branches calling `BBT.Tests.GUI_Backends`.
That package root keeps its current interface (`Launch`, `Shutdown`,
`Window_Appears`, `Observed_Title`, `Window_Contains`, `Widget_Named`,
`Widget_Enabled`, `Press_Widget`) but its body no longer dispatches to
per-OS child bodies: it dispatches to a `Selected` class-wide access to
an abstract `Backend`. A backend crate main registers its concrete
backend type at startup. With no backend registered, the failure is
checked eagerly, at analysis time and before any scenario runs, so
that a document containing GUI steps fails with exactly:

```
Error: no GUI backend available: select one with --gui=<kind>
```

and the whole command output is this single line. The same error can
still occur at run time for a document built dynamically or filtered
late; it then appears in the step result, not in the summary.

`backends/common` (crate `bbt_backends`) hosts what every backend
needs and no GUI stack provides: harness deadline and polling
(`eventually` semantics), text normalization for titles, failure
diagnostics. If a helper ever needs a GUI dependency, it goes down
into the concerned backend, never into the common library.

### Launcher

`--gui=<kind>` on the core CLI:

1. the bbt process replaces itself with `bbt_<kind>` found in the
   PATH, forwarding the remaining arguments unchanged;
2. if not found, bbt fails before running any scenario:

```
bbt_<kind> executable not found in PATH; install it with: alr install bbt_<kind>
```

The executables of the backends are named `bbt_gtk4`, `bbt_webapp`,
`bbt_adi2`. `--backend` and `BBT_BACKEND` are not aliases: they are
removed, with no deprecation path. A backends crate has a minimal main
of its own; all
engine logic lives in the core library. The witness applications
(`Apps/`, e.g. the ToDoMVC GTK 4 witness, crate `todomvc_gtk4`)
exercise the full vocabulary against real GUI
toolkits; they are test targets, not products.

`witness_aria` is not a toolkit: it is a static web page exercising
the WAI-ARIA authoring-practice markup (roles, accessible names,
states, landmarks, focus) across the webapp vocabulary. The witness
is the ToDoMVC application, one app, one behavior spec: each
backend has one implementation directory under `Apps/ToDoMVC`, and
for the webapp backend the single `Apps/ToDoMVC/web` crate carries
both targets: the interactive gnoga application, and the static
page, which needs no build and is served as data.

### Shared suite and cross tables

`Apps/ToDoMVC/docs/features/*.md` contains the common GUI scenario suite:
the same steps run against each backend and its witness. A step
supported by a backend has its scenario passing in that backend CI;
a step port with a failing cell is exactly what must appear in the
cross tables, maintained in `docs/backends.md`.

### Migration of the GUI prototypes

| Prototypes content                         | Destination                    |
|--------------------------------------------|--------------------------------|
| the prototype scenario parser              | dropped: single core grammar   |
| the AT-SPI backend prototype               | `backends/gtk4`                |
| the webdriver backend prototype            | `backends/webapp`              |
| the Adi2 backend prototype                 | `backends/adi2`               |
| witness gnoga apps (`apps/witness`, `apps/witness_gnoga`) | `Apps/ToDoMVC/web` (dynamic target, dedup to arbitrate) |
| `apps/witness_aria` (static ARIA-markup page) | `Apps/ToDoMVC/web` (static target) |
| `apps/witness_adi2` | `Apps/ToDoMVC/adi2` |
| `tests/scenarios`, `docs/proposed/f...`    | `Apps/ToDoMVC/docs/features` + core grammar |
| manifesto, comparables, roadmap documents  | selective merge into bbt docs |
| the Adi2 library                           | extracted first into its own crate (see development plan) |

The repository is then archived read-only.

### Development plan

1. **Core split** — `bbt_lib`/`bbt-main` projects in the root crate;
   backend injection replacing the GPR case; `src/atspi` moves to
   `backends/gtk4`; `libglib` removed from the root manifest;
   launcher `--gui`.
2. **First backend** — `backends/common`, `backends/gtk4`, and the
   ToDoMVC witness GTK 4 implementation (`Apps/ToDoMVC/gtk4`); its
   CI runs the first shared scenarios; initial cross tables with one
   backend column.
3. **Webapp** — migration of the webdriver backend prototype; the
   ToDoMVC web
   implementation, `Apps/ToDoMVC/web`, with its two targets (static
   ARIA page + gnoga app); own CI, own platforms.
4. **Adi2** — migration of the Adi2 backend prototype and its
   witness; the
   dependency on [github.com/ovenpasta/adi2](https://github.com/ovenpasta/adi2)
   (own manifest) is declared by git URL until its publication in
   the community index.
5. **Vocabulary absorption** — validated steps of
   `docs/proposed/G025`…`G075` land one by one in the single core
   grammar, each with its entries in the shared suite and in the
   cross tables.
6. **Retirement** — merge of remaining prototype docs into bbt
   docs; the prototypes repository archived.

## Executable scenarios

The restructuring is mostly packaging; the externally observable
behavior of the core is the launcher and the backend-unavailable
error. The positive delegation path and the per-backend executions
are verified by the backend CIs against the real crates and witnesses,
not by scenarios here.

### Scenario: a GUI step without any backend aborts with a selection hint

- Given the file `scen.md` containing
~~~
## Scenario: window check

- Then a window appears
~~~
- When I run `./bbt scen.md`
- Then the output is
~~~text
Error: no GUI backend available: select one with --gui=<kind>
~~~

### Scenario: `--gui` with a missing backend executable gives an install hint

- Given the file `scen.md` containing
~~~
## Scenario: trivial

- When I run `echo hello`
- Then the output contains `hello`
~~~
- When I run `./bbt scen.md --gui=nosuch`
- Then the output is
~~~text
Error: bbt_nosuch executable not found in PATH; install it with: alr install bbt_nosuch
~~~

## Verification beyond bbt

- CI guard: the root crate must not contain `libglib` anywhere
  (grep on `alire.toml`, plus a clean-workspace `alr build` +
  `(alr with --solve)` assertion).
- Publication dry-run (`alr publish --skip-submit`) for every new
  subcrate.
- Per-backend CI: build backend + witness, run the shared suite,
  publish cross-table updates; tagged per platform, on the model of
  the current tests results publication.
- Unit tests: not applicable here (packaging and process behavior);
  the launcher error handling may get one at implementation time.

## Design implications

- Decisions recorded with the project owner, this session: single
  grammar place (extension API deferred, sketch kept in the design
  discussion notes); embedded gnoga debugger (no `bbt_dbg` crate, no
  debug protocol); one repository with several published crates;
  backend injection instead of per-OS source-dir selection.
- `gui_vocabulary.md` must be revised for backend selection:
  `--gui=<kind>` at the core level replaces `--backend` /
  `BBT_BACKEND`, immediately and with no deprecation period (the
  option was never publicly advertised).
- The dynamic-loading door stays documented (Kazakov pattern) for a
  possible third-party backend future.

## Open questions

- Adi2: publish the [ovenpasta/adi2](https://github.com/ovenpasta/adi2)
  library in the community index, or keep the git-URL dependency in
  `backends/adi2`?
- Witness crates: published like the backend crates, or CI-only?
- The exact widget set of the dynamic webapp witness: the
  prototypes hold two gnoga-served applications (`apps/witness` and
  `apps/witness_gnoga`); only one is kept.

## Finalization target

- New durable document `docs/backends.md`: structure, cross tables.
- README (installation section: one crate per backend kind), User
  Guide (running GUI scenarios), `AGENTS.md` build pointers, and the
  generated `bbt help` reference.
