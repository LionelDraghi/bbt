# Backends: the GUI backend crates

The GUI backends live in this repository, outside the core crate: one
git repository, several published crates, each with its own manifest,
its own dependencies and its own publication; the core reaches none of
them, and builds none of them. The decision, its alternatives and its
consequences are [D16](dev/design_discussions.md#d16-one-repository-several-crates-the-gui-backends).
This page keeps the structure as it stands; what each backend
implements, on which platform and through which toolkit, is the
[GUI capability matrix](gui_matrix.md).

## Structure

- the core `bbt` crate carries the engine and the grammar, GUI vocabulary
  included, and declares no GUI dependency: `bbt_lib.gpr` is the engine
  library, `bbt.gpr` a thin main over `src/main`;
- a backend is one crate per kind, with its own manifest, its
  `bbt_<kind>` executable, its link flags and its `[available]`
  platforms; the core reaches none of them at build time.

## Launcher

`bbt --backend=<kind> ...` replaces the bbt process with `bbt_<kind>`
found in the `PATH`, forwarding the remaining arguments unchanged. A
document containing GUI steps with no backend registered fails before
any scenario runs, and so does a `--backend=<kind>` whose executable is
missing, each with the command that fixes it
(cf. [A320_Backend_Selection.md](cli_features/A320_Backend_Selection.md)).

## The crates

| Crate           | Directory            | Artifact                         | Status |
|-----------------|----------------------|----------------------------------|--------|
| `bbt`           | repository root      | `bbt` executable + `bbt_lib`     | published (v0.6.1) |
| `bbt_backends`  | `backends/common`    | `bbt_backends` static library    | local, unpublished |
| `bbt_gtk4`      | `backends/gtk4`      | `bbt_gtk4` executable (Linux)    | local, unpublished |
| `todomvc_gtk4`  | `Apps/ToDoMVC/gtk4`  | `todomvc` witness (Linux, CI-only)| local, unpublished |

The `bbt_backends` library holds what every backend needs and no GUI
stack provides: the harness deadline and the bounded polling
(`BBT_Backends.Polling`, the `eventually` semantics), text
normalization for titles and rendered text (`BBT_Backends.Text`),
and the failure diagnostics helpers when the second backend brings
shared ones. GUI toolkit helpers never land there, they go down into
the concerned backend crate.

The witness applications are in `Apps/` (e.g. the GTK 4 witness
[ToDoMVC](../Apps/ToDoMVC/ToDoMVC.md), driven through AT-SPI), and stay CI-bound: they are test
targets, not products, and are not in the published dependencies of
any backend crate.

## What each backend supports

The shared suite (`Apps/ToDoMVC/docs/features/`) runs the same steps
against each backend and its witness; which of them each backend
implements, on which platform and through which toolkit, is the
[GUI capability matrix](gui_matrix.md) — one row per vocabulary entry,
one column per declared `(backend, platform)` pair. The same page holds
the prerequisites of a run: the accessibility bus for AT-SPI, a
`chromedriver` and a browser for the webdriver transport.
