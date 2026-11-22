# Backends: the GUI backend crates

The GUI backends live in this repository, outside the core crate:
one git repository, several published crates. The structure, the
launcher (`bbt --gui=<kind>`) and the backend injection are specified
in `docs/proposed/crate_restructuring.md`. This page maintains the
cross tables (step vocabulary × backends), fed by each backend CI and
by local validation runs, in the same spirit as `docs/tests_results/`.

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
[ToDoMVC](Apps/ToDoMVC/ToDoMVC.md), driven through AT-SPI), and stay CI-bound: they are test
targets, not products, and are not in the published dependencies of
any backend crate.

## Step vocabulary × backends

The shared suite (`Apps/ToDoMVC/docs/features/`) runs the same steps against
each backend and its witness. `pass` means the scenario passed on the
platform, run by the backend CI or locally; the missing vocabulary is
absorbed one feature at a time (plan step 5), each entering the suite
and this table.

| Vocabulary step (feature)                     | gtk4 | webapp | adi2 |
|-----------------------------------------------|------|--------|------|
| `a window appears` (G005)                     | pass | —      |      |
| `the window contains `text`` (G010)           | pass | —      |      |
| `there is a role named `name`` (G015)         | pass | —      |      |
| `the name role is enabled / disabled` (G015)  | pass | —      |      |
| typing into a textbox (G035)                  | pass | —      |      |
| `I press the name role` (G020)                | pass | —      |      |
| `there is a list named `name`` (G080)         | pass | —      |      |
| `the name list is empty` (G080)               | pass | —      |      |
| `the name list contains `text`` (G080)        | pass | —      |      |
| `I close the window` (G095)                   | pass | —      |      |
| dialog checks (G025)                          | pending vocabulary | — | — |
| focus checks (G030)                           | pending vocabulary | — | — |

## Platforms × backends

| Platform             | gtk4                                            | webapp | adi2 |
|----------------------|-------------------------------------------------|--------|------|
| Linux (X11/Wayland)  | pass — AT-SPI session required, validated locally | —      |      |
| Windows              | unsupported                                      |        |      |
| macOS                | unsupported                                      |        |      |

(The gtk4 rows are validated by a real `bbt_gtk4` run of the shared
suite against the ToDoMVC witness on a user session with the
accessibility bus enabled; the backend CI reproduces it headlessly.)
