# Apps: applications under test

This directory hosts the applications used as test **witnesses** by bbt's
GUI vocabulary: real apps that bbt launches, drives and observes
through its GUI backends, exactly as the CLI vocabulary launches and
observes command line tools.

Each sub-directory is a named application. Each application:

- has a behavior specification, in a Markdown file at the root of its
  directory, stating its requirements as numbered `[Gnnn]` items traced
  in [docs/UG.md](../docs/UG.md);
- holds one implementation sub-directory per GUI backend (`web/`,
  `gtk4/`, `adi2/`), each providing a runnable app binary built from
  the same behavior spec.

## Applications

- [ToDoMVC](ToDoMVC/ToDoMVC.md) — the classic todo-list application,
  specified on the basis of the TodoMVC project [application
  spec](https://github.com/tastejs/todomvc/blob/master/app-spec.md)
  (Functionality chapter).

## Backends

Implementation branches:

- `web/` — browser applications, driven through a webdriver backend;
- `gtk4/` — native GTK 4 apps, observed and driven through the
  accessibility (AT-SPI) backend;
- `adi2/` — applications instrumented with the Adi2 probe bridge, a
  transport designed for headless CI runs.

Each implementation provides its own launch command; the bbt scenarios
in the behavior specification run against any of them.
