<!--
 bbt, the black box tester (https://github.com/LionelDraghi/bbt)
 SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
 SPDX-FileCopyrightText: 2026 Lionel Draghi
-->

# ToDoMVC — GTK 4 implementation

Native GTK 4 application, observed and driven through the AT-SPI
accessibility backend (the `bbt_gtk4` backend crate, launched with
`bbt --backend=gtk4`), on Linux.

- builds a `./todomvc` binary (`alr build`, with the GtkAda
  27 crate pinned in `alire.toml`),
- satisfies the [behavior spec](../ToDoMVC.md),
- exposes the [widget naming contract](../ToDoMVC.md#widget-naming-contract)
  through the GTK accessibility layer: the visible text and the
  accessible name of every control are set explicitly (the entries via
  `gtk_accessible_update_property`, since GTK 4 does not propagate a
  name there).

## Build

The GtkAda crate is pinned to a commit of its GitHub sources — a
branch pin would follow AdaCore's `main`, whose project file can stop
parsing with the gprbuild the CI installs. Its library must be built
once, by GtkAda's own build system, before `alr build` can link it,
and Alire materializes the pin in a directory named after that commit
(`alire/cache/pins/gtkada4_<commit>`): the sequence is therefore a
target, which reads the commit from the manifest, rather than a
literal path:

```
make pin
```

It runs in the Alire environment (`alr exec`), because GtkAda's bare
`make` needs the compiler and `GPR_PROJECT_PATH` that Alire exports.
An `alr build` alone, on a fresh clone, fails at the link on purpose:
that first failure is what materializes the pin.

The project links the static `libgtkada.a` of the pin
(`with "gtkada.gpr"`, default `LIBRARY_TYPE=static`).

## Notes for the AT-SPI backend users

- a GTK 4 toggle button exposes the AT-SPI role *toggle button* (62),
  not *push button* (43): the app's mark-all control is a plain button
  (43), while the filters are toggles (per spec, the selected one shows
  the checked aspect) — their role differs, and `press` and
  `button named` steps still resolve them;
- a GTK 4 app connects to the accessibility bus only when the session
  reports `org.a11y.Status.IsEnabled = true` — on a test desktop,
  enable it first (`dbus-send ... org.freedesktop.DBus.Properties.Set
  org.a11y.Status IsEnabled variant:boolean:true`), then launch the
  app;
- a fresh GTK 4 application registration on the AT-SPI registry can
  take a few seconds: the backend's bounded-window polls (6 s deadline)
  need headroom on loaded sessions.

## Persistence

State file `todos.state` in the current directory: one `U <title>`
(undone) or `D <title>` (done) line per todo, and a `F <filter>` line
for the active filter. Written on every model change (not on exit), so
closing the window never loses an acknowledged change.
