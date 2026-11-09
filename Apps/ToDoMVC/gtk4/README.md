<!--
 bbt, the black box tester (https://github.com/LionelDraghi/bbt)
 SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
 SPDX-FileCopyrightText: 2026 Lionel Draghi
-->

# ToDoMVC — GTK 4 implementation

Native GTK 4 application, observed and driven through the AT-SPI
accessibility backend (`--backend atspi`), on Linux.

- builds a `./todomvc` binary (`alr build`, with the GtkAda
  27 crate pinned in `alire.toml`),
- satisfies the [behavior spec](../ToDoMVC.md),
- exposes the [widget naming contract](../ToDoMVC.md#widget-naming-contract)
  through the GTK accessibility layer: the visible text and the
  accessible name of every control are set explicitly (the entries via
  `gtk_accessible_update_property`, since GTK 4 does not propagate a
  name there).

## Build

The GtkAda crate is pinned to its GitHub sources; the library must be
built once before `alr build`:

```
export PATH=$HOME/.local/share/alire/toolchains/<gnat>/bin:$HOME/.local/share/alire/toolchains/<gprbuild>/bin:$PATH
cd alire/cache/pins/gtkada4 && make static && cd -
alr build
```

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
