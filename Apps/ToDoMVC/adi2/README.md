<!--
 bbt, the black box tester (https://github.com/LionelDraghi/bbt)
 SPDX-License-Identifier: MIT OR Apache-2.0 WITH LLVM-exception
 SPDX-FileCopyrightText: 2026 Lionel Draghi
-->

# ToDoMVC — Adi2 implementation

Desktop application built on [Adi2](https://github.com/ovenpasta/adi2)
(Ada 2022, SDL3), driven through the adi2 backend: the application is
instrumented with Adi2's own development bridge (`Adi.MCP`), and
observations and interactions travel over that bridge's file IPC
session, namespaced by the launched pid, instead of the accessibility
bus.

- builds a `./todomvc` binary (`alr build`),
- satisfies the [behavior spec](../ToDoMVC.md) and its
  [witness stories](../docs/features), with the same model, the same
  widget naming contract and the same storage format as the
  [GTK 4 implementation](../gtk4/README.md),
- exposes the [widget naming
  contract](../ToDoMVC.md#widget-naming-contract) through the probe
  interface, in the conventions the bridge allows (below).

## Build and run

```
alr build
./todomvc
```

Adi2 is pinned in `alire.toml` (commit, not yet in the community
index); its SDL3 dependencies resolve to the system libraries through
Alire's external definitions (`libsdl3`, `libsdl3_ttf`,
`libsdl3_image`), so a system with the SDL3 development packages builds
it with no further setup. The application needs a display.

The build profile is the probe switch: `[build-profiles] adi2 =
"development"` compiles the bridge (Adi2's `src/mcp`), any other
profile compiles its documented no-op stub, and the binary is then a
plain application that the adi2 backend cannot drive — it fails with a
message saying so, never silently un-probed.

## Scenarios

`scenarios/` links the shared witness stories
(`../docs/features`) — one suite per backend, the same expectations:
`make check` runs them with `--backend=adi2` once the backend is in the
`PATH` (`make -C ../../../backends check-adi2` does both).
`make check STORY=add_a_task.md` restricts the run to one story.

## Notes for the adi2 backend users

The bridge serializes one text per widget and no accessible name, so
the mapping conventions are (the backend documents them, and the probe
audit grounds them):

- **names**: a text bearing widget is named by its text — a button is
  what it displays — and a widget carrying none is named by the label
  that immediately precedes it in tree order: the `New task` label and
  its field, the `To do` title and its list; the window title is the
  root widget's own label, declared for the bridge and left unpainted
  since the window decoration already shows it;
- **roles**: the widget kind, by family — a text input is a `textbox`,
  a list box a `list`, and its rows (plain label widgets) the
  `listitem`s;
- **states**: the serialized `disabled` state drives the enabled
  checks, the serialized `selected` state the selection.

Deviations from the reference projection, and why:

- the filters are buttons, not toggle buttons: Adi2's switch widget
  carries neither a name nor a size, so three named buttons stand in,
  the active one filled — the filter state stays visible, and the
  suite's steps never target them;
- `Complete`, `Edit` and `Delete` sit in a bar of their own rather than
  in the footer: the spec hides the footer when there is no todo, and
  the selection controls have to stay exposed — disabled — on a fresh
  app;
- an `enter` step needs a field that sees its Return key: Adi2's text
  input handles none, so the field type derives from it and reports
  Return (`src/todomvc_fields.ads`); a field built through the
  extension store is also born with neither the `Clickable` nor the
  `Focusable` flag, so the field package raises both — without them
  the widget takes no click and no key;
- the two lists are titled by a label widget above them rather than by
  their own label: a container paints that one inside its clipped
  content box, where the frame cuts it and it covers the first row;

## Persistence

State file `todos.state` in the current directory:
one `U <title>` (undone) or `D <title>` (done)
line per todo, and a `F <filter>` line for the active filter. Written
on every model change (not on exit), so closing the window never loses
an acknowledged change. An edition in progress is not persisted.
