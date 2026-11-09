# ToDoMVC — Adi2 instrumented implementation (placeholder)

Application instrumented with the Adi2 bridge (Adi.MCP development
bridge): observations and interactions travel over a file IPC session
namespaced by the launched pid, instead of the accessibility bus.

Not implemented yet. Its plain pid correlation and non-accessibility
transport make it the natural target for the `--headless` CI option.

When it lands, it will:
- build a `./todomvc` binary,
- satisfy the [behavior spec](../ToDoMVC.md),
- expose the [widget naming contract](../ToDoMVC.md#widget-naming-contract)
  through the probe interface.
