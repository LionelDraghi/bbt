# F005 — Launch and window observation

Executable subset of the vocabulary feature F005 (launch, window
checks), run against the ToDoMVC witness app (Apps/ToDoMVC), the
shared GUI suite of the backends
(cf. docs/proposed/crate_restructuring.md, backends/scenarios).

Run with the backend of the platform, from the app implementation
directory (the app is launched with a relative path, and its state
file lives in the working directory), e.g. for the gtk4 backend:

```
cd Apps/ToDoMVC/gtk4
alr build        # or: make build
make check       # runs this suite with --gui=gtk4
```

## Scenario: The witness launches and its window appears

- Given there is no file `todos.state`
- When I run `./todomvc`
- Then a window appears

## Scenario: Two runs are independent

- Given there is no file `todos.state`
- When I run `./todomvc`
- Then a window appears
- When I run `./todomvc`
- Then a window appears
