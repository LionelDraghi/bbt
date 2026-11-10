# Dependency patches, applied by the build

Some dependency crates of bbt do not build as released with the
current toolchain or environment. The alternatives (pinning forks,
vendored copies) duplicate the whole crates; instead, each problem
get a minimal patch here, applied to the Alire cache copies by a
tools/ script called at each make build. Idempotent, and no-op when
the dependency is not present.

| Patch                              | Dependency    | Symptom                                     |
|------------------------------------|---------------|---------------------------------------------|
| gnoga-2.2.3-PRJ_TARGET.sed         | gnoga 2.2.3   | settings.gpr: value "Win64" illegal for Build_Target (see the sed header) |

Historical: gtkada 26 + GNAT 16 required another 2-lines patch, kept in
git history while the GUI used GtkAda (mitigated by switching the web
debugger to gnoga).
