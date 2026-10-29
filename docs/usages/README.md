# Autoporteur bug reports (docs/usages)

This directory hosts *autoporteur* bug reports: bug reports written as
executable bbt scenarios. Each document tells the story of the bug
(problem, reproducer, actual vs expected result), and can be replayed
with bbt so that a maintainer reproduces it with a single command:

    bbt <report>.md

When a file cannot be run here (it needs a tool that is not installed,
or a specific platform), it is still checked by `make examples` with
`bbt explain`, which verifies that bbt can parse the scenarios without
running them.
