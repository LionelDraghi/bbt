<!-- omit from toc -->
## Feature: running a command through a shell

bbt spawns commands directly: the arguments are split and unquoted by
bbt itself, and no shell interprets the command line (cf.
[B110_Spawn.md](../features/B110_Spawn.md)). When a step needs pipes,
redirections or command substitutions, the workaround is to spawn a
shell explicitly in the command, e.g.
`sh -c "grep ^version alire.toml | cut -d'\"' -f2 > out.version.txt"`
as tested in
[B115_Warning_On_Shell_Metacharacters.md](../features/B115_Warning_On_Shell_Metacharacters.md),
whose warning message recommends this workaround.

This works, but:

- it is verbose, and the author has to know the workaround;
- it embeds shell-level quoting inside the Markdown code span, which
  leads to double-escaping patterns such as `cut -d'\"'`;
- the shell that is spawned is implicit: the scenario says nothing
  about it, and bbt cannot check the command against the syntax of
  the shell that will interpret it.

## The proposal

A dedicated step would spawn a configurable shell, passing the whole
command string as the single `-c` argument, without splitting it:

- `When I run in shell "cmd1 | cmd2 > out"`
- `When I successfully run in shell ...` for the variant with the
  exit status check.

The shell would be configurable:

- a bbt setting, e.g. a command-line option `--shell=<cmd>`, with a
  `BBT_SHELL` environment variable override;
- defaulting to `/bin/sh` on POSIX systems;
- on Windows, defaulting to `cmd.exe`, the `cmd /c` semantics and the
  quoting rules being defined in a dedicated feature file.

The shell metacharacter analysis of B115 would not apply to such
steps: the metacharacters are then expected and interpreted by the
spawned shell. The warning would remain the diagnostic for plain
`run` steps.

## Open questions

- the exact step keyword: `in shell`? `through a shell`?
- should the command string of a shell step be re-analyzed against
  the syntax of the configured shell? The POSIX sh set
  (`| $ \` < > & ; * ?`) differs from other shells: `?` is not a
  wildcard in fish, command substitution is `(...)` instead of
  backticks or `$(...)`, and the quoting and escaping rules differ
  (no escape at all inside fish single quotes);
- the Windows semantics: `cmd.exe /c` with its own quoting rules, or
  PowerShell?
- a shell step interprets everything in the command string, file
  redirections included: the bbt guarantees on the created files and
  on the output files must be restated for this step kind.

## Discussion

The proposal is submitted for feedback in the GitHub discussion
["Proposal: a dedicated \"run in shell\" step, with a configurable
shell"](https://github.com/LionelDraghi/bbt/discussions/42).

