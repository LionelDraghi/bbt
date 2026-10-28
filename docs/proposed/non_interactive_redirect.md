<!-- omit from toc -->
## Feature: non interactive commands output redirect

The commands of the scenarios without `type` or `enter` steps are
run to completion before their output is checked: bbt could then
let the child write its output files directly, with the library
redirections `Set_Output_Stream (File)` and `Set_Error_Stream
(File, Append => True)`, instead of pumping the output streams into
the files.

The pump would remain necessary only for the interactive commands,
where the output must be read while the command is still running.

To be evaluated before adopting:

- the merged standard error case: the standard output and the
  standard error would be two descriptors on the same file, both
  opened with append, and the interleaving of concurrent writes
  must stay equivalent to the current pipe arrival order;
- the interest once `Wait_Event` is available upstream
  (cf. the upstream proposals in [pty.md](pty.md)): the pump would
  then be simple enough for the redirect to be a marginal gain.

Engine change, to be arbitrated with the ada-util / Spawn
comparison (cf. [design_discussions.md](../dev/design_discussions.md),
D1).
