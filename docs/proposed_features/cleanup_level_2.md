<!-- omit from toc -->
## Feature: cleanup level 2, snapshot and diff of the exec dir

The current cleanup (`-c` | `--cleanup`, level 1) removes only the
files and directories that bbt itself created: the files of the
`setup` steps, the redirected output and error files, `Tmp_Dir`
(cf. `BBT.Created_File_List` and its `Add` calls). Anything created
by the tested commands beyond that — build artifacts, intermediate
files — is left behind, and the test directory gets polluted.

The level 2 cleanup extends the coverage by diffing the execution
directory instead of tracking individual files:

- a recursive snapshot of `Settings.Exec_Dir` is taken at the start
  of `Run_Doc`, storing each entry's relative path and kind
  (ordinary file, directory, symbolic link);
- at the end of `Run_Doc`, where `Created_File_List.Delete_All` is
  already called, the tree is walked again, and the entries that are
  not in the snapshot are removed: new files are deleted
  individually, a new directory is removed with `Delete_Tree` (its
  whole content is new by construction), new files in an existing
  directory are deleted without the directory;
- the granularity is per document, as for level 1, so that files
  created by a document do not survive its own cleanup;
- as for level 1, nothing is removed when something went wrong
  during the document run, to ease debugging.

This is deliberately a snapshot and diff, not a file system watch:
watching what the child processes actually write would require
inotify/fanotify on Linux, FSEvents on macOS and
ReadDirectoryChangesW on Windows, that is three implementations and
three sets of subtleties. A single `Ada.Directories.Search` based
diff works identically on all three platforms.

Compared to a shell tracing approach (as smk does on Linux), the
known limitations are acceptable:

- files that existed before the run and were overwritten by a
  tested command are not removed (they were there before: not a
  cleanup problem);
- files created and deleted by a tested command during the run
  are invisible (nothing left to clean);
- anything written in the exec dir by a concurrent process is
  considered garbage (rare in a test context, to be documented).

To be evaluated before adopting:

- the option spelling: a level argument on the existing option
  (`--cleanup=2`) or a distinct `--powerclean`, since level 2
  removes files that bbt did not create itself and must remain
  opt-in;
- safety guards on symbolic links: never traverse a symbolic link
  to a directory, delete the link and not its target, so that
  `Delete_Tree` cannot escape the exec dir;
- the Windows behavior on files still held open by a living
  background process: warn without failing the run, unlike the
  current level 1 `raise`;
- the cost of two recursive walks per document: negligible on a
  usual exec dir, but potentially heavy when bbt is run at the
  root of a large tree.

## Interaction with `--yes`

The level 1 cleanup never prompts: it is explicitly requested on
the command line, and removes only files that bbt itself created.
The powerclean removes files that bbt did not create, and may even
not know about: one confirmation is asked per document, listing the
number of new files and directories about to be removed, not one
prompt per file.

This confirmation goes through the same `Confirm` function as the
setup steps prompts (`BBT.Tests.Actions.File_Operations.Confirm`),
so that `--yes` (and the `[A]ll` answer, which sets
`Settings.Yes`) applies uniformly to all bbt prompts, without any
special case in the code: consistency by construction.

Two consequences:

- with `--yes`, powerclean behaves exactly as the cleanup: it
  removes everything without prompting, which is the expected
  behavior in a non interactive run (CI);
- without `--yes` on a non interactive standard input, the
  prompt would hang (`Get_Immediate`, cf. the fixme in
  `BBT.Tests.Actions.File_Operations`), so the same clean failure
  on a non interactive stdin is a prerequisite for this feature.

