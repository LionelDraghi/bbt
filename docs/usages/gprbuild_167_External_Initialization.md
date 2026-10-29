# GPRbuild does not honour non-Ada dependencies recorded in .ali D lines

> This document is an *autoporteur* bug report: it is both the report and
> its reproducer. A maintainer just runs `bbt <this file>` to reproduce
> the bug on their machine, no copy-paste needed.
>
> Compared to [the original report](https://github.com/AdaCore/gprbuild/issues/167), this one is barely longer,
> and it needs no attached or copy-pasted files: everything the
> reproducer requires is embedded in the document.
>
> Imposing bug reports in bbt format is a good idea. The scenario is
> red before the fix and green after it, so CI can check both
> automatically: a CI step verifies that the report reproduces the bug
> on `main` (the test must fail), and the fix pull request turns it
> green. And once the bug is fixed, the document has not served its
> purpose: it keeps running as-is in a non regression suite, guarding
> against a future reintroduction.

[gpr-167]: https://github.com/AdaCore/gprbuild/issues/167

## Problem

GNAT records a file referenced by `External_Initialization` as a
dependency in the generated ALI file. However, changing that file does
not cause `gprbuild` to rebuild the unit.

As a result, an incremental build can leave stale data embedded in the
executable.

## Reproducer

- Given the new file `main.adb` containing:
  ```ada
  with Ada.Text_IO;

  procedure Main is
     Embedded_Text : constant String
       with External_Initialization => "resource.txt";
  begin
     Ada.Text_IO.Put_Line (Embedded_Text);
  end Main;
  ```

- Given the new file `reproducer.gpr` containing:
  ```ada
  project Reproducer is
     for Main use ("main.adb");
     for Source_Dirs use (".");
     for Object_Dir use ("obj");

     package Compiler is
        for Default_Switches ("Ada") use ("-gnatX");
     end Compiler;
  end Reproducer;
  ```

- Given the new file `resource.txt` containing:
  ```
  Version 1
  ```

First build, and check that the resource is embedded:

- When I successfully run `gprbuild -P reproducer.gpr`
- And I run `./obj/main`
- Then the output contains `Version 1`

Then modify the external resource, and rebuild:

- Given the file `resource.txt` containing `Version 2`
- When I successfully run `gprbuild -P reproducer.gpr`
- And I run `./obj/main`
- Then the output contains `Version 2`

## Actual result

The last step fails: `gprbuild` reports that the unit is up to date,
the executable is not rebuilt, and still prints:

```text
Version 1
```

## Expected result

Changing `resource.txt` should cause the declaring Ada unit to be
recompiled. The rebuilt executable should print:

```text
Version 2
```

## ALI dependency

The generated ALI file already contains a `D` record for
`resource.txt`, including its timestamp:

```text
D /home/lionel/prj/repro/resource.txt  20261027231418 00000000
```

The checksum field for this dependency is `00000000`.

For an occurrence of `External_Initialization` in a subunit, the
dependency is recorded in the ALI file of the parent unit, as the
subunit has no separate ALI file.

## Comparison with gnatmake

Using the same source and resource file:

```sh
gnatmake -f main.adb -gnatX
```

After modifying `resource.txt`, running `gnatmake` again recompiles the
unit and the resulting executable prints the new contents.

## Environment

Extract from `alr version`:

```text
tool #1 gnat:              gnat_native=16.1.0
tool #2 gprbuild:          gprbuild=26.0.1
system package manager:    C:\msys64\usr\bin\pacman.exe

SYSTEM
distribution:              MSYS2
host-arch:                 X86_64
os:                        WINDOWS
target:                    NATIVE
toolchain:                 USER
word-size:                 BITS_64
```

## Workaround

Force recompilation of the declaring unit after changing the external
resource, for example by touching the corresponding Ada source before
running GPRbuild.
