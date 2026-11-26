# A prebuilt binary crate for bbt: `bbt_bin` <!-- omit from toc -->

Source: design discussion with the project owner (2026-11-26), after
the Alire publishing documentation and the AdaCore `_bin` crates of the
community index. Facts verified locally during the discussion, on the
index `stable-1.4.0` that `alr` 2.1.1 uses: the shape of the
`gnatformat_bin`, `gnatcov_bin`, `gnatdoc_bin` and `gnattest_bin`
manifests (`alr show`), the installation prefix semantics
(`alr install --help`), the publication command (`alr publish --help`),
the bbt manifests, the CI workflows and the tag list.

## Contents <!-- omit from toc -->

- [Need](#need)
- [Intended outcome](#intended-outcome)
- [Scope](#scope)
- [Non-goals](#non-goals)
- [Acceptance criteria](#acceptance-criteria)
- [Specification](#specification)
  - [What an Alire binary crate is](#what-an-alire-binary-crate-is)
  - [The artefacts](#the-artefacts)
  - [The crate](#the-crate)
  - [Publication order and immutability](#publication-order-and-immutability)
  - [Installation and collision](#installation-and-collision)
  - [Development plan](#development-plan)
- [Executable scenarios](#executable-scenarios)
  - [Scenario: the packaged executable answers its version](#scenario-the-packaged-executable-answers-its-version)
  - [Scenario: the packaged executable runs a scenario](#scenario-the-packaged-executable-runs-a-scenario)
- [Verification beyond bbt](#verification-beyond-bbt)
- [Design implications](#design-implications)
- [Open questions](#open-questions)
- [Finalization target](#finalization-target)

## Need

`alr install bbt` builds bbt from sources. That requires a GNAT
toolchain at version 16 or later — the `External_Initialization` aspect
of `src/bbt-cmd_line-put_help.adb` forces `-gnatX0`, an experimental
extension set — and it compiles gnoga (through a local patch of its
project files, `patches/`), utilada, termicap and ansiada before the
first scenario can run. For a user who wants to check a command-line
program, that is a compiler installation and a multi-minute build; on a
machine without GNAT it is nothing at all.

bbt is a tool: installing it should be a download. The Alire ecosystem
already provides the answer, and it is the one the AdaCore crates use:
a binary crate. `gnatformat_bin`, `gnatcov_bin`, `gnatdoc_bin` and
`gnattest_bin` are exactly that — the tool, prebuilt, installable with
no compiler and no dependency.

## Intended outcome

- `alr install bbt_bin` yields a ready `bbt` in the installation
  prefix, on every platform the crate ships, with no toolchain and no
  build.
- `alr install bbt` is unchanged: the source crate, for the users who
  build, and the only mode for a platform whose archive is not
  published.
- One bbt either way: same version, same help text, same scenarios. The
  only difference is where the executable comes from.
- The prebuilt mode is a documented first-class path in the README, not
  an annex.

## Scope

- A new crate `bbt_bin/` in this repository, published from its
  subdirectory (`[origin]` `subdir`, as `aaa`/`aaa_texts` do), with its
  manifest, its own version and its own publication.
- A CI packaging job: build in release profile, package, check the
  archive, hash it, attach it to the release of the tag.
- Two `make` targets: `make bin` for the current platform, in the
  development loop, and `make bin-check` for the local index
  installation test.
- The README installation section, `docs/changelog.md`,
  `docs/dev/release_procedure.md` and the `AGENTS.md` build pointers.

## Non-goals

- No change to the source crate: `bbt` keeps `executables = ["bbt"]`,
  its dependencies, its test action and its build profiles.
- No binary crate for the backends. `bbt_gtk4` links `libatspi`,
  `gobject` and `glib` at run time, so a prebuilt backend would ship a
  whole GUI stack and its platform rules — the source build is the
  point of a backend crate.
- No replacement of the Linux packages: the AppImage, deb and rpm
  serve the user who wants neither a toolchain nor an Alire
  installation, this crate serves the user who has Alire and wants no
  build. Both paths are documented.
- No aarch64 Linux archive in the first delivery: the runner and the
  archive come after the first publication (open question 4), so the
  combination is not declared at first.
- No cross-compilation: every archive is built on the platform it
  targets, by that platform's runner.
- No second executable name, no wrapper script, no debugger split: the
  shipped programme is `bbt`, the one the source crate builds.

## Acceptance criteria

- Each declared archive installs and runs: `alr install bbt_bin`, then
  `bbt --version` prints the crate version, and a documented scenario
  passes with that executable.
- The archive needs nothing at run time: no GNAT run time as a shared
  library, no data file beside the executable — the help text is
  embedded at build time.
- `alr publish --skip-build --skip-submit` generates an index manifest
  whose origin resolves for every declared platform, with the `sha256`
  of each archive.
- The `bbt` crate manifest still declares no `[available]`
  (`docs/dev/release_procedure.md`): the restriction of `bbt_bin`
  describes its artefacts, never the project.
- The publication is reproducible from the tag: the archives are
  attached to the release of that tag, and they are immutable
  afterwards.
- The README documents both installation modes and the prefix
  collision rule.

## Specification

### What an Alire binary crate is

The four `_bin` crates of the index today — `gnatformat_bin`,
`gnatcov_bin`, `gnatdoc_bin`, `gnattest_bin` — share one shape, which
this proposal follows rather than invents:

- the origin is a **binary archive per OS and per architecture**,
  optionally carrying its hash — AdaCore records one for `gnatcov_bin`
  and none for `gnatformat_bin`, the index accepts both, and we carry
  one for every archive (see the artefacts):
  `(case OS is Linux => (case Host_Arch is Aarch64 => binary archive
  ...tar.gz at URL with hash sha256:..., X86_64 => ...), Macos => ...,
  Windows => ...)`. The index knows the OS values `Linux`, `Macos`,
  `Windows` and the architecture values `X86_64`, `Aarch64`; AdaCore
  names its Windows archives `...-windows64-...`;
- **no `depends-on`**: nothing is resolved, nothing is compiled;
- **no `executables`**: no source is built, the manifest exists to
  place a binary;
- **`[environment] PATH=${CRATE_ROOT}/bin:${PATH}`** is what makes the
  installed programme visible, and it fixes the archive layout: the
  executable lives under `bin/` at the crate root;
- the version, the archives and their hashes *are* the release: there
  is no source archive to publish, and no build to reproduce.

### The artefacts

One archive per declared platform:

```text
bbt-<version>-x86_64-linux.tar.gz
bbt-<version>-aarch64-darwin.tar.gz
bbt-<version>-x86_64-windows64.tar.gz
```

- **Content:** `bbt-<version>-<arch>-<os>/bin/bbt` (`bin/bbt.exe` on
  Windows), `LICENSE` and `README.md`, all below one single top-level
  directory named after the archive. Alire deploys a binary archive by
  checking that it expands to exactly one directory, then moving that
  directory up as `CRATE_ROOT`
  (`alire-origins-deployers-source_archive.adb`, `Check_And_Move_Up`):
  an archive with a flat `bin/` at its root is rejected there with
  « Unexpected contents where a single directory was expected », and
  the installation fails. Measured on 0.7.0-dev with `alr` 2.1.1, on
  Windows: the flat layout fails, the wrapped one installs. Nothing
  else below the directory: bbt reads no data file at run time, so
  there is no `share/` to ship.
- **Build:** the release profile (`make build-release`), so the shipped
  executable is the optimized one and the local validation build is not
  offered to users.
- **Linux:** built on the oldest supported runner (`ubuntu-22.04`), for
  a glibc floor low enough to run on the current LTS distributions —
  the same choice the AppImage workflow already makes.
- **macOS:** `macos-15` produces the `aarch64-darwin` archive. The
  `x86_64-darwin` one is built and checked by the owner on an Intel
  macOS 13 machine, outside CI, from a clean checkout of the tag
  (`make distclean` then `make build-release`, then `make bin`), and
  the archive is attached to the release by hand: no GitHub runner
  offers that architecture any more, and this machine has a second use
  — its older command-line tools may link where the current SDK stubs
  make every Darwin link fail, the very reason the CI runs on `macos-15`
  (`alire.toml`, the `gnat` dependency note).
- **Windows:** x86_64, archive named `...-x86_64-windows64.tar.gz`, in
  `tar.gz` like the AdaCore crates.
- **Check, before the upload:** extract in a clean directory,
  `bin/bbt --version` equals the version of the tag, a generated
  one-line scenario runs with that extracted executable, and the
  archive contains no build residue. The scenario is generated rather
  than taken from `docs/examples/`, whose documents drive the `sut`
  tool of `tests/`: the smoke test must depend on nothing but the
  archive.
- **Hash:** `sha256` computed per archive and written into the
  manifest. AdaCore does it for `gnatcov_bin`, not for
  `gnatformat_bin`: the index accepts both, and the hash is our only
  integrity guarantee, so we carry it. `tools/bin_archive.sh fragment`
  writes the pair (download URL, sha256) of the platform it runs on, so
  that a release assembles the manifest from records instead of
  transcribing hashes.
- **Layout:** the wrapper directory, `bin/` inside it, and the two
  documents, nothing else: `${CRATE_ROOT}/bin` is that `bin/`, once
  Alire has moved the wrapper up, which is what `[environment]
  PATH.prepend` declares. If bbt ever ships a data file (a completion
  script, a man page), the layout is decided once, here, for every
  archive of every platform.

### The crate

`bbt_bin/alire.toml`. The shape below is the one `alr show` prints for
the AdaCore binary crates, so it states the manifest without inventing
a syntax; the generated manifest is the reference.

```text
name    = "bbt_bin"
version = "<same as bbt>"
        --  no depends-on and no executables: a binary crate resolves
        --  nothing and builds nothing

[environment]
PATH = ${CRATE_ROOT}/bin:${PATH}

[origin]
case OS is
  Linux   => case Host_Arch is
               X86_64  => binary archive bbt-<version>-x86_64-linux.tar.gz
                          at https://github.com/LionelDraghi/bbt/releases/
                             download/<version>/bbt-<version>-x86_64-linux.tar.gz
                          with hash sha256:<...>
               Aarch64 => (no case: no archive, not declared)
  Macos   => case Host_Arch is
               Aarch64 => binary archive bbt-<version>-aarch64-darwin.tar.gz
                          at .../download/<version>/...
                          with hash sha256:<...>
               X86_64  => binary archive bbt-<version>-x86_64-darwin.tar.gz
                          at .../download/<version>/...
                          with hash sha256:<...>
                          (built and uploaded by hand, cf. the artefacts)
  Windows => case Host_Arch is
               X86_64  => binary archive bbt-<version>-x86_64-windows64.tar.gz
                          at .../download/<version>/...
                          with hash sha256:<...>
```

The origin cases are the availability of a binary crate: a combination
with no case has no archive and cannot be installed, and the AdaCore
crates declare no `[available]` section at all — the case list is the
declaration. What this crate covers is therefore what its artefacts
cover, never what the project supports: bbt is multiplatform, and the
core crate declares no availability
(`docs/dev/release_procedure.md`).

One detail of the sketch above is Alire's *rendering*, not its syntax:
`PATH=${CRATE_ROOT}/bin:${PATH}` is what `alr show` prints for an
environment entry, while the manifest writes
`PATH.prepend = "${CRATE_ROOT}/bin"` — written the rendered way, the
manifest is rejected with `expected a table but got a TOML_STRING`.

The second detail is `binary = true`, which every origin case carries;
it is what selects Alire's binary deployment rather than the source one.
The case tables are written in the index syntax the AdaCore crates use
(`gnatdoc_bin`, `gnatcov_bin`, ...), where the key sits inside the case:

```toml
[origin."case(os)".windows."case(host-arch)".x86-64]
url = "https://github.com/LionelDraghi/bbt/releases/download/<version>/bbt-<version>-x86_64-windows64.tar.gz"
hashes = ["sha256:<...>"]
binary = true
```

Measured on 0.7.0-dev: without `binary = true` the very same archive is
deployed as a source crate and the installation fails, so the key is
part of the origin, not an option of it.

The version is the bbt version, always: a prefix must never hold a
`bbt` and a `bbt_bin` of different generations without saying so.

### Publication order and immutability

The order is imposed by the index, which downloads the origin again
when it reviews the submission. The tags of this repository carry no
`v` prefix (`0.6.1`, `continuous`), and the download URL of a release
uses the tag name as it is:

1. tag and GitHub release of bbt, with the archives of that tag
   attached by the packaging job;
2. `alr publish` of `bbt_bin` from its subdirectory;
3. and only then, if the source crate moves as well, its own
   publication — the bbt crate keeps declaring no availability.

The `continuous` release is rewritten at every push on `main`, so it can
never carry an origin: a published crate must point at an immutable
asset of a versioned tag. Every archive the manifest declares must be
attached before the submission, including the one built by hand on the
Intel macOS machine. Two crates are published per release from now
on, and this step belongs in `docs/dev/release_procedure.md`.

### Installation and collision

`alr install --help` is explicit: Alire vets only the trivially
detectable conflicts, it does not know the artefacts a crate produces,
and the consistency of a prefix is the user's responsibility. The two
crates ship the same programme name, `bbt`, so a prefix holds one of
them:

- the documentation states it, and gives the way to tell which one is
  in `PATH`: `bbt --version` and the crate that a
  `alr install --info` lists;
- `--force` reinstalls an already installed release;
- the versions advance together, so a mixed prefix is never silently
  older. `bbt_bin` mirrors the `bbt` version exactly, and the origin
  cases — not the version number — say what a release covers: when one
  platform's archive is missing at a release (a packaging failure, a
  toolkit regression), the crate is published without that case, and
  the platform stays on the source path at the same version. A
  published `bbt_bin` is never republished with other archives under
  the same version.

### Development plan

**Increment 1 is delivered** (2026-11-26): `make bin` and
`bin-crate.yml`, both driving `tools/bin_archive.sh package`.

1. **Packaging job and `make bin`** — delivered. `bin-crate.yml` runs
   on a version tag, on a per-platform matrix (Linux x86_64 on
   `ubuntu-22.04`, macOS arm64 on `macos-15`, Windows x86_64), and its
   packaging step is `make bin-check`: release build, archive and
   sha256, smoke test in a clean directory, then the manifest resolved
   through a private index and the crate really installed in a
   throwaway prefix — so a job that goes green has proved that
   `alr install` works on that platform, and the archive it attaches is
   the one it installed. The Intel macOS archive comes from the owner's
   machine at the same moment, by the same `make bin`. The CI and the
   development loop call the same target, so they cannot drift apart.
2. **The crate** `bbt_bin/`: the manifest is assembled from the
   (URL, sha256) records that `tools/bin_archive.sh fragment` writes on
   each platform, then generated and reviewed; `make bin-check`
   (`tools/bin_archive.sh check`) resolves the generated manifest
   through a private index, with the archive's hash, then installs the
   crate in a throwaway prefix and runs a scenario with the installed
   executable — the end-to-end test, offline, with no publication
   (open question 5); the release procedure carries the assembly and
   the publication order.
3. **Publication** (owner consent, cf. the shared git rules): release
   assets first, then the index submission; the release procedure gains
   its step.
4. **Documentation**: README (two installation modes, the collision
   rule), changelog entry, release procedure, `AGENTS.md` build
   pointers.

## Executable scenarios

The packaging has no CLI behaviour of its own: what the scenarios can
check is that the packaged artefact is the same bbt. They run under
`make bin-check`, against the archive of the current platform, before
any publication.

### Scenario: the packaged executable answers its version

The extracted programme is the shipped one, not the development build:
its version is the one the tag carries, which is the first thing a user
checks after an installation.

- Given the archive `bbt-<version>-<arch>-<os>.tar.gz` built by
  `make bin`
- When I run `tar -xzf bbt-<version>-<arch>-<os>.tar.gz -C <tmp>`
- And I run `<tmp>/bin/bbt --version`
- Then the output contains `bbt version <version>`

### Scenario: the packaged executable runs a scenario

The archive is self-sufficient: extracted in an empty directory, with
no GNAT run time and no data file beside it, the programme still runs a
documented scenario to its verdict.

- Given the extracted archive in `<tmp>`
- And the file `<tmp>/scen.md` containing
  ~~~
  ## Scenario: hello

  - When I run `echo hello`
  - Then the output is `hello`
  ~~~
- When I run `<tmp>/bin/bbt <tmp>/scen.md`
- Then the output contains `## Summary : **Success**, 1 scenarios OK`

## Verification beyond bbt

- `alr publish --skip-build --skip-submit --manifest
  bbt_bin/alire.toml` must generate a manifest, and the origin of each
  declared platform must resolve; a local index (`alr index --add`)
  then installs it in a throwaway prefix.
- Run-time independence: `ldd` on Linux, `otool -L` on macOS,
  `dumpbin /dependents` on Windows — no compiler run time as a shared
  library. The `x86_64-darwin` archive is checked on the owner's Intel
  machine, which is also where its `--version` and scenario checks run.
- Reproducibility: the archive is rebuilt from the tag by CI, and the
  manifest carries the hash of the uploaded file.
- Guard: the `bbt` manifest declares no `[available]`, and `bbt_bin`
  declares exactly the platforms it ships.

## Design implications

- **`docs/dev/release_procedure.md`**: the availability rule already
  there extends naturally — `bbt` declares nothing, `bbt_bin` declares
  its artefacts, a backend declares its platform — and the procedure
  gains the packaging step and the two-crate publication order.
- **[backends.md](../backends.md)**: the crate list of the repository
  gains `bbt_bin`, a published crate with no GUI concern; the remaining
  backend work stays in the TDL of [project_status.md](../dev/project_status.md).
- **`docs/backends.md`**: unchanged — `bbt_gtk4` stays a source crate,
  and a prebuilt backend is a non-goal here.
- **`AGENTS.md`**: the build pointers gain `make bin` and
  `make bin-check`, since `make help` is the documentation of record.
- **The user-facing contract**: a `bbt` that installs in one command,
  and a source build that keeps every guarantee, with the version
  naming which one is in `PATH`.

## Open questions

1. The macOS x86_64 archive — **decided (2026-11-26):** the owner
   builds and checks it on an Intel macOS 13 machine, outside CI, and
   uploads it to the release like the other assets. What remains is
   whether that machine later becomes a self-hosted runner, so the
   packaging job covers it; until then the manual step is written in
   the release procedure, and no platform depends on it to publish.
2. Windows and `tar.gz` — **decided (2026-11-26): do as AdaCore.** The
   archive is a `tar.gz` named `...-x86_64-windows64.tar.gz` on every
   platform, the Windows job extracts it on the Windows runner and runs
   the smoke test there, so the check does not depend on what the index
   CI does.
3. The AppImage, deb and rpm — **decided (2026-11-26): kept, and not a
   duplication.** They serve the user who wants neither a toolchain nor
   an Alire installation; `bbt_bin` serves the user who already has
   Alire and wants to skip the build. The README says which path serves
   which user, and the release publishes all four artefacts.
4. aarch64 Linux: an archive for a later release, not a question of
   principle. The hosted `arm64` runners exist — the alire-index CI
   itself links an Ubuntu 24.04 ARM job, which is what its 0.6.1
   refusal came from — so the packaging matrix gains the entry and runs
   the same smoke test natively. If this repository's plan does not
   include that runner, the fallback is a `qemu-user` container
   (`docker run --platform=linux/arm64`), the smoke test only running
   the extracted binary.
5. Installing from a local origin — **answered (2026-11-27): it works,
   and the first diagnosis was wrong.** Measured on a Windows host with
   `alr` 2.1.1: the failure was not Alire's path deployment but our own
   two mistakes, both fixed in `tools/bin_archive.sh`. The archive must
   expand to a single top-level directory (Alire moves that directory
   up as `CRATE_ROOT`, and rejects a flat layout with « Unexpected
   contents where a single directory was expected »), and the local
   index manifest must declare `binary = true`, without which Alire
   deploys the archive as a source crate. With both, `alr install
   bbt_bin --prefix=<tmp>` installs from a plain `file:` origin, offline,
   and the installed `bin/bbt` runs a scenario: `make bin-check` now
   does exactly that, so no publication and no network are needed to
   test the crate. The end-to-end target is therefore `make bin-check`
   itself, and a separate `make bin-install` is not needed.

## Finalization target

- README: the installation section in two modes, and the collision
  rule; `docs/changelog.md`: the new installation mode, user-facing.
- `docs/dev/release_procedure.md`: the packaging step, the publication
  order, the version pairing.
- `AGENTS.md`: the two `make` targets.
