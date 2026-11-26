<!-- omit from toc -->
# Release procedure

The release procedure is common to all Lionel's Ada projects and lives
in [the reference repository on GitHub](https://github.com/LionelDraghi/ada-project-template/blob/main/release_procedure.md).
Read both documents together; on any conflict, this file wins.

This file gives the parameter values and the bbt-specific additions;
all other steps are described in the common procedure.

## Parameters

- Changelog: `docs/changelog.md`; the development section is
  `[x.y.z-dev]`; its categories are `[Added]`, `[Changed]`, ...
- Complete local validation: `make all`.
- Version check: the version displayed by the appropriate bbt help
  command.
- Regenerated configuration: `src/Alire_config/bbt_config.ads`.
- Clean command: `make distclean`.
- Remote: `origin`.
- GitHub releases page:
  [LionelDraghi/bbt releases](https://github.com/LionelDraghi/bbt/releases).
- Required GitHub Actions checks: Linux, macOS and Windows.
- Alire platform availability: the `bbt` crate declares none. bbt is
  multiplatform, tested on the three runners; an `[available]`
  restriction on the core crate is a publication defect to fix at its
  cause, never a configuration. A backend crate (`bbt_gtk4`, ...)
  declares its own platforms, since a toolkit or an accessibility
  binding is platform-bound (cf.
  [D16](design_discussions.md)).

## bbt-specific additions

- Step 3: update the `generated with BBT x.y.z` line in
  `docs/help/tutorial.md`.
- Step 8: use the `0.2.0` release as the example of the expected
  release-notes presentation.
- Step 10: update the version mentions in the README, including the
  AppImage example.

## The prebuilt binary crate

`bbt_bin` publishes the same executable as prebuilt archives, so that a
user installs bbt without a toolchain (`alr install bbt_bin`); the
crate, its manifest and its limits are specified in
[bin_crate.md](../proposed/bin_crate.md). At each release, after the tag
and the GitHub release exist:

1. the packaging workflow (`bin-crate.yml`) attaches the Linux x86_64,
   macOS arm64 and Windows x86_64 archives to the release of the tag;
2. the macOS x86_64 archive has no runner any more: it is packaged on
   the maintainer's Intel machine, from a clean checkout of the tag
   (`make distclean`, `make build-release`, `make bin`), and attached to
   the same release by hand;
3. on each of those machines, `tools/bin_archive.sh fragment` writes the
   pair (download URL, sha256) of its archive under `obj/bin/`: the
   manifest is assembled from those records, never from a hand-copied
   hash;
4. `bbt_bin/alire.toml` carries one origin case per archive, no case for
   a platform that was not built, `binary = true` in every case, and the
   version of `bbt` itself;
5. `make bin-check` on each platform, before the submission, resolves
   the manifest through a private index and installs the crate in a
   throwaway prefix, offline: it is the end-to-end test of the archive,
   and it needs no publication (cf. open question 5 of the proposal);
6. the order is not negotiable: `alr publish --skip-build --skip-submit`
   then the submission, once every declared archive is attached,
   because the index CI downloads the origin again when it reviews the
   submission.

A published version is never completed later with another platform's
archive: that platform waits for the next version, and its origin case
is simply absent in the meantime.
