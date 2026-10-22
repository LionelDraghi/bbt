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
- Alire platform availability: per the `[available]` section of
  `alire.toml`.

## bbt-specific additions

- Step 3: update the `generated with BBT x.y.z` line in
  `docs/help/tutorial.md`.
- Step 8: use the `0.2.0` release as the example of the expected
  release-notes presentation.
- Step 10: update the version mentions in the README, including the
  AppImage example.
