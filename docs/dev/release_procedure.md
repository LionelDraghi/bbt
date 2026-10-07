# Release procedure

Personal notes complementing this procedure are in bbt.md,
in the repository parent directory.

0. a release publishes what the -dev section of docs/changelog.md describes:
   make sure this section is up to date, and that a full `make` passes
1. choose the number per semver, do not just drop the -dev suffix:
   [Added] or backwards-compatible [Changed] entries in the -dev changelog
   require a MINOR bump (e.g. 0.3.1-dev is released as 0.4.0),
   pure bug fixes only keep the PATCH bump
2. set the version in alire.toml, run `alr update` so that Crate_Version
   in src/Alire_config/bbt_config.ads is regenerated, and check the version
   displayed by `bbt help`
3. update version strings in the sources, e.g. the "generated with BBT x.y.z"
   line in docs/help/tutorial.md
4. close the -dev changelog section: retitle it with the release number
   and the release date, then run a full `make`, and commit the whole
   generated state
5. tag and publish: `git tag x.y.z`, `git push`, then create the GitHub
   release from the tag on the web UI, reformatting the changelog section
   as release notes (see the 0.2.0 release as an example)
6. publish to Alire: `alr publish`
   (https://alire.ada.dev/docs/#publishing-your-projects-in-alire)
7. back to dev: bump the version in alire.toml with the -dev suffix,
   `alr update`, open a new [x.y.z-dev] section with an undefined date
   in docs/changelog.md, update the version mentions in the README
   (AppImage example), run a full `make`, and commit
