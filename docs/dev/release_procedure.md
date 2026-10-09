# Release procedure

This document is the single reference for:

- selecting a release version;
- preparing and validating a release;
- committing the release state;
- creating the Git tag and GitHub release;
- publishing the release to Alire;
- returning the repository to development mode.

Do not commit, push, tag, create a GitHub release or publish to Alire without
the owner's explicit approval.

## 1. Release prerequisites

A release publishes what the current `-dev` section of
`docs/changelog.md` describes.

Before starting the release procedure, verify that:

- the intended development work is complete;
- the current `-dev` changelog section is complete and accurate;
- all development changes have been reviewed, committed and pushed;
- the working tree is clean;
- the current branch is the intended release branch;
- a complete local `make all` has succeeded;
- the latest required GitHub Actions checks have succeeded on:
  - Linux;
  - macOS;
  - Windows.

Check the repository state:

```sh
git status
```

Also inspect the file system for Git-ignored test or build artefacts.

Do not start a release from a dirty or partially generated repository state.

## 2. Select the release version

Do not select the release number by simply removing the `-dev` suffix.

Review the current `-dev` section of `docs/changelog.md` and propose the
release version according to SemVer.

In particular:

- an `[Added]` entry requires a MINOR version bump;
- a backward-compatible `[Changed]` entry requires a MINOR version bump;
- a release containing only backward-compatible bug fixes keeps the PATCH
  bump;
- any incompatible change must be assessed explicitly according to SemVer.

For example, if the current development version is:

```text
0.3.1-dev
```

a release containing an `[Added]` or backward-compatible `[Changed]` entry is
normally released as:

```text
0.4.0
```

A release containing only bug fixes may be released as:

```text
0.3.1
```

Before changing any file:

1. identify the current development version;
2. review the current changelog entries;
3. propose the release version;
4. explain the SemVer reasoning;
5. request explicit owner confirmation.

Do not change the version before it has been confirmed.

The prerelease suffix must be separated with a dash:

```text
0.3.1-dev
```

Do not use:

```text
0.3.1.dev
```

The dot-separated form prevents Alire from loading the workspace.

## 3. Set the release version

After the owner confirms the version, update it in:

```text
alire.toml
```

Then run:

```sh
alr update
```

`alr update` regenerates `Crate_Version` in:

```text
src/Alire_config/bbt_config.ads
```

Running `alr build` alone does not regenerate it.

Verify that the generated `Crate_Version` matches the confirmed release
version.

Search for other version strings that must be updated, including where
applicable:

- version strings in the sources;
- the `generated with BBT x.y.z` line in `docs/help/tutorial.md`;
- examples and generated documentation;
- other explicit release-version mentions.

Do not replace version-independent expressions, regular expressions or
examples unnecessarily.

## 4. Close the release changelog section

In `docs/changelog.md`:

1. verify that the current `-dev` section accurately describes the release;
2. retitle that section with the confirmed release version;
3. replace the undefined date with the release date;
4. preserve the existing changelog structure and categories;
5. keep entries concise and user-oriented;
6. ensure the latest entry appears at the top of its relevant list;
7. remove obsolete placeholders.

The release notes must reflect the actual content of this changelog section.

## 5. Validate the release locally

Run the complete local validation:

```sh
make all
```

If any part of the build, test, check or documentation generation fails:

1. stop;
2. do not commit, push or tag;
3. fix the issue through the normal development workflow;
4. restart release validation from a clean state.

After a successful full run, verify the release version displayed by the
appropriate bbt help command.

It must match the confirmed release version exactly and must not contain the
`-dev` suffix.

Then clean the repository:

```sh
make distclean
```

Inspect both Git state and the file system:

```sh
git status
git diff
git diff --staged
```

Verify that:

- no unwanted build or test artefact remains;
- no Git-ignored artefact remains;
- the release version is correct;
- `Crate_Version` was regenerated;
- the changelog section is closed correctly;
- all required generated results, badges, indexes and documentation are
  consistent;
- only intended release changes remain.

## 6. Confirm and commit the release state

Before staging, present a release-preparation report containing:

- the previous development version;
- the proposed release version;
- the SemVer reasoning;
- the changelog entries included;
- the result of `alr update`;
- the version reported by bbt help;
- the result of `make all`;
- the result of the cleanup;
- the files to be committed;
- the proposed commit message.

Request explicit owner approval before staging, committing or pushing.

After approval, stage the complete release state, including all intended
generated files:

```sh
git add <approved files>
```

Review the staged state:

```sh
git status
git diff --staged
```

Then commit:

```sh
git commit
```

Synchronise with the remote branch before pushing:

```sh
git pull --rebase origin main
```

If a rebase conflict affects generated test results, badges or similar
generated files, keep the local version produced by the latest complete run.

If a conflict affects source code, documentation or another non-generated
file, stop and ask the owner to arbitrate.

Push the release commit:

```sh
git push
```

## 7. Validate the release commit on GitHub

After pushing, verify that GitHub Actions is testing the intended release
commit.

All required checks must succeed on:

- Linux;
- macOS;
- Windows.

Do not create or push the release tag while a required check:

- is pending;
- has failed;
- has been cancelled;
- was run against a different commit.

If a correction is required, return to the development workflow, implement and
validate the fix, then restart the release procedure from a clean state.

## 8. Tag and publish the GitHub release

Once all required GitHub Actions checks are successful, prepare:

- the validated release commit;
- the confirmed release version;
- the proposed tag;
- the release notes derived from the changelog.

The tag format is:

```text
x.y.z
```

For example:

```text
0.4.0
```

Request explicit owner approval before creating or publishing the tag.

After approval, create the tag:

```sh
git tag x.y.z
```

Push the tag:

```sh
git push origin x.y.z
```

Then open the
[bbt GitHub Releases page](https://github.com/LionelDraghi/bbt/releases)
and create the release from that tag.

Reformat the released changelog section as release notes. Use the `0.2.0`
release as an example of the expected presentation.

Before considering the GitHub release complete, verify that:

- the tag name is correct;
- the tag points to the validated release commit;
- the release notes match the changelog;
- the GitHub release is published from the correct tag.

## 9. Publish to Alire

Publish to Alire only after the GitHub release and tag are available.

Before publication, verify that:

- `alire.toml` contains the released version;
- `Crate_Version` contains the same version;
- the Git tag is available remotely;
- the GitHub release is published;
- the required GitHub Actions checks are successful;
- the local repository is clean.

The publication command is:

```sh
alr publish
```

Refer to the
[Alire project publication documentation](https://alire.ada.dev/docs/#publishing-your-projects-in-alire)
when needed.

Before running `alr publish` or submitting any publication change, present the
intended action and request explicit owner approval.

After publication, report:

- the published version;
- the Git tag used;
- the publication or pull-request reference;
- the Alire validation result;
- any requested correction.

Do not silently alter an already published GitHub release to resolve an Alire
publication issue.

## 10. Return to development mode

After the GitHub and Alire publication steps are complete, propose the next
development version.

The new version must use the dash-separated `-dev` suffix.

For example:

```text
0.4.0 -> 0.4.1-dev
```

The appropriate next development version depends on the planned work. Request
explicit owner confirmation before changing it.

After confirmation:

1. update the version in `alire.toml`;
2. run:

   ```sh
   alr update
   ```

3. verify the regenerated `Crate_Version`;
4. open a new `[x.y.z-dev]` section in `docs/changelog.md`;
5. give the new changelog section an undefined date;
6. update version mentions in the README, including the AppImage example;
7. update any other explicit development-version mentions;
8. run:

   ```sh
   make all
   ```

9. verify the development version displayed by bbt help;
10. clean and inspect the repository;
11. request approval before staging, committing and pushing.

After approval, commit and push the complete generated development state
according to `docs/dev/development_workflow.md`.

The repository is back in development mode only when:

- the new `-dev` version has been committed;
- the commit has been pushed;
- the working tree and file system are clean.