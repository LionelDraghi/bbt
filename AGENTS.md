
- `make build` to build bbt and tools 

- `make tools` to build sut and rpl, and to create the sut, bbt and gcc
  links in tests/; run it after a fresh clone, or when those links are missing

- to understand bbt : docs/bbt-skill
  
- test procedure
  - to run a specific test       : `cd tests & ./bbt <test_file>`
  - to run sanity checks (rapid) : `cd tests & ./bbt ../docs/examples`  
  - to run functional tests      : `cd tests & ./bbt ../docs/features` 
  - to run one suite            : `cd tests & make features | examples | non_reg | unit_testing`
  - add the `--exclude Windows_Only` option when on Linux/MacOS
  - add the `--exclude Unix_Only`    option when on Windows
  - `make clean` removes the test run artifacts only, keeping the built
    binaries and links usable; `make distclean` removes everything
    that can be rebuilt, including binaries and links
  - after `make clean` or `make distclean`, verify cleanliness on the file
    system, not only with git status: git ignored files
    (input.*, expected_*, cp...) remain invisible
  - files created by `When I run` steps (e.g. binaries compiled by gcc) are
    not tracked by --cleanup, which only tracks files created in `Given` steps

- commit discipline:
  never commit or push without the owner's explicit consent: prepare the
  change, run a full `make` (build, sut, check, doc), and report the
  result; wait for the go-ahead before committing
  when the owner approves a commit, commit the whole generated state
  together (results, badges,
  bbt_help.txt, indexes...): committing in the middle of the chain
  (e.g. after features only) freezes inconsistent artifacts, such as
  a badge.url still holding the bbt placeholder, or a stale badge.svg

- to do list : 
  - docs/proposed_features
  - docs/fixme_index.md
  - chapter TDL in docs/project.md
  
- about design and tests
  - docs/developer_guide.md

- bbt is tested mostly with bbt

- on changing a feature or an error message format:
  - feature files in docs/features are the specification (TDD first)
  - avoid snapshot testing (cf. bbt-skill), especially for generated files
  - add a line in docs/changelog.md under the current -dev version
  - `Fixme:` comments in docs/ and src/ are indexed in docs/fixme_index.md by `make doc`
  - examples and generated docs must not depend on the machine or on the locale:
    no hard version number (use a regexp or a fixed behavior), and set
    `Given the environment variable LC_ALL is set to C` when a tool message is checked
  - files in docs/help/ are embedded in bbt at build time (External_Initialization):
    rebuild before testing `bbt help`, and regenerate the reference files
    (docs/bbt_help.txt, docs/example.md, docs/examples/gcc_hello_world.md)
    after a change, otherwise B130 fails

- Alire and version numbering:
  - the pre-release suffix must be dash separated (0.3.1-dev), not dot (0.3.1.dev),
    otherwise alr cannot load the workspace
  - after a version change in alire.toml, `alr update` regenerates Crate_Version;
    `alr build` alone does not

- release procedure (personal notes in ../bbt.md):
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
     line in docs/help/tutorial.md (docs/tutorial.md is regenerated from it)
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

- Ada gotchas, learned the hard way:
  - `Ada.Containers.Indefinite_Vectors` exports `"&" (Element_Type, Element_Type)
    return Vector`: as soon as a String parameter overload exists next to a Text one
    (e.g. Put_Step_Result), a `"literal" & String` expression becomes ambiguous in
    scopes where the instance is use visible. Disambiguate with a `String'(...)`
    qualification, or use Append
  - `Line_Index` is based on Positive: `Line_Index (0)` raises a constraint error,
    use 'Base arithmetic for index offsets
  - to pad a string, use Ada.Strings.Fixed.Head (pads or truncates, no overflow)

