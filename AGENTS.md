
- `make build` to build bbt and tools 

- `make sut` to build sut and rpl, and to create the sut, bbt, gcc and rpl
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
  after changing code or tests, run a full `make` (build, sut, check, doc),
  then commit the whole generated state together (results, badges,
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

- Ada gotchas, learned the hard way:
  - `Ada.Containers.Indefinite_Vectors` exports `"&" (Element_Type, Element_Type)
    return Vector`: as soon as a String parameter overload exists next to a Text one
    (e.g. Put_Step_Result), a `"literal" & String` expression becomes ambiguous in
    scopes where the instance is use visible. Disambiguate with a `String'(...)`
    qualification, or use Append
  - `Line_Index` is based on Positive: `Line_Index (0)` raises a constraint error,
    use 'Base arithmetic for index offsets
  - to pad a string, use Ada.Strings.Fixed.Head (pads or truncates, no overflow)

