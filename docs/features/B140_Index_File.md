<!-- omit from toc -->
## Features : Index output

The index output produce a single file containing the result of all run tests, with links to the source scenario files. 

It is triggered by the `--index filename` option (short form `-i`). 

The content is equal to the normal output in verbose mode, whatever verbosity is specified on the command line.
Meaning that you can have `--quiet` and no standard output, the index file will still contains all the details.

In this test, a scenario is run in quiet / normal / verbose mode while writing an index, and check that the index file is always equal to the verbose output.

- [Scenario: Successful run index file](#scenario-successful-run-index-file)
- [Scenario: Unsuccessful run index file](#scenario-unsuccessful-run-index-file)


### Scenario: Successful run index file

- Given the file `OK_scen.md`
  ~~~md
  # Feature : Getting info
  ## Background: setup
  - When I run `./sut --help`
  - Then the output contains `Return code:`
  ## Scenario : Getting the version
  - When I run `./sut -v`
  - Then I get `sut version 1.0`
  ## Scenario : Getting help
  - When I run `./sut -h`
  - Then output contains `Usage:`
  ~~~
- And there is no `index1.md` file
- And there is no `index2.md` file
- And there is no `verbose_output_OK.md` file

Reference output production in verbose mode
- When I successfully run `./bbt -c --yes -v OK_scen.md --index verbose_output_OK.md`

Compare with quiet mode output
- When I successfully run `./bbt -c --yes -q OK_scen.md --index index_1.md`
- Then `index_1.md` is equal to file `verbose_output_OK.md`
  
Compare with default mode output
- When I successfully run `./bbt -c --yes    OK_scen.md --index index_2.md`
- Then `index_2.md` is equal to file `verbose_output_OK.md`


### Scenario: Unsuccessful run index file

- Given the file `NOK_scen.md`
  ~~~md
  # Scenario: sut version
  - When I run `./sut -v`
  - Then I get `v3.1`
  ~~~
- And there is no `index3.md` file
- And there is no `index4.md` file
- And there is no `verbose_output_NOK.md` file

Reference output production in verbose mode
- When I run `./bbt -c --yes -v NOK_scen.md --index verbose_output_NOK.md`
- Then I get an error

- When I run `./bbt -c --yes -q NOK_scen.md --index index_3.md`
- Then I get an error
- And `index_3.md` is equal to file `verbose_output_NOK.md`

Compare with default mode output
- When I  run `./bbt -c --yes    NOK_scen.md --index index_4.md`
- Then  I get an error
- And `index_4.md` is equal to file `verbose_output_NOK.md`

