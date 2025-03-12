## Feature: Tmp dir 

If you want to keep the .out file (log of the standard output during the run), use the --tmp_dir option.

Fixme : --tmp_dir isn't a good name : should be something like --keep_out_file, and the -o option should be modified to accept full name (with dir).

### Scenario 

- Given there is no `tmp.txt` file
- Given there is no `dir1` dir
- Given there is no `dir2` dir

- Given the file `scen1.md`
~~~
# Scenario: 
- When I run `./sut create tmp.txt`
- Then there is a `tmp.txt` file
~~~

- When I run `./bbt scen1.md` 
- Then there is a `scen1.md.out` file

- When I run `./bbt --cleanup scen1.md` 
- Then there is no `scen1.md.out` file

- When I run `./bbt --tmp_dir dir1 scen1.md` 
- Then there is a `dir1` directory
- And  there is a `dir1/scen1.md.out` file

- When I run `./bbt --cleanup --tmp_dir dir2 scen1.md` 
- Then there is no `dir2/scen1.md.out` file
- And  there is no `dir2` directory
