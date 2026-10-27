| Prep  |         |Subject |       Verb       | Object |         Action          | Code block expected | Use Example |  
|-------|---------|--------|------------------|--------|-------------------------|---------------------|-------------|  
| Given |         |        | run        | `text`            | RUN_CMD                 |            | - Given I [successfully] run [shell] `cmd` |  
| Given |         |        | is         | `file`            | CHECK_FILE_EXISTENCE    |            | - Given there is a `config.ini` file |  
| Given |         |        | is         | `dir`             | CHECK_DIR_EXISTENCE     |            | - Given there is a `dir1` directory |  
| Given |         | `dir`  |            |                   | CREATE_IF_NONE          |            | - Given the directory `dir1` |  
| Given |         | `file` |            |                   | CREATE_IF_NONE          |     X      | - Given the file `config.ini` <followed by code fenced lines> |  
| Given |         | `file` | containing |                   | CREATE_IF_NONE          |     X      | - Given the file `config.ini` containing <followed by code fenced lines> |  
| Given |         | `file` | containing | `text`            | CREATE_IF_NONE          |            | - Given the file `config.ini` containing `lang=it` |  
| Given |         | `text` | fail       |                   | RUN_WITH_ERROR          |            | - Given `xmllint mismatched_tag.xml` fails |  
| Given |         | `var`  | is         |                   | SET_ENV_VAR             |            | - Given the environment variable `NO_COLOR` is set |  
| Given |         | `var`  | is         | `text`            | SET_ENV_VAR             |            | - Given the environment variable `LANG` is `C` |  
| Given | new     | `dir`  |            |                   | ERASE_AND_CREATE        |            | - Given the new directory `dir1` |  
| Given | new     | `file` |            |                   | ERASE_AND_CREATE        |     X      | - Given the new file `config.ini` <followed by code fenced lines> |  
| Given | new     | `file` | containing |                   | ERASE_AND_CREATE        |     X      | - Given the new file `config.ini` containing <followed by code fenced lines> |  
| Given | new     | `file` | containing | `text`            | ERASE_AND_CREATE        |            | - Given the new file `config.ini` containing `lang=it` |  
| When  |         |        | run        | `text`            | RUN_CMD                 |            | - When I [successfully] run [shell] `cmd` |  
| When  |         |        | run        | `cmd` [or `cmd`]* | RUN_CMD                 |            | - When I [successfully] run [shell] `cmd` or `cmd2` or `cmd3` |  
| When  |         |        | type       | `text`            | TYPE_TEXT               |            | - When I type `Y` |  
| When  |         |        | enter      | `text`            | ENTER_TEXT              |            | - When I enter `Y` |  
| Then  |         |        | run        | `text`            | RUN_CMD                 |            | - Then I [successfully] run [shell] `cmd` |  
| Then  |         |        | get        |                   | OUTPUT_IS               |     X      | - Then I get <followed by code fenced lines> |  
| Then  |         |        | get        | output            | OUTPUT_NOT_EMPTY        |            | - Then I get output |  
| Then  |         |        | get        | error output      | STDERR_NOT_EMPTY        |            | - Then I get error output |  
| Then  |         |        | get        | `file`            | OUTPUT_IS               |            | - Then I get file `flowers2.txt` |  
| Then  |         |        | get        | `text`            | OUTPUT_IS               |            | - Then I get `msg` |  
| Then  |         |        | get        | error             | ERROR_RETURN_CODE       |            | - Then I get error |  
| Then  |         |        | is         | output            | OUTPUT_NOT_EMPTY        |            | - Then there is output |  
| Then  |         |        | is         | error output      | STDERR_NOT_EMPTY        |            | - Then there is error output |  
| Then  |         |        | is         | `file`            | CHECK_FILE_EXISTENCE    |            | - Then there is a  `config.ini` file |  
| Then  |         |        | is         | `dir`             | CHECK_DIR_EXISTENCE     |            | - Then there is a  `dir1` directory |  
| Then  |         |        | is         | error             | ERROR_RETURN_CODE       |            | - Then there is an error |  
| Then  |         | `file` | contains   |                   | FILE_CONTAINS           |     X      | - Then the file `list` contains <followed by code fenced lines> |  
| Then  |         | `file` | contains   | `file`            | FILE_CONTAINS           |            | - Then the file `list` contains `snippet.txt` file |  
| Then  |         | `file` | contains   | `text`            | FILE_CONTAINS           |            | - Then the file `list` contains `--version` |  
| Then  |         | `file` | matches    | `text`            | FILE_MATCHES            |            | - Then the file `list` matches `*.string.*` |  
| Then  |         | `file` | is         |                   | FILE_IS                 |     X      | - Then the file `list` is <followed by code fenced lines> |  
| Then  |         | `file` | is         | `file`            | FILE_IS                 |            | - Then the file `list` is equal to file `expected/list` |  
| Then  |         | `file` | is         | `text`            | FILE_IS                 |            | - Then the file `list` is `mode=silent` |  
| Then  |         | `text` | fail       |                   | RUN_WITH_ERROR          |            | - Then `xmllint mismatched_tag.xml` fails |  
| Then  |         | output | contains   |                   | OUTPUT_CONTAINS         |     X      | - Then the output contains <followed by code fenced lines> |  
| Then  |         | output | contains   | `file`            | OUTPUT_CONTAINS         |            | - Then the output contains `snippet.txt` file |  
| Then  |         | output | contains   | `text`            | OUTPUT_CONTAINS         |            | - Then the output contains `msg` |  
| Then  |         | output | matches    | `text`            | OUTPUT_MATCHES          |            | - Then the output matches `[:digit:]*.[:digit:]*` |  
| Then  |         | output | is         |                   | OUTPUT_IS               |     X      | - Then the output is equal to <followed by code fenced lines> |  
| Then  |         | output | is         | `file`            | OUTPUT_IS               |            | - Then the output is equal to file `expected.txt` |  
| Then  |         | output | is         | `text`            | OUTPUT_IS               |            | - Then the output is `msg` |  
| Then  |         | stderr | contains   |                   | STDERR_CONTAINS         |     X      | - Then the error output contains <followed by code fenced lines> |  
| Then  |         | stderr | contains   | `file`            | STDERR_CONTAINS         |            | - Then the error output contains file `snippet.txt` |  
| Then  |         | stderr | contains   | `text`            | STDERR_CONTAINS         |            | - Then the error output contains `msg` |  
| Then  |         | stderr | is         |                   | STDERR_IS               |     X      | - Then the error output is <followed by code fenced lines> |  
| Then  |         | stderr | is         | `file`            | STDERR_IS               |            | - Then the error output is equal to file `expected.txt` |  
| Then  |         | stderr | is         | `text`            | STDERR_IS               |            | - Then the error output is `msg` |  
| Then  |         | exit   | is         | `text`            | EXIT_CODE_IS            |            | - Then the exit code is `2` |  
