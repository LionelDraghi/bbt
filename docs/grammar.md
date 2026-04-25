| Prep  |         |Subject |       Verb       | Object |         Action          | Code block expected | Use Example |  
|-------|---------|--------|------------------|--------|-------------------------|---------------------|-------------|  
| Given |         |        | run              | `text`            | RUN_CMD                 |            | - Given I run `cmd` |  
| Given |         |        | successfully run | `text`            | RUN_WITHOUT_ERROR       |            | - Given I successfully run `cmd` |  
| Given |         |        | is               | `file`            | CHECK_FILE_EXISTENCE    |            | - Given there is a `config.ini` file |  
| Given |         |        | is               | `dir`             | CHECK_DIR_EXISTENCE     |            | - Given there is a `dir1` directory |  
| Given |         |        | is no            | `file`            | SETUP_NO_FILE           |            | - Given there is no `config.ini` file |  
| Given |         |        | is no            | `dir`             | SETUP_NO_DIR            |            | - Given there is no `dir1` directory |  
| Given |         | `dir`  |                  |                   | CREATE_IF_NONE          |            | - Given the directory `dir1` |  
| Given |         | `file` |                  |                   | CREATE_IF_NONE          |     X      | - Given the file `config.ini` <followed by code fenced lines> |  
| Given |         | `file` | containing       |                   | CREATE_IF_NONE          |     X      | - Given the file `config.ini` containing <followed by code fenced lines> |  
| Given |         | `file` | containing       | `text`            | CREATE_IF_NONE          |            | - Given the file `config.ini` containing `lang=it` |  
| Given |         | `text` | fail             |                   | RUN_WITH_ERROR          |            | - Given `xmllint mismatched_tag.xml` fails |  
| Given | new     | `dir`  |                  |                   | ERASE_AND_CREATE        |            | - Given the new directory `dir1` |  
| Given | new     | `file` |                  |                   | ERASE_AND_CREATE        |     X      | - Given the new file `config.ini` <followed by code fenced lines> |  
| Given | new     | `file` | containing       |                   | ERASE_AND_CREATE        |     X      | - Given the new file `config.ini` containing <followed by code fenced lines> |  
| Given | new     | `file` | containing       | `text`            | ERASE_AND_CREATE        |            | - Given the new file `config.ini` containing `lang=it` |  
| When  |         |        | run              | `text`            | RUN_CMD                 |            | - When I run `cmd` |  
| When  |         |        | run              | `cmd` [or `cmd`]* | RUN_CMD                 |            | - When I run `cmd` or `cmd2` or `cmd3` |  
| When  |         |        | successfully run | `text`            | RUN_WITHOUT_ERROR       |            | - When I successfully run `cmd` |  
| When  |         |        | successfully run | `cmd` [or `cmd`]* | RUN_WITHOUT_ERROR       |            | - When I successfully run `cmd` or `cmd2` or `cmd3` |  
| Then  |         |        | successfully run | `text`            | RUN_WITHOUT_ERROR       |            | - Then I successfully run `cmd` |  
| Then  |         |        | get              |                   | OUTPUT_IS               |     X      | - Then I get <followed by code fenced lines> |  
| Then  |         |        | get              | `file`            | OUTPUT_IS               |            | - Then I get file `flowers2.txt` |  
| Then  |         |        | get              | `text`            | OUTPUT_IS               |            | - Then I get `msg` |  
| Then  |         |        | get              | error             | ERROR_RETURN_CODE       |            | - Then I get error |  
| Then  |         |        | get no           | output            | NO_OUTPUT               |            | - Then there is no output |  
| Then  |         |        | get no           | error             | NO_ERROR_RETURN_CODE    |            | - Then I get no error |  
| Then  |         |        | is               | `file`            | CHECK_FILE_EXISTENCE    |            | - Then there is a  `config.ini` file |  
| Then  |         |        | is               | `dir`             | CHECK_DIR_EXISTENCE     |            | - Then there is a  `dir1` directory |  
| Then  |         |        | is               | error             | ERROR_RETURN_CODE       |            | - Then there is an error |  
| Then  |         |        | is no            | output            | NO_OUTPUT               |            | - Then there is no output |  
| Then  |         |        | is no            | `file`            | CHECK_NO_FILE           |            | - Then there is no `config.ini` file |  
| Then  |         |        | is no            | `dir`             | CHECK_NO_DIR            |            | - Then there is no `dir1` directory |  
| Then  |         |        | is no            | error             | NO_ERROR_RETURN_CODE    |            | - Then there is no error |  
| Then  |         | `file` | does not contain |                   | FILE_DOES_NOT_CONTAIN   |     X      | - Then the file `list` does not contain <followed by code fenced lines> |  
| Then  |         | `file` | does not contain | `file`            | FILE_DOES_NOT_CONTAIN   |            | - Then the file `list` does not contain `snippet.txt` file |  
| Then  |         | `file` | does not contain | `text`            | FILE_DOES_NOT_CONTAIN   |            | - Then the file `list` does not contain `--version` |  
| Then  |         | `file` | contains         |                   | FILE_CONTAINS           |     X      | - Then the file `list` contains <followed by code fenced lines> |  
| Then  |         | `file` | contains         | `file`            | FILE_CONTAINS           |            | - Then the file `list` contains `snippet.txt` file |  
| Then  |         | `file` | contains         | `text`            | FILE_CONTAINS           |            | - Then the file `list` contains `--version` |  
| Then  |         | `file` | matches          | `text`            | FILE_MATCHES            |            | - Then the file `list` matches `*.string.*` |  
| Then  |         | `file` | does not match   | `text`            | FILE_DOES_NOT_MATCH     |            | - Then the file `list` does not match `*.string.*` |  
| Then  |         | `file` | is               |                   | FILE_IS                 |     X      | - Then the file `list` is <followed by code fenced lines> |  
| Then  |         | `file` | is               | `file`            | FILE_IS                 |            | - Then the file `list` is equal to file `expected/list` |  
| Then  |         | `file` | is               | `text`            | FILE_IS                 |            | - Then the file `list` is `mode=silent` |  
| Then  |         | `file` | is no            | `file`            | FILE_IS_NOT             |            | - Then the file `list` is no more equal to file `previous_list` |  
| Then  |         | `text` | fail             |                   | RUN_WITH_ERROR          |            | - Then `xmllint mismatched_tag.xml` fails |  
| Then  |         | output | does not contain |                   | OUTPUT_DOES_NOT_CONTAIN |     X      | - Then the output does not contain <followed by code fenced lines> |  
| Then  |         | output | does not contain | `file`            | OUTPUT_DOES_NOT_CONTAIN |            | - Then the output does not contain file `snippet.txt` |  
| Then  |         | output | does not contain | `text`            | OUTPUT_DOES_NOT_CONTAIN |            | - Then the output does not contain `msg` |  
| Then  |         | output | contains         |                   | OUTPUT_CONTAINS         |     X      | - Then the output contains <followed by code fenced lines> |  
| Then  |         | output | contains         | `file`            | OUTPUT_CONTAINS         |            | - Then the output contains `snippet.txt` file |  
| Then  |         | output | contains         | `text`            | OUTPUT_CONTAINS         |            | - Then the output contains `msg` |  
| Then  |         | output | matches          | `text`            | OUTPUT_MATCHES          |            | - Then the output matches `[:digit:]*.[:digit:]*` |  
| Then  |         | output | does not match   | `text`            | OUTPUT_DOES_NOT_MATCH   |            | - Then the output does not match `[:digit:]*.[:digit:]*` |  
| Then  |         | output | is               |                   | OUTPUT_IS               |     X      | - Then the output is equal to<followed by code fenced lines> |  
| Then  |         | output | is               | `file`            | OUTPUT_IS               |            | - Then the output is equal to file `expected.txt` |  
| Then  |         | output | is               | `text`            | OUTPUT_IS               |            | - Then the output is `msg` |  
