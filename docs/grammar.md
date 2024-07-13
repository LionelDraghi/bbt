| Prep  |     |Subject |       Verb       |      Object       |        Action        |  
|-------|-----|--------|------------------|-------------------|----------------------|  
| Given |     |        | is               | `file`            | CHECK_FILE_EXISTENCE |  
| Given |     |        | is               | `dir`             | CHECK_DIR_EXISTENCE  |  
| Given |     |        | is no            | `file`            | SETUP_NO_FILE        |  
| Given |     |        | is no            | `dir`             | SETUP_NO_DIR         |  
| Given |     | `dir`  |                  |                   | CREATE_DIRECTORY     |  
| Given |     | `file` |                  |                   | CREATE_FILE          |  
| Given |     | `file` | containing       | `text`            | CREATE_FILE          |  
| Given | new | `dir`  |                  |                   | ERASE_AND_CREATE     |  
| Given | new | `file` |                  |                   | ERASE_AND_CREATE     |  
| Given | new | `file` | containing       | `text`            | ERASE_AND_CREATE     |  
| When  |     |        | run              | `text`            | RUN_CMD              |  
| When  |     |        | run              | `cmd` [or `cmd`]* | RUN_CMD              |  
| When  |     |        | successfully run | `text`            | RUN_WITHOUT_ERROR    |  
| When  |     |        | successfully run | `cmd` [or `cmd`]* | RUN_WITHOUT_ERROR    |  
| Then  |     |        | get              |                   | OUTPUT_IS            |  
| Then  |     |        | get              | `text`            | OUTPUT_IS            |  
| Then  |     |        | get              | error             | ERROR_RETURN_CODE    |  
| Then  |     |        | get no           | error             | NO_ERROR_RETURN_CODE |  
| Then  |     |        | is               | `file`            | CHECK_FILE_EXISTENCE |  
| Then  |     |        | is               | `dir`             | CHECK_DIR_EXISTENCE  |  
| Then  |     |        | is no            | `file`            | CHECK_NO_FILE        |  
| Then  |     |        | is no            | `dir`             | CHECK_NO_DIR         |  
| Then  |     | `file` | contains         |                   | FILE_CONTAINS        |  
| Then  |     | `file` | contains         | `text`            | FILE_CONTAINS        |  
| Then  |     | `file` | is               |                   | FILE_IS              |  
| Then  |     | `file` | is               | `text`            | FILE_IS              |  
| Then  |     | output | contains         |                   | OUTPUT_CONTAINS      |  
| Then  |     | output | contains         | `text`            | OUTPUT_CONTAINS      |  
| Then  |     | output | is               |                   | OUTPUT_IS            |  
| Then  |     | output | is               | `file`            | OUTPUT_IS            |  
| Then  |     | output | is               | `text`            | OUTPUT_IS            |  
