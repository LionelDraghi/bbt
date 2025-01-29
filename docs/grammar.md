| Prep  |     |Subject |       Verb       | Object |         Action          | Code block expected |  
|-------|-----|--------|------------------|--------|-------------------------|------------|  
| Given |     |        | run              | `text` | RUN_CMD                 |            |  
| Given |     |        | successfully run | `text` | RUN_WITHOUT_ERROR       |            |  
| Given |     |        | is               | `file` | CHECK_FILE_EXISTENCE    |            |  
| Given |     |        | is               | `dir`  | CHECK_DIR_EXISTENCE     |            |  
| Given |     |        | is no            | `file` | SETUP_NO_FILE           |            |  
| Given |     |        | is no            | `dir`  | SETUP_NO_DIR            |            |  
| Given |     | `dir`  |                  |        | CREATE_DIRECTORY        |            |  
| Given |     | `file` |                  |        | CREATE_FILE             |     X      |  
| Given |     | `file` | containing       |        | CREATE_FILE             |     X      |  
| Given |     | `file` | containing       | `text` | ERASE_AND_CREATE        |            |  
| Given | new | `dir`  |                  |        | ERASE_AND_CREATE        |            |  
| Given | new | `file` |                  |        | ERASE_AND_CREATE        |     X      |  
| Given | new | `file` | containing       |        | CREATE_FILE             |     X      |  
| Given | new | `file` | containing       | `text` | ERASE_AND_CREATE        |            |  
| When  |     |        | run              | `text` | RUN_CMD                 |            |  
| When  |     |        | run              | `cmd`  | RUN_CMD                 |            |  
| When  |     |        | successfully run | `text` | RUN_WITHOUT_ERROR       |            |  
| When  |     |        | successfully run | `cmd`  | RUN_WITHOUT_ERROR       |            |  
| Then  |     |        | get              |        | OUTPUT_IS               |     X      |  
| Then  |     |        | get              | `file` | OUTPUT_IS               |            |  
| Then  |     |        | get              | `text` | OUTPUT_IS               |            |  
| Then  |     |        | get              | error  | ERROR_RETURN_CODE       |            |  
| Then  |     |        | get no           | output | NO_OUTPUT               |            |  
| Then  |     |        | get no           | error  | NO_ERROR_RETURN_CODE    |            |  
| Then  |     |        | is               | `file` | CHECK_FILE_EXISTENCE    |            |  
| Then  |     |        | is               | `dir`  | CHECK_DIR_EXISTENCE     |            |  
| Then  |     |        | is               | error  | ERROR_RETURN_CODE       |            |  
| Then  |     |        | is no            | output | NO_OUTPUT               |            |  
| Then  |     |        | is no            | `file` | CHECK_NO_FILE           |            |  
| Then  |     |        | is no            | `dir`  | CHECK_NO_DIR            |            |  
| Then  |     |        | is no            | error  | NO_ERROR_RETURN_CODE    |            |  
| Then  |     | `file` | does not contain |        | FILE_DOES_NOT_CONTAIN   |     X      |  
| Then  |     | `file` | does not contain | `file` | FILE_DOES_NOT_CONTAIN   |            |  
| Then  |     | `file` | does not contain | `text` | FILE_DOES_NOT_CONTAIN   |            |  
| Then  |     | `file` | contains         |        | FILE_CONTAINS           |     X      |  
| Then  |     | `file` | contains         | `file` | FILE_CONTAINS           |            |  
| Then  |     | `file` | contains         | `text` | FILE_CONTAINS           |            |  
| Then  |     | `file` | is               |        | FILE_IS                 |     X      |  
| Then  |     | `file` | is               | `file` | FILE_IS                 |            |  
| Then  |     | `file` | is               | `text` | FILE_IS                 |            |  
| Then  |     | `file` | is no            | `file` | FILE_IS_NOT             |            |  
| Then  |     | output | does not contain |        | OUTPUT_DOES_NOT_CONTAIN |     X      |  
| Then  |     | output | does not contain | `file` | OUTPUT_DOES_NOT_CONTAIN |            |  
| Then  |     | output | does not contain | `text` | OUTPUT_DOES_NOT_CONTAIN |            |  
| Then  |     | output | contains         |        | OUTPUT_CONTAINS         |     X      |  
| Then  |     | output | contains         | `file` | OUTPUT_CONTAINS         |            |  
| Then  |     | output | contains         | `text` | OUTPUT_CONTAINS         |            |  
| Then  |     | output | is               |        | OUTPUT_IS               |     X      |  
| Then  |     | output | is               | `file` | OUTPUT_IS               |            |  
| Then  |     | output | is               | `text` | OUTPUT_IS               |            |  
