## Feature : *as in* test


Testing that a message is put on standard output
and that the command returns no error

### Scenario : Short option form
  - When I run 'uut -v'
  - Then I get no error
  - And I get 'uut v0.1.0'

### Scenario : Long form

Checking the postconditions of another test when running something else
(here, an equivalent option on command line

  - Then I get just as in `Short option form` scenario

Testing an expected multiline output

### Scenario : asking for uut help

- Then I get 

```
uut usage :
uut file_name create|read|append [-with text]
```

-- More extensive explanations : http://lionel.draghi.free.fr/Archicheck/rules/
-- 
-- File generated with BBT 0.1.0-dev
