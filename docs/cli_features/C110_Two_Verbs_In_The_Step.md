<!-- omit from toc -->
## Scenario: Detection of ambiguous use of multiple recognized verbs in the same Step


- Given the new file `too_much_verbs_in_step.md`
```
# Scenario
- When I run `git -v`
- Then output contains `git version` (NB : pattern matching for the version number is in the to do list)
```
Here, bbt id going to recognize both 'contains' and 'is' verb. 
If it take contains, the test will be OK. 
If it takes 'is', the test will fail because the version number is missing in the expected text.

- When I run `./bbt -c --yes too_much_verbs_in_step.md`
Then I have no error
- then the output contains 
```
Warning : Verb is 'contains', ignoring following 'is'  
```