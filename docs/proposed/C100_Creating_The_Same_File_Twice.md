## Feature: Overwriting a created file detection (@WIP)

When creating a scenario by cut and pasting, one can have two Given Steps that create the same file.
bbt put a warning in this case.

But the warning should be limited to situation where the file is overwritten in the same Background.
If overwritten in the same scenario, is it always bad situation?
If it's overwritten in the same preconditions sequence, that is one of the backgrounds + Given steps 
in the scenarios, is it always a problem?

So, it's not easy to discriminate what deserve a Warning from what doesn't.

# Background : 
- Given the file `create_twice_the_same_file.md`
~~~
# Scenario:
- Given the new file `tmp.1` containing `content 1`
- Given the new file `tmp.1` containing `content 2`
~~~

# Scenario:
- When I run `./bbt --yes create_twice_the_same_file.md` 
- Then I get no error
- And file `tmp.1` is equal to `content 2`
- And output contains `Warning: overwriting tmp.1`   