# Feature: JUnit XML Export

<!-- omit from toc -->
## Description

This feature adds the ability to export test results in JUnit XML format, which is widely supported by CI/CD systems and test reporting tools.

The JUnit XML format is a standard format for representing test results. It's commonly used in continuous integration systems like Jenkins, GitHub Actions, and GitLab CI. The format definition retained for `bbt` is [this one](https://github.com/testmoapp/junitxml).

<!-- omit from toc -->
## Command Line Interface

```bash
bbt --junit=output.xml [other_options] <test_files>
```

<!-- omit from toc -->
## XML Structure Mapping

| bbt Concept | JUnit XML Element |
|------------|------------------|
| Document | testsuites |
| Feature | testsuite |
| Scenario | testcase |
| Passed test | testcase (no child elements) |
| Failed test | testcase with failure element |
| Error test | testcase with error element |
| Skipped test | testcase with skipped element |

<!-- omit from toc -->
## References

- [Open Test Reporting](https://github.com/ota4j-team/open-test-reporting)
- [GitHub Actions JUnit Annotation](https://github.com/testmoapp/junitxml)
  
--- 

_Table of Contents_:
- [Feature: JUnit XML Export](#feature-junit-xml-export)
  - [Scenario 1 : Basic file with only a single scenario](#scenario-1--basic-file-with-only-a-single-scenario)
    - [Feature : Skipped tests count](#feature--skipped-tests-count)
    - [Background](#background)
  - [Scenario 1: both scenario are run](#scenario-1-both-scenario-are-run)
    - [Scenario 2 : one scenario is skipped](#scenario-2--one-scenario-is-skipped)
  - [Feature : xml robustness](#feature--xml-robustness)
- [Scenario: Test XML escaping with special characters](#scenario-test-xml-escaping-with-special-characters)

## Scenario 1 : Basic file with only a single scenario

- Given there is no file `result.xml`
- Given the file `simple_test.md` containing
```markdown
# Scenario: Passing test
- When I run `./sut --version`
- Then output contains `version`
```

- When I run `./bbt --junit result.xml simple_test.md`

- Then I get no error
- And file `result.xml` contains
```xml
<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="result" tests="1" failures="0" errors="0" skipped="0">
  <testsuite name="" tests="1" failures="0" errors="0" skipped="0">
    <testcase name="Passing test" classname=""/>
  </testsuite>
</testsuites> 
```

### Feature : Skipped tests count

### Background 
- Given there is no `file1` file
- Given there is no `file2` file
- Given the scenarios file `skipped_count_test.md` 
```md
# Scenario: @Windows_Specific test
- When I run `./sut create file1.tmp`
- Then there is a `file1.tmp` file

# Scenario: OS Agnostic test  
- When I run `./sut create file1.tmp`
- Then there is a `file1.tmp` file
```

## Scenario 1: both scenario are run

- Given there is no `all_results.xml` file

- When I successfully run `./bbt --junit all_results.xml skipped_count_test.md`

- Then I get no error
- And file `all_results.xml` contains
```xml
<testsuites name="all_results" tests="2" failures="0" errors="0" skipped="0">
```

### Scenario 2 : one scenario is skipped

- Given there is no `windows_results.xml` file

- When I successfully run `./bbt --junit windows_results.xml --select @Windows_Specific skipped_count_test.md`

- Then file `windows_results.xml` contains
```xml
<testsuites name="windows_results" tests="2" failures="0" errors="0" skipped="1">
```

## Feature : xml robustness 

The Feature and the Scenario name are included in the xml syntax, and so special character like " should be escaped.
(Those characters are called XML character entities, refer to https://www.tutorialspoint.com/xml/xml_character_entities.htm) 

# Scenario: Test XML escaping with special characters
- Given there is no file `result2.xml`
- Given the file `escape_test.md` containing
```markdown
# Scenario: Title with Quotation mark " Ampersand & Greater and less than < > or Apostrophe '"
- When I run `./sut --version`
- Then output contains `version`
```

- When I run `./bbt --junit result2.xml escape_test.md`

- Then I get no error
- And file `result2.xml` contains
```xml
<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="result2" tests="1" failures="0" errors="0" skipped="0">
  <testsuite name="" tests="1" failures="0" errors="0" skipped="0">
    <testcase name="Title with Quotation mark &quot; Ampersand &amp; Greater and less than &lt; &gt; or Apostrophe &apos;&quot;" classname=""/>
  </testsuite>
</testsuites> 
```
