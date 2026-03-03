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
  - [Feature : Time Attribute (@Flaky results depends on execution time on the plafform)](#feature--time-attribute-flaky-results-depends-on-execution-time-on-the-plafform)
    - [Scenario:](#scenario)

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
- And file `result.xml` contains `<?xml version="1.0" encoding="UTF-8"?>`
- And file `result.xml` matches `<testsuites name="result" tests="1" failures="0" errors="0" skipped="0" time="[0-9]*\.[0-9]*">`
- And file `result.xml` matches `  <testsuite name="" tests="1" failures="0" errors="0" skipped="0" time="[0-9]*\.[0-9]*">`
- And file `result.xml` matches `    <testcase name="Passing test" classname="" time="[0-9]*\.[0-9]*"/>`
- And file `result.xml` contains
~~~
  </testsuite>
</testsuites>
~~~

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
- And file `all_results.xml` matches `<testsuites name="all_results" tests="2" failures="0" errors="0" skipped="0" time="[0-9]*\.[0-9]*">`

### Scenario 2 : one scenario is skipped

- Given there is no `windows_results.xml` file

- When I successfully run `./bbt --junit windows_results.xml --select @Windows_Specific skipped_count_test.md`

- Then file `windows_results.xml` matches `<testsuites name="windows_results" tests="2" failures="0" errors="0" skipped="1" time="[0-9]*\.[0-9]*">`

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
- And file `result2.xml` matches `    <testcase name="Title with Quotation mark &quot; Ampersand &amp; Greater and less than &lt; &gt; or Apostrophe &apos;&quot;" classname="" time="[0-9]*\.[0-9]*"/>`

## Feature : Time Attribute (@Flaky results depends on execution time on the plafform) 

This feature tests that timestamp fields are properly included in JUnit XML output at all levels.
This test is using sut capability to wait before returning according to a command line parameter thanks to the `delay` command.

### Scenario: 
- Given there is no file `time_attribute.xml`
- Given the file `time_attribute.md` containing
```md
# Feature: F1
## Scenario: S1
- When I run `./sut delay 0.1`
- Then I get no error

# Feature: F2
## Scenario: S2
- When I run `./sut delay 0.2`
- And  I run `./sut delay 0.4`
- Then I get no error
## Scenario: S3
- When I run `./sut delay 0.3`
- And  I run `./sut delay 0.5`
- Then I get no error
```

So that :
* total should be around 1.5s
* total F1 = total S1 = 0.1s
* total F2 = 1.4
* total S2 = 0.6
* total S3 = 0.8
FIXME: robustness fail when using '-' 

- When I successfully run `./bbt --junit time_attribute.xml time_attribute.md`

- Then file `time_attribute.xml` matches `<testsuites .* time="1.6[0-9]*">`
  Should be 1.5, but it seems that execution time outside steps is between 0.1 and 0.2s on my platform
- And  file `time_attribute.xml` matches ` *<testsuite .* time="1.6[0-9]*">`
- And  file `time_attribute.xml` matches ` *<testcase name="S1" .* time="0.1[0-9]*"/>`
- And  file `time_attribute.xml` matches ` *<testcase name="S2" .* time="0.6[0-9]*"/>`
- And  file `time_attribute.xml` matches ` *<testcase name="S3" .* time="0.8[0-9]*"/>`
