# Feature: 

bbt aims at being as tolerant as possible to formatting imprecisions, while still respecting [the CommonMark](https://spec.commonmark.org/) specification, and 
Markdown with Gherkin (MDG) [definitions](https://github.com/cucumber/gherkin/blob/main/MARKDOWN_WITH_GHERKIN.md)

1. Header and Title
   An [ATX Heading](https://spec.commonmark.org/0.31.2/#atx-heading) is a sequence of # (up to 6) followed by a sequence of spaces, tab or EOL, and possibly concluded with a sequence of #.  
   So, for bbt : 
   + "# Scenario" 
   + "# Scenario ################################"  
   are recognized as a Scenario, but not:  
   + "#Scenario"

   As a specific bbt rule (not sure it is true also for MDG), Gherkin keywords may be followed by a colon, and the colon is not part of the title.  
   This gives the user the ability to use Heading starting with a Gherkin keyword, but not equal to this Keyword.  
   Although not recommended, Features Scenarios, etc. without title are OK.

   For example :  
   * "# Scenario overview" 
   * "# Scenario 1.1 : overview"
   * "# Scenario: overview"  
   * "# Scenario" 
   are legal, and the matching titles will be :
   + "overview"
   + "1.1 : overview"
   + "overview"
   + ""
  
## Scenario: Heading variations

- Given the new file `1.md` containing `# Scenario: S1`
- Given the new file `2.md` containing `#Scenario: S2`
- Given the new file `3.md` containing `# Scenario : S3`
- Given the new file `4.md` containing `# Scenario S4`
- Given the new file `5.md` containing `# Scenario :S5 etc etc! #########################`
- Given the new file `6.md` containing `# Scenario`
- Given the new file `7.md` containing `# Scenario:`
- Given the new file `8.md` containing `# Scenario #################`

- When I successfully run `./bbt 1.md`
- Then output contains `scenario [S1](1.md) is empty, nothing tested`
(We don't care that the test is empty, what we are testing is that the scenario is recognized.)
  
- When I successfully run `./bbt 2.md`
- Then output contains `scenario [S2](2.md) is empty, nothing tested`
- And  output contains `2.md:1: Warning : Markdown expect space in Headings after last '#'`

- When I successfully run `./bbt 3.md`
- Then output contains `scenario [S3](3.md) is empty, nothing tested`

- When I successfully run `./bbt 4.md`
- Then output contains `scenario [S4](4.md) is empty, nothing tested`

- When I successfully run `./bbt 5.md`
- Then output contains `scenario [S5 etc etc!](5.md) is empty, nothing tested`

- When I successfully run `./bbt 6.md`
- Then output contains `scenario [](6.md) is empty, nothing tested`

- When I successfully run `./bbt 7.md`
- Then output contains `scenario [](7.md) is empty, nothing tested`

- When I successfully run `./bbt 8.md`
- Then output contains `scenario [](8.md) is empty, nothing tested`
