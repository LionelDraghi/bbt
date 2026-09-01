# Transforming User Stories to bbt

Convert user stories ("As a [role], I want to [goal] so that [benefit]") into executable bbt scenarios.

---

## User Story Transformation Guide

User stories follow the pattern:
```
As a [role]
I want to [feature]
So that [benefit]
```

**Focus on the "I want to [feature]" part** for bbt scenarios, as this describes the actual functionality to test.

---

## Examples

### Example 1: Basic User Story

**User Story:**
> As a developer, I want to validate my JSON files so that I can catch syntax errors early.

**Transformation:**
~~~markdown
## Scenario: Valid JSON file passes validation

- Given the file `valid.json` containing
```json
{
  "name": "test",
  "value": 42
}
```
- When I run `json_validator valid.json`
- Then I get no error

## Scenario: Invalid JSON file fails validation

- Given the file `invalid.json` containing `{"name": "test",}`
- When I run `json_validator invalid.json`
- Then I get an error
- And output contains `Syntax error`
~~~

**Mapping:**
- Role: developer → Context for scenario
- Feature: validate JSON files → `When I run json_validator`
- Benefit: catch syntax errors early → Test both valid and invalid cases

---

### Example 2: File Processing Story

**User Story:**
> As a data analyst, I want to convert CSV files to JSON format so that I can work with the data in my preferred format.

**Transformation:**
~~~markdown
## Scenario: CSV to JSON conversion

- Given the file `input.csv` containing
```csv
name,age,city
Alice,30,NYC
Bob,25,LA
```
- When I run `converter input.csv output.json`
- Then there is a file `output.json`
- And file `output.json` contains
```json
[{"name": "Alice", "age": 30, "city": "NYC"}, {"name": "Bob", "age": 25, "city": "LA"}]
```
- And I get no error
~~~

---

### Example 3: Error Handling Story

**User Story:**
> As a system administrator, I want to receive clear error messages when configuration is invalid so that I can quickly fix issues.

**Transformation:**
~~~markdown
## Scenario: Invalid configuration produces clear error

- Given the file `invalid_config.yaml` containing
```yaml
key: value
  invalid_indent: true
```
- When I run `app --config invalid_config.yaml`
- Then I get error
- And output contains `Configuration error`
- And output contains `Line 2`
- And output contains `Invalid indentation`
~~~

---

### Example 4: Multi-Step Workflow Story

**User Story:**
> As a DevOps engineer, I want to deploy my application with a single command so that the process is simple and repeatable.

**Transformation:**
~~~markdown
## Scenario: Single-command deployment

- Given the file `deploy.sh` containing
```bash
#!/bin/bash
npm install
npm run build
pm2 start server.js
```
- When I successfully run `chmod +x deploy.sh`
- And I successfully run `./deploy.sh`
- Then output contains `Build successful`
- And output contains `App started`
- And I get no error
~~~

---

### Example 5: Negative Testing Story

**User Story:**
> As a security tester, I want to ensure that the system rejects invalid credentials so that unauthorized access is prevented.

**Transformation:**
```markdown
## Scenario: Invalid credentials are rejected

- Given the file `users.db` containing `admin:password123`
- When I run `login --user admin --password wrongpassword`
- Then I get error
- And output contains `Authentication failed`

## Scenario: Valid credentials are accepted

- Given the file `users.db` containing `admin:password123`
- When I run `login --user admin --password password123`
- Then output contains `Authentication successful`
- And I get no error
```

---

## Transformation Patterns

| **User Story Part** | **bbt Element** | **Example** |
|---------------------|-----------------|-------------|
| **Role** | Context/Tag | `[DevOps]`, `[Security]` |
| **Feature (I want to...)** | Scenario name + `When` steps | "File conversion" → `When I run converter` |
| **Benefit (so that...)** | Test objective | Catch errors → Add error test cases |
| **Acceptance Criteria** | Individual scenarios | Each criterion = one scenario |

---

## Handling Acceptance Criteria

User stories often include acceptance criteria:
```
Given [context]
When [action]
Then [result]
```

These map **directly** to bbt syntax!

**Example:**

**User Story with Criteria:**
> As a user, I want to search for products so that I can find what I need.
>
> **Acceptance Criteria:**
> - Given I have entered a search term, When I click search, Then I see matching products
> - Given I have not entered a search term, When I click search, Then I see an error message

**Transformation:**
```markdown
## Scenario: Search with valid term shows results

- Given I have entered a search term `laptop`
- When I run `search "laptop"`
- Then output contains `Product 1: Laptop`
- And output contains `Product 2: Gaming Laptop`
- And I get no error

## Scenario: Search without term shows error

- Given I have not entered a search term
- When I run `search ""`
- Then I get error
- And output contains `Please enter a search term`
```

---

## Tips for User Story Transformation

1. **Focus on the "I want to" part**: This is the core functionality to test
2. **Create scenarios for each acceptance criterion**: One criterion = one scenario
3. **Test both happy path and edge cases**: Include positive and negative tests
4. **Use the role as a tag**: Add tags like `[DevOps]`, `[Security]`, `[User]`
5. **Include the benefit in scenario descriptions**: Helps understand the purpose
6. **Break down complex stories**: Split into multiple scenarios if needed

---

## Common User Story Templates

### Template 1: Feature Validation
> As a [role], I want to [validate X] so that [benefit].

**bbt:**
```markdown
## Scenario: [X] validation

- Given [setup]
- When I run [validation command]
- Then output contains [expected result]
```

### Template 2: File Processing
> As a [role], I want to [process files] so that [benefit].

**bbt:**
```markdown
## Scenario: File processing

- Given the file [input] containing...
- When I run [processing command]
- Then there is a file [output]
- And file [output] contains...
```

### Template 3: Error Prevention
> As a [role], I want to [prevent X] so that [benefit].

**bbt:**
```markdown
## Scenario: [X] is prevented

- Given [invalid setup]
- When I run [command]
- Then I get error
- And output contains [error message]
```

---

## Complete Example: Full User Story with Scenarios

**Original User Story:**
> As a content manager
> I want to publish articles to the website
> So that readers can access new content
>
> **Acceptance Criteria:**
> - Given I have written an article, When I run the publish command, Then the article appears on the website
> - Given I have not provided a title, When I run the publish command, Then I receive an error
> - Given I have provided invalid markdown, When I run the publish command, Then I receive a formatting error

**Transformed bbt Scenarios:**
~~~markdown
# Feature: Article Publishing, [ContentManager]

## Scenario: Valid article publishes successfully

- Given the file `article.md` containing
```markdown
# My Article Title

This is the article content.
```
- When I run `publish article.md`
- Then output contains `Article published`
- And output contains `http://website.com/my-article-title`
- And I get no error

## Scenario: Missing title produces error

- Given the file `article_no_title.md` containing
```markdown
This is content without a title.
```
- When I run `publish article_no_title.md`
- Then I get error
- And output contains `Title is required`

## Scenario: Invalid markdown produces formatting error

- Given the file `article_bad_markdown.md` containing
```markdown
# Title

This has **invalid** markdown: [unclosed link
```
- When I run `publish article_bad_markdown.md`
- Then I get error
- And output contains `Markdown formatting error`
~~~

---

## Best Practices

1. **Start with the happy path**: Test the ideal scenario first
2. **Add edge cases**: Test boundary conditions and error cases
3. **Use descriptive names**: Include the user story reference if possible
4. **Add tags**: Use the role and any other relevant categorizations
5. **Keep scenarios independent**: Each scenario should work standalone
6. **Document assumptions**: Add comments about any assumptions made
