Feature: String to template

  Scenario: Convert single-quote string to template
    When I insert "const c = 'this is a string'"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "string"
    And I press "C-c C-m st"
    Then I should see "const c = `this is a string`"

  Scenario: Convert double-quote string to template
    When I insert "const c = "this is a string""
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "string"
    And I press "C-c C-m st"
    Then I should see "const c = `this is a string`"

  Scenario: Do nothing on template strings
    When I insert "const c = `this is a string`"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "string"
    And I press "C-c C-m st"
    Then I should see "const c = `this is a string`"

  Scenario: Do nothing on template strings with interpolation
    When I insert "const c = `this is a ${xyz} string`"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "string"
    And I press "C-c C-m st"
    Then I should see "const c = `this is a ${xyz} string`"

  Scenario: Escape backticks inside strings
    When I insert "const c = 'this `is a `string'"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "string"
    And I press "C-c C-m st"
    Then I should see "const c = `this \`is a \`string`"

  Scenario: Handles the empty string
    When I insert "const c = ''"
    And I turn on js2-mode and js2-refactor-mode
    And I go to character "'"
    And I press "C-c C-m st"
    Then I should see "const c = ``"
