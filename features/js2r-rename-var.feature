Feature: Rename variable
  In order to quickly rename a local variable
  As an Emacs user with js2-refactor
  I want to change all occurrences of the variable at once

  Scenario: Rename a single name
    Given delete-selection-mode is active
    When I insert "var abc = 123;"
    And I turn on js2-mode
    And I go to the front of the word "abc"
    And I press "C-c RET rv"
    And I type "def"
    Then I should see "var def = 123;"

  Scenario: Rename two instances of name
    Given delete-selection-mode is active
    When I insert "var abc = 123, def = abc;"
    And I turn on js2-mode
    And I go to character ";"
    And I press "C-b"
    And I press "C-c RET rv"
    And I type "ghi"
    Then I should see "var ghi = 123, def = ghi;"

  Scenario: Rename function params
    Given delete-selection-mode is active
    When I insert "function test(abc) { alert(abc); }"
    And I turn on js2-mode
    And I go to the end of the word "alert"
    And I press "C-f"
    And I press "C-c RET rv"
    And I type "ghi"
    Then I should see "function test(ghi) { alert(ghi); }"

  Scenario: Don't rename object literal keys
    Given delete-selection-mode is active
    When I insert "var abc = { abc: 123 };"
    And I turn on js2-mode
    And I go to the end of the word "var"
    And I press "C-f"
    And I press "C-c RET rv"
    And I type "ghi"
    Then I should see "var ghi = { abc: 123 };"

  Scenario: Don't rename object property access
    Given delete-selection-mode is active
    When I insert "var abc = this.abc;"
    And I turn on js2-mode
    And I go to the end of the word "var"
    And I press "C-f"
    And I press "C-c RET rv"
    And I type "ghi"
    Then I should see "var ghi = this.abc;"
