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
