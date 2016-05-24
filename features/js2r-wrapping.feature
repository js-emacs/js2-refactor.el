Feature: Wrapping stuff

  Scenario: Unwrapping statement
    When I insert "console.log('hi');"
    And I turn on js2-mode and js2-refactor-mode
    And I select "'hi'"
    And I press "C-c C-m uw"
    Then I should see "'hi'"
    And I should not see "console.log"

  Scenario: Unwrapping if
    When I insert:
    """
    if (true) {
        console.log('hi');
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I select "console.log('hi');"
    And I press "C-c C-m uw"
    Then I should not see "if (true) {"
    And I should see "console.log('hi');"
    And I should not see "}"

  Scenario: Multiline unwrap
    When I insert:
    """
    if (true) {
        console.log('well hello');
        console.log('there');
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "console"
    And I set the mark
    And I go to the end of the word "there"
    And I press "C-e"
    And I press "C-c C-m uw"
    Then I should not see "if (true) {"
    And I should see:
    """
    console.log('well hello');
    console.log('there');
    """
    And I should not see "}"

  Scenario: Unwrapping without selection
    When I insert:
    """
    if (true) {
        console.log('well hello');
        console.log('there');
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "hello"
    And I press "C-c C-m uw"
    Then I should not see "if (true) {"
    And I should see "console.log('well hello');"
    And I should not see "console.log('there');"
    And I should not see "}"
