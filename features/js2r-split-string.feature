Feature: Split string

  Scenario: Split simple string
    When I insert: 
    """
    "foo bar"
    """
    And I turn on js2-mode
    And I place the cursor before "bar"
    And I press "C-c C-m ss"
    Then I should see:
    """
    "foo " + "bar"
    """
