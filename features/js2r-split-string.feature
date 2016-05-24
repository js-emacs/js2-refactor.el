Feature: Split string

  Scenario: Split simple string
    When I insert: 
    """
    "foo bar"
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor before "bar"
    And I press "C-c C-m ss"
    Then I should see:
    """
    "foo " + "bar"
    """

  Scenario: Split single quoted string
    When I insert: 
    """
    'foo bar'
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor before "bar"
    And I press "C-c C-m ss"
    Then I should see:
    """
    'foo ' + 'bar'
    """

  Scenario: Join string
    When I insert: 
    """
    "foo" + "bar"
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor after "foo"
    And I press "C-c C-m ss"
    Then I should see:
    """
    "foobar"
    """

  Scenario: Join single quoted string
    When I insert: 
    """
    'foo' + 'bar'
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor after "foo"
    And I press "C-c C-m ss"
    Then I should see:
    """
    'foobar'
    """
