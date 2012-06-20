Feature: Expand and collapse things

  Scenario: Expanding objects
    When I insert "var a = { b: 1, c: 'def' };"
    And I turn on js2-mode
    And I go to the front of the word "b"
    And I press "C-c RET eo"
    Then I should see:
    """
    var a = {
        b: 1,
        c: 'def'
    };
    """

  Scenario: Expanding objects with comma
    When I insert "var a = { b: 1, c: 'def, ghi' };"
    And I turn on js2-mode
    And I go to the front of the word "b"
    And I press "C-c RET eo"
    Then I should see:
    """
    var a = {
        b: 1,
        c: 'def, ghi'
    };
    """
