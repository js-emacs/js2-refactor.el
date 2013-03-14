Feature: Split var declaration

  Scenario: Split simple var
    When I insert "var a, b, c;"
    And I turn on js2-mode
    And I press "C-c C-m sv"
    Then I should see:
    """
    var a;
    var b;
    var c;
    """
    And I should not see "var a, b, c;"

  Scenario: Split vars with values
    When I insert "var a = 1, b = '2', c = { d: 3 };"
    And I turn on js2-mode
    And I press "C-c C-m sv"
    Then I should see:
    """
    var a = 1;
    var b = '2';
    var c = { d: 3 };
    """

  Scenario: Split multiline vars
    When I insert:
    """
    var a = 1,
        b = '2',

        c = {
            d: 3
        };
    """
    And I turn on js2-mode
    And I press "C-c C-m sv"
    Then I should see:
    """
    var a = 1;
    var b = '2';

    var c = {
        d: 3
    };
    """
