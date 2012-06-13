Feature: Split var declaration
  In order to quickly split a multi-var declaration into separate ones
  As an Emacs user with js2-refactor
  I want the editor to do it for me

  Scenario: Split simple var
    When I insert "var a, b, c;"
    And I turn on js2-mode
    And I press "C-c RET sv"
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
    And I press "C-c RET sv"
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
    And I press "C-c RET sv"
    Then I should see:
    """
    var a = 1;
    var b = '2';

    var c = {
        d: 3
    };
    """
