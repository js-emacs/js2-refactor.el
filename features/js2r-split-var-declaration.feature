Feature: Split var declaration

  Scenario: Split simple var
    When I insert "var a, b, c;"
    And I turn on js2-mode and js2-refactor-mode
    And I press "C-c C-m sv"
    Then I should see:
    """
    var a;
    var b;
    var c;
    """
    And I should not see "var a, b, c;"

  Scenario: Split simple let
    When I insert "let a, b, c;"
    And I turn on js2-mode and js2-refactor-mode
    And I press "C-c C-m sv"
    Then I should see:
    """
    let a;
    let b;
    let c;
    """
    And I should not see "let a, b, c;"

  Scenario: Split simple const
    When I insert "const a, b, c;"
    And I turn on js2-mode and js2-refactor-mode
    And I press "C-c C-m sv"
    Then I should see:
    """
    const a;
    const b;
    const c;
    """
    And I should not see "const a, b, c;"

  Scenario: Split vars with values
    When I insert "var a = 1, b = '2', c = { d: 3 };"
    And I turn on js2-mode and js2-refactor-mode
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
    And I turn on js2-mode and js2-refactor-mode
    And I press "C-c C-m sv"
    Then I should see:
    """
    var a = 1;
    var b = '2';

    var c = {
        d: 3
    };
    """
