Feature: Rename variable

  Scenario: Replace simple ternary with if
    When I insert "console.log(a ? 1 : 2);"
    And I turn on js2-mode and js2-refactor-mode
    And I go to character "?"
    And I press "C-c C-m 3i"
    Then I should see:
    """
    if (a) {
        console.log(1);
    } else {
        console.log(2);
    }
    """

  Scenario: Point can be anywhere in ternary
    When I insert "var x = abc ? 1 : 2;"
    And I turn on js2-mode and js2-refactor-mode
    And I go to character "b"
    And I press "C-c C-m 3i"
    Then I should see:
    """
    if (abc) {
        var x = 1;
    } else {
        var x = 2;
    }
    """

  Scenario: Multiline expression
    When I insert:
    """
    $.get(this.url ? this.url : defaultUrl, function (result) {
        console.log(result);
    });
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to character "?"
    And I press "C-c C-m 3i"
    Then I should see:
    """
    if (this.url) {
        $.get(this.url, function (result) {
            console.log(result);
        });
    } else {
        $.get(defaultUrl, function (result) {
            console.log(result);
        });
    }
    """

  Scenario: Multiline ternary
    When I insert:
    """
    console.log(this.a ?
        this.a :
        this.b
    );
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to character "?"
    And I press "C-c C-m 3i"
    Then I should see:
    """
    if (this.a) {
        console.log(this.a);
    } else {
        console.log(this.b);
    }
    """
