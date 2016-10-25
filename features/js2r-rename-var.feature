Feature: Rename variable

  Scenario: Rename a single name
    Given delete-selection-mode is active
    When I insert "var abc = 123;"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m rv"
    And I type "def"
    Then I should see "var def = 123;"

  Scenario: Rename two instances of name
    Given delete-selection-mode is active
    When I insert "var abc = 123, def = abc;"
    And I turn on js2-mode and js2-refactor-mode
    And I go to character ";"
    And I press "C-b"
    And I press "C-c C-m rv"
    And I type "ghi"
    Then I should see "var ghi = 123, def = ghi;"

  Scenario: Rename not confused by comments
    Given delete-selection-mode is active
    When I insert:
    """
    function foo(){}
    var x = {
        foo/**/: 1
    };
    // comment ends in dot.
    foo();
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m rv"
    And I type "bar"
    Then I should see:
    """
    function bar(){}
    var x = {
        foo/**/: 1
    };
    // comment ends in dot.
    bar();
    """

  Scenario: Rename function params
    Given delete-selection-mode is active
    When I insert "function test(abc) { alert(abc); }"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the end of the word "alert"
    And I press "C-f"
    And I press "C-c C-m rv"
    And I type "ghi"
    Then I should see "function test(ghi) { alert(ghi); }"

  Scenario: Don't rename object literal keys
    Given delete-selection-mode is active
    When I insert "var abc = { abc: 123 };"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the end of the word "var"
    And I press "C-f"
    And I press "C-c C-m rv"
    And I type "ghi"
    Then I should see "var ghi = { abc: 123 };"

  Scenario: Don't rename object property access
    Given delete-selection-mode is active
    When I insert "var abc = this.abc;"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the end of the word "var"
    And I press "C-f"
    And I press "C-c C-m rv"
    And I type "ghi"
    Then I should see "var ghi = this.abc;"

  Scenario: Don't rename shadowed variables
    Given delete-selection-mode is active
    When I insert "var abc; abc=1; function shadow(abc) { abc=2; } function noShadow() { abc=3; }"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the end of the word "var"
    And I press "C-f"
    And I press "C-c C-m rv"
    And I type "ghi"
    Then I should see "var ghi; ghi=1; function shadow(abc) { abc=2; } function noShadow() { ghi=3; }"
