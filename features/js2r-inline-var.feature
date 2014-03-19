Feature: Extract var

  Scenario: Inlining variable
    When I insert "var foo = bar(1,2,3); foo();"
    And I turn on js2-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m iv"
    Then I should see:
    """
    bar(1,2,3)();
    """

  Scenario: Inlining variable with tab indentation
    Given I insert:
    """
    function hello() {
    	var foo = bar();
    	baz(foo);
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m iv"
    Then I should see:
    """
    function hello() {
    	baz(bar());
    }
    """