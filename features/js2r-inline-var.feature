Feature: Extract var

  Scenario: Inlining variable
    When I insert "var foo = bar(1,2,3); foo();"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m iv"
    Then I should see:
    """
    bar(1,2,3)();
    """

  Scenario: Inlining variable with no semicolon
    Given I insert:
    """
    const count = 10
    if (count > 5) {
        console.log('true')
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "count"
    And I press "C-c C-m iv"
    Then I should see:
    """
    if (10 > 5) {
        console.log('true')
    }
    """

  Scenario: Inlining variable with tab indentation
    Given I insert:
    """
    function hello() {
    	var foo = bar();
    	baz(foo);
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m iv"
    Then I should see:
    """
    function hello() {
    	baz(bar());
    }
    """

  Scenario: Inlining let with tab indentation
    Given I insert:
    """
    function hello() {
    	let foo = bar();
    	baz(foo);
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m iv"
    Then I should see:
    """
    function hello() {
    	baz(bar());
    }
    """

  Scenario: Inlining const with tab indentation
    Given I insert:
    """
    function hello() {
    	const foo = bar();
    	baz(foo);
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m iv"
    Then I should see:
    """
    function hello() {
    	baz(bar());
    }
    """

  Scenario: Inlining let, multiple statements
    Given I insert:
    """
    function hello() {
    	let foo = bar(), qux = 13;
    	baz(foo);
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m iv"
    Then I should see:
    """
    function hello() {
    	let qux = 13;
    	baz(bar());
    }
    """

  Scenario: Inlining const, multiple statements
    Given I insert:
    """
    function hello() {
    	const foo = bar(), qux = 13;
    	baz(foo);
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m iv"
    Then I should see:
    """
    function hello() {
    	const qux = 13;
    	baz(bar());
    }
    """

  Scenario: Inlining variable defined after the inlining
    Given I insert:
    """
    function xxx() {
        return asdfg;
    }
    var asdfg = 'x';
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "asdfg"
    And I press "C-c C-m iv"
    Then I should see:
    """
    function xxx() {
        return 'x';
    }
    """
