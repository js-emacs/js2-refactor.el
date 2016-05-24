Feature: Arguments to object

  Scenario: Values
    Given I insert "abc(123, 4 + 5, 'hello');"
    And I turn on js2-mode and js2-refactor-mode
    When I go to the end of the word "abc"
    And I press "C-c C-m ao"
    Then I should see:
    """
    abc({
        key: 123,
        key: 4 + 5,
        key: 'hello'
    });
    """

  Scenario: Placeholders
    Given I insert "abc(123, 4 + 5, 'hello');"
    And I turn on js2-mode and js2-refactor-mode
    When I go to the end of the word "abc"
    And I press "C-c C-m ao"
    And I type "def"
    And I press "TAB"
    And I type "ghi"
    And I press "TAB"
    And I type "jkl"
    And I press "TAB"
    Then I should see:
    """
    abc({
        def: 123,
        ghi: 4 + 5,
        jkl: 'hello'
    });
    """

  Scenario: Known names
    Given I insert "abc(def, ghi, jkl);"
    And I turn on js2-mode and js2-refactor-mode
    When I go to the end of the word "abc"
    And I press "C-c C-m ao"
    Then I should see:
    """
    abc({
        def: def,
        ghi: ghi,
        jkl: jkl
    });
    """

  Scenario: Convert params in function
    Given I insert:
    """
    function abc(def, ghi, jkl) {
        var hmm = def + ghi;
        return {
            hmm: hmm,
            jkl: jkl
        };
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the end of the word "abc"
    And I press "C-c C-m ao"
    Then I should see:
    """
    function abc(params) {
        var hmm = params.def + params.ghi;
        return {
            hmm: hmm,
            jkl: params.jkl
        };
    }
    """

  Scenario: Converting both, from function
    Given I insert:
    """
    function add(a, b) {
        return a + b;
    }
    add(1, 3);
    add("abc", {
        a: 2,
        b: 3
    });
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the end of the word "add"
    And I press "C-c C-m ao"
    Then I should see:
    """
    function add(params) {
        return params.a + params.b;
    }
    add({
        a: 1,
        b: 3
    });
    add({
        a: "abc",
        b: {
            a: 2,
            b: 3
        }
    });
    """

  Scenario: Converting both, from call node
    Given I insert:
    """
    function add(a, b) {
        return a + b;
    }
    add(1, 3);
    add("abc", {
        a: 2,
        b: 3
    });
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "abc"
    And I press "C-b C-b"
    And I press "C-c C-m ao"
    Then I should see:
    """
    function add(params) {
        return params.a + params.b;
    }
    add({
        a: 1,
        b: 3
    });
    add({
        a: "abc",
        b: {
            a: 2,
            b: 3
        }
    });
    """

  Scenario: Converting both, constructor
    Given I insert:
    """
    function Add(a, b) {
        this.a = a;
        this.b = b;
    }
    var a = new Add(1, 3);
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the end of the word "new Add"
    And I press "C-c C-m ao"
    Then I should see:
    """
    function Add(params) {
        this.a = params.a;
        this.b = params.b;
    }
    var a = new Add({
        a: 1,
        b: 3
    });
    """
