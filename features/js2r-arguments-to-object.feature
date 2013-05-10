Feature: Arguments to object

  Scenario: Values
    Given I insert "abc(123, 4 + 5, 'hello');"
    And I turn on js2-mode
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
    And I turn on js2-mode
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
    And I turn on js2-mode
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
    And I turn on js2-mode
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
