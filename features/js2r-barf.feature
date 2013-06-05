Feature: JS Barf

  Scenario: Barfing statement out of function
    When I insert:
    """
    function abc() {
        def();
        ghi();
    }
    jkl();
    """

    And I turn on js2-mode
    And I go to the front of the word "def"
    And I press "C-c C-m ba"
    Then I should see:
    """
    function abc() {
        def();
    }
    ghi();
    jkl();
    """
  Scenario: Barfing multiline statement
    When I insert:
    """
    function abc() {
        def();
        ghi({
            jkl: 1,
            mno: 2
        });
    }
    jkl();
    """
    And I turn on js2-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m ba"
    Then I should see:
    """
    function abc() {
        def();
    }
    ghi({
        jkl: 1,
        mno: 2
    });
    jkl();
    """

  Scenario: Barfing out of nested function
    When I insert:
    """
    assert.exception(function () {
        validations.date();
    });
    jkl();
    """
    And I turn on js2-mode
    And I go to the end of the word "function"
    And I press "C-c C-m ba"
    Then I should see:
    """
    assert.exception(function () {
    });
    validations.date();
    jkl();
    """

  Scenario: Barfing out of if-statements
    When I insert:
    """
    if (abc) {
        ghi();
    }
    jkl();
    """
    And I turn on js2-mode
    And I go to the front of the word "ghi"
    And I press "C-c C-m ba"
    Then I should see:
    """
    if (abc) {
    }
    ghi();
    jkl();
    """

  Scenario: Barfing out of else-statements
    When I insert:
    """
    if (abc) {
        bah();
    } else {
        ghi();
    }
    jkl();
    """
    And I turn on js2-mode
    And I go to the front of the word "ghi"
    And I press "C-c C-m ba"
    Then I should see:
    """
    if (abc) {
        bah();
    } else {
    }
    ghi();
    jkl();
    """
