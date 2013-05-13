Feature: JS Slurp

  Scenario: Slurping statement into function
    When I insert:
    """
    function abc() {
        def();
    }
    ghi();
    jkl();
    """
    And I turn on js2-mode
    And I go to the front of the word "def"
    And I press "C-c C-m sl"
    Then I should see:
    """
    function abc() {
        def();
        ghi();
    }
    jkl();
    """

  Scenario: Slurping multiline statement
    When I insert:
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
    And I turn on js2-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m sl"
    Then I should see:
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

  Scenario: Slurping and opening braces
    When I insert:
    """
    function abc() {}
    ghi();
    jkl();
    """
    And I turn on js2-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m sl"
    Then I should see:
    """
    function abc() {
        ghi();
    }
    jkl();
    """

  Scenario: Slurping into nested function
    When I insert:
    """
    assert.exception(function () {
    });
    validations.date();
    jkl();
    """
    And I turn on js2-mode
    And I go to the end of the word "function"
    And I press "C-c C-m sl"
    Then I should see:
    """
    assert.exception(function () {
        validations.date();
    });
    jkl();
    """

  Scenario: Slurping into if-statements
    When I insert:
    """
    if (abc) {
    }
    ghi();
    jkl();
    """
    And I turn on js2-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m sl"
    Then I should see:
    """
    if (abc) {
        ghi();
    }
    jkl();
    """

  Scenario: Slurping into else-statements
    When I insert:
    """
    if (abc) {
        bah();
    } else {
    }
    ghi();
    jkl();
    """
    And I turn on js2-mode
    And I go to the front of the word "else"
    And I press "C-c C-m sl"
    Then I should see:
    """
    if (abc) {
        bah();
    } else {
        ghi();
    }
    jkl();
    """
