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
    And I turn on js2-mode and js2-refactor-mode
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
    And I turn on js2-mode and js2-refactor-mode
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
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m sl"
    Then I should see:
    """
    function abc() {
        ghi();
    }
    jkl();
    """

  Scenario: Slurping and closing braces
    When I insert:
    """
    function abc() {
    }
    ghi();
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m sl"
    Then I should see:
    """
    function abc() {
        ghi();
    }
    """

  Scenario: Slurping and end of buffer (regression test for issue#104)
    When I insert:
    """
    function abc() {
    }
    ghi();"""
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m sl"
    Then I should see:
    """
    function abc() {
        ghi();
    }
    """

  Scenario: Slurping into nested function
    When I insert:
    """
    assert.exception(function () {
    });
    validations.date();
    jkl();
    """
    And I turn on js2-mode and js2-refactor-mode
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
    And I turn on js2-mode and js2-refactor-mode
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
    And I turn on js2-mode and js2-refactor-mode
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
  Scenario: Slurping with negative prefix does a single slurp
    When I insert:
    """
    if (abc) {
    }
    ghi();
    jkl();
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "abc"
    And I press "M-- 1 C-c C-m sl"
    Then I should see:
    """
    if (abc) {
        ghi();
    }
    jkl();
    """

  Scenario: Slurping several statements using number prefix
    When I insert:
    """
    function abc() {
        def();
    }
    jkl();
    mno();
    pqr();
    stu();
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "abc"
    And I press "M-3 C-c C-m sl"
    Then I should see:
    """
    function abc() {
        def();
        jkl();
        mno();
        pqr();
    }
    stu();
    """

  Scenario: Slurping multiline statement followed by single line statement
    When I insert:
    """
    if (abc) {
        def();
    } else {}
    ghi({
        jkl: 1,
        mno: 2
    });
    jkl();
    pqr();
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "else"
    And I press "M-2 C-c C-m sl"
    Then I should see:
    """
    if (abc) {
        def();
    } else {
        ghi({
            jkl: 1,
            mno: 2
        });
        jkl();
    }
    pqr();
    """
