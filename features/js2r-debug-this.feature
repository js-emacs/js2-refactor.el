Feature: Debug this

  Scenario: debug var
    Given I insert "var bah = { b: 1, c: 'def' };"
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "bah"
    And I press "C-c C-m dt"
    Then I should see:
    """
    var bah = { b: 1, c: 'def' };
    debug("bah = %s", bah);
    """

  Scenario: debug param
    Given I insert:
    """
    function abc(def) {
        return def + 1;
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "def"
    And I press "C-c C-m dt"
    Then I should see:
    """
    function abc(def) {
        debug("def = %s", def);
        return def + 1;
    }
    """

  Scenario: debug region
    Given I insert:
    """
    var def = abc(123) + ghi();
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "abc"
    And I set the mark
    And I press "C-8 C-f"
    And I press "C-c C-m dt"
    Then I should see:
    """
    var def = abc(123) + ghi();
    debug("abc(123) = %s", abc(123));
    """

  Scenario: debug property get
    Given I insert:
    """
    def.ghi.jkl + 1;
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "ghi"
    And I press "C-c C-m dt"
    Then I should see:
    """
    def.ghi.jkl + 1;
    debug("def.ghi = %s", def.ghi);
    """

  Scenario: debug before return statement
    Given I insert:
    """
    function abc() {
        return def + 1;
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "def"
    And I press "C-c C-m dt"
    Then I should see:
    """
    function abc() {
        debug("def = %s", def);
        return def + 1;
    }
    """
