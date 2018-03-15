Feature: Log this

  Scenario: Console.log var
    Given I insert "var bah = { b: 1, c: 'def' };"
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "bah"
    And I press "C-c C-m lt"
    Then I should see:
    """
    var bah = { b: 1, c: 'def' };
    console.log("bah = ", bah);
    """

  Scenario: Console.log param
    Given I insert:
    """
    function abc(def) {
        return def + 1;
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "def"
    And I press "C-c C-m lt"
    Then I should see:
    """
    function abc(def) {
        console.log("def = ", def);
        return def + 1;
    }
    """

  Scenario: Console.log region
    Given I insert:
    """
    var def = abc(123) + ghi();
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "abc"
    And I set the mark
    And I press "C-8 C-f"
    And I press "C-c C-m lt"
    Then I should see:
    """
    var def = abc(123) + ghi();
    console.log("abc(123) = ", abc(123));
    """

  Scenario: Console.log property get
    Given I insert:
    """
    def.ghi.jkl + 1;
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "ghi"
    And I press "C-c C-m lt"
    Then I should see:
    """
    def.ghi.jkl + 1;
    console.log("def.ghi = ", def.ghi);
    """

  Scenario: Console.log before return statement
    Given I insert:
    """
    function abc() {
        return def + 1;
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "def"
    And I press "C-c C-m lt"
    Then I should see:
    """
    function abc() {
        console.log("def = ", def);
        return def + 1;
    }
    """

  Scenario: Console.log in single-line function
    Given I insert:
    """
    function a() { callFunc(xxx); return abc; }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "xxx"
    And I press "C-c C-m lt"
    Then I should see:
    """
    function a() { callFunc(xxx);
                   console.log("xxx = ", xxx);return abc; }
    """

  Scenario: Console.log before return in single-line function
    Given I insert:
    """
    function a() { callFunc(arg); return abc; }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "abc"
    And I press "C-c C-m lt"
    Then I should see:
    """
    function a() { callFunc(arg);
                   console.log("abc = ", abc);return abc; }
    """

  Scenario: Console.log before point
    Given I insert:
    """
    function abc() {
        callFunc(arg);
        return def + 1;
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I set logging to be before the use-site
    When I go to the front of the word "arg"
    And I press "C-c C-m lt"
    Then I should see:
    """
    function abc() {
        console.log("arg = ", arg);
        callFunc(arg);
        return def + 1;
    }
    """
