Feature: Extract function

  Scenario: Simple function declaration
    Given I insert:
    """
    function abc() {
        console.log("abc");
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I select "console.log"
    And I press "C-c C-m ef name RET"
    Then I should see:
    """
    function name() {
        console.log("abc");
    }

    function abc() {
        name();
    }
    """

  Scenario: Simple function assignment
    Given I insert:
    """
    var abc = function () {
        console.log("abc");
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I select "console.log"
    And I press "C-c C-m ef name RET"
    Then I should see:
    """
    function name() {
        console.log("abc");
    }

    var abc = function () {
        name();
    }
    """

  Scenario: Simple method
    Given I insert:
    """
    var obj = {
        abc: function () {
            console.log("abc");
        }
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I select "console.log"
    And I press "C-c C-m ef name RET"
    Then I should see:
    """
    function name() {
        console.log("abc");
    }

    var obj = {
        abc: function () {
            name();
        }
    }
    """

  Scenario: Return statement
    Given I insert:
    """
    function abc() {
        return "abc";
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I select "return"
    And I press "C-c C-m ef name RET"
    Then I should see:
    """
    function name() {
        return "abc";
    }

    function abc() {
        return name();
    }
    """

  Scenario: Parameters
    Given I insert:
    """
    function abc(num) {
        return num + 1;
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I select "return"
    And I press "C-c C-m ef name RET"
    Then I should see:
    """
    function name(num) {
        return num + 1;
    }

    function abc(num) {
        return name(num);
    }
    """

  Scenario: Local parameters
    Given I insert:
    """
    function abc(num) {
        var inc = 1;
        return num + inc;
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I select "return"
    And I press "C-c C-m ef name RET"
    Then I should see:
    """
    function name(num, inc) {
        return num + inc;
    }

    function abc(num) {
        var inc = 1;
        return name(num, inc);
    }
    """

  Scenario: Keep reference to var if in use
    Given I insert:
    """
    function abc(num) {
        var inc = 1;
        inc = inc + 7;
        return num + inc;
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "var"
    And I set the mark
    And I go to the end of the word "7"
    And I press "C-c C-m ef name RET"
    Then I should see:
    """
    function name() {
        var inc = 1;
        inc = inc + 7;
        return inc;
    }

    function abc(num) {
        var inc = name();
        return num + inc;
    }
    """

  Scenario: Included parameters
    Given I insert:
    """
    function abc(num) {
        var inc = 1;
        return num + inc;
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "var"
    And I set the mark
    And I go to the end of the word "return"
    And I press "C-c C-m ef name RET"
    Then I should see:
    """
    function name(num) {
        var inc = 1;
        return num + inc;
    }

    function abc(num) {
        return name(num);
    }
    """

  Scenario: Nested expressions
    Given I insert:
    """
    function abc(num) {
        var inc = 1;
        if (true) {
            return num + inc;
        }
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I select "return"
    And I press "C-c C-m ef name RET"
    Then I should see:
    """
    function name(num, inc) {
        return num + inc;
    }

    function abc(num) {
        var inc = 1;
        if (true) {
            return name(num, inc);
        }
    }
    """

  Scenario: Extract method
    Given I insert:
    """
    var obj = {
        abc: function () {
            console.log("abc");
        }
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I select "console.log"
    And I press "C-c C-m em name RET"
    Then I should see:
    """
    var obj = {
        name: function () {
            console.log("abc");
        },

        abc: function () {
            this.name();
        }
    }
    """

  Scenario: Extract method from class
    Given I insert:
    """
    class C {
        abc() {
            console.log("abc");
        }
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    When I select "console.log"
    And I press "C-c C-m em name RET"
    Then I should see:
    """
    class C {
        name() {
            console.log("abc");
        }

        abc() {
            this.name();
        }
    }
    """
