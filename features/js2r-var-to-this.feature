Feature: Changing var to this

  Background:
    Given I insert:
    """
    function abc() {
       var def = 123;
       return def * def + 456;
    }
    """
    And I turn on js2-mode

  Scenario: At declaration
    When I go to the front of the word "def"
    And I press "C-c C-m vt"
    Then I should see:
    """
    function abc() {
       this.def = 123;
       return this.def * this.def + 456;
    }
    """

  Scenario: At use
    When I go to the end of the word "return"
    And I press "C-f"
    And I press "C-c C-m vt"
    Then I should see:
    """
    function abc() {
       this.def = 123;
       return this.def * this.def + 456;
    }
    """

  Scenario: At var statement
    When I go to the front of the word "var"
    And I press "C-c C-m vt"
    Then I should see:
    """
    function abc() {
       this.def = 123;
       return this.def * this.def + 456;
    }
    """
