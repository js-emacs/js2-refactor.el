Feature: Introduce parameter

  Scenario: Introduce parameter
    Given I insert:
    """
    function abc() {
       console.log(123);
    }
    abc();
    """
    And I turn on js2-mode and js2-refactor-mode
    When I go to the front of the word "123"
    And I start an action chain
    And I press "C-c C-m ip"
    And I type "num"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    function abc(num) {
       console.log(num);
    }
    abc(123);
    """
