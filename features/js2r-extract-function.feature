Feature: Extract function

  Scenario: No parameters
    When I insert:
    """
    function abc() {
        console.log('hei');
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "console"
    And I set the mark
    And I go to character ";"
    And I press "C-c RET ef"
    And I type "hello"
    Then I should see:
    """
    function hello() {
        console.log('hei');
    }

    function abc() {
        hello();
    }
    """
