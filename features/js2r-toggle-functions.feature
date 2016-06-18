
  Scenario: Toggle function declaration and function expression
    When I insert "function f (a, b, c) { var t = a + b + c; return t; }"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "t"
    And I press "C-c C-m tf"
    Then I should see:
    """
    var f  = function (a, b, c) { var t = a + b + c; return t; };
    """

  Scenario: Toggle function expression and function declaration
    When I insert "var f = function (a, b, c) { var t = a + b + c; return t; };"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "t"
    And I press "C-c C-m tf"
    Then I should see:
    """
    function f(a, b, c) { var t = a + b + c; return t; }
    """

  Scenario: Toggle function expression and arrow function
    When I insert "var f  = function (a, b, c) { var t = a + b + c; return t; };"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "t"
    And I press "C-c C-m ta"
    Then I should see:
    """
    var f  = (a, b, c) => { var t = a + b + c; return t; };
    """


  Scenario: Toggle arrow function and function expression
    When I insert "var f = (a, b, c) => { var t = a + b + c; return t; };"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "t"
    And I press "C-c C-m ta"
    Then I should see:
    """
    var f = function (a, b, c) { var t = a + b + c; return t; };
    """

