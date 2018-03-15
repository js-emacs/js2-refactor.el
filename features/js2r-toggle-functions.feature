
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

  Scenario: Toggle arrow function without braces
    When I insert "const plus2 = (a) => a + 2;"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "a"
    And I press "C-c C-m ta"
    Then I should see:
    """
    const plus2 = function (a) { return a + 2; };
    """

  Scenario: Toggle arrow function without parens
    When I insert "const plus2 = a => a + 2;"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "a"
    And I press "C-c C-m ta"
    Then I should see:
    """
    const plus2 = function (a) { return a + 2; };
    """

  Scenario: Toggle arrow function returning constants
    When I insert "const two = () => 2;"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "2"
    And I press "C-c C-m ta"
    Then I should see:
    """
    const two = function () { return 2; };
    """

  Scenario: Toggle arrow function as argument
    When I insert "emitter.on('event', evt => console.log(evt));"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "evt"
    And I press "C-c C-m ta"
    Then I should see:
    """
    emitter.on('event', function (evt) { return console.log(evt); });
    """

  Scenario: Toggle function expression as argument
    When I insert "emitter.on('event', function(evt) { return console.log(evt); });"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "evt"
    And I press "C-c C-m ta"
    Then I should see:
    """
    emitter.on('event', evt => { return console.log(evt); });
    """

  Scenario: Toggling async function
    When I insert "p.then(async function(ret) { return process(ret, arg); });"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "process"
    And I press "C-c C-m ts"
    Then I should see:
    """
    p.then(function(ret) { return process(ret, arg); });
    """
  Scenario: Toggling non-async function
    When I insert "p.then(function(ret) { return process(ret, arg); });"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "process"
    And I press "C-c C-m ts"
    Then I should see:
    """
    p.then(async function(ret) { return process(ret, arg); });
    """

  Scenario: Toggling async arrow function
    When I insert "p.then(async (ret) => process(ret, arg));"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "process"
    And I press "C-c C-m ts"
    Then I should see:
    """
    p.then((ret) => process(ret, arg));
    """

  Scenario: Toggling non-async arrow function
    When I insert "p.then(ret => process(ret, arg));"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "process"
    And I press "C-c C-m ts"
    Then I should see:
    """
    p.then(async ret => process(ret, arg));
    """
