Feature: Killing lines

  Scenario: Killing a comment
    When I insert:
    """
    //function foo() {
    //    bar();
    //}
    """
    And I turn on js2-mode
    And I go to the front of the word "function"
    And I press "C-c C-m k"
    Then I should see:
    """
    //
    //    bar();
    //}
    """

  Scenario: Killing a line with parse errors
    When I insert:
    """
    function foo(
        bar();
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "function"
    And I press "C-c C-m k"
    Then I should see:
    """
    
        bar();
    }
    """


  Scenario: Killing in a string
    When I insert:
    """
    function foo() {
        bar('hello world');
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "world"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        bar('hello ');
    }
    """
  Scenario: Killing in an array
    When I insert:
    """
    function foo() {
        return [1, 2, bar, baz]
    }
    """
    And I turn on js2-mode
    And I go to the end of the word "bar"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        return [1, 2, bar]
    }
    """

  Scenario: Killing in a multiline array
    When I insert:
    """
    function foo() {
        return [1, 
                2, 
                bar, 
                baz]
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "bar"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        return [1, 
                2, 
                
                baz]
    }
    """

  Scenario: Killing in an object
    When I insert:
    """
    function foo() {
        return {a: {foo: 4, bar: 5}, b: 1}
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "bar"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        return {a: {foo: 4, }, b: 1}
    }
    """

  Scenario: Killing in an if statement
    When I insert:
    """
    function foo() {
        if (foo) {return 2;}
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "return"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        if (foo) {}
    }
    """

  Scenario: Killing in an if statement with else branch
    When I insert:
    """
    function foo() {
        if (foo) {return 2;} else {return 1;}
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "return"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        if (foo) {} else {return 1;}
    }
    """

  Scenario: Killing in an else branch of an if statement
    When I insert:
    """
    function foo() {
        if (foo) {return 2;} else {bar();}
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "bar"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        if (foo) {return 2;} else {}
    }
    """

  Scenario: Killing in a function
    When I insert:
    """
    var foo = function() { return hello;}; bar;
    """
    And I turn on js2-mode
    And I go to the front of the word "hello"
    And I press "C-c C-m k"
    Then I should see:
    """
    var foo = function() { return }; bar;
    """

  Scenario: Killing in the parameters of a function
    When I insert:
    """
    function a(foo, bar) { return bar;}
    """
    And I turn on js2-mode
    And I go to the end of the word "foo"
    And I press "C-c C-m k"
    Then I should see:
    """
    function a(foo) { return bar;}
    """

  Scenario: Killing in front of a function
    When I insert:
    """
    function a(foo, bar) { return bar;}
    """
    And I turn on js2-mode
    And I go to the front of the word "function"
    And I press "C-c C-m k"
    Then I should see:
    """
    
    """

  Scenario: Killing in front of a variable declaration
    When I insert:
    """
    var foo = 3;
    """
    And I turn on js2-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m k"
    Then I should see:
    """
    var 
    """

  Scenario: Killing in with nested balanced nodes
    When I insert:
    """
    [
        'foo', 
        'bar', 
        'baz'
    ]
    """
    And I turn on js2-mode
    And I go to line "3"
    And I press "C-c C-m k"
    Then I should see:
    """
    [
        'foo', 

        'baz'
    ]
    """
