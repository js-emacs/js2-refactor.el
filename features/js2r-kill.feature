Feature: Killing lines

  Scenario: Killing a comment
    Given I clear the buffer
    When I insert:
    """
    //function foo() {
    //    bar();
    //}
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "function"
    And I press "C-c C-m k"
    Then I should see:
    """
    //
    //    bar();
    //}
    """

  Scenario: Killing a line with parse errors
    Given I clear the buffer
    When I insert:
    """
    function foo(
        bar();
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "function"
    And I press "C-c C-m k"
    Then I should see:
    """
    
        bar();
    }
    """


  Scenario: Killing in a string
    Given I clear the buffer
    When I insert:
    """
    function foo() {
        bar('hello world');
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "world"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        bar('hello ');
    }
    """
  Scenario: Killing in an array
    Given I clear the buffer
    When I insert:
    """
    function foo() {
        return [1, 2, bar, baz]
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the end of the word "bar"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        return [1, 2, bar]
    }
    """

  Scenario: Killing in a multiline array
    Given I clear the buffer
    When I insert:
    """
    function foo() {
        return [1, 
                2, 
                bar, 
                baz]
    }
    """
    And I turn on js2-mode and js2-refactor-mode
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
    Given I clear the buffer
    When I insert:
    """
    function foo() {
        return {a: {foo: 4, bar: 5}, b: 1}
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "bar"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        return {a: {foo: 4, }, b: 1}
    }
    """

  Scenario: Killing in an if statement
    Given I clear the buffer
    When I insert:
    """
    function foo() {
        if (foo) {return 2;}
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "return"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        if (foo) {}
    }
    """

  Scenario: Killing in an if statement with else branch
    Given I clear the buffer
    When I insert:
    """
    function foo() {
        if (foo) {return 2;} else {return 1;}
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "return"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        if (foo) {} else {return 1;}
    }
    """

  Scenario: Killing in an else branch of an if statement
    Given I clear the buffer
    When I insert:
    """
    function foo() {
        if (foo) {return 2;} else {bar();}
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "bar"
    And I press "C-c C-m k"
    Then I should see:
    """
    function foo() {
        if (foo) {return 2;} else {}
    }
    """

  Scenario: Killing in a function
    Given I clear the buffer
    When I insert:
    """
    var foo = function() { return hello;}; bar;
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "hello"
    And I press "C-c C-m k"
    Then I should see:
    """
    var foo = function() { return }; bar;
    """

  Scenario: Killing in the parameters of a function
    Given I clear the buffer
    When I insert:
    """
    function a(foo, bar) { return bar;}
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the end of the word "foo"
    And I press "C-c C-m k"
    Then I should see:
    """
    function a(foo) { return bar;}
    """

  Scenario: Killing in front of a function
    Given I clear the buffer
    When I insert:
    """
    function a(foo, bar) { return bar;}
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "function"
    And I press "C-c C-m k"
    Then I should see:
    """
    
    """

  Scenario: Killing in front of a variable declaration
    Given I clear the buffer
    When I insert:
    """
    var foo = 3;
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "foo"
    And I press "C-c C-m k"
    Then I should see:
    """
    var 
    """

  Scenario: Killing with nested balanced nodes
    Given I clear the buffer
    When I insert:
    """
    [
        'foo', 
        'bar', 
        'baz'
    ]
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to line "3"
    And I press "C-c C-m k"
    Then I should see:
    """
    [
        'foo', 

        'baz'
    ]
    """

  Scenario: Killing a node when in front of it
    Given I clear the buffer
    When I insert:
    """
    foo(['foo', 'bar'], 2, 3);
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor before "'foo"
    And I press "C-c C-m k"
    Then I should see:
    """
    foo([], 2, 3);
    """

  Scenario Killing multiline functions
    Given I clear the buffer
    When I insert:
    """
    baz(function(){
        bar();
    });
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor before "function"
    And I press "C-c C-m k"
    Then I should see:
    """
    baz();
    """

  Scenario Killing multiline statements
    Given I clear the buffer
    When I insert:
    """
    if(foo > bar) {
        baz();
    }
    blah();
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor before "if"
    And I press "C-c C-m k"
    Then I should see:
    """
    blah();
    """
    And I should not see:
    """
    baz();
    """


  Scenario Killing kills trailing semi-colons
    Given I clear the buffer
    When I insert:
    """
    foo();hello(
    );
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor before "hello"
    And I press "C-c C-m k"
    Then I should see:
    """
    foo();
    """
    And I should not see:
    """
    foo();;
    """

  Scenario Killing a node right after a comment
    Given I clear the buffer
    When I insert:
    """
    // test
    var hello = function(test) {
    };
    hello();
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor before "var"
    And I press "C-c C-m k"
    Then I should see:
    """
    // test

    hello();
    """

  Scenario Yanking a Killed node should yank the entire node
    Given I clear the buffer
    When I insert:
    """
    function hello(test) {
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor before "function"
    And I press "C-c C-m k"
    And I press "C-y"
    Then I should see:
    """
    function hello(test) {
    }
    """

  Scenario Yanking a Killed node with semi-colom should yank the semi-colon as well
    Given I clear the buffer
    When I insert:
    """
    var hello = function(test) {
    };
    """
    And I turn on js2-mode and js2-refactor-mode
    And I place the cursor before "var"
    And I press "C-c C-m k"
    And I press "C-y"
    Then I should see:
    """
    var hello = function(test) {
    };
    """
