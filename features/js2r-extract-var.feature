Feature: Extract var

  Scenario: Extracting region
    When I insert "abc(1 + 2 + 3, 4 + 5);"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "1"
    And I set the mark
    And I go to the end of the word "2"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "three"
    Then I should see:
    """
    var three = 1 + 2;
    abc(three + 3, 4 + 5);
    """

  Scenario: Extracting function parameter
    When I insert "abc(1 + 2 + 3, 4 + 5);"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "2"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "six"
    Then I should see:
    """
    var six = 1 + 2 + 3;
    abc(six, 4 + 5);
    """

  Scenario: Extracting function call
    When I insert:
    """
    function f () {
        return abc(123).toString();
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "def"
    Then I should see:
    """
    function f () {
        var def = abc(123);
        return def.toString();
    }
    """

  Scenario: Extracting method call
    When I insert:
    """
    function f () {
        return abc.def(123).toString();
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "def"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "ghi"
    Then I should see:
    """
    function f () {
        var ghi = abc.def(123);
        return ghi.toString();
    }
    """

  Scenario: Extracting attribute accessor
    When I insert:
    """
    abc.def.ghi();
    """
    And I turn on js2-mode and js2-refactor-mode
    And I set js2r-prefer-let-over-var to nil
    And I go to the front of the word "def"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "jkl"
    Then I should see:
    """
    var jkl = abc.def;
    jkl.ghi();
    """

  Scenario: Issue 37, wrong place
    When I insert:
    """
    beforeEach(function () {
      spyOn(Foo, 'bar').andReturn('baz');
    });
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "baz"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "jkl"
    Then I should see:
    """
    beforeEach(function () {
        var jkl = 'baz';
        spyOn(Foo, 'bar').andReturn(jkl);
    });
    """

  Scenario: Issue 43, inside else if
    When I insert:
    """
    if(false) {
        console.log('false');
    } else if(true) {
        console.log('true');
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "true"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "jkl"
    Then I should see:
    """
    var jkl = true;
    if(false) {
        console.log('false');
    } else if(jkl) {
        console.log('true');
    }
    """

  Scenario: Issue 43, inside else if using a region
    When I insert:
    """
    if(false) {
        console.log('false');
    } else if(3 in foo) {
        console.log('true');
    }
    """
    And I turn on js2-mode and js2-refactor-mode
    And I set js2r-prefer-let-over-var to nil
    And I go to the front of the word "3"
    And I set the mark
    And I go to the end of the word "foo"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "jkl"
    Then I should see:
    """
    var jkl = 3 in foo;
    if(false) {
        console.log('false');
    } else if(jkl) {
        console.log('true');
    }
    """

  Scenario: Use let constructs
    When I insert "abc(1 + 2 + 3, 4 + 5);"
    And I turn on js2-mode and js2-refactor-mode
    And I set js2r-prefer-let-over-var to t
    And I go to the front of the word "1"
    And I set the mark
    And I go to the end of the word "2"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "three"
    And I set js2r-prefer-let-over-var to nil
    Then I should see:
    """
    let three = 1 + 2;
    abc(three + 3, 4 + 5);
    """

  Scenario: Use extract-let
    When I insert "abc(1 + 2 + 3, 4 + 5);"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "1"
    And I set the mark
    And I go to the end of the word "2"
    And I press "C-c C-m el"
    And I press "C-u DEL"
    And I type "three"
    Then I should see:
    """
    let three = 1 + 2;
    abc(three + 3, 4 + 5);
    """

  Scenario: Use extract-const
    When I insert "abc(1 + 2 + 3, 4 + 5);"
    And I turn on js2-mode and js2-refactor-mode
    And I go to the front of the word "1"
    And I set the mark
    And I go to the end of the word "2"
    And I press "C-c C-m ec"
    And I press "C-u DEL"
    And I type "three"
    Then I should see:
    """
    const three = 1 + 2;
    abc(three + 3, 4 + 5);
    """