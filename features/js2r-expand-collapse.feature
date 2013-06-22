Feature: Expand and collapse things

  Scenario: Expanding objects
    When I insert "var a = { b: 1, c: 'def' };"
    And I turn on js2-mode
    And I go to the front of the word "b"
    And I press "C-c C-m eo"
    Then I should see:
    """
    var a = {
        b: 1,
        c: 'def'
    };
    """

  Scenario: Expanding objects with comma
    When I insert "var a = { b: 1, c: 'def, ghi' };"
    And I turn on js2-mode
    And I go to the front of the word "b"
    And I press "C-c C-m eo"
    Then I should see:
    """
    var a = {
        b: 1,
        c: 'def, ghi'
    };
    """

  Scenario: Contracting objects
    When I insert:
    """
    var a = {
        b: 1,
        c: 'def'
    };
    """
    And I turn on js2-mode
    And I go to the front of the word "b"
    And I press "C-c C-m co"
    Then I should see "var a = { b: 1, c: 'def' };"

  Scenario: Contracting objects with comma
    When I insert:
    """
    var a = {
        b: 1,
        c: 'def, ghi'
    };
    """
    And I turn on js2-mode
    And I go to the front of the word "b"
    And I press "C-c C-m co"
    Then I should see "var a = { b: 1, c: 'def, ghi' };"

  Scenario: Expanding functions
    When I insert "function f (a, b, c) { var t = a + b + c; return t; }"
    And I turn on js2-mode
    And I go to the front of the word "var"
    And I press "C-c C-m eu"
    Then I should see:
    """
    function f (a, b, c) {
        var t = a + b + c;
        return t;
    }
    """

  Scenario: Expanding functions containing arrays
    When I insert "function f (a, b, c) { var t = a + b + c; var arr = [1, 2, 3, a, b]; return [t, arr]; }"
    And I turn on js2-mode
    And I go to the front of the word "var"
    And I press "C-c C-m eu"
    Then I should see:
    """
    function f (a, b, c) {
        var t = a + b + c;
        var arr = [1, 2, 3, a, b];
        return [t, arr];
    }
    """

  Scenario: Expanding functions containing object literals
    When I insert "function f (a, b, c) { var t = a + b + c; var o = {e1: a, e2: b + 1, e3: 'xyzzy'}; return o; }"
    And I turn on js2-mode
    And I go to the front of the word "var"
    And I press "C-c C-m eu"
    Then I should see:
    """
    function f (a, b, c) {
        var t = a + b + c;
        var o = {e1: a, e2: b + 1, e3: 'xyzzy'};
        return o;
    }
    """

  Scenario: Contracting functions
    When I insert:
    """
    function f (a, b, c) {
        var t = a + b + c;
        return t;
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "var"
    And I press "C-c C-m cu"
    Then I should see "function f (a, b, c) { var t = a + b + c; return t; }"

  Scenario: Contracting functions containing arrays
    When I insert:
    """
    function f (a, b, c) {
        var t = a + b + c;
        var arr = [1, 2, 3, a, b];
        return [t, arr];
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "var"
    And I press "C-c C-m cu"
    Then I should see "function f (a, b, c) { var t = a + b + c; var arr = [1, 2, 3, a, b]; return [t, arr]; }"

  Scenario: Contracting functions containing object literals
    When I insert:
    """
    function f (a, b, c) {
        var t = a + b + c;
        var o = {e1: a, e2: b + 1, e3: 'xyzzy'};
        return o;
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "var"
    And I press "C-c C-m cu"
    Then I should see "function f (a, b, c) { var t = a + b + c; var o = {e1: a, e2: b + 1, e3: 'xyzzy'}; return o; }"

  Scenario: Expanding arrays
    When I insert "var a = [ b, 1, c, 3.1415927 ];"
    And I turn on js2-mode
    And I go to the front of the word "b"
    And I press "C-c C-m ea"
    Then I should see:
    """
    var a = [
        b,
        1,
        c,
        3.1415927
    ];
    """

  Scenario: Contracting arrays
    When I insert:
    """
    var a = [
        b,
        1,
        c,
        3.1415927
    ];
    """
    And I turn on js2-mode
    And I go to the front of the word "b"
    And I press "C-c C-m ca"
    Then I should see "var a = [ b, 1, c, 3.1415927 ];"
