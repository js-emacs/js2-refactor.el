# js2-refactor.el [![Build Status](https://travis-ci.org/magnars/js2-refactor.el.svg)](https://travis-ci.org/magnars/js2-refactor.el)

A JavaScript refactoring library for emacs.

This is a collection of small refactoring functions to further the idea of a
JavaScript IDE in Emacs that started with js2-mode.

## Change in 0.8.0

Add `expand-node-at-point` and `contract-node-at-point` function to Expand / Contract bracketed list according to node type at point (array, object, function, call args).

Removed previous `ea` `ca` `eo` `co` `eu` `cu` `ec` `cc` key bindings.

## Breaking change in 0.7.0

js2-refactor.el is now a minor mode that has to be enabled, with
something like the following:

    (add-hook 'js2-mode-hook #'js2-refactor-mode)

## Breaking change in 0.6.0

You now choose your own keybinding scheme. If you just want what you had
before this change, add this to your init:

    (js2r-add-keybindings-with-prefix "C-c C-m")

See **Setup keybindings** below for more.

## Installation

I highly recommend installing js2-refactor through elpa.

It's available on [melpa](http://melpa.milkbox.net/):

    M-x package-install js2-refactor

You can also install the dependencies on your own, and just dump
js2-refactor in your path somewhere:

 * js2-mode https://github.com/mooz/js2-mode/
 * dash https://github.com/magnars/dash.el
 * s https://github.com/magnars/s.el
 * multiple-cursors https://github.com/magnars/multiple-cursors.el
 * yasnippet https://github.com/capitaomorte/yasnippet

I also recommend that you get
[expand-region](https://github.com/magnars/expand-region.el) to more easily mark
vars, method calls and functions for refactorings.

Then add this to your emacs settings:

    (require 'js2-refactor)
    (add-hook 'js2-mode-hook #'js2-refactor-mode)

js2-refactor does not work in a buffer that has Javascript parse errors. To tell
js2-mode to treat hashbangs as comments, which prevents them from causing parse
errors, add this:

    (setq js2-skip-preprocessor-directives t)

## Setup keybindings

All functions in js2-refactor have a two-letter mnemonic shortcut. For
instance, extract-function is `ef`. You get to choose how those are bound.
Here's how:

    (js2r-add-keybindings-with-prefix "C-c C-m")
    ;; eg. extract function with `C-c C-m ef`.

If you would rather have a modifier key, instead of a prefix, do:

    (js2r-add-keybindings-with-modifier "C-s-")
    ;; eg. extract function with `C-s-e C-s-f`.

If neither of these appeal to your sense of keyboard layout aesthetics, feel free
to pick and choose your own keybindings with a smattering of:

    (define-key js2-refactor-mode-map (kbd "C-c C-e C-f") 'js2r-extract-function)

Additionally, for an interactive list of available keybindings the
following
[hydra](https://gist.github.com/anachronic/7af88c62db136727cd1fed17ee0a662f)
can be used as a starting point.

## Refactorings

 * `ee` is `expand-node-at-point`: Expand bracketed list according to node type at point (array, object, function, call args).
 * `cc` is `contract-node-at-point`: Contract bracketed list according to node type at point (array, object, function, call args).
 * `ef` is `extract-function`: Extracts the marked expressions out into a new named function.
 * `em` is `extract-method`: Extracts the marked expressions out into a new named method in an object literal.
 * `tf` is `toggle-function-expression-and-declaration`: Toggle between function name() {} and var name = function ();
 * `ta` is `toggle-arrow-function-and-expression`: Toggle between function expression to arrow function.
 * `ts` is `toggle-function-async`: Toggle between an async and a regular function.
 * `ip` is `introduce-parameter`: Changes the marked expression to a parameter in a local function.
 * `lp` is `localize-parameter`: Changes a parameter to a local var in a local function.
 * `wi` is `wrap-buffer-in-iife`: Wraps the entire buffer in an immediately invoked function expression
 * `ig` is `inject-global-in-iife`: Creates a shortcut for a marked global by injecting it in the wrapping immediately invoked function expression
 * `ag` is `add-to-globals-annotation`: Creates a `/*global */` annotation if it is missing, and adds the var at point to it.
 * `ev` is `extract-var`: Takes a marked expression and replaces it with a var.
 * `el` is `extract-let`: Similar to `extract-var` but uses a `let`-statement.
 * `ec` is `extract-const`: Similar to `extract-var` but uses a `const`-statement.
 * `iv` is `inline-var`: Replaces all instances of a variable with its initial value.
 * `rv` is `rename-var`: Renames the variable on point and all occurrences in its lexical scope.
 * `vt` is `var-to-this`: Changes local `var a` to be `this.a` instead.
 * `ao` is `arguments-to-object`: Replaces arguments to a function call with an object literal of named arguments.
 * `3i` is `ternary-to-if`: Converts ternary operator to if-statement.
 * `sv` is `split-var-declaration`: Splits a `var` with multiple vars declared, into several `var` statements.
 * `ss` is `split-string`: Splits a `string`.
 * `st` is `string-to-template`: Converts a `string` into a template string.
 * `uw` is `unwrap`: Replaces the parent statement with the selected region.
 * `lt` is `log-this`: Adds a console.log() statement for what is at point (or region). With a prefix argument, use JSON pretty-printing.
 * `dt` is `debug-this`: Adds a debug() statement for what is at point (or region).
 * `sl` is `forward-slurp`: Moves the next statement into current function, if-statement, for-loop or while-loop.
 * `ba` is `forward-barf`: Moves the last child out of current function, if-statement, for-loop or while-loop.
 * `k` is `kill`: Kills to the end of the line, but does not cross semantic boundaries.

There are also some minor conveniences bundled:

 * `C-S-down` and `C-S-up` moves the current line up or down. If the line is an
   element in an object or array literal, it makes sure that the commas are
   still correctly placed.

## Todo

A list of some wanted improvements for the current refactorings.

 * ~~expand- and contract-array: should work recursively with nested
   object literals and nested arrays.~~
   Now the `expand-node-at-point` and `contract-node-at-point` should work,
   by moving point into right place.
 * expand- and contract-function: should deal better with nested
   object literals, array declarations, and statements terminated only
   by EOLs (without semicolons).
 * wrap-buffer-in-iife: should skip comments and namespace initializations at buffer start.
 * extract-variable: could end with a query-replace of the expression in its scope.

## Contributors

* [Matt Briggs](https://github.com/mbriggs) contributed `js2r-add-to-globals-annotation`
* [Alex Chamberlain](https://github.com/apchamberlain) contributed contracting and expanding arrays and functions.
* [Nicolas Petton](https://github.com/NicolasPetton) contributed lots of stuff and is now a co-maintainer of the project.
* [Brian J Brennan](https://github.com/brianloveswords) added support for `const` and `let` to inline-var.
* [James Yang](https://github.com/futurist) added `expand-node-at-point` and `contract-node-at-point` functions.
* [Arne Martin Aurlien](https://github.com/arnemart) added more support for `let` and `const`.

Thanks!

## Contribute

This project is still in its infancy, and everything isn't quite sorted out
yet. If you're eager to contribute, please add an issue here on github and we
can discuss your changes a little before diving into the elisp. :-)

To fetch the test dependencies, install
[cask](https://github.com/rejeep/cask.el) if you haven't already,
then:

    $ cd /path/to/js2-refactor.el
    $ cask

Run the tests with:

    $ ./run-tests.sh

## License

Copyright (C) 2012-2014 Magnar Sveen

Copyright (C) 2015-2016 Magnar Sveen and Nicolas Petton

Author: Magnar Sveen <magnars@gmail.com>, Nicolas Petton <nicolas@petton.fr>

Keywords: javascript refactorings

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
