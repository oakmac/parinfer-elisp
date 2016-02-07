# Parinfer in Emacs Lisp [![Build Status](https://travis-ci.org/oakmac/parinfer-elisp.svg?branch=master)](https://travis-ci.org/oakmac/parinfer-elisp)

A [Parinfer] implementation in [Emacs Lisp].

## About

Having a Parinfer implementation written in Emacs Lisp allows Parinfer to reach
Emacs users easily.

Please note that this repo is solely for the library that implements the main
Parinfer algorithm. It is not an Emacs extension.

This is basically a 1-to-1 copy of [parinfer.js].

The `.json` files in the [tests] folder are copied directly from the [main
Parinfer repo].

This is my first Emacs Lisp project. There is likely lots of room for
improvement in this implementation. PR's welcome :)

## Run Tests

```sh
emacs --script test.el
```

## Usage

TODO: write some examples using the Public API functions

## License

[ISC License]

[Parinfer]:https://shaunlebron.github.io/parinfer/
[Emacs Lisp]:https://www.gnu.org/software/emacs/manual/html_node/elisp/
[parinfer.js]:https://github.com/shaunlebron/parinfer/blob/master/lib/parinfer.js
[tests]:tests/
[main Parinfer repo]:https://github.com/shaunlebron/parinfer/tree/master/lib/test/cases
[ISC License]:LICENSE.md
