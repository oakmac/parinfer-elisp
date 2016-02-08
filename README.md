# Parinfer in Emacs Lisp [![Build Status](https://travis-ci.org/oakmac/parinfer-elisp.svg?branch=master)](https://travis-ci.org/oakmac/parinfer-elisp)

A [Parinfer] implementation in [Emacs Lisp].

## About

Having a Parinfer implementation written in Emacs Lisp allows Parinfer to reach
Emacs users easily.

Please note that this project is solely for the library that implements the core
Parinfer algorithm; it is not an Emacs extension that can be used for editing.

This is basically a 1-to-1 port of [parinfer.js].

The `.json` files in the [tests] folder are copied directly from the [main
Parinfer repo].

This is my first Emacs Lisp project. There is likely lots of room for
improvement in this implementation. PR's welcome :)

## Usage

This library is namespaced under `parinferlib`.

> It is expected that extension authors will use the namespace `parinfer`

It exposes two public functions:

* `(parinferlib-indent-mode txt cursor-x cursor-line cursor-dx)`
* `(parinferlib-paren-mode txt cursor-x cursor-line cursor-dx)`

`txt` should be a string of the text you want to process, and the `cursor-`
arguments should be `nil` or an integer.

The return value is a [Property List] (plist) with keys:

* `:success` - `t` or `nil`, indicating if the input was properly formatted
  enough to create a valid result
* `:text` - a string, is the full text output (this is just the original text if
  `:success` is `nil`)
* `:error` - a plist if `:success` is `nil` containing keys: `:name` `:message`
  `:line-no` `x` with additional information about the error

This library closely mirrors the [parinfer.js API]. Please see there for more
information.

Please see the [test.el] file for usage examples.

## Run Tests

```sh
emacs --script test.el
```

To test performance:

```sh
emacs --script perf.el
```

## License

[ISC License]

[Parinfer]:https://shaunlebron.github.io/parinfer/
[Emacs Lisp]:https://www.gnu.org/software/emacs/manual/html_node/elisp/
[parinfer.js]:https://github.com/shaunlebron/parinfer/blob/master/lib/parinfer.js
[tests]:tests/
[main Parinfer repo]:https://github.com/shaunlebron/parinfer/tree/master/lib/test/cases
[parinfer.js API]:https://github.com/shaunlebron/parinfer/tree/master/lib#api
[Property List]:http://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html
[test.el]:test.el
[ISC License]:LICENSE.md
