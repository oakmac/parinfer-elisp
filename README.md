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

* `(parinferlib-indent-mode txt options)`
* `(parinferlib-paren-mode txt options)`

`txt` should be a string of the text you want to process. Optional `options`
should be a [Property List][1] (plist) with the following keys:

- `:cursor-line` - zero-based line number of the cursor
- `:cursor-x` - zero-based x-position of the cursor
- `:cursor-dx` - amount that the cursor moved horizontally if something was inserted or deleted (only used by Paren Mode)
- `:preview-cursor-scope` - `t` or `nil`, when set to `t` in Indent Mode, it shows
  the cursor's scope on an empty line by inserting close-parens after it (read more [here][2])

The return value is a plist with keys:

- `:success` - `t` or `nil`, indicating if the input was properly formatted enough to create a valid result
- `:text` - a string, is the full text output (this is just the original text if `:success` is `nil`)
- `:cursor-x` - the new x-position of the cursor (since Parinfer may shift it around)
- `:changed-lines` - a vector of plists representing only the lines Parinfer changed, plist keys are:
  - `:line-no` - zero-based line number
  - `:line` - full text of the line
- `:error` - a plist if `:success` is `nil` containing keys:
  - `:name` - the name of the error
  - `:message` - a message describing the error
  - `:line-no` - a zero-based line number where the error occurred
  - `:x` - a zero-based column where the error occurred
- `:tab-stops` - a vector of plists representing Tab stops. Only populated in Indent Mode when a cursor position is supplied. plist keys are:
  - `:x` - a zero-based x-position of the tab stop
  - `:line-no` - a zero-based line number of the open-paren responsible for the tab stop
  - `:ch` - the character of the open-paren responsible for the tab stop (e.g. `(`, `[`, `{`)

This library closely mirrors the [parinfer.js API][2]. Please see there for more
information.

Usage examples can be found in [test.el] and [perf.el].

## Development

To run tests:

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
[1]:http://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Lists.html
[2]:https://github.com/shaunlebron/parinfer/tree/master/lib#api
[test.el]:test.el
[perf.el]:perf.el
[ISC License]:LICENSE.md
