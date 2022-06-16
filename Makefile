parinferlib.elc: parinferlib.el
	emacs --batch -f batch-byte-compile parinferlib.el

test: parinferlib.elc
	emacs --script test.el

test-perf: parinferlib.elc
	emacs --script perf.el

.PHONY: test test-perf
