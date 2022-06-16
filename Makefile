test:
	emacs --script test.el

test-perf:
	emacs --script perf.el

.PHONY: test test-perf
