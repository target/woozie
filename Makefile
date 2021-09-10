.PHONY: test clean

test : .test.flag

clean:
	rm .test.flag

# runs unit tests
.test.flag : ooziefuncs.el ooziefuncstest.el
	emacs --script ooziefuncstest.el && touch .test.flag
