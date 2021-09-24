.PHONY: test clean

test : .test.flag

clean:
	rm .test.flag

# runs unit tests
.test.flag : woozie.el woozietest.el
	emacs --script woozietest.el && touch .test.flag
