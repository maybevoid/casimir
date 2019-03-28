
test:
	stack test

hpack:
	stack hpack

test-all:
	stack --resolver lts-13.14 test
	stack --resolver lts-13.11 test

.PHONY: test hpack test-all