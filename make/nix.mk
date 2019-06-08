
clean:
	cabal v2-clean \
		implicit-effects

repl:
	cabal v2-repl \
		implicit-effects

doc:
	cabal v2-haddock \
		implicit-effects

test:
	cabal v2-test \
		implicit-effects-test

test-repl:
	cabal v2-repl \
		implicit-effects-test

benchmark:
	cabal v2-run \
		implicit-effects-benchmark \
		-- --output '../benchmarks/$(shell date).html'

.PHONY: sync repl doc test test-repl benchmark