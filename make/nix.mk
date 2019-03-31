hpack:
	hpack

sync: hpack
	cabal2nix . > default.nix

clean:
	cabal new-clean

repl: hpack
	cabal new-repl --write-ghc-environment-files never lib:implicit-effects

doc: hpack
	cabal new-haddock --write-ghc-environment-files never

test: hpack
	cabal new-test --write-ghc-environment-files never

test-repl: hpack
	cabal new-repl --write-ghc-environment-files never test:implicit-effects-test

benchmark: hpack
	cabal new-run --write-ghc-environment-files never \
		exe:implicit-effects-benchmark \
		-- --output 'benchmarks/$(shell date).html'

.PHONY: hpack sync repl doc test test-repl benchmark