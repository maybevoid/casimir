hpack:
	cd code && hpack

sync: hpack
	cd nix && cabal2nix ../code > project.nix

clean:
	cd code && cabal new-clean

repl: hpack
	cd code && cabal new-repl --write-ghc-environment-files never lib:implicit-effects

doc: hpack
	cd code && cabal new-haddock --write-ghc-environment-files never

test: hpack
	cd code && cabal new-test --write-ghc-environment-files never

test-repl: hpack
	cd code && cabal new-repl --write-ghc-environment-files never test:implicit-effects-test

benchmark: hpack
	cd code && cabal new-run --write-ghc-environment-files never \
		exe:implicit-effects-benchmark \
		-- --output 'benchmarks/$(shell date).html'

.PHONY: hpack sync repl doc test test-repl benchmark