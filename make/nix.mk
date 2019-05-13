sync:
	cd nix && cabal2nix ../code > project.nix

clean:
	cd code && cabal new-clean

repl:
	cd code && cabal new-repl --write-ghc-environment-files never \
		lib:implicit-effects

doc:
	cd code && cabal new-haddock --write-ghc-environment-files never

test:
	cd code && cabal new-test --write-ghc-environment-files never

test-repl:
	cd code && cabal new-repl --write-ghc-environment-files never \
		implicit-effects-test

benchmark:
	cd code && cabal new-run --write-ghc-environment-files never \
		implicit-effects-benchmark \
		-- --output '../benchmarks/$(shell date).html'

.PHONY: hpack sync repl doc test test-repl benchmark