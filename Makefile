hpack:
	nix-shell --pure shell.nix --run hpack

release: hpack
	nix-build release.nix

repl:
	nix-shell --pure shell.nix --run \
		"cabal new-repl --write-ghc-environment-files never lib:implicit-effects"

doc:
	nix-shell --pure shell.nix --run \
		"cabal new-haddock"

benchmark:
	nix-shell --pure shell.nix --run \
		"cabal new-run --write-ghc-environment-files never exe:implicit-effects-benchmark \
			-- --output 'benchmarks/$(shell date).html'"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

external-shell:
	nix-shell external.nix

test:
	nix-shell --pure shell.nix --run \
		"cabal new-test --write-ghc-environment-files never"

test-repl:
	nix-shell --pure shell.nix --run \
		"cabal new-repl --write-ghc-environment-files never test:implicit-effects-test"

sync: hpack
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

.PHONY: build run dev-server repl shell shell-pure external-shell sync