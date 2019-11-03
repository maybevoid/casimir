
clean-env:
	rm -f .ghc.environment*

release: clean-env
	nix-build nix/release.nix

release-doc: clean-env
	nix-build -A doc nix/release.nix

shell:
	nix-shell nix/shell.nix

external-shell:
	nix-shell nix/external.nix

clean:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/cabal.mk clean"

hoogle:
	nix-shell --pure nix/external.nix --run \
		"cd code && hoogle server --local --host 0.0.0.0 -p 8333"

repl:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/cabal.mk repl"

demo:
	nix-shell --pure nix/shell.nix --run \
		"cabal v2-repl implicit-effects-demo"

doc:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/cabal.mk doc"

benchmark:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/cabal.mk benchmark"

test:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/cabal.mk test"

test-repl:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/cabal.mk test-repl"

.PHONY: clean-env release release-doc shell external-shell \
	hoogle repl demo doc benchmark test test-repl