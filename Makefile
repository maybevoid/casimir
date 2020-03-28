
release:
	nix-build nix/release.nix

release-doc:
	nix-build -A doc nix/release.nix

shell:
	nix-shell -A ghc88 nix/shell.nix

external-shell:
	nix-shell nix/external.nix

clean:
	nix-shell --pure -A default nix/shell.nix --run \
		"make -f make/cabal.mk clean"

hoogle:
	nix-shell --pure -A default nix/external.nix --run \
		"cd code && hoogle server --local --host 0.0.0.0 -p 8333"

repl:
	nix-shell --pure -A default nix/shell.nix --run \
		"make -f make/cabal.mk repl"

build:
	nix-shell --pure -A default nix/shell.nix --run \
		"make -f make/cabal.mk build"

doc:
	nix-shell --pure -A default nix/shell.nix --run \
		"make -f make/cabal.mk doc"

benchmark:
	nix-shell -- -A default nix/shell.nix --run \
		"make -f make/cabal.mk benchmark"

test:
	nix-shell --pure -A ghc88 nix/shell.nix --run \
		"make -f make/cabal.mk test"

	nix-shell --pure -A ghc86 nix/shell.nix --run \
		"make -f make/cabal.mk test"

.PHONY: release release-doc shell external-shell \
	hoogle repl demo doc benchmark test test-repl
