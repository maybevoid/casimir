
hpack:
	nix-shell --pure shell.nix --run hpack

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
		"make -f make/nix.mk clean"

hoogle:
	nix-shell --pure nix/external.nix --run \
		"hoogle server --local --host 0.0.0.0 -p 8333"

repl:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/nix.mk repl"

doc:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/nix.mk doc"

benchmark:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/nix.mk benchmark"

test:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/nix.mk test"

test-repl:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/nix.mk test-repl"

sync:
	nix-shell --pure nix/shell.nix --run \
		"make -f make/nix.mk sync"

.PHONY: hpack clean-env release release-doc shell external-shell hoogle repl benchmark test test-repl sync