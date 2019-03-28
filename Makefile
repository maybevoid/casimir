
hpack:
	nix-shell --pure shell.nix --run hpack

release:
	rm -f .ghc.environment*
	nix-build release.nix

repl:
	nix-shell --pure shell.nix --run \
		"make -f nix.mk repl"

doc:
	nix-shell --pure shell.nix --run \
		"make -f nix.mk doc"

benchmark:
	nix-shell --pure shell.nix --run \
		"make -f nix.mk benchmark"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

external-shell:
	nix-shell external.nix

test:
	nix-shell --pure shell.nix --run \
		"make -f nix.mk test"

test-repl:
	nix-shell --pure shell.nix --run \
		"make -f nix.mk test-repl"

sync:
	nix-shell --pure shell.nix --run \
		"make -f nix.mk sync"

.PHONY: build run dev-server repl shell shell-pure external-shell sync