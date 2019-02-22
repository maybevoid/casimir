hpack:
	nix-shell --pure shell.nix --run hpack

release: hpack
	nix-build release.nix

repl: hpack
	nix-shell --pure shell.nix --run \
		"cabal new-repl --write-ghc-environment-files never lib:implicit-effect"

shell:
	nix-shell shell.nix

shell-pure:
	nix-shell --pure shell.nix

external-shell:
	nix-shell external.nix

sync:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

.PHONY: build run dev-server repl shell shell-pure external-shell sync