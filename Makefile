
release:
	nix-build nix/release.nix

shell:
	nix-shell -A ghc88.shell

clean:
	cabal clean

build:
	cabal build \
		--enable-tests --enable-benchmark \
		--enable-documentation all

build-nix:
	cabal build --project-file cabal-nix.project \
		--enable-tests --enable-benchmark \
		--enable-documentation all

benchmark:
	cabal run \
		casimir-benchmark \
		-- --output '../benchmarks/$(shell date).html'

test:
	cabal run casimir-test

test-nix:
	cabal --project-file=cabal-nix.project run casimir-test

cachix:
	nix-store -qR --include-outputs `nix-instantiate nix/shell.nix` | cachix push maybevoid

.PHONY: release shell clean build benchmark test test-nix cachix
