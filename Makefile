
release:
	nix-build nix/release.nix

shell:
	nix-shell -A ghc88.shell

clean:
	cabal clean

build:
	cabal build --enable-tests --enable-benchmark --enable-documentation all

benchmark:
	cabal run \
		casimir-benchmark \
		-- --output '../benchmarks/$(shell date).html'

test:
	cabal run casimir-test

cachix:
	nix-store -qR --include-outputs `nix-instantiate -A ghc88.shell` | cachix push maybevoid
	nix-store -qR --include-outputs `nix-instantiate -A ghc86.shell` | cachix push maybevoid

.PHONY: release shell clean build benchmark test cachix
