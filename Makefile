
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

test: test-88 test-86

test-88:
	nix-shell --pure -A ghc88.shell --run \
		"cabal run casimir-test"

test-86:
	nix-shell --pure -A ghc86.shell --run \
		"cabal run casimir-test"

cachix:
	nix-store -qR --include-outputs `nix-instantiate -A ghc88.shell` | cachix push maybevoid
	nix-store -qR --include-outputs `nix-instantiate -A ghc86.shell` | cachix push maybevoid

.PHONY: release shell clean build benchmark test cachix
