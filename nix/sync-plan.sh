#!/bin/bash

set -eux

result=$(nix-build nix/plan.nix)

rm -rf nix/plans

cp -r $result nix/plans

find nix/plans/ -type d -exec chmod 755 {} \;
