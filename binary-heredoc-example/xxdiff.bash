#!/bin/bash
set -o nounset -o errexit -o pipefail

function hex {
  xxd -c1 "$1" | cut -d: -f2
}

diff -u <(hex "$1") <(hex "$2")

