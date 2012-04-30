#!/bin/bash
set -o errexit -o pipefail -o nounset


function compile {
  ghc -outputdir ./tmp --make -O2 ./arx.hs -v 2>&1
}

function filter {
  compile | fgrep collect2 |
  sed 's|-lgmp || ; s|-lffi ||
       s|$| -static -lgmp -lffi|
       s| -o arx | -o ./tmp/arx.ubuntu |'
}

compile | filter

