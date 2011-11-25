#!/bin/bash
set -o nounset -o errexit -o pipefail
function usage {
cat <<USAGE
 USAGE: signed-arch.bash <platform tag>?

  Automatically determines GHC target architecture and platform tag. You may
  specify this if, for example, you want to indicate that the binary was only
  tested on Ubuntu or Leopard.

USAGE
}

declare -a sedRE
if sed --version &>/dev/null
then
  sedRE=(sed -r)
else
  sedRE=(sed -E)
fi

d=./tmp/signed

function sedRE {
  "${sedRE[@]}" "$@"
}

function version {
  sedRE -n '/^version *: *([^ ]+)$/ { s//\1/ ; p ;}' ./arx.cabal
}

function ghc_target {
  ghc --info |
  sedRE -n '/^.+"Target platform","([^-]+)-([^"]+)".+$/ { s//\1 \2/ ; p ;}'
}

function sign_and_sum {
  local file="$1"
  local platform="$2"
  local version="$3"
  local bin="$(basename "$file")"-"$version"-"$platform"
  mkdir -p "$d"
  echo "Copying binary to $d/$bin" >&2
  rsync -qa "$file" "$d/$bin"
  echo "Creating GPG signature file, $d/$bin.sig" >&2
  gpg --use-agent --detach-sign "$d/$bin"
  echo "Creating SHA 512 sum, $d/$bin.sha512" >&2
  ( cd "$d" && shasum --portable --algorithm 512 "$bin" > ./"$bin.sha512" )
}

declare -a sign_args=('./tmp/arx')
while [[ $# != 0 ]]
do
  case "$1" in
    -h|-'?'|--help) usage ; exit ;;
    --debug-mode) "$@" ;;
    *) case "${#sign_args[@]}" in
         1) sign_args[1]="$1" ;;
         *) echo 'Bad args.' ; exit 2 ;;
       esac ;;
  esac
  shift
done
declare -a target=( $(ghc_target) )
case "${#sign_args[@]}" in
  1) sign_args[1]="${target[0]}"-"${target[1]}" ;;
  2) sign_args[1]="${target[0]}"-"${sign_args[1]}" ;;
  *) echo 'Bad arguments.' ; exit 2 ;;
esac

echo 'Building stripped binary...' >&2
if ! make ./tmp/arx &>/dev/null
then
  echo 'Error running cabal!' >&2
  exit 4
fi
sign_and_sum "${sign_args[@]}" "$(version)"
echo "Created binary, signature and checksum." >&2

