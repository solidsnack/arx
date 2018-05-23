#!/bin/sh
set -e -u
unset rm_ dir
tmp=true ; run=true
tmpdir= ; rm0=true ; rm1=true ; shared=false ; hash="" # To be set by tool.
token=`date -u +%FT%TZ | tr -d :-`-`hexdump -n4 -e '"%08x"' </dev/urandom`
opts() {
  cmd="$1" ; shift
  n=$#
  i=0
  magic_slash=false
  # Walk the args in order, processing options and placing non-option
  # arguments at the end. When finished, arguments are in reverse order.
  while [ "$i" -lt "$n" ]
  do
    arg="$1" ; shift
    case "$arg!$magic_slash" in
      --shared!false)  shared=true ;;
      --no-rm!false)   rm_=false ;;
      --no-run!false)  run=false ;;
      --extract!false) rm_=false ; tmp=false ; run=false ;;
      //!false)        magic_slash=true ;;
      *)               set -- "$@" "$arg" ;;
    esac
    i=$(($i+1))
  done
  # Unreverse the args.
  n=$#
  i=0
  while [ "$i" -lt "$n" ]
  do
    arg="$1" ; shift
    set -- "$@" "$arg"
    i=$(($i+1))
  done
  # Set the trap.
  if $tmp
  then
    if $shared
    then
      rm_=false
      dir="$tmpdir"/tmpx-"$hash"
    else
      dir="$tmpdir"/tmpx-"$token"
    fi
    : ${rm_:=true}
    if $rm_
    then
      trap 'case $?/$rm0/$rm1 in
              0/true/*)      rm -rf "$dir" ;;
              [1-9]*/*/true) rm -rf "$dir" ;;
            esac' EXIT
      trap 'exit 2' HUP INT QUIT BUS SEGV PIPE TERM
    fi
    mkdir -p "$dir"
    cd "$dir"
  fi
  # Call the command with the reassembled ARGV, options removed.
  "$cmd" "$@"
}
go () {
  [ -f env ] || unpack_env > ./env
  [ -f run ] || unpack_run > ./run
  chmod ug+x ./run
  if [ -d dat ]
  then
    cd dat
  else
    mkdir dat
    cd dat
    unpack_dat
  fi
  if $run
  then
    { . ../env && exec ../run "$@" }
  fi
}
unpack_env () { : # NOOP
  # To be set by tool.
}
unpack_run () { : # NOOP
  # To be set by tool.
}
unpack_dat () { : # NOOP
  # To be set by tool.
}
opts go "$@"
