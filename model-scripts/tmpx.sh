#!/bin/sh
set -e -u
unset rm_ dir
tmp=true ; run=true
rm0=true ; rm1=true # To be set by tool.
for arg in "$@"
do
  case "$arg" in
    --no-rm)    rm_=false ;;
    --no-run)   run=false ;;
    --extract)  rm_=false ; tmp=false ; run=false ;;
  esac
done
if $tmp
then
  dir=/tmp/tmpx.`date -u +%FT%TZ`.$$
  rm -rf $dir
  : ${rm_:=true}
  if $rm_
  then
    trap "case \$?/$rm0/$rm1 in
            0/true/*)      rm -rf $dir ;;
            [1-9]*/*/true) rm -rf $dir ;;
          esac" EXIT
    trap "exit 2" HUP INT QUIT BUS SEGV PIPE TERM
  fi
  mkdir $dir
  cd $dir
fi
go () {
  unpack_env > ./env
  unpack_run > ./run ; chmod ug+x ./run
  mkdir dat
  cd dat
  unpack_dat
  if $run
  then
    ( . ../env && ../run )
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
go
