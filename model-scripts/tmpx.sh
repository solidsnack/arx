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
  ! $rm_ || trap "! \$rm_ || rm -rf $dir" EXIT
  trap "exit 2" HUP INT QUIT BUS SEGV PIPE TERM
  mkdir $dir
  cd $dir
fi
go () {
  unpack_env | bzcat > ./env
  unpack_run | bzcat > ./run ; chmod ug+x ./run
  mkdir dat
  cd dat
  unpack_dat
  if $run
  then
    if ( . ../env && ../run )
    then
      ext=$?
      rm_=$rm0
    else
      ext=$?
      rm_=$rm1
    fi
    exit $ext
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
