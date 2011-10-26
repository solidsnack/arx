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
  mkdir $dir
  cd $dir
fi
unpack_env () { : # NOOP
  # To be set by tool.
} ; unpack_env | bzcat > ./env
unpack_run () { : # NOOP
  # To be set by tool.
} ; unpack_run | bzcat > ./run ; chmod ug+x ./run
mkdir dat
cd dat
unpack_dat () { : # NOOP
  # To be set by tool.
} ; unpack_dat
if $run
then
  ( . ../env && ../run ) || true
  ext=$?
  [ 0 = $ext ] && : ${rm_:=$rm0} || : ${rm_:=$rm1}
  exit $ext
fi
