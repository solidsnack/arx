set -e -u

#tag="`basename "$1"`"
tag=tmpx

screen_S() {
  screen -S "$tag" "$@"
}

screen_eval() {
  cmd="$1" ; shift
  screen_S -dm -ln sleep 2
  # Set up placeholder window and free window 0; then apply settings.
  screen_S      -X eval 'screen 1 sleep 1' 'select 0' kill "$@"
  # Launch command.
  screen_S      -X screen 0 "$cmd"
  screen_S      -X eval 'select 1' kill
}

screen_eval "$1" 'zombie ko'
echo "$tag"
