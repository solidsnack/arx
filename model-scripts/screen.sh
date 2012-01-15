set -e -u

#tag="`basename "$1"`"
tag=tmpx
screen_() {
  screen -S "$tag" "$@"
  sleep 2
}

screen_ -dm -ln sleep 10
# Set up placeholder window and free window 0; then apply configuration.
screen_      -X eval 'screen 1 sleep 5' 'select 0' kill 'zombie ko'
screen_      -X screen 0 "$1"
screen_      -X eval 'select 1' kill
echo "$tag"
