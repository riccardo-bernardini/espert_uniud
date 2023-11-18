#!/bin/sh

my_dir=`dirname $0`

echo "$@" >&2

stderr=$1
shift

stdout=$1
shift

$my_dir/inter2.sh "$@" 2> $stderr > $stdout <&- &

