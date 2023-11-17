#!/bin/sh

my_dir=`dirname $0`

stderr=$1
shift

stdout=$2
shift

$my_dir/inter2.sh "$@" 2> $stderr > $stdout <&- &

