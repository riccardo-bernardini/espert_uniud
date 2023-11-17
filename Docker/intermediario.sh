#!/bin/sh

my_dir=`dirname $0`

$my_dir/accumulator.exe "$@" 2> /tmp/qq  <&- > /tmp/qq &

