#!/bin/sh

my_dir=`dirname $0`

image_dir=$1
shift

$my_dir/accumulator.ext "$@"

cd $image_dir

zip -9 images.zip *.png
