#!/bin/bash -e


if env | grep DRYRUN > /dev/null ; then
    echo=echo
else
    echo=''
fi

function install_filtering {
    for src in $my_dir/spatial_filtering/*.rb ; do
	name=`basename $src`
	dst=$PREFIX/bin/$name
	
	$echo cp $src $dst
	$echo chmod u+x $dst
    done
}

function install_event_to_frame {
    $echo cd $my_dir/accumulator

    $echo gprbuild

    $echo cp obj/main      $PREFIX/bin/events-to-frames
    $echo cp obj/convert   $PREFIX/bin/event-conversion
}

if [ $# -ne 1 ]; then
    echo "Usage: $0 prefix" >&2
    exit 1
fi


my_dir=`dirname $0`

PREFIX=$1

install_filtering

false

install_event_to_frame

