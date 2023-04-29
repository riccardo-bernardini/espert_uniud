#!/bin/bash -e



if env | grep DRYRUN > /dev/null ; then
    echo=echo
else
    echo=''
fi

doc_files=''

function install_doc {
    doc_src_dir=$1

    mkdir -p $PREFIX/doc
    
    for doc_src in $doc_src_dir/*.md ; do
	base=`basename $doc_src .md`
	target=$PREFIX/doc/$base.html

	doc_files="$doc_files:$doc_src"

	pandoc $doc_src -o "$target"
    done
}

function install_filtering {
    for src in $my_dir/spatial_filtering/*.rb ; do
	name=`basename $src`
	dst=$PREFIX/bin/$name
	
	$echo cp $src $dst
	$echo chmod u+x $dst
    done

    install_doc $my_dir/spatial_filtering/doc
}

function install_events_to_frames {
    $echo cd $my_dir/accumulator

    $echo gprbuild -p

    $echo cp obj/main      $PREFIX/bin/events-to-frames
    $echo cp obj/convert   $PREFIX/bin/event-conversion

    install_doc $my_dir/accumulator/doc
}

function install_index {
    working_dir=$my_dir/.work
    mkdir -p $working_dir

    lib/make_index.rb $working_dir/index.md "$doc_files"

    pandoc $working_dir/index.md -o $PREFIX/doc/index.html
}


if [ $# -ne 1 ]; then
    echo "Usage: $0 prefix" >&2
    exit 1
fi


my_dir=`dirname $0`
my_dir=`readlink -f $my_dir`

PREFIX=$1

$echo mkdir -p $PREFIX/bin

install_filtering

install_events_to_frames

install_index 

exit 0

