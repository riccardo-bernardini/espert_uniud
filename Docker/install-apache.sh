#!/bin/bash -e

my_dir=`dirname $0`
cd $my_dir

configuration_file=/usr/local/apache2/conf/httpd.conf

cp ./my-httpd.conf $configuration_file

root=`awk '/^ServerRoot *"([^"]+)" *$/ {print $2} ' $configuration_file | tr -d '"'`

html_dir=$root/htdocs
mkdir -p $html_dir

cp index.html $html_dir


cgi_dir=$root/cgi-bin
mkdir -p $cgi_dir

cp frame_maker.rb $cgi_dir


lib_dir=$root/library

mkdir -p $lib_dir

cp working_for_you.thtml $lib_dir
cp worker.rb             $lib_dir
cp channel.rb            $lib_dir
cp micro_macro_proc.rb   $lib_dir
cp definitions.rb        $lib_dir

cd ../accumulator
gprbuild

cp obj/main              $lib_dir/accumulator

cd $my_dir

job_dir=$root/jobs

mkdir -p $job_dir
