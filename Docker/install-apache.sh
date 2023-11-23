#!/bin/bash -e

my_dir=`dirname $0`
cd $my_dir

source ./dir_names.sh

configuration_file=$HTTPD_PREFIX/conf/httpd.conf

cp ./my-httpd.conf $configuration_file



mkdir -p $html_dir

cp index.html $html_dir


mkdir -p $cgi_dir

cp frame_maker.rb $cgi_dir



mkdir -p $lib_dir

cp working-for-you.thtml $lib_dir
cp worker.rb             $lib_dir
cp channel.rb            $lib_dir
cp micro_macro_proc.rb   $lib_dir
cp definitions.rb        $lib_dir
cp dir_names.sh          $lib_dir

cd ../accumulator
gprbuild main

cp obj/main              $lib_dir/accumulator

cd $my_dir


mkdir -p $job_dir
