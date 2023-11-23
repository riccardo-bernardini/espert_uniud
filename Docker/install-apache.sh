#!/bin/bash -eu

my_dir=`dirname $0`
cd $my_dir

source ./dir_names.sh

cp ./my-httpd.conf $conf_dir/httpd.conf

chmod a+x http-and-worker.sh
cp http-and-worker.sh /usr/local/bin

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
cp worker-manager.sh     $lib_dir

cd ../accumulator
gprbuild main

cp obj/main              $lib_dir/accumulator

cd $my_dir


mkdir -p $job_dir
