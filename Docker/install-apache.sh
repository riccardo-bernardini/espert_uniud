#!/bin/bash -eu

#
# This script is called during the creation of the Docker image
# and does mostly of the installation work: it creates the
# required directories, it copies executables and library files in
# the right directories, it compiles the Ada program, ...
#

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
chmod a+x $cgi_dir/frame_maker.rb


mkdir -p $lib_dir

cp working-for-you.thtml $lib_dir
cp channel_client.rb     $lib_dir
cp channel_server.rb     $lib_dir
cp socket_name.rb        $lib_dir
cp micro_macro_proc.rb   $lib_dir
cp definitions.rb        $lib_dir
cp tree.db               $lib_dir

mkdir -p $bin_dir

for i in worker.rb dir_names.sh  worker-manager.sh ; do
    cp $i $bin_dir
    chmod a+x $bin_dir/$i
done


(
    cd ../accumulator
    gprbuild main

    cp obj/main  $bin_dir/accumulator
)

mkdir -p $socket_dir
chown www-data $socket_dir

mkdir -p $job_dir
chown www-data $job_dir

mkdir -p $log_dir
chown www-data $log_dir

