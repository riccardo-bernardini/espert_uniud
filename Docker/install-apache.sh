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
chmod a+x $cgi_dir/frame_maker.rb


mkdir -p $lib_dir

cp working-for-you.thtml $lib_dir
cp channel.rb            $lib_dir
cp micro_macro_proc.rb   $lib_dir
cp definitions.rb        $lib_dir


for i in worker.rb dir_names.sh  worker-manager.sh ; do
    cp $i $lib_dir
    chmod a+x $lib_dir/$i
done


cd ../accumulator
gprbuild main

cp obj/main              $lib_dir/accumulator

cd $my_dir

mkdir -p $socket_dir
chown www-data $socket_dir

mkdir -p $job_dir
chown www-data $job_dir

mkdir -p $DVlog_dir
chown www-data $DVlog_dir
chown www-data $log_dir

ln -s $DVlog_dir/worker.log $html_dir/worker.txt
ln -s $DVlog_dir/cgi.log    $html_dir/cgi_log.txt
