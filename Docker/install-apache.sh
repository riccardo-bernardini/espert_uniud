#!/bin/sh

my_dir=`dirnname $0`
cd $my_dir

configuration_file=/usr/local/apache2/conf/httpd.conf

root=`awk '/^ServerRoot *"([^"]+)" *$/ {print $2} ' $configuration_file | tr -d '"'`

html_dir=$root/htdocs
mkdir -p $html_dir

cp index.html $html_dir


cgi_dir=$root/cgi-bin
mkdir -p $cgi_bin

cp frame_maker.rb $cgi_bin


lib_dir=$root/library

mkdir -p $lib_dirb

cp working_for_you.thtml $lib_dir
cp worker.rb             $lib_dir
cp channel.rb            $lib_dir
cp micro_macro_proc.rb   $lib_dir
cp my_config.rb          $lib_dir

cd ../accumulator
gprbuild

cp obj/main              $lib_dir/accumulator

job_dir=$root/jobs

mkdir -p $job_dir
