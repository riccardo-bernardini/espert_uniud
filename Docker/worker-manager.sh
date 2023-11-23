#!/usr/bin/bash -eu

source $HTTPD_PREFIX/library/dir_names.sh

log_file=$log_dir/worker.log

max_fast_restart=10

let n_fast_restart=0

echo "$(date) MANAGER: Starting" >> $log_file

while true ; do
    let start=`date +%s`

    echo "$(date) MANAGER: Starting worker" >> $log_file

    $lib_dir/worker.rb 2>> $log_file

    let stop=`date +%s`

    let delta=stop-start

    if [ $delta -gt 300 ] ; then
	let n_fast_restart=0
    else
	let n_fast_restart++

	if [ $n_fast_restart -gt $max_fast_restart ]; then
	    echo "$(date) MANAGER: Too many fast restarts. Giving up" >> $log_file
	    break
	fi
    fi
done

echo "$(date) MANAGER: Stopping" >> $log_file
