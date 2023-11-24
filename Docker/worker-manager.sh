#!/usr/bin/bash -eu

id

su www-data

source /usr/local/apache2/library/dir_names.sh

log_file=$log_dir/worker.log

max_fast_restart=10

n_fast_restart=0

echo "$(date) MANAGER: Starting" >> $log_file

while true ; do
    start=`date +%s`

    echo "$(date) MANAGER: Starting worker" >> $log_file

    $bin_dir/worker.rb 2>> $log_file

    stop=`date +%s`

    delta=$((stop-start))

    if [ $delta -gt 300 ] ; then
	n_fast_restart=0
    else
	n_fast_restart=$((n_fast_restart+1))

	if [ $n_fast_restart -gt $max_fast_restart ]; then
	    echo "$(date) MANAGER: Too many fast restarts. Giving up" >> $log_file
	    break
	fi
    fi
done

echo "$(date) MANAGER: Stopping" >> $log_file
