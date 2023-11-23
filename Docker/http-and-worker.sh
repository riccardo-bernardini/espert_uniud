#!/bin/sh

set -e

root=$HTTPD_PREFIX
bin_dir=$root/library

$bin_dir/worker-manager.sh &

# Apache gets grumpy about PID files pre-existing
rm -f /usr/local/apache2/logs/httpd.pid

exec httpd -DFOREGROUND "$@"
