#!/bin/sh

set -e

source $HTTPD_PREFIX/library/dir_names.sh

$bin_dir/worker-manager.sh 2>$log_dir/manager.log &

# Apache gets grumpy about PID files pre-existing
rm -f /usr/local/apache2/logs/httpd.pid

exec httpd -DFOREGROUND "$@"
