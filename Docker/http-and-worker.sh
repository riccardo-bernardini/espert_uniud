#!/bin/bash

set -e

source $HTTPD_PREFIX/library/dir_names.sh

(runuser -u www-data $bin_dir/worker-manager.sh  2>$log_dir/manager.log) &

# Apache gets grumpy about PID files pre-existing
rm -f /usr/local/apache2/logs/httpd.pid

httpd -DFOREGROUND "$@"
