#!/bin/bash

#
# This script is the entry point of the production Docker file
# Its duty is to start the worker-manager (which in turn will launch
# worker.rb) and then the HTTP server
#

set -e

source $HTTPD_PREFIX/library/dir_names.sh

(runuser -u www-data $bin_dir/worker-manager.sh  2>$log_dir/manager.log) &

# Apache gets grumpy about PID files pre-existing
rm -f /usr/local/apache2/logs/httpd.pid

httpd -DFOREGROUND "$@"
