
configuration_file=/usr/local/apache2/conf/httpd.conf

root=`awk '/^ServerRoot *"([^"]+)" *$/ {print $2} ' $configuration_file | tr -d '"'`

html_dir=$root/htdocs
