root=$HTTPD_PREFIX

eval `(while read dir path ; do 
         echo ${dir}_dir=\$root/$path\; 
       done) < tree.db`

# html_dir=$root/htdocs
# cgi_dir=$root/cgi-bin
# lib_dir=$root/library
# job_dir=$root/jobs
# log_dir=$root/logs
# DVlog_dir=$html_dir/logs
# conf_dir=$root/conf
# socket_dir=$lib_dir/sockets


