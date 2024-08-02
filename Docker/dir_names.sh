#
# Read the file tree.db that contains pairs <label> <path> and
# for every line create an environment variable called <label>_dir
# that contains $root/<path>.
#
# Why this involuted approach instead of a simple script with
# the assignments?  Because the same directories are required
# also by ruby scripts which can read tree.db directly
#
root=$HTTPD_PREFIX

#
# Quite an hack: read tree.db and create a sequence of assignments
# from its content.  The sequence of assignments is then given
# to eval
#
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


