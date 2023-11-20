#!/usr/bin/bash -e

my_dir=`dirname $0`
cd $my_dir

top_dir=$HOME/tmp/pino

if test ! -d $top_dir ; then
    echo $top_dir is not a directory.  Exiting
    exit 1
fi

mkdir -p $top_dir/www/html

ln -s $PWD/form.html  $top_dir/www/html

mkdir -p $top_dir/www/exe

ln -s $PWD/worker.rb       $top_dir/www/exe
ln -s $PWD/frame_maker.rb  $top_dir/www/exe

(cd ../accumulator ; gprbuild main)

ln -s $PWD/../accumulator/obj/main  $top_dir/www/exe/accumulator.exe

mkdir -p $top_dir/www/exe/lib

ln -s $PWD/my_config.rb        $top_dir/www/exe/lib
ln -s $PWD/channel.rb          $top_dir/www/exe/lib
ln -s $PWD/micro_macro_proc.rb $top_dir/www/exe/lib

ln -s $PWD/working-for-you.thtml $top_dir/www/exe/lib


mkdir -p $top_dir/cache/lighttpd/uploads
mkdir -p $top_dir/log/lighttpd

./r4.rb root=$top_dir < lighttpd.conf.in > $top_dir/lighttpd.conf

cat > $top_dir/start-server.sh <<EOF
#!/bin/sh

my_dir=\`dirname \$0\`
cd \$my_dir

lighttpd -D -f lighttpd.conf
EOF

chmod u+x $top_dir/start-server.sh



