#!/bin/bash

my_dir=`dirname $0`
cd $my_dir

function is_git_status_dirty() {
    status=$(git status -s)
    [ -n "$status" ]
}


if is_git_status_dirty ;  then
    echo "CURRENT TREE IS NOT CLEAN. Exiting"
    echo ---------------------------
    echo
    
    git status

    exit 1 
fi


sudo docker build -f ./Dockerfile.base-to-prod -t prod-httpd "$@" .
