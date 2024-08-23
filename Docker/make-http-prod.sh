#!/bin/bash

my_dir=`dirname $0`
cd $my_dir

if [ 0 -gt 0 ]; then
    branch=$1
    shift
else
    branch=`git rev-parse --abbrev-ref HEAD`
fi

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


if [ "$branch" = "master" ]; then
    target=prod-httpd
else
    target=test-httpd
fi

sudo docker build \
     --no-cache  \
     -f ./Dockerfile.base-to-prod \
     --build-arg "branch=$branch" \
     -t "$target" \
     "$@" .
