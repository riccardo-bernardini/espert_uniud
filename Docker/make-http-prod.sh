#!/bin/bash

my_dir=`dirname $0`
cd $my_dir

sudo docker build -f ./Dockerfile.base-to-prod -t prod-httpd .
