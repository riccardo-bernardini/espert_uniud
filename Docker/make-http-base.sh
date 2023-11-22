#!/bin/bash

my_dir=`dirname $0`
cd $my_dir

sudo docker build -f ./Dockerfile.httpd-to-base -t base-httpd .
