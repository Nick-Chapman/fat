#!/bin/bash

if [ $# != 1 ]; then echo "**Usage: $0 [image-file]"; exit 1; fi
primary=$1

copy=/tmp/interact-$$.image

jenga install $primary $copy

./run-script-on-image.sh $copy script-ls /dev/null /dev/null
