#!/bin/bash

if [ $# != 3 ]; then echo "**Usage: $0 [start.image] [script] [finish.image]"; exit 1; fi
start=$1
script=$2
final=$3

fs_image=/tmp/fat.image
fs=/mnt/play

cp $start $fs_image

here=$(pwd)
mount $fs
(cd $fs; . $here/$script)
sleep 0
umount $fs

cp $fs_image $final
