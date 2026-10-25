#!/bin/bash

./create-empty-fat.sh empty.image

here=$(pwd)

script=$1
result=$2

image=/tmp/fat.image
fs=/mnt/play

cp empty.image $image

mount $fs
(
cd $fs
. $here/$script
)


sleep 0
umount $fs

cp $image $result
