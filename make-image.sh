#!/bin/bash

# Make a sample filesystem image to decode

here=$(pwd)

width=$1
result=$2

image=/tmp/fat.image
fs=/mnt/play

dd if=/dev/zero of=$image bs=512 count=20000 2>&1
/sbin/mkfs.vfat -s 1 -F $width $image 2>&1
file $image

mount $fs
(
cd $fs
. $here/script1
)


sleep 0
umount $fs

cp $image $result
