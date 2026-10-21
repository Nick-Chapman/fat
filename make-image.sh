#!/bin/bash

# Make a sample filesystem image to decode

here=$(pwd)

script=$1
result=$2

image=/tmp/fat.image
fs=/mnt/play

dd if=/dev/zero of=$image bs=512 count=40000 2>&1
/sbin/mkfs.vfat -s 1 -F 16 $image 2>&1
file $image

mount $fs
(
cd $fs
. $here/$script
)


sleep 0
umount $fs

cp $image $result
