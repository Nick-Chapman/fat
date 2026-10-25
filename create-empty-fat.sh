#!/bin/bash

if [ $# != 1 ]; then echo "**Usage: $0 [image-file]"; exit 1; fi
image=$1

dd if=/dev/zero of=$image bs=512 count=40000 2>&1
/sbin/mkfs.vfat -s 1 -F 16 $image 2>&1
