#!/bin/bash

if [ $# != 3 ]; then echo "**Usage: $0 [start.image] [script] [finish.image]"; exit 1; fi
start=$1
script=$2
final=$3

fs_image=/tmp/fat.image
fs=/mnt/play

rm -f $fs_image
cp $start $fs_image

here=$(pwd)
mount $fs
sudo unshare --time bash -c 'date -s "1997-12-29 12:25:02"' > /dev/null
# The date set above for the separate time domain strangley persists
# when running the script
(cd $fs; . $here/$script)
sleep 0
umount -l $fs

cp $fs_image $final
