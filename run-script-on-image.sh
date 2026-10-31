#!/bin/bash

if [ $# != 4 ]; then echo "**Usage: $0 [start.image] [script] [finish.image] [birth.out]"; exit 1; fi
start=$1
script=$2
final=$3
birth=$4

fs_image=/tmp/fat.image
fs=/mnt/play

rm -f $fs_image
cp $start $fs_image

here=$(pwd)
mount $fs
sleep 0

# TODO: make it work, even without setting a referecne time...
#sudo unshare --time bash -c 'date -s "1997-12-29 12:25:02"' > /dev/null
sudo unshare --time bash -c 'date -s "2011-11-11 12:34:56"' > /dev/null


(cd $fs; . $here/$script)
stat $fs/birth | grep Birth | cut -d' ' -f3- | cut -c1-22 > $birth
sleep 0
umount -l $fs

cp $fs_image $final
