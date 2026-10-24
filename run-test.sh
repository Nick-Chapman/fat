#!/bin/bash

here=$(pwd)

image=/tmp/fat.image
fs=/mnt/play

dd if=/dev/zero of=$image bs=512 count=40000 2>&1
/sbin/mkfs.vfat -s 1 -F 16 $image 2>&1

cp $image 0.image
sleep 0
mount $fs
sleep 0
echo xxx > $fs/aaaaaaaaaaaaa
echo yyy > $fs/bbbbbbbbbbbbb
sleep 0
umount $fs
cp $image 1.image


jenga exec src/fat.exe -- 0.image -m xxx aaaaaaaaaaaaa
jenga exec src/fat.exe -- 0.image -m yyy bbbbbbbbbbbbb

diff <(hd 0.image) <(hd 1.image)
