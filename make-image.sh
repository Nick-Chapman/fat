#!/bin/bash

# Make a sample filesystem image to decode

image=/tmp/fat.image
fs=/mnt/play

dd if=/dev/zero of=$image bs=512 count=1000
/sbin/mkfs.vfat -F 32 $image
file $image

#umount $fs
mount $fs
(
cd $fs

echo 'contents' > first

#echo '' > 1
#echo '' > 12345678
#echo '' > 123456789
#echo '' > 123456789a
#echo '' > 1.a
#echo '' > 1.abc
#echo '' > 1.abcd

echo -n '' > zero
echo -n 'x' > single
echo '0123456789' > numbers.txt
echo 'abcdefghijklmnopqrstuvwxyz' > letters

#echo -n 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' > a
echo 'x' > b
echo 'x' > c

mkdir sub
echo 'subfile' > sub/thing

mkdir sub/sub2
echo 'sub2file' > sub/sub2/deep

echo 'x' > d
echo 'x' > e

#cat /usr/share/dict/words | head -150 > words

echo 'AAAAAAAAAAAAAAAAAAA-LAST' >> a

)


sleep 0
umount $fs

cp $image $1
