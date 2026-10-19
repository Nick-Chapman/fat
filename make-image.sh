#!/bin/bash -v

# Make a sample filesystem image to decode

width=$1
result=$2

image=/tmp/fat.image
fs=/mnt/play

dd if=/dev/zero of=$image bs=512 count=20000
/sbin/mkfs.vfat -s 1 -F $width $image
file $image

mount $fs
(
cd $fs

echo 'contents' > first

echo 'q' > abc-defghijklmnopqrstuvwxyz

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

echo -n 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' > a
echo 'x' > b
echo 'x' > c

mkdir sub
echo 'subfile' > sub/thing

mkdir sub/sub2
echo 'sub2file' > sub/sub2/deep

echo 'x' > d
echo 'x' > e

echo 'a' > sub/thing2
echo 'b' > sub/thing3
echo 'c' > sub/thing4
echo 'd' > sub/thing5
echo 'e' > sub/thing6
echo 'f' > sub/thing7
echo 'g' > sub/thing8
echo 'h' > sub/thing9
echo 'i' > sub/thing10
echo 'j' > sub/thing11
echo 'k' > sub/thing12
echo 'l' > sub/thing13
echo 'm' > sub/thing14
echo 'n' > sub/thing15
echo 'o' > sub/thing16
echo 'p' > sub/thing17

#cat /usr/share/dict/words | head -150 > words

echo 'AAAAAAAAAAAAAAAAAAA-LAST' >> a
)


sleep 0
umount $fs

cp $image $result
