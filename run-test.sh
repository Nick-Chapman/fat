#!/bin/bash

./create-empty-fat.sh 0.image

image=/tmp/fat.image
fs=/mnt/play

cp 0.image $image
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

./diff-images.sh 0.image 1.image
