#!/bin/bash

if [ $# != 1 ]; then echo "**Usage: $0 [image-file]"; exit 1; fi
primary=$1

copy=/tmp/interact-$$.image

jenga install $primary $copy
chmod +w $copy
jenga exec src/fat.exe -- -i $copy

echo interact-done
rm $copy
