#!/bin/bash

diff <(hd -v $1 | head -20000) <(hd -v $2 | head -20000)
