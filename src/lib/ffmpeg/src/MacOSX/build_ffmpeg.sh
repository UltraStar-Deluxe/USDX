#!/bin/bash

cd ffmpeg
./configure --enable-shared --disable-static --disable-mmx
make

