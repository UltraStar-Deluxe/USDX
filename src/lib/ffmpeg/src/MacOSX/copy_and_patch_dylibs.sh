#!/bin/bash

# Copy dylibs:
cp ffmpeg/libavcodec/libavcodec.51.dylib ../../libavcodec.dylib
cp ffmpeg/libavformat/libavformat.52.dylib ../../libavformat.dylib
cp ffmpeg/libavutil/libavutil.49.dylib ../../libavutil.dylib

# Patching libavcodec:
install_name_tool -id @executable_path/libavcodec.dylib ../../libavcodec.dylib
install_name_tool -change /usr/local/lib/libavutil.dylib @executable_path/libavutil.dylib ../../libavcodec.dylib

# Patching libavformat:
install_name_tool -id @executable_path/libavformat.dylib ../../libavformat.dylib
install_name_tool -change /usr/local/lib/libavutil.dylib @executable_path/libavutil.dylib ../../libavformat.dylib
install_name_tool -change /usr/local/lib/libavcodec.dylib @executable_path/libavcodec.dylib ../../libavformat.dylib

# Patching libavcodec:
install_name_tool -id @executable_path/libavutil.dylib ../../libavutil.dylib

# Printing result:
otool -L ../../libavutil.dylib
otool -L ../../libavcodec.dylib
otool -L ../../libavformat.dylib