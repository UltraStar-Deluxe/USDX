#!/bin/sh
# Run this to set up the build system: configure, makefiles, etc.
set -e

srcdir=`dirname $0`
test -n "$srcdir" && cd "$srcdir"

echo "Updating build configuration files for USDX, please wait..."

autoreconf -isfv
if ! [ -e dists/autogen/config.sub ] ; then
	# autoreconf < 2.70 relied on automake to install aux files
	# but didn't run it in projects that don't use automake macros
	automake -a 2>&1 | grep installing || true
fi
