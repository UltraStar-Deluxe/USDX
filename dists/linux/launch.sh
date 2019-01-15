#!/bin/sh
# Launch script for UltraStar Deluxe
# Set $DEBUGGER to launch the app with a debugger.

# Change to game directory
GAMEPATH="`readlink -f "$0"`"
cd "`dirname "$GAMEPATH"`"

# What architecture are we running?
MACHINE=`uname -m`
if [ "$MACHINE" = "x86_64" ]
then
	# Set path to binary (64 bit)
	BIN=./ultrastardx.x86_64
else
	# Default to x86. If it's not x86, it might be able to emulate it.
	BIN=./ultrastardx.x86
fi

# Run the game, (optionally) with the debugger
exec $DEBUGGER $BIN "$@"
