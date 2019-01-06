#!/bin/sh
# Launch script for UltraStar Deluxe
# Set $DEBUGGER to launch the game with a debugger.

# Change to game directory
GAMEPATH="`readlink -f "$0"`"
cd "`dirname "$GAMEPATH"`"

# Set path to libraries and binary
BIN=./ultrastardx
LIBPATH=./lib

# Run the game, (optionally) with the debugger
LD_LIBRARY_PATH="$LIBPATH:$LD_LIBRARY_PATH" exec $DEBUGGER $BIN $@
