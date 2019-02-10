#!/bin/sh
# Launch script for UltraStar Deluxe
# Set $DEBUGGER to launch the app with a debugger.

# Change to game directory
GAMEPATH="`readlink -f "$0"`"
cd "`dirname "$GAMEPATH"`"

BIN=./ultrastardx

# Run the game, (optionally) with the debugger
exec $DEBUGGER $BIN "$@"
