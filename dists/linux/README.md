UltraStar Deluxe Linux builder
==============================

These scripts builds USDX and its dependencies. It copies the required dynamic libraries and outputs a distro-independent package that can be extracted and launched. Run this on Ubuntu 14.04.

How to use
----------

First you need to install a few dependencies:

`curl chrpath cmake build-essential fpc liblua5.3-dev`

To build the game, run `make`.

To clean up, run `make clean`. This will only remove the built compressed file and build dir.

To do a complete clean, run `make distclean`. This will delete downloaded dependencies, build prefix and build. Run this to free disk space.

`launch.sh` is copied to the game dir after build and it is what you use to launch the game.

`make compress` creates a compressed archive that can be distributed and `make upload` uploads it to transfer.sh and returns a link to it.
