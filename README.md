# UltraStar Deluxe

[![Travis Build Status](https://travis-ci.org/UltraStar-Deluxe/USDX.svg?branch=master)](https://travis-ci.org/UltraStar-Deluxe/USDX)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/UltraStar-Deluxe/USDX?branch=master&svg=true)](https://ci.appveyor.com/project/basisbit/usdx/branch/master)
[![License](https://img.shields.io/badge/license-GPLv2-blue.svg)](LICENSE)

Official Project Website: https://usdx.eu/

![UltraStar Deluxe Logo](https://github.com/UltraStar-Deluxe/USDX/blob/master/icons/ultrastardx-icon_256.png)


### 1. About
UltraStar Deluxe (USDX) is a free and open source karaoke game. It allows up to six players to sing along with music using microphones in order to score points, depending on the pitch of the voice and the rhythm of singing.
UltraStar Deluxe is a fork of the original UltraStar (developed by corvus5).
Many features have been added like party mode, theme support and support for more audio and video formats.
The improved stability and code quality of USDX enabled ports to Linux and Mac OS X.

### 2. Configuration
- To set additional song directories change your config.ini like this:
```ini
  [Directories]
  SongDir1=C:\Users\My\Music\MyUSDXSongs
  SongDir2=F:\EvenMoreUSDXSongs
  SongDir...=... (some more directories)
```
- To enable joypad support change config.ini `Joypad=Off` to `Joypad=On`
- To enable 2 or 3 player each on 2 screens, disable the full screen mode, extend your desktop horizontally and set the resolution to fill one screen. Then, in the config.ini set `Screens=2` and restart the game.
- The primary folder for songs on OS X is `$HOME/Music/UltraStar Deluxe`, which is created when UltraStar Deluxe is run for the first time.
- On OS X, by default the `config.ini` file is created in `$HOME/Library/Application Support/UltraStarDeluxe` when UltraStar Deluxe is run for the first time.

### 3. Command-Line Parameters
See https://github.com/UltraStar-Deluxe/USDX/wiki/Command-Line-Parameters

### 4. Controls
See https://github.com/UltraStar-Deluxe/USDX/wiki/Controls

### 5. Compiling
There are two main ways to compile the game:

1. Lazarus IDE
2. `./autogen.sh && ./configure && make`

The executable will be `game/ultrastardx[.exe]`.

For extended information, dependencies, OS-specific notes and configure flags, see [COMPILING.md](COMPILING.md).

### 6. Making a release
1. Find the contents of `VERSION` (strip the `+dev`) throughout the code.
    This should result in four places:
    * VERSION
    * UConfig.pas
    * variables.nsh
    * ultrastardx.appdata.xml
2. Make the release:
    * in the first two files, update it to the new version _without_ the `+dev` bit
    * in `variables.nsh` update both blocks immediately and swap the comments
    * in `ultrastardx.appdata.xml` add a new entry
3. Commit, `git tag v<the-new-version>` and then `git push origin master v<the-new-version>`\
    __It is important that these two get pushed together__ otherwise Flatpak gets confused.
4. Wait and get the artifacts from the CI.
    If any of them fail, just add an extra `;` on one of the already commented lines in `variables.nsh`, commit, and then push only `master`.
5. Add `+dev` to the version in the first two files and swap the comments in `variables.nsh` again, commit, push.
    This is just to set the dev version again.
6. Attach the artifacts to the release page and publish it.
    Don't forget to also create a PR for this release in
    https://github.com/UltraStar-Deluxe/ultrastar-deluxe.github.io
