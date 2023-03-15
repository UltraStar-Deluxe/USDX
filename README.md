# UltraStar Deluxe Development README

[![Travis Build Status](https://travis-ci.org/UltraStar-Deluxe/USDX.svg?branch=master)](https://travis-ci.org/UltraStar-Deluxe/USDX)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/UltraStar-Deluxe/USDX?branch=master&svg=true)](https://ci.appveyor.com/project/basisbit/usdx/branch/master)
[![License](https://img.shields.io/badge/license-GPLv2-blue.svg)](LICENSE)

```
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                   _______  _________
            ___   /       \/         \
           /   \  \      _/    /\____/\__________
          /   _/  /     / \______      \         \___    _____
         /   |___/      \     \_/       /          \  \  /     \
         \              /\             /   |\       \  \/      /
          \            /  \_____/    |/       /         /
           \__________/              |_____________/          \
                                                 /      /\      \
                                                 \_____/  \_____/  .eu
                                                                    
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
```
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

### 5. Build and Run
[Freepascal](http://freepascal.org/) 3.0.0 or newer is required to compile UltraStar Deluxe. If you had some older version of fpc installed before, make sure to remove everything of it correctly before trying to install freepascal (otherwise compiling will fail with various weird error messages). Also, using the 3.0-development branch with current fixes is suggested.
If you want to help the project by coding patches, we suggest you to use the [Lazarus 1.6](http://www.lazarus-ide.org/) or newer integrated development environment.
For linking and running the game, the following libraries are also required:
- SDL2, SDL2_image
- ffmpeg 2.8 or older
- sqlite
- [bass](http://www.un4seen.com/bass.html)
- some fonts like DejaVu
- portaudio
- lua 5.1 or 5.2 or 5.3
- opencv if you want webcam support
- projectM if you want audio visualisation support

#### Compiling using Lazarus
1. Start Lazarus.
2. Choose Project → Open Project … in the menu bar. A file-dialog box will show.
3. Change to the src subdirectory of your USDX working copy (e.g. ultrastardx/src).
  * If you are running Windows, open the ultrastardx-win.lpi project-file (Preferably use the win32 verison of lazarus, as the included libraries are 32 bit).
  * On Unix-like systems use the ultrastardx-unix.lpi file.
4. Now you can compile USDX by choosing the menu entry Run → Build or pressing Ctrl+F9.
5. If you want to compile and/or start USDX directly choose Run → Run or press F9.

#### Compiling using make
##### Install prequisites
###### Linux/BSD
Required libraries:
- Debian/Ubuntu: `git automake make gcc fpc libsdl2-image-dev libavformat-dev libswscale-dev libsqlite3-dev libfreetype6-dev portaudio19-dev libportmidi-dev liblua5.3-dev libopencv-videoio-dev fonts-dejavu`
- Fedora: `git automake make gcc fpc SDL2_image-devel ffmpeg-devel sqlite-devel freetype-devel portaudio-devel portmidi-devel lua-devel opencv-devel`
- Archlinux: see the dependencies in the [ultrastardx-git](https://aur.archlinux.org/packages/ultrastardx-git) AUR package

Optional libraries:
- ProjectM visualization: `g++ libprojectm-dev` (Debian/Ubuntu) or `gcc-c++ libprojectM-devel` (Fedora)
- Webcam: `g++ libopencv-dev` (Debian/Ubuntu)

###### MacOS (High Sierra and above)
- Install Homebrew. Follow instructions from [brew.sh](http://brew.sh)
- `brew install fpc` or get it from [freepascal.org](http://www.freepascal.org/down/i386/macosx.var)
- `xcode-select --install`
- `brew install sdl2 sdl2_image automake portaudio binutils sqlite freetype lua libtiff pkg-config ffmpeg`

###### Windows using MSYS2
- Install [MSYS2](https://www.msys2.org)
- Install [FPC](https://www.freepascal.org). You need at least a custom installation with the Free Pascal Utils (for `fpcres`) and the Units.
- `pacman -S autoconf-wrapper automake-wrapper gcc git make mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_gfx mingw-w64-x86_64-SDL2_image mingw-w64-x86_64-SDL2_mixer mingw-w64-x86_64-SDL2_net mingw-w64-x86_64-SDL2_ttf mingw-w64-x86_64-ffmpeg mingw-w64-x86_64-lua51 pkgconf`
- Add some information to `.bash_profile`:
  * Path to FPC, something like `PATH="${PATH}:/c/FPC/3.2.2/bin/i386-win32"`
  * Path to mingw64 libraries, `PKG_CONFIG_PATH="${PKG_CONFIG_PATH}:/mingw64/lib/pkgconfig"`

##### Compile and run
- `git clone https://github.com/UltraStar-Deluxe/USDX`
- `cd USDX`
- `./autogen.sh`
- `./configure` (see optional flags below)
- `make` (on MacOS: `make macosx-standalone-app`)
- `./game/ultrastardx[.exe]` (on MacOS: `open UltraStarDeluxe.app`)

###### configure flags
* `--without-portaudio`: Use SDL audio input (default = Portaudio)
* `--with-opencv-cxx-api`: Use OpenCV's newer C++ API (default = old C API)

#### Compiling on Linux using flatpak-builder
- The Flatpak manifest uses the org.freedesktop.Platform 20.08 runtime, which is available for the major architectures on the [Flathub](https://flathub.org/repo/flathub.flatpakrepo) remote. If it isn't available for your architecture, you can lower the version in the manifest. Below 19.08 you either need to enable the dav1d module or disable AV1 support in the ffmpeg module by removing the --enable-libdav1d configure option. For some architectures the runtime is not hosted by Flathub but can be downloaded from the [Freedesktop SDK](https://releases.freedesktop-sdk.io/freedesktop-sdk.flatpakrepo) remote.
- The build has to be done outside of the USDX source code tree since flatpak-builder will to copy the whole source tree into the build directory. Also note that flatpak-builder will create a hidden directory `.flatpak-builder` in the directory it was called in where downloads and build results are cached.
- Assuming you can use the Flathub remote and you didn't already add it to your flatpak configuration, you can do it with
  * `flatpak remote-add --user flathub https://flathub.org/repo/flathub.flatpakrepo`
- Then building and installing the USDX flatpak is just a matter of
  * `flatpak-builder --user --install-deps-from=flathub --install build $USDX_SOURCE_TREE/dists/flatpak/eu.usdx.UltraStarDeluxe.yaml`
- The `.flatpak-builder` and `build` directories can be removed afterwards.
- Songs must be placed in `~/.var/app/eu.usdx.UltraStarDeluxe/.ultrastardx/songs`

### 6. Making a release
- Find and replace the contents of `VERSION` everywhere throughout the code
  This should be three places:
  * VERSION
  * UConfig.pas (also recompile the game after this)
  * variables.nsh (update both blocks when making the release and swap the comments, then after the release you only have to swap the comments back)
- Create Windows portable version: zip the contents of the `game` directory
- Create Windows installer:
  * Install NSIS (also install the Graphics and Language components during setup)
  * Copy the DLLs from `game` to `installer/dependencies/dll`
  * `C:\...\makensis "installer/UltraStar Deluxe.nsi"` (this will take a while)
  * The .exe will be placed in `installer/dist`

Feel free to fork this project, modify it to your hearts content and maybe also do pull requests to this repository for additional features, improvements or clean-ups.
