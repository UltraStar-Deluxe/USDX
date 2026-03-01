# Compiling
[Free Pascal](http://freepascal.org/) 3.0.0 or newer is required to compile UltraStar Deluxe. If you had some older version of fpc installed before, make sure to remove everything of it correctly before trying to install Free Pascal (otherwise compiling will fail with various weird error messages). Also, using the newest version is suggested.
If you want to help the project by coding patches, we suggest you to use the [Lazarus 1.6](http://www.lazarus-ide.org/) or newer integrated development environment.
For linking and running the game, the following libraries are also required:
- SDL2, SDL2_image
- FFmpeg 4.0-8.0
- SQLite 3
- [BASS](http://www.un4seen.com/bass.html)
- some fonts like DejaVu
- PortAudio
- Lua 5.1, 5.2, 5.3 or 5.4
- OpenCV if you want webcam support
- projectM 2,x if you want audio visualisation support

Prebuilt DLLs for SDL2, SDL2_image, FFmpeg, SQLite, PortAudio, and Lua can be found in the releases section of [our MXE fork](https://github.com/UltraStar-Deluxe/mxe). You can use the dldlls.py script to download the DLLs for the checked out code. The remaining DLLs needed for Windows builds are part of this repository.

## Compiling using Lazarus
1. Start Lazarus.
2. Choose Project → Open Project … in the menu bar. A file-dialog box will show.
3. Change to the src subdirectory of your USDX working copy (e.g. ultrastardx/src).
  * If you are running Windows, open the ultrastardx-win.lpi project-file (use the win64 version of Lazarus, as the included libraries are 64 bit).
  * On Unix-like systems use the ultrastardx-unix.lpi file.
4. Now you can compile USDX by choosing the menu entry Run → Build or pressing Ctrl+F9.
5. If you want to compile and/or start USDX directly choose Run → Run or press F9.

## Compiling using make
### Install prequisites
#### Linux/BSD
Required libraries:
- Debian/Ubuntu: `git automake make gcc fpc libsdl2-image-dev libavformat-dev libavcodec-dev libavutil-dev libswresample-dev libswscale-dev libsqlite3-dev libfreetype6-dev portaudio19-dev libportmidi-dev liblua5.3-dev libopencv-videoio-dev fonts-dejavu`
- Fedora: `git automake make gcc fpc SDL2_image-devel ffmpeg-devel sqlite-devel freetype-devel portaudio-devel portmidi-devel lua-devel opencv-devel`
- Archlinux: see the dependencies in the [ultrastardx-git](https://aur.archlinux.org/packages/ultrastardx-git) AUR package

Optional libraries:
- ProjectM visualization: `g++ libprojectm-dev` (Debian/Ubuntu) or `gcc-c++ libprojectM-devel` (Fedora)
- Webcam: `g++ libopencv-dev` (Debian/Ubuntu)

#### MacOS (High Sierra and above)
- Install Homebrew. Follow instructions from [brew.sh](http://brew.sh)
- `brew install fpc` or get it from [freepascal.org](http://www.freepascal.org/down/i386/macosx.var)
- `xcode-select --install`
- `brew install sdl2 sdl2_image automake portaudio binutils sqlite freetype lua libtiff pkg-config ffmpeg`

#### Windows using MSYS2
- Install [MSYS2](https://www.msys2.org)
- Install [FPC](https://www.freepascal.org). Use the Win32 cross-to-Win64 installer so you get `ppcrossx64` and the Win64 RTL units.
- `pacman -S autoconf-wrapper automake-wrapper gcc git make mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_image mingw-w64-x86_64-ffmpeg mingw-w64-x86_64-lua mingw-w64-x86_64-pkgconf pkgconf`
- Add some information to `.bash_profile`:
  * Path to the cross compiler: `PATH="${PATH}:/c/FPC/3.2.2/bin/i386-win32"`
  * FPC config and base: `FPCCFG="/c/FPC/3.2.2/bin/i386-win32/fpc.cfg"` and `FPCDIR="/c/FPC/3.2.2"`
  * mingw64 pkg-config path: `PKG_CONFIG_PATH="${PKG_CONFIG_PATH}:/mingw64/lib/pkgconfig:/mingw64/share/pkgconfig"`
  * (Optional) pin the compiler: `PPC="ppcrossx64"`

### Compile and run
- `git clone https://github.com/UltraStar-Deluxe/USDX`
- `cd USDX`
- `./autogen.sh`
- `./configure` (see optional flags below)
- `make` (on MacOS: `make macosx-standalone-app`)
- `./game/ultrastardx[.exe]` (on MacOS: `open UltraStarDeluxe.app`)

#### configure flags
* `--enable-debug`: Outputs warnings and errors from Error.log also to the console, and prints stacktraces when an EAccessViolation occurs.
* `--with-portaudio`: This is the default.
* `--without-portaudio`: Use SDL audio input instead.
  This should support newer platforms like PulseAudio and PipeWire.
  Might not be able to detect the number of channels correctly when used with PulseAudio.
  Might fix issues experienced when using PortAudio on certain distributions.
* `--without-opencv-cxx-api`: This is the default.
  OpenCV does not need to be present at build time.
  It will look for it at runtime using a deprecated C API, and enable webcam functionality if found.
  Current Linux distributions do not offer the C API.
* `--with-opencv-cxx-api`: Use OpenCV's newer C++ API.
  Required for webcam support under Linux, but requires OpenCV to be present at both build time as well as runtime.

## Compiling on Linux using flatpak-builder
- The manifest for our Flathub releases is in a different repository:
  *  `git clone --recurse-submodules https://github.com/flathub/eu.usdx.UltraStarDeluxe manifest-dir`
- The Flatpak manifest uses the org.freedesktop.Platform 23.08 runtime, which is available for the major architectures on the [Flathub](https://flathub.org/repo/flathub.flatpakrepo) remote. If it isn't available for your architecture, you can lower the version in the manifest. Below 23.08 you might want to add your own build of FFmpeg and dav1d for better file format support. For some architectures the runtime is not hosted by Flathub but can be downloaded from the [Freedesktop SDK](https://releases.freedesktop-sdk.io/freedesktop-sdk.flatpakrepo) remote.
- If you change the manifest to use a USDX source tree from your hard disk, the build has to be done outside of that tree since flatpak-builder will try to copy the whole source tree into the build directory. Also note that flatpak-builder will create a hidden directory `.flatpak-builder` in the directory it was called in where downloads and build results are cached.
- Assuming you can use the Flathub remote and you didn't already add it to your flatpak configuration, you can do it with
  * `flatpak remote-add --user flathub https://flathub.org/repo/flathub.flatpakrepo`
- Then building and installing the USDX flatpak is just a matter of
  * `flatpak-builder --user --install-deps-from=flathub --install build manifest-dir/eu.usdx.UltraStarDeluxe.yaml`
- The `.flatpak-builder` and `build` directories can be removed afterwards.
- Songs must be placed in `~/.var/app/eu.usdx.UltraStarDeluxe/.ultrastardx/songs`

# Windows installer
The CI does this for you, but if you need to do it manually:
- Complete the set of DLLs in the `game` directory using a matching release from [here](https://github.com/UltraStar-Deluxe/mxe).
- Create Windows portable version: zip the contents of the `game` directory
- Create Windows installer:
  * Install NSIS (also install the Graphics and Language components during setup)
  * Copy the DLLs from `game` to `installer/dependencies/dll`
  * `C:\...\makensis "installer/UltraStar Deluxe.nsi"` (this will take a while)
  * The .exe will be placed in `installer/dist`
