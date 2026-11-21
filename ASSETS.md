# Assets: sources and licenses

The UltraStar Deluxe code is licensed under GPL-2.0-or-later (see [LICENSE](LICENSE)). The licenses of bundled third-party libraries are in `game/LICENSE.*.txt`.

This file lists the non-code assets in `game/` with their source and license, as far as they are known. Entries marked **unknown** have no recorded source. If you know where one of them comes from, please open an issue or pull request.

Commit hashes refer to this repository or, for the SourceForge SVN era (2007–2016), to the [legacy SVN mirror](https://github.com/UltraStar-Deluxe/legacy-sourceforge-svn-mirror).

## Fonts (`game/fonts/`)

| Files | Source | License |
|---|---|---|
| `DejaVu/` | [DejaVu fonts](https://dejavu-fonts.github.io/) | Bitstream Vera license; DejaVu changes are public domain (see `DejaVu/LICENSE`) |
| `FreeSans/` | [GNU FreeFont](https://www.gnu.org/software/freefont/) | GPL-3.0-or-later with font exception (see `FreeSans/COPYING`, `FreeSans/README`) |
| `NotoSans/` | [Go Noto Universal](https://github.com/satbyy/go-noto-universal), based on Google Noto fonts | SIL Open Font License 1.1 (see `NotoSans/LICENSE_OFL.txt`) |

## Sounds (`game/sounds/`)

| Files | Source | License |
|---|---|---|
| `background track.mp3` | DJad, "Walking Alone", via Jamendo (http://www.jamendo.com/de/track/543970). Added to SVN trunk on 2010-10-08 by canni0 as the winner of a background-track vote (SVN mirror commit 1d9027ed, r2644). | CC BY-SA 3.0 (see `background track license.txt`) |
| `dismissed.mp3` | **unknown**. Added to SVN trunk on 2007-05-17 by mota23 ("Added: dismissed sound for party-mode", SVN mirror commit fb785ffe, r202) | **unknown** |
| `Common back.mp3`, `Common start.mp3`, `menu swoosh.mp3`, `option change col.mp3`, `rimshot022b.wav`, `select music change music.mp3`, `select music change music 50.mp3` | **unknown**. Added to SVN branch `1.01` on 2007-10-09 by jaybinks ("these are needed to do a full build.", SVN mirror commit 0a944483, r486). `rimshot022b.wav` replaced `rimshot022b.mp3` in 2025. | **unknown** |
| `Applause.mp3` | **unknown**. Added to SVN branch `experimental` on 2009-12-09 by brunzelchen ("added missing file", SVN mirror commit 5196a888, r2012) | **unknown** |

## Theme graphics (`game/themes/`)

| Files | Source | License |
|---|---|---|
| `Modern/[score]rating_0.png` | [Face-monkey.svg](https://commons.wikimedia.org/wiki/File:Face-monkey.svg) by the Tango! Desktop Project, via Wikimedia Commons. Converted to PNG for USDX (2018). | Public domain |
| `Modern/[score]rating_1.png` … `rating_7.png` | GNOME Project emote icons (GNOME icon theme version 3), via Wikimedia Commons: 1 = [Gnome3-wink](https://commons.wikimedia.org/wiki/File:Gnome3-wink.svg), 2 = [Gnome3-glasses](https://commons.wikimedia.org/wiki/File:Gnome3-glasses.svg), 3 = [Gnome3-smile](https://commons.wikimedia.org/wiki/File:Gnome3-smile.svg), 4 = [Gnome3-smile-big](https://commons.wikimedia.org/wiki/File:Gnome3-smile-big.svg), 5 = [Gnome3-laugh](https://commons.wikimedia.org/wiki/File:Gnome3-laugh.svg), 6 = [Gnome3-surprise](https://commons.wikimedia.org/wiki/File:Gnome3-surprise.svg), 7 = [Gnome3-devilish](https://commons.wikimedia.org/wiki/File:Gnome3-devilish.svg). Converted to PNG for USDX (2018). | Wikimedia Commons lists two licenses: [CC BY-SA 3.0 US](https://creativecommons.org/licenses/by-sa/3.0/us/) and [LGPL-3.0-or-later](https://www.gnu.org/licenses/lgpl-3.0.html). The PNG versions are distributed under the same licenses. Attribution: "GNOME Project". |

## Avatars (`game/avatars/`)

| Files | Source | License |
|---|---|---|
| `female1.jpg` … `female10.jpg`, `male1.jpg` … `male10.jpg` | **unknown**. Not in the SourceForge SVN history; first appear in the git import of 2015-11-13 (commit c4fc4aaf, "codebase of USDX V1.2 trunk (alpha)") | **unknown** |

## Visualizer presets (`game/visuals/projectM/presets/`)

| Files | Source | License |
|---|---|---|
| `*.milk` (about 250 files) | MilkDrop presets by community authors (Rovastar, Aderrasi, Geiss, Fvese, Krash and others; the author is part of each file name) | No license statement in the files |

## About files without a recorded source

Many files were added by project members to the SourceForge SVN repository (2007–2016) or to this git repository without a note on where they came from. Adding a file to the project does not by itself settle its license:

- If the person who added a file made it themselves, they most likely meant it to be distributed as part of UltraStar Deluxe. Under which license exactly was not recorded.
- If the file came from somewhere else (e.g. a sample collection, another program or a website), adding it to the project cannot license it. Only the rights holder can do that. The same applies to contributions on GitHub: they fall under the repository's license (GitHub Terms of Service, section D.6), but only as far as the contributor holds the rights.

This is why this file only names a license where one is documented. If you added one of the files marked **unknown**, or know where it comes from, please tell us in an issue or pull request, ideally with a short statement whether you made it yourself and under which license it may be used.
