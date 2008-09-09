---------------------------------------
1. Introduction
---------------------------------------
This directory contains two ebuilds for UltraStar Deluxe
- ultrastardx-9999.ebuild: a live ebuild using SVN sources
- ultrastardx-1.1_alpha.ebuild: a snapshot ebuild that might be appended to the official portage someday (Note: at the moment there is no source snapshot for USDX so this will not work)

---------------------------------------
2. Create a portage overlay
---------------------------------------
If you want to try one of the ebuilds (at the moment only use the live ebuild) you must have a portage overlay.
In case you do not have one or do not know what it is, see
  http://gentoo-wiki.com/HOWTO_Create_an_Updated_Ebuild

Normally this can be achieved (as root) with:
  mkdir -p /usr/local/portage && echo 'PORTDIR_OVERLAY="/usr/local/portage"' >> /etc/make.conf

---------------------------------------
3. Add the USDX ebuild to your overlay
---------------------------------------
First create the directory structure (as root):
  mkdir -p /usr/local/portage/games-arcade/ultrastardx

Now copy the (live) ebuild to the new directory:
  cp ultrastardx-9999.ebuild /usr/local/portage/games-arcade/ultrastardx

Go to the overlay directory:
  cd /usr/local/portage/games-arcade/ultrastardx

Create a manifest:
  ebuild ultrastardx-9999.ebuild manifest

And you are done!

Now you can emerge USDX with:
  emerge ultrastardx -av
