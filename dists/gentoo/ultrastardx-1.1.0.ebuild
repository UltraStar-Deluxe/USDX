# Copyright 1999-2008 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header $

inherit eutils games

SONGS_PKG=USDX-SongPackage
SONGS_VER=01

DESCRIPTION="An open-source karaoke game"
HOMEPAGE="http://www.ultrastardeluxe.org/"
SRC_URI="mirror://sourceforge/${PN}/${P}-src.tar.gz
	songs? ( mirror://sourceforge/${PN}/${SONGS_PKG}-${SONGS_VER}.zip )"

LICENSE="GPL-2
	songs? (
		CCPL-Attribution-ShareAlike-NonCommercial-2.5
		CCPL-Attribution-NonCommercial-NoDerivs-2.5
	)"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="projectm debug songs"

RDEPEND="virtual/opengl
	virtual/glu
	media-libs/libsdl
	media-libs/sdl-image
	media-libs/freetype
	media-libs/libpng
	=media-libs/portaudio-19*
	media-video/ffmpeg
	dev-db/sqlite
	projectm? ( media-libs/libprojectm )"
DEPEND="${RDEPEND}
	dev-util/pkgconfig
	>=dev-lang/fpc-2.2.0"

S=${WORKDIR}/${P}-src

pkg_setup() {
    games_pkg_setup
    built_with_use media-libs/libsdl opengl \
        || die "You need to compile media-libs/libsdl with USE=opengl."
}

src_compile() {
	egamesconf \
		$(use_with projectm libprojectM) \
		$(use_enable debug) \
		|| die
	emake || die "emake failed"
}

src_install() {
	emake DESTDIR="${D}" install || die "emake install failed"

	if use songs; then
		insinto "${GAMES_DATADIR}"/${PN}/songs
		doins -r ${WORKDIR}/Songs/* || die "doins songs failed"
	fi

	dodoc AUTHORS.txt ChangeLog.german.txt ChangeLog.txt README.txt

	doicon icons/${PN}-icon.svg
	make_desktop_entry ${PN} "UltraStar Deluxe"

	prepgamesdirs
}
