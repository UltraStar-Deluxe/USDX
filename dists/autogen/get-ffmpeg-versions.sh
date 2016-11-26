#!/bin/sh

DIR=$(dirname $0)/../../
cd "$DIR/src/lib/"

FFMPEG_VERSIONS=$(ls -d1 ffmpeg* | sort -rV)

for version in $FFMPEG_VERSIONS; do

	grep -e 'LIBAVUTIL_\(MIN\|MAX\)_VERSION_.*=' $version/avutil.pas | tr -d '\n' | \
		sed -e 's/ *LIBAVUTIL_\(MIN\|MAX\)_VERSION_MAJOR *= *\([0-9]*\);.*\1_VERSION_MINOR *= *\([0-9]*\);.*\1_VERSION_RELEASE *= *\([0-9]*\);/\1 \2 \3 \4\n/g' | \
                {
			echo -n $version
			while read mode major minor release ; do
				printf " %s %02d%03d%03d " "$mode" "$major" "$minor" "$release"
			done
			echo
		}
done | \
sed \
	-e 's/MIN/$libavutil_VERSION_INT -ge/' \
	-e 's/MAX/$libavutil_VERSION_INT -le/' \
	-e 's/-le \([0-9]*\) .* -ge \1/-eq \1/' \
	-e 's/^ffmpeg-\([0-9.]*\) \(.*\) /elif test \2; then\n    FFMPEG_VERSION="\1"/' \
	-e 's/^ffmpeg \(.*\) /elif test \1; then\n    FFMPEG_VERSION="0"/' \
	-e 's/\([0-9]\) *$libavutil/\1 -a $libavutil/' \
	-e '1s/^elif/if  /' \
        -e 's/libavutil_VERSION_INT/[$1][_VERSION_INT]/g' \
	-e 's/^/        /gm'
