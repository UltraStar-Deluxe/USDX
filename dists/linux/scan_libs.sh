#!/usr/bin/env bash

set -eo pipefail

if ! tput setaf 1 || ! tput bold || ! tput sgr0 ; then
	tput() { return 0 ; }
fi >/dev/null 2>/dev/null

# TODO: Use list from
# https://raw.githubusercontent.com/probonopd/AppImages/master/excludelist

systemlibs=(
	'ld-linux[\.|\-]'
	'libc\.'
	'libm\.'
	'libdl\.'
	'libcrypt\.'
	'libmvec\.'
	'libpthread\.'
	'librt\.'
	'libutil\.'
)

skiplibs=(
	'libX11\.'
	'libasound\.'
	'libgcc_s\.'
	'libstdc\+\+\.'
)

inputfile="$1"
libdir="$2"
extralibs="$3"

libcache=()

scan_libs() {
	if [ ! -f "$1" ]; then return; fi
	local libs=$(readelf -d "$1" | sed -n 's/^.*(NEEDED)\s*Shared library: \[\(.*\)\]$/\1/p' | grep -E -vf <(IFS=$'\n'; printf "${systemlibs[*]}"))
	if [ -z "$libs" ]; then return; fi # no needed libs, so don't check ldd

	local lddoutput="$(ldd "$1")"
	local indent="$2"
	local IFS=$'\n'
	local lines=$(echo -n "$libs" | grep -c '^')
	local counter=0
	while read -r file
	do
		let counter=counter+1
		if [ $counter -eq $lines ]; then
			local indentstyle="└── "
			local nextindent="    "
		else
			local indentstyle="├── "
			local nextindent="│   "
		fi
		if [ -z "$file" ]; then continue; fi

		local includelib=$(echo "$file" | grep -E -vf <(IFS=$'\n'; printf "${skiplibs[*]}"))
		if [ -z "$includelib" ]; then
			echo -n "$indent$indentstyle"
			tput setaf 3
			echo -n "$file (skipped)"
			tput sgr0
			echo
			continue
		fi

		local filepath="$(realpath -s "$(echo "$lddoutput" | grep -F "$file" | head -n1 | awk '{print $3}')" || true)"

		if [ -f "$filepath" ]; then
			echo -n "$indent$indentstyle"
			local glibcversion=""
			if [[ ! "${libcache[@]}" =~ "$filepath" ]]; then
				# only copy if not already copied
				cp -u "$filepath" -t "$libdir/"
				strip -s "$libdir/$file" || true
				patchelf --set-rpath '$ORIGIN' "$libdir/$file"
				glibcversion=" ($(readelf -s "$filepath" | sed -n 's/^.*\(GLIBC_[.0-9]*\).*$/\1/p' | sort -u --version-sort | tail -n1))"
				tput setaf 2
			fi
			echo -n "$file"

			tput sgr0
			echo -n "$glibcversion"

			libcache+=("$filepath")
			if [[ "$3" == *"[$file]"* ]]; then
				echo " (recursive)"
			else
				echo
				scan_libs "$filepath" "$indent$nextindent" "[$file]$3"
			fi
		else
			>&2 tput setaf 1 && tput bold
			>&2 echo "==> Error: Library $file not found (required by $(basename "$1"))"
			>&2 tput sgr0
			return 1
		fi
	done <<< "$libs"
}

basename "$inputfile"
scan_libs "$inputfile"

# handle extras
IFS=' '
while read -r filepath
do
	if [ -z "$filepath" ]; then continue; fi
	file="$(basename "$filepath")"
	filepath="$(realpath -s "$filepath" || echo "$filepath")"
	if [ -f "$filepath" ]; then
		echo "$file"
		if [[ ! "${libcache[@]}" =~ "$filepath" ]]; then
			cp -u "$filepath" -t "$libdir/"
			strip -s "$libdir/$file" || true
			patchelf --set-rpath '$ORIGIN' "$libdir/$file"
		fi
		libcache+=("$filepath")
		scan_libs "$filepath"
	else
		>&2 tput setaf 1 && tput bold
		>&2 echo "==> Error: Library $file not found"
		>&2 tput sgr0
		exit 1
	fi
done <<< "$extralibs"

tput setaf 5 && tput bold
echo -n "==> Minimum GLIBC version: "
tput sgr0
tput bold
readelf -s "$inputfile" "$libdir/"*.so* | sed -n 's/^.*\(GLIBC_[.0-9]*\).*$/\1/p' | sort -u --version-sort | tail -n1
tput sgr0
