#!/usr/bin/env bash

set -eo pipefail

systemlibs=(
	'ld-linux[\.|\-]'
	'libc\.'
	'libm\.'
	'libdl\.'
	'libcrypt\.'
	'libmvec\.'
	'libnsl\.'
	'libpthread\.'
	'librt\.'
	'libutil\.'
)

skiplibs=(
	'libX11\.'
	'libgcc_s\.'
	'libglib\.'
	'libstdc\+\+\.'
)

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

		local filepath="$(realpath -esq "$(echo "$lddoutput" | grep -F "$file" | head -n1 | awk '{print $3}')" || true)"

		local includelib=$(echo "$file" | grep -E -vf <(IFS=$'\n'; printf "${skiplibs[*]}"))
		if [ -z "$includelib" ]; then
			echo -n "$indent$indentstyle"
			tput setaf 3
			echo "$file (skipped)"
			tput sgr0
			continue
		fi

		if [ -f "$filepath" ]; then
			echo -n "$indent$indentstyle"
			if [[ ! "${libcache[@]}" =~ "$filepath" ]]; then
				# only copy if not already copied
				cp "$filepath" "$libdir/"
				tput setaf 2 && tput bold
			fi
			echo "$file"
			tput sgr0

			libcache+=("$filepath")
			scan_libs "$filepath" "$nextindent$indent"
		else
			tput setaf 1 && tput bold
			echo "==> Error: $file not found"
			tput sgr0
			return 1
		fi
	done <<< "$libs"
}


basename "$1"
scan_libs "$1"

# handle extras
IFS=' '
while read -r filepath
do
	if [ -z "$filepath" ]; then continue; fi
	file="$(basename "$filepath")"
	filepath="$(realpath -esq "$filepath" || echo "$filepath")"
	if [ -f "$filepath" ]; then
		echo "$file"
		if [[ ! "${libcache[@]}" =~ "$filepath" ]]; then
			cp "$filepath" "$libdir/"
		fi
		libcache+=("$filepath")
		scan_libs "$filepath"
	else
		tput setaf 1 && tput bold
		echo "==> Error: $file not found"
		tput sgr0
		exit 1
	fi
done <<< "$extralibs"

