#!/usr/bin/python3

# UltraStar Deluxe - Karaoke Game
#
# UltraStar Deluxe is the legal property of its developers, whose names
# are too numerous to list here. Please refer to the COPYRIGHT
# file distributed with this source distribution.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING. If not, write to
# the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

from __future__ import with_statement, print_function

import re
import sys
import os
import codecs

# buffer english file
english = []
with open("English.ini", "r", encoding="utf8") as f:
	for line in f:
		english.append(line.strip())

transPattern = re.compile("\s*(\w+)\s*=(.+)$")

	
def update(lang):
	print("\nUpdate " +  lang)

	# buffer translation file
	translation = []
	with open(lang, "r", encoding="utf8") as f:
		for line in f:
			translation.append(line.strip())

	outList = []
	# find new fields
	for line in english:
		# header
		if re.search("\[Text\]", line, re.I):
			outList.append("[Text]")
			continue
		# ignore help text sections for now
		if re.match("\s*\[", line):
			break
		# ignore comments
		elif re.match("\s*[;#]", line):
			continue
		# copy empty lines
		elif re.match("\s*$", line):
			outList.append("")
			continue
		m = transPattern.match(line)
		if (not m):
			print("Invalid line: " + line)
			sys.exit(1)
		untranslated = True
		for transline in translation:
			m2 = re.match("\s*" + m.group(1) + "\s*=(.+)$", transline)
			if (m2):
				outList.append(m.group(1) + "=" + m2.group(1))
				untranslated = False
				break
		if (untranslated):
			print("  +" + m.group(1))
			outList.append(";TODO: " + line)

	# find unsupported (not in English.ini) translations
	for line in translation:
		# ignore header
		if re.search("\[Text\]", line, re.I):
			continue
		# ignore TODOs
		if re.match(";TODO:", line):
			continue
		# copy comments
		elif re.match("\s*[;#]", line):
			outList.append(line)
			continue
		# ignore empty line
		elif re.match("\s*$", line):
			continue
		m = transPattern.match(line)
		if (not m):
			print("  -" + line)
			outList.append(";INVALID: " + line)
			continue
		# check if field is in English.ini
		unsupported = True
		for orig in english:
			m2 = re.match("\s*" + m.group(1) + "\s*=(.+)$", orig)
			# ignore translated lines (already written in first pass)
			if (m2):
				unsupported = False
				break
		# unsupported translation
		if (unsupported):
			print("  -" + m.group(1))
			outList.append(";UNUSED: " + m.group(1) + "=" + m.group(2))

	# check if file changed
	changed = False
	if (len(outList) != len(translation)):
		changed = True
	else:
		# search for a changed line
		for i in range(len(outList)):
			if (outList[i] != translation[i]):
				changed = True
				break

	# write changes
	if changed:
		# create a backup first
		oldLang = lang + ".bak"
		if (os.path.exists(oldLang)):
			os.remove(oldLang)
		os.rename(lang, oldLang)

		with open(lang, 'wb') as f:
			f.write(codecs.BOM_UTF8)
		with open(lang, 'a', encoding='utf-8', newline='\n') as f:
			for line in outList:
				f.write(line + "\n")

if len(sys.argv) >= 2:
	# update specific language file passed as command-line argument
	update(sys.argv[1])
else:
	# update all language (ini) files
	iniList=os.listdir(".")
	for ini in iniList: 
		if not re.search(".ini$", ini):
			continue
		if ini == "English.ini":
			continue
		update(ini);

	# update template (do not use an .ini prefix as USDX would load it)
	update("Language.new");
