#!/usr/bin/python

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
from collections import OrderedDict

import re
import sys
import os
import codecs

def xopen(file, mode, encoding):
	if sys.version_info[0] > 2:
		return open(file=file, mode=mode, encoding=encoding)
	else:
		return open(name=file, mode=mode)
		
def xwriteline(file, content):
	if sys.version_info[0] > 2:
		# line endings should be handled by the file writer in newer python version
		file.write(content + u"\n")
	else:
		# binary mode does not convert "\n" to the os specific line-ending.
		# Use os.linesep instead.
		file.write(content + os.linesep)
		
def xutf8header():
	if sys.version_info[0] > 2:
		return u'\ufeff';
	else:
		return codecs.BOM_UTF8;

def parse(lines, regpat):
	#print("Processing %s..." % (file))
	listk = []
	listv = []
	for line in lines:
		m = regpat.match(line)
		if (m): 
			listk.append(m.group('key'))
			listv.append(m.group('trans'))
			#print("Key: %s  Value: %s" % (m.group('key'), m.group('trans')))
		else:
			listk.append("")
			listv.append("")

	return listk, listv

# buffer english file (always open binary, handle newline uniformly as "\n")
english = []

#sys.exit(1)

with xopen("English.nsh", "rU", encoding="utf8") as f:
	for line in f:
		english.append(line.strip())

transPattern = re.compile('^\s*\$.*?\s+(?P<key>[\w_0-9]+)\s+"(?P<trans>.*)"$')
headerPattern = re.compile('!insertmacro\s+(LANG[\w_0-9]*)\s*(?P<trans>[\w_0-9]+)')
headerReplacement = '!insertmacro LANGFILE_EXT %s'
replacementPattern = '${LangFileString} %s "%s"'


englishkeys,englishtrans = parse(english, transPattern)
print("Source lines count: %i"  % (len(english)))
print("Source translations count: %i"  % (len(englishkeys)-englishkeys.count("")))
print("Source arrays: %i %i %i"  % (len(english), len(englishkeys), len(englishtrans)))

def update(lang, createortransfer = False, noremove = False):
	print("\nUpdate " +  lang)
	
	langname = lang
	m = re.match("(\w+)\.", lang)
	if (m): langname = m.group(1)

	# buffer translation file (always open binary, handle newline uniformly)
	langlines = []
	langcheck = []
	langkeys = []
	langtrans = []
	if os.path.exists(lang):
		with xopen(lang, "rU", encoding="utf8") as f:
			for line in f:
				stripped = line.strip()
				langlines.append(stripped)
				m = transPattern.match(stripped)
				if (m):
					langcheck.append(True)
					langkeys.append(m.group('key'))
					langtrans.append(m.group('trans'))
				else:
					langcheck.append(False)
					langkeys.append("")
					langtrans.append("")

	if createortransfer == True:
		with xopen(lang, 'w', encoding='utf-8') as f:
			n = len(english)
			headerset = False
			for i in range(0, n):
				key = englishkeys[i]
				if (key == ""):
					if not headerset and headerPattern.match(english[i]):
						xwriteline(f, headerReplacement % langname)
						headerset = True
					else:
						xwriteline(f, english[i])
				elif (key in langkeys):
					index = langkeys.index(key)
					newstring = replacementPattern % (key, langtrans[index]);
					xwriteline(f, newstring)
				else:
					print("key %s not in translation file" % (key))
					newstring = ";TODO " +replacementPattern % (key, "");
					xwriteline(f, newstring)
					
		return
		
	# create a backup first
	if os.path.exists(lang):
		oldLang = lang + ".bak"
		if (os.path.exists(oldLang)):
			os.remove(oldLang)
		os.rename(lang, oldLang)
	
	outList = []
	outKeys = []
	outTodo = []
	haskeys = False
	transindex = 0
	headerset = False
	
	# copy lines into arrays to sort new entries into
	# remove lines which do not exist in new language file
	n = len(langlines)
	for i in range(0, n):
		if re.match("\s*;\s*TODO", langlines[i]):
			print("Old TODO removed: %s" % (langlines[i]))
			continue
			
		key = langkeys[i]
		if key == "":
			outList.append(langlines[i])
			outKeys.append("")
			outTodo.append(False)
			continue
				
			
		if key not in englishkeys:
			if noremove:
				outList.append(langlines[i])
				outKeys.append("")
				outTodo.append(False)
				continue	
			else:
				print("Removed '%s' with: %s" % (key, langlines[i]))
				
				# check if previous entries should also be removed
				if len(outKeys) > 0 and (i>= n or langcheck[i+1] == False):
					w = len(outList)-1
					while w>=0:
						if (re.match("^\s*;", outList[w])) and haskeys: outList.pop() # is a comment
						elif (outKeys[w] == False or outList[w] == ""): outList.pop()
						else: break;
						w -= 1
			
		else:
			# find a previous key which also exists in the copied list
			index_insert = -1
			index_in_eng = englishkeys.index(key)
			
			w = index_in_eng-1
			while w >= 0:
				if englishkeys[w] == "": break # previous item wasn't a key, append to list
				elif englishkeys[w] in outKeys:
					index_insert = outKeys.index(englishkeys[w])
					break
				w -= 1
				
			if index_insert >= 0:
				outList.insert(index_insert+1, langlines[i])
				outKeys.insert(index_insert+1, langkeys[i])
				outTodo.insert(index_insert+1, False)
			else:
				outList.append(langlines[i])
				outKeys.append(langkeys[i])
				outTodo.append(False)
				
			haskeys = True

			
	# insert new entries into
	n = len(englishkeys)
	for i in range(0, n):
		key = englishkeys[i]
		if key == "": continue
		if key in outKeys: continue
		
		# find a previous key which also exists in the copied list
		index_insert = -1
		index_in_eng = englishkeys.index(key)
		
		w = index_in_eng-1
		while w >= 0:
			if englishkeys[w] == "": break # previous item wasn't a key, skip and search for next
			elif re.match("^\s*;", englishkeys[w]): break # is a comment
			elif englishkeys[w] in outKeys:
				index_insert = outKeys.index(englishkeys[w])
				break
			w -= 1
			
		if index_insert >= 0:
			outList.insert(index_insert+1, english[i])
			outKeys.insert(index_insert+1, key)
			outTodo.insert(index_insert+1, True)
		else:
			w = index_in_eng+1
			while w < len(outList):
				if englishkeys[w] == "": break # next item wasn't a key, skip. append to list
				elif re.match("^\s*;", outList[w]): break # is a comment
				elif englishkeys[w] in outKeys:
					index_insert = outKeys.index(englishkeys[w])+1
					break
				w += 1
			
			if index_insert >= 0:
				outList.insert(index_insert-1, english[i])
				outKeys.insert(index_insert-1, key)
				outTodo.insert(index_insert-1, True)
			
		if index_insert < 0:
			outList.append(english[i])
			outKeys.append(key)
			outTodo.append(True)
			
			langkeys = []
			
	print ("Lines %i"  % (len(langlines)))
	print ("Translations %i"  % (langcheck.count(True)))
	
	print ("Removed translations: %i"  % (0 if noremove else langcheck.count(True) - ((len(outTodo)-outKeys.count("")) - outTodo.count(True))))
	print ("New translations: %i"  % (outTodo.count(True)))
			
	with xopen(lang, 'w', encoding='utf-8') as f:
		n = len(outList)
		for i in range(0, n):
			if outTodo[i] == True:
				xwriteline(f, ";TODO "+outList[i])
			else:
				xwriteline(f, outList[i])

				
if len(sys.argv) >= 2:
	# update specific language file passed as command-line argument
	createortransfer = False
	if len(sys.argv) > 2: createortransfer = sys.argv[2];
	noremove = False
	if len(sys.argv) > 3: noremove = sys.argv[3];
	update(sys.argv[1], createortransfer, noremove)
else:
	# update all language (nsh) files
	nshList=os.listdir(".")
	for nsh in nshList: 
		if not re.search(".nsh$", nsh):
			continue
		if nsh == "English.nsh":
			continue
		update(nsh);

	# update template (do not use an .nsi prefix as NSIS might load it)
	update("Language.new");
