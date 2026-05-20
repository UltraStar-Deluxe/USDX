#!/usr/bin/python3

from __future__ import print_function

import re
import sys
import os
import time

# Pattern for key=value lines (accept any key chars, allow empty value)
transPattern = re.compile(r"\s*([^=]+?)\s*=(.*)$")


def read_lines(path):
    with open(path, 'r', encoding='utf8') as f:
        return f.read().splitlines()


def parse_blocks(lines):
    """Parse lines into blocks: ('raw', line) or ('keyblock', section, key, [lines])
    Continuation lines are any subsequent physical lines that are not a section header,
    comment, blank, or another key=value line.
    """
    blocks = []
    cur_section = None
    i = 0
    while i < len(lines):
        line = lines[i]
        if re.match(r"\s*\[.*\]", line):
            cur_section = line.strip()
            blocks.append(('raw', line))
            i += 1
            continue
        if re.match(r"\s*[;#]", line) or re.match(r"\s*$", line):
            blocks.append(('raw', line))
            i += 1
            continue
        m = transPattern.match(line)
        if m:
            key = m.group(1).strip()
            blk_lines = [line]
            j = i + 1
            while j < len(lines):
                nxt = lines[j]
                if re.match(r"\s*\[.*\]", nxt) or re.match(r"\s*[;#]", nxt) or transPattern.match(nxt):
                    break
                blk_lines.append(nxt)
                j += 1
            blocks.append(('keyblock', cur_section, key, blk_lines))
            i = j
        else:
            blocks.append(('raw', line))
            i += 1
    return blocks


def parse_translation_blocks(lines):
    """Return mapping (section,key) -> list_of_physical_lines for translation file."""
    trans = {}
    cur_section = None
    i = 0
    while i < len(lines):
        line = lines[i]
        if re.match(r"\s*\[.*\]", line):
            cur_section = line.strip()
            i += 1
            continue
        if re.match(r"\s*[;#]", line) or re.match(r"\s*$", line):
            i += 1
            continue
        m = transPattern.match(line)
        if m:
            key = m.group(1).strip()
            blk_lines = [line]
            j = i + 1
            while j < len(lines):
                nxt = lines[j]
                if re.match(r"\s*\[.*\]", nxt) or re.match(r"\s*[;#]", nxt) or transPattern.match(nxt):
                    break
                blk_lines.append(nxt)
                j += 1
            trans[(cur_section, key)] = blk_lines
            i = j
        else:
            i += 1
    return trans


def update(lang, dry_run=False):
    print('\nUpdate', lang)
    english_lines = read_lines('English.ini')
    translation_lines = read_lines(lang)

    eng_blocks = parse_blocks(english_lines)
    trans_map = parse_translation_blocks(translation_lines)

    out_lines = []
    used = set()

    for blk in eng_blocks:
        if blk[0] == 'raw':
            out_lines.append(blk[1])
            continue
        _, section, key, eng_blk_lines = blk
        tkey = (section, key)
        if tkey in trans_map:
            used.add(tkey)
            tr_lines = trans_map[tkey]
            # output same number of physical lines as eng block
            for idx in range(len(eng_blk_lines)):
                if idx < len(tr_lines):
                    if idx == 0:
                        # preserve left-hand formatting from English first line
                        m_en = re.match(r'^(\s*[^=]+?\s*=\s*)(.*)$', eng_blk_lines[0])
                        lhs = m_en.group(1) if m_en else (key + '=')
                        # get RHS from translation line (strip key= if present)
                        m_tr = transPattern.match(tr_lines[0])
                        rhs = m_tr.group(2) if m_tr else tr_lines[0]
                        out_lines.append(lhs + rhs)
                    else:
                        out_lines.append(tr_lines[idx])
                else:
                    # missing translated continuation -> mark TODO with English continuation
                    out_lines.append(';TODO: ' + eng_blk_lines[idx])
        else:
            # no translation for this keyblock -> output TODO for each english physical line
            for line in eng_blk_lines:
                out_lines.append(';TODO: ' + line)

    # detect unused translations
    unused = []
    for (sec, key), lines in trans_map.items():
        if (sec, key) not in used:
            unused.append((sec, key, lines))

    # compare and write
    changed = (out_lines != translation_lines)
    if changed:
        if dry_run:
            print('Dry-run: would update', lang, '({} lines -> {} lines)'.format(len(translation_lines), len(out_lines)))
            if unused:
                print('  Unused translations:', len(unused))
            return out_lines, unused
        # backup existing file
        stamp = time.strftime('%Y%m%d%H%M%S')
        bak = lang + '.' + stamp + '.bak'
        os.rename(lang, bak)
        with open(lang, 'w', encoding='utf-8', newline='\n') as f:
            for l in out_lines:
                f.write(l + '\n')
        print('Wrote', lang, 'backup->', bak)
        if unused:
            unused_file = lang + '.unused'
            with open(unused_file, 'w', encoding='utf-8', newline='\n') as f:
                for sec, key, lines in unused:
                    f.write('; Section: %s\n' % (sec or '<global>'))
                    for ln in lines:
                        f.write('; %s\n' % ln)
            print('Wrote unused translations to', unused_file)
    else:
        print('No changes for', lang)
    return out_lines, unused


if __name__ == '__main__':
    dry = False
    args = sys.argv[1:]
    if '--dry-run' in args:
        dry = True
        args.remove('--dry-run')
    dump = False
    if '--dump' in args:
        dump = True
        args.remove('--dump')
    if args:
        out, unused = update(args[0], dry_run=dry)
        if dump:
            for l in out:
                print(l)
    else:
        # run on all .ini except English.ini
        for fn in os.listdir('.'):
            if not fn.endswith('.ini'):
                continue
            if fn == 'English.ini':
                continue
            update(fn, dry_run=dry)
