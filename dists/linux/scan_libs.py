#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import subprocess
import re
import os.path
import shutil


system_libs = [
    'ld-linux.so.2',
    'ld-linux-x86-64.so.2',
    'libanl.so.1',
    'libc.so.6',
    'libdl.so.2',
    'libm.so.6',
    'libmvec.so.1',
    'libnss_compat.so.2',
    'libnss_db.so.2',
    'libnss_dns.so.2',
    'libnss_files.so.2',
    'libnss_hesiod.so.2',
    'libnss_nisplus.so.2',
    'libnss_nis.so.2',
    'libpthread.so.0',
    'libresolv.so.2',
    'librt.so.1',
    'libthread_db.so.1',
    'libutil.so.1'
]

skip_libs = [
    'libGL.so.1',
    'libX11.so.6',
    'libasound.so.2',
    'libstdc++.so.6',
    'libgcc_s.so.1',
    'libz.so.1',
    'libfreetype.so.6'
]

re_match_readelf_dynamic = re.compile(br'0x[0-9a-f]+\s+\((NEEDED|RPATH)\).*: \[(.*)\]$')
re_match_readelf_symbols = re.compile(br'[0-9]+: [0-9a-f]+\s+[0-9]+\s+.+?\s+.+?\s+.+?\s+.+?\s+(.*?)( \([0-9]+\))?$')
re_match_glibc = re.compile(r'.*?@GLIBC_([0-9].+)')
re_match_ldd = re.compile(br'(.*) => (.*) \(0x[0-9a-f]*\)$')
re_match_ldd_not_found = re.compile(br'(.*) => not found$')
re_match_ldd_version = re.compile(br'ldd \((.*?)\) (.*)$')

file_cache = {}


def ansicolor(*arg):
    global args
    if args.no_color:
        return ''
    return '\033[' + ';'.join(map(str, arg or [0])) + 'm'


def file_exists(filepath):
    try:
        return os.path.isfile(filepath)
    except Exception:
        return False


def readelf_dynamic(filename, filter_libs=[]):
    libs = []
    p = subprocess.Popen(["readelf", "-d", filename], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result = p.stdout.readlines()

    for line in result:
        m = re_match_readelf_dynamic.match(line.strip())
        if m:
            key = str(m.group(1).decode())
            value = str(m.group(2).decode())
            if key == 'NEEDED' and value not in system_libs:
                libs.append(value)
            # elif key == 'RPATH':
            #     print(ansicolor(33) + filename, 'rpath:', value + ansicolor())
    return libs


def readelf_symbols(filename, filter_symbols=None):
    symbols = []
    p = subprocess.Popen(["readelf", "-s", filename], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result = p.stdout.readlines()

    for line in result:
        m = re_match_readelf_symbols.match(line.strip())
        if m:
            symbol = str(m.group(1).decode())
            if filter_symbols:
                m = filter_symbols.match(symbol)
                if m:
                    symbol = m.group(1)
                else:
                    symbol = None
            if symbol and symbol not in symbols:
                symbols.append(symbol)
    return symbols


def get_minimum_glibc_version(filepath):
    global args
    if args.no_glibc_check:
        return None
    symbols = readelf_symbols(filepath, re_match_glibc)
    symbols.sort(key=lambda s: list(map(int, list(filter(None, s.split('.'))))))
    return symbols[-1] if symbols else None


def ldd(filenames):
    libs = {}
    p = subprocess.Popen(["ldd"] + filenames, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    result = p.stdout.readlines()

    for line in result:
        line = line.strip()
        m = re_match_ldd.match(line)
        if m:
            libs[str(m.group(1).decode())] = str(m.group(2).decode())
        else:
            # libs that weren't found
            m = re_match_ldd_not_found.match(line)
            if m:
                libs[str(m.group(1).decode())] = None

    return libs


def ldd_version():
    version = None
    p = subprocess.Popen(["ldd", "--version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result = p.stdout.readlines()
    for line in result:
        m = re_match_ldd_version.match(line.strip())
        if m:
            version = str(m.group(2).decode())
        break
    return version


def find_dependencies(file, parent_lib=None, parent_libs=[]):
    # walk the dependency tree recursively
    # returns a list tree or False in case a dependency is not found

    tree = []

    if file not in file_cache:
        file_cache[file] = (ldd([file]), readelf_dynamic(file, system_libs), get_minimum_glibc_version(file))

    lddoutput, neededlibs, glibc_version = file_cache.get(file)

    for neededlib in neededlibs:
        libpath = lddoutput.get(neededlib)

        if not libpath or not file_exists(libpath):
            print(ansicolor(1, 31) + "==> Error: Library " + neededlib + " not found (required by " + (parent_lib or file) + ")" + ansicolor())
            return False
        libpath = os.path.abspath(libpath)

        if neededlib in parent_libs:
            tree.append((libpath, neededlib, 'circular', None))
            continue

        if neededlib in skip_libs:
            tree.append((libpath, neededlib, 'skipped', None))
            continue

        result = find_dependencies(libpath, neededlib, parent_libs + [neededlib])
        if result is False:
            return False
        tree.append(result)

    return (file, parent_lib, glibc_version, tree)


def print_dependency_tree(item, is_last=[], unique_libs=[]):
    indent = ''
    line = '' + ansicolor(37)
    levels = len(is_last)

    for counter, last in enumerate(is_last):
        if counter == levels - 1:
            if last:
                indent += '└── '
            else:
                indent += '├── '
        else:
            if last:
                indent += '    '
            else:
                indent += '│   '
    if indent:
        line += '  ' + indent
    else:
        line += '> '
    line += ansicolor(0)

    libname = item[1]
    if item[2] == 'skipped':
        line += ansicolor(33) + libname + ansicolor(2) + ' (skipped)' + ansicolor()
    elif item[2] == 'circular':
        line += ansicolor(36) + libname + ansicolor(2) + ' (circular dependency)' + ansicolor()
    else:
        if libname not in unique_libs:
            line += ansicolor(0, 32)
        line += libname
        if libname not in unique_libs:
            line += ansicolor(0)
            if item[2]:
                line += ' '
                line += ansicolor(2, 32)
                line += '(GLIBC_' + item[2] + ')'
                line += ansicolor(0)

    if libname not in unique_libs:
        unique_libs.append(libname)

    print(line)

    if item[3]:
        total = len(item[3])
        for counter, subitem in enumerate(item[3]):
            print_dependency_tree(subitem, is_last + [counter == total - 1], unique_libs)


def print_glibc_version(item, cache={}, level=0):
    filename = item[0]
    libname = item[1]

    if item[2] != 'skipped' and item[2] != 'circular':
        if filename not in cache:
            version = item[2]
            cache[filename] = (version, libname, level)
        elif level < cache[filename][2]:
            info = cache[filename]
            cache[filename] = (info[0], info[1], level)

    if item[3]:
        for subitem in item[3]:
            print_glibc_version(subitem, cache, level + 1)

    if level == 0:
        sorted_versions = sorted([s for s in cache.items() if s[1][0]], key=lambda s: list(map(int, s[1][0].split('.'))))
        if sorted_versions:
            minimum_version = sorted_versions[-1][1][0]
            print(ansicolor(1, 35) + '==> Minimum GLIBC version:' + ansicolor(0, 1) + ' ' + minimum_version + ansicolor())


def copy_libraries(tree, outdir, strip, rpath):
    liblist = {}

    def walk(item):
        if item[2] != 'skipped' and item[2] != 'circular':
            if not item[0] in liblist:
                liblist[item[0]] = item[1]
                if item[3]:
                    for subitem in item[3]:
                        walk(subitem)

    for item in tree[3]:
        walk(item)
    print('==> Copying ' + str(len(liblist)) + ' libraries to ' + outdir)
    for src, libname in liblist.items():
        dest = os.path.join(outdir, os.path.basename(src))
        if file_exists(dest) and os.path.samefile(src, dest):
            print(ansicolor(33) + 'Already exists', src, dest + ansicolor())
            continue
        shutil.copy(src, outdir)
        if strip:
            subprocess.call(['strip', '-s', dest], stdout=sys.stdout.fileno(), stderr=sys.stderr.fileno())
        if rpath:
            subprocess.call(['patchelf', '--set-rpath', '$ORIGIN', dest], stdout=sys.stdout.fileno(), stderr=sys.stderr.fileno())


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description='Scans the library dependencies of an executable and copies them to a directory.')
    parser.add_argument('input_executable', help='Executable to scan')
    parser.add_argument('output_directory', help='Where to copy libraries')
    parser.add_argument('--dry-run', action='store_true', help='Don\'t do any file operations')
    parser.add_argument('--strip', action='store_true', help='Strip debug symbols of copied libs (recommended for release)')
    parser.add_argument('--rpath', action='store_true', help='Change rpath to $ORIGIN of copied libs (recommended, requires patchelf)')
    parser.add_argument('--no-glibc-check', action='store_true', help='Don\'t check for minimum GLIBC versions')
    parser.add_argument('--no-color', action='store_true', help='Disable color and bold in the output')
    parser.add_argument('--no-tree', action='store_true', help='Don\' output a library tree')
    parser.add_argument('--simple-list', action='store_true', help='Output a list of libraries instead of a tree')
    parser.add_argument('--libraries', nargs='+', help='Paths to additional libraries to be included', metavar='PATH')
    parser.add_argument('--system-libraries', nargs='+', help='Libraries that should be ignored by the scanner (e.g. libc)', metavar='LIBRARY')
    parser.add_argument('--skip-libraries', nargs='+', help='Libraries that should be highlighted by the scanner but ignored (libraries that are expected the user have available e.g. libX11)', metavar='LIBRARY')
    parser.add_argument('--output-treefile', type=str, help='Save a copy of the tree to file', metavar='FILENAME')
    args = parser.parse_args()

    filename = args.input_executable
    libdir = args.output_directory
    filename = os.path.abspath(filename)
    tree = find_dependencies(filename, os.path.basename(filename))
    file_cache.clear()
    if tree is False:
        exit(1)
    else:
        if not args.no_tree and not args.simple_list:
            print_dependency_tree(tree)
        elif args.simple_list:
            print('simple list')
        print_glibc_version(tree)
        if not args.dry_run and args.output_directory:
            if not os.path.isdir(args.output_directory):
                print(ansicolor(1, 31) + '==> Error: ' + args.output_directory + ' is not a directory' + ansicolor())
                exit(1)
            copy_libraries(tree, args.output_directory, strip=args.strip, rpath=args.rpath)
