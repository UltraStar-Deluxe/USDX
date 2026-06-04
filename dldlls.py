#!/usr/bin/python3

import argparse
import json
import os
import posixpath
import shutil
import sys
import urllib.request
import zipfile

sha = 'bce8633438abaa66e19f1169f2503df86dca766c'
filename = 'usdx-dlls-x86_64'
archive_name = filename + '.zip'
urlbase = 'https://api.github.com/repos/UltraStar-Deluxe/mxe/'
headers = {
    'Accept': 'application/vnd.github+json',
    'X-GitHub-Api-Version': '2022-11-28'
    }

root_dir = os.path.dirname(os.path.abspath(__file__))
game_dir = os.path.join(root_dir, 'game')
src_dir = os.path.join(root_dir, 'src')
archive_path = os.path.join(root_dir, archive_name)
bass_zip_path = os.path.join(root_dir, 'bass24.zip')
bass_target = os.path.join(game_dir, 'bass.dll')
config_target = os.path.join(src_dir, 'config-win.inc')

required_game_dlls = [
    'SDL2.dll',
    'SDL2_image.dll',
    'avcodec-62.dll',
    'avformat-62.dll',
    'avutil-60.dll',
    'libdav1d.dll',
    'libdl.dll',
    'libfreetype-6.dll',
    'libgcc_s_seh-1.dll',
    'libjpeg-8.dll',
    'libopencv_core4130.dll',
    'libopencv_imgcodecs4130.dll',
    'libopencv_imgproc4130.dll',
    'libopencv_videoio4130.dll',
    'libpng16-16.dll',
    'libprojectM-0.dll',
    'libsharpyuv-0.dll',
    'libstdc++-6.dll',
    'libtiff-6.dll',
    'libwebp-7.dll',
    'libwinpthread-1.dll',
    'lua54.dll',
    'opencvwrapper.dll',
    'portaudio_x64.dll',
    'projectM-cwrapper.dll',
    'sqlite3.dll',
    'swresample-6.dll',
    'swscale-9.dll',
    'zlib1.dll',
]

token = os.environ.get('ARTIFACT_ACCESS_TOKEN')
if token == None or token.strip() == '':
    token = os.environ.get('GITHUB_TOKEN')
if token != None and token.strip() != '':
    headers['Authorization'] = 'Bearer ' + token

def ensure_dirs():
    os.makedirs(game_dir, exist_ok=True)
    os.makedirs(src_dir, exist_ok=True)


def copy_zip_member(zf, member, target):
    with zf.open(member) as src, open(target, 'wb') as dst:
        shutil.copyfileobj(src, dst)


def missing_runtime_files():
    missing = []
    if not os.path.isfile(config_target):
        missing.append(os.path.relpath(config_target, root_dir))
    for dll in required_game_dlls:
        target = os.path.join(game_dir, dll)
        if not os.path.isfile(target):
            missing.append(os.path.relpath(target, root_dir))
    if not os.path.isfile(bass_target):
        missing.append(os.path.relpath(bass_target, root_dir))
    return missing


def download_bass(force=False):
    ensure_dirs()
    bass_url = 'https://www.un4seen.com/files/bass24.zip'
    if not force and os.path.isfile(bass_target):
        print('Using existing ' + os.path.relpath(bass_target, root_dir))
        return
    with urllib.request.urlopen(bass_url) as dl, open(bass_zip_path, 'wb') as out:
        out.write(dl.read())
    with zipfile.ZipFile(bass_zip_path, 'r') as zf:
        member = 'x64/bass.dll'
        copy_zip_member(zf, member, bass_target)
    print('Downloaded ' + member + ' to ' + os.path.relpath(bass_target, root_dir))


def search_releases():
    pagesuffix = ''
    page = 1
    links = '"next"'
    while links != None and links.find('"next"') != -1:
        req = urllib.request.Request(url = urlbase + 'tags' + pagesuffix,
                                     headers = headers)
        rsp = urllib.request.urlopen(req)
        links = rsp.headers['link']
        for tag in json.load(rsp):
            if tag['commit']['sha'] == sha:
                req = urllib.request.Request(url = urlbase + 'releases/tags/'
                                                 + tag['name'],
                                             headers = headers)
                rsp = urllib.request.urlopen(req)
                release = json.load(rsp)
                for asset in release['assets']:
                    if asset['name'].startswith(filename):
                        print('Found binaries in release ' + release['name'])
                        headers.clear()
                        return asset['browser_download_url']
        page = page + 1
        pagesuffix = '?page={}'.format(page)
    print('No release matches')
    return None


def search_artifacts():
    pagesuffix = ''
    page = 1
    links = '"next"'
    while links != None and links.find('"next"') != -1:
        req = urllib.request.Request(url = urlbase
                                         + 'actions/artifacts'
                                         + '?name=' + filename
                                         + pagesuffix,
                                     headers = headers)
        rsp = urllib.request.urlopen(req)
        links = rsp.headers['link']
        for artifact in json.load(rsp)['artifacts']:
            if artifact['workflow_run']['head_sha'] == sha and not artifact['expired']:
                print('Found binaries in workflow run {}'.format(artifact['workflow_run']['id']))
                return artifact['archive_download_url']
        page = page + 1
        pagesuffix = '&page={}'.format(page)
    print('No workflow artifact matches')
    return None


def download_dll_archive(force=False):
    if not force and os.path.isfile(archive_path):
        print('Using existing ' + archive_name)
        return

    print('Searching for binaries built from commit ' + sha)
    dllurl = search_releases()
    if dllurl == None and token != None:
        dllurl = search_artifacts()
    if dllurl == None:
        sys.exit(1)
    print('Downloading from ' + dllurl)
    req = urllib.request.Request(url = dllurl)
    for key in headers:
        req.add_unredirected_header(key, headers[key])
    with urllib.request.urlopen(req) as response, open(archive_path, 'wb') as out:
        shutil.copyfileobj(response, out)


def extract_dll_archive():
    ensure_dirs()
    extracted = 0
    with zipfile.ZipFile(archive_path, 'r') as zf:
        for member in zf.infolist():
            filename_in_zip = member.filename.replace('\\', '/')
            basename = posixpath.basename(filename_in_zip)
            if basename == '':
                continue
            if basename.lower().endswith('.dll'):
                copy_zip_member(zf, member, os.path.join(game_dir, basename))
                extracted += 1
            elif basename == 'config-win.inc':
                copy_zip_member(zf, member, config_target)
                extracted += 1
    print('Extracted {} files from {}'.format(extracted, archive_name))


def ensure_windows_runtime(force=False):
    missing = missing_runtime_files()
    if not force and not missing:
        print('Windows runtime files are already present')
        return

    if missing:
        print('Missing Windows runtime files:')
        for path in missing:
            print('  ' + path)

    download_dll_archive(force=force)
    extract_dll_archive()
    download_bass(force=force)


def main():
    parser = argparse.ArgumentParser(description='Download Windows DLLs for USDX.')
    parser.add_argument('--extract', action='store_true',
                        help='extract DLLs to game/ and config-win.inc to src/')
    parser.add_argument('--ensure', action='store_true',
                        help='download and extract only when Windows runtime files are missing')
    parser.add_argument('--force', action='store_true',
                        help='redownload and overwrite existing runtime files')
    args = parser.parse_args()

    if args.ensure:
        ensure_windows_runtime(force=args.force)
        return

    download_bass(force=args.force)
    download_dll_archive(force=args.force)
    if args.extract:
        extract_dll_archive()


if __name__ == '__main__':
    main()
