#!/usr/bin/python3

import urllib.request
import shutil
import json
import sys
import os
import zipfile

sha = 'cf77d4e33eb71ad972f32bfb2633fb6ad704dc4a'
filename = 'usdx-dlls-x86_64'
urlbase = 'https://api.github.com/repos/UltraStar-Deluxe/mxe/'
headers = {
    'Accept': 'application/vnd.github+json',
    'X-GitHub-Api-Version': '2022-11-28'
    }

token = os.environ.get('ARTIFACT_ACCESS_TOKEN')
if token == None or token.strip() == '':
    token = os.environ.get('GITHUB_TOKEN')
if token != None and token.strip() != '':
    headers['Authorization'] = 'Bearer ' + token

print('Searching for binaries built from commit ' + sha)

def download_bass():
    bass_url = 'https://www.un4seen.com/files/bass24.zip'
    zip_name = 'bass24.zip'
    target = 'game/bass.dll'
    with urllib.request.urlopen(bass_url) as dl, open(zip_name, 'wb') as out:
        out.write(dl.read())
    with zipfile.ZipFile(zip_name, 'r') as zf:
        member = 'x64/bass.dll'
        with zf.open(member) as src, open(target, 'wb') as dst:
            dst.write(src.read())
    print('Downloaded ' + member + ' to ' + target)


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

download_bass()

dllurl = search_releases()
if dllurl == None and token != None:
    dllurl = search_artifacts()
if dllurl == None:
    sys.exit(1)
print('Downloading from ' + dllurl)
req = urllib.request.Request(url = dllurl)
for key in headers:
    req.add_unredirected_header(key, headers[key])
shutil.copyfileobj(urllib.request.urlopen(req),
                   open(filename + '.zip', 'wb'))
