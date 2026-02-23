import json
import urllib.request
import zipfile
from pathlib import Path

bass_url = 'https://www.un4seen.com/files/bass24.zip'
zip_name = 'bass24.zip'
target = Path('game') / 'bass.dll'
with urllib.request.urlopen(bass_url) as dl, open(zip_name, 'wb') as out:
    out.write(dl.read())
with zipfile.ZipFile(zip_name, 'r') as zf:
    member = 'x64/bass.dll'
    with zf.open(member) as src, open(target, 'wb') as dst:
        dst.write(src.read())
print(f'Downloaded {member} to {target}')

api_url = 'https://api.github.com/repos/dgruss/mxe/releases'
with urllib.request.urlopen(api_url) as resp:
    releases = json.load(resp)
    if not releases:
        print('No releases found.')
        exit(1)
    pattern = r'^usdx-dlls-.*\.zip$'
    for release in releases:
        for asset in release.get('assets', []):
            print(f"Downloading {asset['name']} from release '{release.get('name', release.get('tag_name', ''))}'...")
            # Rename to only keep up to the third dash (usdx-dlls-arch.zip)
            parts = asset['name'].split('-')
            if len(parts) >= 3:
                new_name = '-'.join(parts[:3]) + '.zip'
            else:
                new_name = asset['name']
            with urllib.request.urlopen(asset['browser_download_url']) as dl, open(new_name, 'wb') as out:
                out.write(dl.read())
            print('Download complete.')
            exit(0)
    print('No usdx-dlls-*.zip asset found in any release.')
    exit(1)
