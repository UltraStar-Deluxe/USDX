## Introduction to ReplayGain
It is desirable for each song in USDX to have equal loudness, so the audio volume does not need to be frequently adjusted while playing. However, the loudness of audio files may vary significantly.

To solve this problem, USDX supports ReplayGain during playback of a song's `#MP3`/`#AUDIO` file. ReplayGain works by scanning a file ahead of time and "tagging" it with metadata that contains a volume adjustment. During playback, USDX will read the file's tag and apply the requested volume adjustment automatically. This ensures that all files play back at the same perceived volume.

ReplayGain is not enabled by default in USDX. To enable it, navigate to Tools->Options->Sound, and set the ReplayGain setting to 'On'.

## Adding ReplayGain Tags to Your UltraStar Library
USDX will read the ReplayGain tags of your audio files, but it won't write them. This means you will need another program to scan your audio files and write the ReplayGain tags, so they can later be read by USDX.

### USDB Syncer
Recent versions of the [USDB Syncer](https://github.com/bohning/usdb_syncer) support ReplayGain tag writing. However, ReplayGain is not enabled by default. To enable it, navigate to Tools->Settings, and set the "Normalization" setting to "ReplayGain".

When enabled, the Syncer will automatically scan downloaded audio files for loudness and write the ReplayGain tags.

### Full Library Scanning
For ReplayGain to work properly, your *entire* library needs to have tags. It's not good enough to write ReplayGain tags to newly added songs only. If you already have a large existing UltraStar library, you will need to write ReplayGain tags to those files too.

The easiest way to perform a full library scan is by using [UltraStar Manager](https://github.com/UltraStar-Deluxe/UltraStar-Manager), which has a built-in ReplayGain scanner since version 2.1.0. To add ReplayGain tags to your entire library, use Ctrl + A to highlight all songs, then right click and select "Calculate Song ReplayGain" from the context menu.

The full library scan only needs to be performed once. In the future, when you add new songs to your UltraStar library, the Syncer will handle ReplayGain for you automatically.

## Adjusting the Volume of Game Sounds
After enabling ReplayGain, you may notice that the game-provided sounds such as the background music and menu sound effects have become loud relative to the singing music. To normalize the sound, ReplayGain generally *reduces* the volume of the audio tracks. But no ReplayGain adjustment is applied to the game-provided sounds.

To counteract this effect, USDX has separate volume settings for the game-provided sounds. If you use ReplayGain, it recommended to adjust the following settings in Tools->Options->Sound:
- BG Music Volume: Set to 40%
- SFX Volume: Set to 60%

This restores the dynamics between the music and the game-provided sounds when using ReplayGain.
