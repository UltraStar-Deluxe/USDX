# Making a release
## Check for DLL updates
The draft release notes probably already mention them, but check the [mxe releases](https://github.com/UltraStar-Deluxe/mxe/releases) for any pre-release versions.
If there are any pre-releases present, you probably need to check [UPDATING-DLLS.md](UPDATING-DLLS.md).

## Releasing USDX
1. Find the contents of `VERSION` (strip the `+dev`) throughout the code.
    This should result in six places:
    * [VERSION](VERSION)
    * [UConfig.pas](src/base/UConfig.pas)
    * [variables.nsh](installer/settings/variables.nsh)
    * [ultrastardx.appdata.xml](dists/ultrastardx.appdata.xml)
    * 2x [Info.plist](src/macosx/Info.plist)
2. Make the release:
    * in the first two files, update it to the new version _without_ the `+dev` bit
    * in `variables.nsh` update both blocks immediately and swap the comments
    * in `ultrastardx.appdata.xml` add a new entry
    * in `Info.plist` update the version number
3. Commit, `git tag v<the-new-version>` and then `git push origin master v<the-new-version> master:release`
4. Wait and get the artifacts from the CI.
    If any of them fail, just add an extra `;` on one of the already commented lines in `variables.nsh`, commit, and then push only `master`.
5. Add `+dev` to the version in the first two files and swap the comments in `variables.nsh` again, commit, push.
    This is just to set the dev version again.
6. Attach the artifacts to the release page and publish it.
    Don't forget to also create a PR for this release in
    https://github.com/UltraStar-Deluxe/ultrastar-deluxe.github.io
7. Create a PR in [the FlatHub repository](https://github.com/flathub/eu.usdx.UltraStarDeluxe) that updates the tag and commit values
    See this PR for an example: https://github.com/flathub/eu.usdx.UltraStarDeluxe/pull/7/files
