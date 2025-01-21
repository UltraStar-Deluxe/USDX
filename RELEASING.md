# Making a release
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
3. If there are pre-releases in [mxe releases](https://github.com/UltraStar-Deluxe/mxe/releases), see [UPDATING-DLLS.md](UPDATING-DLLS.md).
4. Commit, `git tag v<the-new-version>` and then `git push origin master v<the-new-version> master:release`
5. Wait and get the artifacts from the CI.
    If any of them fail, retry the job.
6. Add `+dev` to the version in the first two files and swap the comments in `variables.nsh` again, commit, push.
    This is just to set the dev version again.
7. Attach the artifacts to the release page and publish it.
    Don't forget to also create a PR for this release in
    https://github.com/UltraStar-Deluxe/ultrastar-deluxe.github.io
8. Create a PR in [the FlatHub repository](https://github.com/flathub/eu.usdx.UltraStarDeluxe) that updates the tag and commit values.
    See this PR for an example: https://github.com/flathub/eu.usdx.UltraStarDeluxe/pull/7/files
