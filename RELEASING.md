# Making a release
1. Find the contents of `VERSION` (strip the `+dev`) throughout the code.
    This should result in four places:
    * VERSION
    * UConfig.pas
    * variables.nsh
    * ultrastardx.appdata.xml
2. Make the release:
    * in the first two files, update it to the new version _without_ the `+dev` bit
    * in `variables.nsh` update both blocks immediately and swap the comments
    * in `ultrastardx.appdata.xml` add a new entry
3. Commit, `git tag v<the-new-version>` and then `git push origin master v<the-new-version>`\
    __It is important that these two get pushed together__ otherwise Flatpak gets confused.
4. Wait and get the artifacts from the CI.
    If any of them fail, just add an extra `;` on one of the already commented lines in `variables.nsh`, commit, and then push only `master`.
5. Add `+dev` to the version in the first two files and swap the comments in `variables.nsh` again, commit, push.
    This is just to set the dev version again.
6. Attach the artifacts to the release page and publish it.
    Don't forget to also create a PR for this release in
    https://github.com/UltraStar-Deluxe/ultrastar-deluxe.github.io
