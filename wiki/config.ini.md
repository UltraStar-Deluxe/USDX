Some settings are currently only available through setting/changing them in config.ini. This mostly affects newer settings, as the themes are currently too tightly coupled.

## Location

The config file can also be set explicitly with the `-ConfigFile` command-line parameter. Relative paths passed to `-ConfigFile` are resolved relative to the executable directory.

### Linux

When UltraStar Deluxe runs from a local build or portable tree, and the executable directory contains a `languages` directory and is writable, `config.ini` is read from and written to the executable directory.

Otherwise, `config.ini` is stored in `.ultrastardx/config.ini` below the user's home directory. The home directory is resolved from the passwd entry for the current user. If that is unavailable, the `HOME` environment variable is used as fallback.

For most installations this means:

```text
~/.ultrastardx/config.ini
```

Flatpak installations use the Flatpak app data directory instead:

```text
~/.var/app/eu.usdx.UltraStarDeluxe/.ultrastardx/config.ini
```

```
[Advanced]
DefaultSingMode=Instrumental # starts every song in instrumental mode
DefaultSingMode=Regular # default, behavior as it was
```
