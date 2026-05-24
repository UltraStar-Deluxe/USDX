This page lists various breaking and non-breaking theme changes that providers of custom themes will want to incorporate.

### 2025.12.0
The way the options screens work has been reworked.

All elements that start with any of these names should be entirely removed:
* `OptionsGame`
* `OptionsGraphics`
* `OptionsSound`
* `OptionsInput`
* `OptionsLyrics`
* `OptionsThemes`
* `OptionsRecord`
* `OptionsAdvanced`
* `OptionsNetwork`
* `OptionsWebcam`
* `OptionsJukebox`

Copy (and if desired: modify) all elements that start with these names from `Deluxe.ini` or `Modern.ini`:
* `OptionsSub`
* `OptionsNetworkLegend`

### 2026.2.0

Themes now support extending (across themes) and inheritance (within a theme).

#### Extending
Instead of duplicating an entire theme, a theme only needs to:
* `[Theme].BaseTheme = Modern` (or any other theme that does _not_ define `BaseTheme`)
* Define the _changed_ elements _completely_ with all attributes.
* Define any elements you want to _disable_ with a single attribute `Enabled = 0`.
  This only works on _optional_ elements, these usually end in `*Text1/2/3/...` or `*Static1/2/3/...`.

#### Inheritance
Basically instead of doing this:
<details>
<summary>Theme code with lots of duplication</summary>

```
[MainButtonSolo]
X = 95
Y = 270
W = 150
H = 50
Tex = Button
Color = ColorLight
DColor = ColorDark
Type = Transparent
Texts = 1
Reflection = 1
ReflectionSpacing = 15
DeSelectReflectionSpacing = 280
Fade = 1
FadeText = 1
SelectH = 150
FadeTex = ButtonFade
FadeTexPos = 0

[MainButtonMulti]
X = 250
Y = 270
W = 150
H = 50
Tex = Button
Color = ColorLight
DColor = ColorDark
Type = Transparent
Texts = 1
Reflection = 1
ReflectionSpacing = 15
DeSelectReflectionSpacing = 280
Fade = 1
FadeText = 1
SelectH = 150
FadeTex = ButtonFade
FadeTexPos = 0
```
</details>

you can write `MainMuttonMulti` as:
```
[MainButtonMulti]
Inherits = MainButtonSolo
X = 250
```

In this case, `MainButtonSolo` can also be defined by the `BaseTheme`.

Note: `Enabled = 0` can't be inherited.
In the above example, if you were to set `[MainButtonSolo].Enabled = 0`, this doesn't magically also disable the `MainButtonMulti`.

### 2026.3.0

Any theme element whose name starts with `SongCarousel`, `SongSlotMachine`, `SongSlide` or `SongMosaic` is obsolete.
