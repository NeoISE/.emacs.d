# My Personal Emacs Configs

## Background
I was contemplating about putting up my configs online, but then I had to move computers.
I found physically moving the files to be too tedious, so now the public parts of the configs are on Github.

## Fonts Used
Technically, non-system default fonts are not required to be installed on the system for this configuration to work properly--the config will run just fine.
Though to experience the full look-and-feel of the config, one needs to install the following fonts, listed below in **bold**; the listed fonts are not apart of this config nor is it included/shipped with this config.

Purpose | Preferred Font | Microsoft Windows Operating System Backup Font | macOS Backup Font | Linux (or UNIX) Backup Font
--- | --- | --- | --- | ---
`default` face (the default font of the system) | **[Hack](https://github.com/source-foundry/Hack)** | Consolas¹ | Menlo | DejaVu Sans Mono
`prettify-symbols-mode` Unicode Characters | - | **DejaVu Sans Mono** | - | -
`fixed-pitch` | same font as `default` is used | - | - | -
`fixed-pitch-serif` | **[Anonymous Pro](http://www.marksimonson.com/fonts/view/anonymous-pro)** | Courier New | Courier New | Courier New
`variable-pitch` | **[FiraGO](https://bboxtype.com/typefaces/FiraGO/)** | Arial | Arial | Arial

¹ Suprisingly, Consolas is missing some crucial unicode symbols, like ⇒, which is needed for `prettify-symbols-mode`. Thus, if one does not want to install **Hack** on a Microsoft Windows operating system, then at least install **DejaVu Sans Mono** font to cover the unicode characters that Consolas lacks.

## News

* 2019-01-30: Updated the icons and fonts; after moving to the new themes, the icons sort of felt out-of-place.
  Thus, I updated the icons' colors to conform to [Google's Material Design suggestions](https://material.io/tools/color/) and used a palette made of 6 colors to color the icons.

  After using `variable-pitch-mode` many times and `fixed-pitch-serif` font setting (in some modes) and hating the defaults for a while, I decided to finally come with *better* defaults; thus, the font settings are revamped.
  See the fonts used table for more information on which fonts will be used.

* 2019-01-06: New Year, New Version; I am finally moving my init config to fully support the new (and changed) features of major version 26.
  The transition initially started before the new year, but since v25 was still working fine, there was little to motivate the move.
  It was only when I started to review the code that I found how "clunky" some of the code was that a move to v26 was deemed necessary.
  Most notable change is:
  - `linum-mode` to `display-line-numbers-mode`

## Ground Rules
Anyone can
* reference/fork my settings
* report issues
* make pull requests for issues

However, please do not do the following:

1. Report issues to _request features_
2. Make a PR for purposes *_other than_ for fixing issues*
