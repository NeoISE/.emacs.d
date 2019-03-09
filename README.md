# My Personal Emacs Configs

## Background
I was contemplating about putting up my configs online, but then I had to move computers.
I found physically moving the files to be too tedious, so now the public parts of the configs are on Github.

## Fonts Used
Technically, non-system default/included fonts are not required to be installed on the system for this configuration to work properly--the config will run just fine.
Though to experience the configuration fully, one might want to install the following fonts, listed below in **bold**; the listed fonts are not a part of this config nor is it included/shipped with this config.

Purpose | My Preferred Font | My Preferred Backup Font for Microsoft Windows Operating System | My Preferred Backup Font for macOS Operating System | My Preferred Backup Font for Linux (or UNIX) Operating Systems
--- | --- | --- | --- | ---
`default` face (the default font of the system) | **[Hack](https://github.com/source-foundry/Hack)** | Consolas (with **[DejaVu Sans Mono](https://dejavu-fonts.github.io/)** for `prettify-symbols-mode` Unicode Characters)¹ | Menlo | DejaVu Sans Mono
`fixed-pitch` | same font as `default` is used | - | - | -
`fixed-pitch-serif` | **[Anonymous Pro](http://www.marksimonson.com/fonts/view/anonymous-pro)** | Courier New | Courier New | Courier New
`variable-pitch` | **[FiraGO](https://bboxtype.com/typefaces/FiraGO/)** | Arial | Arial | Arial

¹ Suprisingly, Consolas is missing some crucial unicode symbols, like ⇒, which is needed for `prettify-symbols-mode`. Thus, if one does not want to install **Hack** on a Microsoft Windows operating system, then at least install **DejaVu Sans Mono** font to cover the unicode characters that Consolas lacks.

## News

* 2019-03-08: Added some new functionality to the cut and copy commands, ability to toggle relative line-numberings (binded to <kbd>F6</kbd>), and updated the README.

  Using the key combination:
  1. <kbd> C </kbd> - <kbd> u </kbd> <kbd> 0 </kbd> <kbd> C </kbd>-<kbd> w </kbd> for *cutting* all visible text in the narrowing (if in effect) or whole buffer (if narrowing is not in effect) without changing the current location of the cursor,
  2. <kbd> C </kbd> - <kbd> u </kbd> <kbd> 0 </kbd> <kbd> M </kbd>-<kbd> w </kbd> for *copying* all visible text in the narrowing (if in effect) or whole buffer (if narrowing is not in effect) without changing the current location of the cursor.
  These combinations help to reduce the number of <kbd> C </kbd>-<kbd> u </kbd> <kbd> C </kbd>-<kbd> SPC </kbd> pressing to restore point after the typical approach of <kbd> C </kbd>-<kbd> h </kbd> <kbd> C </kbd>-<kbd> w </kbd> for cutting whole buffer or <kbd> C </kbd>-<kbd> h </kbd> <kbd> M </kbd>-<kbd> w </kbd> for copying whole buffer.

  We can also now use (input a valid number in place of NUM):
  3. <kbd> C </kbd> - <kbd> u </kbd> <kbd> NUM </kbd> <kbd> C </kbd>-<kbd> w </kbd>
  4. <kbd> C </kbd> - <kbd> u </kbd> <kbd> NUM </kbd> <kbd> M </kbd>-<kbd> w </kbd>
  for cutting and copying a region of text that includes the current line and *NUM* number of lines above and *NUM* number of lines below the current line. **The absolute value of NUM is used**.
  For a shortcut, use
  5. <kbd> C </kbd> - <kbd> u </kbd> <kbd> C </kbd>-<kbd> w </kbd>
  6. <kbd> C </kbd> - <kbd> u </kbd> <kbd> M </kbd>-<kbd> w </kbd>
  for cutting and copying a region just like above but without the numeric argument, an implicit value of *4* is used.
  In fact, for each <kbd> C </kbd> - <kbd> u </kbd> pressed after the first, the implicit numeric argument is multiplied by 4.


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
