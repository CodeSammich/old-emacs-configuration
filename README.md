# My personal Emacs configuration #

My own home-grown Emacs configuration with
[use-package](https://github.com/jwiegley/use-package).

## Setup — How do you use it? ##

You need Emacs 25 snapshot builds, straight from Git `master`.  Stable
releases of GNU Emacs won’t work, I build GNU Emacs weekly.

```console
$ git clone https://github.com/lunaryorn/.emacs.d.git ~/.emacs.d
$ brew install trash coreutils
$ brew install aspell --with-lang-de --with-lang-en
```

plus all the standard tools for all the various programming languages (`sbt`,
`hlint`, `stack`, `pandoc`, `pylint` and stuff).  Read the comments in `init.el`
for more information.

## Layout — Where do you find things? ##

It’s a single big `init.el`, containing only `use-package` declarations for all
the built-in and 3rd party packages I use.  These declarations have all the
configuration and setup for the specific packages.

There’s also `lisp/` which has my own personal extensions and libraries with
custom functions, etc.  These libraries are loaded like normal packages with
`use-package` in `init.el`.

## Highlights — What you should probably copy from this! ##

- Very good OS X support, even with stock GNU Emacs, including a font setup that
  supports Math, Symbols and Coloured Emojis (yay 😍)
- A well-designed key bindings scheme, greatly inspired by Spacemacs and
  supported by Which Key Mode
- Rules for buffer displays in `display-buffer-alist`
- A very powerful and comprehensive LaTeX setup with AUCTeX
- Nice configurations for Scala, Emacs Lisp and a couple of other languages

## Final words ##

Have fun, and copy freely!  Please feel free to mess around, and take whatever
you like!  Credits mandatory (respect the license), feedback appreciated!

## License ##

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with GNU
Emacs; see the file COPYING.  If not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
