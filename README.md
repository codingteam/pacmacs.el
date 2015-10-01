[![Build Status](https://travis-ci.org/rexim/pacmacs.el.svg?branch=master)](https://travis-ci.org/rexim/pacmacs.el)
[![Coverage Status](https://coveralls.io/repos/rexim/pacmacs.el/badge.svg?branch=master&service=github)](https://coveralls.io/github/rexim/pacmacs.el?branch=master)

# Pacmacs #

Pacman for Emacs

## Usage ##

1. `$ git clone git://github.com/rexim/pacmacs.el.git && cd pacmacs.el`;
2. `M-x find-file RET /path/to/pacmacs.el/pacmacs.el RET`
3. `M-x eval-expression RET (add-to-list 'load-path default-directory) RET`;
4. `M-x eval-buffer RET`;
5. `M-x pacmacs-start RET`.

## Unit Tests ##

For Unit Tests you need to install
[Cask](http://cask.readthedocs.org/en/latest/) first. To run unit
tests:

    $ cask # only once to download development dependencies
    $ cask exec ert-runner

## Limitations ##

- Emacs should be compiled with displaying images support
- Emacs should have the [XPM](https://en.wikipedia.org/wiki/X_PixMap) image format support

## Licensing ##

All the source code of this game is distributed under the MIT
license. All the work in the `sprites/` directory is distributed under
[CC-BY](https://creativecommons.org/licenses/by/4.0/).

Check `LICENSE.md` and `sprites/LICENSE.md` files for more
information.
