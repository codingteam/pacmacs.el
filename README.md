[![Build Status](https://travis-ci.org/rexim/pacman.el.svg?branch=master)](https://travis-ci.org/rexim/pacman.el)
[![Coverage Status](https://coveralls.io/repos/rexim/pacman.el/badge.svg?branch=master&service=github)](https://coveralls.io/github/rexim/pacman.el?branch=master)

# Pacman for Emacs #

**This branch contains proof of concept of using one image inside of
  Emacs as a canvas. I'm not going to continue to develop it, but
  don't wanna through it away too. So I will just leave it here and
  maybe reuse that concept somewhere in my another Emacs Lisp project
  in the future. :)**

## Usage ##

1. `$ git clone git://github.com/rexim/pacman.el.git && cd pacman.el`;
2. `M-x find-file RET /path/to/pacman.el/pacman.el RET`
3. `M-x eval-expression RET (add-to-list 'load-path default-directory) RET`;
4. `M-x eval-buffer RET`;
5. `M-x pacman-start RET`.

## Unit Tests ##

For Unit Tests you need to install
[Cask](http://cask.readthedocs.org/en/latest/) first. To run unit
tests:

    $ cask # only once to download development dependencies
    $ cask exec ert-runner

## Licensing ##

All the source code of this game is distributed under the MIT
license. All the work in the `sprites/` directory is distributed under
[CC-BY](https://creativecommons.org/licenses/by/4.0/).

Check `LICENSE.md` and `sprites/LICENSE.md` files for more
information.
