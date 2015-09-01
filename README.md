[![Build Status](https://travis-ci.org/rexim/pacman.el.svg?branch=master)](https://travis-ci.org/rexim/pacman.el)
[![Coverage Status](https://coveralls.io/repos/rexim/pacman.el/badge.svg?branch=master&service=github)](https://coveralls.io/github/rexim/pacman.el?branch=master)

# Pacman for Emacs #

## Usage ##

1. clone the repo somewhere and cd to it: `$ git clone git://github.com/rexim/pacman.el.git && cd pacman.el`;
2. open `pacman.el` file in Emacs;
3. add game folder to the Load Path: `M-x eval-expression RET (add-to-list 'load-path default-directory) RET`;
4. eval entire buffer: `M-x eval-buffer RET`;
5. start the game: `M-x pacman-start RET`.

## Unit Tests ##

For Unit Tests you need to install [Cask](http://cask.readthedocs.org/en/latest/) first. To run unit tests:

    $ cask # only once to download development dependencies
    $ cask exec ert-runner

