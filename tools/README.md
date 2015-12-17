# Project Tools #

Tools that help to develop the game

## pacmacs-rr.el ##

RR stands for Record and Replay. Additional module for recording and
replaying integration test cases.

## att.el ##

Runs `./it-cases/it-case03.el` it case using `pacmacs-rr` module and
outputs the ATT to `./att.txt` file.

### Usage ###

To run this script do

    $ cask exec emacs -Q --load ./tools/att.el

## compile.el ##

Compiles all the source code, treats warnings as errors and exits with
non-zero exit code on any error (which `cask build` does not yet).

### Usage ###

To run this script do

    $ emacs -Q --script ./tools/compile.el

