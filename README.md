[![Build Status](https://travis-ci.org/codingteam/pacmacs.el.svg?branch=master)](https://travis-ci.org/codingteam/pacmacs.el)
[![Coverage Status](https://coveralls.io/repos/codingteam/pacmacs.el/badge.svg?branch=master&service=github)](https://coveralls.io/github/codingteam/pacmacs.el?branch=master)
[![MELPA](http://melpa.org/packages/pacmacs-badge.svg)](http://melpa.org/#/pacmacs)

# Pacmacs #

Pacman for Emacs

<p align="center"><img src="http://i.imgur.com/QcNqAcL.png" href="http://i.imgur.com/QcNqAcL.png" /></p>
<p align="center"><img src="http://i.imgur.com/sJBrzTe.png" href="http://i.imgur.com/sJBrzTe.png" /></p>
<p align="center"><img src="http://i.imgur.com/WCuGfnk.png" href="http://i.imgur.com/WCuGfnk.png" /></p>

## Requirements ##

- Emacs 24.4+
- Emacs should have displaying images support
- Emacs should have the [XPM](https://en.wikipedia.org/wiki/X_PixMap) image format support

## Installation ##

Pacmacs available on [MELPA](http://melpa.org/). Add the following to
your emacs config file somewhere (.emacs, init.el, whatever):

```
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
```

Then use `M-x package-install RET pacmacs RET` to install the game.

## Usage ##

### Functions ###

1. `M-x pacmacs-start RET` — start the game;
2. `M-x pacmacs-score RET` — show the 10 best scores.

### Controls ###

Only available in the `*Pacman*` buffer.

1. `<up>` — move Pacman up;
2. `<down>` — move Pacman down;
3. `<left>` — move Pacman left;
4. `<right>` — move Pacman right;
5. `q` — quit the game;
6. `SPC` — pause the game.


## Development ##

Before developing the game please remove it from Emacs if it was
installed before. You'll need [Cask][cask] to install the dependencies.

I usually use the following workflow when I develop this game:

1. `$ git clone git://github.com/codingteam/pacmacs.el.git && cd pacmacs.el`;
2. `$ cask install`;
3. `$ cask exec emacs`;
4. `M-x find-file RET /path/to/pacmacs.el/pacmacs.el RET`;
5. `M-x eval-expression RET (add-to-list 'load-path default-directory) RET`;
6. `M-x eval-buffer RET`;
7. `M-x pacmacs-start RET`;
8. `M-x pacmacs-quit RET`;
9. Change something in the source code;
10. Go to the step `6`.

## Unit Tests ##

For Unit Tests you need to install [Cask][cask] first. To run unit tests:

    $ cask # only once to download development dependencies
    $ cask exec ert-runner

## Licensing ##

All the source code of this game is distributed under the MIT
license. All the work in the `sprites/` directory is distributed under
[CC-BY](https://creativecommons.org/licenses/by/4.0/).

Check `LICENSE.md` and `sprites/LICENSE.md` files for more
information.

[cask]: http://cask.readthedocs.org/en/latest/
