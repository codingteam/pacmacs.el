[![Build Status](https://travis-ci.org/rexim/pacman.el.svg?branch=master)](https://travis-ci.org/rexim/pacman.el)

# Pacman for Emacs #

## Usage ##

1. Clone the repo somewhere and cd to it: `$ git clone git://github.com/rexim/pacman.el.git && cd pacman.el`
2. The game requires a sprite sheet file, but we currently do not provide it. You have to find it (or even make it) yourself. Current sprite sheet requirements:
    1. The sprite sheet file should be localted at the games folder
    2. The sprite sheet file should be named `pacman10-hp-sprite.png`
    3. The sprite sheet should have at least three frames at the following coordinates (in the format `X Y WIDTH HEIGHT`)
        - `20 0 20 20`
        - `0  0 20 20`
        - `40 0 20 20`
3. Add game folder to the `load-path`
4. Open `pacman.el` file in Emacs
5. Eval entire file: `M-x eval-buffer`
6. Start the game: `M-x pacman-start`
