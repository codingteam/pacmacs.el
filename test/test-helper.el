(require 'cl)                           ;el-mock doesn't work without
                                        ;this
(require 'el-mock)
(require 'undercover)

(undercover "*.el")

(add-to-list 'load-path ".")
(load "pacman-anim.el")
(load "pacman-image.el")
