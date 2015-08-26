;;; pacman.el --- Pacman for Emacs

;; Copyright (C) 2015 Codingteam

;; Author: Codingteam <codingteam@conference.jabber.ru>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/pacman.el
;; Version: 0.0.1

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(require 'pacman-anim)

(defconst pacman-buffer-name "*Pacman*")

(defvar pacman-timer nil)
(defvar pacman-counter 0)

(defvar pacman-board-width 10)
(defvar pacman-board-height 10)
(defvar pacman-player-state nil)
(setq pacman-player-state
      (list :row 0
            :column 0
            :direction 'right
            :animation (list :frames (list '(20 0 20 20)
                                           '(0 0 20 20)
                                           '(40 0 20 20))
                             :current-frame 0)))

(defun pacman-load-resource (filename)
  (create-image (concat default-directory filename)
                'png nil :heuristic-mask t))

(defvar pacman-resource (pacman-load-resource "pacman10-hp-sprite.png"))

(define-derived-mode pacman-mode special-mode "pacman-mode"
  (define-key pacman-mode-map (kbd "<up>") 'pacman-up)
  (define-key pacman-mode-map (kbd "<down>") 'pacman-down)
  (define-key pacman-mode-map (kbd "<left>") 'pacman-left)
  (define-key pacman-mode-map (kbd "<right>") 'pacman-right)
  (define-key pacman-mode-map (kbd "q") 'pacman-quit)
  (add-hook 'kill-buffer-hook 'pacman-destroy nil t))

(defun pacman-insert-image (resource resource-vector)
  (insert-image resource " " nil resource-vector))

(defun pacman-start ()
  (interactive)
  (switch-to-buffer-other-window pacman-buffer-name)
  (pacman-mode)
  (unless pacman-timer
    (setq pacman-timer (run-at-time nil 0.1 'pacman-tick))))

(defun pacman-destroy ()
  (when pacman-timer
    (cancel-timer pacman-timer)
    (setq pacman-timer nil)))

(defun pacman-quit ()
  (interactive)
  (when (get-buffer pacman-buffer-name)
    (kill-buffer pacman-buffer-name)))

(defun pacman-tick ()
  (interactive)
  (with-current-buffer pacman-buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq pacman-player-state
            (pacman-anim-object-next-frame pacman-player-state))
      (pacman-render-state))))

(defun pacman-render-state ()
  (let* ((player-anim (plist-get pacman-player-state :animation))
         (player-vector (pacman-anim-get-frame player-anim)))
    (dotimes (row pacman-board-height)
      (dotimes (column pacman-board-width)
        (if (and (equal row (plist-get pacman-player-state :row))
                 (equal column (plist-get pacman-player-state :column)))
            (pacman-insert-image pacman-resource player-vector)
          (pacman-insert-image pacman-resource player-vector)))
      (insert "\n"))))

(defun pacman-up ()
  (interactive)
  )

(defun pacman-down ()
  (interactive)
  )

(defun pacman-left ()
  (interactive)
  )

(defun pacman-right ()
  (interactive)
  )

(provide 'pacman)

;;; pacman.el ends here
