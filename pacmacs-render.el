;;; pacmacs-render.el --- Pacman for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015 Codingteam

;; Author: Codingteam <codingteam@conference.jabber.ru>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/pacmacs.el

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

;;; Commentary:

;; Routines for working with rendering

;;; Code:

(require 'pacmacs-image)
(require 'pacmacs-anim)
(require 'pacmacs-board)

(defvar pacmacs--empty-cell nil)
(defvar pacmacs--life-icon nil)

(defun pacmacs--render-empty-cell ()
  (when (not pacmacs--empty-cell)
    (setq pacmacs--empty-cell (pacmacs-create-transparent-block 40 40)))
  (pacmacs-insert-image pacmacs--empty-cell '(0 0 40 40)))

(defun pacmacs--render-life-icon ()
  (when (not pacmacs--life-icon)
    (setq pacmacs--life-icon
          (pacmacs-load-anim "Pacman-Chomping-Right"))
    (plist-put pacmacs--life-icon :current-frame 2))
  (pacmacs--render-anim pacmacs--life-icon))

(defun pacmacs--render-anim (anim)
  (let* ((sprite-sheet (plist-get anim :sprite-sheet))
         (current-frame (plist-get (pacmacs-anim-get-frame anim) :frame)))
    (pacmacs-insert-image sprite-sheet current-frame)))

(defun pacmacs--render-object (anim-object)
  (if anim-object
      (let* ((anim (plist-get anim-object :current-animation)))
        (pacmacs--render-anim anim))
    (pacmacs--render-empty-cell)))

(defun pacmacs--render-track-board (track-board)
  (plist-bind ((width :width)
               (height :height))
      track-board
    (dotimes (row height)
      (dotimes (column width)
        (let ((x (pacmacs--cell-get track-board row column)))
          (cond
           ((null x)
            (insert "."))
           ((equal x 'left)
            (insert "<"))
           ((equal x 'right)
            (insert ">"))
           ((equal x 'up)
            (insert "^"))
           ((equal x 'down)
            (insert "v")))))
      (insert "\n"))))

(provide 'pacmacs-render)

;;; pacmacs-render.el ends here
