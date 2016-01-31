;;; pacmacs-render.el --- Pacman for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Codingteam

;; Author: Codingteam <codingteam@conference.jabber.ru>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/codingteam/pacmacs.el

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

(require 'pacmacs-walls)
(require 'pacmacs-anim)
(require 'pacmacs-board)

(defvar pacmacs--life-icon nil)

(defmacro with-render-target (render-target-buffer &rest body)
  (declare (indent 1) (debug (sexp &rest form)))
  `(with-current-buffer ,render-target-buffer
     (let ((inhibit-read-only t))
       ,@body)))

(defun pacmacs-insert-image (resource resource-vector)
  (insert-image resource " " nil resource-vector))

(defun pacmacs--render-empty-cell ()
  (pacmacs-insert-image (pacmacs--create-wall-tile
                         40 40
                         (make-list 8 nil))
                        '(0 0 40 40)))

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

(defun pacmacs--render-track-cell (track-cell)
  (insert "\t")
  (if track-cell
      (insert (int-to-string track-cell))
    (insert ".")))

(defun pacmacs--render-board (board cell-renderer)
  (plist-bind ((width :width)
               (height :height))
      board
    (dotimes (row height)
      (dotimes (column width)
        (let ((cell (pacmacs--cell-wrapped-get board row column)))
          (funcall cell-renderer cell)))
      (insert "\n"))))

(defun pacmacs--render-track-board (track-board)
  (pacmacs--render-board track-board
                         #'pacmacs--render-track-cell))

(defun pacmacs--render-object-board (object-board)
  (pacmacs--render-board object-board
                         (-compose #'pacmacs--render-object #'car))
  (insert "\n"))

(provide 'pacmacs-render)

;;; pacmacs-render.el ends here
