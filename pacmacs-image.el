;;; pacmacs-image.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Routines for working with game resources

;;; Code:

(defconst pacmacs--flip-xbm-bits (eq system-type 'windows-nt))

(defun pacmacs-load-image (filename)
  (create-image filename 'xpm nil :heuristic-mask t))

(defun pacmacs-insert-image (resource resource-vector)
  (insert-image resource " " nil resource-vector))

(defun pacmacs-create-color-block (width height color)
  (apply
   #'create-image
   (make-vector
    width (make-bool-vector height t))
   'xbm t :width width :height height
   (if (not pacmacs--flip-xbm-bits)
       (list
        :foreground color
        :background nil)
     (list
      :foreground nil
      :background color))))

(defun pacmacs--create-wall-block (width height color bottom right top left)
  (let ((wall-block (make-vector
                     width nil)))

    (dotimes (i width)
      (aset wall-block i (make-bool-vector height nil)))
    
    (when left
      (dotimes (i height)
        (aset (aref wall-block i) 0 t)
        (aset (aref wall-block i) 1 t)
        (aset (aref wall-block i) 2 t)
        ))

    (when right
      (dotimes (i height)
        (aset (aref wall-block i) (1- width) t)
        (aset (aref wall-block i) (- width 2) t)
        (aset (aref wall-block i) (- width 3) t)
        ))

    (when top
      (dotimes (i width)
        (aset (aref wall-block 0) i t)
        (aset (aref wall-block 1) i t)
        (aset (aref wall-block 2) i t)
        ))
    
    (when bottom
      (dotimes (i width)
        (aset (aref wall-block (1- height)) i t)
        (aset (aref wall-block (- height 2)) i t)
        (aset (aref wall-block (- height 3)) i t)
        ))

    (create-image wall-block 'xbm t :width width :height height
                  :foreground color
                  :background nil)))

(defun pacmacs-create-transparent-block (width height)
  (create-image
   (make-vector
    width (make-bool-vector height nil))
   'xbm t :width width :height height))

(provide 'pacmacs-image)

;;; pacmacs-anim.el ends here
