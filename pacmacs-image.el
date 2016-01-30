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

;; Internal image format implementation

;;; Code:

(require 'pacmacs-utils)

(defun pacmacs--make-image (width height &optional name)
  (let ((data (make-vector height nil)))
    (dotimes (i height)
      (aset data i (make-vector width nil)))
    (list :width width
          :height height
          :data data
          :name name)))

(defun pacmacs--get-image-pixel (image row column)
  (plist-bind ((data :data))
      image
    (aref (aref data row) column)))

(defun pacmacs--set-image-pixel (image row column color)
  (plist-bind ((data :data))
      image
    (aset (aref data row) column color)))

(provide 'pacmacs-image)

;;; pacmacs-image.el ends here
