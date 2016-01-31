;;; pacmacs-vector.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Routines for working with vector

;;; Code:

(defun pacmacs--vector-components-operation (vector1 vector2 operation)
  (-let (((row1 . column1) vector1)
         ((row2 . column2) vector2))
    (cons (funcall operation row1 row2)
          (funcall operation column1 column2))))

(defun pacmacs--vector- (vector1 vector2)
  (pacmacs--vector-components-operation
   vector1 vector2 #'-))

(defun pacmacs--squared-distance (row1 column1 row2 column2)
  (let ((d-row (- row2 row1))
        (d-column (- column2 column1)))
      (+ (* d-row d-row)
         (* d-column d-column))))

(provide 'pacmacs-vector)

;;; pacmacs-vector.el ends here
