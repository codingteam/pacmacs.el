;; Copyright (C) 2015-2016 Codingteam

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

(require 'cl)                           ;el-mock doesn't work without
                                        ;this
(require 'el-mock)
(require 'undercover)
(require 'dash)

(undercover "*.el")

(defun pacmacs--list-to-bool-vector (xs)
  (let* ((index 0)
         (size (length xs))
         (result (make-bool-vector (length xs) nil)))
    (dolist (x xs)
      (aset result index x)
      (cl-incf index))
    result))

(defun pacmacs--bool-vector-to-list (xs)
  (-map #'identity xs))

(defun pacmacs--construct-2d-bool-vector (data)
  (apply #'vector
         (-map #'pacmacs--list-to-bool-vector data)))

(defun pacmacs--bits-to-lists (bits)
  (-map #'pacmacs--bool-vector-to-list bits))

(add-to-list 'load-path ".")
(load "pacmacs.el")
