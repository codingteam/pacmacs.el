;;; pacmacs-utils.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Some common utils

;;; Code:

(require 'cl-lib)

(defconst pacmacs--base (file-name-directory load-file-name))

(defmacro plist-bind (keys expr &rest body)
  (declare (indent 2) (debug (sexp form &rest form)))
  (let ((expr-name (cl-gensym)))
    `(let* ((,expr-name ,expr)
            ,@(mapcar #'(lambda (key)
                          (cons (car key)
                                `((plist-get ,expr-name ,(cadr key)))))
                      keys))
       ,@body)))

(defun plist-map (plist property transformer)
  "Transform the value of PROPERTY in PLIST with TRANSFORMER.
This function modifies plist with plist-put. So it does the same
side-effects."
  (plist-bind ((value property)) plist
    (plist-put plist property
               (funcall transformer value))))

(defun pacmacs--find-resource-file (filename)
  (expand-file-name filename pacmacs--base))

(defun pacmacs--direction-vector (direction)
  (let ((direction-table (list 'left  (cons  0 -1)
                               'right (cons  0  1)
                               'up    (cons -1  0)
                               'down  (cons  1  0))))
    (plist-get direction-table direction)))

(defun pacmacs--direction-name (direction-vector)
  (let ((direction-table '((( 0 . -1) . left)
                           (( 0 .  1) . right)
                           ((-1 .  0) . up)
                           (( 1 .  0) . down))))
    (cdr (assoc direction-vector
                direction-table))))

(defun pacmacs--levelname-from-filename (filename)
  (when (string-match "\\(map[0-9]+\\)\\.txt" filename)
    (match-string 1 filename)))

(provide 'pacmacs-utils)

;;; pacmacs.el ends here
