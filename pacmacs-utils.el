;;; pacmacs-utils.el --- Pacmacs for Emacs

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

;; Some common utils

;;; Code:

(defmacro plist-bind (keys expr &rest body)
  (declare (indent 2) (debug t))
  (let ((expr-name (gensym)))
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

(provide 'pacmacs-utils)

;;; pacmacs.el ends here
