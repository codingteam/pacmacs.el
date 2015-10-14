;;; pacmacs-recorder.el --- Pacman for Emacs

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

;; Routines for recording integration test cases

;;; Code:

(require 'cl-lib)

(defvar pacmacs--tick-counter 0)
(defvar pacmacs--recorded-actions nil)

(defun pacmacs--reset-recorder ()
  (setq pacmacs--tick-counter 0)
  (setq pacmacs--recorded-actions nil))

(defun pacmacs--save-test-case (filename)
  (interactive "fFile to save the test case: ")
  (with-temp-buffer
    (-> score-table
        (pp-to-string)
        (insert))
    (write-file filename)))

(defun pacmacs--record-action (action-name)
  (add-to-list 'pacmacs--recorded-actions
               (cons action-name pacmacs--tick-counter)))

(defun pacmacs--inc-tick-counter ()
  (incf pacmacs--tick-counter))

(provide 'pacmacs-recorder)

;;; pacmacs-recorder.el ends here
