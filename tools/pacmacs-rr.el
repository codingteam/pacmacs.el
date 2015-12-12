;;; pacmacs-rr.el --- Pacman for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015 Codingteam

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

;; Additional module for recording and replaying integration test
;; cases.

;;; Code:

(require 'pacmacs)

(require 'dash)
(require 'f)
(require 'cl-lib)

(defvar pacmacs--tick-counter 0)
(defvar pacmacs--recorded-actions nil)
(defvar pacmacs--tick-times nil)
(defvar pacmacs-replay-finished-hook nil)

(defun pacmacs--record-action (action-name)
  (push (cons action-name pacmacs--tick-counter)
        pacmacs--recorded-actions))

(defun pacmacs--reset-recorder ()
  (setq pacmacs--tick-counter 0)
  (setq pacmacs--recorded-actions nil))

(defun pacmacs--save-test-case (filename)
  (interactive "FFile to save the test case: ")
  (with-temp-buffer
    (-> pacmacs--recorded-actions
        (reverse)
        (pp-to-string)
        (insert))
    (write-file filename)))

(defun pacmacs--load-test-case (filename)
  (-> (f-read-text filename)
      (read-from-string)
      (car)))

(defun pacmacs-record-up ()
  (interactive)
  (pacmacs--record-action 'pacmacs-up)
  (pacmacs-up))

(defun pacmacs-record-down ()
  (interactive)
  (pacmacs--record-action 'pacmacs-down)
  (pacmacs-down))

(defun pacmacs-record-left ()
  (interactive)
  (pacmacs--record-action 'pacmacs-left)
  (pacmacs-left))

(defun pacmacs-record-right ()
  (interactive)
  (pacmacs--record-action 'pacmacs-right)
  (pacmacs-right))

(defun pacmacs-record-tick ()
  (interactive)
  (cl-incf pacmacs--tick-counter)
  (pacmacs-tick))

(defun pacmacs-replay-tick ()
  (cl-incf pacmacs--tick-counter)

  (push (pacmacs--measure-time (pacmacs-tick))
        pacmacs--tick-times)

  (if pacmacs--recorded-actions
      (-let ((((action . tick-number) . _) pacmacs--recorded-actions))
        (when (= tick-number pacmacs--tick-counter)
          (funcall action)
          (setq pacmacs--recorded-actions (cdr pacmacs--recorded-actions))))
    (pacmacs-quit)
    (run-hooks 'pacmacs-replay-finished-hook)))

(define-derived-mode pacmacs-it-recorder-mode pacmacs-mode "pacmacs-it-recorder-mode"
  (define-key pacmacs-it-recorder-mode-map (kbd "<up>") 'pacmacs-record-up)
  (define-key pacmacs-it-recorder-mode-map (kbd "<down>") 'pacmacs-record-down)
  (define-key pacmacs-it-recorder-mode-map (kbd "<left>") 'pacmacs-record-left)
  (define-key pacmacs-it-recorder-mode-map (kbd "<right>") 'pacmacs-record-right))

(defun pacmacs--average-tick-time ()
  (/ (-sum pacmacs--tick-times) (length pacmacs--tick-times)))

(defun pacmacs--start-it-recorder ()
  (interactive)
  (pacmacs--initialize-game 'pacmacs-record-tick)
  (pacmacs-it-recorder-mode)
  (pacmacs--reset-recorder))

(defun pacmacs--start-it-replayer (filename)
  (interactive "fLoad test case: ")
  (pacmacs--initialize-game 'pacmacs-replay-tick)
  (pacmacs-mode)

  (setq pacmacs--recorded-actions (pacmacs--load-test-case filename))
  (setq pacmacs--tick-counter 0)
  (setq pacmacs--tick-times nil))

(provide 'pacmacs-rr)

;;; pacmacs-rr.el ends here
