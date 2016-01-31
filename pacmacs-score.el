;;; pacmacs-score.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Routines for working with score

;;; Code:

(require 'f)
(require 'dash)
(require 'dash-functional)

(defconst pacmacs--max-score-nick-size 8)
(defconst pacmacs--max-score-table-size 10)
(defconst pacmacs--score-file-name "~/.pacmacs-score")
(defconst pacmacs--score-buffer-name "*Pacmacs Score*")

(defun pacmacs--read-score-table ()
  (when (file-exists-p pacmacs--score-file-name)
    (->> pacmacs--score-file-name
         (f-read-text)
         (read-from-string)
         (car)
         (pacmacs--sort-score-table))))

(defun pacmacs--write-score-table (score-table)
  (with-temp-buffer
    (->> score-table
         (pacmacs--sort-score-table)
         (-take pacmacs--max-score-table-size)
         (pp-to-string)
         (insert))
    (write-file pacmacs--score-file-name)))

(defun pacmacs--sort-score-table (score-table)
  (sort score-table
        (-lambda ((_ . score1) (_ . score2))
          (> score1 score2))))

(defun pacmacs--position-of-new-score (score-table new-score)
  (->> score-table
       (-take-while (-lambda ((_ . score)) (< new-score score)))
       (length)))

(defun pacmacs--render-score-page (render-score-sign)
  (funcall render-score-sign)
  (let ((score-table (pacmacs--read-score-table)))
    (if score-table
        (pacmacs--render-score-table score-table)
      (insert "(there are not records yet)"))))

(defun pacmacs--render-score-table (score-table)
  (-each score-table #'pacmacs--render-score-record))

(defun pacmacs--add-entry-to-score-table (nickname score)
  (->> (pacmacs--read-score-table)
       (cons (cons nickname score))
       (pacmacs--write-score-table)))

(defun pacmacs--render-score-record (record)
  (-let (((nickname . score) record))
    (insert (format "%s%s %d\n"
                    nickname
                    (make-string (- pacmacs--max-score-nick-size
                                    (length nickname))
                                 ?\s)
                    score))))

(provide 'pacmacs-score)

;;; pacmacs-score.el ends here
