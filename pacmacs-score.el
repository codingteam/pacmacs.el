;;; pacmacs-score.el --- Pacman for Emacs

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

;; Routines for working with score

;;; Code:

(require 'dash)
(require 'dash-functional)

(defconst pacmacs--score-file-name "~/.pacmacs-score")
(defconst pacmacs--score-buffer-name "*Pacmacs Score*")

(defun pacmacs-score ()
  (interactive)
  (switch-to-buffer-other-window pacmacs--score-buffer-name)
  (text-mode)
  (read-only-mode)
  (with-current-buffer pacmacs--score-buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer)
      (-> (pacmacs--read-score-table)
          (pacmacs--sort-score-table)
          (pacmacs--render-score-table)))))

(defun pacmacs--read-score-table ()
  (-> pacmacs--score-file-name
      (pacmacs--file-content)
      (read-from-string)
      (car)))

(defun pacmacs--write-score-table (score-table)
  (with-temp-buffer
    (-> score-table
        (pp-to-string)
        (insert))
    (write-file pacmacs--score-file-name)))

(defun pacmacs--sort-score-table (score-table)
  (sort score-table
        (-lambda ((_ . score1) (_ . score2))
          (> score1 score2))))

(defun pacmacs--render-score-table (score-table)
  (let ((max-nickname-length
         (--> (pacmacs--read-score-table)
              (-map (-compose #'length #'car) it)
              (apply #'max it))))
    (-each score-table
      (-lambda ((nickname . score))
        (insert (format "%s%s %d\n"
                        nickname
                        (make-string (- max-nickname-length
                                        (length nickname))
                                     ?\s)
                        score))))))

(defun pacmacs--add-entry-to-score-table (nickname score)
  (->> (pacmacs--read-score-table)
       (cons (cons nickname score))
       (pacmacs--sort-score-table)
       (-take 10)
       (pacmacs--write-score-table)))

(defun pacmacs--register-new-score (score)
  (let ((nickname (read-from-minibuffer "Nickname: ")))
    (pacmacs--add-entry-to-score-table nickname score)))

(provide 'pacmacs-score)

;;; pacmacs-score.el ends here
