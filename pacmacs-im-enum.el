;;; pacmacs-im-enum.el --- Pacman for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2016 Codingteam

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

;; Routines related to ImageMagick pixel enumeration format

;;; Code:

(require 'pacmacs-image)
(require 'pacmacs-utils)

(defun pacmacs--parse-im-enum-header (header)
  (let ((header-regexp (concat "# ImageMagick pixel enumeration: "
                               "\\([[:digit:]]+\\),\\([[:digit:]]+\\),"
                               "\\([[:digit:]]+\\)")))
    (if (string-match header-regexp header)
        (list (string-to-number (match-string 1 header))
              (string-to-number (match-string 2 header))
              (string-to-number (match-string 3 header)))
      (error "Incorrect ImageMagick pixel enumeration header: %s" header))))

(defun pacmacs--parse-im-enum-pixel (pixel colorspace)
  (let ((pixel-regexp (concat "\\([[:digit:]]+\\),\\([[:digit:]]+\\):[[:space:]]*"
                              "([[:space:]]*\\([[:digit:]]+\\),"
                              "[[:space:]]*\\([[:digit:]]+\\),"
                              "[[:space:]]*\\([[:digit:]]+\\),"
                              "[[:space:]]*\\([[:digit:]]+\\))")))
    (if (string-match pixel-regexp pixel)
        (list (string-to-number (match-string 1 pixel))
              (string-to-number (match-string 2 pixel))
              (let ((r (pacmacs--match-int 3 pixel))
                    (g (pacmacs--match-int 4 pixel))
                    (b (pacmacs--match-int 5 pixel))
                    (a (pacmacs--match-int 6 pixel))
                    (k (/ 1.0 colorspace)))
                (if (zerop a)
                    nil
                  (color-rgb-to-hex (* r k)
                                    (* g k)
                                    (* b k)))))
      (error "Incorrect ImageMagick pixel enumeration pixel: %s" pixel))))

(defun pacmacs--read-im-enum-image (filename)
  (-let* (((header-raw . data-raw) (-> filename
                                       (f-read-text)
                                       (split-string "\n" t)))
          (header (pacmacs--parse-im-enum-header header-raw)))
    (-let (((width height colorspace) header))
      (let ((image (pacmacs--make-image width height)))
        (-each data-raw
          (-lambda (pixel-raw)
            (-let (((x y color) (pacmacs--parse-im-enum-pixel pixel-raw colorspace)))
              (pacmacs--set-image-pixel image x y color))))
        image))))

(provide 'pacmacs-im-enum)

;;; pacmacs-im-enum.el ends here
