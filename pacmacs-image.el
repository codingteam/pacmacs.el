;;; pacmacs-image.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Routines for working with game resources

;;; Code:

(require 'dash)

(defconst pacmacs--wall-palette ["#1111bb"
                                 "#3333dd"
                                 "#5555ff"])

(defvar pacmacs--wall-blocks
  (make-hash-table))

(defun pacmacs-load-image (filename)
  (create-image filename 'xpm nil :heuristic-mask t))

(defun pacmacs-insert-image (resource resource-vector)
  (insert-image resource " " nil resource-vector))

(defun pacmacs--put-bits-dot (bits row column weight)
  (dotimes (i weight)
    (dotimes (j weight)
      (aset (aref bits (+ i row)) (+ j column) 0))))

(defun pacmacs--put-vertical-bar (bits column height weight)
  (dotimes (w weight)
    (dotimes (i height)
      (aset (aref bits i) (+ column w) w))))

(defun pacmacs--put-horizontal-bar (bits row width weight)
  (dotimes (w weight)
    (dotimes (i width)
      (aset (aref bits (+ row w)) i w))))

(defun pacmacs--bit-list-to-integer (bit-list)
  (let ((result 0))
    (dolist (bit bit-list)
      (setq result (logior (lsh result 1)
                           (if bit 1 0))))
    result))

(defun pacmacs--generate-xpm-palette (palette)
  (let* ((n (length palette))
         (palette-indices (number-sequence 0 (1- n))))
    (->> (-map #'identity palette)
         (-zip-with #'cons palette-indices)
         (-map (-lambda ((index . color))
                 (format "\"%c c %s\",\n" (+ index ?a) color)))
         (apply #'concat))))

(defun pacmacs--bits-to-xpm (bits width height)
  (concat
   "/* XPM */\n"
   "static char *tile[] = {\n"
   "/**/\n"
   (format "\"%d %d %d 1\",\n" width height (1+ (length pacmacs--wall-palette)))
   "\"  c None\",\n"
   (pacmacs--generate-xpm-palette pacmacs--wall-palette)
   "/* pixels */\n"
   (mapconcat
    (lambda (row)
      (format "\"%s\""
              (mapconcat (-lambda (bit)
                           (if bit (format "%c" (+ bit ?a)) " "))
                         row "")))
    bits
    ",\n")
   "\n};"))

(defun pacmacs--normalize-wall-bits (wall-bits)
  (-let (((bottom right top left left-upper right-upper left-bottom right-bottom)
          wall-bits))
    (list bottom right top left
          (and left-upper   (not left)  (not top))
          (and right-upper  (not right) (not top))
          (and left-bottom  (not left)  (not bottom))
          (and right-bottom (not right) (not bottom)))))

(defun pacmacs--create-wall-tile (width height
                                  bottom right
                                  top left
                                  left-upper right-upper
                                  left-bottom right-bottom)
  (let* ((wall-bits (list bottom right top left
                          left-upper right-upper
                          left-bottom right-bottom))
         (cache-index (-> wall-bits
                          (pacmacs--normalize-wall-bits)
                          (pacmacs--bit-list-to-integer))))
    (-if-let (cached-tile (gethash cache-index pacmacs--wall-blocks))
        cached-tile
      (puthash cache-index
               (let ((wall-block (make-vector width nil))
                     (weight 3))

                 (dotimes (i width)
                   (aset wall-block i (make-vector height nil)))

                 (when left-upper
                   (pacmacs--put-bits-dot wall-block 0 0 weight))

                 (when right-upper
                   (pacmacs--put-bits-dot wall-block 0 (- width weight) weight))

                 (when left-bottom
                   (pacmacs--put-bits-dot wall-block (- height weight) 0 weight))

                 (when right-bottom
                   (pacmacs--put-bits-dot wall-block (- height weight) (- width weight) weight))

                 (when left
                   (pacmacs--put-vertical-bar wall-block 0 height weight))

                 (when right
                   (pacmacs--put-vertical-bar wall-block (- width weight) height weight))

                 (when top
                   (pacmacs--put-horizontal-bar wall-block 0 width weight))
                 
                 (when bottom
                   (pacmacs--put-horizontal-bar wall-block (- height weight) width weight))

                 (create-image (pacmacs--bits-to-xpm wall-block width height)
                               'xpm t))
               pacmacs--wall-blocks))))

(provide 'pacmacs-image)

;;; pacmacs-anim.el ends here
