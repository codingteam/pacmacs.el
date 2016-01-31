;;; pacmacs-image.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Internal image format implementation

;;; Code:

(require 'pacmacs-utils)

(defun pacmacs--make-image (width height &optional name)
  (let ((data (make-vector height nil)))
    (dotimes (i height)
      (aset data i (make-vector width nil)))
    (list :width width
          :height height
          :data data
          :name name)))

(defun pacmacs--get-image-pixel (image x y)
  (plist-bind ((data :data))
      image
    (aref (aref data y) x)))

(defun pacmacs--set-image-pixel (image x y color)
  (plist-bind ((data :data))
      image
    (aset (aref data y) x color)))

(defun pacmacs--palette-from-image (image)
  (plist-bind ((width :width)
               (height :height)
               (data :data))
      image
    (let ((palette '()))
      (dotimes (y height)
        (dotimes (x width)
          (let ((pixel (aref (aref data y) x)))
            (when pixel
              (push pixel palette)
              (delete-dups palette)))))
      palette)))

(defun pacmacs--make-palette-char-map (palette)
  (let* ((n (length palette))
         (palette-indices (number-sequence 0 (1- n))))
    (->> (-zip-with #'cons palette palette-indices)
         (-map (-lambda ((color . index))
                 (cons color (+ index ?a)))))))

(defun pacmacs--render-xpm-palette (palette-char-map)
  (->> palette-char-map
       (-map (-lambda ((color . char))
               (format "\"%c c %s\",\n" char color)))
       (apply #'concat)))

(defun pacmacs--generate-xpm-palette (palette)
  (->> palette
       (pacmacs--make-palette-char-map)
       (pacmacs--render-xpm-palette)))

(defun pacmacs--image-to-xpm (image)
  (plist-bind ((width :width)
               (height :height)
               (data :data)
               (name :name))
      image
    (let* ((palette (pacmacs--palette-from-image image))
           (palette-char-map (pacmacs--make-palette-char-map palette)))
      (concat
       "/* XPM */\n"
       "static char *" (if name name "image") "[] = {\n"
       "/**/\n"
       (format "\"%d %d %d 1\",\n" width height (1+ (length palette)))
       "\"  c None\",\n"
       (pacmacs--render-xpm-palette palette-char-map)
       "/* pixels */\n"
       (mapconcat
        (lambda (row)
          (format "\"%s\""
                  (mapconcat (-lambda (color)
                               (let ((char (cdr (assoc color palette-char-map))))
                                 (if char (format "%c" char) " ")))
                             row "")))
        data
        ",\n")
       "\n};"))))

(provide 'pacmacs-image)

;;; pacmacs-image.el ends here
