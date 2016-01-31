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

(defun pacmacs--make-image-data (width height)
  (let ((data (make-vector height nil)))
    (dotimes (i height)
      (aset data i (make-vector width nil)))
    data))

(defun pacmacs--make-image (width height &optional name)
  (list :width width
        :height height
        :data (pacmacs--make-image-data width height)
        :name name))

(defun pacmacs--get-image-pixel (image x y)
  (plist-bind ((data :data))
      image
    (aref (aref data y) x)))

(defun pacmacs--set-image-pixel (image x y color)
  (plist-bind ((data :data))
      image
    (aset (aref data y) x color)))

(defun pacmacs--image-width (image)
  (plist-get image :width))

(defun pacmacs--image-height (image)
  (plist-get image :height))

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

(defun pacmacs--make-image-from-data (raw-data)
  (let* ((height (length raw-data))
         (width (->> raw-data
                     (-map #'length)
                     (apply #'max)))
         (data (pacmacs--make-image-data width height)))
    (dotimes (y (length raw-data))
      (dotimes (x (length (aref raw-data y)))
        (let ((color (aref (aref raw-data y) x)))
          (aset (aref data y) x color))))
    (list :width width
          :height height
          :data data
          :name nil)))

(defun pacmacs--draw-image (canvas-image image x y)
  (plist-bind ((image-width :width)
               (image-height :height)
               (image-data :data))
      image
    (plist-bind ((canvas-width :width)
                 (canvas-height :height)
                 (canvas-data :data))
        canvas-image
      (dotimes (image-y (length image-data))
        (dotimes (image-x (length (aref image-data image-y)))
          (let ((image-color (aref (aref image-data image-y) image-x))
                (canvas-x (+ image-x x))
                (canvas-y (+ image-y y)))
            (when (and (<= 0 canvas-x (1- canvas-width))
                       (<= 0 canvas-y (1- canvas-height)))
              (aset (aref canvas-data canvas-y) canvas-x
                    image-color))))))))

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
