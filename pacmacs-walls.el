;;; pacmacs-walls.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Routines for working with game resources

;;; Code:

(require 'dash)
(require 'color)

(require 'pacmacs-vector)

(defconst pacmacs--wall-color "#5555ff")
(defconst pacmacs--wall-weight 10)

(defvar pacmacs--wall-tiles-cache
  (make-hash-table))

(defun pacmacs--clear-wall-tiles-cache ()
  (interactive)
  (clrhash pacmacs--wall-tiles-cache))

(defun pacmacs--put-wall-tile-corner (bits row column weight weights-to-color)
  (dotimes (i weight)
    (dotimes (j weight)
      (aset (aref bits (+ i row)) (+ j column) (funcall weights-to-color i j)))))

(defun pacmacs--put-vertical-bar (bits column height weight weight-to-color)
  (dotimes (w weight)
    (dotimes (i height)
      (aset (aref bits i) (+ column w) (funcall weight-to-color w)))))

(defun pacmacs--put-horizontal-bar (bits row width weight weight-to-color)
  (dotimes (w weight)
    (dotimes (i width)
      (aset (aref bits (+ row w)) i (funcall weight-to-color w)))))

(defun pacmacs--bit-list-to-integer (bit-list)
  (let ((result 0))
    (dolist (bit bit-list)
      (setq result (logior (lsh result 1)
                           (if bit 1 0))))
    result))

(defun pacmacs--generate-xpm-palette (palette)
  (let* ((n (length palette))
         (palette-indices (number-sequence 0 (1- n))))
    (->> palette
         (-zip-with #'cons palette-indices)
         (-map (-lambda ((index . color))
                 (format "\"%c c %s\",\n" (+ index ?a) color)))
         (apply #'concat))))

(defun pacmacs--color-hex-gradient (start stop step-number)
  (-map (-lambda (color)
          (apply #'color-rgb-to-hex color))
        (color-gradient
         (color-name-to-rgb start)
         (color-name-to-rgb stop)
         step-number)))

(defun pacmacs--wall-tile-to-xpm (wall-tile width height palette)
  (concat
   "/* XPM */\n"
   "static char *tile[] = {\n"
   "/**/\n"
   (format "\"%d %d %d 1\",\n" width height (1+ (length palette)))
   "\"  c None\",\n"
   (pacmacs--generate-xpm-palette palette)
   "/* pixels */\n"
   (mapconcat
    (lambda (row)
      (format "\"%s\""
              (mapconcat (-lambda (bit)
                           (if bit (format "%c" (+ bit ?a)) " "))
                         row "")))
    wall-tile
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

(defun pacmacs--inverted-weight-to-color (weight)
  (-lambda (w) (- weight w 1)))

(defun pacmacs--two-weights-to-color (row-inverted column-inverted color-inverted weight)
  (-lambda (row-weight column-weight)
    (let ((color (->> (pacmacs--squared-distance
                       0 0
                       (if row-inverted
                           (- weight row-weight 1)
                         row-weight)
                       (if column-inverted
                           (- weight column-weight 1)
                         column-weight))
                      (sqrt)
                      (floor)
                      (min (1- weight)))))
      (if color-inverted
          (- weight color 1)
        color))))

(defun pacmacs--wall-bits-get-corners (wall-bits)
  (-drop 4 wall-bits))

(defun pacmacs--wall-bits-get-bars (wall-bits)
  (-take 4 wall-bits))

(defun pacmacs--put-inner-corners (wall-tile width height weight wall-bits)
  (-let (((left-upper right-upper left-bottom right-bottom)
          (pacmacs--wall-bits-get-corners wall-bits)))
    (when left-upper
      (pacmacs--put-wall-tile-corner wall-tile 0 0 weight
                                     (pacmacs--two-weights-to-color nil nil nil weight)))

    (when right-upper
      (pacmacs--put-wall-tile-corner wall-tile 0 (- width weight) weight
                                     (pacmacs--two-weights-to-color nil t nil weight)))

    (when left-bottom
      (pacmacs--put-wall-tile-corner wall-tile (- height weight) 0 weight
                                     (pacmacs--two-weights-to-color t nil nil weight)))

    (when right-bottom
      (pacmacs--put-wall-tile-corner wall-tile (- height weight) (- width weight) weight
                                     (pacmacs--two-weights-to-color t t nil weight)))))

(defun pacmacs--put-bars (wall-tile width height weight wall-bits)
  (-let (((bottom right top left)
          (pacmacs--wall-bits-get-bars wall-bits)))
    (when left
      (pacmacs--put-vertical-bar wall-tile 0 height weight #'identity))

    (when right
      (pacmacs--put-vertical-bar wall-tile (- width weight) height weight
                                 (pacmacs--inverted-weight-to-color weight)))

    (when top
      (pacmacs--put-horizontal-bar wall-tile 0 width weight #'identity))
    
    (when bottom
      (pacmacs--put-horizontal-bar wall-tile (- height weight) width weight
                                   (pacmacs--inverted-weight-to-color weight)))))

(defun pacmacs--put-outer-corners (wall-tile width height weight wall-bits)
  (-let (((bottom right top left)
          (pacmacs--wall-bits-get-bars wall-bits)))
    (when (and left top) ;left-upper
      (pacmacs--put-wall-tile-corner wall-tile 0 0 weight
                                     (pacmacs--two-weights-to-color t t t weight)))

    (when (and right top) ;right-upper
      (pacmacs--put-wall-tile-corner wall-tile 0 (- width weight) weight
                                     (pacmacs--two-weights-to-color t nil t weight)))

    (when (and left bottom) ;left-bottom
      (pacmacs--put-wall-tile-corner wall-tile (- height weight) 0 weight
                                     (pacmacs--two-weights-to-color nil t t weight)))

    (when (and right bottom) ;right-bottom
      (pacmacs--put-wall-tile-corner wall-tile (- height weight) (- width weight) weight
                                     (pacmacs--two-weights-to-color nil nil t weight)))))

(defun pacmacs--create-wall-tile (width height wall-bits)
  "Creates a wall tile based on the WALL-BITS.
WALL-BITS go as follow (bottom right top left left-upper
right-upper left-bottom right-bottom). WIDTH and HEIGHT are the
size of the tile. All the created tiles are cached."
  (let* ((cache-index (-> wall-bits
                          (pacmacs--normalize-wall-bits)
                          (pacmacs--bit-list-to-integer))))
    (-if-let (cached-tile (gethash cache-index pacmacs--wall-tiles-cache))
        cached-tile
      (puthash cache-index
               (let* ((wall-tile (make-vector width nil))
                      (palette (pacmacs--color-hex-gradient
                                (face-attribute 'default :background)
                                pacmacs--wall-color
                                pacmacs--wall-weight)))

                 (dotimes (i width)
                   (aset wall-tile i (make-vector height nil)))

                 (pacmacs--put-inner-corners wall-tile width height pacmacs--wall-weight wall-bits)
                 (pacmacs--put-bars wall-tile width height pacmacs--wall-weight wall-bits)
                 (pacmacs--put-outer-corners wall-tile width height pacmacs--wall-weight wall-bits)

                 (create-image (pacmacs--wall-tile-to-xpm wall-tile width height palette)
                               'xpm t))
               pacmacs--wall-tiles-cache))))

(provide 'pacmacs-walls)

;;; pacmacs-anim.el ends here
