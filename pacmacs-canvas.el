;;; pacmacs-canvas.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Canvas implementation

;;; Code:

(require 'dash)
(require 'pacmacs-image)

(defconst pacmacs--canvas-tile-width 40)
(defconst pacmacs--canvas-tile-height 40)

(defun pacmacs--make-tiles-grid (grid-width grid-height)
  (let ((grid (make-vector grid-height nil)))
    (dotimes (row grid-height)
      (->> (pacmacs--make-image pacmacs--canvas-tile-width
                                pacmacs--canvas-tile-height)
           (make-vector grid-width)
           (aset grid row)))
    grid))

(defun pacmacs--make-canvas (width height)
  (when (or (not (zerop (mod width pacmacs--canvas-tile-width))))
    (error (format "Width %d is not divisable by tile width %d"
                   width pacmacs--canvas-tile-width)))

  (when (or (not (zerop (mod width pacmacs--canvas-tile-height))))
    (error (format "Width %d is not divisable by tile height %d"
                   height pacmacs--canvas-tile-height)))

  (let ((grid-width (/ width pacmacs--canvas-tile-width))
        (grid-height (/ height pacmacs--canvas-tile-height)))
    (list :width width
          :height height
          :grid-width width
          :grid-height height
          :tiles (pacmacs--make-tiles-grid grid-width grid-height))))

(require 'pacmacs-image)

;;; pacmacs-canvas.el ends here
