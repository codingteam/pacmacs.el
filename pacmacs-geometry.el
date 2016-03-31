;;; pacmacs-geometry.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Geometry routines

;;; Code:

(require 'dash)
(require 'pacmacs-vector)

(defun pacmacs--point-within-rect-p (point rect)
  (-let (((point-x point-y) point)
         ((rect-x rect-y rect-width rect-height) rect))
    (and (<= rect-x point-x (1- (+ rect-x rect-width)))
         (<= rect-y point-y (1- (+ rect-y rect-height))))))

(defun pacmacs--rect-rbc (rect)
  "Returns coordinates of the right-bottom corner of the rectangle."
  (-let (((rect-x rect-y rect-width rect-height) rect))
    (list (1- (+ rect-x rect-width))
          (1- (+ rect-y rect-height)))))

(defun pacmacs--rect-luc (rect)
  "Returns coordinates of the left-upper corner of the rectangle."
  (-let (((rect-x rect-y rect-width rect-height) rect))
    (list rect-x rect-y)))

(defun pacmacs--point-to-rect (lu-point rb-point)
  (-let (((lu-x lu-y) lu-point)
         ((rb-x rb-y) rb-point))
    (list lu-x lu-y
          (1+ (- rb-x lu-x))
          (1+ (- rb-y lu-y)))))

(defun pacmacs--rects-intersection (rect-1 rect-2)
  (cond
   ((-> (pacmacs--rect-rbc rect-1)
        (pacmacs--point-within-rect-p rect-2))
    (if (-> (pacmacs--rect-luc rect-1)
            (pacmacs--point-within-rect-p rect-2))
        rect-1
      (pacmacs--point-to-rect
       (pacmacs--rect-luc rect-2)
       (pacmacs--rect-rbc rect-1))))
   ((-> (pacmacs--rect-rbc rect-2)
        (pacmacs--point-within-rect-p rect-1))
    (if (-> (pacmacs--rect-luc rect-2)
            (pacmacs--point-within-rect-p rect-1))
        rect-2
      (pacmacs--point-to-rect
       (pacmacs--rect-luc rect-1)
       (pacmacs--rect-rbc rect-2))))))

(provide 'pacmacs-geometry)

;;; pacmacs-geometry.el ends here
