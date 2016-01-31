;;; pacmacs-board.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Routines for working with board

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'pacmacs-utils)

(defun pacmacs--make-board (width height)
  (let ((board (make-vector height nil)))
    (dotimes (row height)
      (aset board row (make-vector width nil)))
    (list :width width
          :height height
          :data board)))

(defun pacmacs--cell-wrapped-get (board row column)
  (plist-bind ((width :width)
               (height :height)
               (data :data))
      board
    (aref (aref data (mod row height))
          (mod column width))))

(defun pacmacs--cell-wrapped-set (board row column value)
  (plist-bind ((width :width)
               (height :height)
               (data :data))
      board
    (aset (aref data (mod row height))
          (mod column width)
          value)))

(defun pacmacs--object-type-at-p (board row column type)
  (let ((cell (pacmacs--cell-wrapped-get board row column)))
    (-find (-lambda (game-object)
             (plist-bind ((object-type :type))
                 game-object
               (equal object-type type)))
           cell)))

(defun pacmacs--step-point (board row column direction)
  (plist-bind ((width :width)
               (height :height))
      board
    (let* ((velocity (pacmacs--direction-vector direction))
           (d-row (car velocity))
           (d-column (cdr velocity)))
      (cons (mod (+ row d-row) height)
            (mod (+ column d-column) width)))))

(defun pacmacs--fill-board (board value)
  (plist-bind ((width :width)
               (height :height)
               (data :data))
      board
    (dotimes (row height)
      (dotimes (column width)
        (aset (aref data row) column value)))))

(provide 'pacmacs-board)

;;; pacmacs-board.el ends here

