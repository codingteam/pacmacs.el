;;; pacmacs-image.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Routines for working with game resources

;;; Code:

(require 'dash)

(defconst pacmacs--flip-xbm-bits (eq system-type 'windows-nt))
(defvar pacmacs--wall-blocks (make-vector 256 nil))

(defun pacmacs-load-image (filename)
  (create-image filename 'xpm nil :heuristic-mask t))

(defun pacmacs-insert-image (resource resource-vector)
  (insert-image resource " " nil resource-vector))

(defun pacmacs-create-color-block (width height color)
  (apply
   #'create-image
   (make-vector
    width (make-bool-vector height t))
   'xbm t :width width :height height
   (if (not pacmacs--flip-xbm-bits)
       (list
        :foreground color
        :background nil)
     (list
      :foreground nil
      :background color))))

(defun pacmacs--put-bits-dot (bits row column weight)
  (dotimes (i weight)
    (dotimes (j weight)
      (aset (aref bits (+ i row)) (+ j column) t))))

(defun pacmacs--put-vertical-bar (bits column height weight)
  (dotimes (w weight)
    (dotimes (i height)
      (aset (aref bits i) (+ column w) t))))

(defun pacmacs--put-horizontal-bar (bits row width weight)
  (dotimes (w weight)
    (dotimes (i width)
      (aset (aref bits (+ row w)) i t))))

(defun pacmacs--bit-list-to-integer (bit-list)
  (let ((result 0))
    (dolist (bit bit-list)
      (setq result (logior (lsh result 1)
                           (if bit 1 0))))
    result))

(defun pacmacs--create-wall-block (width
                                   height color

                                   bottom right
                                   top left
                                   left-upper right-upper
                                   left-bottom right-bottom)
  (let ((cache-index
         (pacmacs--bit-list-to-integer
          (list bottom right top left
                left-upper right-upper
                left-bottom right-bottom))))
    (-if-let (cached-tile (aref pacmacs--wall-blocks cache-index))
        cached-tile
      (aset pacmacs--wall-blocks cache-index
            (let ((wall-block (make-vector
                               width nil))
                  (weight 3))

              (dotimes (i width)
                (aset wall-block i (make-bool-vector height nil)))

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

              (create-image wall-block 'xbm t :width width :height height
                            :foreground color
                            :background nil))))))

(defun pacmacs-create-transparent-block (width height)
  (create-image
   (make-vector
    width (make-bool-vector height nil))
   'xbm t :width width :height height))

(provide 'pacmacs-image)

;;; pacmacs-anim.el ends here
