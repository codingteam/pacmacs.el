;;; pacman-anim.el --- Pacman for Emacs

;; Copyright (C) 2015 Codingteam

;; Author: Codingteam <codingteam@conference.jabber.ru>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/pacman.el

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

;; Routines for working with animation

;;; Code:

(require 'json)

(require 'pacman-resources)

(defun pacman-make-anim (frames sprite-sheet)
  (list :frames frames
        :current-frame 0
        :sprite-sheet sprite-sheet))

(defun pacman-load-anim (aseprite-json-file sprite-sheet-file)
  (let* ((aseprite-json (json-read-file aseprite-json-file))
         (aseprite-frames (cdr (assoc 'frames aseprite-json)))
         (sprite-sheet (pacman-load-image sprite-sheet-file)))
    (pacman-make-anim
     (mapcar 'pacman-convert-aseprite-frame
             (sort aseprite-frames
                   'pacman-compare-aseprite-frames))
     sprite-sheet)))

(defun pacman-aseprite-frame-get-order (aseprite-frame)
  (let ((frame-name (symbol-name (car aseprite-frame))))
    (string-match "\\([0-9]+\\)\\.ase$" frame-name)
    (string-to-number (match-string 1 frame-name))))

(defun pacman-compare-aseprite-frames (aseprite-frame1 aseprite-frame2)
  (let ((order1 (pacman-aseprite-frame-get-order aseprite-frame1))
        (order2 (pacman-aseprite-frame-get-order aseprite-frame2)))
    (< order1 order2)))

(defun pacman-convert-aseprite-frame (aseprite-frame)
  (let* ((frame (cdr (assoc 'frame (cdr aseprite-frame))))
         (x (cdr (assoc 'x frame)))
         (y (cdr (assoc 'y frame)))
         (w (cdr (assoc 'w frame)))
         (h (cdr (assoc 'h frame))))
    (list x y w h)))

(defun pacman-anim-get-frame (anim)
  (let ((frames (plist-get anim :frames))
        (current-frame (plist-get anim :current-frame)))
    (nth current-frame frames)))

(defun pacman-anim-next-frame (anim)
  (let* ((frames (plist-get anim :frames))
         (current-frame (plist-get anim :current-frame))
         (new-current-frame (mod (+ current-frame 1)
                                 (length frames))))
    (plist-put anim :current-frame new-current-frame)))

(defun pacman-anim-object-next-frame (anim-object)
  (let ((anim (plist-get anim-object :animation)))
    (plist-put anim-object :animation
               (pacman-anim-next-frame anim))))

(provide 'pacman-anim)

;;; pacman-anim.el ends here
