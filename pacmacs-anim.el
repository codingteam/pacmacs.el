;;; pacmacs-anim.el --- Pacman for Emacs -*- lexical-binding: t -*-

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

;; Routines for working with animation

;;; Code:

(require 'json)
(require 'dash)

(require 'pacmacs-walls)
(require 'pacmacs-utils)

(defun pacmacs-load-image (filename)
  (create-image filename 'xpm nil :heuristic-mask t))

(defun pacmacs-make-anim (frames sprite-sheet)
  (list :frames frames
        :current-frame 0
        :duration-counter 0
        :sprite-sheet sprite-sheet))

(defun pacmacs-make-frame (frame duration)
  (list :frame frame
        :duration duration))

(defun pacmacs-load-anim (animation-name)
  (let* ((aseprite-json-file (pacmacs--find-resource-file
                              (format "sprites/%s.json" animation-name)))
         (sprite-sheet-file (pacmacs--find-resource-file
                             (format "sprites/%s.xpm" animation-name)))
         (aseprite-json (json-read-file aseprite-json-file))
         (aseprite-frames (cdr (assoc 'frames aseprite-json)))
         (sprite-sheet (pacmacs-load-image sprite-sheet-file)))
    (pacmacs-make-anim
     (mapcar 'pacmacs-convert-aseprite-frame
             (sort aseprite-frames
                   'pacmacs-compare-aseprite-frames))
     sprite-sheet)))

(defun pacmacs-aseprite-frame-get-order (aseprite-frame)
  (let ((frame-name (symbol-name (car aseprite-frame))))
    (string-match "\\([0-9]+\\)\\.ase$" frame-name)
    (string-to-number (match-string 1 frame-name))))

(defun pacmacs-compare-aseprite-frames (aseprite-frame1 aseprite-frame2)
  (let ((order1 (pacmacs-aseprite-frame-get-order aseprite-frame1))
        (order2 (pacmacs-aseprite-frame-get-order aseprite-frame2)))
    (< order1 order2)))

(defun pacmacs-convert-aseprite-frame (aseprite-frame)
  (let* ((frame (cdr (assoc 'frame (cdr aseprite-frame))))
         (duration (cdr (assoc 'duration (cdr aseprite-frame)))))
    (pacmacs-make-frame (mapcar #'(lambda (n)
                                    (cdr (assoc n frame)))
                               '(x y w h))
                       duration)))

(defun pacmacs-anim-get-frame (anim)
  (plist-bind ((frames :frames)
               (current-frame :current-frame))
      anim
    (nth current-frame frames)))

(defun pacmacs-anim-next-frame (anim time)
  (plist-bind ((frames :frames)
               (current-frame :current-frame)
               (duration-counter :duration-counter))
      anim
    (let ((duration (plist-get (pacmacs-anim-get-frame anim) :duration)))
      (if (<= duration (+ duration-counter time))
          (let ((new-current-frame (mod (+ current-frame 1)
                                        (length frames))))
            (plist-put anim :duration-counter 0)
            (plist-put anim :current-frame new-current-frame))
        (plist-put anim :duration-counter (+ duration-counter time))))))

(defun pacmacs--anim-object-next-frame (anim-object time)
  (plist-map anim-object :current-animation
             #'(lambda (anim)
                 (pacmacs-anim-next-frame anim time))))

(defun pacmacs--anim-object-list-next-frame (anim-object-list time)
  (-each anim-object-list
    #'(lambda (anim-object)
        (pacmacs--anim-object-next-frame anim-object time))))

(provide 'pacmacs-anim)

;;; pacmacs-anim.el ends here
