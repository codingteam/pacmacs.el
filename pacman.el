;;; pacman.el --- Pacman for Emacs

;; Copyright (C) 2015 Codingteam

;; Author: Codingteam <codingteam@conference.jabber.ru>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/pacman.el
;; Version: 0.0.1

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

;; Pacman game for Emacs
;;
;; PLiOAioBxutV6QXPjBczSu7Xb_5kj-3KYA

;;; Code:

(require 'pacman-anim)
(require 'pacman-resources)

(defconst pacman-buffer-name "*Pacman*")

(defvar pacman-timer nil)
(defvar pacman-counter 0)

(defvar pacman-board-width 10)
(defvar pacman-board-height 10)
(defvar pacman-player-state nil)
(setq pacman-player-state
      (list :row 0
            :column 0
            :direction 'right
            :animation (pacman-load-anim "sprites/Pacman-Chomping-Right.json"
                                         "sprites/Pacman-Chomping-Right.png")))
(defvar pacman-empty-cell nil)
(setq pacman-empty-cell
      (list :animation (pacman-make-anim '((0 0 40 40))
                                         (create-image
                                          (make-vector 40 (make-bool-vector 40 nil)) 'xbm t :width 40 :height 40))))

(defun pacman-init-board (width height player-state)
  (let ((board (make-vector height nil)))
    (dotimes (row height)
      (aset board row (make-vector width pacman-empty-cell)))
    (aset (aref board 0) 0 player-state)
    board))

(defvar pacman-board nil)
(setq pacman-board (pacman-init-board pacman-board-width
                                      pacman-board-height
                                      pacman-player-state))

(define-derived-mode pacman-mode special-mode "pacman-mode"
  (define-key pacman-mode-map (kbd "<up>") 'pacman-up)
  (define-key pacman-mode-map (kbd "<down>") 'pacman-down)
  (define-key pacman-mode-map (kbd "<left>") 'pacman-left)
  (define-key pacman-mode-map (kbd "<right>") 'pacman-right)
  (define-key pacman-mode-map (kbd "q") 'pacman-quit)
  (add-hook 'kill-buffer-hook 'pacman-destroy nil t)
  (setq cursor-type nil))

(defun pacman-start ()
  (interactive)
  (switch-to-buffer-other-window pacman-buffer-name)
  (pacman-mode)
  (unless pacman-timer
    (setq pacman-timer (run-at-time nil 0.05 'pacman-tick))))

(defun pacman-destroy ()
  (when pacman-timer
    (cancel-timer pacman-timer)
    (setq pacman-timer nil)))

(defun pacman-quit ()
  (interactive)
  (when (get-buffer pacman-buffer-name)
    (with-current-buffer pacman-buffer-name
      (if (get-buffer-window pacman-buffer-name)
          (kill-buffer-and-window)
        (kill-buffer pacman-buffer-name)))))

(setq square-width 40)
(setq square-height 30)
(setq square-x 150)
(setq square-y 10)
(setq square-dx 1)
(setq square-dy 1)
(setq canvas-width 200)
(setq canvas-height 200)

(defvar frame nil)
(setq frame (make-vector canvas-height nil))
(dotimes (i canvas-height)
  (aset frame i (make-string canvas-width ?\s)))

(defun fits-frame (x y)
  (and (<= 0 x (1- canvas-width))
       (<= 0 y (1- canvas-height))))

(defun pacman-tick ()
  (interactive)
  (with-current-buffer pacman-buffer-name
    (let ((inhibit-read-only t))
      ;; Check borders
      (when (or (> (+ square-x square-width) canvas-width)
                (< square-x 0))
        (setq square-dx (- square-dx)))
      (when (or (> (+ square-y square-height) canvas-height)
                (< square-y 0))
        (setq square-dy (- square-dy)))

      ;; Move square
      (setq square-x (+ square-x square-dx))
      (setq square-y (+ square-y square-dy))

      ;; Render frame
      (dotimes (x canvas-width)
        (dotimes (y canvas-height)
          (aset (aref frame y) x ?\s)))
      (dotimes (x square-width)
        (dotimes (y square-height)
          (when (fits-frame (+ square-x x) (+ square-y y))
            (aset (aref frame (+ square-y y)) (+ square-x x) ?\.))))
      (erase-buffer)
      (insert-image
       (create-image
        (concat
         "/* XPM */\n"
         "static char *square[] = {\n"
         "/**/\n"
         (format "\"%d %d 2 1\",\n" canvas-width canvas-height)
         "\"  c #000000\",\n"
         "\". c #ffff00\",\n"
         "/* pixels */\n"
         (mapconcat
          (lambda (row)
            (format "\"%s\"" row))
          frame
          ",\n")
         "\n};")
        'xpm
        t)))))

(defun pacman-render-object (anim-object)
  (let* ((anim (plist-get anim-object :animation))
         (sprite-sheet (plist-get anim :sprite-sheet))
         (current-frame (pacman-anim-get-frame anim)))
    (pacman-insert-image sprite-sheet current-frame)))

(defun pacman-render-state ()
  (dotimes (row pacman-board-height)
    (dotimes (column pacman-board-width)
      (let ((anim-object (aref (aref pacman-board row) column)))
        (pacman-render-object anim-object)))
    (insert "\n")))

(defun pacman-up ()
  (interactive)
  )

(defun pacman-down ()
  (interactive)
  )

(defun pacman-left ()
  (interactive)
  )

(defun pacman-right ()
  (interactive)
  )

(provide 'pacman)

;;; pacman.el ends here
