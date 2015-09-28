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
(require 'pacman-image)
(require 'pacman-utils)

(defconst pacman-buffer-name "*Pacman*")
(defconst pacman-tick-duration-ms 100)

(defvar pacman-timer nil)
(defvar pacman-counter 0)

(defvar pacman-board-width 10)
(defvar pacman-board-height 10)

(defvar pacman-player-state nil)
(setq pacman-player-state
      (list :row 0
            :column 0
            :direction 'right
            :animation (pacman-load-anim "Red-Ghost-Right")))

(defvar pacman-direction-table nil)
(setq pacman-direction-table
      (list 'left  (cons -1 0)
            'right (cons 1 0)
            'up    (cons 0 -1)
            'down  (cons 0 1)))

(defvar pacman-direction-animation-table nil)
(setq pacman-direction-animation-table
      (list 'left  (pacman-load-anim "Red-Ghost-Left")
            'right (pacman-load-anim "Red-Ghost-Right")
            'up    (pacman-load-anim "Red-Ghost-Up")
            'down  (pacman-load-anim "Red-Ghost-Down")))

(defvar pacman-empty-cell nil)
(setq pacman-empty-cell
      (list :animation
            (pacman-make-anim '((0 0 40 40))
                              (pacman-create-transparent-block 40 40))))

(defvar pacman-score 0)

(defun pacman--make-wall-cell (row column)
  (list :animation (pacman-make-anim '((0 0 40 40))
                                     (pacman-create-color-block 40 40 "red"))
        :row row
        :column column))

(defvar pacman-wall-cells nil)
(setq pacman-wall-cells
      (mapcar (lambda (n)
                (pacman--make-wall-cell n n))
              (number-sequence 1 9)))


(defun pacman--make-pill (row column)
  (list :animation (pacman-load-anim "Pill")
        :row row
        :column column))

(defvar pacman-pills nil)
(setq pacman-pills
      (mapcar (lambda (n)
                (pacman--make-pill n (1+ n)))
              (number-sequence 1 8)))

(defun pacman-init-board (width height)
  (let ((board (make-vector height nil)))
    (dotimes (row height)
      (aset board row (make-vector width nil)))
    board))

(defvar pacman-board nil)
(setq pacman-board (pacman-init-board pacman-board-width
                                      pacman-board-height))

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
    (setq pacman-timer (run-at-time nil (* pacman-tick-duration-ms 0.001) 'pacman-tick))))

(defun pacman-destroy ()
  (when pacman-timer
    (cancel-timer pacman-timer)
    (setq pacman-timer nil)))

(defun pacman--kill-buffer-and-its-window (buffer-or-name)
  (let ((buffer-window (get-buffer-window buffer-or-name)))
    (if (and buffer-window
             (window-parent buffer-window))
        (with-current-buffer buffer-or-name
          (kill-buffer-and-window))
      (kill-buffer buffer-or-name))))

(defun pacman--object-at-p (row column objects)
  (member (cons row column)
          (mapcar (lambda (object)
                    (plist-bind ((row :row)
                                 (column :column))
                        object
                      (cons row column)))
                  objects)))

(defun pacman--wall-at-p (row column)
  (pacman--object-at-p row column pacman-wall-cells))

(defun pacman--pill-at-p (row column)
  (pacman--object-at-p row column pacman-pills))

(defun pacman-quit ()
  (interactive)
  (when (get-buffer pacman-buffer-name)
    (pacman--kill-buffer-and-its-window pacman-buffer-name)))

(defun pacman-step-object (game-object)
  (plist-bind ((row :row)
               (column :column)
               (direction :direction))
      game-object
    (let* ((velocity (plist-get pacman-direction-table direction))
           (new-row (+ row (cdr velocity)))
           (new-column (+ column (car velocity))))
      (when (and (<= 0 new-row (1- pacman-board-height))
                 (<= 0 new-column (1- pacman-board-width))
                 (not (pacman--wall-at-p new-row new-column)))
        (plist-put game-object :row new-row)
        (plist-put game-object :column new-column)))))

(defun pacman-tick ()
  (interactive)
  (with-current-buffer pacman-buffer-name
    (let ((inhibit-read-only t))
      (pacman-anim-object-next-frame pacman-player-state pacman-tick-duration-ms)
      (dolist (pill pacman-pills)
        (pacman-anim-object-next-frame pill pacman-tick-duration-ms))
      
      (pacman-step-object pacman-player-state)
      (let* ((direction (plist-get pacman-player-state :direction))
             (animation (plist-get pacman-direction-animation-table direction)))
        (plist-put pacman-player-state :animation animation))

      (plist-bind ((row :row)
                   (column :column))
          pacman-player-state
        (let ((pill (pacman--pill-at-p row column)))
          (when pill
            (setq pacman-score (+ pacman-score 10))
            (setq pacman-pills
                  (remove-if (lambda (pill)
                               (plist-bind ((p-row :row)
                                            (p-column :column))
                                   pill
                                 (and (= row p-row)
                                      (= column p-column))))
                             pacman-pills)))))

      (erase-buffer)
      (pacman-render-state))))

(defun pacman-render-object (anim-object)
  (let* ((anim (plist-get anim-object :animation))
         (sprite-sheet (plist-get anim :sprite-sheet))
         (current-frame (plist-get (pacman-anim-get-frame anim) :frame)))
    (pacman-insert-image sprite-sheet current-frame)))

(defun pacman-clear-board ()
  (dotimes (row pacman-board-height)
    (dotimes (column pacman-board-width)
      (aset (aref pacman-board row) column pacman-empty-cell))))

(defun pacman-put-object (anim-object)
  (plist-bind ((row :row)
               (column :column))
      anim-object
    (when (and (<= 0 row (1- pacman-board-height))
               (<= 0 column (1- pacman-board-width)))
      (aset (aref pacman-board row) column anim-object))))

(defun pacman-render-state ()
  (insert (format "Score: %d\n" pacman-score))

  (pacman-clear-board)
  (pacman-put-object pacman-player-state)

  (dolist (pill pacman-pills)
    (pacman-put-object pill))

  (dolist (wall pacman-wall-cells)
    (pacman-put-object wall))

  (dotimes (row pacman-board-height)
    (dotimes (column pacman-board-width)
      (let ((anim-object (aref (aref pacman-board row) column)))
        (pacman-render-object anim-object)))
    (insert "\n")))

(defun pacman-up ()
  (interactive)
  (plist-put pacman-player-state :direction 'up))

(defun pacman-down ()
  (interactive)
   (plist-put pacman-player-state :direction 'down))

(defun pacman-left ()
  (interactive)
  (plist-put pacman-player-state :direction 'left))

(defun pacman-right ()
  (interactive)
  (plist-put pacman-player-state :direction 'right))

(provide 'pacman)

;;; pacman.el ends here
