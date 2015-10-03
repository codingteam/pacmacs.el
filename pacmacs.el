;;; pacmacs.el --- Pacman for Emacs

;; Copyright (C) 2015 Codingteam

;; Author: Codingteam <codingteam@conference.jabber.ru>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/pacmacs.el
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

;; Pacmacs game for Emacs
;;
;; PLiOAioBxutV6QXPjBczSu7Xb_5kj-3KYA

;;; Code:

(require 'dash)

(require 'pacmacs-anim)
(require 'pacmacs-image)
(require 'pacmacs-utils)

(defconst pacmacs-buffer-name "*Pacmacs*")
(defconst pacmacs-tick-duration-ms 100)
(defconst pacmacs-debug-output nil)

(defvar pacmacs-timer nil)

(defvar pacmacs-board-width 10)
(defvar pacmacs-board-height 10)
(defvar pacmacs-score 0)

(defvar pacmacs-direction-table nil)
(setq pacmacs-direction-table
      (list 'left  (cons -1 0)
            'right (cons 1 0)
            'up    (cons 0 -1)
            'down  (cons 0 1)))

(defvar pacmacs-inversed-direction-table nil)
(setq pacmacs-inversed-direction-table
      (list (cons (cons -1 0) 'left)
            (cons (cons 1 0) 'right)
            (cons (cons 0 -1) 'up)
            (cons (cons 0 1) 'down)))

(defvar pacmacs-player-state nil)

(defvar pacmacs-ghosts nil)
(defvar pacmacs-wall-cells nil)
(defvar pacmacs-pills nil)

(defvar pacmacs-empty-cell nil)

(defvar pacmacs-board nil)
(defvar pacmacs-track-board nil)

(define-derived-mode pacmacs-mode special-mode "pacmacs-mode"
  (define-key pacmacs-mode-map (kbd "<up>") 'pacmacs-up)
  (define-key pacmacs-mode-map (kbd "<down>") 'pacmacs-down)
  (define-key pacmacs-mode-map (kbd "<left>") 'pacmacs-left)
  (define-key pacmacs-mode-map (kbd "<right>") 'pacmacs-right)
  (define-key pacmacs-mode-map (kbd "q") 'pacmacs-quit)
  (add-hook 'kill-buffer-hook 'pacmacs-destroy nil t)
  (setq cursor-type nil))

(defun pacmacs-start ()
  (interactive)
  (switch-to-buffer-other-window pacmacs-buffer-name)
  (pacmacs-mode)
  (pacmacs-load-map "map02")
  (unless pacmacs-timer
    (setq pacmacs-timer (run-at-time nil (* pacmacs-tick-duration-ms 0.001) 'pacmacs-tick))))

(defun pacmacs-destroy ()
  (when pacmacs-timer
    (cancel-timer pacmacs-timer)
    (setq pacmacs-timer nil)))

(defun pacmacs--make-wall-cell (row column)
  (list :current-animation (pacmacs-make-anim (list (pacmacs-make-frame '(0 0 40 40) 100))
                                              (pacmacs-create-color-block 40 40 "red"))
        :row row
        :column column))

(defun pacmacs--make-pill (row column)
  (list :current-animation (pacmacs-load-anim "Pill")
        :row row
        :column column))

(defun pacmacs--make-ghost (row column)
  (list :row row
        :column column
        :direction 'right
        :current-animation (pacmacs-load-anim "Red-Ghost-Right")
        :direction-animations (list 'left  (pacmacs-load-anim "Red-Ghost-Left")
                                    'right (pacmacs-load-anim "Red-Ghost-Right")
                                    'up    (pacmacs-load-anim "Red-Ghost-Up")
                                    'down  (pacmacs-load-anim "Red-Ghost-Down"))
        :speed 1
        :speed-counter 0))

(defun pacmacs--make-player (row column)
  (list :row row
        :column column
        :direction 'right
        :current-animation (pacmacs-load-anim "Pacman-Chomping-Right")
        :direction-animations (list 'left  (pacmacs-load-anim "Pacman-Chomping-Left")
                                    'right (pacmacs-load-anim "Pacman-Chomping-Right")
                                    'up    (pacmacs-load-anim "Pacman-Chomping-Up")
                                    'down  (pacmacs-load-anim "Pacman-Chomping-Down"))
        :speed 0
        :speed-counter 0))

(defun pacmacs-init-board (width height)
  (let ((board (make-vector height nil)))
    (dotimes (row height)
      (aset board row (make-vector width nil)))
    board))

(defun pacmacs--kill-buffer-and-its-window (buffer-or-name)
  (let ((buffer-window (get-buffer-window buffer-or-name)))
    (if (and buffer-window
             (window-parent buffer-window))
        (with-current-buffer buffer-or-name
          (kill-buffer-and-window))
      (kill-buffer buffer-or-name))))

(defun pacmacs--object-at-p (row column objects)
  (member (cons (mod row pacmacs-board-height)
                (mod column pacmacs-board-width))
          (mapcar #'(lambda (object)
                      (plist-bind ((row :row)
                                   (column :column))
                          object
                        (cons row column)))
                  objects)))

(defun pacmacs--wall-at-p (row column)
  (pacmacs--object-at-p row column pacmacs-wall-cells))

(defun pacmacs--pill-at-p (row column)
  (pacmacs--object-at-p row column pacmacs-pills))

(defun pacmacs-quit ()
  (interactive)
  (when (get-buffer pacmacs-buffer-name)
    (pacmacs--kill-buffer-and-its-window pacmacs-buffer-name)))

(defun pacmacs--cell-tracked-p (row column)
  (aref (aref pacmacs-track-board (mod row pacmacs-board-height))
        (mod column pacmacs-board-width)))

(defun pacmacs--within-of-map-p (row column)
  (and (<= 0 row (1- pacmacs-board-height))
       (<= 0 column (1- pacmacs-board-width))))

(defun pacmacs--switch-direction (game-object direction)
  (plist-bind ((direction-animations :direction-animations))
      game-object
    (plist-put game-object :direction direction)
    (plist-put game-object :current-animation (plist-get direction-animations direction))))

(defun pacmacs--make-empty-cell ()
  (if pacmacs-empty-cell
      pacmacs-empty-cell
    (setq pacmacs-empty-cell
          (list :current-animation
                (pacmacs-make-anim (list (pacmacs-make-frame '(0 0 40 40) 100))
                                   (pacmacs-create-transparent-block 40 40))))))

(defun pacmacs-step-object (game-object)
  (plist-bind ((row :row)
               (column :column)
               (direction :direction)
               (speed-counter :speed-counter)
               (speed :speed))
      game-object
    (if (zerop speed-counter)
        (let* ((velocity (plist-get pacmacs-direction-table direction))
               (new-row (mod (+ row (cdr velocity))
                             pacmacs-board-height))
               (new-column (mod (+ column (car velocity))
                                pacmacs-board-width)))
          (plist-put game-object :speed-counter speed)
          (when (not (pacmacs--wall-at-p new-row new-column))
            (plist-put game-object :row new-row)
            (plist-put game-object :column new-column)))
      (plist-put game-object :speed-counter (1- speed-counter)))))

(defun pacmacs--fill-board (board width height value)
  (dotimes (row height)
    (dotimes (column width)
      (aset (aref board row) column value))))

(defun pacmacs--possible-ways (row column)
  (list (cons (1+ row) column)
        (cons row (1+ column))
        (cons (1- row) column)
        (cons row (1- column))))

(defun pacmacs--filter-candidates (p)
  (let ((row (car p))
        (column (cdr p)))
    (or (pacmacs--wall-at-p row column)
        (pacmacs--cell-tracked-p row column))))

(defun pacmacs--track-point (start end)
  (let* ((start-row (car start))
         (start-column (cdr start))

         (end-row (car end))
         (end-column (cdr end))

         (d-row (- end-row start-row))
         (d-column (- end-column start-column)))
    (aset (aref pacmacs-track-board (mod start-row pacmacs-board-height))
          (mod start-column pacmacs-board-width)
          (cdr
           (assoc (cons d-column
                        d-row)
                  pacmacs-inversed-direction-table)))))

(defun pacmacs--recalc-track-board ()
  (pacmacs--fill-board pacmacs-track-board
                       pacmacs-board-width
                       pacmacs-board-height
                       nil)
  (plist-bind ((player-row :row)
               (player-column :column))
      pacmacs-player-state
    (let ((wave (list (cons player-row player-column))))
      (while (not (null wave))
        (let ((next-wave nil))
          (dolist (p wave)
            (let* ((row (car p))
                   (column (cdr p))
                   (possible-ways (pacmacs--possible-ways row column))
                   (candidate-ways
                    (remove-if #'pacmacs--filter-candidates possible-ways)))
              (dolist (candidate-way candidate-ways)
                (pacmacs--track-point candidate-way p))
              (setq next-wave
                    (append next-wave candidate-ways))))
          (setq wave next-wave))))))

(defun pacmacs--track-object (game-object)
  (plist-bind ((row :row)
               (column :column))
      game-object
    (let ((direction (aref (aref pacmacs-track-board row) column)))
      (pacmacs--switch-direction game-object direction))))

(defun pacmacs-tick ()
  (interactive)
  (with-current-buffer pacmacs-buffer-name
    (let ((inhibit-read-only t))
      (pacmacs-anim-object-next-frame pacmacs-player-state pacmacs-tick-duration-ms)
      (dolist (ghost pacmacs-ghosts)
        (pacmacs-anim-object-next-frame ghost pacmacs-tick-duration-ms))
      (dolist (pill pacmacs-pills)
        (pacmacs-anim-object-next-frame pill pacmacs-tick-duration-ms))
      
      (pacmacs-step-object pacmacs-player-state)

      (plist-bind ((row :row)
                   (column :column))
          pacmacs-player-state
        (let ((pill (pacmacs--pill-at-p row column)))
          (when pill
            (setq pacmacs-score (+ pacmacs-score 10))
            (setq pacmacs-pills
                  (remove-if #'(lambda (pill)
                                 (plist-bind ((p-row :row)
                                              (p-column :column))
                                     pill
                                   (and (= row p-row)
                                        (= column p-column))))
                             pacmacs-pills)))))

      (pacmacs--recalc-track-board)
      (dolist (ghost pacmacs-ghosts)
        (pacmacs--track-object ghost)
        (pacmacs-step-object ghost))
      
      (erase-buffer)
      (pacmacs-render-state))))

(defun pacmacs-render-object (anim-object)
  (let* ((anim (plist-get anim-object :current-animation))
         (sprite-sheet (plist-get anim :sprite-sheet))
         (current-frame (plist-get (pacmacs-anim-get-frame anim) :frame)))
    (pacmacs-insert-image sprite-sheet current-frame)))

(defun pacmacs-put-object (anim-object)
  (plist-bind ((row :row)
               (column :column))
      anim-object
    (when (and (<= 0 row (1- pacmacs-board-height))
               (<= 0 column (1- pacmacs-board-width)))
      (aset (aref pacmacs-board row) column anim-object))))

(defun pacmacs-render-track-board ()
  (dotimes (row pacmacs-board-height)
    (dotimes (column pacmacs-board-width)
      (let ((x (aref (aref pacmacs-track-board row) column)))
        (cond
         ((null x)
          (insert "."))
         ((equal x 'left)
          (insert "<"))
         ((equal x 'right)
          (insert ">"))
         ((equal x 'up)
          (insert "^"))
         ((equal x 'down)
          (insert "v")))))
    (insert "\n")))

(defun pacmacs-render-state ()
  (insert (format "Score: %d\n" pacmacs-score))

  (when pacmacs-debug-output
    (pacmacs-render-track-board))

  (pacmacs--fill-board pacmacs-board
                       pacmacs-board-width
                       pacmacs-board-height
                       (pacmacs--make-empty-cell))

  (pacmacs-put-object pacmacs-player-state)

  (dolist (pill pacmacs-pills)
    (pacmacs-put-object pill))

  (dolist (ghost pacmacs-ghosts)
    (pacmacs-put-object ghost))
  
  (dolist (wall pacmacs-wall-cells)
    (pacmacs-put-object wall))

  (dotimes (row pacmacs-board-height)
    (dotimes (column pacmacs-board-width)
      (let ((anim-object (aref (aref pacmacs-board row) column)))
        (pacmacs-render-object anim-object)))
    (insert "\n")))

(defun pacmacs-up ()
  (interactive)
  (pacmacs--switch-direction pacmacs-player-state 'up))

(defun pacmacs-down ()
  (interactive)
  (pacmacs--switch-direction pacmacs-player-state 'down))

(defun pacmacs-left ()
  (interactive)
  (pacmacs--switch-direction pacmacs-player-state 'left))

(defun pacmacs-right ()
  (interactive)
  (pacmacs--switch-direction pacmacs-player-state 'right))

(defun pacmacs--file-content (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun pacmacs-load-map (map-name)
  (let* ((lines (split-string (pacmacs--file-content (format "maps/%s.txt" map-name)) "\n" t))
         (board-width (apply 'max (mapcar #'length lines)))
         (board-height (length lines)))
    (setq pacmacs-board-width board-width)
    (setq pacmacs-board-height board-height)

    (setq pacmacs-board (pacmacs-init-board pacmacs-board-width
                                            pacmacs-board-height))
    (setq pacmacs-track-board (pacmacs-init-board pacmacs-board-width
                                                  pacmacs-board-height))

    (setq pacmacs-wall-cells nil)
    (setq pacmacs-pills nil)
    (setq pacmacs-ghosts nil)

    (loop
     for line being the element of lines using (index row)
     do (loop for x being the element of line using (index column)
              do (cond ((char-equal x ?#)
                        (add-to-list 'pacmacs-wall-cells (pacmacs--make-wall-cell row column)))

                       ((char-equal x ?.)
                        (add-to-list 'pacmacs-pills (pacmacs--make-pill row column)))

                       ((char-equal x ?o)
                        (if (not pacmacs-player-state)
                            (setq pacmacs-player-state (pacmacs--make-player row column))
                          (plist-put pacmacs-player-state :row row)
                          (plist-put pacmacs-player-state :column column)))

                       ((char-equal x ?g)
                        (add-to-list 'pacmacs-ghosts (pacmacs--make-ghost row column))))))))

(provide 'pacmacs)

;;; pacmacs.el ends here
