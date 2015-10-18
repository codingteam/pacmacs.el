;;; pacmacs.el --- Pacman for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015 Codingteam

;; Author: Codingteam <codingteam@conference.jabber.ru>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/pacmacs.el
;; Version: 0.0.1
;; Package-Requires: ((dash "2.11.0") (dash-functional "1.2.0") (cl-lib "0.5") (f "0.18.0"))

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

(require 'cl-lib)
(require 'dash)

(require 'pacmacs-anim)
(require 'pacmacs-board)
(require 'pacmacs-image)
(require 'pacmacs-utils)
(require 'pacmacs-render)
(require 'pacmacs-score)

(defconst pacmacs-buffer-name "*Pacmacs*")
(defconst pacmacs-tick-duration-ms 100)

(defvar pacmacs-debug-output nil)

(defvar pacmacs-timer nil)

(defvar pacmacs-score 0)

(defvar pacmacs--player-state nil)
(defvar pacmacs--ghosts nil)
(defvar pacmacs--wall-cells nil)
(defvar pacmacs--pills nil)

(defvar pacmacs--object-board nil)
(defvar pacmacs--track-board nil)

(defvar pacmacs-play-pause nil)

;;; play death prepare level-beaten game-over
(defvar pacmacs-game-state 'play)

(defvar pacmacs-lives 3)

(defvar pacmacs-levels nil)
(defvar pacmacs-current-level 0)

(defvar pacmacs-waiting-counter 0)

(define-derived-mode pacmacs-mode special-mode "pacmacs-mode"
  (define-key pacmacs-mode-map (kbd "<up>") 'pacmacs-up)
  (define-key pacmacs-mode-map (kbd "<down>") 'pacmacs-down)
  (define-key pacmacs-mode-map (kbd "<left>") 'pacmacs-left)
  (define-key pacmacs-mode-map (kbd "<right>") 'pacmacs-right)
  (define-key pacmacs-mode-map (kbd "q") 'pacmacs-quit)
  (define-key pacmacs-mode-map (kbd "SPC") 'pacmacs-pause)
  (add-hook 'kill-buffer-hook 'pacmacs-destroy nil t)
  (setq cursor-type nil)
  (setq truncate-lines t))

;;;###autoload
(defun pacmacs-start ()
  (interactive)
  (pacmacs--initialize-game 'pacmacs-tick)
  (pacmacs-mode))

(defun pacmacs--initialize-game (tick-function)
  (switch-to-buffer pacmacs-buffer-name)

  (setq pacmacs-lives 3)
  (setq pacmacs-score 0)
  (setq pacmacs-levels (pacmacs--get-list-of-levels))
  (setq pacmacs-current-level 0)

  (pacmacs--load-current-level)
  (pacmacs--switch-to-play-state)

  (unless pacmacs-timer
    (setq pacmacs-timer (run-at-time nil (* pacmacs-tick-duration-ms 0.001)
                                     tick-function))))

(defun pacmacs-destroy ()
  (when pacmacs-timer
    (cancel-timer pacmacs-timer)
    (setq pacmacs-timer nil)))

(defun pacmacs--load-current-level ()
  (pacmacs--load-map (aref pacmacs-levels
                          pacmacs-current-level)))

(defun pacmacs--load-next-level ()
  (setq pacmacs-current-level
        (mod (1+ pacmacs-current-level)
             (length pacmacs-levels)))
  (pacmacs--load-current-level))

(defun pacmacs--make-wall-cell (row column)
  (list :current-animation (pacmacs-make-anim (list (pacmacs-make-frame '(0 0 40 40) 100))
                                              (pacmacs-create-color-block 40 40 "red"))
        :row row
        :column column
        :type 'wall))

(defun pacmacs--make-pill (row column)
  (list :current-animation (pacmacs-load-anim "Pill")
        :row row
        :column column
        :type 'pill))

(defun pacmacs--make-ghost (row column)
  (list :row row
        :column column
        :init-row row
        :init-column column
        :prev-row row
        :prev-column column
        :direction 'right
        :current-animation (pacmacs-load-anim "Red-Ghost-Right")
        :direction-animations (list 'left  (pacmacs-load-anim "Red-Ghost-Left")
                                    'right (pacmacs-load-anim "Red-Ghost-Right")
                                    'up    (pacmacs-load-anim "Red-Ghost-Up")
                                    'down  (pacmacs-load-anim "Red-Ghost-Down"))
        :speed 1
        :speed-counter 0
        :type 'ghost))

(defun pacmacs--make-player (row column)
  (list :row row
        :column column
        :init-row row
        :init-column column
        :prev-row row
        :prev-column column
        :direction 'right
        :current-animation (pacmacs-load-anim "Pacman-Chomping-Right")
        :direction-animations (list 'left  (pacmacs-load-anim "Pacman-Chomping-Left")
                                    'right (pacmacs-load-anim "Pacman-Chomping-Right")
                                    'up    (pacmacs-load-anim "Pacman-Chomping-Up")
                                    'down  (pacmacs-load-anim "Pacman-Chomping-Down"))
        :speed 0
        :speed-counter 0
        :type 'player))

(defun pacmacs--reset-object-position (game-object)
  (plist-bind ((init-row :init-row)
               (init-column :init-column))
      game-object
    (pacmacs--remove-object game-object)
    (plist-put game-object :row init-row)
    (plist-put game-object :column init-column)
    (pacmacs--put-object game-object)))

(defun pacmacs--step-back-object (game-object)
  (plist-bind ((prev-row :prev-row)
               (prev-column :prev-column))
      game-object
    (pacmacs--remove-object game-object)
    (plist-put game-object :row prev-row)
    (plist-put game-object :column prev-column)
    (pacmacs--put-object game-object)))

(defun pacmacs--kill-buffer-and-its-window (buffer-or-name)
  (let ((buffer-window (get-buffer-window buffer-or-name)))
    (if (and buffer-window
             (window-parent buffer-window))
        (with-current-buffer buffer-or-name
          (kill-buffer-and-window))
      (kill-buffer buffer-or-name))))

(defun pacmacs--wall-at-p (row column)
  (pacmacs--object-type-at-p pacmacs--object-board
                             row column 'wall))

(defun pacmacs--pill-at-p (row column)
  (pacmacs--object-type-at-p pacmacs--object-board
                             row column 'pill))

(defun pacmacs--ghost-at-p (row column)
  (pacmacs--object-type-at-p pacmacs--object-board
                             row column 'ghost))

(defun pacmacs-quit ()
  (interactive)
  (when (get-buffer pacmacs-buffer-name)
    (pacmacs--kill-buffer-and-its-window pacmacs-buffer-name)))

(defun pacmacs--cell-tracked-p (row column)
  (pacmacs--cell-wrapped-get pacmacs--track-board row column))

(defun pacmacs--switch-direction (game-object direction)
  (plist-bind ((direction-animations :direction-animations))
      game-object
    (plist-put game-object :direction direction)
    (plist-put game-object :current-animation (plist-get direction-animations direction))))

(defun pacmacs--step-object (game-object)
  (plist-bind ((row :row)
               (column :column)
               (direction :direction)
               (speed-counter :speed-counter)
               (speed :speed))
      game-object
    (plist-put game-object :prev-row row)
    (plist-put game-object :prev-column column)
    (if (zerop speed-counter)
        (let* ((new-point (pacmacs--step-point pacmacs--object-board row column direction))
               (new-row (car new-point))
               (new-column (cdr new-point)))
          (plist-put game-object :speed-counter speed)
          (when (not (pacmacs--wall-at-p new-row new-column))
            (pacmacs--remove-object game-object)
            (plist-put game-object :row new-row)
            (plist-put game-object :column new-column)
            (pacmacs--put-object game-object)))
      (plist-put game-object :speed-counter (1- speed-counter)))))

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
    
    (pacmacs--cell-wrapped-set pacmacs--track-board
                       start-row start-column
                       (pacmacs--direction-name (cons d-row d-column)))))

(defun pacmacs--recalc-track-board ()
  (pacmacs--fill-board pacmacs--track-board nil)
  (plist-bind ((player-row :row)
               (player-column :column))
      pacmacs--player-state
    (let ((wave (list (cons player-row player-column))))
      (while (not (null wave))
        (let ((next-wave nil))
          (dolist (p wave)
            (let* ((row (car p))
                   (column (cdr p))
                   (possible-ways (pacmacs--possible-ways row column))
                   (candidate-ways
                    (cl-remove-if #'pacmacs--filter-candidates possible-ways)))
              (dolist (candidate-way candidate-ways)
                (pacmacs--track-point candidate-way p))
              (setq next-wave
                    (append next-wave candidate-ways))))
          (setq wave next-wave))))))

(defun pacmacs--track-object (game-object)
  (plist-bind ((row :row)
               (column :column))
      game-object
    (let ((direction (pacmacs--cell-wrapped-get pacmacs--track-board row column)))
      (pacmacs--switch-direction game-object direction))))

(defun pacmacs-tick ()
  (interactive)

  (cond
   ((equal pacmacs-game-state 'play)
    (pacmacs-play-state-logic))
   ((equal pacmacs-game-state 'death)
    (pacmacs-death-state-logic))
   ((equal pacmacs-game-state 'prepare)
    (pacmacs-waiting-logic #'pacmacs--switch-to-play-state))
   ((equal pacmacs-game-state 'level-beaten)
    (pacmacs-waiting-logic #'(lambda ()
                               (pacmacs--load-next-level)
                               (pacmacs--switch-to-prepare-state)))))

  (pacmacs--render-state))

(defun pacmacs--step-ghosts ()
  (dolist (ghost pacmacs--ghosts)
    (pacmacs--track-object ghost)
    (pacmacs--step-object ghost)))

(defun pacmacs--detect-pill-collision ()
  (plist-bind ((row :row)
               (column :column))
      pacmacs--player-state
    (-when-let (pill (pacmacs--pill-at-p row column))
      (setq pacmacs-score (+ pacmacs-score 10))
      (setq pacmacs--pills (-remove (-partial #'eql pill) pacmacs--pills))
      (pacmacs--remove-object pill))))

(defun pacmacs--ghost-collision-p ()
  (plist-bind ((row :row)
               (column :column))
      pacmacs--player-state
    (pacmacs--ghost-at-p row column)))

(defun pacmacs-play-state-logic ()
  (when (not pacmacs-play-pause)
    (pacmacs--anim-object-next-frame pacmacs--player-state pacmacs-tick-duration-ms)
    (pacmacs--anim-object-list-next-frame pacmacs--ghosts pacmacs-tick-duration-ms)
    (pacmacs--anim-object-list-next-frame pacmacs--pills pacmacs-tick-duration-ms)

    (pacmacs--recalc-track-board)
    (if pacmacs--pills
        (progn
          (pacmacs--step-object pacmacs--player-state)
          (if (pacmacs--ghost-collision-p)
              (progn (pacmacs--step-back-object pacmacs--player-state)
                     (pacmacs--switch-to-death-state))
            (pacmacs--detect-pill-collision)
            (pacmacs--step-ghosts)
            (when (pacmacs--ghost-collision-p)
              (dolist (ghost pacmacs--ghosts)
                (pacmacs--step-back-object ghost))
              (pacmacs--switch-to-death-state))))
      (pacmacs--switch-to-level-beaten-state))))

(defun pacmacs-death-state-logic ()
  (pacmacs--anim-object-next-frame pacmacs--player-state
                                   pacmacs-tick-duration-ms)
  (pacmacs--anim-object-list-next-frame pacmacs--ghosts
                                        pacmacs-tick-duration-ms)
  
  (when (= 0 (plist-get
              (plist-get pacmacs--player-state
                         :current-animation)
              :current-frame))
    (if (<= pacmacs-lives 0)
        (pacmacs--switch-to-game-over-state)
      (pacmacs--switch-to-play-state))))

(defun pacmacs-waiting-logic (switcher)
  (if (<= pacmacs-waiting-counter 0)
      (funcall switcher)
    (cl-decf pacmacs-waiting-counter
             pacmacs-tick-duration-ms)))

(defun pacmacs--put-object (game-object)
  (when game-object
    (plist-bind ((row :row)
                 (column :column))
        game-object
      (let ((cell (pacmacs--cell-wrapped-get pacmacs--object-board
                                             row column)))
        (pacmacs--cell-wrapped-set pacmacs--object-board row column
                                   (cons game-object cell))))))

(defun pacmacs--remove-object (game-object)
  (when game-object
    (plist-bind ((row :row)
                 (column :column))
        game-object
      (let ((cell (pacmacs--cell-wrapped-get pacmacs--object-board
                                             row column)))
        (pacmacs--cell-wrapped-set pacmacs--object-board
                                   row column
                                   (-remove (-partial #'eql game-object) cell))))))

(defun pacmacs--switch-to-death-state ()
  (setq pacmacs-game-state 'death)
  (cl-decf pacmacs-lives)
  (plist-put pacmacs--player-state :current-animation
             (pacmacs-load-anim "Pacman-Death"))
  (dolist (ghost pacmacs--ghosts)
    (plist-put ghost :current-animation
               (pacmacs-load-anim "Red-Ghost-Win"))))

(defun pacmacs--switch-to-game-over-state ()
  (setq pacmacs-game-state 'game-over)
  (pacmacs--load-map "game-over")
  (pacmacs--register-new-score pacmacs-score))

(defun pacmacs--switch-to-play-state ()
  (setq pacmacs-game-state 'play)
  (setq pacmacs-play-pause nil)
  (pacmacs--reset-object-position pacmacs--player-state)
  (dolist (ghost pacmacs--ghosts)
    (pacmacs--reset-object-position ghost))
  (pacmacs--switch-direction pacmacs--player-state 'right))

(defun pacmacs--switch-to-prepare-state ()
  (setq pacmacs-game-state 'prepare)
  (setq pacmacs-waiting-counter 1000))

(defun pacmacs--switch-to-level-beaten-state ()
  (setq pacmacs-game-state 'level-beaten)
  (setq pacmacs-waiting-counter 1000))

(defun pacmacs--fill-object-board ()
  (pacmacs--fill-board pacmacs--object-board nil)

  (dolist (pill pacmacs--pills)
    (pacmacs--put-object pill))

  (dolist (ghost pacmacs--ghosts)
    (pacmacs--put-object ghost))

  (pacmacs--put-object pacmacs--player-state)
  
  (dolist (wall pacmacs--wall-cells)
    (pacmacs--put-object wall)))

(defun pacmacs--render-state ()
  (with-current-buffer pacmacs-buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer)

      (insert (format "Score: %d\n\n" pacmacs-score))

      (when pacmacs-debug-output
        (pacmacs--render-track-board pacmacs--track-board))

      (plist-bind ((width :width)
                   (height :height))
          pacmacs--object-board
        (dotimes (row height)
          (dotimes (column width)
            (let ((anim-object (car (pacmacs--cell-wrapped-get pacmacs--object-board
                                                               row column))))
              (pacmacs--render-object anim-object)))
          (insert "\n")))
      (insert "\n")
      (dotimes (i pacmacs-lives)
        (ignore i)
        (pacmacs--render-life-icon))

      (when (equal pacmacs-game-state 'game-over)
        (-> (pacmacs--read-score-table)
            (pacmacs--sort-score-table)
            (pacmacs--render-score-table)))
      (goto-char 0))))

(defun pacmacs--unpaused-play-state-p ()
  (and (equal pacmacs-game-state 'play)
       (not pacmacs-play-pause)))

(defun pacmacs-up ()
  (interactive)
  (when (pacmacs--unpaused-play-state-p)
    (pacmacs--switch-direction pacmacs--player-state 'up)))

(defun pacmacs-down ()
  (interactive)
  (when (pacmacs--unpaused-play-state-p)
    (pacmacs--switch-direction pacmacs--player-state 'down)))

(defun pacmacs-left ()
  (interactive)
  (when (pacmacs--unpaused-play-state-p)
    (pacmacs--switch-direction pacmacs--player-state 'left)))

(defun pacmacs-right ()
  (interactive)
  (when (pacmacs--unpaused-play-state-p)
    (pacmacs--switch-direction pacmacs--player-state 'right)))

(defun pacmacs-pause ()
  (interactive)
  (when (equal pacmacs-game-state 'play)
    (setq pacmacs-play-pause (not pacmacs-play-pause))))

(defun pacmacs--get-list-of-levels ()
  (->> (directory-files (pacmacs--find-resource-file "./maps/"))
       (-map #'pacmacs--levelname-from-filename)
       (-remove #'null)
       (-sort #'string-lessp)
       (apply #'vector)))

(defun pacmacs--load-map (map-name)
  (let* ((lines (split-string (->> map-name
                                   (format "./maps/%s.txt")
                                   (pacmacs--find-resource-file)
                                   (pacmacs--file-content))
                              "\n" t))
         (board-width (apply 'max (mapcar #'length lines)))
         (board-height (length lines)))
    (setq pacmacs--object-board (pacmacs--make-board board-width
                                                     board-height))
    (setq pacmacs--track-board (pacmacs--make-board board-width
                                                    board-height))

    (setq pacmacs--wall-cells nil)
    (setq pacmacs--pills nil)
    (setq pacmacs--ghosts nil)
    (setq pacmacs--player-state nil)

    (cl-loop
     for line being the element of lines using (index row)
     do (cl-loop for x being the element of line using (index column)
                 do (cond ((char-equal x ?#)
                           (add-to-list 'pacmacs--wall-cells (pacmacs--make-wall-cell row column)))

                          ((char-equal x ?.)
                           (add-to-list 'pacmacs--pills (pacmacs--make-pill row column)))

                          ((char-equal x ?o)
                           (setq pacmacs--player-state (pacmacs--make-player row column)))

                          ((char-equal x ?g)
                           (add-to-list 'pacmacs--ghosts (pacmacs--make-ghost row column))))))
    (pacmacs--fill-object-board)))

(provide 'pacmacs)

;;; pacmacs.el ends here
