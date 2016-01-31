;;; pacmacs.el --- Pacman for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 Codingteam

;; Author: Codingteam <codingteam@conference.jabber.ru>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/codingteam/pacmacs.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (dash "2.11.0") (dash-functional "1.2.0") (cl-lib "0.5") (f "0.18.0"))

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
(require 'f)
(require 'widget)
(require 'wid-edit)

(require 'pacmacs-anim)
(require 'pacmacs-board)
(require 'pacmacs-walls)
(require 'pacmacs-utils)
(require 'pacmacs-render)
(require 'pacmacs-score)
(require 'pacmacs-vector)

(defconst pacmacs-buffer-name "*Pacmacs*")
(defconst pacmacs-tick-duration-ms 100)
(defconst pacmacs--ghost-blinking-threshold-ms 2500)
(defconst pacmacs--ghost-terrified-time-ms 5000)
(defconst pacmacs--score-table-render-offset 3)

(defvar pacmacs-debug-output nil)

(defvar pacmacs-timer nil)

(defvar pacmacs-score 0)

(defvar pacmacs--player-state nil)
(defvar pacmacs--ghosts nil)
(defvar pacmacs--terrified-ghosts nil)
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

(defgroup pacmacs nil
  "Customizations for Pacmacs game."
  :prefix "pacmacs-"
  :group 'wp
  :link '(url-link "https://github.com/codingteam/pacmacs.el"))

(defcustom pacmacs-levels-folder nil
  "A folder from where the Pacmacs game loads its levels."
  :group 'pacmacs
  :type '(radio (const :tag "Default path")
                (directory :tag "Custom path")))

(define-derived-mode pacmacs-mode special-mode "Pacmacs"
  (define-key pacmacs-mode-map (kbd "<up>") 'pacmacs-up)
  (define-key pacmacs-mode-map (kbd "<down>") 'pacmacs-down)
  (define-key pacmacs-mode-map (kbd "<left>") 'pacmacs-left)
  (define-key pacmacs-mode-map (kbd "<right>") 'pacmacs-right)
  (define-key pacmacs-mode-map (kbd "q") 'pacmacs-quit)
  (define-key pacmacs-mode-map (kbd "SPC") 'pacmacs-pause)
  (add-hook 'kill-buffer-hook 'pacmacs-destroy nil t)
  (setq cursor-type nil)
  (setq truncate-lines t))

(define-derived-mode pacmacs-game-over-mode special-mode "Pacmacs-Game-Over"
  (define-key pacmacs-game-over-mode-map (kbd "q") 'pacmacs-quit)
  (setq cursor-type nil)
  (setq truncate-lines t))

(define-derived-mode pacmacs-scores-mode special-mode "Pacmacs-Scores"
  (define-key pacmacs-game-over-mode-map (kbd "q")
    (-partial #'pacmacs--kill-buffer-and-its-window
              pacmacs--score-buffer-name))
  (setq cursor-type nil)
  (setq truncate-lines t))

(defun pacmacs-toggle-debug-output ()
  (interactive)
  (setq pacmacs-debug-output (not pacmacs-debug-output)))

;;;###autoload
(defun pacmacs-start ()
  (interactive)
  (pacmacs--initialize-game 'pacmacs-tick)
  (pacmacs-mode))

(defun pacmacs--initialize-game (tick-function)
  (pacmacs--clear-wall-tiles-cache)

  (when (get-buffer pacmacs-buffer-name)
    (kill-buffer pacmacs-buffer-name))
  (switch-to-buffer pacmacs-buffer-name)
  (buffer-disable-undo pacmacs-buffer-name)

  (setq pacmacs-lives 3)
  (setq pacmacs-score 0)
  (setq pacmacs-levels (pacmacs--get-list-of-levels))

  (when (zerop (length pacmacs-levels))
    (error (concat "`%s' doesn't contain levels. Nothing to play. "
                   "Customize `pacmacs-levels-folder' accordingly")
           (pacmacs--get-levels-folder)))

  (setq pacmacs-current-level 0)

  (pacmacs--load-current-level)
  (pacmacs--switch-to-play-state)

  (unless pacmacs-timer
    (setq pacmacs-timer (run-at-time nil (* pacmacs-tick-duration-ms 0.001)
                                     tick-function))))

(defun pacmacs-destroy ()
  "Destroys the game timer without killing the game buffer."
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
  (list :current-animation nil
        :row row
        :column column
        :type 'wall))

(defun pacmacs--make-pill (row column anim-name points type)
  (list :current-animation (pacmacs-load-anim anim-name)
        :row row
        :column column
        :type type
        :points points))

(defun pacmacs--make-regular-pill (row column)
  (pacmacs--make-pill row column "Pill" 10 'pill))

(defun pacmacs--make-big-pill (row column)
  (pacmacs--make-pill row column "Big-Pill" 50 'big-pill))

(defun pacmacs--decrease-terrified-timers ()
  (dolist (terrified-ghost pacmacs--terrified-ghosts)
    (plist-map terrified-ghost :terrified-timer
               (-lambda (terrified-timer)
                 (cl-decf terrified-timer
                          pacmacs-tick-duration-ms)))))

(defun pacmacs--handle-ghost-blinking-threshold ()
  (dolist (terrified-ghost pacmacs--terrified-ghosts)
    (plist-bind ((terrified-timer :terrified-timer))
        terrified-ghost
      (when (<= terrified-timer
                pacmacs--ghost-blinking-threshold-ms
                (+ terrified-timer pacmacs-tick-duration-ms))
        (plist-put terrified-ghost
                   :current-animation
                   (pacmacs-load-anim "Blinking-Terrified-Ghost"))))))

(defun pacmacs--switch-direction-animation-callback (animation-prefix)
  (let ((direction-animations (-mapcat
                               (-lambda (direction)
                                 (->> direction
                                      (symbol-name)
                                      (capitalize)
                                      (concat animation-prefix "-")
                                      (pacmacs-load-anim)
                                      (list direction)))
                               '(left right up down))))
    (-lambda (game-object direction)
      (plist-put game-object :direction direction)
      (let* ((animation (plist-get direction-animations direction)))
        (plist-put game-object :current-animation animation)))))

(defun pacmacs--switch-direction-callback (game-object direction)
  (plist-put game-object :direction direction))

(defun pacmacs--make-ghost (row column)
  (list :row row
        :column column
        :init-row row
        :init-column column
        :prev-row row
        :prev-column column
        :direction 'right
        :current-animation (pacmacs-load-anim "Red-Ghost-Right")
        :switch-direction-callback (pacmacs--switch-direction-animation-callback "Red-Ghost")
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
        :switch-direction-callback (pacmacs--switch-direction-animation-callback "Pacman-Chomping")
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
  (-if-let (pill (pacmacs--object-type-at-p pacmacs--object-board
                                            row column 'pill))
      pill
    (pacmacs--object-type-at-p pacmacs--object-board
                               row column 'big-pill)))

(defun pacmacs--ghost-at-p (row column)
  (pacmacs--object-type-at-p pacmacs--object-board
                             row column 'ghost))

(defun pacmacs--terrified-ghost-at-p (row column)
  (pacmacs--object-type-at-p pacmacs--object-board
                             row column 'terrified-ghost))

(defun pacmacs-quit ()
  (interactive)
  (when (get-buffer pacmacs-buffer-name)
    (pacmacs--kill-buffer-and-its-window pacmacs-buffer-name)))

(defun pacmacs--cell-tracked-p (row column)
  (pacmacs--cell-wrapped-get pacmacs--track-board row column))

(defun pacmacs--switch-direction (game-object direction)
  (plist-bind ((switch-direction-callback :switch-direction-callback))
      game-object
    (funcall switch-direction-callback game-object direction)))

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

(defun pacmacs--possible-side-ways (row column)
  (list (cons (1+ row) column)          ;down
        (cons row (1+ column))          ;right
        (cons (1- row) column)          ;up
        (cons row (1- column))))        ;left

(defun pacmacs--possible-diagonal-ways (row column)
  (list (cons (1- row) (1- column))
        (cons (1- row) (1+ column))
        (cons (1+ row) (1- column))
        (cons (1+ row) (1+ column))))

(defun pacmacs--filter-candidates (p)
  (let ((row (car p))
        (column (cdr p)))
    (or (pacmacs--wall-at-p row column)
        (pacmacs--cell-tracked-p row column))))

(defun pacmacs--recalc-track-board ()
  (pacmacs--fill-board pacmacs--track-board nil)
  (plist-bind ((player-row :row)
               (player-column :column))
      pacmacs--player-state
    (let ((wave (list (cons player-row player-column))))
      (pacmacs--cell-wrapped-set pacmacs--track-board
                                 player-row player-column
                                 0)
      (while (not (null wave))
        (let ((next-wave nil))
          (dolist (current-tile wave)
            (let* ((current-row (car current-tile))
                   (current-column (cdr current-tile))
                   (possible-ways (pacmacs--possible-side-ways current-row
                                                               current-column))
                   (candidate-ways
                    (cl-remove-if #'pacmacs--filter-candidates
                                  possible-ways))
                   (candidate-distance (1+ (pacmacs--cell-wrapped-get
                                            pacmacs--track-board
                                            current-row current-column))))
              (-each candidate-ways
                (-lambda ((candidate-row . candidate-column))
                  (pacmacs--cell-wrapped-set pacmacs--track-board
                                             candidate-row
                                             candidate-column
                                             candidate-distance)))
              (setq next-wave
                    (append next-wave candidate-ways))))
          (setq wave next-wave))))))

(defun pacmacs--track-object-direction (game-object distance-comparator)
  (plist-bind ((row :row)
               (column :column))
      game-object
    (let* ((candidate-ways (cons
                            (cons row column)
                            (-remove (-lambda ((candidate-row . candidate-column))
                                       (pacmacs--wall-at-p candidate-row candidate-column))
                                     (pacmacs--possible-side-ways row column))))
           (candidate-distances (-map (-lambda ((candidate-row . candidate-column))
                                        (pacmacs--cell-wrapped-get pacmacs--track-board
                                                                   candidate-row
                                                                   candidate-column))
                                      candidate-ways))
           (next-tile (->> (-zip candidate-distances candidate-ways)
                           (-sort (-lambda ((distance-1 . _) (distance-2 . _))
                                    (funcall distance-comparator
                                             distance-1 distance-2)))
                           (cdar))))
      (when next-tile
        (->> (pacmacs--vector- next-tile (cons row column))
             (pacmacs--direction-name))))))

(defun pacmacs--track-object (game-object distance-comparator)
  (-when-let (direction (pacmacs--track-object-direction
                         game-object
                         distance-comparator))
    (pacmacs--switch-direction game-object direction)))

(defun pacmacs--track-object-to-player (game-object)
  (pacmacs--track-object game-object #'<))

(defun pacmacs--track-object-from-player (game-object)
  (pacmacs--track-object game-object #'>))

(defun pacmacs-tick ()
  (interactive)

  (cond
   ((equal pacmacs-game-state 'play)
    (pacmacs--play-state-logic))
   ((equal pacmacs-game-state 'death)
    (pacmacs-death-state-logic))
   ((equal pacmacs-game-state 'prepare)
    (pacmacs-waiting-logic #'pacmacs--switch-to-play-state))
   ((equal pacmacs-game-state 'level-beaten)
    (pacmacs-waiting-logic #'(lambda ()
                               (pacmacs--load-next-level)
                               (pacmacs--switch-to-prepare-state)))))

  (when (not (equal pacmacs-game-state 'game-over))
    (with-render-target pacmacs-buffer-name
      (erase-buffer)
      (pacmacs--render-state)
      (goto-char (point-min)))))

(defun pacmacs--step-ghosts ()
  (dolist (ghost pacmacs--ghosts)
    (pacmacs--track-object-to-player ghost)
    (pacmacs--step-object ghost)))

(defun pacmacs--step-terrified-ghosts ()
  (dolist (terrified-ghost pacmacs--terrified-ghosts)
    (pacmacs--track-object-from-player terrified-ghost)
    (pacmacs--step-object terrified-ghost)))

(defun pacmacs--terrified-ghost-timed-out-p (terrified-ghost)
  (<= (plist-get terrified-ghost :terrified-timer) 0))

(defun pacmacs--terrify-ghost (ghost)
  (plist-put ghost :current-animation
             (pacmacs-load-anim "Terrified-Ghost"))
  (plist-put ghost :switch-direction-callback
             #'pacmacs--switch-direction-callback)
  (plist-put ghost :type 'terrified-ghost)
  (plist-put ghost :terrified-timer
             pacmacs--ghost-terrified-time-ms)
  ghost)

(defun pacmacs--unterrify-ghost (terrified-ghost)
  (plist-put terrified-ghost :switch-direction-callback
             (pacmacs--switch-direction-animation-callback "Red-Ghost"))
  (plist-put terrified-ghost :type 'ghost)
  (pacmacs--switch-direction terrified-ghost
                             (plist-get terrified-ghost :direction))
  terrified-ghost)

(defun pacmacs--terrify-all-ghosts ()
  (dolist (terrified-ghost pacmacs--terrified-ghosts)
    (pacmacs--terrify-ghost terrified-ghost))

  (dolist (ghost pacmacs--ghosts)
    (push (pacmacs--terrify-ghost ghost)
          pacmacs--terrified-ghosts))
  (setq pacmacs--ghosts nil))

(defun pacmacs--unterrify-timed-out-ghosts ()
  (dolist (terrified-ghost (-filter #'pacmacs--terrified-ghost-timed-out-p
                                    pacmacs--terrified-ghosts))
    (push (pacmacs--unterrify-ghost terrified-ghost)
          pacmacs--ghosts))

  (setq pacmacs--terrified-ghosts
        (-remove #'pacmacs--terrified-ghost-timed-out-p
                 pacmacs--terrified-ghosts)))

(defun pacmacs--detect-pill-collision ()
  (plist-bind ((row :row)
               (column :column))
      pacmacs--player-state
    (-when-let (pill (pacmacs--pill-at-p row column))
      (setq pacmacs-score (+ pacmacs-score (plist-get pill :points)))
      (setq pacmacs--pills (-remove (-partial #'eql pill) pacmacs--pills))
      (pacmacs--remove-object pill)

      (when (equal (plist-get pill :type) 'big-pill)
        (pacmacs--terrify-all-ghosts)))))

(defun pacmacs--detect-terrified-ghost-collision ()
  (plist-bind ((row :row)
               (column :column))
      pacmacs--player-state
    (-when-let (terrified-ghost (pacmacs--terrified-ghost-at-p row column))
      (setq pacmacs-score (+ pacmacs-score 200))
      (setq pacmacs--terrified-ghosts (-remove (-partial #'eql terrified-ghost)
                                               pacmacs--terrified-ghosts))
      (pacmacs--remove-object terrified-ghost))))

(defun pacmacs--ghost-collision-p ()
  (plist-bind ((row :row)
               (column :column))
      pacmacs--player-state
    (pacmacs--ghost-at-p row column)))

(defun pacmacs--play-state-logic ()
  (when (not pacmacs-play-pause)
    (pacmacs--anim-object-next-frame pacmacs--player-state pacmacs-tick-duration-ms)
    (pacmacs--anim-object-list-next-frame pacmacs--ghosts pacmacs-tick-duration-ms)
    (pacmacs--anim-object-list-next-frame pacmacs--pills pacmacs-tick-duration-ms)
    (pacmacs--anim-object-list-next-frame pacmacs--terrified-ghosts pacmacs-tick-duration-ms)

    (pacmacs--recalc-track-board)

    (pacmacs--unterrify-timed-out-ghosts)
    (pacmacs--decrease-terrified-timers)
    (pacmacs--handle-ghost-blinking-threshold)

    (if pacmacs--pills
        (progn
          (pacmacs--step-object pacmacs--player-state)
          (pacmacs--detect-terrified-ghost-collision)
          (if (pacmacs--ghost-collision-p)
              (progn (pacmacs--step-back-object pacmacs--player-state)
                     (pacmacs--switch-to-death-state))
            (pacmacs--detect-pill-collision)
            (pacmacs--step-ghosts)
            (pacmacs--step-terrified-ghosts)
            (pacmacs--detect-terrified-ghost-collision)
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

(defun pacmacs--align-score-record-nickname (nickname)
  (let* ((padding-size (max 0 (- pacmacs--max-score-nick-size
                                 (length nickname))))
         (padding (make-string padding-size ?\s)))
    (concat nickname padding)))

(defun pacmacs--make-submit-nickname-action (score)
  (lambda (widget &optional event)
    (ignore event)
    (let ((nickname (widget-value widget)))
      (pacmacs--add-entry-to-score-table nickname score)
      (widget-value-set widget (pacmacs--align-score-record-nickname nickname))
      (widget-delete widget)
      (pacmacs-game-over-mode))))

(defun pacmacs--switch-to-game-over-state ()
  (pacmacs--load-map-sign "game-over")
  (pacmacs-destroy)
  (setq pacmacs-game-state 'game-over)

  (fundamental-mode)
  (read-only-mode 0)

  (with-current-buffer pacmacs-buffer-name
    (erase-buffer)

    (pacmacs--render-state)

    (let* ((score-table (pacmacs--read-score-table))
           (new-score-position (pacmacs--position-of-new-score
                                score-table
                                pacmacs-score)))
      (if (< new-score-position pacmacs--max-score-table-size)
          (progn
            (->> score-table
                 (-take new-score-position)
                 (pacmacs--render-score-table))
            (widget-create 'editable-field
                           :size pacmacs--max-score-nick-size
                           :action (pacmacs--make-submit-nickname-action pacmacs-score))
            (insert (format " %d\n" pacmacs-score))
            (->> score-table
                 (-drop new-score-position)
                 (pacmacs--render-score-table))

            (plist-bind ((height :height))
                pacmacs--object-board
              (goto-char (point-min))
              (forward-line (+ height pacmacs--score-table-render-offset new-score-position)))
            (use-local-map widget-keymap)
            (widget-setup))
        (pacmacs--render-score-table score-table)
        (goto-char (point-min))
        (pacmacs-game-over-mode)))))

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
  (insert (format "Score: %d\n\n" pacmacs-score))

  (when pacmacs-debug-output
    (pacmacs--render-track-board pacmacs--track-board))

  (pacmacs--render-object-board pacmacs--object-board)

  (dotimes (i pacmacs-lives)
    (ignore i)
    (pacmacs--render-life-icon)))

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

(defun pacmacs--get-levels-folder ()
  (if pacmacs-levels-folder
      pacmacs-levels-folder
    (pacmacs--find-resource-file "./maps/")))

(defun pacmacs--get-list-of-levels ()
  (->> (condition-case err
           (directory-files (pacmacs--get-levels-folder))
         (file-error (error (concat "Error during loading levels: `%s'. "
                                    "Customize `pacmacs-levels-folder' accordingly.")
                            err)))
       (-map #'pacmacs--levelname-from-filename)
       (-remove #'null)
       (-sort #'string-lessp)
       (apply #'vector)))

;;;###autoload
(defun pacmacs-score ()
  (interactive)

  (switch-to-buffer pacmacs--score-buffer-name)
  (pacmacs-scores-mode)
  (pacmacs--load-map-sign "scores")

  (with-render-target pacmacs--score-buffer-name
    (erase-buffer)
    (pacmacs--render-score-page
     (-partial #'pacmacs--render-object-board
               pacmacs--object-board))
    (goto-char (point-min))))

(defun pacmacs--wall-tile-at (row column)
  (pacmacs--create-wall-tile
   40 40
   (-map (-lambda ((row . column))
           (not (pacmacs--wall-at-p row column)))
         (append (pacmacs--possible-side-ways row column)
                 (pacmacs--possible-diagonal-ways row column)))))

(defun pacmacs--load-map (map-name)
  (->> map-name
       (format "%s/%s.txt" (pacmacs--get-levels-folder))
       (pacmacs--load-map-file)))

(defun pacmacs--load-map-sign (sign-name)
  (->> sign-name
       (format "./signs/%s.txt")
       (pacmacs--find-resource-file)
       (pacmacs--load-map-file)))

(defun pacmacs--load-map-file (map-file-name)
  (let* ((lines (split-string (f-read-text map-file-name)
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
    (setq pacmacs--terrified-ghosts nil)
    (setq pacmacs--player-state nil)

    (cl-loop
     for line being the element of lines using (index row)
     do (cl-loop for x being the element of line using (index column)
                 do (cond ((char-equal x ?#)
                           (push (pacmacs--make-wall-cell row column) pacmacs--wall-cells))

                          ((char-equal x ?.)
                           (push (pacmacs--make-regular-pill row column) pacmacs--pills))

                          ((char-equal x ?+)
                           (push (pacmacs--make-big-pill row column) pacmacs--pills))

                          ((char-equal x ?o)
                           (setq pacmacs--player-state (pacmacs--make-player row column)))

                          ((char-equal x ?g)
                           (push (pacmacs--make-ghost row column) pacmacs--ghosts)))))

    (pacmacs--fill-object-board)

    (dolist (wall pacmacs--wall-cells)
      (plist-bind ((row :row)
                   (column :column))
          wall
        (plist-put wall :current-animation
                   (pacmacs-make-anim (list (pacmacs-make-frame '(0 0 40 40) 100))
                                      (pacmacs--wall-tile-at row column)))))))


(provide 'pacmacs)

;;; pacmacs.el ends here
