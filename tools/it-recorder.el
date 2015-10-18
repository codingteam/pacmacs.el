(require 'pacmacs)

(require 'cl-lib)

(defvar pacmacs--tick-counter 0)
(defvar pacmacs--recorded-actions nil)

(defun pacmacs--record-action (action-name)
  (add-to-list 'pacmacs--recorded-actions
               (cons action-name pacmacs--tick-counter)))

(defun pacmacs--reset-recorder ()
  (setq pacmacs--tick-counter 0)
  (setq pacmacs--recorded-actions nil))

(defun pacmacs--save-test-case (filename)
  (interactive "FFile to save the test case: ")
  (with-temp-buffer
    (-> pacmacs--recorded-actions
        (pp-to-string)
        (insert))
    (write-file filename)))

(defun pacmacs-record-up ()
  (interactive)
  (pacmacs--record-action 'up)
  (pacmacs-up))

(defun pacmacs-record-down ()
  (interactive)
  (pacmacs--record-action 'down)
  (pacmacs-down))

(defun pacmacs-record-left ()
  (interactive)
  (pacmacs--record-action 'left)
  (pacmacs-left))

(defun pacmacs-record-right ()
  (interactive)
  (pacmacs--record-action 'right)
  (pacmacs-right))

(defun pacmacs-record-tick ()
  (interactive)
  (cl-incf pacmacs--tick-counter)
  (pacmacs-tick))

(define-derived-mode pacmacs-it-recorder-mode pacmacs-mode "pacmacs-it-recorder-mode"
  (define-key pacmacs-it-recorder-mode-map (kbd "<up>") 'pacmacs-record-up)
  (define-key pacmacs-it-recorder-mode-map (kbd "<down>") 'pacmacs-record-down)
  (define-key pacmacs-it-recorder-mode-map (kbd "<left>") 'pacmacs-record-left)
  (define-key pacmacs-it-recorder-mode-map (kbd "<right>") 'pacmacs-record-right))

(defun pacmacs--start-it-recorder ()
  (interactive)
  (switch-to-buffer pacmacs-buffer-name)
  (pacmacs-it-recorder-mode)

  (setq pacmacs-lives 3)
  (setq pacmacs-score 0)
  (setq pacmacs-levels (pacmacs--get-list-of-levels))
  (setq pacmacs-current-level 0)
  (pacmacs--reset-recorder)

  (pacmacs--load-current-level)
  (pacmacs--switch-to-play-state)

  (unless pacmacs-timer
    (setq pacmacs-timer (run-at-time nil (* pacmacs-tick-duration-ms 0.001) 'pacmacs-record-tick))))
