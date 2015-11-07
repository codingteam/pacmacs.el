(require 'pacmacs)

(require 'dash)
(require 'f)
(require 'cl-lib)

(defvar pacmacs--tick-counter 0)
(defvar pacmacs--recorded-actions nil)
(defvar pacmacs--tick-times nil)

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
        (reverse)
        (pp-to-string)
        (insert))
    (write-file filename)))

(defun pacmacs--load-test-case (filename)
  (-> (f-read-text filename)
      (read-from-string)
      (car)))

(defun pacmacs-record-up ()
  (interactive)
  (pacmacs--record-action 'pacmacs-up)
  (pacmacs-up))

(defun pacmacs-record-down ()
  (interactive)
  (pacmacs--record-action 'pacmacs-down)
  (pacmacs-down))

(defun pacmacs-record-left ()
  (interactive)
  (pacmacs--record-action 'pacmacs-left)
  (pacmacs-left))

(defun pacmacs-record-right ()
  (interactive)
  (pacmacs--record-action 'pacmacs-right)
  (pacmacs-right))

(defun pacmacs-record-tick ()
  (interactive)
  (cl-incf pacmacs--tick-counter)
  (pacmacs-tick))

(defun pacmacs-replay-tick ()
  (cl-incf pacmacs--tick-counter)

  (add-to-list 'pacmacs--tick-times
               (pacmacs--measure-time
                (pacmacs-tick)))

  (if (not pacmacs--recorded-actions)
      (pacmacs-quit)
    (-let ((((action . tick-number) . _) pacmacs--recorded-actions))
      (when (= tick-number pacmacs--tick-counter)
        (funcall action)
        (setq pacmacs--recorded-actions (cdr pacmacs--recorded-actions))))))

(define-derived-mode pacmacs-it-recorder-mode pacmacs-mode "pacmacs-it-recorder-mode"
  (define-key pacmacs-it-recorder-mode-map (kbd "<up>") 'pacmacs-record-up)
  (define-key pacmacs-it-recorder-mode-map (kbd "<down>") 'pacmacs-record-down)
  (define-key pacmacs-it-recorder-mode-map (kbd "<left>") 'pacmacs-record-left)
  (define-key pacmacs-it-recorder-mode-map (kbd "<right>") 'pacmacs-record-right))

(defun pacmacs--average-tick-time ()
  (/ (-sum pacmacs--tick-times) (length pacmacs--tick-times)))

(defun pacmacs--start-it-recorder ()
  (interactive)
  (pacmacs--initialize-game 'pacmacs-record-tick)
  (pacmacs-it-recorder-mode)
  (pacmacs--reset-recorder))

(defun pacmacs--start-it-replayer (filename)
  (interactive "fLoad test case: ")
  (pacmacs--initialize-game 'pacmacs-replay-tick)
  (pacmacs-mode)

  (setq pacmacs--recorded-actions (pacmacs--load-test-case filename))
  (setq pacmacs--tick-counter 0)
  (setq pacmacs--tick-times nil))
