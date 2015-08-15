(defconst pacman-buffer-name "*Pacman*")

(defvar pacman-timer nil)
(defvar pacman-counter 0)

(define-derived-mode pacman-mode special-mode "pacman-mode"
  (define-key pacman-mode-map (kbd "<up>") 'pacman-up)
  (define-key pacman-mode-map (kbd "<down>") 'pacman-down)
  (define-key pacman-mode-map (kbd "<left>") 'pacman-left)
  (define-key pacman-mode-map (kbd "<right>") 'pacman-right)
  (define-key pacman-mode-map (kbd "q") 'pacman-quit)
  (add-hook 'kill-buffer-hook 'pacman-destroy))

(defun pacman-start ()
  (interactive)
  (switch-to-buffer-other-window pacman-buffer-name)
  (pacman-mode)
  (unless pacman-timer
    (setq pacman-timer (run-at-time nil 0.1 'pacman-tick))))

(defun pacman-destroy ()
  (when pacman-timer
    (cancel-timer pacman-timer)
    (setq pacman-timer nil)))

(defun pacman-quit ()
  (interactive)
  (when (get-buffer pacman-buffer-name)
    (kill-buffer pacman-buffer-name)))

(defun pacman-tick ()
  (interactive)
  (with-current-buffer pacman-buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%d" pacman-counter))
      (setq pacman-counter (+ pacman-counter 1)))))

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
