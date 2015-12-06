(add-to-list 'load-path default-directory)
(add-to-list 'load-path (concat default-directory "/tools/"))

(require 'pacmacs-rr)
(require 'f)

(defconst att-result-file-path "./att.txt")
(defvar att-it-case-path "./it-cases/it-case03.el")

(defun att-replayer-finished ()
  (f-write (format "Average Tick Time: %fms" (pacmacs--average-tick-time))
           'utf-8
           att-result-file-path)
  (kill-emacs 0))

(add-hook 'pacmacs-replay-finished-hook #'att-replayer-finished)
(pacmacs--start-it-replayer att-it-case-path)
