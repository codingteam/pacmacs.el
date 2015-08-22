
(defun pacman-load-resource (filename)
  (create-image (concat default-directory filename)
                'png nil :heuristic-mask t))

(defun pacman-insert-image (resource resource-vector)
  (insert-image resource " " nil resource-vector))

(provide 'pacman-resources)
