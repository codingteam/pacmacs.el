(require 'pacmacs-image)

(let ((canvas (pacmacs--make-image 1000 1000))
      (image (pacmacs--make-image 500 500)))
  (benchmark 1000000
             (progn
               (pacmacs--draw-image canvas image 0 0)
               (pacmacs--draw-image canvas image 100 100)
               (pacmacs--draw-image canvas image 900 900))))
