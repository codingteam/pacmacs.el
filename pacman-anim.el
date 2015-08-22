
(defun pacman-anim-get-frame (anim)
  (let ((frames (plist-get anim :frames))
        (current-frame (plist-get anim :current-frame)))
    (nth current-frame frames)))

(defun pacman-anim-next-frame (anim)
  (let* ((frames (plist-get anim :frames))
         (current-frame (plist-get anim :current-frame))
         (new-current-frame (mod (+ current-frame 1)
                                 (length frames))))
    (plist-put anim :current-frame new-current-frame)))

(defun pacman-anim-object-next-frame (anim-object)
  (let ((anim (plist-get anim-object :animation)))
    (plist-put anim-object :animation
               (pacman-anim-next-frame anim))))

(provide 'pacman-anim)
