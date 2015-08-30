(require 'json)

(defun pacman-make-anim (frames)
  (list :frames frames
        :current-frame 0))

(defun pacman-load-anim (aseprite-json-file)
  (let* ((aseprite-json (json-read-file aseprite-json-file))
         (aseprite-frames (cdr (assoc 'frames aseprite-json))))
    (pacman-make-anim
     (mapcar 'pacman-convert-aseprite-frame
             (pacman-aseprite-sort-frame-hack aseprite-frames)))))

(defun pacman-aseprite-frame-get-order (aseprite-frame)
  (let ((frame-name (symbol-name (car aseprite-frame))))
    (string-match "\\([0-9]+\\)\\.ase$" frame-name)
    (string-to-number (match-string 1 frame-name))))

(defun pacman-aseprite-sort-frame-hack (aseprite-frames)
  (sort aseprite-frames
        #'(lambda (f1 f2)
            (let ((o1 (pacman-aseprite-frame-get-order f1))
                  (o2 (pacman-aseprite-frame-get-order f2)))
              (< o1 o2)))))

(defun pacman-convert-aseprite-frame (aseprite-frame)
  (let* ((frame (cdr (assoc 'frame (cdr aseprite-frame))))
         (x (cdr (assoc 'x frame)))
         (y (cdr (assoc 'y frame)))
         (w (cdr (assoc 'w frame)))
         (h (cdr (assoc 'h frame))))
    (list x y w h)))

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
