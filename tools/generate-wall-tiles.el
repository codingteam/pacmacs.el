(defun pacmacs--put-bits-dot (bits row column weight)
  (dotimes (i weight)
    (dotimes (j weight)
      (aset (aref bits (+ i row)) (+ j column) t))))

(defun pacmacs--put-vertical-bar (bits column height weight)
  (dotimes (w weight)
    (dotimes (i height)
      (aset (aref bits i) (+ column w) t))))

(defun pacmacs--put-horizontal-bar (bits row width weight)
  (dotimes (w weight)
    (dotimes (i width)
      (aset (aref bits (+ row w)) i t))))

(defun pacmacs--bit-list-to-integer (bit-list)
  (let ((result 0))
    (dolist (bit bit-list)
      (setq result (logior (lsh result 1)
                           (if bit 1 0))))
    result))

(defun pacmacs--integer-to-bit-list (x)
  (let ((result nil))
    (dotimes (i 8)
      (ignore i)
      (setq result (cons (logand x 1) result))
      (setq x (lsh x -1)))
    result))

(defun pacmacs--normalize-wall-bits (wall-bits)
  (-let (((bottom right top left
           left-upper right-upper
           left-bottom right-bottom)
          wall-bits))
    (list bottom right top left
          (if (and left-upper
                   (not left)
                   (not top)) 0 1)
          (if (and right-upper
                   (not right)
                   (not top)) 0 1)
          (if (and left-bottom
                   (not left)
                   (not bottom)) 0 1)
          (if (and right-bottom
                   (not right)
                   (not bottom)) 0 1))))

(defun pacmacs--generate-xpm (wall-block)
  )

(defun pacmacs--create-wall-tile (width height weight
                                  bottom right
                                  top left
                                  left-upper right-upper
                                  left-bottom right-bottom)
  (let ((wall-block (make-vector width nil)))
    (dotimes (i width)
      (aset wall-block i (make-vector height nil)))

    (when left-upper
      (pacmacs--put-bits-dot wall-block 0 0 weight))

    (when right-upper
      (pacmacs--put-bits-dot wall-block 0 (- width weight) weight))

    (when left-bottom
      (pacmacs--put-bits-dot wall-block (- height weight) 0 weight))

    (when right-bottom
      (pacmacs--put-bits-dot wall-block (- height weight) (- width weight) weight))

    (when left
      (pacmacs--put-vertical-bar wall-block 0 height weight))

    (when right
      (pacmacs--put-vertical-bar wall-block (- width weight) height weight))

    (when top
      (pacmacs--put-horizontal-bar wall-block 0 width weight))
    
    (when bottom
      (pacmacs--put-horizontal-bar wall-block (- height weight) width weight))

    wall-block))

(pp
 (->> (number-sequence 0 255)
      (-map #'pacmacs--integer-to-bit-list)
      (-map #'pacmacs--normalize-wall-bits)
      (-group-by #'identity)
      (-map #'car)
      (-map (-partial #'-map (-compose #'not #'zerop)))
      (-map (-partial #'apply #'pacmacs--create-wall-tile 40 40 3))))
