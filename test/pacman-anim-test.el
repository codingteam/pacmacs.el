(ert-deftest pacman-anim-get-frame-test ()
  (let ((anim (list :frames (number-sequence 1 5)
                    :current-frame 2)))
    (should (equal 3
                   (pacman-anim-get-frame anim)))))
