(ert-deftest pacman-anim-get-frame-test ()
  (let ((anim (list :frames (number-sequence 1 5)
                    :current-frame 2)))
    (should (equal 3 (pacman-anim-get-frame anim)))))

(ert-deftest pacman-anim-next-frame-test ()
  (let ((anim (list :frames (number-sequence 1 4)
                    :current-frame 2)))
    (pacman-anim-next-frame anim)
    (should (equal 3 (plist-get anim :current-frame)))

    (pacman-anim-next-frame anim)
    (should (equal 0 (plist-get anim :current-frame)))))

(ert-deftest pacman-anim-object-next-frame-test ()
  (with-mock
   (stub pacman-anim-next-frame => 42)
   (let ((anim-object '(:animation 41)))
     (pacman-anim-object-next-frame anim-object)
     (should (equal '(:animation 42)
                    anim-object)))))
