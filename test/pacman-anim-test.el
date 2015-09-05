(require 'cl)                           ;el-mock doesn't work without
                                        ;this
(require 'json)

(ert-deftest pacman-make-anim-test ()
  (should (equal (list :frames (list 1 2 3 4 5)
                       :current-frame 0
                       :sprite-sheet 42)
                 (pacman-make-anim (number-sequence 1 5) 42))))

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

(ert-deftest pacman-convert-aseprite-frame-test ()
  (let ((aseprite-frame '(khooy
                          (foo . bar)
                          (frame
                           (x . 1)
                           (y . 2)
                           (h . 3)
                           (w . 4))))
        (expected-frame (list 1 2 4 3)))
    (should (equal expected-frame
                   (pacman-convert-aseprite-frame aseprite-frame)))))

(ert-deftest pacman-aseprite-frame-get-order-test ()
  (let ((aseprite-frame '(khooy42.ase
                          (foo . bar)
                          (frame
                           (x . 1)
                           (y . 2)
                           (h . 3)
                           (w . 4)))))
    (should (equal 42 (pacman-aseprite-frame-get-order aseprite-frame)))))

(ert-deftest pacman-compare-aseprite-frames-test ()
  (let ((aseprite-frame1 '(khooy42.ase
                           (foo . bar)
                           (frame
                            (x . 1)
                            (y . 2)
                            (h . 3)
                            (w . 4))))
        (aseprite-frame2 '(khooy43.ase
                           (foo . bar)
                           (frame
                            (x . 1)
                            (y . 2)
                            (h . 3)
                            (w . 4)))))
    (should (pacman-compare-aseprite-frames aseprite-frame1 aseprite-frame2))
    (should (not (pacman-compare-aseprite-frames aseprite-frame2 aseprite-frame1)))))

(ert-deftest pacman-load-anim-test ()
  (let* ((input-aseprite-format '((frames
                                   (frame-3\.ase (frame (h . 3) (w . 3) (y . 3) (x . 3)))
                                   (frame-2\.ase (frame (h . 2) (w . 2) (y . 2) (x . 2)))
                                   (frame-1\.ase (frame (h . 1) (w . 1) (y . 1) (x . 1)))
                                   (frame-0\.ase (frame (h . 0) (w . 0) (y . 0) (x . 0))))))
         (input-sprite-sheet 42)
         (expected-output (pacman-make-anim
                           (mapcar #'(lambda (x)
                                       (make-list 4 x))
                                   (number-sequence 0 3))
                           input-sprite-sheet)))
    (with-mock
     (mock (json-read-file *) => input-aseprite-format)
     (mock (pacman-load-resource *) => input-sprite-sheet)
     (should (equal expected-output
                    (pacman-load-anim "foo" "bar"))))))
