(ert-deftest pacmacs--make-board-test ()
  (let ((width 5)
        (height 4)
        (expected-board [[nil nil nil nil nil]
                         [nil nil nil nil nil]
                         [nil nil nil nil nil]
                         [nil nil nil nil nil]]))
    (should (equal expected-board
                   (pacmacs--make-board width height)))))
