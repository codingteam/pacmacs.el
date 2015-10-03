
(ert-deftest pacmacs--fill-board-test ()
  (let ((input-board [[nil nil]
                      [nil nil]])
        (expected-board [[5 5]
                         [5 5]]))
    (pacmacs--fill-board input-board 2 2 5)
    (should (equal expected-board
                   input-board))))


(ert-deftest pacmacs--init-board-test ()
  (let ((width 5)
        (height 4)
        (expected-board [[nil nil nil nil nil]
                         [nil nil nil nil nil]
                         [nil nil nil nil nil]
                         [nil nil nil nil nil]]))
    (should (equal expected-board
                   (pacmacs--init-board width height)))))
