
(ert-deftest pacmacs--fill-board-test ()
  (let ((input-board [[nil nil]
                      [nil nil]])
        (expected-board [[5 5]
                         [5 5]]))
    (pacmacs--fill-board input-board 2 2 5)
    (should (equal expected-board
                   input-board))))
