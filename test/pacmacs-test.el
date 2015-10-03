
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

(ert-deftest pacmacs--object-at-p-test ()
  (let ((pacmacs-board-width 5)
        (pacmacs-board-height 4)
        (objects (-map #'(lambda (x)
                           (list :row x
                                 :column x))
                       (number-sequence 0 3))))
    (should (pacmacs--object-at-p 0 0 objects))
    (should (not (pacmacs--object-at-p 0 1 objects)))
    (should (pacmacs--object-at-p 0 5 objects))
    (should (not (pacmacs--object-at-p 1 5 objects)))))
