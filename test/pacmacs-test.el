
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

(ert-deftest pacmacs--cell-tracked-p-test ()
  (let ((pacmacs-board-width 2)
        (pacmacs-board-height 2)
        (pacmacs-track-board [[nil nil]
                              ['left 'right]]))
    (should (not (pacmacs--cell-tracked-p 0 0)))
    (should (pacmacs--cell-tracked-p 1 0))))

(ert-deftest pacmacs--track-point-test ()
  (let ((pacmacs-board-width 2)
        (pacmacs-board-height 2)
        (pacmacs-track-board [[nil nil]
                              [nil nil]]))
    (pacmacs--track-point (cons 0 0) (cons 0 1))
    (should (equal [[right nil]
                    [nil nil]]
                   pacmacs-track-board))
    (pacmacs--track-point (cons 1 0) (cons 1 -1))
    (should (equal [[right nil]
                    [left nil]]
                   pacmacs-track-board))))

(ert-deftest pacmacs--track-object-test ()
  (let ((pacmacs-board-width 2)
        (pacmacs-board-height 2)
        (pacmacs-track-board [[right down]
                              [up left]])
        (game-object (list :row 0
                           :column 0)))
    (with-mock
     (mock (pacmacs--switch-direction (list :row 0 :column 0) 'right) :times 1)
     (pacmacs--track-object game-object))))

(ert-deftest pacmacs--put-object-test ()
  (let ((pacmacs-board-width 2)
        (pacmacs-board-height 2)
        (pacmacs-board [[nil nil]
                        [nil nil]])
        (anim-object (list :row 0 :column 1)))
    (pacmacs--put-object anim-object)
    (should (equal [[nil (:row 0 :column 1)]
                    [nil nil]]
                   pacmacs-board))))
