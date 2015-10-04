(ert-deftest pacmacs--make-board-test ()
  (let ((width 5)
        (height 4)
        (expected-board (list :width 5
                              :height 4
                              :data [[nil nil nil nil nil]
                                     [nil nil nil nil nil]
                                     [nil nil nil nil nil]
                                     [nil nil nil nil nil]])))
    (should (equal expected-board
                   (pacmacs--make-board width height)))))

(ert-deftest pacmacs--cell-get-test ()
  (let ((input-board (list :width 3
                           :height 2
                           :data [[nil nil nil]
                                  [nil 42 nil]])))
    (should (equal 42 (pacmacs--cell-get input-board 1 1)))))

(ert-deftest pacmacs--cell-set-test ()
  (let ((input-board (list :width 3
                           :height 2
                           :data [[nil nil nil]
                                  [nil nil nil]]))
        (expected-board (list :width 3
                              :height 2
                              :data [[nil nil nil]
                                     [nil 42 nil]])))
    (pacmacs--cell-set input-board 1 1 42)
    (should (equal expected-board
                   input-board))))

(ert-deftest pacmacs--object-at-p-test ()
  (let ((board (list :width 5
                     :height 4))
        (objects (-map #'(lambda (x)
                           (list :row x
                                 :column x))
                       (number-sequence 0 3))))
    (should (pacmacs--object-at-p board 0 0 objects))
    (should (not (pacmacs--object-at-p board 0 1 objects)))
    (should (pacmacs--object-at-p board 0 5 objects))
    (should (not (pacmacs--object-at-p board 1 5 objects)))))

(ert-deftest pacmacs--step-point-test ()
  (let ((board (list :width 3
                     :heigth 2))
        (row 0)
        (column 0))
    (should (equal (cons 0 1) (pacmacs--step-point board
                                                   row column
                                                   'right)))
    (should (equal (cons 0 2) (pacmacs--step-point board
                                                   row column
                                                   'left)))))

