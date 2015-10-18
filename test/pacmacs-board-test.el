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

(ert-deftest pacmacs--cell-wrapped-get-test ()
  (let ((input-board (list :width 3
                           :height 2
                           :data [[nil nil nil]
                                  [nil 42 nil]])))
    (should (equal 42 (pacmacs--cell-wrapped-get input-board 1 1)))))

(ert-deftest pacmacs--cell-wrapped-set-test ()
  (let ((input-board (list :width 3
                           :height 2
                           :data [[nil nil nil]
                                  [nil nil nil]]))
        (expected-board (list :width 3
                              :height 2
                              :data [[nil nil nil]
                                     [nil 42 nil]])))
    (pacmacs--cell-wrapped-set input-board 1 1 42)
    (should (equal expected-board
                   input-board))))

(ert-deftest pacmacs--object-type-at-p-test ()
  (let ((board (pacmacs--make-board 5 4)))
    (dotimes (i 4)
      (pacmacs--cell-wrapped-set
       board i i
       (list (list :row i
                   :column i
                   :type 'khooy))))

    (should (pacmacs--object-type-at-p board 0 0 'khooy))
    (should (not (pacmacs--object-type-at-p board 0 1 'khooy)))
    (should (pacmacs--object-type-at-p board 0 5 'khooy))
    (should (not (pacmacs--object-type-at-p board 1 5 'khooy)))))

(ert-deftest pacmacs--step-point-test ()
  (let ((board (list :width 3
                     :height 2))
        (row 0)
        (column 0))
    (should (equal (cons 0 1) (pacmacs--step-point board
                                                   row column
                                                   'right)))
    (should (equal (cons 0 2) (pacmacs--step-point board
                                                   row column
                                                   'left)))))

(ert-deftest pacmacs--fill-board-test ()
  (let ((input-board (list :width 2
                           :height 2
                           :data [[nil nil]
                                  [nil nil]]))
        (expected-board (list :width 2
                              :height 2
                              :data [[5 5]
                                     [5 5]])))
    (pacmacs--fill-board input-board 5)
    (should (equal expected-board
                   input-board))))
