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
