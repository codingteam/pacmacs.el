
(ert-deftest pacmacs--cell-tracked-p-test ()
  (let ((pacmacs--track-board (list :width 2
                                     :height 2
                                     :data [[nil nil]
                                            ['left 'right]])))
    (should (not (pacmacs--cell-tracked-p 0 0)))
    (should (pacmacs--cell-tracked-p 1 0))))

(ert-deftest pacmacs--track-point-test ()
  (let ((pacmacs--track-board (list :width 2
                                     :height 2
                                     :data [[nil nil]
                                            [nil nil]])))
    (pacmacs--track-point (cons 0 0) (cons 0 1))
    (should (equal [[right nil]
                    [nil nil]]
                   (plist-get pacmacs--track-board :data)))
    (pacmacs--track-point (cons 1 0) (cons 1 -1))
    (should (equal [[right nil]
                    [left nil]]
                   (plist-get pacmacs--track-board :data)))))

(ert-deftest pacmacs--track-object-test ()
  (let ((pacmacs--track-board (list :width 2
                                     :height 2
                                     :data [[right down]
                                            [up left]]))
        (game-object (list :row 0
                           :column 0)))
    (with-mock
     (mock (pacmacs--switch-direction (list :row 0 :column 0) 'right) :times 1)
     (pacmacs--track-object game-object))))

(ert-deftest pacmacs--put-object-test ()
  (let ((pacmacs--render-board (list :width 2
                                      :height 2
                                      :data [[nil nil]
                                             [nil nil]]))
        (anim-object (list :row 0 :column 1)))
    (pacmacs--put-object anim-object)
    (should (equal (list :width 2
                         :height 2
                         :data [[nil (:row 0 :column 1)]
                                [nil nil]])
                   pacmacs--render-board))))
