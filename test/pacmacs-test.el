
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
  (let ((pacmacs--object-board (list :width 2
                                      :height 2
                                      :data [[nil nil]
                                             [nil nil]]))
        (anim-object (list :row 0 :column 1)))
    (pacmacs--put-object anim-object)
    (should (equal (list :width 2
                         :height 2
                         :data [[nil ((:row 0 :column 1))]
                                [nil nil]])
                   pacmacs--object-board))))

(ert-deftest pacmacs--decrease-terrified-timers-test ()
  (let ((pacmacs--terrified-ghosts '((:terrified-timer 1000)
                                     (:terrified-timer 400)))
        (pacmacs-tick-duration-ms 200)
        (expected-outcome '((:terrified-timer 800)
                            (:terrified-timer 200))))
    (pacmacs--decrease-terrified-timers)
    (should (equal expected-outcome
                   pacmacs--terrified-ghosts))))

(ert-deftest pacmacs--run-away-direction-test ()
  (let ((runner '(:row 2 :column 2))
        (bogey '(:row 2 :column 3))
        (walls '((1 . 2)
                 (3 . 2)))
        (blocked-tile-predicate (-lambda (row column)
                                  (-find (-partial #'equal (cons row column))
                                         walls))))
    (should (equal (pacmacs--run-away-direction runner bogey
                                                blocked-tile-predicate)
                   'left))))

(ert-deftest pacmacs--replace-game-objects-test ()
  (let* ((game-objects '((:row 10 :column 20)
                         (:row 30 :column 40)))
         (replaced-objects nil)
         (destroyed-objects nil)
         (new-constructor (-lambda (row column)
                            (setq replaced-objects
                                  (cons (list :row (1+ row)
                                              :column (1+ column))
                                        replaced-objects))))
         (old-destructor (-lambda (game-object)
                           (setq destroyed-objects (cons game-object
                                                         destroyed-objects)))))
    (pacmacs--replace-game-objects game-objects new-constructor old-destructor)
    (should (equal '((:row 10 :column 20)
                     (:row 30 :column 40))
                   game-objects))
    (should (equal '((:row 31 :column 41)
                     (:row 11 :column 21))
                   replaced-objects))
    (should (equal '((:row 30 :column 40)
                     (:row 10 :column 20))
                   destroyed-objects))))
