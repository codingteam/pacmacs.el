
(ert-deftest pacmacs--cell-tracked-p-test ()
  (let ((pacmacs--track-board (list :width 2
                                     :height 2
                                     :data [[nil nil]
                                            ['left 'right]])))
    (should (not (pacmacs--cell-tracked-p 0 0)))
    (should (pacmacs--cell-tracked-p 1 0))))

(ert-deftest pacmacs--track-object-to-player-test ()
  (let ((pacmacs--track-board (list :width 3
                                    :height 2
                                    :data [[2 1 2]
                                           [2 0 2]]))
        (game-object (list :row 0
                           :column 0)))
    (with-mock
     (mock (pacmacs--wall-at-p * *) => nil)
     (mock (pacmacs--switch-direction (list :row 0 :column 0) 'right) :times 1)
     (pacmacs--track-object-to-player game-object))))

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

(ert-deftest pacmacs--handle-ghost-blinking-threshold-test ()
  (let ((pacmacs--terrified-ghosts '((:terrified-timer 900 :current-animation 1)
                                     (:terrified-timer 999 :current-animation 2)
                                     (:terrified-timer 1001 :current-animation 3)
                                     (:terrified-timer 899 :current-animation 4)))
        (pacmacs-tick-duration-ms 100)
        (pacmacs--ghost-blinking-threshold-ms 1000))
    (with-mock
     (mock (pacmacs-load-anim "Blinking-Terrified-Ghost") => 42 :times 2)
     (pacmacs--handle-ghost-blinking-threshold)
     (should (equal '((:terrified-timer 900 :current-animation 42)
                      (:terrified-timer 999 :current-animation 42)
                      (:terrified-timer 1001 :current-animation 3)
                      (:terrified-timer 899 :current-animation 4))
                    pacmacs--terrified-ghosts)))))

(ert-deftest pacmacs--load-next-level-test ()
  (let ((pacmacs-current-level 2)
        (pacmacs-levels [1 2 3]))
    (with-mock
     (mock (pacmacs--load-current-level) => nil)
     (pacmacs--load-next-level)
     (should (= 0 pacmacs-current-level)))))
