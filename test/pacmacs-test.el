
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

(ert-deftest pacmacs--terrify-ghost-test ()
  (let* ((pacmacs--ghost-terrified-time-ms 60065)
         (ghost (list :property-1 1
                      :property-2 2
                      :current-animation 'ghost-animation
                      :switch-direction-callback 'ghost-direction-switcher
                      :type 'ghost))
         (terrified-ghost (list :property-1 1
                                :property-2 2
                                :current-animation 'terrified-ghost-animation
                                :switch-direction-callback 'pacmacs--switch-direction-callback
                                :type 'terrified-ghost
                                :terrified-timer pacmacs--ghost-terrified-time-ms)))
    (with-mock
     (mock (pacmacs-load-anim "Terrified-Ghost") => 'terrified-ghost-animation)
     (should (equal terrified-ghost
                    (pacmacs--terrify-ghost ghost))))))

(ert-deftest pacmacs--unterrify-ghost-test ()
  (let* ((pacmacs--ghost-terrified-time-ms 60065)
         (terrified-ghost (list :property-1 1
                                :property-2 2
                                :direction 'right
                                :current-animation 'terrified-ghost-animation
                                :switch-direction-callback 'terrified-ghost-direction-switcher
                                :type 'terrified-ghost
                                :terrified-timer pacmacs--ghost-terrified-time-ms))
         (ghost (list :property-1 1
                      :property-2 2
                      :direction 'right
                      :current-animation 'terrified-ghost-animation
                      :switch-direction-callback 'ghost-direction-switcher
                      :type 'ghost
                      :terrified-timer pacmacs--ghost-terrified-time-ms)))
    (with-mock
     (mock (pacmacs--switch-direction * 'right))
     (mock (pacmacs--switch-direction-animation-callback "Red-Ghost")
           => 'ghost-direction-switcher)

     (should (equal ghost
                    (pacmacs--unterrify-ghost terrified-ghost))))))

(ert-deftest pacmacs--terrified-ghost-timed-out-p-test ()
  (should (not
          (pacmacs--terrified-ghost-timed-out-p
           (list :terrified-timer 60065))))
  (should (pacmacs--terrified-ghost-timed-out-p
           (list :terrified-timer 0)))
  (should (pacmacs--terrified-ghost-timed-out-p
           (list :terrified-timer -1))))

(ert-deftest pacmacs--terrify-all-ghosts-test ()
  (let* ((pacmacs--terrified-ghosts (number-sequence 1 5))
         (pacmacs--ghosts (number-sequence 1 6)))
    (with-mock
     (mock (pacmacs--terrify-ghost *) => 42 :times 11)
     (pacmacs--terrify-all-ghosts)

     (should (null pacmacs--ghosts))
     (should (= 11 (length pacmacs--terrified-ghosts))))))

(ert-deftest pacmacs--align-score-record-nickname-test ()
  (let ((pacmacs--max-score-nick-size 5)
        (nickname "abc"))
    (should (equal "abc  "
                   (pacmacs--align-score-record-nickname nickname)))))

(ert-deftest pacmacs--make-submit-nickname-action-test ()
  (with-mock
   (mock (widget-value 'widget) => 'nickname)
   (mock (pacmacs--add-entry-to-score-table 'nickname 'score))
   (mock (pacmacs--align-score-record-nickname 'nickname) => 'aligned-nickname)
   (mock (widget-value-set 'widget 'aligned-nickname))
   (mock (widget-delete 'widget))
   (funcall (pacmacs--make-submit-nickname-action 'score)
            'widget
            'khooy)))

(ert-deftest pacmacs--make-wall-cell-test ()
  (should (equal (list :current-animation nil
                       :row 10
                       :column 20
                       :type 'wall)
                 (pacmacs--make-wall-cell 10 20))))

(ert-deftest pacmacs--step-ghosts-test ()
  (let ((pacmacs--ghosts (make-list 10 'ghost)))
    (with-mock
     (mock (pacmacs--track-object-to-player 'ghost) :times 10)
     (mock (pacmacs--step-object 'ghost) :times 10)
     (pacmacs--step-ghosts))))
