
(ert-deftest pacmacs--render-track-board-test ()
  (let ((track-board (pacmacs--make-board 2 2)))
    (pacmacs--cell-wrapped-set track-board 0 0 10)
    (with-temp-buffer
      (pacmacs--render-track-board track-board)
      (should (equal "\t10\t.\n\t.\t.\n"
                     (buffer-string))))))
