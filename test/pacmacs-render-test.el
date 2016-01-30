(ert-deftest pacmacs-insert-image-test ()
  (let ((resource "khooy")
        (resource-vector "pew"))
    (with-mock
     (mock (insert-image resource " " nil resource-vector) => 42 :times 1)
     (should (= 42 (pacmacs-insert-image resource resource-vector))))))

(ert-deftest pacmacs--render-track-board-test ()
  (let ((track-board (pacmacs--make-board 2 2)))
    (pacmacs--cell-wrapped-set track-board 0 0 10)
    (with-temp-buffer
      (pacmacs--render-track-board track-board)
      (should (equal "\t10\t.\n\t.\t.\n"
                     (buffer-string))))))
