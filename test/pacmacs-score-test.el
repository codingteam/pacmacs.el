(ert-deftest pacmacs--sort-score-table-test ()
  (let ((input-table '(("hello" . 30)
                       ("world" . 20)
                       ("foo" . 40)
                       ("bar" . 10)))
        (expected-table '(("foo" . 40)
                          ("hello" . 30)
                          ("world" . 20)
                          ("bar" . 10))))
    (should (equal expected-table
                   (pacmacs--sort-score-table input-table)))))

(ert-deftest pacmacs--render-score-table-test ()
  (let ((input-table '(("foo" . 40)
                       ("hello" . 30)
                       ("world" . 20)
                       ("bar" . 10)))
        (expected-string (concat ;"Best Scores:\n"
                                 ;"------------\n"
                                 "foo      40\n"
                                 "hello    30\n"
                                 "world    20\n"
                                 "bar      10\n")))
    (with-temp-buffer
      (pacmacs--render-score-table input-table)
      (should (equal expected-string
                     (buffer-string))))))

(ert-deftest pacmacs--position-of-new-score-test ()
  (let ((score-table '(("hello" . 40)
                       ("world" . 30)
                       ("foo" . 20)
                       ("bar" . 10)))
        (new-score 25))
    (should (= 2 (pacmacs--position-of-new-score score-table new-score)))))
