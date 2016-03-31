(ert-deftest pacmacs--point-within-rect-p-test ()
  (should (pacmacs--point-within-rect-p '(1 2) '(-1 -1 3 4)))
  (should (not (pacmacs--point-within-rect-p '(1 1) '(2 3 5 5))))
  (should (not (pacmacs--point-within-rect-p '(9 8) '(6 6 2 2)))))

(ert-deftest pacmacs--rect-luc-test ()
  (should (equal '(3 4) (pacmacs--rect-luc '(3 4 34 5)))))

(ert-deftest pacmacs--rect-rbc-test ()
  (should (equal '(4 6) (pacmacs--rect-rbc '(3 4 2 3)))))

(ert-deftest pacmacs--point-to-rect-test ()
  (should (equal '(3 4 3 2)
                 (pacmacs--point-to-rect '(3 4) '(5 5)))))

(ert-deftest pacmacs--rects-intersection-test ()
  (should (not (pacmacs--rects-intersection '(0 0 5 4) '(-3 -4 3 4))))
  (should (not (pacmacs--rects-intersection '(0 0 5 4) '(5 4 2 3))))
  (should (equal '(2 1 3 3)
                 (pacmacs--rects-intersection '(0 0 5 4) '(2 1 5 4))))
  (should (equal '(6 6 2 2)
                 (pacmacs--rects-intersection '(5 5 5 4) '(6 6 2 2))))
  (should (equal '(0 0 1 1)
                 (pacmacs--rects-intersection '(-2 -3 3 4) '(0 0 5 4)))))
