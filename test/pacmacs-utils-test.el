(ert-deftest plist-map-test ()
  (let ((plist (list :foo 1
                     :bar 2)))
    (plist-map plist :bar #'1+)
    (should (equal (list :foo 1
                         :bar 3)
                   plist))))

(ert-deftest pacmacs--levelname-from-filename-test ()
  (should (equal "map06" (pacmacs--levelname-from-filename "map06.txt")))
  (should (not (pacmacs--levelname-from-filename "."))))

(ert-deftest pacmacs--make-2d-vector ()
  (should (equal [[nil nil]
                  [nil nil]
                  [nil nil]]
                 (pacmacs--make-2d-vector 2 3)))
  (should (equal [[42 42 42]
                  [42 42 42]]
                 (pacmacs--make-2d-vector 3 2 42))))

(ert-deftest pacmacs--intersect-rectangles-test ()
  (should (not (pacmacs--intersect-rectangles 5 4 -3 -4 3 4)))
  (should (not (pacmacs--intersect-rectangles 5 4 5 4 2 3)))
  (should (equal '(2 1 3 3)
                 (pacmacs--intersect-rectangles 5 4 2 1 5 4)))
  (should (equal '(1 1 2 2)
                 (pacmacs--intersect-rectangles 5 4 1 1 2 2)))
  (should (equal '(0 0 1 1)
                 (pacmacs--intersect-rectangles 5 4 -2 -3 3 4))))
