
(ert-deftest pacmacs--squared-distance-test ()
  (should (= (pacmacs--squared-distance 1 1 3 3)
             8)))
