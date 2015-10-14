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

(ert-deftest pacmacs--file-content-test ()
  (with-mock
   (should (equal "hello\n"
                  (pacmacs--file-content
                   "test-data/file-with-hello-string.txt")))))
