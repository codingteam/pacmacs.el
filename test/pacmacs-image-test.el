(ert-deftest pacmacs--make-image-test ()
  (let ((expected-image (list :width 2
                              :height 3
                              :data [[nil nil]
                                     [nil nil]
                                     [nil nil]]
                              :name "khooy")))
    (should (equal expected-image
                   (pacmacs--make-image 2 3 "khooy")))))

(ert-deftest pacmacs--get-set-image-pixel-test ()
  (let ((image (pacmacs--make-image 10 10)))
    (should (null (pacmacs--get-image-pixel image 3 4)))
    (pacmacs--set-image-pixel image 3 4 "red")
    (should (equal "red" (pacmacs--get-image-pixel image 3 4)))))

(ert-deftest pacmacs--palette-from-image-test ()
  (let ((image (pacmacs--make-image 3 3))
        (expected-palette (list "red" "green" "blue")))
    (pacmacs--set-image-pixel image 0 0 "red")
    (pacmacs--set-image-pixel image 1 0 "green")
    (pacmacs--set-image-pixel image 1 1 "blue")
    (should (equal (sort expected-palette #'string<)
                   (sort (pacmacs--palette-from-image image) #'string<)))))

(ert-deftest pacmacs--make-palette-char-map-test ()
  (let ((palette (list "red" "green" "blue"))
        (expected-palette-char-map (list (cons "red" ?a)
                                         (cons "green" ?b)
                                         (cons "blue" ?c))))
    (should (equal expected-palette-char-map
                   (pacmacs--make-palette-char-map palette)))))

(ert-deftest pacmacs--render-xpm-palette-test ()
  (let ((palette-char-map (list (cons "red" ?a)
                                (cons "green" ?b)
                                (cons "blue" ?c)))
        (expected-xpm-palette (concat "\"a c red\",\n"
                                      "\"b c green\",\n"
                                      "\"c c blue\",\n")))
    (should (equal expected-xpm-palette
                   (pacmacs--render-xpm-palette palette-char-map)))))

(ert-deftest pacmacs--generate-xpm-palette-test ()
  (let ((palette (list "red" "green" "blue"))
        (expected-xpm-palette (concat "\"a c red\",\n"
                                      "\"b c green\",\n"
                                      "\"c c blue\",\n")))
    (should (equal expected-xpm-palette
                   (pacmacs--generate-xpm-palette palette)))))

