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
