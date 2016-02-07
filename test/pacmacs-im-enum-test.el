(ert-deftest pacmacs--parse-im-enum-header-test ()
  (let* ((input-header "# ImageMagick pixel enumeration: 80,40,255,srgba")
         (expected-result (list 80 40 255)))
    (should (equal expected-result
                   (pacmacs--parse-im-enum-header input-header)))
    (should-error (pacmacs--parse-im-enum-header "khooy") :type 'error)))

(ert-deftest pacmacs--parse-im-enum-pixel-test ()
  (let ((input-pixel "60,16: ( 255,  255,   255,    255)  #khooy")
        (expected-result (list 60 16 "#ffffff")))
    (should (equal expected-result
                   (pacmacs--parse-im-enum-pixel input-pixel 255)))
    (should-error (pacmacs--parse-im-enum-pixel "khooy") :type 'error)))

(ert-deftest pacmacs--read-im-enum-image-test ()
  (let* ((image-filename "test-data/2x2.txt")
         (image (pacmacs--read-im-enum-image image-filename))
         (expected-pixels '((0 0 "#ff0000")
                            (0 1 "#00ff00")
                            (1 0 "#0000ff")
                            (1 1 nil))))

    (should (= 2 (pacmacs--image-width image)))
    (should (= 2 (pacmacs--image-height image)))

    (-all-p (-lambda ((x y color))
              (should (equal color (pacmacs--get-image-pixel image x y))))
            expected-pixels)))
