(ert-deftest pacmacs-load-image-test ()
  (with-mock
   (mock (create-image "pew" 'xpm nil :heuristic-mask t) => 42 :times 1)
   (should (= 42 (pacmacs-load-image "pew")))))

(ert-deftest pacmacs-insert-image-test ()
  (let ((resource "khooy")
        (resource-vector "pew"))
    (with-mock
     (mock (insert-image resource " " nil resource-vector) => 42 :times 1)
     (should (= 42 (pacmacs-insert-image resource resource-vector))))))

(ert-deftest pacmacs-create-color-block-test ()
  (let ((width 10)
        (height 20)
        (color "red")
        (bool-vector-result 42)
        (make-vector-result 43)
        (create-image-result 44))
    (with-mock
     (mock (make-bool-vector height t) => bool-vector-result :times 1)
     (mock (make-vector width bool-vector-result) => make-vector-result :times 1)

     (if (not pacmacs--flip-xbm-bits)
         (mock (create-image make-vector-result
                             'xbm t
                             :width width :height height
                             :foreground color
                             :background nil) => create-image-result :times 1)
       (mock (create-image make-vector-result
                           'xbm t
                           :width width :height height
                           :foreground nil
                           :background color) => create-image-result :times 1))

     (should (equal create-image-result
                    (pacmacs-create-color-block width height color))))))

(ert-deftest pacmacs-create-transparent-block ()
  (let ((width 10)
        (height 20)
        (bool-vector-result 42)
        (make-vector-result 43)
        (create-image-result 44))
    (with-mock
     (mock (make-bool-vector height nil) => bool-vector-result :times 1)
     (mock (make-vector width bool-vector-result) => make-vector-result :times 1)
     (mock (create-image make-vector-result
                         'xbm t
                         :width width
                         :height height) => create-image-result :times 1)
     (should (equal create-image-result
                    (pacmacs-create-transparent-block width height))))))

(ert-deftest pacmacs--put-bits-dot-test ()
  (let ((input-bits (pacmacs--construct-2d-bool-vector
                     '((nil nil nil)
                       (nil nil nil)
                       (nil nil nil))))
        (expected-bits (pacmacs--construct-2d-bool-vector
                        '((t   t   nil)
                          (t   t   nil)
                          (nil nil nil)))))
    (pacmacs--put-bits-dot input-bits 0 0 2)
    (should (equal expected-bits
                   input-bits))))

(ert-deftest pacmacs--put-vertical-bar-test ()
  (let ((input-bits (pacmacs--construct-2d-bool-vector
                     '((nil nil nil)
                       (nil nil nil)
                       (nil nil nil))))
        (expected-bits (pacmacs--construct-2d-bool-vector
                        '((t t nil)
                          (t t nil)
                          (t t nil)))))
    (pacmacs--put-vertical-bar input-bits 0 3 2)
    (should (equal (pacmacs--bits-to-lists expected-bits)
                   (pacmacs--bits-to-lists input-bits)))))

(ert-deftest pacmacs--put-horizontal-bar-test ()
  (let ((input-bits (pacmacs--construct-2d-bool-vector
                     '((nil nil nil)
                       (nil nil nil)
                       (nil nil nil))))
        (expected-bits (pacmacs--construct-2d-bool-vector
                        '((t   t   t)
                          (t   t   t)
                          (nil nil nil)))))
    (pacmacs--put-horizontal-bar input-bits 0 3 2)
    (should (equal (pacmacs--bits-to-lists expected-bits)
                   (pacmacs--bits-to-lists input-bits)))))
