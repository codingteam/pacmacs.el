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

(ert-deftest pacmacs--image-width-height-test ()
  (let ((image (pacmacs--make-image 10 20)))
    (should (equal 10 (pacmacs--image-width image)))
    (should (equal 20 (pacmacs--image-height image)))))

(ert-deftest pacmacs--make-image-from-data-test ()
  (let* ((input-data [["red" "blue" nil]
                      ["green"]
                      [nil nil nil]])
         (image (pacmacs--make-image-from-data input-data))
         (expected-pixels '((0 0 "red")
                            (1 0 "blue")
                            (2 0 nil)
                            (0 1 "green")
                            (1 1 nil))))
    (should (equal 3 (pacmacs--image-width image)))
    (should (equal 3 (pacmacs--image-height image)))
    (-each expected-pixels
      (-lambda ((x y color))
        (should (equal color (pacmacs--get-image-pixel image x y)))))))

(ert-deftest pacmacs--draw-image-test ()
  (let ((dest-image (pacmacs--make-image 5 5))
        (image (pacmacs--make-image-from-data
                [["red" "blue" "red"]
                 ["blue" "green" "blue"]
                 ["red" "blue" "red"]]))
        (expected-dest-data [["red" "blue" "red" nil   nil]
                             ["blue" "green" "blue" nil   nil]
                             ["red" "blue" "red" nil   nil]
                             [nil   nil   nil   "red" "blue"]
                             [nil   nil   nil   "blue" "green"]]))
    (pacmacs--draw-image dest-image image 0 0)
    (pacmacs--draw-image dest-image image 3 3)
    (dotimes (y (length expected-dest-data))
      (dotimes (x (length (aref expected-dest-data y)))
        (let ((color (aref (aref expected-dest-data y) x)))
          (should (equal color
                         (pacmacs--get-image-pixel dest-image x y))))))))

(ert-deftest pacmacs--draw-image-slice-test ()
  (let ((dest-image (pacmacs--make-image 5 5))
        (image (pacmacs--make-image-from-data
                [["red" "blue" "red"]
                 ["blue" "green" "blue"]
                 ["red" "blue" "red"]]))
        (expected-dest-data [["green" "blue" nil nil nil]
                             ["blue" "red" nil nil nil]
                             [nil "blue" "red" nil nil]
                             [nil "green" "blue" nil nil]
                             [nil nil nil nil nil]]))
    (pacmacs--draw-image-slice dest-image image 0 0 '(1 1 2 2))
    (pacmacs--draw-image-slice dest-image image 1 2 '(1 0 2 2))
    (dotimes (y (length expected-dest-data))
      (dotimes (x (length (aref expected-dest-data y)))
        (let ((color (aref (aref expected-dest-data y) x)))
          (should (equal color
                         (pacmacs--get-image-pixel dest-image x y))))))))

(ert-deftest pacmacs--fill-image-test ()
  (let* ((width 2)
         (height 2)
         (image (pacmacs--make-image width height))
         (color "red"))
    (pacmacs--fill-image image color)
    (dotimes (y height)
      (dotimes (x width)
        (should (equal color (pacmacs--get-image-pixel image x y)))))))
