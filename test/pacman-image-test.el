(ert-deftest pacman-load-image-test ()
  (let ((default-directory "/khooy/"))
    (with-mock
     (mock (create-image "/khooy/pew" 'xpm nil :heuristic-mask t) => 42 :times 1)
     (should (= 42 (pacman-load-image "pew"))))))

(ert-deftest pacman-insert-image-test ()
  (let ((resource "khooy")
        (resource-vector "pew"))
    (with-mock
     (mock (insert-image resource " " nil resource-vector) => 42 :times 1)
     (should (= 42 (pacman-insert-image resource resource-vector))))))

(ert-deftest pacman-create-color-block-test ()
  (let ((width 10)
        (height 20)
        (color "red")
        (bool-vector-result 42)
        (make-vector-result 43)
        (create-image-result 44))
    (with-mock
     (mock (make-bool-vector height t) => bool-vector-result :times 1)
     (mock (make-vector width bool-vector-result 'xbm t
                        :width width :height height
                        :foreground color) => make-vector-result :times 1)
     (mock (create-image make-vector-result) => create-image-result :times 1)
     (should (equal create-image-result
                    (pacman-create-color-block width height color))))))

(ert-deftest pacman-create-transparent-block ()
  (let ((width 10)
        (height 20)
        (bool-vector-result 42)
        (make-vector-result 43)
        (create-image-result 44))
    (with-mock
     (mock (make-bool-vector height nil) => bool-vector-result :times 1)
     (mock (make-vector width bool-vector-result 'xbm t
                        :width width :height height) => make-vector-result :times 1)
     (mock (create-image make-vector-result) => create-image-result :times 1)
     (should (equal create-image-result
                    (pacman-create-transparent-block width height))))))
