
(ert-deftest pacmacs--make-canvas-test ()
  (should (pacmacs--make-canvas 80 80))
  (should-error (pacmacs--make-canvase 40 41)))

(ert-deftest pacmacs--make-tiles-grid-test ()
  (let* ((grid-width 2)
         (grid-height 3)
         (grid (pacmacs--make-tiles-grid grid-width
                                         grid-height)))
    (should (= grid-height (length grid)))
    (dotimes (i grid-height)
      (should (= grid-width (length (aref grid i)))))
    (dotimes (row grid-height)
      (dotimes (column grid-width)
        (let ((tile (aref (aref grid row) column)))
          (should (= pacmacs--canvas-tile-width (pacmacs--image-width tile)))
          (should (= pacmacs--canvas-tile-height (pacmacs--image-height tile))))))))
