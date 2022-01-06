;; Copyright (C) 2015-2016 Codingteam

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(ert-deftest pacmacs-load-image-test ()
  (with-mock
   (mock (create-image "pew" 'xpm nil :heuristic-mask t) => 42 :times 1)
   (should (= 42 (pacmacs-load-image "pew")))))

(ert-deftest pacmacs-make-anim-test ()
  (should (equal (list :frames (list 1 2 3 4 5)
                       :current-frame 0
                       :duration-counter 0
                       :sprite-sheet 42)
                 (pacmacs-make-anim (number-sequence 1 5) 42))))

(ert-deftest pacmacs-anim-get-frame-test ()
  (let ((anim (list :frames (number-sequence 1 5)
                    :current-frame 2)))
    (should (equal 3 (pacmacs-anim-get-frame anim)))))

(ert-deftest pacmacs-anim-next-frame-test ()
  (let ((anim (list :frames (mapcar #'(lambda (x)
                                        (pacmacs-make-frame (+ 41 x) 100))
                                    (number-sequence 1 4)) 
                    :current-frame 2
                    :duration-counter 0)))
    (pacmacs-anim-next-frame anim 50)
    (should (equal 2 (plist-get anim :current-frame)))

    (pacmacs-anim-next-frame anim 100)
    (should (equal 3 (plist-get anim :current-frame)))

    (pacmacs-anim-next-frame anim 100)
    (should (equal 0 (plist-get anim :current-frame)))))

(ert-deftest pacmacs--anim-object-next-frame-test ()
  (with-mock
   (stub pacmacs-anim-next-frame => 42)
   (let ((anim-object '(:current-animation 41)))
     (pacmacs--anim-object-next-frame anim-object 100)
     (should (equal '(:current-animation 42)
                    anim-object)))))

(ert-deftest pacmacs-convert-aseprite-frame-test ()
  (let ((aseprite-frame '(khooy
                          (foo . bar)
                          (frame
                           (x . 1)
                           (y . 2)
                           (h . 3)
                           (w . 4))
                          (duration . 100)))
        (expected-frame (list :frame (list 1 2 4 3)
                              :duration 100)))
    (should (equal expected-frame
                   (pacmacs-convert-aseprite-frame aseprite-frame)))))

(ert-deftest pacmacs-aseprite-frame-get-order-test ()
  (let ((aseprite-frame '(khooy42.ase
                          (foo . bar)
                          (frame
                           (x . 1)
                           (y . 2)
                           (h . 3)
                           (w . 4)))))
    (should (equal 42 (pacmacs-aseprite-frame-get-order aseprite-frame)))))

(ert-deftest pacmacs-compare-aseprite-frames-test ()
  (let ((aseprite-frame1 '(khooy42.ase
                           (foo . bar)
                           (frame
                            (x . 1)
                            (y . 2)
                            (h . 3)
                            (w . 4))))
        (aseprite-frame2 '(khooy43.ase
                           (foo . bar)
                           (frame
                            (x . 1)
                            (y . 2)
                            (h . 3)
                            (w . 4)))))
    (should (pacmacs-compare-aseprite-frames aseprite-frame1 aseprite-frame2))
    (should (not (pacmacs-compare-aseprite-frames aseprite-frame2 aseprite-frame1)))))

(ert-deftest pacmacs-load-anim-test ()
  (let* ((input-aseprite-format '((frames
                                   (frame-3\.ase (frame (h . 3) (w . 3) (y . 3) (x . 3))
                                                 (duration . 400))
                                   (frame-2\.ase (frame (h . 2) (w . 2) (y . 2) (x . 2))
                                                 (duration . 300))
                                   (frame-1\.ase (frame (h . 1) (w . 1) (y . 1) (x . 1))
                                                 (duration . 200))
                                   (frame-0\.ase (frame (h . 0) (w . 0) (y . 0) (x . 0))
                                                 (duration . 100)))))
         (input-sprite-sheet 42)
         (expected-output (pacmacs-make-anim
                           (mapcar #'(lambda (x)
                                       (pacmacs-make-frame (make-list 4 x)
                                                          (* (1+ x) 100)))
                                   (number-sequence 0 3))
                           input-sprite-sheet)))
    (with-mock
     (mock (json-read-file *) => input-aseprite-format)
     (mock (pacmacs-load-image *) => input-sprite-sheet)
     (should (equal expected-output
                    (pacmacs-load-anim "foo"))))))

(ert-deftest pacmacs--anim-object-list-next-frame-test ()
  (with-mock
   (mock (pacmacs--anim-object-next-frame 42 43) :times 5)
   (pacmacs--anim-object-list-next-frame (make-list 5 42) 43)))
