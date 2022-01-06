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

(ert-deftest pacmacs--sort-score-table-test ()
  (let ((input-table '(("hello" . 30)
                       ("world" . 20)
                       ("foo" . 40)
                       ("bar" . 10)))
        (expected-table '(("foo" . 40)
                          ("hello" . 30)
                          ("world" . 20)
                          ("bar" . 10))))
    (should (equal expected-table
                   (pacmacs--sort-score-table input-table)))))

(ert-deftest pacmacs--render-score-table-test ()
  (let ((input-table '(("foo" . 40)
                       ("hello" . 30)
                       ("world" . 20)
                       ("bar" . 10)))
        (expected-string (concat "foo      40\n"
                                 "hello    30\n"
                                 "world    20\n"
                                 "bar      10\n")))
    (with-temp-buffer
      (pacmacs--render-score-table input-table)
      (should (equal expected-string
                     (buffer-string))))))

(ert-deftest pacmacs--position-of-new-score-test ()
  (let ((score-table '(("hello" . 40)
                       ("world" . 30)
                       ("foo" . 20)
                       ("bar" . 10)))
        (new-score 25))
    (should (= 2 (pacmacs--position-of-new-score score-table new-score)))))
