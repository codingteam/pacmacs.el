(require 'cl)                           ;el-mock doesn't work without
                                        ;this
(require 'el-mock)
(require 'undercover)
(require 'dash)

(undercover "*.el")

(defun pacmacs--list-to-bool-vector (xs)
  (let* ((index 0)
         (size (length xs))
         (result (make-bool-vector (length xs) nil)))
    (dolist (x xs)
      (aset result index x)
      (cl-incf index))
    result))

(defun pacmacs--bool-vector-to-list (xs)
  (-map #'identity xs))

(defun pacmacs--construct-2d-bool-vector (data)
  (apply #'vector
         (-map #'pacmacs--list-to-bool-vector data)))

(defun pacmacs--bits-to-lists (bits)
  (-map #'pacmacs--bool-vector-to-list bits))

(add-to-list 'load-path ".")
(load "pacmacs.el")
