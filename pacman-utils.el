
(defmacro plist-bind (keys expr &rest body)
  (declare (indent 2) (debug t))
  (let ((expr-name (gensym)))
    `(let* ((,expr-name ,expr)
            ,@(mapcar #'(lambda (key)
                          (cons (car key)
                                `((plist-get ,expr-name ,(cadr key)))))
                      keys))
       ,@body)))

(provide 'pacman-utils)
