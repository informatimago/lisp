;;;; -*- coding:utf-8 -*-

#.((lambda ()
     (let ((*standard-input*   *query-io*)
           (*standard-output*  *query-io*)
           (*error-output*     *query-io*)
           (*query-io*         *query-io*))
       (format t "~%;; Enter: (QUIT [result]) to get out of this REPL.")
       (finish-output)
       (do ((hist 1 (1+ hist))
            (+eof+ (gensym)))
           (nil)
         (format t "~%~A[~D]> " (package-name *package*) hist)
         (finish-output)
         (handler-case
             (progn
               (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
               (cond ((eq     - +eof+)   (return))
                     ((equalp - '(quit)) (return))
                     ((and (consp -) (equalp (car -) 'quit))
                      (return (ignore-errors (eval (second -))))))
               (setf /// //   // /   / (multiple-value-list (eval -)))
               (setf *** **   ** *   * (first /))
               (format t "~& --> ~{~S~^ ;~%     ~}~%" /))
           (error (err) (format t "~S~%~A~%" err err)))))))