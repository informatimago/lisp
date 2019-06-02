(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")


(defun test/reader-c-sexp-list ()
  (assert (equal
           (with-input-from-string (input "
        (sscanf b \"%d\" (address bv))
        (sprintf res \"%d\" (+ a b))
        (return res)
   })")
             (read-c-sexp-list input))
           '((com.informatimago.languages.linc.c::|sscanf| com.informatimago.languages.linc.c::\b "%d"
              (com.informatimago.languages.linc.c::|address| com.informatimago.languages.linc.c::|bv|))
             (com.informatimago.languages.linc.c::|sprintf| com.informatimago.languages.linc.c::|res| "%d"
              (com.informatimago.languages.linc.c::+ com.informatimago.languages.linc.c::\a
               com.informatimago.languages.linc.c::\b))
             (com.informatimago.languages.linc.c::|return| com.informatimago.languages.linc.c::|res|))))
  :success)

(defun test/read-ellipsis ()
  (map nil (lambda (result expected)
             (if (find expected #(simple-stream-error))
                 (assert (cl:typep result expected))
                 (assert (cl:equal result expected))))

       (mapcar (lambda (string)
                 (with-input-from-string (input string)
                   (read-char input)
                   (handler-case (read-ellipsis input)
                     (error (err) err))))
               '(". a b c"
                 ".;hello d e"
                 ".#foo d e"
                 ".abc d e"

                 ".. a b c"
                 "..;hello d e"
                 "..#foo d e"
                 "..abc d e"

                 "... a b c"
                 "...;hello d e"
                 "...#foo d e"
                 "...abc d e"))

       '(|.| |.| |.#FOO| .abc simple-stream-error simple-stream-error ..\#FOO ..abc |...| |...| ...\#FOO ...abc))
  :success)

(enable-c-sexp-reader-macros)

#-(and) (defparameter *c-source* '#{

                            (define-function string_add ((a (char *)) (b (char *))) (char *)
                              (let ((av int)
                                    (bv int)
                                    (res (char *) (malloc (+ 2 (max (strlen a) (strlen b))))))
                                (sscanf a "%d" (address av))
                                (sscanf b "%d" (address bv))
                                (sprintf res "%d" (+ a b))
                                (return res)))

                            })

;; (print-c-sexp-form *c-source*)

(defun test/all ()
  (test/reader-c-sexp-list)
  (test/read-ellipsis))

(test/all)
