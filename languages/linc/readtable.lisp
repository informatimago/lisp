(in-package "COM.INFORMATIMAGO.LANGUAGES.LINC")

(defvar *c-readtable*
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) :preserve)
    rt)
  "Readtable to read S-expified C code.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *c-package-name*  "COM.INFORMATIMAGO.LANGUAGES.LINC.C")
  (defvar *c-opening-brace* 'COM.INFORMATIMAGO.LANGUAGES.LINC.C::\{ )
  (defvar *c-closing-brace* 'COM.INFORMATIMAGO.LANGUAGES.LINC.C::\} )
  (defvar *c-progn*         'COM.INFORMATIMAGO.LANGUAGES.LINC.C::|progn| ))

(defun read-c-sexp-list (stream)
  (let ((*package*   (load-time-value (find-package *c-package-name*)))
        (*readtable* *c-readtable*))
    (loop
      :with c-sexps := '()
      :for ch := (peek-char t stream)
      :do (case ch
            ((#\})
             (read-char stream)
             (return (nreverse c-sexps)))
            ((#\;)
             (read-line stream)
             (return (read-c-sexp-list stream)))
            ((#\#)
             (read-char stream)
             (if (char= #\| (peek-char nil stream))
                 (let ((object-after-comment (read stream)))
                   (if (eql *c-closing-brace* object-after-comment)
                       (return (nreverse c-sexps))
                       (push object-after-comment c-sexps)))
                 (progn
                   (unread-char #\# stream)
                   (push (read stream) c-sexps))))
            (otherwise
             (push (read stream) c-sexps))))))

(defun reader-macro-c-sexp-list (stream ch)
  (declare (ignore ch))
  (read-c-sexp-list stream))

(defun reader-dispatching-macro-c-sexp-list (stream ch sub)
  (declare (ignore ch sub))
  (cons *c-progn* (read-c-sexp-list stream)))

(defmacro enable-c-sexp-reader-macros ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-macro-character #.(character *c-opening-brace*)
                          (function reader-macro-c-sexp-list)
                          nil
                          *readtable*)
     (set-dispatch-macro-character #\# #.(character *c-opening-brace*)
                                   (function reader-dispatching-macro-c-sexp-list)
                                   *readtable*)))

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



(defun print-c-sexp-form (form &optional (*standard-output* *standard-output*))
  (let ((*package* (load-time-value (find-package *c-package-name*)))
        (*readtable* *c-readtable*)
        (*print-right-margin* 72))
    (write-string "{")
    (pprint form)
    (terpri)
    (write-line "}")
    (values)))

;;; ----------------------------------------

(test/reader-c-sexp-list)

(enable-c-sexp-reader-macros)

(defparameter *c-source* '#{

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



