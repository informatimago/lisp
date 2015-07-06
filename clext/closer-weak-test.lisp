;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               closer-weak-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Test the closer-weak package.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-24 <PJB> Added this header.  Moved tests.lisp in here.
;;;;BUGS
;;;;
;;;;    - integrate tests with simple-test for standard reporting.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

;; #+clisp (pushnew :weak-test *features*)
;; #+#.#+(and clisp (not debug-weak))
;; (cl:if (cl:y-or-n-p "Are we debugging it on clisp?") '(and) '(or))
;; #-(and clisp (not debug-weak))'(or)  (pushnew :debug-weak *features*)


(defpackage "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK.TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK")
  #+clisp (:import-from "EXT" "GC")
  #+cmu   (:import-from "EXTENSIONS" "GC")
  #+ccl   (:import-from "CCL" "GC")
  (:shadowing-import-from "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK" "GETHASH")
  (:export "TEST/ALL"))
(in-package "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK.TEST")

#+sbcl (defun gc () (sb-ext:gc :full t))



;;;
;;; This tests.lisp is taken from clisp-2.38/tests/tests.lisp
;;; and modified to take only the weak-*.tst files we're interested in.
;;;

;; run the test suit:

(defun princ-error (c) (format t "~&[~A]: ~A~%" (type-of c) c))

#+old-clisp
;; Binding *ERROR-HANDLER* is a hammer technique for catching errors. It also
;; disables CLCS processing and thus breaks tests that rely on the condition
;; system, such as:
;;   - the compile-file on structure literal test in clos.lisp
;;   - all tests that use IGNORE-ERRORS
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(block ,b
       (let ((*error-handler*
              #'(lambda (&rest args)
                  (with-standard-io-syntax
                    (let* ((*print-readably* nil)
                           (error-message (apply #'format nil (cdr args))))
                      (terpri) (princ error-message)
                      (return-from ,b (values 'error error-message)))))))
         ,@forms))))

#+(or (and akcl (not gcl)) ecl)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym))
        (h (gensym)))
    `(block ,b
       (let ((,h (symbol-function 'system:universal-error-handler)))
         (unwind-protect
              (progn (setf (symbol-function 'system:universal-error-handler)
                           #'(lambda (&rest args) (return-from ,b 'error)))
                     ,@forms)
           (setf (symbol-function 'system:universal-error-handler) ,h))))))

#+allegro
(defmacro with-ignored-errors (&rest forms)
  (let ((r (gensym)))
    `(let ((,r (multiple-value-list (excl:errorset (progn ,@forms)))))
       (if (car ,r) (values-list (cdr ,r)) 'error))))

#-(or old-clisp (and akcl (not gcl)) ecl allegro)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(block ,b
       (handler-bind
           ((error #'(lambda (condition)
                       (princ-error condition)
                       (return-from ,b (values 'error
                                               (princ-to-string condition))))))
         ,@forms))))

(defun merge-extension (type filename)
  (make-pathname :type type :defaults filename))

;; (lisp-implementation-type) may return something quite long, e.g.,
;; on CMUCL it returns "CMU Common Lisp".
(defvar lisp-implementation
  #+clisp "CLISP" #+(and akcl (not gcl)) "AKCL" #+gcl "GCL" #+ecl "ECL" #+allegro "ALLEGRO" #+cmu "CMUCL"
  #-(or clisp akcl gcl ecl allegro cmu) (lisp-implementation-type))

(defvar *eval-method* :eval)
(defvar *eval-out* nil)
(defvar *eval-err* nil)
(defun my-eval (form)
  (when *eval-out* (get-output-stream-string *eval-out*))
  (when *eval-err* (get-output-stream-string *eval-err*))
  #+mocl
  (ecase *eval-method*
    (:eval (eval form)))
  #-mocl
  (ecase *eval-method*
    (:eval (eval form))
    (:compile (funcall (compile nil `(lambda () ,form))))
    (:both (let ((e-value (eval form))
                 (c-value (funcall (compile nil `(lambda () ,form)))))
             (unless (equalp e-value c-value)
               (error "eval: ~S; compile: ~S" e-value c-value))
             e-value))))

(defgeneric pretty-compare (result my-result log)
  (:documentation "print a pretty comparison of two results")
  (:method ((result sequence) (my-result sequence) (log stream))
    (let ((pos (mismatch result my-result :test #'equalp)))
      (let ((*print-length* 10))
        (if pos
            (flet ((pretty-tail-10 (seq)
                     (if (and (> (length seq) (+ pos 10))
                              (typep seq 'string))
                         (concatenate 'string (subseq seq pos (+ pos 10)) "...")
                         (subseq seq pos))))
              (format log "~&Differ at position ~:D: ~S vs ~S~%CORRECT: ~S~%~7A: ~S~%"
                      pos
                      (if (< pos (length result))
                          (elt result pos) 'end-of-sequence)
                      (if (< pos (length my-result))
                          (elt my-result pos) 'end-of-sequence)
                      (pretty-tail-10 result)
                      lisp-implementation
                      (pretty-tail-10 my-result)))
            (format log "~&Type mismatch: ~S should be ~S~%"
                    (type-of my-result) (type-of result))))))
  (:method ((result pathname) (my-result pathname) (log stream))
    (dolist (slot '(pathname-host pathname-device pathname-directory
                    pathname-name pathname-type pathname-version))
      (let ((s-r (funcall slot result)) (s-m (funcall slot my-result)))
        (format log "~&~S:~%CORRECT: ~S~%~7A: ~S~%~:[ DIFFERENT!~;same~]~%"
                slot s-r lisp-implementation s-m (equal s-r s-m)))))
  (:method ((result t) (my-result t) (log stream)))) ; do nothing

(defun show (object &key ((:pretty *print-pretty*) *print-pretty*))
  "Print the object on its own line and return it. Used in many tests!"
  (fresh-line) (prin1 object) (terpri) object)

(defun type-error-handler (err)
  "Print the condition and THROW.
Usage: (handler-bind ((type-error #'type-error-handler)) ...)"
  (princ-error err)
  (let ((da (type-error-datum err)) (et (type-error-expected-type err)))
    (show (list :datum da :expected-type et) :pretty t)
    (throw 'type-error-handler (typep da et))))

(defvar *test-ignore-errors* t)
(defvar *test-result-in-file* t
  "T: CLISP-style: evaluation result in the file after the test form.
NIL: sacla-style: forms should evaluate to non-NIL.")
(defun do-test (stream log)
  (let ((eof stream) (error-count 0) (total-count 0))
    (loop
       (let ((form (read stream nil eof)) out err (result nil))
         (when (or (eq form eof) (eq result eof)) (return))
         (if *test-result-in-file*
             (setq result (read stream))
             (setq form `(not ,form)))
         (incf total-count)
         (show form)
         (multiple-value-bind (my-result error-message)
             (if *test-ignore-errors*
                 (with-ignored-errors (my-eval form)) ; return ERROR on errors
                 (my-eval form)) ; don't disturb the condition system when testing it!
           (setq out (and *eval-out* (get-output-stream-string *eval-out*))
                 err (and *eval-err* (get-output-stream-string *eval-err*)))
           (cond ((eql result my-result)
                  (format t "~&EQL-OK: ~S~%" result)
                  (progress-success))
                 ((equal result my-result)
                  (format t "~&EQUAL-OK: ~S~%" result)
                  (progress-success))
                 ((equalp result my-result)
                  (format t "~&EQUALP-OK: ~S~%" result)
                  (progress-success))
                 (t
                  (incf error-count)
                  (progress-failure-message form "~&ERROR!! ~S should be ~S !~%" my-result result)
                  (format t "~&ERROR!! ~S should be ~S !~%" my-result result)
                  (format log "~&Form: ~S~%CORRECT: ~S~%~7A: ~S~%~@[~A~%~]"
                          form result lisp-implementation
                          my-result error-message)
                  (pretty-compare result my-result log)
                  (format log "~[~*~:;OUT:~%~S~%~]~[~*~:;ERR:~%~S~]~2%"
                          (length out) out (length err) err))))))
    (values total-count error-count)))

(defmacro check-ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (type-error (c)
       (if (ignore-errors
             (typep (type-error-datum c) (type-error-expected-type c)))
           (format nil "[~S --> ~A]: ~S is of type ~S" ',body c
                   (type-error-datum c) (type-error-expected-type c))
           c))
     (stream-error (c)
       (if (streamp (stream-error-stream c)) c
           (format nil "[~S --> ~A]: ~S is not a ~S" ',body c
                   (stream-error-stream c) 'stream)))
     (file-error (c)
       (let ((path (file-error-pathname c)))
         (if (or (pathnamep path) (stringp path) (characterp path)) c
             (format nil "[~S --> ~A]: ~S is not a ~S" ',body c
                     (file-error-pathname c) 'pathname))))
     (package-error (c)
       (let ((pack (package-error-package c)))
         (if (or (packagep pack) (stringp pack) (characterp pack)) c
             (format nil "[~S --> ~A]: ~S is not a ~S" ',body c
                     (package-error-package c) 'package))))
     (cell-error (c)
       (if (cell-error-name c) c
           (format nil "[~S --> ~A]: no cell name" ',body c)))
     (error (c) c)
     (:no-error (v) (format t "~&no error, value: ~S~%" v))))

(defun do-errcheck (stream log)
  (let ((eof "EOF") (error-count 0) (total-count 0))
    (loop
       (let ((form (read stream nil eof))
             (errtype (read stream nil eof)))
         (when (or (eq form eof) (eq errtype eof)) (return))
         (incf total-count)
         (show form)
         (let ((my-result (check-ignore-errors (my-eval form)))
               (out (and *eval-out* (get-output-stream-string *eval-out*)))
               (err (and *eval-err* (get-output-stream-string *eval-err*))))
           (multiple-value-bind (typep-result typep-error)
               (ignore-errors (typep my-result errtype))
             (cond ((and (not typep-error) typep-result)
                    (format t "~&OK: ~S~%" errtype)
                    (progress-success))
                   (t
                    (incf error-count)
                    (progress-failure-message form  "~&ERROR!! ~S instead of ~S !~%" my-result errtype)
                    (format t "~&ERROR!! ~S instead of ~S !~%" my-result errtype)
                    (format log "~&Form: ~S~%CORRECT: ~S~%~7A: ~S~%~
                                ~[~*~:;OUT:~%~S~%~]~[~*~:;ERR:~%~S~]~2%"
                            form errtype lisp-implementation my-result
                            (length out) out (length err) err)))))))
    (values total-count error-count)))


(defvar *dirpath* nil)
(eval-when (:compile-toplevel)
  (defparameter *dirpath* #.(make-pathname :name nil :type nil :version nil
                                           :defaults *compile-file-pathname*)))
(eval-when (:load-toplevel :execute)
  (defparameter *dirpath* (or *dirpath* (make-pathname :name nil :type nil :version nil
                                                       :defaults *load-pathname*))))


(defvar *run-test-tester* #'do-test)
(defvar *run-test-type* "tst")
(defvar *run-test-erg* "erg")
(defvar *run-test-truename*)
(defun run-test (testname
                 &key ((:tester *run-test-tester*) *run-test-tester*)
                 ((:ignore-errors *test-ignore-errors*)
                  *test-ignore-errors*)
                 ((:eval-method *eval-method*) *eval-method*)
                 (logname testname)
                 &aux (logfile (merge-extension *run-test-erg* logname))
                 error-count total-count *run-test-truename*)
  (let ((*default-pathname-defaults* *dirpath*))
   (with-open-file (s (merge-pathnames
                       (merge-extension *run-test-type* testname)
                       *dirpath* nil)
                     :direction :input)
     (setq *run-test-truename* (truename s))
     (format t "~&~s: started ~s~%" 'run-test s)
     (with-open-file (log (merge-pathnames logfile *dirpath* nil)
                          :direction :output
                          #+(or cmu sbcl) :if-exists
                          #+(or cmu sbcl) :supersede
                          #+ansi-cl :if-exists #+ansi-cl :new-version)
       (setq logfile (truename log))
       (let* ((*package* *package*) (*print-circle* t) (*print-pretty* nil)
              (*eval-err* (make-string-output-stream))
              (*error-output* (make-broadcast-stream *error-output* *eval-err*))
              (*eval-out* (make-string-output-stream))
              (*standard-output* (make-broadcast-stream *standard-output*
                                                        *eval-out*)))
         (setf (values total-count error-count)
               (funcall *run-test-tester* s log))))))
  (format t "~&~s: finished ~s (~:d error~:p out of ~:d test~:p)~%"
          'run-test testname error-count total-count)
  (if (zerop error-count)
      (delete-file logfile)
      (format t "~s: see ~a~%" 'run-test logfile))
  (list testname total-count error-count))

(defun report-results (res)
  "res = list of RUN-TEST return values (testname total-count error-count)"
  (let ((error-count (reduce #'+ res :key #'third)))
    (format
        t "~&finished ~3d file~:p:~31T ~3:d error~:p out of~50T ~5:d test~:p~%"
        (length res) error-count (reduce #'+ res :key #'second))
    (loop :with here = (truename "./") :for rec :in res :for count :upfrom 1 :do
       (format t "~&~3d ~25@a:~31T ~3:d error~:p out of~50T ~5:d test~:p~%"
               count (enough-namestring (first rec) here)
               (third rec) (second rec)))
    error-count))

(defun run-some-tests (&key (dirlist '("./"))
                       ((:eval-method *eval-method*) *eval-method*))
  (let ((files (mapcan (lambda (dir)
                         (directory (make-pathname :name :wild
                                                   :type *run-test-type*
                                                   :defaults dir)))
                       dirlist)))
    (if files (report-results (mapcar #'run-test files))
        (warn "no ~S files in directories ~S" *run-test-type* dirlist))))

(defun run-all-tests (&key (disable-risky t)
                           (verbose t)
                      ((:eval-method *eval-method*) *eval-method*))
  (let ((res ())
        #+clisp (custom:*load-paths* nil)
        (*features* (if disable-risky *features*
                        (cons :enable-risky-tests *features*)))
        (*standard-output* (if verbose
                               *standard-output*
                               (make-broadcast-stream))))
    ;; Since weakptr can run on #+cmu, we should run
    ;; the other too with CLOSER-WEAK.
    (dolist (ff '(#+(or clisp cmu sbcl)                           "weak-oid"
                  #+(or clisp cmu sbcl)                           "weak"
                  #+(or clisp cmu sbcl allegro openmcl lispworks) "weakhash"
                  #+(or clisp cmu sbcl lispworks)                 "weakhash2"
                  ))
      (push (run-test ff) res))
    #+(or clisp cmu sbcl allegro lispworks)
    (let ((tmp (list "weakptr" 0 0)))
      (push tmp res)
      (dotimes (i 20)
        (let ((weak-res (run-test "weakptr")))
          (incf (second tmp) (second weak-res))
          (incf (third tmp) (third weak-res)))))
    (report-results (nreverse res))))


(define-test test/all ()
  (run-all-tests :verbose nil))


#-(and) (progn

          (mapcar
           (lambda (x)
             (list (weak-pointer-p x)
                   (weak-list-p x)
                   (weak-and-relation-p x)
                   (weak-or-relation-p x)
                   (weak-mapping-p x)
                   (weak-and-mapping-p x)
                   (weak-or-mapping-p x)
                   (weak-alist-p x)))
           (list '(a b c)
                 #(a b c)
                 (make-weak-pointer (list 'x))
                 (make-weak-list (list 'x 'y 'z))
                 (make-weak-and-relation (list (list 'x)))
                 (make-weak-or-relation (list (list 'x)))
                 (make-weak-mapping '#:g15 '#:g16)
                 (make-weak-and-mapping (list '#:g15 '#:g16) '#:g17)
                 (make-weak-or-mapping (list '#:g15 '#:g16) '#:g17)
                 (make-weak-alist)))

          (let ((a (list 'x))
                (b (list 'y)))
            (let ((w (make-weak-and-mapping (list a) b)))
              (gc)
              (list (multiple-value-list (weak-and-mapping-pair w))
                    (multiple-value-list (weak-and-mapping-value w)))))



          (let ((a (list 'x))
                (b (list 'y)))
            (let ((w (make-weak-or-mapping (list a) b)))
              (gc)
              (list (multiple-value-list (weak-or-mapping-pair w))
                    (multiple-value-list (weak-or-mapping-value w)))))

          (let
              ((a1 (list 'x1)) (a2 (list 'x2)) (a3 (list 'x3)) (a4 (list 'x4))
               (a5 (list 'x5)))
            (let
                ((w1 (make-weak-alist :initial-contents (list (cons a3 a4))))
                 (w2 (make-weak-alist :initial-contents (list (cons a1 a2))))
                 (w3 (make-weak-alist :initial-contents (list (cons a4 a5))))
                 (w4 (make-weak-alist :initial-contents (list (cons a2 a3)))))
              (setq a1 nil a2 nil a4 nil a5 nil) (gc)
              (list (weak-alist-contents w2)
                    (weak-alist-contents w4)
                    (weak-alist-contents w1)
                    (weak-alist-contents w3))))


          (let ((a1 (list 'x1)) (a2 (list 'x2)) (a3 (list 'x3)) (a4 (list 'x4)) (a5 (list 'x5))) (let ((w1 (make-weak-alist :initial-contents (list (cons a3 a4)))) (w2 (make-weak-alist :initial-contents (list (cons a1 a2)))) (w3 (make-weak-alist :initial-contents (list (cons a4 a5)))) (w4 (make-weak-alist :initial-contents (list (cons a2 a3))))) (setq a1 nil a2 nil a4 nil a5 nil) (gc) (list (weak-alist-contents w2) (weak-alist-contents w4) (weak-alist-contents w1) (weak-alist-contents w3))))


          );; progn


;;;; THE END ;;;;
