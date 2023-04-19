(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER.TESTS"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SIMPLE-TEST"
        "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP")
  (:export "TEST-ALL"))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER.TESTS.TOKENS"
  (:use))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER.TESTS")




(defclass test-scanner (comment-scanner-mixin buffered-scanner)
  ()
  (:default-initargs
   :spaces               (coerce #(#\space #\newline #\tab ) 'string)
   :token-kind-package   (load-time-value (find-package "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER.TESTS.TOKENS"))
   :token-symbol-package (load-time-value (find-package "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER.TESTS.TOKENS"))))

(defmethod scan-next-token ((scanner test-scanner) &optional parser-data)
  "RETURN: (scanner-current-token scanner)"
  (declare (ignore parser-data))
  (let (match)
    (setf match (string-match
                 (format nil "^([~A]+)" (coerce (scanner-spaces scanner) 'string))
                 (scanner-buffer scanner)
                 :start (1- (scanner-column scanner))))
    (when match
      (setf (scanner-column scanner) (1+ (match-end 1 match))))
    (let ((pos (1- (scanner-column scanner))))
      (cond ((scanner-end-of-source-p scanner)
             (setf (scanner-column scanner)        (1+ (length (scanner-buffer scanner)))
                   (scanner-current-text scanner)  "<END OF SOURCE>"
                   (scanner-current-token scanner) (token-end-of-source-kind)))

            ((scanner-end-of-line-p scanner)
             (advance-line scanner))

            ((setf match (string-match '"^([0-9][0-9]*)" (scanner-buffer scanner) :start pos))
             (setf (scanner-column scanner)        (1+ (match-end 1 match))
                   (scanner-current-text scanner)  (match-string 1 (scanner-buffer scanner) match)
                   (scanner-current-token scanner) 'integer))

            ((setf match (string-match '"^([A-Za-z][-A-Za-z0-9]*)" (scanner-buffer scanner) :start pos))
             (setf (scanner-column scanner)        (1+ (match-end 1 match))
                   (scanner-current-text scanner)  (match-string 1 (scanner-buffer scanner) match)
                   (scanner-current-token scanner) 'identifier))

            (t
             (error 'scanner-error-invalid-character
                    :file (scanner-file scanner)
                    :line (scanner-line scanner)
                    :column (scanner-column scanner)
                    :state (scanner-state scanner)
                    :current-token (scanner-current-token scanner)
                    :scanner scanner
                    :invalid-character (aref (scanner-buffer scanner) pos)
                    :format-control "Invalid character ~S at position: ~D~%"
                    :format-arguments (list (aref (scanner-buffer scanner) pos)
                                            (scanner-column scanner)))))))
  (setf (scanner-current-token scanner) (make-current-token scanner)))


(defun make-test-scanner (source)
  (make-instance 'test-scanner :source source))

(defun scan-all-tokens (scanner)
  (loop :for token := (progn (skip-spaces scanner) (scan-next-token scanner))
        :until (token-end-of-source-p token)
        :collect (ecase (token-kind token)
                   ((identifier) (intern (string-upcase (token-text token))
                                         (load-time-value *package*)))
                   ((integer)    (parse-integer (token-text token))))))


(define-test raw-scan ()
  (let ((scanner (make-test-scanner "
begin
    hello world
    421
end
")))
    (format t "Scanning text: ~S~%" (scanner-source scanner))
    (check equal
          (scan-all-tokens scanner)
          '(begin hello world 421 end))))

(define-test test-getchar ()
  (let* ((input  "
begin ; comment
    hello world
;; anotheer comment
;;; comment
    421;comment
end;comment
")
         (scanner (make-test-scanner input)))
    (format t "Scanning text: ~S~%" (scanner-source scanner))
    (setf (slot-value scanner 'comment-syntaxes) (list (make-instance 'single-line-comment :start-token ";")))
    (check equal
           (with-output-to-string (*standard-output*)
             (loop
               :for ch := (getchar scanner)
               :while ch
               :do (princ ch)))
           input)))

(define-test test-skip-spaces ()
  (let* ((input  "
begin ; comment
    hello world
;; anotheer comment
;;; comment
    421;comment
end;comment
")
         (expected-output  '("begin" "hello" "world" "421" "end"))
         (scanner (make-test-scanner input)))
    (format t "Scanning text: ~S~%" (scanner-source scanner))
    (setf (slot-value scanner 'comment-syntaxes) (list (make-instance 'single-line-comment :start-token ";")))
    (check equal
           (print (loop
                    :until (scanner-end-of-source-p scanner)

                    :do (print (multiple-value-list (skip-spaces scanner)))
                    :collect (with-output-to-string (output)
                               (loop :for ch := (getchar scanner)
                                     :while (and ch
                                                 (not (or (find ch (scanner-spaces scanner))
                                                          (some (lambda (syntax)
                                                                  (when (comment-starting-p scanner syntax ch)
                                                                    (ungetchar scanner ch)
                                                                    (scan-comment scanner syntax)
                                                                    t))
                                                                (comment-syntaxes scanner)))))
                                     :do (princ ch output)
                                     :finally (when ch (ungetchar scanner ch))))))
           expected-output)))

(define-test test-semi-colon-comment ()
  (let ((scanner (make-test-scanner "
begin ; comment
    hello world
;; anotheer comment
;;; comment
    421;comment
end;comment
")))
    (format t "Scanning text: ~S~%" (scanner-source scanner))
    (setf (slot-value scanner 'comment-syntaxes) (list (make-instance 'single-line-comment :start-token ";")))
    (check equal
           (scan-all-tokens scanner)
           '(begin hello world 421 end))))


(define-test test-all ()
  (raw-scan)
  (test-getchar)
  (test-skip-spaces)
  (test-semi-colon-comment))




#|
(make-instance 'single-line-comment :start-token ";")
from-column-comment
multi-line-comment
recursive-line-comment

(define-test test-all ()
  (test-comment-))
|#
