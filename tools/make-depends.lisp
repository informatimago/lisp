;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              make-depends.lisp
;;;;LANGUAGE:          Common-Lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;
;;;;    This script generates dependencies for lisp sources, based on 
;;;;    (require) sexps, a load-path, a set of logical pathname translations
;;;;    and ad-hoc processing.
;;;;    
;;;;    Object files can be either elisp compiled (.elc) or clisp compiled
;;;;    (.fas), cmucl compiled (.x86f), or sbcl compiled (.fasl).
;;;;    and source files can be either elisp (.el) or clisp or cmucl (.lisp,
;;;;    .lsp, .cl), and elisp sources may (require) common-lisp files
;;;;    (.lisp, .lsp, .cl extensions for sources, but .elc compiled form).
;;;;
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-10 <PJB> Completed MAKE-ASD.
;;;;    2003-05-04 <PJB> Converted to Common-Lisp from emacs.
;;;;    2002-11-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2002 - 2012
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
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages))
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (com.informatimago.common-lisp.cesarum.package:add-nickname
   "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML" "HTML"))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.TOOLS.MAKE-DEPENDS"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
  (:export
   "GENERATE-SUMMARY" "MAKE-COMPONENTS" "MAKE-ASD-SEXP" "GENERATE-ASD"
   "GET-CLOSED-DEPENDENCIES" "GET-DEPENDENCIES" "GET-PACKAGE" "GET-DEPENDS"
   "MAKE-DEPENDS")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
                "SAFE-TEXT-FILE-TO-STRING-LIST")
  (:documentation
   "This script generates dependencies for lisp sources, based on 
    (require) sexps, a load-path, a set of logical pathname translations
    and ad-hoc processing.
    
    Copyright Pascal J. Bourguignon 2003 - 2005
    This package is provided under the GNU General Public Licence.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.TOOLS.MAKE-DEPENDS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro pdebug (&rest args)
  (when (member :debug *features*)
    `(format t ,@args)))

(defmacro show (&rest expressions)
  (when (member :debug *features*)
      `(progn
         ,@(mapcar (lambda (expr) `(format *trace-output* "~20A = ~S~%" ',expr ,expr))
                   expressions))))


(defvar extensions-emacs '(("el" . "elc"))
  "A list of cons of extensions for emacs lisp source and object files.")


(defvar extensions-clisp  '(("lisp" . "fas")  ("lsp" . "fas")  ("cl" . "fas")
                            ("lisp" . "fasl") ("lsp" . "fasl") ("cl" . "fasl")
                            ("lisp" . "x86f") ("lsp" . "x86f") ("cl" . "x86f"))
  "A list of cons of extensions for Common-Lisp source and object files.")


(defun object-extensions (sext-oext-list)
  (delete-duplicates (mapcar (function cdr) sext-oext-list)))
  

(defun source-extensions (sext-oext-list)
  (delete-duplicates (mapcar (function car) sext-oext-list)))



(defun read-sexp-from-file (stream &optional (eof-symbol :eof))
  (handler-case (read stream nil eof-symbol)
    (error (err)
      (format *error-output* "#!!! ~A: ~A~%" (pathname stream) err)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass source-package ()
  ((name                  :initarg :name                  
                          :type string
                          :accessor source-package-name)
   (nicknames             :initarg :nicknames             
                          :type list
                          :accessor source-package-nicknames)
   (documentation         :initarg :documentation         
                          :type (or null string)
                          :accessor source-package-documentation)
   (use                   :initarg :use                   
                          :type list
                          :accessor source-package-use)
   (shadow                :initarg :shadow                
                          :type list
                          :accessor source-package-shadow)
   (shadowing-import-from :initarg :shadowing-import-from 
                          :type list
                          :accessor source-package-shadowing-import-from)
   (import-from           :initarg :import-from
                          :type list
                          :accessor source-package-import-from)
   (export                :initarg :export                
                          :type list
                          :accessor source-package-export)
   (intern                :initarg :intern                
                          :type list
                          :accessor source-package-intern))
  (:documentation "The description of a defpackage in a source file."))


(defmethod print-object ((self source-package) stream)
  (flet ((option (key slot) (when (slot-value self slot)
                              `((,key ,(slot-value self slot))))))
    (format stream "#.~S"
     `(defpackage ,(source-package-name self)
        ,@(option :nicknames             'nicknames)
        ,@(option :use                   'use)
        ,@(option :shadow                'shadow)
        ,@(option :shadowing-import-from 'shadowing-import-from)
        ,@(option :import-from           'import-from)
        ,@(option :export                'export)
        ,@(option :intern                'intern)
        ,@(when (source-package-documentation self)
                `((:documentation ,(source-package-documentation self)))))))
  self)


(defclass source-file ()
  ((pathname         :initarg :pathname
                     :type pathname
                     :accessor source-file-pathname)
   (external-format  :initarg :external-format
                     :initform :default
                     :accessor source-file-external-format)
   (emacs-variables  :initarg :emacs-variables
                     :type list
                     :accessor source-file-emacs-variables)
   (header           :initarg :header
                     :type list
                     :accessor source-file-header)
   (packages-defined :initarg :packages-defined
                     :type list
                     :accessor source-file-packages-defined
                     :documentation "A list of SOURCE-PACKAGE instances, one 
for each of the DEFPACKAGE forms found in the source file.")
   (packages-used    :initarg :packages-used
                     :type list
                     :accessor source-file-packages-used
                     :documentation "Each item is (operator package-name) with
operator being one of CL:IN-PACKAGE CL:USE-PACKAGE CL-USER::ALSO-USE-PACKAGES
CL:IMPORT CL:SHADOWING-IMPORT")
   (added-nicknames  :initarg :added-nicknames
                     :type list
                     :accessor source-file-added-nicknames
                     :documentation "A list of PACKAGE:ADD-NICKNAMES forms.")
   (requires         :initarg :requires
                     :type list
                     :accessor source-file-requires
                     :documentation "A list of CL:REQUIRE forms.")
   (provides         :initarg :provides
                     :type list
                     :accessor source-file-provides
                     :documentation "A list of CL:PROVIDES forms."))
  (:documentation "The description of the packages and dependencies of a source file."))


(defmethod print-object ((self source-file) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~& :PATHNAME ~S"         (source-file-pathname self))
    (format stream "~& :EXTERNAL-FORMAT ~S"  (source-file-external-format self))
    (format stream "~& :HEADER ~S"           (source-file-header self))
    (format stream "~& :PACKAGES-DEFINED ~S" (source-file-packages-defined self))
    (format stream "~& :PACKAGES-USED ~S"    (source-file-packages-used self))
    (format stream "~& :ADDED-NICKNAMES ~S"  (source-file-added-nicknames self))
    (format stream "~& :REQUIRES ~S"         (source-file-requires self))
    (format stream "~& :PROVIDES ~S"         (source-file-provides self)))
  self)


(defun symbol-package-name (symbol)
  (package-name (symbol-package symbol)))

(defgeneric dependencies (self))

(defmethod dependencies ((self source-package))
    "
RETURN:     A list of packages the source-package depends on.
"
    (delete-duplicates
     (nconc
      (copy-list (source-package-use self))
      ;; Kind of an anti-dependency:
      ;; (mapcar (function symbol-package-name) (source-package-shadow self))
      (mapcar (function first)
              (source-package-shadowing-import-from self))
      (mapcar (function first)
              (source-package-import-from self)))
     :test (function string=)))

(defmethod dependencies ((self source-file))
    "
RETURN:     A list of packages the source-file depends on.
"
    (delete-duplicates
     (nconc
      (mapcan (lambda (used)
                (ecase (first used)
                  ((cl:in-package cl:use-package)
                   (list (second used)))
                  ((cl-user::also-use-packages)
                   (copy-list (rest used)))
                  ((cl:import cl:shadowing-import)
                   (when (eq 'quote (caadr used))
                     (if (consp (cadadr used))
                         (mapcar (function symbol-package-name)
                                 (cadadr used))
                         (list (symbol-package-name (cadadr used))))))))
              (source-file-packages-used self))
      (mapcan (function dependencies)
              (source-file-packages-defined self)))
     :test (function string=)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scan a header p-list for specific fields:
;;;

(defun header-slot (header slot)
  (getf header slot))

(defun header-licence (header)
  "
RETURN: Just the LICENCE from the LEGAL section.
"
  (let ((legal (member :legal header)))
    (when legal (first (ensure-list (cadr legal))))))

(defun header-description (header)
  (let ((description (member :description header)))
    (when description
      (list-trim '("") (ensure-list (cadr description))
                 :test (function string=)))))

(defun header-authors (header)
  (let ((authors (member :authors header)))
    (when authors
      (list-trim '("") (ensure-list (cadr authors))
                 :test (function string=)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs File Local Variables
;;;

(defun parse-emacs-variables (line)
  (when (search "-*-" line)
    (flet ((chunk (text start end) (string-trim " " (subseq text start end))))
      (loop
         :with start = (+ 3 (search "-*-" line))
         :with end = (or (search "-*-" line :start2 start) (length line))
         :with result = '()
         :for colon = (and (< start end) (position #\: line :start start))
         :while (and colon (< colon end))
         :do (let ((vend
                    (or (and (< (1+ colon) end)
                             (position #\; line :start (1+ colon) :end end))
                        end)))
               (push (intern (string-upcase (chunk line start colon)) "KEYWORD")
                     result)
               (push (chunk line (1+ colon) vend) result)
               (setf start (1+ vend)))
         :finally (return (nreverse result))))))


(defun file-emacs-variables (filepath)
  "
RETURN:  A p-list containing the emacs file local variables in the file at FILEPATH.
"
  (let* ((start-label "Local Variables:")
         (end-label   "End:")
         (lines (let ((*readtable* (copy-readtable)))
                  ;; Quick and dirty disable : --> read three or four tokens
                  ;; for pack:sym or pack::sym
                  (set-macro-character #\: (lambda (stream char)
                                             (declare (ignore stream char))
                                             #\:) nil)
                  (safe-text-file-to-string-list filepath)))
         (first-line (if (and (< 2 (length (first lines)))
                              (string= "#!" (first lines) :end2 2))
                         (second lines)
                         (first lines)))
         (first-line-variables (parse-emacs-variables first-line))
         (rlines (reverse lines))
         (endc   (loop
                    :for cur :on rlines
                    :until (or (null cur) (search end-label   (car cur)))
                    :finally (return cur)))
         (begc   (loop
                    :for cur :on endc
                    :until (or (null cur) (search start-label (car cur)))
                    :finally (return cur)))
         (vars   (or (nreverse (ldiff endc (cdr begc)))
                     (return-from file-emacs-variables first-line-variables)))
         (start  (search start-label (first vars)))
         (end    (+ start (length start-label)))
         (prefix (subseq (first vars) 0 start))
         (suffix (subseq (first vars) end))
         (vars   (mapcan
                  (lambda (line)
                    (if (and (<= (+ (length prefix) (length suffix))
                                 (length line))
                             (string= prefix line :end2 (length prefix))
                             (string= suffix line
                                      :start2 (- (length line) (length suffix))))
                        (list (subseq line (length prefix)
                                      (- (length line) (length suffix))))
                        nil)) vars))
         (vars   (loop
                    :with result = '()
                    :with line = ""
                    :for cur :in vars
                    :do (if (and (< 1 (length cur))
                                 (char= #\\ (aref cur (1- (length cur)))))
                            (setf line (concatenate
                                           'string line
                                           (subseq cur 0 (1- (length cur)))))
                            (progn
                              (push (concatenate 'string line cur) result)
                              (setf line "")))
                    :finally (return (nreverse result)))))
    (nconc first-line-variables
           (let ((*readtable* (copy-readtable)))
             ;; (setf (readtable-case *readtable*) :preserve)
             (mapcan
              (lambda (line)
                (let ((colon (position #\: line)))
                  (when colon
                    (list (intern (string-upcase
                                   (string-trim " " (subseq line 0 colon)))
                                  "KEYWORD")
                          (read-from-string (subseq line (1+ colon)))))))
                     (butlast (rest vars)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SOURCE-HEADER
;;;



;; Patch #+clisp REGEXP:MATCH-STRING to work on multi-byte encodings:
;; (in-package :regexp)
;; (ext:without-package-lock (:regexp)
;;   (intern "BYTES" :regexp))
;; (ext:without-package-lock (:regexp)
;;   (defun match-string (string match)
;;     (let ((start (match-start match))
;;           (end   (match-end match))
;;           (bytes (ext:convert-string-to-bytes
;;                   string custom:*misc-encoding*)))
;;       (ext:convert-string-from-bytes
;;        (make-array (- end start)
;;                    :element-type '(unsigned-byte 8)
;;                    :displaced-to bytes
;;                    :displaced-index-offset start)
;;        custom:*misc-encoding*))))
;; (in-package :source)


(defun count-re-groups (re)
  "
RETURN: The number of occurences of '\(' in the string RE.
"
  (loop
     :for pos = (search "\\(" re) :then (search "\\(" re :start2 (1+ pos))
     :while pos
     :count 1))


#+clisp
(defun safe-encode (string)
  (ext:convert-string-from-bytes
   (ext:convert-string-to-bytes string charset:utf-8)
   charset:iso-8859-1))

#+clisp
(defun safe-decode (string)
  (ext:convert-string-from-bytes
   (ext:convert-string-to-bytes string charset:iso-8859-1)
   charset:utf-8))

#+clisp
(defparameter *patched-clisp-p*
  (ext:letf ((custom:*misc-encoding* charset:iso-8859-1))
    (= 1 (regexp:match-start (regexp:match "t" "été" )))))

(defun safe-regexp-compile (regexp)
  #-clisp (declare (ignore regexp))
  #+clisp (ext:letf ((custom:*misc-encoding* charset:iso-8859-1))
            (if  *patched-clisp-p*
              ;; We've got an unpatched clisp
              (regexp:regexp-compile (safe-encode regexp))
              ;; We've got a patched clisp
              (regexp:regexp-compile regexp)))
  #-clisp nil)

(defun safe-regexp-exec (regexp string)
  #-clisp (declare (ignore regexp string))
  #+clisp (ext:letf ((custom:*misc-encoding* charset:iso-8859-1))
            (if  *patched-clisp-p*
              ;; We've got an unpatched clisp
              (regexp:regexp-exec regexp (safe-encode string))
              ;; We've got a patched clisp
              (regexp:regexp-exec regexp string)))
  #-clisp (values))


(defun safe-regexp-match-string (string match)
  #-clisp (declare (ignore string match))
  #+clisp  (ext:letf ((custom:*misc-encoding* charset:iso-8859-1))
             (if  *patched-clisp-p*
               ;; We've got an unpatched clisp
               (safe-decode (regexp:match-string (safe-encode string) match))
               ;; We've got a patched clisp
               (regexp:match-string string match)))
  #-clisp "")


(defun read-source-header (stream
                           &key (comment-start #+clisp "^[;]\\+" #-clisp ";")
                           (comment-end) (keep-spaces nil))
  "
RETURN: A p-list containing the header found in the stream
        at the current position;
        the last line read.
NOTE:   Reading stops as soon as a non-comment line is read.
        If the stream can be positionned, the FILE-POSITION is set at the 
        beginning of this first non-comment line.
"
  (show stream (stream-external-format stream)
        keep-spaces comment-start comment-end)
  (flet ((chunk (text start end) (string-trim " " (subseq text start end)))
         (clean (cont)
           (setf cont (nreverse cont))
           (when (string= "" (car cont)) (pop cont))
           cont))
    (let (#+clisp(saved custom:*misc-encoding*))
      (unwind-protect
           (progn
             #+clisp(setf custom:*misc-encoding* (stream-external-format stream))
             (loop
                :with asso  = '()
                :with cont  = '()
                #+clisp :with #+clisp re #+clisp =
                #+clisp (safe-regexp-compile
                         (format nil "~A\\(.*\\)~:[~;~:*~A~]"
                                 comment-start comment-end))
                #+clisp :with #+clisp gindex #+clisp =
                #+clisp (1+ (count-re-groups comment-start))
                :for pos  = (file-position stream)
                :for line = (read-line stream nil nil)
                :for meat
                = (and
                   line 
                   #+clisp
                   (let ((matches (multiple-value-list
                                   (safe-regexp-exec re line))))
                     (when (and matches (nth gindex matches))
                       (safe-regexp-match-string line (nth gindex matches))))
                   #-clisp
                   (let ((pstart (length comment-start)))
                     (when (and (<= (length comment-start) (length line))
                                (string= comment-start line
                                         :end2 (length comment-start)))
                       (let ((pend
                              (if comment-end
                                  (let ((pend (- (length line)
                                                 (length comment-end))))
                                    (if (and (<= pstart pend)
                                             (string= comment-end line
                                                      :start2 pend))
                                        pend
                                        (length line)))
                                  (length line))))
                         (subseq line pstart pend)))))
                :while meat
                :do (cond
                      ((zerop (length meat)))
                      ((search "-*-" meat)
                       (let ((ev (parse-emacs-variables meat)))
                         #+(and clisp ffi)
                         (when (getf ev :coding)
                           (setf custom:*foreign-encoding*
                                 (or (symbol-value
                                      (find-symbol (string-upcase
                                                    (getf ev :coding))
                                                   "CHARSET"))
                                     custom:*foreign-encoding*)))
                         (setf asso (nconc (nreverse ev ) asso))))
                      ((alpha-char-p (aref meat 0)) ; a keyword
                       (when cont 
                         (push (clean cont) asso)
                         (setf cont nil))
                       (let ((pcolon (position (character ":") meat)))
                         (if pcolon
                             (progn     ; "keyword : value"
                               (push (intern (chunk meat 0 pcolon)
                                             "KEYWORD") asso)
                               (push (chunk meat (1+ pcolon) nil) asso))
                             (progn     ; "keyword" alone
                               (push (intern (chunk meat 0 nil) "KEYWORD") asso)
                               (push "" cont)))))
                      ((keywordp (car asso)) ; a value
                       (push (if keep-spaces meat (chunk meat 0 nil)) cont)))
                :finally
                (when pos  (file-position stream pos))
                (when cont (push (clean cont) asso))
                (return (values (nreverse asso) line))))
        ;; cleanup:
        #+clisp (setf custom:*misc-encoding* saved)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading the source files


(defmacro define-package (name &rest declarations)
  "
DO:         Declares a package.
            This includes loading the packages depended on,
            adding nicknames to the packages used under these nicknames,
            defining the package, and going into it (with IN-PACKAGE).
"
  (setq name (string name))
  (multiple-value-bind (dependencies renames defpack-args)
      (package::parse-package-declarations declarations)
    (let* ((used-packages
            (nconc
             (reduce (function nconc)
                     (mapcar (function cdr)
                             (remove :use defpack-args
                                     :test (complement (function eql))
                                     :key (function first))))
             (mapcar (function second) (remove :import-from defpack-args
                                               :test (complement (function eql))
                                               :key (function first)))))
           (also-use-packages
            (set-difference
             (remove-if (function package::built-in-p) dependencies)
             used-packages :test (function string=))))
      `(progn
         ;; (in-package "COMMON-LISP-USER") ; this is useless here.
         ,@(when also-use-packages
                 `((declaim (declaration common-lisp-user::also-use-packages))
                   (declaim (common-lisp-user::also-use-packages
                             ,@also-use-packages))))
         ,@(when renames
                 `((eval-when (:compile-toplevel :load-toplevel :execute)
                      ,@(mapcar
                         (lambda (rename) `(package:add-nickname
                                            ,(car rename) ,(cdr rename)))
                         renames))))
         (defpackage ,name ,@defpack-args)
         (in-package ,name)))))


;; in-package common-lisp-user
;; declaim also-use-packages
;; eval-when add-nickname ...
;; defpackage ...
;; in-package x

;; define-package
;; added nicknames
;; defpackage
;; require
;; provide

(defun read-source-code (stream)
  "
RETURN: An a-list of interesting forms frorm the source code.
BUGS:   This should be rewritten using COM.INFORMATIMAGO.COMMON-LISP.SOURCE
"
  (let ((*readtable*      (copy-readtable nil))
        (*package*        (find-package "COMMON-LISP-USER"))
        (defined-packages '())
        (nicknames        '())
        (provides         '())
        (requires         '())
        (used-packages    '())
        (eof  (gensym "EOF")))
    ;; We keep evaluating the readtime expressions, assuming
    ;; none has any deleterious side effect, and none is used
    ;; to variate what we depend on from source files.
    #-(and)
    (set-dispatch-macro-character #\# #\. (lambda (stream subchar arg)
                                            (declare (ignore subchar arg))
                                            (read-sexp-from-file stream eof)))
    (labels ((process-sexp (sexp)
               ;; (let ((*print-readably* t)) (print sexp *trace-output*))
               (case (first sexp)
                 ((defpackage)
                  (ignore-errors (eval sexp))
                  (push sexp defined-packages))
                 ((in-package)
                  (setf *package* (or (ignore-errors
                                        (find-package (second sexp)))
                                      *package*))
                  (push sexp used-packages))
                 ((use-package import shadowing-import)
                  (push sexp used-packages))
                 ((declaim)
                  (dolist (decl (cdr sexp))
                    (when (and (consp decl)
                               (eql 'common-lisp-user::also-use-packages
                                    (car decl)))
                      (push decl used-packages))))
                 ((cl-user::requires ;; norvig code uses it.
                   require)                (push sexp requires))
                 ((provide)                (push sexp provides))
                 ((progn)                  (dolist (item (cdr sexp))
                                             (process-sexp item)))
                 ((eval-when)              (dolist (item (cddr sexp))
                                             (process-sexp item)))
                 ((package:add-nickname)   (push (cdr sexp) nicknames))
                 ((package:define-package)
                  (process-sexp
                   (macroexpand-1 (cons 'define-package (cdr sexp))))))))
      (loop
         :for sexp = (read-sexp-from-file stream eof)
         :until (eql sexp eof)
         :do (when  (consp sexp) (process-sexp sexp))
         :finally (return
                    (list
                     (cons :packages-defined (nreverse defined-packages))
                     (cons :packages-used    (nreverse used-packages))
                     (cons :added-nicknames  (nreverse nicknames))
                     (cons :provides         (nreverse provides))
                     (cons :requires         (nreverse requires))))))))


(defun ensure-source-file-pathname (source-file &key (source-type "lisp"))
  (if (ignore-errors (probe-file source-file))
      source-file
      (make-pathname :type source-type :defaults source-file)))


(defun scan-source-file (source-filepath &key (external-format nil)
                         (load nil) (verbose nil) (keep-spaces nil))
  "
DO:               Makes a SOURCE-FILE instance and fills it with data got from 
                  the source file at SOURCE-FILEPATH.
EXTERNAL-FORMAT:  NIL => try to guess the encoding (use emacs local variables).
LOAD:             Whether the file should be loaded before scanning it.
                  (needed until COM.INFORMATIMAGO.COMMON-LISP.SOURCE is used).
COMMENT-START:    A regexp matching the line comment start for the header lines.
COMMENT-END:      A regexp matching the line comment end for the header lines.
KEEP-SPACES:      
NOTE:             COMMENT-START and COMMENT-END are regexp only on clisp until
                  COM.INFORMATIMAGO.COMMON-LISP.REGEXP can be used.

RETURN:           A SOURCE-FILE instance filled with the data from the source 
                  file.
"
  (let* ((emacs-variables (file-emacs-variables source-filepath))
         (external-format
          (or external-format
              (let ((coding (header-slot emacs-variables :coding)))
                (when coding
                  (emacs-encoding-to-lisp-external-format coding)))
              :default)))
    (when load
      (load source-filepath :external-format external-format :verbose verbose))
    (with-open-file (src source-filepath
                         :direction :input
                         :external-format external-format
                         :if-does-not-exist :error)
      (let ((header (read-source-header
                     src
                     :comment-start #+clisp "^[;]\\+" #-clisp ";;;;"
                     :comment-end   nil
                     :keep-spaces   keep-spaces))
            (forms (read-source-code src)))
        (make-instance 'source-file
          :pathname          (truename source-filepath)
          :external-format   external-format
          :emacs-variables   emacs-variables
          :header            header
          :packages-defined
          (flet ((select (key alist)
                         (mapcan (lambda (ass)
                                   (when (eq key (car ass))
                                     (list (copy-list (cdr ass))))) alist))
                 (selectn (key alist)
                   (mapcan (lambda (ass)
                             (when (eq key (car ass))
                               (copy-list (cdr ass)))) alist)))
            (mapcar
             (lambda (defpack)
               (let ((options (cddr defpack)))
                 (make-instance 'source-package
                   :name                  (second  defpack)
                   :nicknames             (selectn :nicknames    options)
                   :documentation         (second
                                           (assoc :documentation options))
                   :use                   (selectn :use          options)
                   :shadow                (selectn :shadow       options)
                   :shadowing-import-from (select  :shadowing-import-from
                                                   options)
                   :import-from           (select  :import-from  options)
                   :export                (selectn :export       options)
                   :intern                (selectn :intern       options))))
             (cdr (assoc :packages-defined forms))))
          :packages-used     (cdr (assoc :packages-used   forms))
          :added-nicknames   (cdr (assoc :added-nicknames forms))
          :requires          (cdr (assoc :requires        forms))
          :provides          (cdr (assoc :provides        forms)))))))


(defparameter *source-file-db* (make-hash-table :test (function equal)))

(defun get-source-file (filepath)
  ;; In clisp with ext:fasthash-equal, hashing pathname doesn't work.
  (let ((filepath (ensure-source-file-pathname filepath)))
    (or  (gethash (namestring (truename filepath)) *source-file-db*)
         (let ((source-file
                (block :source-file
                  (handler-bind
                      ((error (lambda (err) (princ err *error-output*)
                                 (invoke-debugger err)
                                 (return-from :source-file nil))))
                    (scan-source-file filepath
                                      :load (string/= "el" (pathname-type filepath))
                                      :verbose *load-verbose*
                                      :keep-spaces t)))))
           (when source-file
             (setf (gethash (namestring (truename filepath))
                            *source-file-db*) source-file
                   (gethash (namestring (truename (source-file-pathname source-file)))
                            *source-file-db*) source-file))))))



(defun extract-source-from-require-sexp (sexp)
  "
PRE:    sexp is of the form: (REQUIRE module-name &OPTIONAL pathname-list)
        module-name can be 'toto or (quote toto).
        Each path name can be either a namestring, a physical path name or
        a logical path name.
RETURN: A new list containing the pathname-list if present, or a list 
                              containing the symbol-name  of the module-name.
"
  (let ((symb  (nth 1 sexp))
        (files (cddr sexp)) )
    (if (null files)
        (list (symbol-name (eval symb)))
        (copy-seq files))))



#||
(defun get-requires (source-file &key (verbose nil))
  "
RETURN:  A list of REQUIRE sexps found on the top-level of the SOURCE-FILE.
"
  (with-open-file (in source-file :direction :input :if-does-not-exist :error)
    (when verbose
      (format *trace-output* "# get-requires ~A~%" source-file))
    (let ((*readtable* (copy-readtable nil)))
      (set-dispatch-macro-character
       #\# #\. (lambda (&rest args) args))
      (loop with eof = (gensym "EOF")
         for sexp = (read-sexp-from-file in eof)
         until (eq sexp eof)
         when (and (consp sexp)
                   (or (eq (car sexp) 'require)
                       (eq (car sexp) 'requires))) ;; norvig code uses it.
         collect sexp into result
         finally (return result)))))
||#



(defun get-requires (source-filepath)
  "
RETURN:  A list of REQUIRE sexps found on the top-level of the SOURCE-FILE.
"
  (let ((source-file (get-source-file source-filepath)))
    (when source-file
      (source-file-requires source-file))))


(defun get-package (source-filepath)
  "
RETURN:  The first DEFINE-PACKAGE sexp found on the top-level of the
         SOURCE-FILE. If none found, then the first DEFPACKAGE sexp.
         Or else, the first provide sexp, or else the file-name.
"
  (let ((source-file (get-source-file source-filepath)))
    (if source-file
      (or (first (source-file-packages-defined source-file))
          (source-file-provides source-file)
          `(file-name ,(pathname-name (source-file-pathname source-file))))
      `(file-name ,(pathname-name (source-file-pathname source-file))))))



#||
(defun get-package (source-file &key (external-format :default)
                    (source-type "lisp"))
  "
RETURN:  The first DEFINE-PACKAGE sexp found on the top-level of the
         SOURCE-FILE. If none found, then the first DEFPACKAGE sexp.
         Or else, the first provide sexp, or else the file-name.
"
(unless (probe-file source-file)
  (setq source-file (merge-pathnames
                     (make-pathname :type source-type) source-file nil)))
  (with-open-file (in source-file :direction :input
                      :external-format external-format
                      :if-does-not-exist :error)
    (let ((*readtable* (copy-readtable nil)))
      (set-dispatch-macro-character #\# #\. (lambda (&rest args) args))
      (loop
         :with eof = (gensym "EOF")
         :with define-package-sexp = nil
         :with defpackage-sexp = nil
         :with provide-sexp = nil
         :for sexp = (read-sexp-from-file in eof)
         :until (or (eql sexp eof) define-package-sexp)
         :do (cond
               ((or (atom sexp) (not (symbolp (car sexp)))))
               ((and (not define-package-sexp)
                     (string= (car sexp) 'define-package))
                (setf define-package-sexp sexp))
               ((and (not defpackage-sexp) 
                     (string= (car sexp) 'defpackage))
                (setf defpackage-sexp sexp))
               ((and (not provide-sexp)
                     (string= (car sexp) 'provide))
                (setf provide-sexp `(provide ,(second (second sexp))))))
         :finally (return (or define-package-sexp defpackage-sexp provide-sexp
                              `(file-name ,(pathname-name (pathname in)))))))))
||#









;; object-file can be either .fas or .elc
;;
;; In  both cases, it  may have  as source,  either a  common-lisp source
;; (.lisp, .lsp  or .cl), or a elisp  source (.el). (If a  .fas, we seach
;; first for a .lisp, and if a .elc, we search first for a .el).
;; 
;;
;; For  required files,  we search  whatever source  file (first  of same
;; class as  the source found for  object-file), and return  in anycase a
;; corresponding   object  file   of  the   same  class   (extension)  as
;; object-file.
;;
;; A .fas  cannot require and load a  .elc, it requires and  loads only a
;; .fas, while  a .elc cannot  require and load  a .fas, it  requires and
;; loads only a .elc.


(defun source-file-for-object (object-file)
  "
RETURN: The source file for OBJECT-FILE (finding the right extension);
        the extension of the object-file;
        and the list of extensions couples.
"
  (unless (pathname-type object-file)
    (error
     "make-depends: expected an object file name with an extension, not ~S.~%"
     object-file))
  (let* ((extension    (pathname-type object-file))
         (extensions
          (if (member  extension (object-extensions extensions-clisp)
                       :test (function string=))
              (append extensions-clisp extensions-emacs)
              (append extensions-emacs extensions-clisp)))
         (source-file
          ;; given the object-file  extension, find the source file extension
          ;; for which a source file exists.
          (do* ((sext-oext extensions (cdr  sext-oext))
                (sext  (caar sext-oext) (caar sext-oext))
                ;;(OEXT  (CDAR SEXT-OEXT) (CDAR SEXT-OEXT))
                (sname nil)
                (result nil))
               ((or (null sext-oext)
                    (progn
                      (setf sname (make-pathname :type sext
                                                 :defaults object-file))
                      (setf result (ignore-errors (probe-file sname)))))
                (if result sname nil)))))
    (values source-file extension extensions)))


(defun find-file-path (fname dir-paths)
  "
RETURN:  NIL or the path of FNAME found in one of DIR-PATHS.
"
  (do* ((paths dir-paths   (cdr paths))
        (dpath (car paths) (car paths))
        (fpath ) )
       ( (or (null dpath) 
             (probe-file 
              (setq fpath (if (string= dpath ".") 
                              fname
                              (sconc dpath "/" fname)))))
        (if dpath  fpath nil) )))


(defun find-file-with-extension (path extensions)
  (assert path (path) "FIND-FILE-WITH-EXTENSION needs a path for PATH")
  (loop
     for ext in extensions
     for path-ext = (make-pathname :type ext :defaults path)
     do (progn
          (pdebug "~&# -------------------------------------~%")
          (pdebug "~&#    FFWE: EXTENSION   = ~S~%" ext)
          (pdebug "~&#    FFWE: PATH-EXT    = ~S~%" path-ext)
          (pdebug "~&#    FFWE: PROBE-FILE  = ~S~%"
                  (handler-case (probe-file path-ext) (error () nil))))
     (when (handler-case (probe-file path-ext) (error () nil))
       (return-from  find-file-with-extension path-ext))
     finally (return nil)))


(defun find-file-in-directory (file directories extensions)
  (assert file (file) "FIND-FILE-IN-DIRECTORY needs a path for FILE")
  (loop
     for directory in directories
     for dir-file = (make-pathname :directory (parse-namestring directory)
                                   :defaults file)
     for found =  (find-file-with-extension dir-file extensions)
     until found
     finally (return found)))

(defun package-member-p (package packlist)
  (let* ((packname (etypecase package
                     (string package)
                     (symbol (string package))
                     (package (package-name package))))
         (dotted (position (character ".") packname))
         (packlist (mapcar (lambda (package)
                             (etypecase package
                               (string package)
                               (symbol (string package))
                               (package (package-name package))))
                           packlist)))
    (or (member packname packlist :test (function string=))
        (when dotted
          (find-if (lambda (pack)
                     (and (prefixp pack packname)
                          (< (length pack) (length packname))
                          (char= (character ".") (aref packname (length pack)))))
                   packlist)))))
                      

(defun get-dependencies (object-file packages load-paths &key (verbose nil))
  "
PRE:            OBJECT-FILE is foo.fas or foo.elc, etc.
RETURN:         A list of dependency for this OBJECT-FILE,
                including the source-file and all the object files
                of required files.
OBJECT-FILE:    The file of which the dependencies are computed.
PACKAGES:       A list of preloaded package names.
LOAD-PATHS:     A list of directory path names where to find the files when
                not found thru the logical pathname translations.
                The presence in LOAD-PATHS of a logical pathname warrants 
                the presence in HOST-LPT of an entry mapping it to a physical
                path.
VERBOSE:        Prints information on *TRACE-OUTPUT*.
"
  (pdebug "~&# -------------------------------------~%")
  (pdebug "~&# get-dependencies ~S~%" object-file)
  (when verbose
    (format *trace-output* "~&# get-dependencies ~S~%" object-file))
  (multiple-value-bind (source-filepath obj-ext extensions)
      (source-file-for-object object-file)
    (when source-filepath
      (cons
       source-filepath
       (mapcan 
        (lambda (pack-name)
          (pdebug "~&#    processing ~S~%" pack-name)
          (when verbose
            (format *trace-output* "~&#    processing ~S~%" pack-name))
          ;; "COM.INFORMATIMAGO.COMMON-LISP.TOOLS.MAKE-DEPENDS.MAKE-DEPENDS"
          ;; --> PACKAGE::PACKAGE-PATHNAME
          ;; "DICTIONARY"
          ;; "PJB-STRING"
          ;; --> LOAD-PATHS
          (unless (member pack-name package::*built-in-packages*
                          :test (function string=))
            (let* ((src-ext   (source-extensions extensions))
                   (pack-path (package::package-pathname pack-name))
                   (path
                    (or (find-file-with-extension   pack-path  src-ext)
                        (find-file-in-directory pack-name load-paths src-ext))))
              (pdebug "~&#    source extensions are ~S~%" src-ext)
              (pdebug "~&#    path of package   is ~S~%"
                      (package::package-pathname pack-name))
              (pdebug "~&#    path              is ~S~%" path)
              (if path
                  (progn
                    (setf path
                          (let ((fntlp (file-namestring
                                        (translate-logical-pathname path))))
                            (if (and
                                 (string=
                                  (directory-namestring
                                   (translate-logical-pathname path))
                                  (directory-namestring
                                   (translate-logical-pathname pack-path)))
                                 (ignore-errors (probe-file fntlp))) fntlp path)))
                    (pdebug "~&#    short path        is ~S~%" path))
                  (warn "Can't find a source file for package ~A" pack-name))
              (when path
                (setf path (make-pathname :type obj-ext :defaults path)))
              (pdebug "~&#    result is ~S~%" path)
              (list path))))
        (remove-if
         (lambda (pack)
           (pdebug "~&# package dependencies: ~S~%" pack)
           (package-member-p pack packages))
         (dependencies (get-source-file source-filepath))))))))



(defun get-closed-dependencies (object-file load-paths &key (verbose nil))
  "
PRE:     OBJECT-FILE is foo.fas or foo.elc, etc.
RETURN:  A list of object files recursively required by OBJECT-FILE.
"
  (multiple-value-bind 
        (source-file extension extensions) (source-file-for-object object-file)
    (when source-file
      (cons object-file 
            (mapcan ;; for each required file
             (lambda (item)
               (mapcar ;; find the object file path corresponding to an
                ;;        existing source file path.
                (lambda (sext-oext)
                  (let* ((sname (sconc item (car sext-oext)))
                         (spath (find-file-path sname load-paths)))
                    (if spath
                        (get-closed-dependencies 
                         (if (string= "." (directory-namestring spath))
                             (sconc item extension)
                             (sconc (directory-namestring spath)
                                    "/" item extension))
                         load-paths
                         :verbose verbose)
                        nil)))
                extensions))
             (get-requires source-file))))))


(defun get-depends (object-files packages load-paths)
  "
RETURN:     A list of (cons object-file dependency-list).
NOTE:       GET-DEPENDS uses the logical pathname translations in place
            when called.
"
  (mapcar (lambda (object)
            (cons object (get-dependencies object packages load-paths)))
          object-files))


(defun make-depends (object-files packages translations load-paths
                     &key (idf nil) (verbose nil))
  "
DO:             Finds the dependencies of the object files.
PACKAGES:       A list of names of the packages preloaded.
NOTE:           Since the list of preloaded packages depends on the lisp
                compiler, it should be given along with object files
                compiled by this compiler.  However, if it is known that
                only a common list of predefined package is used in the
                source packages, it can be given.
TRANSLATIONS:   A list of (CONS HOST TRANSLATIONS) that will be used to
                set the logical pathname translations. These translations
                are used first
NOTE:           We set the logical pathname translations only in here to avoid
                problems loading this program.
LOAD-PATHS:     A list of directory path names where to find the files when
                not found thru the logical pathname translations.
                The presence in LOAD-PATHS of a logical pathname warrants 
                the presence in HOST-LPT of an entry mapping it to a physical
                path.
IDF:            If NIL, write the dependencies on the standard output,
                else, write the dependencies of each object file into its
                own .d file.
"
  (mapc (lambda (args)
          (setf (logical-pathname-translations (car args)) (cdr args)))
        translations)
  (setq packages (mapcar (function string) packages))
  (dolist (object object-files)
    (when verbose
      (format *trace-output* "# Processing ~A~%" object))
    (let ((line (format nil "~A :: ~A" object
                        (unsplit-string
                         (mapcar
                          (lambda (item)
                            (if item
                                (string
                                 (namestring
                                  (translate-logical-pathname item)))
                                ""))
                          (get-dependencies object packages load-paths))
                         " "))))
      (if idf
          (with-open-file
              (out (merge-pathnames
                    (make-pathname
                     :name (concatenate 'string (pathname-name object)
                                        "." (pathname-type object))
                     :type "d")
                    object)
                   :direction :output
                   :if-exists :overwrite
                   :if-does-not-exist :create)
            (format out "~A~%" line))
          (format t "~A~%" line)))))



  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERATE-SUMMARY
;;;

(defun generate-summary (sources &key (summary-path #p"SUMMARY.HTML")
                         (character-set "US-ASCII")
                         (source-type "LISP")
                         (verbose nil) (repository-url nil))
  "Generates a HTML summary of the sources"
  (assert (functionp repository-url) (repository-url)
          "REPOSITORY-URL must be a (function (pathname) string)")
  (let ((cs (etypecase character-set
              (character-set character-set)
              (string (find-character-set character-set))
              (symbol (find-character-set (string character-set))))))
    (unless cs (error "Cannot find the character set ~A" character-set))
    (with-open-file (html summary-path
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :external-format (character-set-to-lisp-encoding cs))
      (html:with-html-output (html :encoding cs)
        (html:doctype :transitional
          (html:comment "-*- coding:~A -*-"
                        (character-set-to-emacs-encoding cs))
          (html:html -
            (html:head -
              (html:title - (html:pcdata "Summary"))
              (html:meta
                  (list :http-equiv "Content-Type"
                        :content (format nil "text/html;charset=~A"
                                         (character-set-to-mime-encoding cs)))))
            (html:body -
              (dolist (source sources)
                (let* ((path             (make-pathname
                                          :name (string-downcase source)
                                          :type source-type
                                          :case :local))
                       (source-file     (get-source-file path))
                       (header          (source-file-header source-file))
                       (package (or (header-slot header :package)
                                    (first (source-file-packages-defined source-file))
                                    ;; for files without a package (eg emacs files)
                                    ;; we make a pseudo-package named as the file.
                                    (make-instance 'source-package
                                        :name (pathname-name path)
                                        :nicknames '()
                                        :documentation nil
                                        :use '()
                                        :shadow '()
                                        :shadowing-import-from '()
                                        :import-from '()
                                        :export '()
                                        :intern '()))))
                  (when verbose
                    (format *trace-output* ";; Processing ~S~%" source)
                    (format *trace-output* ";;   PATH    = ~S~%" path)
                    ;;(format *trace-output* ";;   HEADER  = ~S~%" header)
                    (format *trace-output* ";;   PACKAGE = ~S~%"
                            (source-package-name package))
                    (finish-output *trace-output*))
                  (unless (header-slot header :noweb)
                    (html:li -
                      (html:tt -
                        (html:b -
                          (html:a
                              (:href (funcall repository-url
                                              (package:package-pathname
                                               (source-package-name package))))
                            (html:pcdata "~A" (source-package-name package)))))
                      (html:pre -
                        (dolist (line (header-description header))
                          (html:cdata "~A~%" line))))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERATE-ASD
;;;

(unless (find-package :asdf) (defpackage :asdf (:use :cl) (:export "DEFSYSTEM")))

(defun make-components (paths &key (predefined-packages '("COMMON-LISP"))
                        (component-class :file)
                        (implicit-dependencies '())
                        (load-paths (list (make-pathname
                                           :directory '(:relative)))))
  (mapcar
   (lambda (depend)
     (let* ((depend (mapcar (lambda (path) (pathname-name path)) depend))
            (target (first depend))
            (depends  (delete (first depend)
                              (append implicit-dependencies (rest depend))
                              :test (function string=))))
       (list* component-class target (when depends (list :depends-on depends)))))
   (get-depends paths predefined-packages load-paths)))

(defun gen-defsystem-form (name paths &key  description (version "0.0.0") 
                      author licence license
                      (component-class :file)
                      (predefined-packages '("COMMON-LISP"))
                      (implicit-dependencies '())
                      (depends-on '())
                      (load-paths (list (make-pathname
                                         :directory '(:relative)))))
  "
DO:             Generate an ASD file for ASDF.
NAME:           Name of the generated ASDF system.
PATHS:          List of pathnames to the source files of this ASDF system.
DESCRIPTION:    A description string for the ASDF system.
VERSION:        A version string for the ASDF system.
AUTHOR:         An author string for the ASDF system.
LICENCE:        A licence string for the ASDF system.
LICENSE:        A licence string for the ASDF system.
PREDEFINED-PACKAGES:   A list of packages that are removed from the dependencies.
IMPLICIT-DEPENDENCIES: A list of dependencies added to all targets.
LOAD:-PATHS     A list of directory paths where the sources are searched in.
"
  (flet ((enumerate (list) (format nil "~{~A, ~}~:[none~;~1@*~{~A~^ and ~A~}~]"
                                   (butlast list 2) (last  list 2))))
    (let* ((headers (mapcar (lambda (path) (list* :path path
                                             (with-open-file (stream path)
                                               (read-source-header stream))))
                            paths))
           (authors (or author
                        (enumerate (delete-duplicates 
                                    (apply (function append)
                                           (mapcar (function header-authors)
                                                   headers))
                                    :test (function string-equal)))))
           (licence (or licence license
                        (enumerate (delete-duplicates
                                    (mapcar (function header-licence) headers)
                                    :test (function string-equal)))))
           (description
            (unsplit-string 
             (or (ensure-list description)
                 (flatten
                  (mapcar
                   (lambda (header)
                     (list (format nil "~2%PACKAGE: ~A~2%"
                                   (second
                                    (get-package (header-slot header :path))))
                           (mapcar (lambda (line) (format nil "~A~%" line))
                                   (header-description header))
                           (format nil "~%")))
                   headers)))))
           (components (make-components
                        paths
                        :component-class component-class
                        :predefined-packages (append depends-on
                                                     predefined-packages)
                        :implicit-dependencies implicit-dependencies
                        :load-paths load-paths)))
      `(asdf:defsystem ,name
           :description ,description
           :version     ,version
           :author      ,authors
           :licence     ,licence
           :depends-on  ,depends-on
           :components  ,components) )))


(defun generate-asd (system-name sources source-type
                     &key description (version "0.0.0")
                     author licence license
                     (predefined-packages '("COMMON-LISP"))
                     (implicit-dependencies '())
                     (depends-on '())
                     (load-paths (list (make-pathname :directory '(:relative))))
                     (vanillap nil))
  "
VANILLAP:  if true, then generate a simple, vanilla system.
           Otherwise, decorate it with PJB output-files.
"
  (let ((*package* (find-package :com.informatimago.common-lisp.tools.make-depends.make-depends)))
    (with-open-file (out (make-pathname :directory '(:relative)
                                        :name "system"
                                        ;;(string-downcase system-name)
                                        :type "asd" :version nil)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      #+(or)(push (truename (merge-pathnames
                             (make-pathname :directory '(:relative)
                                            :name nil :type nil :version nil)
                             out)) asdf::*central-registry*)
      (format out ";; -*- mode:lisp -*-~%")
      (mapc
       (lambda (sexp) (print sexp out) (terpri out))
       ;; Out to the asd file:
       (append
        (unless vanillap
          `((defpackage "COM.INFORMATIMAGO.ASDF" (:use "COMMON-LISP"))
            (in-package "COM.INFORMATIMAGO.ASDF")
            ;; ASDF imposes the file type classes to be
            ;; in the same package as the defsystem.
            (unless (handler-case (find-class 'pjb-cl-source-file) (t () nil))
              (defclass pjb-cl-source-file (asdf::cl-source-file) ())
              (flet ((output-files (c)
                       (flet ((implementation-id ()
                                (flet ((first-word (text)
                                         (let ((pos (position (character " ")
                                                              text)))
                                           (remove (character ".")
                                                   (if pos
                                                       (subseq text 0 pos)
                                                       text)))))
                                  (format
                                   nil "~A-~A-~A"
                                   (cond 
                                     ((string-equal
                                       "International Allegro CL Enterprise Edition"
                                       (lisp-implementation-type))
                                      "ACL")
                                     (t (first-word (lisp-implementation-type))))
                                   (first-word (lisp-implementation-version))
                                   (first-word (machine-type))))))
                         (let* ((object (compile-file-pathname
                                         (asdf::component-pathname c)))
                                (path (merge-pathnames
                                       (make-pathname
                                        :directory
                                        (list :relative
                                              (format nil "OBJ-~:@(~A~)"
                                                      (implementation-id)))
                                        :name (pathname-name object)
                                        :type (pathname-type object))
                                       object)))
                           (ensure-directories-exist path)
                           (list path)))))
                (defmethod asdf::output-files ((operation asdf::compile-op)
                                               (c pjb-cl-source-file))
                  (output-files c))
                (defmethod asdf::output-files ((operation asdf::load-op)
                                               (c pjb-cl-source-file))
                  (output-files c))))))
        
        `(,(gen-defsystem-form
            system-name
            (mapcar
             (lambda (source) (make-pathname :name (string-downcase (string source))
                                             :type source-type))
             sources)
            :description (or description
                             (format nil
                                     "This ASDF system gathers all the ~A packages."
                                     (string-upcase system-name)))
            :version version
            :author author
            :licence (or licence license)
            :component-class (if vanillap :cl-source-file :pjb-cl-source-file)
            :predefined-packages predefined-packages
            :implicit-dependencies implicit-dependencies
            :depends-on depends-on
            :load-paths load-paths)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE END

#+(or)
(progn
  (get-depends paths '("COMMON-LISP")
               (list (make-pathname :directory '(:relative))))
  (predefined-packages '("COMMON-LISP"))
  (implicit-dependencies '())
  (load-paths (list (make-pathname :directory '(:relative))))
  (make-asd :com.informatimago.common-lisp paths
            :implicit-dependencies '("package"))

  (get-dependencies (car paths) nil   (list (com.informatimago.pjb:pwd)))
  (get-depends paths nil (list (com.informatimago.pjb:pwd)))
  (mapcar (lambda  (path)  (with-open-file (in path :direction :input :if-does-not-exist :error) (cons path (read-source-header  in))))  (directory "*.lisp"))

  (mapcar (lambda  (path)  (with-open-file (in path :direction :input :if-does-not-exist :error) (cons path (header-licence (read-source-header  in)))))  (directory "*.lisp"))

  (mapcar (lambda  (path)  (with-open-file (in path :direction :input :if-does-not-exist :error) (cons path (header-description (read-source-header  in)))))  (directory "*.lisp"))

  (package:load-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (use-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST")
  (package:load-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (use-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (make-asd "COM.INFORMATIMAGO.COMMON-LISP" (directory "*.ilsp"))
  )

;;;; THE END ;;;;

