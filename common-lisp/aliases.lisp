;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               aliases.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a function to read sendmail aliases files.
;;;;    
;;;;    (setf db (load-aliases [file]))
;;;;    (save-aliases db [file])
;;;;    (db-records db) --> list-of-records
;;;;
;;;;    (make-alias name address-list) --> record
;;;;    (make-comment text) --> record
;;;;    (find-record-if db predicate) --> record
;;;;    (alias-record   db name) --> record
;;;;    (comment-record-containing db substring) --> record
;;;;    (insert-record  db record [:before record] [:after record])
;;;;    (delete-record  db record)
;;;;
;;;;    (list-all-aliases db) --> ("name1" "name2" ...)
;;;;    (alias-addresses db name) --> list
;;;;    (setf (alias-addresses db name) list)
;;;;    (insert-alias db name type value [:before record] [:after record])
;;;;    (delete-alias db name)
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-01-14 <PJB> Added alias db functions.
;;;;    2005-09-01 <PJB> Made use of iso6429.
;;;;    2005-05-19 <PJB> Corrected handling of :include: in parse-address 
;;;;                     adding a follow set attribute.
;;;;    2003-10-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.ECMA048"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ALIASES"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.LIST"
        "COMMON-LISP")
  (:EXPORT "READ-DOT-FORWARD" "READ-ALIASES")
  (:DOCUMENTATION
   "This package exports a function to read unix aliases files.

    Copyright Pascal J. Bourguignon 2003 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ALIASES")



(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-verbose* nil)) (ecma048:generate-all-functions-in-ecma048)))

(defvar +HT+ (or (ignore-errors (read-from-string "#\\TAB"))
                 (CODE-CHAR ecma048:ht)))
;; How can we define a HT character portably?
;; Even (code-char 9) may not work...
;; If the TAB code doesn't exist on the host,
;; the input files wouldn't contain any, and we shouldn't evey try to test it.

(defvar +SPHTCRLF+
  (FORMAT NIL " ~C~C~C"
          (CODE-CHAR ecma048:ht) (CODE-CHAR ecma048:cr) (CODE-CHAR ecma048:lf))
  "A string containing space, tabulation, carriage return and line feed.")


(defvar +SPHT+
  (FORMAT NIL " ~C" (CODE-CHAR ecma048:ht))
  "A string containing space and tabulation.")


(defvar +CRLF+
  (FORMAT NIL "~C~C" (CODE-CHAR ecma048:cr) (CODE-CHAR ecma048:lf))
  "A string containing a carriage return and a line feed.")


(defvar +CR+
  (FORMAT NIL "~C" (CODE-CHAR ecma048:cr))
  "A string containing a carriage return.")


;; alias   ::=  address ':' address-list .
;; address ::=  '"' {not-double-quote} '"' | { letter-or-digit-or-underline } .
;; address-list ::= address ',' address-list | address .
;;
;; address                ( :address  address )
;; \address               ( :quote    address )
;; /file/name             ( :file     /file/name )
;; |command               ( :command  command )
;; :include:/file/name    ( :include  /file/name )


(DEFUN PARSE-TOKEN (LINE POS TOKEN)
  (AND (<= (+ POS (LENGTH TOKEN)) (LENGTH LINE))
       (STRING= LINE TOKEN :START1 POS :END1 (+ POS (LENGTH TOKEN)))))


(DEFUN SKIP-SPACES (LINE POS)
  (WHILE (PARSE-TOKEN LINE POS " ")
    (INCF POS))
  POS)


(DEFUN PARSE-ADDRESS (LINE POS follow)
  "
start ::= address | file | command | include .
address ::= rfc822-address .
file ::= '/' path .
command ::= '|' command .
include ::= ':include:' '/' path.
"
  (SETQ POS (SKIP-SPACES LINE POS))
  (COND
    ((<= (LENGTH LINE) POS) (ERROR "Expected an address, got end of line."))
    ((CHAR= (CHARACTER "\"") (CHAR LINE POS))
     (READ-FROM-STRING LINE NIL NIL :START POS))
    (T  (DO ((I POS (1+ I)))
            ((OR (<= (LENGTH LINE) I)
                 (POSITION (CHAR LINE I) follow))
             (VALUES (SUBSEQ LINE POS I) I))))))


(DEFUN PARSE-ADDRESS-LIST (LINE POS)
  (MULTIPLE-VALUE-BIND (ADDRESS NPOS) (PARSE-ADDRESS LINE POS ", #")
    (LET ((ADDRESS-LIST (LIST ADDRESS)))
      (SETQ POS (SKIP-SPACES LINE NPOS))
      (loop :while (< pos (length line)) :do
         
         (MULTIPLE-VALUE-BIND (ADDRESS NPOS)
             (PARSE-ADDRESS LINE (+ (if (PARSE-TOKEN LINE POS ",")
                                        1 0) POS) ", #")
           (PUSH ADDRESS ADDRESS-LIST)
           (SETQ POS (SKIP-SPACES LINE NPOS))))
      (VALUES (nreverse ADDRESS-LIST) POS))))

;; address-list ::= address | address-list [ ',' ] address .
;; address      ::= '"' ( but-quote-or-antislash | '\' anychar ) * '"'
;;                  | but-quote-space-or-comma .


(DEFUN PARSE-EOLN (LINE POS)
  (SETQ POS (SKIP-SPACES LINE POS))
  (<= (LENGTH LINE) POS))


(DEFUN ENCAPSULATE (ADDR)
  (COND
    ((CHAR= (CHAR ADDR 0) (CHARACTER "\\"))
     (CONS :QUOTE   (STRING-DOWNCASE (SUBSEQ ADDR 1))))
    ((CHAR= (CHAR ADDR 0) (CHARACTER "|"))
     (CONS :COMMAND (SUBSEQ ADDR 1)))
    ((CHAR= (CHAR ADDR 0) (CHARACTER "/"))
     (CONS :FILE    ADDR))
    ((PREFIXP ":include:" ADDR)
     (CONS :INCLUDE (SUBSEQ ADDR 9)))
    (T
     (CONS :ADDRESS (STRING-DOWNCASE ADDR)))))

     
(DEFUN PARSE-DOT-FORWARD-ALIAS (LINE)
  (MULTIPLE-VALUE-BIND (ADDRESS-LIST POS) (PARSE-ADDRESS-LIST LINE 0)
    ;; TODO: Check that we reach EOLN!!!
    (PARSE-EOLN LINE POS)
    (MAPCAR (FUNCTION ENCAPSULATE) ADDRESS-LIST)))


(DEFUN PARSE-ALIAS (LINE)
  (if (or (zerop (length line)) (char= #\# (aref line 0)))
      ;; A comment
      (list :comment line)
      ;; An alias
      (MULTIPLE-VALUE-BIND (ADDRESS POS) (PARSE-ADDRESS LINE 0 ": #")
        (SETQ POS (SKIP-SPACES LINE POS))
        (IF (PARSE-TOKEN LINE POS ":")
            (MULTIPLE-VALUE-BIND (ADDRESS-LIST POS)
                (PARSE-ADDRESS-LIST LINE (1+ POS))
              ;; TODO: Check that we reach EOLN!!!
              (PARSE-EOLN LINE POS)
              (CONS (STRING-DOWNCASE ADDRESS)
                    (MAPCAR (FUNCTION ENCAPSULATE) ADDRESS-LIST)))
            (ERROR "Expected a ':'.")))))


(DEFUN JOIN-CONTINUATION-LINES (LINES)
  (NREVERSE
   (CDR
    (LET ((TAG (GENSYM)))
      (REDUCE (LAMBDA (&OPTIONAL A B)
                (LET (JOINED LINE)
                  (IF (AND (LISTP A) (EQ TAG (CAR A)))
                      (SETQ JOINED A LINE B)
                      (SETQ JOINED B LINE A))
                  (IF (AND (and (plusp (length line))
                                (CHAR= (CHARACTER " ") (CHAR LINE 0)))
                           (FIRST (CDR JOINED)))
                      ;; continuation-line
                      (SETF (FIRST (CDR JOINED))
                            (CONCATENATE 'STRING (FIRST (CDR JOINED)) LINE))
                      (SETF (CDR JOINED) (CONS LINE (CDR JOINED))))
                  JOINED))
              LINES
              :INITIAL-VALUE (CONS TAG NIL))))))


(DEFUN REMOVE-COMMENTS (LINES)
  (MAPCAN (LAMBDA (LINE)
            (SETQ LINE (STRING-RIGHT-TRIM " " LINE))
            (WHEN (AND (< 0 (LENGTH LINE))
                       (CHAR/= (CHARACTER "#") (CHAR LINE 0)))
              (LIST LINE)))
          LINES))


(DEFUN CLEAN-LINES (LINES)
  "
DO:    Clean CR/LF stuff and replace tabulations by spaces.
"
  (MAPCAN (LAMBDA (IN-LINE)
            (MAPCAR (LAMBDA (LINE)
                      (SUBSTITUTE (CHARACTER " ") (CODE-CHAR 9) LINE))
                    ;; This SPLIT-STRING returns NIL for an empty string.
                    (SPLIT-STRING (STRING-TRIM +CRLF+ IN-LINE) +CRLF+)))
          LINES))



(DEFUN READ-ALIASES (&OPTIONAL (ALIAS-FILE-PATH "/etc/aliases"))
  "
RETURN:  A list of ( alias address...).
         alias is a downcased string containing the alias name.
         address is a cons:
          ( :address . address )    ;; address  (downcased)
          ( :quote   . address )    ;; \address (downcased)
          ( :file    . /file/name ) ;; /file/name
          ( :command . command )    ;; |command
          ( :include . /file/name ) ;; :include:/file/name
"
  (MAPCAR (FUNCTION PARSE-ALIAS)
          (JOIN-CONTINUATION-LINES
           (REMOVE-COMMENTS
            (CLEAN-LINES
             (WITH-OPEN-FILE (IN ALIAS-FILE-PATH
                                 :DIRECTION :INPUT
                                 :IF-DOES-NOT-EXIST :ERROR)
               (STREAM-TO-STRING-LIST IN)))))))
  

(DEFUN READ-DOT-FORWARD (FORWARD-FILE-PATH)
  "
RETURN:  A list of ( address...).
         address is a cons:
          ( :address . address )    ;; address  (downcased)
          ( :quote   . address )    ;; \address (downcased)
          ( :file    . /file/name ) ;; /file/name
          ( :command . command )    ;; |command
          ( :include . /file/name ) ;; :include:/file/name
"
  (MAPCAR (FUNCTION PARSE-DOT-FORWARD-ALIAS)
          (JOIN-CONTINUATION-LINES
           (REMOVE-COMMENTS
            (CLEAN-LINES
             (WITH-OPEN-FILE (IN FORWARD-FILE-PATH
                                 :DIRECTION :INPUT
                                 :IF-DOES-NOT-EXIST nil)
               (when in
                 (STREAM-TO-STRING-LIST IN))))))))



(defparameter *normal-characters*
  ".-/0123456789@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz|")
(defun normal-character-p (ch) (position ch *normal-characters*))
(defun normal-string-p (string)
  (every (function normal-character-p) string))
(defun quote-string (string)
  (with-output-to-string (out)
    (princ #\" out)
    (loop
       :for ch :across string
       :do (when (or (char= #\\ ch) (char= #\" ch))
             (princ #\\ out))
       :do (princ ch out))
    (princ #\" out)))
(defun normal-or-quote-string (string)
  (if (normal-string-p string)
      string
      (quote-string string)))


(defun make-comment (text) (list :comment text))
(defun commentp (object)
  (and (listp object) (eq :comment (first object))))
(defun comment-text (comment) (second comment))


(defparameter *address-types* '(:address :quote :file :command :include))
(defun make-address (type value)
  (assert (and (member type *address-types*) (stringp value)))
  (cons type value))
(defun addressp (object)
  (and (consp object)
       (member (car object) *address-types*) (stringp (cdr object))))
(defun address-type  (address) (car address))
(defun address-value (address) (cdr address))

(defun address-string (address)
  (let ((value (address-value address)))
    (case (address-type address)
      ((:address :file) (normal-or-quote-string value))
      ((:quote)
       (if (normal-string-p value)
           (format nil "\\~A" value)
           (format nil "\"\\~A" (subseq (quote-string value) 1))))
      ((:command) (normal-or-quote-string (format nil "|~A" value)))
      ((:include)
       (if (normal-string-p value)
           (format nil ":include:~A" value)    
           (normal-or-quote-string (format nil ":include:~A" value)))))))


(defun make-alias (name addresses)
  (assert (and (stringp name) (every (function addressp) addresses)))
  (cons name addresses))
(defun aliasp (object)
  (and (listp object)
       (stringp (first object))
       (listp (rest object))
       (every (function addressp) (rest object))))
(defun alias-name      (alias) (first alias))
(defun alias-addresses (alias) (rest alias))
(defun (setf alias-addresses) (addresses alias) (setf (rest alias) addresses))



(defstruct db path records)

(defun clean-db-lines (lines)
  (loop
     :for current :on lines
     :do (setf (car current) (substitute #\space #\tab  (car current))))
  lines)

(defun load-aliases (&OPTIONAL (ALIAS-FILE-PATH "/etc/aliases")
                     &key (external-format :default))
  (make-db :path alias-file-path
           :records (MAPCAR
                     (FUNCTION PARSE-ALIAS)
                     (JOIN-CONTINUATION-LINES
                      (CLEAN-DB-LINES
                       (WITH-OPEN-FILE (IN ALIAS-FILE-PATH
                                           :DIRECTION :INPUT
                                           :IF-DOES-NOT-EXIST :ERROR
                                           :external-format external-format)
                         (STREAM-TO-STRING-LIST IN)))))))

(defparameter *max-column* 78)

(defun save-aliases (db &key (stream *standard-output* streamp)
                     (FILE-PATH (db-path db) file-path-p)
                     (if-exists :error)
                     (if-does-not-exist :create)
                     (external-format :default))
  (flet ((write-aliases (out)
           (dolist (record (db-records db))
             (if (commentp record)
                 (format out "~A~%" (comment-text record))
                 (let ((name (normal-or-quote-string (alias-name record)))
                       (column 0))
                   (format out "~A: " name)
                   (incf column (+ (length name) 2))
                   (let* ((address (first  (alias-addresses record)))
                          (item (address-string address)))
                     (if (< (+ column (length item)) *max-column*)
                         (progn (format out "~A" item)
                                (incf column (length item)))
                         (progn (format out "~%    ~A" item)
                                (setf column (+ 4 (length item))))))
                   (dolist (address (rest (alias-addresses record)))
                     (let ((item (address-string address)))
                       (if (< (+ column 2 (length item)) *max-column*)
                           (progn (format out ", ~A" item)
                                  (incf column (+ 2 (length item))))
                           (progn (format out "~%    ~A" item)
                                  (setf column (+ 4 (length item)))))))
                   (format out "~%"))))))
    (when (and streamp file-path-p)
      (error ":stream and :file-path are mutually exclusive."))
    (if file-path-p
        (with-open-file (out file-path
                             :direction :output
                             :if-exists if-exists
                             :if-does-not-exist if-does-not-exist
                             :external-format external-format)
          (write-aliases out))
        (write-aliases stream)))
  (values)) 


(defun find-record-if (db predicate)
  (find-if predicate (db-records db)))

(defun alias-record (db name)
  (find-record-if db (lambda (record)
                       (and (aliasp record)
                            (string-equal (alias-name record) name)))))

(defun comment-record-containing (db substring)
  (find-record-if db (lambda (record)
                       (and (commentp record)
                            (search substring (comment-text record)
                                    :test (function char=))))))


(defun list-all-aliases (db)
  (mapcan (lambda (record) (when (aliasp record) (list (alias-name record))))
          (db-records db)))

(defun insert-record (db record &key (before nil beforep) (after :all afterp))
  (when (and beforep afterp)
    (error "Cannot insert both :BEFORE and :AFTER at the same time."))
  (flet ((insert (where which)
           (loop
              :for current :on (db-records db)
              :until (or (null (cdr current))
                         (eq (funcall where current) which))
              :finally (push record (cdr current)))))
    (if beforep
        (if (or (eq :all before) (eq (first (db-records db)) before))
            (push record (db-records db))
            (insert (function cadr) before))
        (if (eq :all after)
            (push record (cdr (last (db-records db))))
            (insert (function car) after))))
  (values))


(defun delete-record (db record)
  (setf (db-records db) (delete record (db-records db)))
  (values))



(defun remove-user (db name)
  (let ((removed-records '()))
    (dolist (record (db-records db))
      (when (aliasp record)
        (setf (alias-addresses record)
              (delete-if (lambda (address)
                           (and (eq :address (address-type address))
                                (string-equal (address-value address) name)))
                         (alias-addresses record)))
        (when (null (alias-addresses record))
          (push record removed-records))))
    (when removed-records
      (setf (db-records db)
            (delete-if (lambda (record) (member record removed-records))
                       (db-records db))))
    (loop
       :for record :in (db-records db)
       :if (and (aliasp record) (equal name (alias-name record)))
       :collect record :into deleted
       :else :collect record :into kept
       :finally (setf (db-records db) kept
                      removed-records (nconc deleted removed-records)))
    removed-records))


(defun records-between-tags (db begin end)
  (loop
     :for record :in (cdr (member-if
                            (lambda (record)
                              (and (commentp record)
                                   (search begin (comment-text record)
                                           :test (function char=))))
                            (db-records db)))
     :while (and record (not (and (commentp record)
                                   (search end (comment-text record)
                                           :test (function char=)))))
     :collect record))


(defmacro with-alias-file ((varname pathname &key (external-format :default))
                           &body body)
  `(let ((,varname (load-aliases ,pathname :external-format ,external-format)))
     (prog1 (progn ,@body)
       (save-aliases db
                     :file-path ,pathname
                     :external-format ,external-format
                     :if-exists :supersede))))


;;; (ext:shell "diff -twb afaa.alias afaa-new.alias") 
;;;  postalias -f -q file2 hash:test.alias

