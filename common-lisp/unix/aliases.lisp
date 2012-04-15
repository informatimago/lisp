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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2005
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
(declaim (also-use-packages "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048"))

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.UNIX.ALIASES"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM")
  (:export "READ-DOT-FORWARD" "READ-ALIASES")
  (:documentation
   "This package exports a function to read unix aliases files.

    Copyright Pascal J. Bourguignon 2003 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.UNIX.ALIASES")



(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-verbose* nil)) (com.informatimago.common-lisp.cesarum.ecma048:generate-all-functions-in-ecma048)))

(defvar +ht+ (or (ignore-errors (read-from-string "#\\TAB"))
                 (code-char com.informatimago.common-lisp.cesarum.ecma048:ht)))
;; How can we define a HT character portably?
;; Even (code-char 9) may not work...
;; If the TAB code doesn't exist on the host,
;; the input files wouldn't contain any, and we shouldn't evey try to test it.

(defvar +sphtcrlf+
  (format nil " ~C~C~C"
          (code-char com.informatimago.common-lisp.cesarum.ecma048:ht) (code-char com.informatimago.common-lisp.cesarum.ecma048:cr) (code-char com.informatimago.common-lisp.cesarum.ecma048:lf))
  "A string containing space, tabulation, carriage return and line feed.")


(defvar +spht+
  (format nil " ~C" (code-char com.informatimago.common-lisp.cesarum.ecma048:ht))
  "A string containing space and tabulation.")


(defvar +crlf+
  (format nil "~C~C" (code-char com.informatimago.common-lisp.cesarum.ecma048:cr) (code-char com.informatimago.common-lisp.cesarum.ecma048:lf))
  "A string containing a carriage return and a line feed.")


(defvar +cr+
  (format nil "~C" (code-char com.informatimago.common-lisp.cesarum.ecma048:cr))
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


(defun parse-token (line pos token)
  (and (<= (+ pos (length token)) (length line))
       (string= line token :start1 pos :end1 (+ pos (length token)))))


(defun skip-spaces (line pos)
  (while (parse-token line pos " ")
    (incf pos))
  pos)


(defun parse-address (line pos follow)
  "
start ::= address | file | command | include .
address ::= rfc822-address .
file ::= '/' path .
command ::= '|' command .
include ::= ':include:' '/' path.
"
  (setq pos (skip-spaces line pos))
  (cond
    ((<= (length line) pos) (error "Expected an address, got end of line."))
    ((char= (character "\"") (char line pos))
     (read-from-string line nil nil :start pos))
    (t  (do ((i pos (1+ i)))
            ((or (<= (length line) i)
                 (position (char line i) follow))
             (values (subseq line pos i) i))))))


(defun parse-address-list (line pos)
  (multiple-value-bind (address npos) (parse-address line pos ", #")
    (let ((address-list (list address)))
      (setq pos (skip-spaces line npos))
      (loop :while (< pos (length line)) :do
         
         (multiple-value-bind (address npos)
             (parse-address line (+ (if (parse-token line pos ",")
                                        1 0) pos) ", #")
           (push address address-list)
           (setq pos (skip-spaces line npos))))
      (values (nreverse address-list) pos))))

;; address-list ::= address | address-list [ ',' ] address .
;; address      ::= '"' ( but-quote-or-antislash | '\' anychar ) * '"'
;;                  | but-quote-space-or-comma .


(defun parse-eoln (line pos)
  (setq pos (skip-spaces line pos))
  (<= (length line) pos))


(defun encapsulate (addr)
  (cond
    ((char= (char addr 0) (character "\\"))
     (cons :quote   (string-downcase (subseq addr 1))))
    ((char= (char addr 0) (character "|"))
     (cons :command (subseq addr 1)))
    ((char= (char addr 0) (character "/"))
     (cons :file    addr))
    ((prefixp ":include:" addr)
     (cons :include (subseq addr 9)))
    (t
     (cons :address (string-downcase addr)))))

     
(defun parse-dot-forward-alias (line)
  (multiple-value-bind (address-list pos) (parse-address-list line 0)
    ;; TODO: Check that we reach EOLN!!!
    (parse-eoln line pos)
    (mapcar (function encapsulate) address-list)))


(defun parse-alias (line)
  (if (or (zerop (length line)) (char= #\# (aref line 0)))
      ;; A comment
      (list :comment line)
      ;; An alias
      (multiple-value-bind (address pos) (parse-address line 0 ": #")
        (setq pos (skip-spaces line pos))
        (if (parse-token line pos ":")
            (multiple-value-bind (address-list pos)
                (parse-address-list line (1+ pos))
              ;; TODO: Check that we reach EOLN!!!
              (parse-eoln line pos)
              (cons (string-downcase address)
                    (mapcar (function encapsulate) address-list)))
            (error "Expected a ':'.")))))


(defun join-continuation-lines (lines)
  (nreverse
   (cdr
    (let ((tag (gensym)))
      (reduce (lambda (&optional a b)
                (let (joined line)
                  (if (and (listp a) (eq tag (car a)))
                      (setq joined a line b)
                      (setq joined b line a))
                  (if (and (and (plusp (length line))
                                (char= (character " ") (char line 0)))
                           (first (cdr joined)))
                      ;; continuation-line
                      (setf (first (cdr joined))
                            (concatenate 'string (first (cdr joined)) line))
                      (setf (cdr joined) (cons line (cdr joined))))
                  joined))
              lines
              :initial-value (cons tag nil))))))


(defun remove-comments (lines)
  (mapcan (lambda (line)
            (setq line (string-right-trim " " line))
            (when (and (< 0 (length line))
                       (char/= (character "#") (char line 0)))
              (list line)))
          lines))


(defun clean-lines (lines)
  "
DO:    Clean CR/LF stuff and replace tabulations by spaces.
"
  (mapcan (lambda (in-line)
            (mapcar (lambda (line)
                      (substitute (character " ") (code-char 9) line))
                    ;; This SPLIT-STRING returns NIL for an empty string.
                    (split-string (string-trim +crlf+ in-line) +crlf+)))
          lines))



(defun read-aliases (&optional (alias-file-path "/etc/aliases"))
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
  (mapcar (function parse-alias)
          (join-continuation-lines
           (remove-comments
            (clean-lines
             (with-open-file (in alias-file-path
                                 :direction :input
                                 :if-does-not-exist :error)
               (stream-to-string-list in)))))))
  

(defun read-dot-forward (forward-file-path)
  "
RETURN:  A list of ( address...).
         address is a cons:
          ( :address . address )    ;; address  (downcased)
          ( :quote   . address )    ;; \address (downcased)
          ( :file    . /file/name ) ;; /file/name
          ( :command . command )    ;; |command
          ( :include . /file/name ) ;; :include:/file/name
"
  (mapcar (function parse-dot-forward-alias)
          (join-continuation-lines
           (remove-comments
            (clean-lines
             (with-open-file (in forward-file-path
                                 :direction :input
                                 :if-does-not-exist nil)
               (when in
                 (stream-to-string-list in))))))))



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

(defun load-aliases (&optional (alias-file-path "/etc/aliases")
                     &key (external-format :default))
  (make-db :path alias-file-path
           :records (mapcar
                     (function parse-alias)
                     (join-continuation-lines
                      (clean-db-lines
                       (with-open-file (in alias-file-path
                                           :direction :input
                                           :if-does-not-exist :error
                                           :external-format external-format)
                         (stream-to-string-list in)))))))

(defparameter *max-column* 78)

(defun save-aliases (db &key (stream *standard-output* streamp)
                     (file-path (db-path db) file-path-p)
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

