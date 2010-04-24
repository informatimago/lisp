;;;;**************************************************************************
;;;;FILE:               hquery.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This packages exports utility routines for web applications.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-05-30 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2007 - 2007
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
;;;;**************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
;; (DECLAIM (DECLARATION ALSO-USE-PACKAGES)
;;          (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.HTML"))
;; (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;   (COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:ADD-NICKNAME
;;      "COM.INFORMATIMAGO.COMMON-LISP.HTML" "HTML"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HQUERY"
  (:USE "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.STRING")
  (:EXPORT "QUERY-ESCAPE" "QUERY-UNESCAPE" "QUERY-PARSE" "QUERY-ARGUMENT"
           "BUILD-QUERY")
  (:DOCUMENTATION
   "This packages exports utility routines for web applications.
    
    Copyright Pascal Bourguignon 2007 - 2007
    
    This program is free software you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation either version
    2 of the License, or (at your option) any later version.
"))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HQUERY")


(DEFUN QUERY-ESCAPE (string)
  "
RETURN:     A string where the spaces are replaced by '+'
            and other unsavory characters are replaced by %HL sequences.
"
  (DO ((RESULT (MAKE-STRING (* 3 (LENGTH string))))
       (I 0)
       (J 0))
      ((<= (LENGTH string) I) (SUBSEQ RESULT 0 J))
    (COND
      ((CHAR= (CHARACTER " ") (CHAR STRING I))
       (SETF (CHAR RESULT J) (CHARACTER "+")) (INCF J) (INCF I))
      ((position
        (char string i)
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
       (SETF (CHAR RESULT J) (CHAR STRING I)) (INCF J) (INCF I))
      (t
       (let ((escape (format nil "%~16,2R" (char-code (char string i)))))
         (SETF (CHAR RESULT J) (char escape 0)) (incf j)
         (SETF (CHAR RESULT J) (char escape 1)) (incf j)
         (SETF (CHAR RESULT J) (char escape 2)) (incf j)
         (incf i))))))


(DEFUN QUERY-UNESCAPE (QPART)
  "
RETURN:     A string where the + are replaced by spaces
            and the %HL are replaced by the caracter whose code is HL
            in hexadecimal.
"
  (DO ((RESULT (MAKE-STRING (LENGTH QPART)))
       (I 0)
       (J 0))
      ((<= (LENGTH QPART) I) (SUBSEQ RESULT 0 J))
    (COND
      ((CHAR= (CHARACTER "+") (CHAR QPART I))
       (SETF (CHAR RESULT J) (CHARACTER " "))
       (INCF J)
       (INCF I))
      ((AND (CHAR= (CHARACTER "%") (CHAR QPART I))
            (< (+ I 2) (LENGTH QPART)))
       (LET ((CODE (PARSE-INTEGER QPART :START (+ I 1) :END (+ I 3) :RADIX 16)))
         (COND
           ((NULL CODE)
            (SETF (CHAR RESULT J) (CHAR QPART I)) (INCF J) (INCF I))
           ((= 13 CODE)
            (INCF I 3))
           (T
            (SETF (CHAR RESULT J) (CODE-CHAR CODE)) (INCF J) (INCF I 3))) ))
      (T
       (SETF (CHAR RESULT J) (CHAR QPART I)) (INCF J) (INCF I)))))


(DEFUN QUERY-PARSE (QUERY-STRING)
  "
RETURN:  the QUERY-ARGUMENTS, a list of lists (variable . value)
         found in the HTML CGI QUERY-STRING.
"
  (MAPCAR
   (LAMBDA (ASS)
     (LET* ((POSEP (POSITION (CHARACTER "=") ASS))
            (VAR (IF POSEP (SUBSEQ ASS 0 POSEP) ASS))
            (VAL (IF POSEP (SUBSEQ ASS (1+ POSEP)) "")))
       (CONS (QUERY-UNESCAPE VAR) (QUERY-UNESCAPE VAL))))
   (SPLIT-STRING QUERY-STRING "&")))


(DEFUN QUERY-ARGUMENT (NAME QUERY-ARGUMENTS)
  "
RETURN:  The value of the QUERY-STRING argument named NAME.
"
  (CDR (ASSOC NAME QUERY-ARGUMENTS :TEST (FUNCTION STRING-EQUAL))))


(defun build-query (&rest field-value)
  (format nil "~{~A=~A~^&~}" 
          (mapcar (lambda (item) (query-escape (if (stringp item) 
                                                   item
                                                   (format nil "~A" item))))
                  field-value)))

;;;; THE END ;;;;