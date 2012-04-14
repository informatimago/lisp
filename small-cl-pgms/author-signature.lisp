;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               author-signature.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     Common-Lisp
;;;;DESCRIPTION
;;;;    
;;;;    This program compute an "author signature" from a text.
;;;;    See: http://unix.dsu.edu/~johnsone/comp.html
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2003-03-13 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2003 - 2003
;;;;    mailto:pjb@informatimago.com
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


(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.AUTHOR-SIGNATURE"
  (:DOCUMENTATION
   "This program compute an \"author signature\" from a text.
    See: http://unix.dsu.edu/~johnsone/comp.html
    
    Copyright Pascal Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:USE "COMMON-LISP")
  (:EXPORT COMPARE-TEXTS TALLY-COMPARE
           TALLY-WORD-LENGTHS TALLY-SMALL-WORDS
           TALLY-PERCENT  SPLIT-WORDS )
  );;COM.INFORMATIMAGO.COMMON-LISP.AUTHOR-SIGNATURE
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.AUTHOR-SIGNATURE")
 

(DEFUN STREAM-AS-STRING (STREAM)
  "
RETURN:  A string containing all the character read from the stream.
"
  (LOOP WITH RESULT = ""
        WITH EOLN = (FORMAT NIL "~%")
        FOR LINE = (READ-LINE STREAM NIL NIL)
        WHILE LINE
        DO (SETQ RESULT (CONCATENATE 'STRING RESULT LINE EOLN))
        FINALLY (RETURN RESULT))
  );;STREAM-AS-STRING


(DEFUN REMOVE-PONCTUATION (TEXT)
  "
RETURN: A copy of the text string where all character not alphanumeric is
        replaced by a space.
"
  (SETQ TEXT (COPY-SEQ TEXT))
  (LOOP FOR I FROM 0 BELOW (LENGTH TEXT)
        FOR CH = (CHAR TEXT I)
        DO (UNLESS (ALPHANUMERICP CH) (SETF (CHAR TEXT I) #\SPACE)))
  TEXT
  );;REMOVE-PONCTUATION


(DEFUN SPLIT-WORDS (TEXT)
  "
RETURN: A list of words read from the text.
"
  (WITH-INPUT-FROM-STRING
   (IN (REMOVE-PONCTUATION TEXT))
   (LET ((RESULT  ())
         (CH (READ-CHAR IN NIL NIL)))
     (LOOP WHILE CH DO
           (LOOP WHILE (AND CH (EQL #\SPACE CH)) ;;skip spaces
                 DO (SETQ CH (READ-CHAR IN NIL NIL)))
           (LOOP WHILE (AND CH (NOT (EQL #\SPACE CH)))
                 COLLECT CH INTO WORD
                 DO (SETQ CH (READ-CHAR IN NIL NIL))
                 FINALLY (WHEN (< 0 (LENGTH WORD))
                           (PUSH (MAKE-ARRAY (LIST (LENGTH WORD))
                                             :ELEMENT-TYPE 'CHARACTER
                                             :INITIAL-CONTENTS WORD) RESULT)))
           )
     (NREVERSE RESULT)))
  ) ;;SPLIT-WORDS


(DEFUN TALLY-WORD-LENGTHS (WORD-LIST)
  "
RETURN: An array containing the number of words of each length (in
        slot 0 is stored the number of words greater than (length result),
        and (length word-list).
"
  ;; max word length in French: 36.
  (LET* ((MAX-LEN 36)
         (TALLY (MAKE-ARRAY (LIST (1+ MAX-LEN))
                            :ELEMENT-TYPE 'FIXNUM
                            :INITIAL-ELEMENT 0))
         )
    (LOOP FOR WORD IN WORD-LIST
          FOR LEN = (LENGTH WORD)
          FOR COUNT = 0 THEN (1+ COUNT)
          DO
          (IF (< MAX-LEN LEN)
            (INCF (AREF TALLY 0))
            (INCF (AREF TALLY LEN)))
          FINALLY (RETURN (VALUES TALLY COUNT))))
  );;TALLY-WORD-LENGTHS


(DEFUN TALLY-SMALL-WORDS (WORD-LIST)
  "
RETURN: An array containing the number of occurences of a list of
        small words returned as third value.
        The second value is (length word-list).
"
  (LET* ((SMALL-WORDS '("A" "BUT" "IN" "NO" "OUR" "THE" "US"
                        "WE" "WHICH" "WITH"))
         (MAX-LEN (LENGTH SMALL-WORDS))
         (TALLY (MAKE-ARRAY (LIST (1+ MAX-LEN))
                            :ELEMENT-TYPE 'FIXNUM
                            :INITIAL-ELEMENT 0))
         )
    (LOOP FOR WORD IN WORD-LIST
          FOR COUNT = 0 THEN (1+ COUNT)
          FOR POS = (POSITION WORD SMALL-WORDS :TEST (FUNCTION STRING-EQUAL))
          DO
          (IF POS
            (INCF (AREF TALLY (1+ POS)))
            (INCF (AREF TALLY 0)))
          FINALLY (RETURN (VALUES TALLY COUNT SMALL-WORDS))))
  );;TALLY-SMALL-WORDS


;; (TALLY-SMALL-WORDS (SPLIT-WORDS (WITH-OPEN-FILE (IN "~/tmp/misc/test.txt" :DIRECTION :INPUT) (STREAM-AS-STRING IN))))


(DEFUN TALLY-PERCENT (TALLY COUNT)
  (LET ((RESULT  (MAKE-ARRAY (LIST (LENGTH TALLY))
                             :ELEMENT-TYPE 'FLOAT
                             :INITIAL-ELEMENT 0.0)))
    (DO ((I 0 (1+ I)))
        ((<= (LENGTH TALLY) I) RESULT)
      (SETF (AREF RESULT I) (COERCE (/ (AREF TALLY I) COUNT) 'FLOAT))))
  );;TALLY-PERCENT


(DEFUN MODULE (VECTOR)
  "
RETURN:  The module of the vector. [ sqrt(x^2+y^2+z^2) ]
"
  (SQRT (APPLY (FUNCTION +)
               (MAP 'LIST (FUNCTION (LAMBDA (X) (* X X))) VECTOR)))
  );;MODULE


(DEFUN TALLY-COMPARE (TALLY-1 TALLY-2)
  "
RETURN:  The module and the vector of percentages of differences
         between vectors tally-1 and tally-2.
"
  (ASSERT (= (LENGTH TALLY-1) (LENGTH TALLY-2)))
  (LET ((DIFFERENCES (MAKE-ARRAY (LIST (LENGTH TALLY-1))
                                 :ELEMENT-TYPE 'FLOAT
                                 :INITIAL-ELEMENT 0.0)))
    (DO* ((I 0 (1+ I))
          (D) (M))
        ((<= (LENGTH DIFFERENCES) I))
      (SETQ D (ABS (- (AREF TALLY-1 I) (AREF TALLY-2 I)))
            M (MAX (AREF TALLY-1 I) (AREF TALLY-2 I)))
      (SETF (AREF DIFFERENCES I) (IF (= 0.0 M) M (COERCE (/ D M) 'FLOAT))) )
    (VALUES (MODULE DIFFERENCES) DIFFERENCES))
  );;TALLY-COMPARE


(DEFUN COMPARE-TEXTS (PATH-LIST TALLY-FUNCTION)
  (LET ((TALLIES ()))
    (MAPC
     (LAMBDA (PATH)
       (WITH-OPEN-FILE (INPUT PATH  :DIRECTION :INPUT)
         (PUSH (CONS (NAMESTRING PATH)
                     (MULTIPLE-VALUE-BIND (TALLY C)
                         (FUNCALL TALLY-FUNCTION 
                          (SPLIT-WORDS (STREAM-AS-STRING INPUT)))
                       (TALLY-PERCENT TALLY C))) TALLIES)))
     PATH-LIST)
    (DO* ((T1 TALLIES (CDR T1))
          (N-TALLY-1 (CAR T1) (CAR T1))
          (TALLY-1 (CDR N-TALLY-1) (CDR N-TALLY-1)) )
        ((NULL T1))
  
      (DO* ((T2 (CDR T1) (CDR T2))
            (N-TALLY-2 (CAR T2) (CAR T2))
            (TALLY-2 (CDR N-TALLY-2) (CDR N-TALLY-2)) )
          ((NULL T2))

          (MULTIPLE-VALUE-BIND
           (M D) (TALLY-COMPARE TALLY-1 TALLY-2)
           (FORMAT T "~20A ~20A ~8A~%   ~A~%~%"
                   (CAR N-TALLY-1) (CAR N-TALLY-2) M D))
        ))
    TALLIES)
  );;COMPARE-TEXTS


;; (COMPARE-TEXTS (DIRECTORY "i-*.txt") (FUNCTION TALLY-WORD-LENGTHS))
;; (COMPARE-TEXTS (DIRECTORY "i-*.txt") (FUNCTION TALLY-SMALL-WORDS))

;; (TALLY-COMPARE
;;  (MULTIPLE-VALUE-BIND (TALLY C)
;;      (TALLY-WORD-LENGTHS (SPLIT-WORDS STR))
;;    (TALLY-PERCENT TALLY C))
;;  (MULTIPLE-VALUE-BIND (TALLY C)
;;      (TALLY-WORD-LENGTHS (SPLIT-WORDS STR2))
;;    (TALLY-PERCENT TALLY C)))



;;;; author-signature.lisp            -- 2004-03-14 01:32:40 -- pascal   ;;;;
