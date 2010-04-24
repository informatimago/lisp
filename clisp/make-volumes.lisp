;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               make-volumes.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;NOWEB:              T
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a function to tally a directory tree 
;;;;    and make if it 'volumes' of a given maximum size.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-06-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
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

(DEFINE-PACKAGE "COM.INFORMATIMAGO.CLISP.MAKE-VOLUMES"
  (:DOCUMENTATION
   "This package exports a function to tally a directory tree and make if it
    'volumes' of a given maximum size.")
  (:FROM "COMMON-LISP" :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.LIST"    :IMPORT "NSPLIT-LIST" "AGET")
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" :IMPORT "WHILE")
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING"  :IMPORT "SPLIT-STRING")
  (:USE  "COM.INFORMATIMAGO.CLISP.SUSV3" :AS "SUSV3")
  (:EXPORT  "MAIN" "MAKE-VOLUMES"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TREE 
;;


(DEFUN DEEP-COPY-TREE (TREE)
  "
NOTE:           COPY-TREE ONLY DUPLICATES THE CONS, NOT THE OBJECTS.
                THIS IS UNFORTUNATE, BECAUSE WE OFTEN NEED TO DUPLICATE
                THE OBJECTS (STRINGS, ARRAY) TOO, BECAUSE OF THE
                MUTABLE/IMMUTABLE PROBLEM.
DO:             MAKES A COPY OF THE TREE, COPYING THE LEAF OBJECTS TOO.
"
  (COND
    ((CONSP TREE)   (CONS (DEEP-COPY-TREE (CAR TREE))
                          (DEEP-COPY-TREE (CDR TREE))))
    ((VECTORP TREE) (COPY-SEQ TREE))
    (T              TREE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AN ENUMERATOR IS A FUNCTION THAT RETURNS EACH TIME IT'S CALLED THE
;; NEXT ITEM OF A (POSSIBLY VIRTUAL AND POSSIBLY INFINITE) SEQUENCE AND T.
;; WHEN NO ITEM REMAINS, IT RETURNS (VALUES NIL NIL).
;; ENUMERATORS CAN BE CONCATENATED WITH APPEND-ENUMERATORS.
;; (OF COURSE APPENDING AN ENUMERATOR AFTER AN INFINITE ENUMERATOR WOULD
;; BE USELESS).


(DEFUN MAKE-LIST-ENUMERATOR (LIST)
  "
RETURN:         A ENUMERATOR FUNCTION FOR THE LIST.
NOTE:           THE ENUMERATOR FUNCTION RETURNS IN TURN EACH ELEMENT OF THE
                LIST AS FIRST VALUE AND A BOOLEAN T UNLESS THE END OF THE
                LIST IS REACHED AS SECOND VALUE.
"
  (LAMBDA ()
    (MULTIPLE-VALUE-PROG1
        (VALUES (CAR LIST) (NOT (NULL LIST)))
      (SETQ LIST (CDR LIST)))))



(DEFUN APPEND-ENUMERATORS (&REST ENUMERATORS)
  "
RETURN:        An enumerator that enumerates all the enumerators in turn.
"
  (LAMBDA ()
    (BLOCK :META-ENUM
      (LOOP
         (IF (NULL ENUMERATORS)
             (RETURN-FROM :META-ENUM (VALUES NIL NIL))
             (MULTIPLE-VALUE-BIND (VAL IND) (FUNCALL (CAR ENUMERATORS))
               (IF IND
                   (RETURN-FROM :META-ENUM (VALUES VAL IND))
                   (POP ENUMERATORS))))))))



(DEFUN COLLECT-ENUMERATOR (ENUMERATOR)
  (DO ((RESULT '())
       (DONE NIL))
      (DONE RESULT)
    (MULTIPLE-VALUE-BIND (VAL IND) (FUNCALL ENUMERATOR)
      (IF IND
          (PUSH VAL RESULT)
          (SETQ DONE T)))))



(DEFUN MAP-ENUMERATOR (LAMBDA-EXPR ENUMERATOR)
  (DO ((RESULT '())
       (DONE NIL))
      (DONE (NREVERSE RESULT))
    (MULTIPLE-VALUE-BIND (VAL IND) (FUNCALL ENUMERATOR)
      (IF IND
          (PUSH (FUNCALL LAMBDA-EXPR VAL) RESULT)
          (SETQ DONE T)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UNIX PATHNAMES

(DEFUN BASENAME (UNIX-PATH)
  "
UNIX-PATH:  A STRING CONTAINING A UNIX PATH.
RETURN:         THE BASENAME, THAT IS, THE LAST COMPONENT OF THE PATH.
                TRAILING '/'S ARE REMOVED FIRST.
"
  (DO* ((END (DO ((END (1- (LENGTH UNIX-PATH)) (1- END)))
                 ((OR (< END 0)
                      (CHAR/= (CHARACTER "/") (CHAR UNIX-PATH END)))
                  (1+ END))))
        (START (1- END) (1- START)))
       ((OR (< START 0)
            (CHAR= (CHARACTER "/") (CHAR UNIX-PATH START)))
        (SUBSEQ UNIX-PATH (1+ START) END))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; XSI:



(DEFUN UNIX-FS-NODE-NAME (NODE) ;; --> NAME
  (declare (ignore node)))
(DEFUN UNIX-FS-NODE-KIND (NODE)
  (declare (ignore node)))
(DEFUN UNIX-FS-NODE-DIRECTORY-PATH (NODE) ;; --> "/DIR/PATH"
  (declare (ignore node)))
(DEFUN UNIX-FS-NODE-PATH (NODE) ;; --> /DIR/PATH/NAME
  (declare (ignore node)))

(defun LOGICAL-PATHNAME-NAMESTRING-P (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun PARSE-LOGICAL-PATHNAME-NAMESTRING (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun PARSE-UNIX-PATHNAME-NAMESTRING (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun SAFE-MAKE-PATHNAME (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun SAFE-DIRECTORY (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))


(DEFUN MAKE-VOLUMES (ROOT-DIR)
  (LET ((ROOT-APATH
         ;; TODO: WHAT IF NOT UNIX?
         (IF (LOGICAL-PATHNAME-NAMESTRING-P  ROOT-DIR)
             (PARSE-LOGICAL-PATHNAME-NAMESTRING ROOT-DIR)
             (PARSE-UNIX-PATHNAME-NAMESTRING ROOT-DIR)))
        PATHSPEC)
    (WHEN (EQ :ERROR ROOT-APATH)
      (ERROR "BAD PATHNAME ~S." ROOT-DIR))
    (SETQ PATHSPEC (SAFE-MAKE-PATHNAME
                    :HOST      (AGET ROOT-APATH :HOST)
                    :DEVICE    (AGET ROOT-APATH :DEVICE)
                    :DIRECTORY (APPEND (AGET ROOT-APATH :DIRECTORY)
                                       (LIST :WILD-INFERIORS))
                    :NAME :WILD
                    :TYPE :WILD
                    :VERSION NIL
                    :CASE :COMMON))
    (FORMAT T "PATHSPEC=~S~%" PATHSPEC)
    (SAFE-DIRECTORY PATHSPEC)))

#||
(LOAD "PACKAGES:COM;INFORMATIMAGO;ENCOURS;MAKE-VOLUMES")
(IN-PACKAGE "COM.INFORMATIMAGO.CLISP.MAKE-VOLUMES")
(MAKE-VOLUMES "/tmp/")
||#

;;;; make-volumes.lisp                --                     --          ;;;;
