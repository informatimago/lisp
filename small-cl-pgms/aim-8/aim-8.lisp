;;;;****************************************************************************
;;;;FILE:               aim-8.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements the LISP described in AIM-8 in Common-Lisp.
;;;;    Usage:  (load "aim-8.lisp") 
;;;;            (aim-8:repl)
;;;;    Then at the aim-8 prompt, you have LISP, plus:
;;;;       (DEFINE name sexp)     corresponding to =
;;;;       (RELOAD)               to reload aim-8 if you edit it.
;;;;       (DUMP-ENVIRONMENT)     to dump the defined symbols.
;;;;       (LOAD "path")          to load an aim-8 source. Try "aim-8.aim-8".
;;;;
;;;;     AIM-8 -- 4 MARCH 1959 -- J. MCCARTHY
;;;;     With an addendum dated 23 MARCH 1959
;;;;     ftp://publications.ai.mit.edu/ai-publications/pdf/AIM-008.pdf
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-10-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2004 - 2004
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

(DEFPACKAGE "AIM-8"
  (:USE "COMMON-LISP")
  (:EXPORT "REPL")
  (:DOCUMENTATION
   "Implements the lisp of AIM-8 -- 4 MARCH 1959 -- J. MCCARTHY
With an addendum dated 23 MARCH 1959
ftp://publications.ai.mit.edu/ai-publications/pdf/AIM-008.pdf"))
(IN-PACKAGE "AIM-8")


(DEFPARAMETER *ENVIRONMENT* (MAKE-HASH-TABLE :TEST (FUNCTION EQ)))
(DEFMACRO DEF     (NAME)       `(GETHASH ,NAME *ENVIRONMENT*))
(DEFUN   %BOUNDP  (NAME) (MULTIPLE-VALUE-BIND (VAL BND) (DEF NAME)
                          (DECLARE (IGNORE VAL)) BND))
(DEFMACRO DEFINE  (NAME VALUE) `(SETF (GETHASH ',NAME *ENVIRONMENT*) ',VALUE))
(DEFUN   FDEFINE  (NAME VALUE)  (SETF (GETHASH NAME *ENVIRONMENT*) VALUE))

(DEFINE NIL ())
(DEFINE F   ())
(DEFINE T   T)
(DEFINE AND     (LAMBDA (A B) (COND (A (COND (B T) (T NIL))) (T NIL))))
(DEFINE OR      (LAMBDA (A B) (COND (A T) (B T) (T NIL))))
(DEFINE NOT     (LAMBDA (A)   (COND (A NIL) (T T))))
(DEFINE MAPLIST 
        (LAMBDA (X F)
          (COND ((NULL X) NIL)
                (T (COMBINE (F X) (MAPLIST (REST X) F))))))
(DEFINE SUBST 
        (LAMBDA (X Y A)
          (COND ((NULL A) NIL)
                ((ATOM A) (COND ((EQ Y A) X) (T A)))
                (T (COMBINE (SUBST X Y (FIRST A))
                            (SUBST X Y (REST A))))
                )))


(DEFUN %SUBST (X Y A)
  (COND ((NULL A) NIL)
        ((ATOM A) (COND ((EQ Y A) X) (T A)))
        (T (CONS (%SUBST X Y (FIRST A)) (%SUBST X Y (REST A))))))


(DEFUN %SUBSQ (X Y Z)
  (COND ((NULL Z) NIL)
        ((ATOM Z) (COND ((EQ Y Z) X)  (T Z)))
        ((EQ (FIRST Z) 'QUOTE) Z)
        (T (CONS (%SUBSQ X Y (FIRST Z)) (%SUBSQ X Y (REST Z))))))


(DEFUN %EVCON (C)
  (COND ((%EVAL (FIRST (FIRST C))) (%EVAL (FIRST (REST (FIRST C)))))
        (T (%EVCON (REST C)))))


(DEFUN %EVLAM (VARS EXP ARGS)
  (COND ((NULL VARS) (%EVAL EXP))
        (T (%EVLAM (REST VARS) (%SUBSQ (FIRST ARGS) (FIRST VARS) EXP)
                   (REST ARGS)))))


(DEFUN %APPLY (F ARGS) (%EVAL (CONS F ARGS)))


(DEFUN %EVAL (E)
  (COND
    ;; begin extensions:
    ((ATOM E) (COND ((%BOUNDP E) (DEF E))
                    (T (ERROR "Undefined: ~A" (FIRST E)))))
    ;; end extensions.
    (T (CASE (FIRST E)
         ((NULL)    (NULL  (%EVAL (FIRST (REST E)))))
         ((ATOM)    (ATOM  (%EVAL (FIRST (REST E)))))
         ((QUOTE)                 (FIRST (REST E)))
         ((EQ)      (EQ    (%EVAL (FIRST (REST E)))
                           (%EVAL (FIRST (REST (REST E))))))
         ((COMBINE) (CONS  (%EVAL (FIRST (REST E)))
                           (%EVAL (FIRST (REST (REST E))))))
         ((FIRST)   (FIRST (%EVAL (FIRST (REST E)))))
         ((REST)    (REST  (%EVAL (FIRST (REST E)))))
         ((COND)    (%EVCON (REST E)))
         ;; begin extensions:
         ((LOAD)    (LOAD  (%EVAL (FIRST (REST E)))))
         ((PRINT)   (PRINT (%EVAL (FIRST (REST E)))))
         ((READ)    (read))
         (OTHERWISE
          (COND
            ((ATOM (FIRST E))
             (COND ((%BOUNDP (FIRST E)) (%APPLY (DEF (FIRST E)) (REST E)))
                   (T (ERROR "Undefined: ~A" (FIRST E)))))
            ;; end extensions.
            (T (CASE (FIRST (FIRST E))
                 ((LAMBDA) (%EVLAM (FIRST (REST (FIRST E)))
                              (FIRST (REST (REST (FIRST E))))
                              (REST E)))
                 ((LABEL) (%EVAL (CONS (%SUBST (FIRST E)
                                               (FIRST (REST (FIRST E)))
                                               (FIRST (REST (REST (FIRST E)))))
                                       (REST E))))
                 (OTHERWISE (ERROR "Invalid: ~A" (FIRST E)))))))))))



(DEFUN HELP ()
  (FORMAT T "~&You've got:  
    LAMBDA LABEL
    COND AND OR NOT  COMBINE FIRST REST
    NULL ATOM EQ NIL T QUOTE
Extensions:
    DEFINE RELOAD DUMP-ENVIRONMENT LOAD
    QUIT"))


(defmacro handling-errors (&body body)
  `(HANDLER-CASE (progn ,@body)
     (simple-condition 
      (ERR) 
      (format *error-output* "~&~A: ~%" (class-name (class-of err)))
      (apply (function format) *error-output*
             (simple-condition-format-control   err)
             (simple-condition-format-arguments err))
      (format *error-output* "~&"))
     (condition 
      (ERR) 
      (format *error-output* "~&~A: ~%  ~S~%" (class-name (class-of err)) err))))


(DEFUN REPL ()
  (LET ((*PACKAGE* (FIND-PACKAGE "AIM-8")))
    (HELP)
    (LOOP
       (TERPRI)
       (PRINC "AIM-8> ")
       (HANDLING-ERRORS
        (LET ((SEXP (READ)))
          (COND
            ((EQUAL SEXP '(QUIT))
             (FORMAT T "GOOD BYE") (RETURN-FROM REPL))
            ((EQUAL SEXP '(RELOAD))
             (LOAD "aim-8") (REPL) (RETURN-FROM REPL))
            ((EQUAL SEXP '(DUMP-ENVIRONMENT))
             (FORMAT T "~:{~16@A = ~A~%~}" 
                     (LET ((RES '()))
                       (MAPHASH (LAMBDA (K V) (PUSH (LIST K V) RES)) 
                                *ENVIRONMENT*) RES)))
            ((AND (LISTP SEXP) (EQ (FIRST SEXP) 'DEFINE))
             (FDEFINE (SECOND SEXP) (THIRD SEXP))
             (FORMAT T "~A" (SECOND SEXP)))
            (T 
             (FORMAT T "~S" (%EVAL SEXP))))))))
  (TERPRI)
  (VALUES))


(DEFPACKAGE "AIM-8-USER"
  (:USE)
  (:IMPORT-FROM "AIM-8"
                "DEFINE" "LAMBDA" "LABEL"
                "COND"  "COMBINE" "FIRST" "REST"
                "NULL" "ATOM" "EQ" "NIL" "T" "QUOTE"))

;;;; aim-8.lisp                       --                     --          ;;;;
