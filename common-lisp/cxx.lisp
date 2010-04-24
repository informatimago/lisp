;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               cxx.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     none
;;;;DESCRIPTION
;;;;    
;;;;    Parsing C++ sources.
;;;;    This is a restricted parser, used just to analyze
;;;;    the call graph of C++ functions and methods.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-01-07 <PJB> Updated.
;;;;    1996-10-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 1996 - 2003
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

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CXX"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.GRAPH" "COMMON-LISP")
  (:EXPORT "BUILD-METHOD-CALL-GRAF" "PARSE" "C++PROGRAM")
  (:DOCUMENTATION
   "Parsing C++ sources.
This is a restricted parser, used just to analyze
the call graph of C++ functions and methods."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CXX")




;;  (defclass Char-Filter             ()
;;      (defclass File-Filter             (Char-Filter)
;;      (defclass Look-ahead-Char-Filter      (Char-Filter)
;;  
;;  (defclass Token-Filter            ()
;;      (defclass C++Token-Filter         (Token-Filter) 
;;          (defclass C++NoNLToken-Filter     (C++Token-Filter)
;;      (defclass Look-ahead-Token-Filter (Token-Filter)
;;      (defclass C++Comment-Filter       (Token-Filter)
;;      (defclass CPreprocessor-Filter    (Token-Filter)
;;      (defclass CComment-Filter         (Token-Filter)

;;----------------------------------------------------------------------
;; method-header ::= [ type ] class-name '::' method-name '(' arguments ')' .
;; body          ::= '{' { statement | body } '}' .
;; statement     ::= { token } 


(DEFGENERIC SET-FILE (SELF F))
(DEFGENERIC READ-A-CHAR (SELF))
(DEFGENERIC DUMP (SELF))
(DEFGENERIC DO-QUOTES (SELF FLAG))
(DEFGENERIC SET-SOURCE (SELF INPUT))
(DEFGENERIC PUSH-ON-FILTER (SELF F))
(DEFGENERIC READ-A-TOKEN (SELF))
(DEFGENERIC PARSE-STATEMENT-LIST (SELF FILTER))
(DEFGENERIC C++CLASS-NAME (SELF))
(DEFGENERIC NAME (SELF))
(DEFGENERIC RES-TYPE (SELF))
(DEFGENERIC ARGUMENTS (SELF))
(DEFGENERIC CALLED-METHODS (SELF))
(DEFGENERIC PARSE (SELF FILE-NAME-LIST))
(DEFGENERIC ADD-C++METHOD (SELF METHOD))
(DEFGENERIC UNIFY-METHODS-BY-NAME (SELF))
(DEFGENERIC BUILD-METHOD-CALL-GRAF (SELF))
(DEFGENERIC PRINT-C++METHOD-NAMES (SELF))

;;----------------------------------------------------------------------

(DEFCLASS CHAR-FILTER ()
  ((PREVIOUS :ACCESSOR PREVIOUS :INITFORM NIL)))

(DEFMETHOD PUSH-ON-FILTER ((SELF CHAR-FILTER) F)
  (SETF (PREVIOUS SELF) F))

(DEFMETHOD READ-A-CHAR ((SELF CHAR-FILTER))
  (IF (NULL (PREVIOUS SELF))
      'EOF
      (READ-A-CHAR (PREVIOUS SELF))))

(DEFMETHOD DUMP ((SELF CHAR-FILTER))
  (LET ((C (READ-A-CHAR SELF)))
    (LOOP WHILE (NOT (EQUAL 'EOF C)) DO
         (WRITE-CHAR C)
         (SETQ C (READ-A-CHAR SELF)))))

;;----------------------------------------------------------------------

(DEFCLASS FILE-FILTER (CHAR-FILTER)
  ((FILE  :ACCESSOR FILE :INITFORM NIL)))

(DEFMETHOD SET-FILE ((SELF FILE-FILTER) F)
  (SETF (FILE SELF) F))

(DEFMETHOD READ-A-CHAR ((SELF FILE-FILTER))
  (IF (NULL (FILE SELF))
      'EOF
      (READ-CHAR (FILE SELF) NIL 'EOF)))

;;----------------------------------------------------------------------

(DEFCLASS LOOK-AHEAD-CHAR-FILTER (CHAR-FILTER)
  ((NEXT-CHAR :ACCESSOR NEXT-CHAR :INITFORM NIL)))

(DEFMETHOD PUSH-ON-FILTER :BEFORE ((SELF LOOK-AHEAD-CHAR-FILTER) F)
  (DECLARE (IGNORE F))
  (SETF (NEXT-CHAR SELF) NIL))

(DEFMETHOD READ-A-CHAR ((SELF LOOK-AHEAD-CHAR-FILTER))
  (IF (NULL (PREVIOUS SELF))
      'EOF
      (LET ((RES NIL))
        (IF (NULL (NEXT-CHAR SELF))
            (SETF (NEXT-CHAR SELF) (READ-A-CHAR (PREVIOUS SELF))))
        (SETQ RES (NEXT-CHAR SELF))
        (SETF (NEXT-CHAR SELF) (READ-A-CHAR (PREVIOUS SELF)))
        RES)))

;;----------------------------------------------------------------------

(DEFCLASS TOKEN-FILTER ()
  ((PREVIOUS :ACCESSOR PREVIOUS :INITFORM NIL)))

(DEFMETHOD DO-QUOTES ((SELF TOKEN-FILTER) FLAG)
  (IF (NULL (PREVIOUS SELF))
      NIL
      (DO-QUOTES (PREVIOUS SELF) FLAG)))

(DEFMETHOD PUSH-ON-FILTER ((SELF TOKEN-FILTER) (F TOKEN-FILTER))
  (SETF (PREVIOUS SELF) F))

(DEFMETHOD READ-A-TOKEN ((SELF TOKEN-FILTER))
  (IF (NULL (PREVIOUS SELF))
      'EOF
      (READ-A-TOKEN (PREVIOUS SELF))))

(DEFMETHOD DUMP ((SELF TOKEN-FILTER))
  (LET ((C (READ-A-TOKEN SELF)))
    (LOOP WHILE (NOT (EQUAL 'EOF C)) DO
         (PRINT C)
         (SETQ C (READ-A-TOKEN SELF)))))

;;----------------------------------------------------------------------

(DEFUN CHAR-KIND (C)
  (COND
    ((EQUAL C 'EOF)                                            'EOF)
    ((OR (CHAR= C (CODE-CHAR 9))  (CHAR= C (CHARACTER " ")))   'SPACE)
    ((OR (CHAR= C (CODE-CHAR 10)) (CHAR= C (CODE-CHAR 13)))    'NEW-LINE)
    ((OR (CHAR= (CHARACTER "_") C)
         (CHAR= (CHARACTER "~") C)
         (alpha-char-p c))                                         'LETTER)
    ((digit-char-p c)                                          'DIGIT)
    ((OR (CHAR= (CHARACTER "'") C) (CHAR= (CHARACTER "\"") C)) 'QUOTE)
    (T                                                         'SPECIAL)))

(DEFUN TOKEN-MEMBER (TOK LISTE)
  (COND
    ((NULL LISTE) NIL)
    ((EQUAL TOK (CAR LISTE)) T)
    (T (TOKEN-MEMBER TOK (CDR LISTE)))))

(DEFUN TOKEN-KIND (TOK)
  (LET* ((C (CHAR TOK 0)) (KIND (CHAR-KIND C)))
    (COND
      ((EQUAL KIND 'LETTER)
       (IF (TOKEN-MEMBER
            TOK 
            '("sizeof" "delete" "this" "friend" "typedef"
              "auto" "register" "static" "extern" "inline" 
              "virtual" "const" "volatile" "char" "short" "int" 
              "long" "signed" "unsigned" "float" "double" "void" 
              "enum" "class" "struct" "union" "asm" "private" 
              "protected" "public" "operator" "new" "case"
              "default"
              "if" "else" "switch" "while" "do" "for" "break" 
              "continue" "return" "goto" "template" "try" "catch"
              "throw"))
           'KEYWORD
           'IDENTIFIER))
      ((EQUAL KIND 'DIGIT)
       'NUMBER)
      ((EQUAL KIND 'QUOTE)
       'STRING)
      ((EQUAL KIND 'NEW-LINE)
       'NEW-LINE)
      (T
       'SPECIAL))))

;;----------------------------------------------------------------------

(DEFCLASS C++TOKEN-FILTER (TOKEN-FILTER)
  ((SOURCE   :ACCESSOR SOURCE   :INITFORM NIL)
   (DOQUOTES :ACCESSOR DOQUOTES :INITFORM T)
   (DOTRACE  :ACCESSOR DOTRACE  :INITFORM NIL)))

(DEFMETHOD DO-QUOTES ((SELF C++TOKEN-FILTER) FLAG)
  (LET ((OLD (DOQUOTES SELF)))
    (SETF (DOQUOTES SELF) FLAG)
    OLD))

(DEFMETHOD SET-SOURCE ((SELF C++TOKEN-FILTER) (INPUT LOOK-AHEAD-CHAR-FILTER))
  (SETF (SOURCE SELF) INPUT))

(DEFMETHOD READ-A-TOKEN ((SELF C++TOKEN-FILTER))
  ;; * '::' ( ) { } ;; '->'  "<char>*" CR '/*' '*/' '//'
  (LET ((R
         (LET ((S NIL) (C (READ-A-CHAR (SOURCE SELF))))
           ;; skip spaces;; note we don't skip new-lines here.
           (LOOP WHILE (EQUAL (CHAR-KIND C) 'SPACE) DO
                (SETQ C (READ-A-CHAR (SOURCE SELF))))
           (IF (EQUAL 'EOF C)
               'EOF
               (LET ((KIND (CHAR-KIND C)))
                 (SETQ S (CONS C S))
                 (COND
                   ((EQUAL KIND 'LETTER)
                    (SETQ C (NEXT-CHAR (SOURCE SELF)))
                    (SETQ KIND (CHAR-KIND C))
                    (LOOP WHILE (OR (EQUAL KIND 'LETTER) 
                                    (EQUAL KIND 'DIGIT)) DO
                         (SETQ C (READ-A-CHAR (SOURCE SELF)))
                         (SETQ S (CONS C S))
                         (SETQ C (NEXT-CHAR (SOURCE SELF)))
                         (SETQ KIND (CHAR-KIND C))))
                   ((EQUAL KIND 'DIGIT)
                    (SETQ C (NEXT-CHAR (SOURCE SELF)))
                    (SETQ KIND (CHAR-KIND C))
                    (LOOP WHILE (OR (EQUAL KIND 'DIGIT)
                                    (EQUAL C (CHARACTER ".")) 
                                    (AND (CHAR<= (CHARACTER "a") C)
                                         (CHAR<= C (CHARACTER "g")))
                                    (AND (CHAR<= (CHARACTER "A") C)
                                         (CHAR<= C (CHARACTER "G")))
                                    (EQUAL C (CHARACTER "x"))
                                    (EQUAL C (CHARACTER "X"))
                                    ) DO
                         (SETQ C (READ-A-CHAR (SOURCE SELF)))
                         (SETQ S (CONS C S))
                         (SETQ C (NEXT-CHAR (SOURCE SELF)))
                         (SETQ KIND (CHAR-KIND C))))
                   ((AND (DOQUOTES SELF) (EQUAL KIND 'QUOTE))
                    (LET ((TERM C))
                      (SETQ C (READ-A-CHAR (SOURCE SELF)))
                      (LOOP WHILE (NOT (OR (EQUAL 'EOF C)
                                           (EQUAL (CHAR-KIND C) 'NEW-LINE)
                                           (EQUAL TERM C))) DO
                           (SETQ S (CONS C S))
                           (IF (CHAR= (CHARACTER "\\") C)
                               (progn
                                 (SETQ C (READ-A-CHAR 
                                          (SOURCE SELF)))
                                 (SETQ S (CONS C S))))
                           (SETQ C (READ-A-CHAR (SOURCE SELF))))
                      (IF (EQUAL TERM C)
                          (SETQ S (CONS C S)))))
                   ((EQUAL KIND 'NEW-LINE)
                    (SETQ C (NEXT-CHAR (SOURCE SELF)))
                    (LOOP WHILE (EQUAL (CHAR-KIND C) 'NEW-LINE) DO
                         (SETQ C (READ-A-CHAR (SOURCE SELF)))
                         (SETQ S (CONS C S))
                         (SETQ C (NEXT-CHAR (SOURCE SELF)))))
                   ((CHAR= (CHARACTER "-") C)
                    (IF (CHAR= (CHARACTER ">")
                               (NEXT-CHAR (SOURCE SELF)))
                        (progn
                          (SETQ C (READ-A-CHAR (SOURCE SELF)))
                          (SETQ S (CONS C S)))))
                   ((CHAR= (CHARACTER "/") C)
                    (IF (OR (CHAR= (CHARACTER "/")
                                   (NEXT-CHAR (SOURCE SELF)))
                            (CHAR= (CHARACTER "*")
                                   (NEXT-CHAR (SOURCE SELF))))
                        (progn
                          (SETQ C (READ-A-CHAR (SOURCE SELF)))
                          (SETQ S (CONS C S)))))
                   ((CHAR= (CHARACTER "*") C)
                    (IF (CHAR= (CHARACTER "/")
                               (NEXT-CHAR (SOURCE SELF)))
                        (progn
                          (SETQ C (READ-A-CHAR (SOURCE SELF)))
                          (SETQ S (CONS C S)))))
                   ((CHAR= (CHARACTER ":") C)
                    (IF (CHAR= (CHARACTER ":")
                               (NEXT-CHAR (SOURCE SELF)))
                        (progn
                          (SETQ C (READ-A-CHAR (SOURCE SELF)))
                          (SETQ S (CONS C S))))))
                 (concatenate 'string (REVERSE S)))))))
    (IF (DOTRACE SELF)
        (FORMAT T "~a " R))
    R))

;;----------------------------------------------------------------------

(DEFCLASS C++NONLTOKEN-FILTER (C++TOKEN-FILTER)
  ())

(DEFMETHOD READ-A-TOKEN ((SELF C++NONLTOKEN-FILTER))
  (LET ((TOK (READ-A-TOKEN (PREVIOUS SELF))))
    (COND
      ((EQUAL TOK 'EOF)                     'EOF)
      ((EQUAL (TOKEN-KIND TOK) 'NEW-LINE)   (READ-A-TOKEN SELF))
      (T                                    TOK))))

;;----------------------------------------------------------------------

(DEFCLASS LOOK-AHEAD-TOKEN-FILTER (TOKEN-FILTER)
  ((DOTRACE    :ACCESSOR DOTRACE    :INITFORM NIL)
   (NEXT-TOKEN :ACCESSOR NEXT-TOKEN :INITFORM NIL)))

(DEFMETHOD PUSH-ON-FILTER :BEFORE 
    ((SELF LOOK-AHEAD-TOKEN-FILTER) (F TOKEN-FILTER))
  (SETF (NEXT-TOKEN SELF) NIL))

(DEFMETHOD READ-A-TOKEN ((SELF LOOK-AHEAD-TOKEN-FILTER))
  (IF (NULL (PREVIOUS SELF))
      'EOF
      (LET ((RES NIL))
        (IF (NULL (NEXT-TOKEN SELF))
            (SETF (NEXT-TOKEN SELF) (READ-A-TOKEN (PREVIOUS SELF))))
        (SETQ RES (NEXT-TOKEN SELF))
        (SETF (NEXT-TOKEN SELF) (READ-A-TOKEN (PREVIOUS SELF)))
        (IF (DOTRACE SELF)
            (FORMAT T "~a " RES))
        RES)))

;;----------------------------------------------------------------------

(DEFUN SKIP-COMMENT (SELF START-STRING STOP-LAMBDA)
  (IF (NULL (PREVIOUS SELF))
      'EOF
      (LET ((CUR-TOKEN (READ-A-TOKEN (PREVIOUS SELF))))
        (LOOP WHILE (EQUAL CUR-TOKEN START-STRING) DO
             (LET ((SAVED (DO-QUOTES SELF NIL)))
               (SETQ CUR-TOKEN (READ-A-TOKEN (PREVIOUS SELF)))
               (LOOP WHILE (NOT (OR (EQUAL CUR-TOKEN 'EOF)
                                    (APPLY STOP-LAMBDA (LIST CUR-TOKEN))))
                  DO (SETQ CUR-TOKEN (READ-A-TOKEN (PREVIOUS SELF))))
               (DO-QUOTES SELF SAVED)
               (unless (EQUAL CUR-TOKEN 'EOF)
                 (SETQ CUR-TOKEN (READ-A-TOKEN (PREVIOUS SELF))))))
        CUR-TOKEN)))

;;----------------------------------------------------------------------

(DEFCLASS CPREPROCESSOR-FILTER (TOKEN-FILTER)
  ())

(DEFMETHOD READ-A-TOKEN ((SELF CPREPROCESSOR-FILTER))
  (SKIP-COMMENT SELF "#" (LAMBDA (X) (EQUAL (TOKEN-KIND X) 'NEW-LINE))))

;;----------------------------------------------------------------------

(DEFCLASS CCOMMENT-FILTER (TOKEN-FILTER) ())

(DEFMETHOD READ-A-TOKEN ((SELF CCOMMENT-FILTER))
  (SKIP-COMMENT SELF "/*" (LAMBDA (X) (EQUAL X "*/"))))

;;----------------------------------------------------------------------

(DEFCLASS C++COMMENT-FILTER (TOKEN-FILTER) ())

(DEFMETHOD READ-A-TOKEN ((SELF C++COMMENT-FILTER))
  (SKIP-COMMENT SELF "//" (LAMBDA (X) (EQUAL (TOKEN-KIND X) 'NEW-LINE))))

;;----------------------------------------------------------------------

(DEFCLASS C++HEADER ()
  ((RES-TYPE         :ACCESSOR RES-TYPE         :INITFORM NIL)
   (C++CLASS-NAME    :ACCESSOR C++CLASS-NAME    :INITFORM NIL)
   (C++METHOD-NAME   :ACCESSOR C++METHOD-NAME   :INITFORM NIL)
   (ARGUMENTS        :ACCESSOR ARGUMENTS        :INITFORM NIL)
   (HEADER-KIND      :ACCESSOR HEADER-KIND      :INITFORM NIL)
   (BAD-TOKEN-LIST   :ACCESSOR BAD-TOKEN-LIST   :INITFORM NIL)))

(DEFUN RANGE (S FROM END) (SUBSEQ S FROM END))

(DEFMETHOD PARSE ((SELF C++HEADER) (FILTER TOKEN-FILTER))
  (LET ((L NIL) (TOK (READ-A-TOKEN FILTER)) (I 0))
    (LOOP WHILE (NOT (OR 
                      (EQUAL TOK 'EOF)
                      (EQUAL TOK ")")
                      (EQUAL TOK ";;"))) DO
         (SETQ L (CONS TOK L))
         (SETQ TOK (READ-A-TOKEN FILTER)))
    (when (NOT (EQUAL TOK 'EOF))
      (SETQ L (CONS TOK L)))
    (SETQ L (REVERSE L))
    (SETQ I (SEARCH "::" L))
    (IF (NULL I)
        (progn
          (SETF (HEADER-KIND SELF) 'FUNCTION)
          (SETQ I (SEARCH "(" L))
          (IF (OR (NULL I) (= 0 I))
              (progn
                (SETF (BAD-TOKEN-LIST SELF) L)
                NIL)
              (progn
                (SETF (C++CLASS-NAME SELF)  NIL)
                (SETF (RES-TYPE SELF)       (RANGE L 0 (- I 2)))
                (SETF (C++METHOD-NAME SELF) (CAR (RANGE L (1- I) (1- I))))
                (SETF (ARGUMENTS SELF)  (RANGE L I NIL))
                T)))
        (progn
          (SETF (HEADER-KIND SELF) 'METHOD)
          (SETF (C++CLASS-NAME SELF) (CAR (RANGE L (1- I) (1- I))))
          (SETF (RES-TYPE SELF)      (RANGE L 0 (- I 2)))
          (IF (EQUAL (NTH (1+ I) L) "~") 
              (progn
                (SETF (C++METHOD-NAME SELF) (CAR (RANGE L (1+ I) (+ I 2))))
                (SETF (ARGUMENTS SELF)  (RANGE L (+ I 3) NIL)))
              (progn
                (SETF (C++METHOD-NAME SELF) (CAR (RANGE L (1+ I) (1+ I))))
                (SETF (ARGUMENTS SELF)  (RANGE L (+ I 2) NIL))))
          T))))

;;----------------------------------------------------------------------
;; body          ::= '{' { statement | body } '}' .
;; statement     ::= { token } 

(DEFCLASS C++BODY ()
  ((INITIALIZER :ACCESSOR INITIALIZER :INITFORM NIL)
   (STATEMENTS  :ACCESSOR STATEMENTS  :INITFORM NIL)))

(DEFMETHOD PARSE ((SELF C++BODY) (FILTER TOKEN-FILTER))
  (LET ((TOK (READ-A-TOKEN FILTER)) (I NIL))
    (IF (EQUAL TOK ":")
        (LOOP WHILE (NOT (TOKEN-MEMBER TOK '("{" "}" 'EOF))) DO
             (SETQ I (CONS TOK I))
             (SETQ TOK (READ-A-TOKEN FILTER))))
    (SETF (INITIALIZER SELF) (REVERSE I))
    (IF (EQUAL TOK "{")
        (BLOCK NIL
          (SETF (STATEMENTS SELF) (PARSE-STATEMENT-LIST SELF FILTER))
          T)
        NIL)))

(DEFMETHOD PARSE-STATEMENT-LIST ((SELF C++BODY) (FILTER TOKEN-FILTER))
  (LET ((TOK (READ-A-TOKEN FILTER)) (S NIL))
    (LOOP WHILE (NOT (OR (EQUAL 'EOF TOK) (EQUAL TOK "}"))) DO
         (IF (EQUAL TOK "{")
             (SETQ S (CONS (PARSE-STATEMENT-LIST SELF FILTER) S))
             (SETQ S (CONS TOK S)))
         (SETQ TOK (READ-A-TOKEN FILTER)))
    (IF (EQUAL TOK "}")
        (REVERSE S)
        NIL)))

(DEFUN SEARCH-METHOD-CALLS (STATEMENTS)
  (COND
    ((OR (NULL STATEMENTS) (NULL (CDR STATEMENTS))) NIL)
    ((LISTP (CAR STATEMENTS)) 
     (APPEND (SEARCH-METHOD-CALLS (CAR STATEMENTS))
             (SEARCH-METHOD-CALLS (CDR STATEMENTS))))
    ((EQUAL "(" (CADR STATEMENTS))
     (IF (EQUAL 'IDENTIFIER (TOKEN-KIND (CAR STATEMENTS)))
         (CONS (CAR STATEMENTS) (SEARCH-METHOD-CALLS (CDR STATEMENTS)))
         (SEARCH-METHOD-CALLS (CDR STATEMENTS))))
    (T (SEARCH-METHOD-CALLS (CDR STATEMENTS)))))


(DEFMETHOD CALLED-METHODS ((SELF C++BODY))
  (SEARCH-METHOD-CALLS (STATEMENTS SELF)))

;;----------------------------------------------------------------------
;; Some methods are merely functions, that is methods without a class.
;; In that case: (null (c++class-name method))

(DEFCLASS C++METHOD ()
  ((HEADER  :ACCESSOR HEADER :INITFORM NIL)
   (BODY    :ACCESSOR BODY   :INITFORM NIL)))

(DEFMETHOD PARSE ((SELF C++METHOD) (FILTER TOKEN-FILTER))
  (SETF (HEADER SELF) (MAKE-INSTANCE 'C++HEADER))
  (SETF (BODY SELF)   (MAKE-INSTANCE 'C++BODY))
  (AND (PARSE (HEADER SELF) FILTER) (PARSE (BODY SELF) FILTER)))
;;SEE: We should have a TokenFilter class to test here for a "{" token.

(DEFMETHOD C++CLASS-NAME ((SELF C++METHOD))
  (C++CLASS-NAME (HEADER SELF)))

(DEFMETHOD NAME ((SELF C++METHOD))
  (C++METHOD-NAME (HEADER SELF)))

(DEFMETHOD RES-TYPE ((SELF C++METHOD))
  (RES-TYPE (HEADER SELF)))

(DEFMETHOD ARGUMENTS ((SELF C++METHOD))
  (ARGUMENTS (HEADER SELF)))

(DEFMETHOD CALLED-METHODS ((SELF C++METHOD))
  (CALLED-METHODS (BODY SELF)))

;;----------------------------------------------------------------------
(DEFCLASS C++CLASS ()
  ((METHODS :ACCESSOR METHODS :INITFORM NIL)))

(DEFMETHOD PARSE ((SELF C++CLASS) (FILTER TOKEN-FILTER))
  (LET ((M (MAKE-INSTANCE 'C++METHOD)))
    (when (PARSE M FILTER)
      (ADD-C++METHOD SELF M)
      (PARSE SELF FILTER))))

(DEFMETHOD ADD-C++METHOD ((SELF C++CLASS) METHOD)
  (push METHOD (METHODS SELF)))

;;----------------------------------------------------------------------

(DEFCLASS C++PROGRAM ()
  ((METHODS  :ACCESSOR METHODS  :INITFORM NIL)
   (DOTRACE  :ACCESSOR DOTRACE  :INITFORM NIL)))

(DEFMETHOD PARSE ((SELF C++PROGRAM) FILE-NAME-LIST)
  (COND
    ((STRINGP FILE-NAME-LIST)  (PARSE SELF (LIST FILE-NAME-LIST)))
    ((NULL FILE-NAME-LIST)     T)
    (T (LET (
             (SOURCE            (MAKE-INSTANCE 'FILE-FILTER))
             (LASOURCE          (MAKE-INSTANCE 'LOOK-AHEAD-CHAR-FILTER))
             (TOKENS            (MAKE-INSTANCE 'C++TOKEN-FILTER))
             (SKIP-CCOMMENTS    (MAKE-INSTANCE 'CCOMMENT-FILTER))
             (SKIP-C++COMMENTS  (MAKE-INSTANCE 'C++COMMENT-FILTER))
             (PREPROCESS        (MAKE-INSTANCE 'CPREPROCESSOR-FILTER))
             (NONLTOKENS        (MAKE-INSTANCE 'C++NONLTOKEN-FILTER))
             (ANALYSIS          (MAKE-INSTANCE 'LOOK-AHEAD-TOKEN-FILTER)))

         (SET-FILE       SOURCE           (OPEN (CAR FILE-NAME-LIST)))
         (PUSH-ON-FILTER LASOURCE         SOURCE)
         (SET-SOURCE     TOKENS           LASOURCE)
         (PUSH-ON-FILTER SKIP-CCOMMENTS   TOKENS)
         (PUSH-ON-FILTER SKIP-C++COMMENTS SKIP-CCOMMENTS)
         (PUSH-ON-FILTER PREPROCESS       SKIP-C++COMMENTS)
         (PUSH-ON-FILTER NONLTOKENS       PREPROCESS)
         (PUSH-ON-FILTER ANALYSIS         NONLTOKENS)
         (SETF (DOTRACE ANALYSIS) (DOTRACE SELF))
         (LOOP until (EQUAL 'EOF (NEXT-TOKEN ANALYSIS)) DO
              (LET ((METHOD (MAKE-INSTANCE 'C++METHOD)))
                (IF (PARSE METHOD ANALYSIS)
                    (PROGN
                      (IF (DOTRACE SELF)
                          (FORMAT T "~%--------------------------~%"))
                      (ADD-C++METHOD SELF METHOD)
                      (FORMAT T "Added method ~a::~a~%"
                              (C++CLASS-NAME METHOD) (NAME METHOD))
                      (when (DOTRACE SELF)
                        (FORMAT T "~%--------------------------~%")))
                    (PROGN
                      (when (DOTRACE SELF)
                        (FORMAT T "~%--------------------------~%"))
                      (FORMAT T "Could not parse a method. ")
                      (FORMAT T "Bad tokens:~a~%" 
                              (BAD-TOKEN-LIST (HEADER METHOD)))
                      (when (DOTRACE SELF)
                        (FORMAT T "~%--------------------------~%")))))))
       (PARSE SELF (CDR FILE-NAME-LIST)))))

(DEFMETHOD ADD-C++METHOD ((SELF C++PROGRAM) METHOD)
  (push METHOD (METHODS SELF)))

(DEFUN SEARCH-UNIF-METH-NAMED (NAME UMLIST)
  (COND
    ((NULL UMLIST) NIL)
    ((EQUAL NAME (NAME (CAAR UMLIST))) UMLIST)
    (T (SEARCH-UNIF-METH-NAMED NAME (CDR UMLIST)))))

(DEFMETHOD UNIFY-METHODS-BY-NAME ((SELF C++PROGRAM))
  (LET ((UMLIST NIL) (UM NIL))
    (DO ((METH (METHODS SELF) (CDR METH)))
        ((NULL METH) UMLIST)
      (SETQ UM (SEARCH-UNIF-METH-NAMED (NAME (CAR METH)) UMLIST))
      (if (NULL UM)
          (SETQ UMLIST (CONS (LIST (CAR METH)) UMLIST))
          (RPLACA UM (CONS (CAR METH) (CAR UM)))))))

(DEFMETHOD BUILD-METHOD-CALL-GRAF ((SELF C++PROGRAM))
  (LET ( (UNIF-METH-LIST (UNIFY-METHODS-BY-NAME SELF))
        (G (MAKE-INSTANCE 'GRAF)))
    (ADD-NODES G UNIF-METH-LIST)
    (DO ((UNIFMETH UNIF-METH-LIST (CDR UNIFMETH)))
        ((NULL UNIFMETH) G)
      (DO ((METH (CAR UNIFMETH) (CDR METH)))
          ((NULL METH) NIL)
        (DO ((NAME (CALLED-METHODS (CAR METH)) (CDR NAME)))
            ((NULL NAME) NIL)
          (ADD-EDGE
           G (LIST (CAR UNIFMETH)
                   (CAR (SEARCH-UNIF-METH-NAMED (CAR NAME)
                                                UNIF-METH-LIST)))))))))

(DEFMETHOD PRINT-C++METHOD-NAMES ((SELF C++PROGRAM))
  (DO ((METH (METHODS SELF) (CDR METH)))
      ((NULL METH) NIL)
    (FORMAT T "~a::~a~%" (C++CLASS-NAME (CAR METH)) (NAME (CAR METH)))))

;;----------------------------------------------------------------------

(DEFUN BUILD-C++METHOD-NAME-LIST (MLIST)
  (IF (NULL MLIST)
      NIL
      (CONS
       (CONCATENATE 'STRING (C++CLASS-NAME (CAR MLIST)) "::" (NAME (CAR MLIST)))
       (BUILD-C++METHOD-NAME-LIST (CDR MLIST)))))

(DEFUN BUILD-UNIF-METH-NAME-LIST (UMLIST)
  (IF (NULL UMLIST)
      NIL
      (CONS (BUILD-C++METHOD-NAME-LIST (CAR UMLIST))
            (BUILD-UNIF-METH-NAME-LIST (CDR UMLIST)))))

(DEFUN NAME-METHODS (L)
  (COND
    ((NULL L) NIL)
    ((LISTP L) (CONS (NAME-METHODS (CAR L)) (NAME-METHODS (CDR L))))
    (T (CONCATENATE 'STRING (C++CLASS-NAME L) "::" (NAME L)))))

(DEFUN NODE-NAMED (G N)
  (CAR (SEARCH-UNIF-METH-NAMED N (NODES G))))

;;----------------------------------------------------------------------


#||
(package:load-package "COM.INFORMATIMAGO.COMMON-LISP.CXX")
(use-package          "COM.INFORMATIMAGO.COMMON-LISP.CXX")

(setq source (make-instance 'File-Filter))
(set-File source (open "/home/pascal/firms/bauhaus/hermstedt/cme_stutel/generic/CME.cpp"))

(set-File source (open "/home/pascal/firms/hbedv/src-HEAD/avmailgate/avmailgate/bcstring.h"))


(setq skipccomments (make-instance 'CCommentFilter))
(push-on-filter skipccomments source)

(setq skipcxxcomments (make-instance 'CxxCommentFilter))
(push-on-filter skipcxxcomments skipccomments)

(setq preprocess (make-instance 'CPreprocessorFilter))
(push-on-filter preprocess skipcxxcomments)

(setq lookahead (make-instance 'LookaheadFilter))
(push-on-filter lookahead preprocess)

(setq analysis lookahead)

(setq p (make-instance 'CxxProgram))
(parse p (mapcar
          (lambda (file)
            (sconc "/home/pascal/firms/bauhaus/hermstedt/cme_stutel/" file))
          '("generic/AL.cpp"
            "generic/Buffer.cpp"
            "generic/CME.cpp"
            "generic/CME_Result.cpp"
            "generic/Directory.cpp"
            "generic/EmptyBuffer.cpp"
            "generic/EmptyTLVBuffer.cpp"
            "generic/File.cpp"
            "generic/FtpReason.cpp"
            "generic/FullBuffer.cpp"
            "generic/FullTLVBuffer.cpp"
            "generic/ISDN_Cause.cpp"
            "generic/Item.cpp"
            "generic/List.cpp"
            "generic/PCI_Status.cpp"
            "generic/StutelReason.cpp"
            "generic/TLV_Buffer.cpp"
            "generic/TOrigin.cpp"
            "generic/TResult.cpp"
            "generic/VCO.cpp"
            "generic/V_PRIMITIVE.cpp"
            "generic/X213_CauseOrigin.cpp"
            "generic/X25_CauseDiag.cpp"
            "generic/debug.cpp"
            "generic/util.cpp"
            "acme/AnticipationWindow.cpp"
            "acme/FileHeader.cpp"
            "acme/NAF.cpp"
            "acme/N_SDU.cpp"
            "acme/PCIMessage.cpp"
            "acme/PciAL.cpp"
            "acme/SBV_Command.cpp"
            "acme/StutelCME.cpp"
            "acme/StutelVCO.cpp"
            "acme/T_DU.cpp"
            "acme/T_Response_neg.cpp"
            "acme/T_Response_pos.cpp"
            ))) 
(print (methods p))

||#

;;;; cxx.lisp                         --                     --          ;;;;
