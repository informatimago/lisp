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

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.CXX.CXX"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH" )
  (:export "BUILD-METHOD-CALL-GRAF" "PARSE" "C++PROGRAM")
  (:documentation
   "Parsing C++ sources.
This is a restricted parser, used just to analyze
the call graph of C++ functions and methods."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.CXX.CXX")




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


(defgeneric set-file (self f))
(defgeneric read-a-char (self))
(defgeneric dump (self))
(defgeneric do-quotes (self flag))
(defgeneric set-source (self input))
(defgeneric push-on-filter (self f))
(defgeneric read-a-token (self))
(defgeneric parse-statement-list (self filter))
(defgeneric c++class-name (self))
(defgeneric name (self))
(defgeneric res-type (self))
(defgeneric arguments (self))
(defgeneric called-methods (self))
(defgeneric parse (self file-name-list))
(defgeneric add-c++method (self method))
(defgeneric unify-methods-by-name (self))
(defgeneric build-method-call-graf (self))
(defgeneric print-c++method-names (self))

;;----------------------------------------------------------------------

(defclass char-filter ()
  ((previous :accessor previous :initform nil)))

(defmethod push-on-filter ((self char-filter) f)
  (setf (previous self) f))

(defmethod read-a-char ((self char-filter))
  (if (null (previous self))
      'eof
      (read-a-char (previous self))))

(defmethod dump ((self char-filter))
  (let ((c (read-a-char self)))
    (loop while (not (equal 'eof c)) do
         (write-char c)
         (setq c (read-a-char self)))))

;;----------------------------------------------------------------------

(defclass file-filter (char-filter)
  ((file  :accessor file :initform nil)))

(defmethod set-file ((self file-filter) f)
  (setf (file self) f))

(defmethod read-a-char ((self file-filter))
  (if (null (file self))
      'eof
      (read-char (file self) nil 'eof)))

;;----------------------------------------------------------------------

(defclass look-ahead-char-filter (char-filter)
  ((next-char :accessor next-char :initform nil)))

(defmethod push-on-filter :before ((self look-ahead-char-filter) f)
  (declare (ignore f))
  (setf (next-char self) nil))

(defmethod read-a-char ((self look-ahead-char-filter))
  (if (null (previous self))
      'eof
      (let ((res nil))
        (if (null (next-char self))
            (setf (next-char self) (read-a-char (previous self))))
        (setq res (next-char self))
        (setf (next-char self) (read-a-char (previous self)))
        res)))

;;----------------------------------------------------------------------

(defclass token-filter ()
  ((previous :accessor previous :initform nil)))

(defmethod do-quotes ((self token-filter) flag)
  (if (null (previous self))
      nil
      (do-quotes (previous self) flag)))

(defmethod push-on-filter ((self token-filter) (f token-filter))
  (setf (previous self) f))

(defmethod read-a-token ((self token-filter))
  (if (null (previous self))
      'eof
      (read-a-token (previous self))))

(defmethod dump ((self token-filter))
  (let ((c (read-a-token self)))
    (loop while (not (equal 'eof c)) do
         (print c)
         (setq c (read-a-token self)))))

;;----------------------------------------------------------------------

(defun char-kind (c)
  (cond
    ((equal c 'eof)                                            'eof)
    ((or (char= c (code-char 9))  (char= c (character " ")))   'space)
    ((or (char= c (code-char 10)) (char= c (code-char 13)))    'new-line)
    ((or (char= (character "_") c)
         (char= (character "~") c)
         (alpha-char-p c))                                     'letter)
    ((digit-char-p c)                                          'digit)
    ((or (char= (character "'") c) (char= (character "\"") c)) 'quote)
    (t                                                         'special)))

(defun token-member (tok liste)
  (cond
    ((null liste) nil)
    ((equal tok (car liste)) t)
    (t (token-member tok (cdr liste)))))

(defun token-kind (tok)
  (let* ((c (char tok 0)) (kind (char-kind c)))
    (cond
      ((equal kind 'letter)
       (if (token-member
            tok 
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
           'keyword
           'identifier))
      ((equal kind 'digit)
       'number)
      ((equal kind 'quote)
       'string)
      ((equal kind 'new-line)
       'new-line)
      (t
       'special))))

;;----------------------------------------------------------------------

(defclass c++token-filter (token-filter)
  ((source   :accessor source   :initform nil)
   (doquotes :accessor doquotes :initform t)
   (dotrace  :accessor dotrace  :initform nil)))

(defmethod do-quotes ((self c++token-filter) flag)
  (let ((old (doquotes self)))
    (setf (doquotes self) flag)
    old))

(defmethod set-source ((self c++token-filter) (input look-ahead-char-filter))
  (setf (source self) input))

(defmethod read-a-token ((self c++token-filter))
  ;; * '::' ( ) { } ;; '->'  "<char>*" CR '/*' '*/' '//'
  (let ((r (let ((s nil) (c (read-a-char (source self))))
             ;; skip spaces;; note we don't skip new-lines here.
             (loop while (equal (char-kind c) 'space) do
                  (setq c (read-a-char (source self))))
             (if (equal 'eof c)
                 'eof
                 (let ((kind (char-kind c)))
                   (setq s (cons c s))
                   (cond
                     ((equal kind 'letter)
                      (setq c (next-char (source self)))
                      (setq kind (char-kind c))
                      (loop while (or (equal kind 'letter) 
                                      (equal kind 'digit)) do
                           (setq c (read-a-char (source self)))
                           (setq s (cons c s))
                           (setq c (next-char (source self)))
                           (setq kind (char-kind c))))
                     ((equal kind 'digit)
                      (setq c (next-char (source self)))
                      (setq kind (char-kind c))
                      (loop while (or (equal kind 'digit)
                                      (equal c (character ".")) 
                                      (and (char<= (character "a") c)
                                           (char<= c (character "g")))
                                      (and (char<= (character "A") c)
                                           (char<= c (character "G")))
                                      (equal c (character "x"))
                                      (equal c (character "X"))
                                      ) do
                           (setq c (read-a-char (source self)))
                           (setq s (cons c s))
                           (setq c (next-char (source self)))
                           (setq kind (char-kind c))))
                     ((and (doquotes self) (equal kind 'quote))
                      (let ((term c))
                        (setq c (read-a-char (source self)))
                        (loop while (not (or (equal 'eof c)
                                             (equal (char-kind c) 'new-line)
                                             (equal term c))) do
                             (setq s (cons c s))
                             (if (char= (character "\\") c)
                                 (progn
                                   (setq c (read-a-char 
                                            (source self)))
                                   (setq s (cons c s))))
                             (setq c (read-a-char (source self))))
                        (if (equal term c)
                            (setq s (cons c s)))))
                     ((equal kind 'new-line)
                      (setq c (next-char (source self)))
                      (loop while (equal (char-kind c) 'new-line) do
                           (setq c (read-a-char (source self)))
                           (setq s (cons c s))
                           (setq c (next-char (source self)))))
                     ((char= (character "-") c)
                      (if (char= (character ">")
                                 (next-char (source self)))
                          (progn
                            (setq c (read-a-char (source self)))
                            (setq s (cons c s)))))
                     ((char= (character "/") c)
                      (if (or (char= (character "/")
                                     (next-char (source self)))
                              (char= (character "*")
                                     (next-char (source self))))
                          (progn
                            (setq c (read-a-char (source self)))
                            (setq s (cons c s)))))
                     ((char= (character "*") c)
                      (if (char= (character "/")
                                 (next-char (source self)))
                          (progn
                            (setq c (read-a-char (source self)))
                            (setq s (cons c s)))))
                     ((char= (character ":") c)
                      (if (char= (character ":")
                                 (next-char (source self)))
                          (progn
                            (setq c (read-a-char (source self)))
                            (setq s (cons c s))))))
                   (concatenate 'string (reverse s)))))))
    (if (dotrace self)
        (format t "~a " r))
    r))

;;----------------------------------------------------------------------

(defclass c++nonltoken-filter (c++token-filter)
  ())

(defmethod read-a-token ((self c++nonltoken-filter))
  (let ((tok (read-a-token (previous self))))
    (cond
      ((equal tok 'eof)                     'eof)
      ((equal (token-kind tok) 'new-line)   (read-a-token self))
      (t                                    tok))))

;;----------------------------------------------------------------------

(defclass look-ahead-token-filter (token-filter)
  ((dotrace    :accessor dotrace    :initform nil)
   (next-token :accessor next-token :initform nil)))

(defmethod push-on-filter :before ((self look-ahead-token-filter) (f token-filter))
  (declare (ignore f))
  (setf (next-token self) nil))

(defmethod read-a-token ((self look-ahead-token-filter))
  (if (null (previous self))
      'eof
      (let ((res nil))
        (if (null (next-token self))
            (setf (next-token self) (read-a-token (previous self))))
        (setq res (next-token self))
        (setf (next-token self) (read-a-token (previous self)))
        (if (dotrace self)
            (format t "~a " res))
        res)))

;;----------------------------------------------------------------------

(defun skip-comment (self start-string stop-lambda)
  (if (null (previous self))
      'eof
      (let ((cur-token (read-a-token (previous self))))
        (loop while (equal cur-token start-string) do
             (let ((saved (do-quotes self nil)))
               (setq cur-token (read-a-token (previous self)))
               (loop while (not (or (equal cur-token 'eof)
                                    (apply stop-lambda (list cur-token))))
                  do (setq cur-token (read-a-token (previous self))))
               (do-quotes self saved)
               (unless (equal cur-token 'eof)
                 (setq cur-token (read-a-token (previous self))))))
        cur-token)))

;;----------------------------------------------------------------------

(defclass cpreprocessor-filter (token-filter)
  ())

(defmethod read-a-token ((self cpreprocessor-filter))
  (skip-comment self "#" (lambda (x) (equal (token-kind x) 'new-line))))

;;----------------------------------------------------------------------

(defclass ccomment-filter (token-filter) ())

(defmethod read-a-token ((self ccomment-filter))
  (skip-comment self "/*" (lambda (x) (equal x "*/"))))

;;----------------------------------------------------------------------

(defclass c++comment-filter (token-filter) ())

(defmethod read-a-token ((self c++comment-filter))
  (skip-comment self "//" (lambda (x) (equal (token-kind x) 'new-line))))

;;----------------------------------------------------------------------

(defclass c++header ()
  ((res-type         :accessor res-type         :initform nil)
   (c++class-name    :accessor c++class-name    :initform nil)
   (c++method-name   :accessor c++method-name   :initform nil)
   (arguments        :accessor arguments        :initform nil)
   (header-kind      :accessor header-kind      :initform nil)
   (bad-token-list   :accessor bad-token-list   :initform nil)))

(defun range (s from end) (subseq s from end))

(defmethod parse ((self c++header) (filter token-filter))
  (let ((l nil) (tok (read-a-token filter)) (i 0))
    (loop while (not (or 
                      (equal tok 'eof)
                      (equal tok ")")
                      (equal tok ";;"))) do
         (setq l (cons tok l))
         (setq tok (read-a-token filter)))
    (when (not (equal tok 'eof))
      (setq l (cons tok l)))
    (setq l (reverse l))
    (setq i (search "::" l))
    (if (null i)
        (progn
          (setf (header-kind self) 'function)
          (setq i (search "(" l))
          (if (or (null i) (= 0 i))
              (progn
                (setf (bad-token-list self) l)
                nil)
              (progn
                (setf (c++class-name self)  nil)
                (setf (res-type self)       (range l 0 (- i 2)))
                (setf (c++method-name self) (car (range l (1- i) (1- i))))
                (setf (arguments self)  (range l i nil))
                t)))
        (progn
          (setf (header-kind self) 'method)
          (setf (c++class-name self) (car (range l (1- i) (1- i))))
          (setf (res-type self)      (range l 0 (- i 2)))
          (if (equal (nth (1+ i) l) "~") 
              (progn
                (setf (c++method-name self) (car (range l (1+ i) (+ i 2))))
                (setf (arguments self)  (range l (+ i 3) nil)))
              (progn
                (setf (c++method-name self) (car (range l (1+ i) (1+ i))))
                (setf (arguments self)  (range l (+ i 2) nil))))
          t))))

;;----------------------------------------------------------------------
;; body          ::= '{' { statement | body } '}' .
;; statement     ::= { token } 

(defclass c++body ()
  ((initializer :accessor initializer :initform nil)
   (statements  :accessor statements  :initform nil)))

(defmethod parse ((self c++body) (filter token-filter))
  (let ((tok (read-a-token filter)) (i nil))
    (if (equal tok ":")
        (loop while (not (token-member tok '("{" "}" 'eof))) do
             (setq i (cons tok i))
             (setq tok (read-a-token filter))))
    (setf (initializer self) (reverse i))
    (if (equal tok "{")
        (block nil
          (setf (statements self) (parse-statement-list self filter))
          t)
        nil)))

(defmethod parse-statement-list ((self c++body) (filter token-filter))
  (let ((tok (read-a-token filter)) (s nil))
    (loop while (not (or (equal 'eof tok) (equal tok "}"))) do
         (if (equal tok "{")
             (setq s (cons (parse-statement-list self filter) s))
             (setq s (cons tok s)))
         (setq tok (read-a-token filter)))
    (if (equal tok "}")
        (reverse s)
        nil)))

(defun search-method-calls (statements)
  (cond
    ((or (null statements) (null (cdr statements))) nil)
    ((listp (car statements)) 
     (append (search-method-calls (car statements))
             (search-method-calls (cdr statements))))
    ((equal "(" (cadr statements))
     (if (equal 'identifier (token-kind (car statements)))
         (cons (car statements) (search-method-calls (cdr statements)))
         (search-method-calls (cdr statements))))
    (t (search-method-calls (cdr statements)))))


(defmethod called-methods ((self c++body))
  (search-method-calls (statements self)))

;;----------------------------------------------------------------------
;; Some methods are merely functions, that is methods without a class.
;; In that case: (null (c++class-name method))

(defclass c++method ()
  ((header  :accessor header :initform nil)
   (body    :accessor body   :initform nil)))

(defmethod parse ((self c++method) (filter token-filter))
  (setf (header self) (make-instance 'c++header))
  (setf (body self)   (make-instance 'c++body))
  (and (parse (header self) filter) (parse (body self) filter)))
;;SEE: We should have a TokenFilter class to test here for a "{" token.

(defmethod c++class-name ((self c++method))
  (c++class-name (header self)))

(defmethod name ((self c++method))
  (c++method-name (header self)))

(defmethod res-type ((self c++method))
  (res-type (header self)))

(defmethod arguments ((self c++method))
  (arguments (header self)))

(defmethod called-methods ((self c++method))
  (called-methods (body self)))

;;----------------------------------------------------------------------
(defclass c++class ()
  ((methods :accessor methods :initform nil)))

(defmethod parse ((self c++class) (filter token-filter))
  (let ((m (make-instance 'c++method)))
    (when (parse m filter)
      (add-c++method self m)
      (parse self filter))))

(defmethod add-c++method ((self c++class) method)
  (push method (methods self)))

;;----------------------------------------------------------------------

(defclass c++program ()
  ((methods  :accessor methods  :initform nil)
   (dotrace  :accessor dotrace  :initform nil)))

(defmethod parse ((self c++program) file-name-list)
  (cond
    ((stringp file-name-list)  (parse self (list file-name-list)))
    ((null file-name-list)     t)
    (t (let (
             (source            (make-instance 'file-filter))
             (lasource          (make-instance 'look-ahead-char-filter))
             (tokens            (make-instance 'c++token-filter))
             (skip-ccomments    (make-instance 'ccomment-filter))
             (skip-c++comments  (make-instance 'c++comment-filter))
             (preprocess        (make-instance 'cpreprocessor-filter))
             (nonltokens        (make-instance 'c++nonltoken-filter))
             (analysis          (make-instance 'look-ahead-token-filter)))

         (set-file       source           (open (car file-name-list)))
         (push-on-filter lasource         source)
         (set-source     tokens           lasource)
         (push-on-filter skip-ccomments   tokens)
         (push-on-filter skip-c++comments skip-ccomments)
         (push-on-filter preprocess       skip-c++comments)
         (push-on-filter nonltokens       preprocess)
         (push-on-filter analysis         nonltokens)
         (setf (dotrace analysis) (dotrace self))
         (loop until (equal 'eof (next-token analysis)) do
              (let ((method (make-instance 'c++method)))
                (if (parse method analysis)
                    (progn
                      (if (dotrace self)
                          (format t "~%--------------------------~%"))
                      (add-c++method self method)
                      (format t "Added method ~a::~a~%"
                              (c++class-name method) (name method))
                      (when (dotrace self)
                        (format t "~%--------------------------~%")))
                    (progn
                      (when (dotrace self)
                        (format t "~%--------------------------~%"))
                      (format t "Could not parse a method. ")
                      (format t "Bad tokens:~a~%" 
                              (bad-token-list (header method)))
                      (when (dotrace self)
                        (format t "~%--------------------------~%")))))))
       (parse self (cdr file-name-list)))))

(defmethod add-c++method ((self c++program) method)
  (push method (methods self)))

(defun search-unif-meth-named (name umlist)
  (cond
    ((null umlist) nil)
    ((equal name (name (caar umlist))) umlist)
    (t (search-unif-meth-named name (cdr umlist)))))

(defmethod unify-methods-by-name ((self c++program))
  (let ((umlist nil) (um nil))
    (do ((meth (methods self) (cdr meth)))
        ((null meth) umlist)
      (setq um (search-unif-meth-named (name (car meth)) umlist))
      (if (null um)
          (setq umlist (cons (list (car meth)) umlist))
          (rplaca um (cons (car meth) (car um)))))))

(defmethod build-method-call-graf ((self c++program))
  (let ( (unif-meth-list (unify-methods-by-name self))
        (g (make-instance 'graf)))
    (add-nodes g unif-meth-list)
    (do ((unifmeth unif-meth-list (cdr unifmeth)))
        ((null unifmeth) g)
      (do ((meth (car unifmeth) (cdr meth)))
          ((null meth) nil)
        (do ((name (called-methods (car meth)) (cdr name)))
            ((null name) nil)
          (add-edge
           g (list (car unifmeth)
                   (car (search-unif-meth-named (car name)
                                                unif-meth-list)))))))))

(defmethod print-c++method-names ((self c++program))
  (do ((meth (methods self) (cdr meth)))
      ((null meth) nil)
    (format t "~a::~a~%" (c++class-name (car meth)) (name (car meth)))))

;;----------------------------------------------------------------------

(defun build-c++method-name-list (mlist)
  (if (null mlist)
      nil
      (cons
       (concatenate 'string (c++class-name (car mlist)) "::" (name (car mlist)))
       (build-c++method-name-list (cdr mlist)))))

(defun build-unif-meth-name-list (umlist)
  (if (null umlist)
      nil
      (cons (build-c++method-name-list (car umlist))
            (build-unif-meth-name-list (cdr umlist)))))

(defun name-methods (l)
  (cond
    ((null l) nil)
    ((listp l) (cons (name-methods (car l)) (name-methods (cdr l))))
    (t (concatenate 'string (c++class-name l) "::" (name l)))))

(defun node-named (g n)
  (car (search-unif-meth-named n (nodes g))))

;;----------------------------------------------------------------------


#||
(use-package          "COM.INFORMATIMAGO.COMMON-LISP.CXX.CXX")

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
