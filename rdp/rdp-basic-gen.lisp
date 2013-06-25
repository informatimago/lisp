;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rdp-basic-gen.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A (pseudo) basic generator for the recusive descent parser generator.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-07-19 <PJB> Updated for new rdp.
;;;;    2006-09-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2012
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.RDP")


(defparameter *linenum* 0)

(defun emit (fctrl &rest args)
  (format t "~&~4D " (incf *linenum* 10))
  (apply (function format) t fctrl args))


(defmethod generate-boilerplate ((target (eql :basic)) grammar &key (trace nil))
  (declare (ignore trace))
  `(progn
    (emit "SCANSRC$=\"\" : SCANFUN$=\"\" : SCANPOS=0")
    (emit "CURTOK$=\"\"  : CURTXT$=\"\"  : CURPOS=0")
    (emit "SPACES$=" "+CHR$(10)+CHR$(13)+CHR$(9)")
    (emit "DEF SCANEOF : IF LEN(SCANSRC$)<=SCANPOS THEN RETURN 1 ELSE RETURN 0 : ENDFUN")
    (emit "SUB ACCEPT")
    (emit "  IF TOKEN$ <> CURTOK$ THEN")
    (emit "     PRINT \"ERROR: AT POSITION\",CURPOS,\"EXPECTED \",TOKEN$,\" NOT \",CURTOK$")
    (emit "     STOP")
    (emit "  ELSE")
    (emit "     ACCEPTOK$=CURTOK$:ACCEPTXT$=CURTXT$:ACCEPPOS$=CURPOS$")
    (emit "     CALL SCANFUN$")
    (emit "  ENDIF")
    (emit "ENDSUB")
    (emit "MAXCONS=100000")
    (emit "NIL=0:CONS=1:STRING=2:NUMBER=3")
    (emit "TYPELABEL$[NIL]=\"NIL\"")
    (emit "TYPELABEL$[CONS]=\"CONS\"")
    (emit "TYPELABEL$[STRING]=\"STRING\"")
    (emit "TYPELABEL$[NUMBER]=\"NUMBER\"")
    (emit "DIM TYPES[MAXCONS],CAR[MAXCONS],CDR[MAXCONS],STRINGS$[MAXCONS],NUMBERS[MAXCONS]")
    (emit "TYPES[NIL]=NIL:CAR[NIL]=NIL:CDR[NIL]=NIL:STRINGS$[NIL]=\"NIL\":NUMBERS[NIL]=0")
    (emit "FREE=MAXCONS")
    (emit "SUB CONS")
    (emit "  IF FREE<=1 THEN PRINT \"ERROR: OUT OF CONS SPACE\" : STOP : ENDIF")
    (emit "  FREE=FREE-1")
    (emit "  TYPES[FREE]=CONS")
    (emit "  CAR[FREE]=NCAR")
    (emit "  CDR[FREE]=NCDR")
    (emit "  RES=FREE")
    (emit "ENDSUB")
    (emit "SUB MKSTR")
    (emit "  IF FREE<=1 THEN PRINT \"ERROR: OUT OF CONS SPACE\" : STOP : ENDIF")
    (emit "  FREE=FREE-1")
    (emit "  TYPES[FREE]=STRING")
    (emit "  STRING$[FREE]=NSTRING$")
    (emit "  RES=FREE")
    (emit "ENDSUB")
    (emit "SUB MKNUM")
    (emit "  IF FREE<=1 THEN PRINT \"ERROR: OUT OF CONS SPACE\" : STOP : ENDIF")
    (emit "  FREE=FREE-1")
    (emit "  TYPES[FREE]=NUMBER")
    (emit "  NUMBER[FREE]=NNUMBER")
    (emit "  RES=FREE")
    (emit "ENDSUB")
    (emit "SUB REVERSE")
    (emit "  REV=0:TREV=NIL")
    (emit "  WHILE LIST<>0")
    (emit "   IF TYPES[LIST]<>CONS THEN")
    (emit "      PRINT \"ERROR: REVERSE EXPECTS A LIST, NOT A \",TYPELABEL$[TYPES[LIST]]")
    (emit "      STOP")
    (emit "    ELSE")
    (emit "      NEW=CDR[LIST]")
    (emit "      CDR[LIST]=REV:TYPES[LIST]=TREV")
    (emit "      REV=LIST:TREV=CONS")
    (emit "      LIST=NEW")
    (emit "    ENDIF")
    (emit "  ENDWHILE")
    (emit "  RES=REV")
    (emit "ENDSUB")))


(defmethod gen-scanner-function-name ((target (eql :basic)) grammar-name)
   (format nil "SCAN~A" grammar-name))


(defmethod generate-scanner ((target (eql :basic)) grammar &key (trace nil))
  (let* ((an-terminals  (sort (remove-if-not
                               (lambda (item)
                                 (and (stringp item)
                                      (alphanumericp (aref item (1- (length item))))))
                               (grammar-all-terminals grammar))
                              (function >) :key (function length)))
         (nan-terminals (sort (remove-if
                               (lambda (item)
                                 (or (not (stringp item))
                                     (alphanumericp (aref item (1- (length item))))))
                               (grammar-all-terminals grammar))
                              (function >) :key (function length)))
         (nl-terminals (remove-if (function stringp) (grammar-terminals grammar)))
         (lit-an-terminals-regexp
          (format nil "^(~{~A~^|~})([^A-Za-z0-9]|$)"
                  (mapcar (function regexp-quote-extended) an-terminals)))
         (lit-nan-terminals-regexp
          (format nil "^(~{~A~^|~})"
                  (mapcar (function regexp-quote-extended)  nan-terminals)))
         (fname (gen-scanner-function-name target (grammar-name grammar))))
    `(progn
       (emit "SUB ~A" ',fname)
       ,@(when trace `((emit "PRINT \"> ~A\"" ',(symbol-name fname))))
       (emit "  WHILE POS(SCANSRC$[SCANPOS],SPACES$)>0 : SCANPOS=SCANPOS+1 : ENDWHILE")
       (emit "  CURPOS=SCANPOS")
       (emit "  IF SCANEOF<>0 THEN")
       (emit "    SCANPOS=LEN(SCANSRC$)")
       (emit "    SCANTXT$=\"<END OF SOURCE>\"")
       (emit "    SCANTOK$=\"\"")
       (emit "  ELSE")
       (emit "    REM *** ASSUMING THERE IS SOME WAY TO MATCH REGEXPS IN BASIC...")
       (when an-terminals
         (emit "    MATCHREGEXP  \"~A\" SCANSRC$,SCANPOS INTO START,END" ',lit-an-terminals-regexp)
         (emit "    IF START>0 THEN")
         (emit "      SCANPOS=END")
         (emit "      SCANTXT$=MID$(SCANSRC$,START,END-1)")
         (emit "      SCANTOK$=SCANTXT$")
         (emit "    ELSE"))
       (when nan-terminals
         (emit "      MATCHREGEXP  \"~A\" SCANSRC$,SCANPOS INTO START,END" ',lit-nan-terminals-regexp)
         (emit "      IF START>0 THEN")
         (emit "        SCANPOS=END")
         (emit "        SCANTXT$=MID$(SCANSRC$,START,END)")
         (emit "        SCANTOK$=SCANTXT$")
         (emit "      ELSE"))
       ,@(labels ((gen (terminals)
                       (if (null terminals)
                           `( (emit "       PRINT \"ERROR: AT POSITION\",CURPOS,\"EXPECTED \",TOKEN$,\" NOT \",CURTOK$")
                              (emit "       STOP"))
                           (let ((terminal (car terminals)))
                             `(
                               (emit "   MATCHREGEXP \"^(~A)\" SCANSRC$,SCANPOS INTO START,END" ',(second terminal))
                               (emit "   IF START>0 THEN")
                               (emit "        SCANPOS=END")
                               (emit "        SCANTXT$=MID$(SCANSRC$,START,END)")
                               (emit "        SCANTOK$=\"~A\"" ',(first terminal))
                               (emit "   ELSE")
                               ,@(gen (cdr terminals))
                               (emit "   ENDIF"))))))
                 (gen  (grammar-terminals grammar)))
       (when nan-terminals (emit "      ENDIF"))
       (when an-terminals  (emit "    ENDIF"))
       (emit "  ENDIF")
       ,@(when trace `((emit "PRINT \"< ~A\"" ',(symbol-name fname))))
       (emit "ENDSUB"))))


(defmethod gen-parse-function-name ((target (eql :basic)) grammar non-terminal)
   (format nil "PARSE~A" non-terminal))

(defmethod gen-in-firsts ((target (eql :basic)) firsts)
  (format nil "(~{CURTOK$=\"~A\"~^ OR ~})"  firsts))


(defparameter *lex* 0)

(defmethod gen-parsing-statement ((target (eql :basic)) grammar item)
  (if (atom item)
      (if (terminalp grammar item)
          `(emit "TOKEN$=~S : CALL ACCEPT" ',(string item))
          (let* ((firsts (first-set grammar item))
                 (emptyp (member nil firsts)))
            `(progn
               (emit "IF ~A THEN" ',(gen-in-firsts target (remove nil firsts)))
               (emit "  CALL ~A" ',(gen-parse-function-name target grammar item))
               (emit "ELSE")
               ,(if emptyp
                    `(emit "  RET=NIL")
                    `(progn
                       (emit "  PRINT \"ERROR: UNEXPECTED TOKEN \",SCANTOK$")
                       (emit "  STOP")))
               (emit "ENDIF"))))
      (ecase (car item)
        ((seq)
         (destructuring-bind (seq items actions) item
           (let ((index 0)
                 (lex (incf *lex*)))
             `(progn
                ,@(mapcar (lambda (item)
                            `(progn
                               ,(gen-parsing-statement target grammar item)
                               (emit "L~DA~D=RES" ,lex ,(incf index))))
                          items)
                ,@(loop
                     :for prev = "NIL" :then "A0"
                     :for i :from index :downto 1
                     :collect
                     `(emit "A~D=L~DA~D:NCAR=A~D:NCDR=~A:CALL CONS:A0=RES"
                            ,i ,lex ,i ,i ,prev))
                ,@(mapcar (lambda (act) `(emit "~A" ',act)) actions)))))
        ((rep)
         (let ((lex (incf *lex*)))
           `(progn
              (emit "L~DRES=NIL" ,lex)
              (emit "WHILE ~A"
                    ',(gen-in-firsts target (first-set grammar (second item))))
              ,(gen-parsing-statement target grammar (second item))
              (emit "NCAR=RET:NCDR=L~DRES:CALL CONS:L~DRES=RES" ,lex ,lex)
              (emit "ENDWHILE")
              (emit "LIST=L~DRES:CALL REVERSE" ,lex))))
        ((opt)
         (let ((lex (incf *lex*)))
           `(progn
              (emit "L~DRES=NIL" ,lex)
              (emit "IF ~A THEN"
                    ',(gen-in-firsts target (first-set grammar (second item))))
              ,(gen-parsing-statement target grammar (second item))
              (emit "ELSE")
              (emit "  RES=NIL")
              (emit "ENDIF"))))
        ((alt)
         (labels ((gen (items)
                    (if (null items)
                        `(progn
                           (emit "PRINT \"ERROR: DID NOT EXPECT \",CURTOK$")
                           (emit "STOP"))
                        `(progn
                           (emit "IF ~A THEN"
                                 ',(gen-in-firsts target (first-set grammar (car items))))
                           ,(gen-parsing-statement target grammar (car items))
                           (emit "ELSE")
                           ,(gen (cdr items))
                           (emit "ENDIF")))))
           (gen (second item)))))))


(defmethod generate-nt-parser ((target (eql :basic)) grammar non-terminal &key (trace nil))
  (let ((fname (gen-parse-function-name target grammar non-terminal)))
    `(progn
       (emit "SUB ~A" ',fname)
       ,@(when trace `((emit "PRINT \"> ~A\"" ',(symbol-name fname))))
       ,(gen-parsing-statement target grammar (find-rule grammar non-terminal))
       ,@(when trace `((emit "PRINT \"< ~A\"" ',(symbol-name fname))))
       (emit "ENDSUB"))))


(defmethod generate-parser ((target (eql :basic)) grammar &key (trace nil))
  (let ((scanner-function (gen-scanner-function-name target (grammar-name grammar)))
        (fname (gen-parse-function-name target grammar (grammar-name grammar))))
    `(progn
       (emit "SUB ~A" ',fname)
       ,@(when trace `((emit "PRINT \"> ~A\"" ',(symbol-name fname))))
       (emit "  SCANSRC$=SOURCE$ : SCANPOS=0 : SCANFUN$=\"~A\"" ',scanner-function)
       (emit "  CALL SCANFUN$")
       (emit "  CALL ~A" ',(gen-parse-function-name target grammar (grammar-start grammar)))
       (emit "  IF SCANEOF<>0 THEN")
       (emit "    PRINT \"ERROR: END OF SOURCE NOT REACHED\"")
       (emit "    STOP")
       (emit "  ENDIF")
       ,@(when trace `((emit "PRINT \"< ~A\"" ',(symbol-name fname))))
       (emit "ENDSUB"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; THE END ;;;;
