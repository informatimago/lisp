;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               parse-html.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A Simple HTML parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-02-22 <PJB> Optimized WALK for HTML-SEQ.
;;;;    2003-11-12 <PJB> Created.
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
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PARSE-HTML"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.HTML-ISO8879-1"
        "COM.INFORMATIMAGO.COMMON-LISP.PEEK-STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "COM.INFORMATIMAGO.COMMON-LISP.LIST"
        "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.STRING")
  (:EXPORT "HTML-ATTRIBUTE" "HTML-CONTENTS" "HTML-ATTRIBUTES" "HTML-TAG"
           "UNPARSE-HTML" "PARSE-HTML-STRING" "PARSE-HTML-FILE")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING" "UNSPLIT-STRING"
                "SPLIT-STRING" "STRING-REPLACE")
  (:DOCUMENTATION "This package exports functions to parse HTML pages."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PARSE-HTML")


;; ------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package "COMMON-LISP-USER")
  (package:load-package "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
  (in-package "COM.INFORMATIMAGO.COMMON-LISP.PARSE-HTML")
  (use-package "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
  (defparameter +tag-package+ (find-package "KEYWORD"))
  (DEFVAR *ATTRIBUTES* () "List of symbols of all attributes defined.")
  (DEFVAR *ELEMENTS*   () "List of symbols of all elements defined."))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (DEFMACRO DEFATTRIBUTE (ATTR-NAME ELEMENTS TYPE DEFAULT OPTIONS DOCUMENTATION)
    "
DO:       Defines an HTML attribute.
"
    (declare (ignore ATTR-NAME ELEMENTS TYPE DEFAULT OPTIONS DOCUMENTATION))
    ;; NOP
    (VALUES))
  );;eval-when

(eval-when (:compile-toplevel :load-toplevel :execute)
  (DEFSTRUCT ELEMENT
    NAME OPTIONS DOCUMENTATION))


(defun find-element (element-name) (getf *elements* element-name))


(DEFUN ELEMENT-EMPTY-P (ELEMENT-NAME)
  (and (find-element ELEMENT-NAME)
       (MEMBER :EMPTY (ELEMENT-OPTIONS (find-element ELEMENT-NAME))
               :TEST (FUNCTION EQ))))


(DEFUN ELEMENT-START-OPTIONAL-P (ELEMENT-NAME)
  (or (not (find-element ELEMENT-NAME))
      (MEMBER :START-OPTIONAL (ELEMENT-OPTIONS (find-element ELEMENT-NAME))
              :TEST (FUNCTION EQ))))


(DEFUN ELEMENT-END-OPTIONAL-P (ELEMENT-NAME)
  (or (not (find-element ELEMENT-NAME))
      (MEMBER :END-OPTIONAL (ELEMENT-OPTIONS (find-element ELEMENT-NAME))
              :TEST (FUNCTION EQ))))


(DEFUN ELEMENT-END-FORBIDDEN-P (ELEMENT-NAME)
  (and (find-element ELEMENT-NAME)
       (MEMBER :END-FORBIDDEN (ELEMENT-OPTIONS (find-element ELEMENT-NAME))
               :TEST (FUNCTION EQ))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (DEFMACRO DEFELEMENT (NAME OPTIONS &OPTIONAL (DOCUMENTATION "A HTML element."))
    "
DO:         Defines a HTML element macro.
NAME:       A symbol that will be used to define a macro.
OPTIONS:    A list of keywords: :START-OPTIONAL :END-FORBIDDEN :EMPTY
                                :DEPRECATED :LOOSE-DTD or :FRAMESET-DTD.
            :END-FORBIDDEN  -> the close tag is not generated.
            :DEPRECATED     -> warning when the macro is used.
            :EMPTY          -> the macro won't take a BODY.
            :START-OPTIONAL -> ignored.
            :LOOSE-DTD      -> error when *DOCTYPE* isn't :LOOSE.
            :FRAMESET-DTD   -> error when *DOCTYPE* isn't :FRAMESET.
DOCUMENTATION:  A string used as documentation string for the macro NAME.
"
    (WITH-GENSYMS (GNAME)
      `(LET ((,GNAME (INTERN ,(STRING NAME) +tag-package+)))
            (PUSH (MAKE-ELEMENT :NAME ,GNAME
                                :OPTIONS ',OPTIONS
                                :DOCUMENTATION ',DOCUMENTATION) *ELEMENTS*)
            (PUSH ,GNAME *ELEMENTS*))))
  );;eval-when

(eval-when (:compile-toplevel :load-toplevel :execute)
  (LOAD
   ( #+allegro (lambda (designator)
                 (if (stringp designator)
                     (let ((colon (position #\: designator)))
                       (format nil "~:@(~A~)~(~A~)"
                               (subseq designator 0 colon)
                               (subseq designator colon)))
                     designator))
     #-allegro identity
     
     "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;HTML401.LISP")))


;;; (defun set-tag-package (package)
;;;   "
;;; DO:    Set the package where the tags will be interned.
;;; "
;;;   (setf +tag-package+ package)
;;;   (setf *elements* nil)
;;;   (LOAD "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;HTML401.LISP")
;;;   );;set-tag-package



;; ------------------------------------------------------------------------
;; HTML Parser

;; (:name "html"
;;  :package "COM.INFORMATIMAGO.COMMON-LISP.PARSE-HTML"
;;  :identifier-start-chars
;;  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
;;  :identifier-continue-chars
;;  "-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
;;  :intern-identifier t
;;  :string-delimiter #.(code-char 0)
;;  :symbol-delimiter #.(code-char 0)
;;  :lex-cats (
;;             (ident      "[A-Za-z][-_A-Za-z0-9]*")
;;             (string-s   #.(format nil "'\\([^'~C~C]*\\)'" 
;;                            (code-char 10) (code-char 13)))
;;             (string-d   #.(format nil "\"\\([^\"~C~C]*\\)\""
;;                            (code-char 10) (code-char 13)))
;;             (string-n   "\\([-+,./0-9:?A-Z_a-z]+\\)")
;;             (comment-k  "[^-]+")
;;             (data       "[^<]+")
;;             )
;;  :grammar  "zebu-mg"
;;  :case-sensitive nil
;;  )
;; 
;; ;; Domain definition
;; 
;; html-seq   := [(first) (rest)];
;; comment    := [(data)];
;; attribute  := [(name ident) (value)];
;; definition := [(name ident) (attributes)];
;; open-tag   := [(name ident) (attributes)];
;; close-tag   := [(name ident)];
;; 
;; 
;; 
;; ;; Productions
;; 
;; file --> tag        { html-seq: [(first tag)  (rest nil) ] }
;;        | data       { html-seq: [(first data) (rest nil) ] }
;;        | tag  file  { html-seq: [(first tag)  (rest file)] }
;;        | data file  { html-seq: [(first data) (rest file)] } ;
;; 
;; tag -->   "<!" ident ">"
;;            { definition: [(name ident) ] }
;;         | "<!" ident aivs ">"
;;            { definition: [(name ident) (attributes aivs)] }
;;         | "<!-" comments "--" ">" 
;;            { comment: [(data comments)] }
;;         | "<!--" comments "--" ">"
;;            { comment: [(data comments)] }
;;         | "<" ident ( "/>" | ">" )
;;            { open-tag: [(name ident) ] }
;;         | "<" ident attributes ( "/>" | ">" )
;;            { open-tag: [(name ident) (attributes attributes)] }
;;         | "</" ident ">" 
;;            { close-tag: [(name ident)] } ;
;; 
;; comments --> comment-k              
;;              {html-seq: [(first comment-k) (rest nil)]}
;;            | comment-k     comments 
;;              {html-seq: [(first comment-k) (rest comments)]}
;;            | comment-k "-" comments 
;;              {html-seq: [(first comment-k) (rest comments)]};
;; 
;; aivs -->   aiv      { html-seq: [(first aiv) (rest nil) ] }
;;          | aiv aivs { html-seq: [(first aiv) (rest aivs)] } ;
;; aiv  --> attribute | ident | value ;
;; 
;; attributes -->   attribute
;;                  { html-seq: [(first attribute) (rest nil) ] }
;;                | attribute attributes 
;;                  { html-seq: [(first attribute) (rest attributes)] };
;; 
;; attribute -->   ident
;;                  { attribute: [(name ident) (value nil)  ] }
;;               | value
;;                  { attribute: [(name value) (value nil)  ] }
;;               | ident "=" value 
;;                  { attribute: [(name ident) (value value)] } ;
;; 
;; value -->  string-s  
;;          | string-d 
;;          | string-n ;


;; ------------------------------------------------------------------------
;; ZEBU Scanner/Parser
;; ------------------------------------------------------------------------

;; (DEFTYPE STRING-D   () 'STRING)
;; (DEFTYPE STRING-S   () 'STRING)
;; (DEFTYPE STRING-N   () 'STRING)
;; (DEFTYPE DATA       () 'STRING)
;; (DEFTYPE COMMENT-K  () 'STRING)
;; (DEFTYPE IDENT      () 'STRING)
;; 
;; (DEFUN STRING-D-P  (OBJECT) (STRINGP OBJECT))
;; (DEFUN STRING-S-P  (OBJECT) (STRINGP OBJECT))
;; (DEFUN STRING-N-P  (OBJECT) (STRINGP OBJECT))
;; (DEFUN DATA-P      (OBJECT) (STRINGP OBJECT))
;; (DEFUN COMMENT-K-P (OBJECT) (STRINGP OBJECT))
;; (DEFUN IDENT-P     (OBJECT) (STRINGP OBJECT))
;; 
;; 
;; 
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (load "html-domain.lisp")
;;   (defparameter gr-html (zebu:zebu-load-file "html-grammar.tab")))
;; 
;; 
;; (defun parse-file (pathname)
;;   (car (zebu:file-parser pathname
;;                          :grammar (zebu:find-grammar "html")
;;                          :print-parse-errors t
;;                          :verbose nil))
;;   );;parse-file
;; 
;; 
;; (defun parse-string (string &key (junk-allowed nil)
;;                             (print-parse-error t)
;;                             (error-fn (function error))
;;                             (start 0))
;;   (zebu:read-parser string
;;                     :grammar (zebu:find-grammar "html")
;;                     :junk-allowed junk-allowed
;;                     :print-parse-errors print-parse-error
;;                     :error-fn error-fn
;;                     :start start));;parse-string



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scanner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (html-scanner (:constructor %make-html-scanner))
  (state      :normal           :type symbol)
  (next-state nil               :type symbol)
  (source (make-instance 'peek-stream :stream *standard-input*)
          :type peek-stream)) ;;html-scanner


(defun make-html-scanner (&key (source *standard-input*) (state :normal))
  (%make-html-scanner :source (make-instance 'peek-stream :stream source)
                      :state state))


(defmacro defcharset (name characters &key complement)
  (let ((characters (eval characters))
        (table (make-array '(256) :element-type 'bit
                           :initial-element (if complement 1 0))))
    (dotimes (i (length characters))
      (setf (aref table (char-code (aref characters i))) (if complement 0 1)))
    `(let ((table ,table))
       (defun ,name (ch)
         (let ((code (char-code ch)))
           (if (<= 0 code 255)
               (/= 0 (aref table code))
               ,complement)))))) ;;defcharset

    
(defcharset cs-space-p
    (format nil "~{~C~}" (mapcar (function code-char) '(32 10 13 9 11 12))))


(defcharset cs-crlf-p
    (format nil "~{~C~}" (mapcar (function code-char) '(10 13))))


(defcharset cs-string-d-char-p
    (format nil "\"~{~C~}" (mapcar (function code-char) '(10 13)))
  :complement t)


(defcharset cs-string-s-char-p
    (format nil "'~{~C~}" (mapcar (function code-char) '(10 13)))
  :complement t)


(defcharset cs-string-n-char-p
    (format nil "~{~C~}\"'=>" (mapcar (function code-char) '(32 10 13 9 11 12)))
  :complement t)
;; "-+,./0123456789:?ABCDEFGHIJKLMNOPQRSTUVWXYZZ_abcdefghijklmnopqrstuvwxyzz"


(defcharset cs-alpha-char-p
    "ABCDEFGHIJKLMNOPQRSTUVWXYZZabcdefghijklmnopqrstuvwxyz")

(defcharset cs-ident-char-p
    "0123456789-ABCDEFGHIJKLMNOPQRSTUVWXYZZ_abcdefghijklmnopqrstuvwxyz:")

;; xmlns:rdf
(defparameter +crlf+ (format nil "~C~C" (code-char 10) (code-char 13)))

;;  <td width=\"180\" align=\"rig valign=bottom nowrap><object classid=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\" codebase=\"http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=6,0,29,0\" width=\"140\" height=\"29\"><param name=\"movie\" value=\"/images/pest_tiendas.swf\"><param name=\"quality\" value=\"high\">


;; <input type="hidden" name="dire" value="Pol. Industrial, 2 "E"">
;; :: if in tag and close-" 
;; :: then look ahead while closed and not '>' and not '='

(defun heuristic-quote-in-string (string start end-of-string)
  "( *[a-z]+ *= *{string})>"
  (let* ((next-quote (position end-of-string   string :start (1+ start)))
         (equal-sign (position (character "=") string :start (1+ start)
                               :end next-quote)))
    (format t "next-quote=~S equal-sign=~S~%" next-quote equal-sign)
    (if next-quote
        (if (and equal-sign (< equal-sign next-quote))
            (values start nil)          ; sss"xxx=xxx"
            (values (1+ next-quote) t)) ; ssss"ssss"xx
        (values start nil))))           ; sss"xxx=xxx>
      

#||
(heuristic-quote-in-string (setf string "Pol. Industrial, 2 \"E\"\"")  
                           (position #\" string)  #\> #\")
(heuristic-quote-in-string (setf string"test \"name=\"titi\"" ) 
                           (position #\" string)  #\> #\")
(heuristic-quote-in-string (setf string"test \"name=\", all.\"")  
                           (position #\" string)  #\> #\")
(heuristic-quote-in-string (setf string"test \"name\", all.\"")  
                           (position #\" string)  #\> #\")
(heuristic-quote-in-string (setf string"test \"name\", all")  
                           (position #\" string)  #\> #\")
||#


(defun get-token (scanner)
  (let ((value (make-array '(16) :fill-pointer 0 :adjustable t 
                           :element-type 'character)))
    (macrolet
        ((getch (keep)
           `(let ((ch (getchar (html-scanner-source scanner))))
              ,(when keep `(when ch (vector-push-extend ch value))) ch))
         (ungetch (ch)
           `(progn
              (ungetchar (html-scanner-source scanner) ,ch)
              (vector-pop value)))
         (nextch ()
           `(nextchar (html-scanner-source scanner))))
      (case (html-scanner-state scanner)
        ((:normal)
         (let ((ch (getch t)))
           (cond
             ;; eof
             ((null ch) (return-from get-token (values :eof nil)))
             ;; open a tag
             ((char= ch (character "<"))
              (setf ch (getch t))
              (cond
                ;; eof
                ((null ch) (return-from get-token (values :pcdata value)))
                ;; <?
                ((char= ch (character "?"))
                 (loop
                    :named :foreign
                    :with state = :foreign
                    :for ch = (getch t)
                    :while ch
                    :do (ecase state
                          ((:foreign) (when (char= ch (character "?"))
                                        (setf state :end)))
                          ((:end)     (if (char= ch (character ">"))
                                          (return-from :foreign
                                            (values :foreign value))
                                          (setf state :foreign))))))
                ;; <!
                ((char= ch (character "!"))
                 (setf ch (getch t))
                 (cond
                   ;; eof
                   ((null ch) (return-from get-token (values :pcdata value)))
                   ;; <!- comment
                   ((char= ch (character "-"))
                    (loop named :comment
                       with state = :comment
                       for ch = (getch t)
                       while ch 
                       do (case state
                            ((:comment) (when (char= ch (character "-"))
                                          (setf state :dash)))
                            ((:dash) (if (char= ch (character "-"))
                                         (setf state :dash-dash)
                                         (setf state :comment)))
                            ((:dash-dash) (cond
                                            ((char= ch (character " ")))
                                            ((char= ch (character ">"))
                                             (return-from :comment
                                               (values :comment value)))
                                            (t (setf state :comment)))))))
                   ;; <! open definition
                   (t (ungetch ch)
                      (setf (html-scanner-state scanner) :tag)
                      (values :open-def value))))
                ;; </ open close tag
                ((char= ch (character "/"))
                 (setf (html-scanner-state scanner) :tag)
                 (values :close-tag value))
                ;; < open open tag
                (t
                 (ungetch ch) 
                 (setf (html-scanner-state scanner) :tag-ident)
                 (values :open-tag value))))
             ;; outside of a tag: pcdata
             (t (loop for ch = (getch t)
                   while (and ch (char/= ch (character "<")))
                   finally (when ch (ungetch ch)))
                (values :pcdata value)))))
        ((:script :style)
         (setf (html-scanner-state scanner) :normal)
         (values :cdata (loop for ch = (getch t)
                           for nc = (nextch)
                           while (and ch nc
                                      (or (char/= ch (character "<"))
                                          (char/= nc (character "/"))))
                           finally (progn (ungetch ch) (return value)))))
        ((:tag-ident :tag)
         (let ((ch (loop for ch = (getch nil)
                      while (and ch (cs-space-p ch))
                      finally (progn (when ch (vector-push-extend ch value))
                                     (return ch)))))
           (cond
             ;; eof
             ((null ch) (return-from get-token (values :eof nil)))
             ;; /> close close tag
             ((and (char= ch (character "/")) (char= (nextch) (character ">")))
              (getch t)
              (setf (html-scanner-state scanner) :normal)
              (values :close-end-tag value))
             ;; > close close tag
             ((char= ch (character ">"))
              (setf (html-scanner-state scanner)
                    (if (html-scanner-next-state scanner)
                        (prog1  (html-scanner-next-state scanner)
                          (setf (html-scanner-next-state scanner) nil))
                        :normal))
              (values :end-tag value))
             ;; identifier
             ((cs-alpha-char-p ch) ;;ident
              (loop for ch = (getch t)
                 while (and ch (cs-ident-char-p ch))
                 finally (when ch (ungetch ch)))
              (when (eq (html-scanner-state scanner) :tag-ident)
                (setf (html-scanner-state scanner) :tag)
                (cond
                  ((string-equal "script" value)
                   (setf (html-scanner-next-state scanner) :script))
                  ((string-equal "style" value)
                   (setf (html-scanner-next-state scanner) :style))))
              (values :identifier value))
             ;; "string" or 'string'
             ((or (char= ch (character "\"")) ;; string-d
                  (char= ch (character "'"))) ;; string-n
              (let ((char-set (if (char= ch (character "\""))
                                  (function cs-string-d-char-p)
                                  (function cs-string-s-char-p)))
                    (end-of-string ch))
                (vector-pop value)
                (tagbody
                 :go-on
                   (loop for ch = (getch t)
                      while (and ch (or (funcall char-set ch)
                                        (position ch +crlf+)))
                      when (position ch +crlf+)
                      do  (warn "Newline inside a string: ~S" value)
                      finally (if (or (null ch) (char/= ch end-of-string))
                                  (error "Newline inside a string: ~S" value)
                                  (vector-pop value)))
                   (cond
                     ((char= (nextch) end-of-string)
                      ;; let's handle a syntax error: <tag attrib="xyz"">
                      (let ((ch (getch nil)))
                        (unless (char= (nextch) (character ">"))
                          (ungetch ch))))
                     ((and (< 0 (fill-pointer value))
                           (char= (aref value (1- (fill-pointer value)))
                                  (character " "))
                           (not (position (nextch) " >")))
                      ;; let's handle a syntax error: 
                      ;; <tag attrib="Jojo "The Beef" Steeck">
                      (let ((start (fill-pointer value)))
                        (vector-push-extend end-of-string value) 
                        (loop for ch = (getch t)
                           while (and ch (not (position ch "\">"))))
                        (multiple-value-bind (end go-on)
                            (heuristic-quote-in-string value start end-of-string)
                          (loop while (< end (fill-pointer value))
                             do (ungetch (aref value (1- (fill-pointer value)))))
                          (when go-on (go :go-on))))))))
              (values :string value))
             ;; unquoted attribute: cdata
             ((cs-string-n-char-p ch) ;; string-n
              (loop for ch = (getch t)
                 while (and ch (cs-string-n-char-p ch))
                 finally (ungetch ch))
              (values :string value))
             ((char= ch (character "="))
              (values :equal value))
             (t (error "Invalid character '~C' in tag." ch)))))
        (otherwise (error "Invalid state ~S" 
                          (html-scanner-state scanner))))))) ;;get-token


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Keep these structures disjoint types for the walk methods below.
(defstruct html-seq first rest)
(defstruct comment data)
(defstruct foreign data)
(defstruct attribute name value)
(defstruct definition name attributes)
(defstruct open-tag name attributes closed)
(defstruct close-tag name attributes)


(defstruct html-parser  
  scanner
  token value
  next-token next-value)


(defun advance (parser)
  (multiple-value-bind (tok val) (get-token (html-parser-scanner parser))
    (setf (html-parser-token parser)      (html-parser-next-token parser)
          (html-parser-value parser)      (html-parser-next-value parser) 
          (html-parser-next-token parser) tok
          (html-parser-next-value parser) val))) ;;advance


;; Productions
 
 
(defun report-error (parser message)
  (error "~A; (~S ~S) (~S ~S)" message
         (html-parser-token parser)
         (html-parser-value parser)
         (html-parser-next-token parser)
         (html-parser-next-value parser))) ;;report-error


(defun parse-file (parser)
  ;; file --> tag        { html-seq: [(first tag)  (rest nil) ] }
  ;;        | data       { html-seq: [(first data) (rest nil) ] }
  ;;        | tag  file  { html-seq: [(first tag)  (rest file)] }
  ;;        | data file  { html-seq: [(first data) (rest file)] } ;
  (loop with items = '()
     for synthetic = (case (html-parser-token parser)
                       ((:eof)       nil)
                       ((:pcdata)    (prog1 (html-parser-value parser) 
                                       (advance parser)))
                       ((:cdata)     (prog1 (html-parser-value parser) 
                                       (advance parser)))
                       ((:OPEN-DEF)  (parse-definition parser))
                       ((:foreign)   (prog1 (make-foreign
                                             :data (html-parser-value parser))
                                       (advance parser)))
                       ((:COMMENT)   (prog1 (make-comment 
                                             :data (html-parser-value parser))
                                       (advance parser)))
                       ((:OPEN-TAG)  (parse-open-tag parser))
                       ((:CLOSE-TAG) (parse-close-tag parser))
                       (otherwise   (report-error parser
                                                  "Unexpected token")))
     while synthetic do (push synthetic items)
     finally (return (when items
                       (let ((result '()))
                         (dolist (item items result)
                           (setf result (make-html-seq
                                         :first item
                                         :rest result)))))))) ;;parse-file


(defun parse-definition (parser)
  ;; tag -->   "<!" ident ">"
  ;;            { definition: [(name ident) ] }
  ;;         | "<!" ident aivs ">"
  ;;            { definition: [(name ident) (attributes aivs)] }
  (advance parser)
  (unless (eq :identifier (html-parser-token parser))
    (report-error parser "Expected an identifier"))
  (let ((ident (html-parser-value parser)))
    (advance parser)
    (if (eq :end-tag (html-parser-token parser))
        (progn (advance parser) (make-definition :name ident :attributes nil))
        (let ((attributes (parse-aivs parser)))
          (if (eq :end-tag (html-parser-token parser))
              (advance parser)
              (report-error parser "Expected a \">\""))
          (make-definition :name ident :attributes attributes)))))


(defun parse-aivs (parser)
  ;; aivs -->   aiv      { html-seq: [(first aiv) (rest nil) ] }
  ;;          | aiv aivs { html-seq: [(first aiv) (rest aivs)] } ;
  ;; aiv  --> attribute | ident | value ;
  (make-html-seq :first (if (eq :string (html-parser-token parser))
                            (prog1 (html-parser-value parser) (advance parser))
                            (parse-attribute parser))
                 :rest  (when (member (html-parser-token parser)
                                      '(:string :identifier))
                          (parse-aivs parser)))) ;;parse-aivs

(defun parse-open-tag (parser)
  ;;         | "<" ident ">"
  ;;            { open-tag: [(name ident) ] }
  ;;         | "<" ident attributes ">"
  ;;            { open-tag: [(name ident) (attributes attributes)] }
  (advance parser)
  (unless (eq :identifier (html-parser-token parser))
    (report-error parser "Expected a tag identifier"))
  (let ((tag (prog1 (make-open-tag 
                     :name (prog1 (html-parser-value parser) (advance parser))
                     :attributes (unless (member (html-parser-token parser) 
                                                 '(:end-tag :close-end-tag))
                                   (parse-attributes parser))
                     :closed (eq (html-parser-token parser) :close-end-tag))
               (unless (member (html-parser-token parser)
                               '(:end-tag :close-end-tag))
                 (report-error parser "Expected a \">\""))
               (advance parser))))
    tag)) ;;parse-open-tag


(defun parse-close-tag (parser)
  ;; Same as open-tag, but for the make-close-tag.
  (advance parser)
  (unless (eq :identifier (html-parser-token parser))
    (report-error parser "Expected a tag identifier")) 
  (prog1 (make-close-tag 
          :name (prog1 (html-parser-value parser) (advance parser))
          :attributes (unless (eq :end-tag (html-parser-token parser))
                        (parse-attributes parser)))
    (unless (eq :end-tag (html-parser-token parser))
      (report-error parser "Expected a \">\""))
    (advance parser))) ;;parse-close-tag

  
(defun parse-attributes (parser)
  ;; attributes -->   attribute
  ;;                  { html-seq: [(first attribute) (rest nil) ] }
  ;;                | attribute attributes 
  ;;                  { html-seq: [(first attribute) (rest attributes)] };
  (make-html-seq :first (parse-attribute parser)
                 :rest  (when (eq (html-parser-token parser) :identifier)
                          (parse-attributes parser)))) ;;parse-attributes


(defun parse-attribute (parser)
  ;; attribute -->   ident
  ;;                  { attribute: [(name ident) (value nil)  ] }
  ;;               | ident "=" value 
  ;;                  { attribute: [(name ident) (value value)] } ;
  (unless (member (html-parser-token parser) '(eq :identifier :string))
    (report-error parser "Expected an attribute identifier or a string."))
  (make-attribute 
   :name (prog1 (html-parser-value parser) (advance parser))
   :value (if (eq :equal (html-parser-token parser))
              (progn (advance parser)
                     (unless (member (html-parser-token parser)
                                     '(:string :identifier))
                       (report-error parser "Expected an attribute value"))
                     (prog1 (html-parser-value parser) (advance parser)))
              nil))) ;;parse-attribute
             

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric walk (html))

(DEFMETHOD WALK ((SELF HTML-SEQ))
  (do ((current self (html-seq-rest current))
       (result '()))
      ((not (typep current 'html-seq))
       (assert (null current))
       (nreverse result))
    (push (WALK (HTML-SEQ-FIRST current)) result)))


(DEFMETHOD WALK ((SELF COMMENT))
  (LIST :COMMENT
        '() (WALK (COMMENT-DATA SELF))))


(DEFMETHOD WALK ((SELF foreign))
  (LIST :foreign
        '() (WALK (foreign-DATA SELF))))


(DEFMETHOD WALK ((SELF ATTRIBUTE))
  (LIST (INTERN (STRING-UPCASE (ATTRIBUTE-NAME SELF)) +tag-package+)
        (OR (ATTRIBUTE-VALUE SELF) T)))


(DEFMETHOD WALK ((SELF OPEN-TAG))
  (LIST :OPEN
        (INTERN (STRING-UPCASE (OPEN-TAG-NAME SELF)) +tag-package+)      
        (WALK (OPEN-TAG-ATTRIBUTES SELF))))


(DEFMETHOD WALK ((SELF CLOSE-TAG))
  (LIST :CLOSE
        (INTERN (STRING-UPCASE (CLOSE-TAG-NAME SELF)) +tag-package+)
        (WALK (close-TAG-ATTRIBUTES SELF))))


(DEFMETHOD WALK ((SELF DEFINITION))
  (LIST :DEFINITION
        (INTERN (STRING-UPCASE (DEFINITION-NAME SELF)) +tag-package+)      
        (WALK (DEFINITION-ATTRIBUTES SELF))))


(DEFMETHOD WALK ((SELF T))  SELF)


(DEFUN CLEAN-ATTRIBUTE (ATTR)
  (cond ((not (stringp attr)) attr)
        ((OR (AND (<= 2 (length attr))
                  (CHAR= (CHARACTER "'") (CHAR ATTR 0))
                  (CHAR= (CHARACTER "'") (CHAR ATTR (1- (LENGTH ATTR)))))
             (AND (<= 2 (length attr))
                  (CHAR= (CHARACTER "\"") (CHAR ATTR 0))
                  (CHAR= (CHARACTER "\"") (CHAR ATTR (1- (LENGTH ATTR))))))
         (SUBSEQ ATTR 1 (- (LENGTH ATTR) 1)))
        (t    ATTR))) ;;CLEAN-ATTRIBUTE


(DEFUN ENCASE (TAG-LIST)
  ;;     content-list-reversed
  ;;     ( (tag (attributes)) content-list-reversed)
  ;;     ( (tag (attributes)) content-list-reversed)
  ;;     ( (tag (attributes)) content-list-reversed)
  ;;     ( (tag (attributes)) )
  (DO* ((STACK    (list NIL))
        (TAG-LIST TAG-LIST       (CDR TAG-LIST))
        (TAG      (CAR TAG-LIST) (CAR TAG-LIST)))
       ((NULL TAG-LIST)      (nreverse (pop stack))) ;;(car (pop STACK)))
    (COND
      ((OR (ATOM TAG) (EQ (CAR TAG) :COMMENT))
       (PUSH TAG (CAR STACK)))
      ((EQ (CAR TAG) :DEFINITION)
       ;; ignore
       )
      ((EQ (CAR TAG) :OPEN)
       (PUSH (CONS (SECOND TAG)
                   (LIST
                    (MAPCAN (LAMBDA (KV)
                              (LIST (FIRST KV)
                                    (CLEAN-ATTRIBUTE (SECOND KV))))
                            (THIRD TAG)))) (CAR STACK))
       (unless (ELEMENT-END-FORBIDDEN-P (second tag))
         (PUSH NIL STACK)))
      ((and (EQ (CAR TAG) :CLOSE)
            (position (cadr tag) stack :key (lambda (item)  (and (consp item) 
                                                                 (consp (car item)) 
                                                                 (caar item)))))
       (UNTIL (AND (CONSP (CAAR STACK)) (EQ (CADR TAG) (CAAAR STACK)))
              (LET ((ATTRIBUTES (NREVERSE (POP STACK))))
                (SETF (CAAR STACK) (APPEND (CAAR STACK) ATTRIBUTES))))))))
;; (eq (car tag) :close) and no corresponding open )))


(defun parse-html-file (pathname &key (verbose nil) (external-format :default))
  (let ((name (namestring pathname))
        synthetic walked encased)
    (when verbose
      (format *trace-output* "~&starting parsing of file ~S~%" name))
    (setf synthetic (with-open-file (src pathname :direction :input 
                                         :if-does-not-exist :error
                                         :external-format external-format)
                      (let ((parser (make-html-parser 
                                     :scanner (make-html-scanner :source src))))
                        (advance parser)
                        (advance parser)
                        (parse-file parser))))
    (when verbose (format *trace-output* "~&file ~S parsed~%" name))
    (setf walked (walk synthetic))
    (when verbose (format *trace-output* "~&file ~S walked~%" name))
    (setf encased (encase walked))
    (when verbose (format *trace-output* "~&file ~S encased -- done.~%" name))
    encased))


(defun parse-html-string (string &key (start 0) (verbose nil))
  (when verbose
    (format *trace-output* "~&starting string parsing from ~D~%" start))
  (let ((synthetic  (with-input-from-string (src string :start start)
                      (let ((parser (make-html-parser
                                     :scanner (make-html-scanner :source src))))
                        (advance parser)
                        (advance parser)
                        (parse-file parser))))
        walked encased)
    (when verbose (format *trace-output* "~&string parsed~%"))
    (setf walked (walk synthetic))
    (when verbose (format *trace-output* "~&string walked~%"))
    (setf encased (encase walked))
    (when verbose (format *trace-output* "~&string encased -- done.~%"))
    encased))



(defun html-tag        (html)     (first  html))
(defun html-attributes (html)     (second html))
(defun html-contents   (html)     (cddr   html))
(defun html-attribute  (html key) (cadr (member key (second html))))


(defparameter *nl* (make-hash-table))

(defmacro defnl (tag &rest nls)
  `(setf (gethash
          (intern (string-upcase ,(symbol-name tag)) +tag-package+) *nl*)
         ',nls))

;; ^  <a>^  x^  </a>^
;; :bo   :ao :bc    :ac

(defnl A                :bo :ac)
(defnl ABBR             )
(defnl ACRONYM          )
(defnl ADDRESS          :bo :ac)
(defnl APPLET           :bo :ao :bc :ac)
(defnl AREA             :bo :ac)
(defnl B                )
(defnl BASE             )
(defnl BASEFONT         )
(defnl BDO              )
(defnl BIG              )
(defnl BLOCKQUOTE       :bo :ao :bc :ac)
(defnl BODY             :bo :ao :bc :ac)
(defnl BR               :bo :ac)
(defnl BUTTON           :bo :ac)
(defnl CAPTION          )
(defnl CENTER           :bo :ac)
(defnl CITE             :bo :ac)
(defnl CODE             )
(defnl COL              )
(defnl COLGROUP         )
(defnl DD               :bo)
(defnl DEL              )
(defnl DFN              )
(defnl DIR              )
(defnl DIV              :bo :ac)
(defnl DL               :bo :ac)
(defnl DT               :bo)
(defnl EM               )
(defnl FIELDSET         )
(defnl FONT             )
(defnl FORM             :bo :ao :bc :ac)
(defnl FRAME            :bo :ao :bc :ac)
(defnl FRAMESET         :bo :ao :bc :ac)
(defnl H1               :bo :ac)
(defnl H2               :bo :ac)
(defnl H3               :bo :ac)
(defnl H4               :bo :ac)
(defnl H5               :bo :ac)
(defnl H6               :bo :ac)
(defnl HEAD             :bo :ao :bc :ac)
(defnl HR               :bo :ac)
(defnl HTML             :bo :ao :bc :ac)
(defnl I                )
(defnl IFRAME           :bo :ao :bc :ac)
(defnl IMG              :bo :ac)
(defnl INPUT            :bo :ac)
(defnl INS              )
(defnl ISINDEX          )
(defnl KBD              )
(defnl LABEL            )
(defnl LEGEND           )
(defnl LI               :bo :ac)
(defnl LINK             :bo :ac)
(defnl MAP              :bo :ao :bc :ac)
(defnl MENU             :bo :ao :bc :ac)
(defnl META             :bo :ac)
(defnl NOFRAMES         :bo :ao :bc :ac)
(defnl NOSCRIPT         :bo :ao :bc :ac)
(defnl OBJECT           :bo :ao :bc :ac)
(defnl OL               :bo :ao :bc :ac)
(defnl OPTGROUP         :bo :ao :bc :ac)
(defnl OPTION           :bo :ac)
(defnl P                :bo :ac)
(defnl PARAM            :bo :ac)
(defnl PRE              :bo :ac)
(defnl Q                )
(defnl S                )
(defnl SAMP             ) 
(defnl SCRIPT           :bo :ao :bc :ac)
(defnl SELECT           :bo :ao :bc :ac)
(defnl SMALL            )
(defnl SPAN             :bo :ac)
(defnl STRIKE           )
(defnl STRONG           )
(defnl STYLE            :bo :ac)
(defnl SUB              )
(defnl SUP              )
(defnl TABLE            :bo :ao :bc :ac)
(defnl TBODY            :bo :ao :bc :ac)
(defnl TD               :bo :ac)
(defnl TEXTAREA         :bo :ac)
(defnl TFOOT            :bo :ao :bc :ac)
(defnl TH               :bo :ac)
(defnl THEAD            :bo :ao :bc :ac)
(defnl TITLE            :bo :ac)
(defnl TR               :bo :ac)
(defnl TT               )
(defnl U                )
(defnl UL               :bo :ao :bc :ac)
(defnl VAR              )


(defun unparse-html (html &optional (stream *standard-output*))
  (if (atom html)
      (format stream "~A" html)
      (let ((nl (gethash (html-tag html) *nl*)))
        (format stream "~:[~;~&~]<~A~{ ~A=~S~}>~:[~;~&~]" 
                (member :bo nl)
                (string-downcase (html-tag html))
                (html-attributes html)
                (member :ao nl))
        (dolist (item (html-contents html))
          (unparse-html item stream))
        (format stream "~:[~;~&~]</~A>~:[~;~&~]"
                (member :bc nl)
                (string-downcase (html-tag html))
                (member :ac nl)))))


;;;; parse-html.lisp                  --                     --          ;;;;

