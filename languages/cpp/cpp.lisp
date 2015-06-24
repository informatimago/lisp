;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cpp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This is a C preprocessor.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-06-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LANGUAGES.CPP"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:shadow "IMPORT" "INCLUDE")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
                          "STRING-DESIGNATOR")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
                          "COPY-STREAM")
  (:export "TOKEN" "TOKEN-LINE" "TOKEN-COLUMN" "TOKEN-FILE"
           "TOKEN-TEXT" "IDENTIFIER-TOKEN" "NUMBER-TOKEN" "PUNCTUATION-TOKEN"
           "OTHER-TOKEN"

           "READ-CPP-TOKENS"
           "ENVIRONMENT-MACRO-DEFINITION"
   "ENVIRONMENT-MACRO-DEFINEDP"
   "ENVIRONMENT-MACRO-UNDEFINE"
           "PROCESS"))

(in-package "COM.INFORMATIMAGO.COMMON-LISP.LANGUAGES.CPP")


(defun number-lines (lines file-name &key (start 1))
  (loop
    :for lino :from start
    :for line :in lines
    :collect (list line lino file-name)))

(defstruct (numbered-line
            (:type list)
            (:conc-name line-))
  (text "")
  (lino 1)
  (file "-"))



(declaim (inline trigraph-character))
(defun trigraph-character (char)
  ;; Trigraph:       ??(  ??)  ??<  ??>  ??=  ??/  ??'  ??!  ??-
  ;; Replacement:      [    ]    {    }    #    \    ^    |    ~
  (case char
    (#\( #\[)
    (#\) #\])
    (#\< #\{)
    (#\> #\})
    (#\= #\#)
    (#\/ #\\)
    (#\' #\^)
    (#\! #\|)
    (#\- #\~)
    (otherwise nil)))

(defun substitute-trigraphs (line &key warn-on-trigraph)
  (let ((text (line-text line)))
    (when (search "??" text)
      (loop
        :with tri := nil
        :with j := 0
        :with i := 0
        :while (< i (length text))
        :if (and (< (+ 2 i) (length text))
                 (char= #\? (aref text i))
                 (char= #\? (aref text (1+ i)))
                 (setf tri (trigraph-character (aref text (+ 2 i)))))
          :do (when warn-on-trigraph
                (warn "~A:~A: found trigraph ??~A converted to ~A"
                      (line-file line) (line-lino line) (aref text (+ 2 i)) tri))
              (setf (aref text j) tri)
              (incf j)
              (incf i 3)
        :else
          :do (setf (aref text j) (aref text i))
              (incf j)
              (incf i)
        :finally (setf (line-text line) (subseq text 0 j)))))
  line)


(defun continued-line-p (line)
  (let ((len (length line)))
    (when (plusp len)
      (let ((spaces     (char= #\space (aref line (decf len))))
            (last-char  (find-if (lambda (ch) (char/= #\space ch))
                                 line :from-end t)))
        (values (when (eql last-char #\\)
                  (position #\\ line :from-end t))
                spaces)))))

(defun merge-continued-lines (lines &key  warn-spaces-in-continued-lines)
  (loop
    :while lines
    :collect (let ((line (pop lines)))
               (multiple-value-bind (continuedp spacesp) (continued-line-p (line-text line))
                 (if continuedp
                     (let ((lino (line-lino line))
                           (file (line-file line)))
                       (make-numbered-line
                        :text (concatenate-strings
                               (loop :with result = '()
                                     :while continuedp
                                     :do (when (and spacesp warn-spaces-in-continued-lines)
                                           (warn "~A:~A: spaces after line continuation character." file lino))
                                         (push (subseq (line-text line) 0 continuedp) result)
                                         (setf line (pop lines))
                                         (if (null line)
                                             (progn
                                               (warn "~A:~A: last line is a continued line"
                                                     file lino)
                                               (setf continuedp nil))
                                             (multiple-value-setq (continuedp spacesp) (continued-line-p (line-text line))))
                                     :finally (push (line-text line) result)
                                              (return (nreverse result))))
                        :lino lino
                        :file file))
                     line)))))



(defun remove-comments-in-line (comment-start-line current-line state single-line-comments)
  (destructuring-bind (text lino file) current-line
    (flet ((concatenate-chunks (chunks)
             (if comment-start-line
                 (make-numbered-line :text (mapconcat (function identity)
                                                      (cons (line-text comment-start-line) chunks)
                                                      " ")
                                     :lino (line-lino comment-start-line)
                                     :file (line-file comment-start-line))
                 (make-numbered-line :text (mapconcat (function identity) chunks " ")
                                     :lino (line-lino current-line)
                                     :file (line-file current-line)))))
      (loop
        :with chunks := '()
        :with start := (ecase state
                         (:top 0)
                         (:in-multiline-comment (length text)))
        :with i := 0
        :while (< i (length text))
        :do (let ((ch (aref text i)))
              (ecase state
                (:top
                 (case ch
                   ((#\")     (incf i) (setf state :in-string))
                   ((#\')     (incf i) (setf state :in-character))
                   ((#\/)
                    (incf i)
                    (when (< i (length text))
                      (let ((ch (aref text i)))
                        (case ch
                          ((#\/) ;;single line comment
                           (when single-line-comments
                             (return-from remove-comments-in-line
                               (values (concatenate-chunks (nreverse (cons (subseq text start (1- i)) chunks)))
                                       state))))
                          ((#\*)
                           (incf i)
                           (push (subseq text start (- i 2)) chunks)
                           (setf state :in-multiline-comment
                                 start (length text)))))))
                   (otherwise (incf i))))
                (:in-multiline-comment
                 (case ch
                   ((#\*)
                    (incf i)
                    (when (< i (length text))
                      (let ((ch (aref text i)))
                        (when (char= ch #\/)
                          (incf i)
                          (setf start i)
                          (setf state :top)))))
                   (otherwise
                    (incf i))))
                (:in-string
                 (case ch
                   ((#\\)
                    (incf i)
                    (if (< i (length text))
                        (incf i)
                        (progn (cerror "Continue" "~A:~A: backslash in string literal at the end of the line" file lino)
                               (setf state :top))))
                   ((#\")
                    (incf i)
                    (setf state :top))
                   (otherwise (incf i))))
                (:in-character
                 (case ch
                   ((#\\)
                    (incf i)
                    (if (< i (length text))
                        (incf i)
                        (progn (cerror "Continue" "~A:~A: backslash in character literal at the end of the line" file lino)
                               (setf state :top))))
                   ((#\')
                    (incf i)
                    (setf state :top))
                   (otherwise (incf i))))))
        :finally (return (case state
                           (:in-string
                            (cerror "Continue" "~A:~A: unterminated string literal at the end of the line" file lino)
                            (values (concatenate-chunks (nreverse chunks)) :top))
                           (:in-character
                            (cerror "Continue" "~A:~A: unterminated character literal at the end of the line" file lino)
                            (values (concatenate-chunks (nreverse chunks)) :top))
                           (:top
                            (values (concatenate-chunks (nreverse (if (< start (length text))
                                                                      (cons (subseq text start) chunks)
                                                                      chunks)))
                                    state))
                           (:in-multiline-comment
                            (values (concatenate-chunks (nreverse chunks))
                                    state))))))))

(defun remove-comments (lines &key (single-line-comments t))
  (loop
    :with state := :top
    :while lines
    :collect (let* ((line (pop lines))
                    (lino (line-lino line))
                    (file (line-file line)))
               (multiple-value-bind (new-line new-state)
                   (remove-comments-in-line nil line state single-line-comments)
                 (loop
                   :while (and (eql new-state :in-multiline-comment) lines)
                   :do (multiple-value-setq (new-line new-state)
                         (remove-comments-in-line new-line (pop lines) new-state single-line-comments)))
                 (when (eql new-state :in-multiline-comment)
                   (cerror "Continue" "~A:~A: end of file before end of multiline comment" file lino)
                   (setf new-state :top))
                 (setf state new-state)
                 new-line))))



(defclass token ()
  ((line   :initform 0   :initarg :line   :accessor token-line)
   (column :initform 0   :initarg :column :accessor token-column)
   (file   :initform "-" :initarg :file   :accessor token-file)
   (text                 :initarg :text   :accessor token-text)))

(defmacro define-token-class (name)
  (let ((class-name (intern (concatenate 'string (string name) (string '-token)))))
    `(progn
       (defclass ,class-name   (token) ())
       (defmethod print-object ((self ,class-name) stream)
         (print-unreadable-object (self stream :identity nil :type t)
           (format stream "~A:~A:~A: ~S"
                   (token-file self) (token-line self) (token-column self) (token-text self)))
         self)
       (defun ,(intern (concatenate 'string (string 'make-) (string name))) (text column line file)
         (make-instance ',class-name :text text :column column :line line :file file)))))

(define-token-class identifier)
(define-token-class number)
(define-token-class string-literal)
(define-token-class character-literal)
(define-token-class punctuation)
(define-token-class other)


(defparameter *whitespaces* #(#\space #\tab #\vt #\page #\nul #\newline #\return #\linefeed))

(defun skip-spaces (text start)
  (loop
    :while (and (< start (length text))
                (find (aref text start) *whitespaces*))
    :do (incf start))
  start)

(defun small-unicode-escape-p (text start)
  (and (<= (+ start 6) (length text))
       (char= #\\ (aref text start))
       (char= #\u (aref text (1+ start)))
       (let ((hex (subseq text (+ 2 start) (+ 6 start))))
         (and (every (lambda (ch) (digit-char-p ch 16)) hex)
              (parse-integer hex :radix 16 :junk-allowed t)))))

(defun big-unicode-escape-p (text start)
  (and (<= (+ start 10) (length text))
       (char= #\\ (aref text start))
       (char= #\U (aref text (1+ start)))
       (let ((hex (subseq text (+ 2 start) (+ 10 start))))
         (and (every (lambda (ch) (digit-char-p ch 16)) hex)
              (parse-integer hex :radix 16 :junk-allowed t)))))


;; (small-unicode-escape-p "(\\uBEB)" 1)


(defun scan-identifier (line start special &key (accept-unicode-escapes nil))
  "SPECIAL: The special characters allowed in identifiers (`_' or `_$')
RETURN: the token text; the end position."
  ;; /[_$a-zA-Z][_$a-zA-Z0-9]*/
  ;; $ is a gcc extension.
  ;; \uxxxx and \Uxxxxxxxx accepted here
  ;; (the 1999 C standard would allow extended characters too).
  (loop
    :with text := (line-text line)
    :with end := start
    :while (< end (length text))
    :do (let ((ch (aref text end)))
          (cond ((or (alphanumericp ch)
                     (find ch special))
                 (incf end))
                ((and accept-unicode-escapes (small-unicode-escape-p text end))
                 (incf end 6))        ;\uxxxx
                ((and accept-unicode-escapes (big-unicode-escape-p text end))
                 (incf end 10))       ;\Uxxxxxxxx
                (t
                 (loop-finish))))
    :finally (return (values (subseq text start end) end))))


(defun scan-number (line start)
  ;; preprocessing number,   /.?[0-9]\([a-zA-Z0-9_.]\|[EepP][-+]\)+/
  ;; all normal integer and floating point constants.
  ;; 0xE+12 is a preprocessing number, not 0xE + 12.
  ;; We've already checked for /.[0-9]/ or /[0-9]/
  (loop 
    :with text := (line-text line)
    :with end := (if (and (char= #\. (aref text start))
                          (< (1+ start) (length text))
                          (digit-char-p (aref text (1+ start))))
                     (1+ start)
                     start)
    :while (< end (length text))
    :do (let ((ch (aref text end)))
          (cond
            ((find ch "eEpP")
             (if (and (< (1+ end) (length text))
                      (find (aref text (1+ end)) "-+"))
                 (incf end 2)
                 (incf end 1)))
            ((or (alphanumericp ch) (find ch "._"))
             (incf end))
            (t
             (loop-finish))))
    :finally (return (values (subseq text start end) end))))


(defun scan-delimited-literal (line start)
  (loop
    :with text := (line-text line)
    :with terminator = (ecase (aref text start)
                         (#\" #\")
                         (#\' #\')
                         (#\< #\>))
    :with end := (1+ start)
    :while (< end (length text))
    :do (let ((ch (aref text end)))
          (cond
            ((char= terminator ch)
             (incf end)
             (loop-finish))
            ((char= #\\ ch)
             (if (< (1+ end) (length text))
                 (incf end 2)
                 (cerror "Continue" "~A:~A: unterminated ~:[string~;character~] literal ending with incomplete escape"
                       (line-file line) (line-lino line)
                       (char= terminator #\'))))
            (t
             (incf end))))
    :finally (return (values (subseq text start end) end))))


(defun scan-punctuation (line start)
  ;;      Digraph:        <%  %>  <:  :>  %:  %:%:
  ;;      Punctuator:      {   }   [   ]   #    ##
  (let* ((text (line-text line))
         (ch   (aref text start)))
    (flet ((greedy2 (alternatives)
             (if (and (< (1+ start) (length text))
                      (find (aref text (1+ start)) alternatives))
                 (values (subseq text start (+ 2 start)) (+ 2 start))
                 (values (subseq text start (1+ start) ) (1+ start))))
           (greedy3 (alternatives token)
             (cond
               ((and (<= (+ (length token) start) (length text))
                     (string= text token :start1 start :end1 (+ (length token) start)))
                (values (subseq text start (+ (length token) start)) (+ (length token) start)))
               ((and (< (1+ start) (length text))
                     (find (aref text (1+ start)) alternatives))
                (values (subseq text start (+ 2 start)) (+ 2 start)))
               (t
                (values (subseq text start (1+ start) ) (1+ start))))))
      (case ch
        ((#\( #\) #\, #\; #\? #\@ #\[ #\] #\{ #\} #\~)
         (values (subseq text start (1+ start)) (1+ start)))
        ((#\! #\*  #\/ #\= #\^) (greedy2 "="))
        ((#\#)                  (greedy2 "#"))
        ((#\&)                  (greedy2 "&="))
        ((#\+)                  (greedy2 "+="))
        ((#\:)                  (greedy2 ":>"))
        ((#\|)                  (greedy2 "|="))
        ((#\.)                  (greedy3 "*"    "..."))
        ((#\-)                  (greedy3 "-=>"  "->*"))
        ((#\<)                  (greedy3 "%:<=" "<<="))
        ((#\>)                  (greedy3 "=>"   ">>="))
        ((#\%)                  (greedy3 "=>:"  "%:%:"))
        (otherwise
         (cerror "Continue" "~A:~A: invalid punctuation: ~S"
                 (line-file line) (line-lino line) ch)
         (values "?" (1+ start)))))))


(defun punctuatorp (ch)
  (find ch "!#%&()*+,-./:;<=>?[\\]^{|}~"))


(defun tokenize-line (line &key (accept-unicode-escapes nil)
                             (dollar-is-punctuation nil))
  (destructuring-bind (text lino file) line
    (let ((start 0)
          (first-identifier (if dollar-is-punctuation
                                "_"
                                "_$")))
      (loop
        :with header = 1
        :do (setf start (skip-spaces text start))
        :while (< start (length text))
        :collect (let ((ch (aref text start)))
                   (cond
                     ((or (find ch first-identifier)
                          (alpha-char-p ch)
                          (and accept-unicode-escapes
                               (char= #\\ ch)
                               (< (1+ start) (length text))
                               (char-equal #\u (aref text (1+ start)))))
                      (multiple-value-bind (token end) (scan-identifier line start first-identifier
                                                                        :accept-unicode-escapes accept-unicode-escapes)
                        (if (and (eql 2 header) (or (string= "include" token)
                                                    (string= "import" token)))
                            (setf header t)
                            (setf header nil))
                        (prog1 (make-identifier token start lino file)
                          (setf start end))))
                     ((or (and (char= ch #\.)
                               (< (1+ start) (length text))
                               (digit-char-p (aref text (1+ start))))
                          (digit-char-p ch))
                      (multiple-value-bind (token end) (scan-number line start)
                        (setf header nil)
                        (prog1 (make-number token start lino file)
                          (setf start end))))
                     ((char= #\" ch)
                      (multiple-value-bind (token end) (scan-delimited-literal line start)
                        (setf header nil)
                        (prog1 (make-string-literal token start lino file)
                          (setf start end))))
                     ((char= #\' ch)
                      (multiple-value-bind (token end) (scan-delimited-literal line start)
                        (setf header nil)
                        (prog1 (make-character-literal token start lino file)
                          (setf start end))))
                     ((and (eq header t) (char= #\< ch))
                      (multiple-value-bind (token end) (scan-delimited-literal line start)
                        (setf header nil)
                        (prog1 (make-string-literal token start lino file)
                          (setf start end))))
                     ((punctuatorp ch)
                      (multiple-value-bind (token end) (scan-punctuation line start)
                        (if (and (eql 1 header) (string= "#" token))
                            (setf header 2)
                            (setf header nil))
                        (prog1 (make-punctuation token start lino file)
                          (setf start end))))
                     (t ;; others
                      (setf header nil)
                      (prog1 (make-other (subseq text start (1+ start)) start lino file)
                        (incf start)))))))))



;; The preprocessor is greedy: a+++++b --> a ++ ++ + b
;; even when a ++ + ++ b could be legal c but not a ++ ++ + b.
;; 
;; 
;; The compiler doesn't retokenize: the pre-processor provides a token
;; stream to the compiler.
;; 
;; Tokens:
;; 
;; - identifiers,          /[_$a-zA-Z][_$a-zA-Z0-9]*/
;;    $ is a gcc extension.
;;    \u and \U accepted here \uxxxx or \Uxxxxxxxx
;;    (the 1999 C standard would allow extended characters too).
;;    
;; - preprocessing number,    /.?[0-9]\([a0zA0Z0-9_.]\|[EepP][-+]\)+/
;;   all normal integer and floating point constants.
;;   0xE+12 is a preprocessing number, not 0xE + 12.
;;    
;; - string literals, and character literals "…" '…' with \ escapes.
;;   + "…" and <…> for header file names, where \ is a normal character.
;;   NUL is preserved.
;;   
;; - punctuators,
;;    all ascii punctuation but `@’, ‘$’, and ‘`’.
;;    and 2 and 3 character operators, including digraphs:
;;    
;;      Digraph:        <%  %>  <:  :>  %:  %:%:
;;      Punctuator:      {   }   [   ]   #    ##
;; 
;; - others: `@’, ‘$’, and ‘`’.
;;   control characters but NUL, 127<code, 
;; 
;; Outside of strings, NUL is considered a whitespace.



(defun read-cpp-tokens (character-stream
                        &key
                          (file-name "-")
                          (substitute-trigraphs nil)
                          (warn-on-trigraph nil)
                          (warn-spaces-in-continued-lines nil)
                          (single-line-comments t)
                          (accept-unicode-escapes nil)
                          (dollar-is-punctuation nil))
  (let ((lines (number-lines (stream-to-string-list character-stream) file-name)))
    (when substitute-trigraphs
      (dolist (line lines)
        (substitute-trigraphs line :warn-on-trigraph warn-on-trigraph)))
    (mapcar (lambda (line)
              (tokenize-line line
                             :accept-unicode-escapes accept-unicode-escapes
                             :dollar-is-punctuation dollar-is-punctuation))
            (remove-comments (merge-continued-lines lines
                                                    :warn-spaces-in-continued-lines
                                                    warn-spaces-in-continued-lines)
                             :single-line-comments single-line-comments))))


(defun sharpp (token)
  (and (typep token 'punctuation-token)
       (or (string= "#"  (token-text token)))))

(defun sharpsharpp (token)
  (and (typep token 'punctuation-token)
       (or (string= "##"  (token-text token)))))

(defun openp (token)
  (and (typep token 'punctuation-token)
       (or (string= "("  (token-text token)))))

(defun closep (token)
  (and (typep token 'punctuation-token)
       (or (string= ")"  (token-text token)))))

(defun commap (token)
  (and (typep token 'punctuation-token)
       (or (string= ","  (token-text token)))))

(defun ellipsisp (token)
  (and (typep token 'punctuation-token)
       (or (string= "..."  (token-text token)))))


(defun identifierp (token)
  (typep token 'identifier-token))


(defgeneric environment-macro-definedp (environment macro-name))
(defgeneric environment-macro-undefine (environment macro-name))
(defgeneric environment-macro-definition (environment macro-name))
(defgeneric (setf environment-macro-definition) (definition environment macro-name))

(defmethod environment-macro-definedp ((environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (nth-value 1 (gethash macro-name environment)))

(defmethod environment-macro-undefine ((environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (remhash macro-name environment))

(defmethod environment-macro-definition ((environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (gethash macro-name environment))

(defmethod (setf environment-macro-definition) (definition (environment hash-table) (macro-name string))
  (assert (eq 'equal (hash-table-test environment)))
  (setf (gethash macro-name environment) definition))



(defun parse-stringifies (line parameters)
  (loop
    :while line
    :if (sharpp (first line))
      :collect (let* ((sharp (pop line))
                      (file  (token-file sharp))
                      (lino  (token-line sharp)))
                 (let ((parameter (pop line)))
                   (if (or (null parameter)
                           (not (member (token-text parameter) parameters
                                        :key (function token-text)
                                        :test (function string=))))
                       (progn
                         (cerror "Continue" "~A:~A: '#' is not followed by a macro parameter" file lino)
                         (if (null parameter)
                             sharp
                             parameter))
                       `(:stringify ,parameter))))
    :else
      :collect (pop line)))

(defun parse-concatenates (line)
  (if (sharpsharpp (first line))
      (progn
        (cerror "Continue" "~A:~A: '##' cannot appear at either end of a macro expansion"
                (token-file (first line)) (token-line (first line)))
        line)
      (loop
        :with result = ()
        :while line
        :do (let ((curr (pop line)))
              (if (sharpsharpp (first line))
                  (let ((file (token-file (first line)))
                        (lino (token-line (first line))))
                    (push (loop
                            :with concat = (list curr)
                            :while (sharpsharpp (first line))
                            :do (pop line)
                                (unless line
                                  (cerror "Continue" "~A:~A: '##' cannot appear at either end of a macro expansion" file lino))
                                (unless (sharpsharpp (first line)) 
                                  (push (pop line) concat))
                            :finally (return `(:concatenate ,@(nreverse concat)))) result))
                  (push curr result)))
        :finally (return (nreverse result)))))

(defun parse-function-macro-definition-body (line parameters)
  (when line
    (parse-concatenates (parse-stringifies line parameters))))

(defun parse-object-macro-definition-body (line)
  (when line
    (parse-concatenates line)))


(defun parse-macro-definition (line)
  (cond
    ((null line)
     '())
    ((openp (first line))
     (let ((file (token-file (first line)))
           (lino (token-line (first line))))
       (pop line)
       (let ((parameters (loop
                           :collect (let ((parameter (pop line)))
                                      (cond
                                        ((identifierp parameter)
                                         (if (and line (ellipsisp (first line)))
                                             (progn
                                               (pop line)
                                               (unless (and line (closep (first line)))
                                                 (cerror "Continue" "~A:~A: ellipsis should be the last macro parameter"
                                                        (token-file parameter) (token-line parameter)))
                                               (list ':ellipsis parameter))
                                             (progn
                                               (unless (and line (or (commap (first line)) (closep (first line))))
                                                 (cerror "Continue" "~A:~A: Missing a comma after parameter ~A" 
                                                        (token-file parameter) (token-line parameter) (token-text parameter)))
                                               parameter)))
                                        ((ellipsisp parameter)
                                         (unless (and line (closep (first line)))
                                           (cerror "Continue" "~A:~A: ellipsis should be the last macro parameter"
                                                  (token-file parameter) (token-line parameter)))
                                         '(:ellipsis))
                                        (t
                                         (cerror "Continue" "~A:~A: Expected a macro parameter name, not ~S"
                                                 (token-file parameter) (token-line parameter) (token-text parameter))
                                         parameter)))
                           :while (and line (commap (first line)))
                           :do (pop line)
                           :finally (if (and line (closep (first line)))
                                        (pop line)
                                        (cerror "Continue" "~A:~A: Expected a closing parentheses after the parameter list" file lino)))))
         (list :function parameters (parse-function-macro-definition-body line parameters)))))
    (t
     (list :object (parse-object-macro-definition-body line)))))


(defun define (line environment)
  (let ((file (token-file (first line)))
        (lino (token-file (first line))))
    (pop line) (pop line)
    (if line
        (let ((name (pop line)))
          (if (identifierp name)
              (let ((old-definition (environment-macro-definition environment (token-text name)))
                    (new-definition (parse-macro-definition line)))
                (when (environment-macro-definedp environment (token-text name))
                  (unless (equal old-definition new-definition)
                    (warn "~A:~A: Redefiniting the macro ~A with a different definition"
                          (line-file name) (line-lino name) (token-text name))))
                (setf (environment-macro-definition environment (token-text name)) new-definition))
              (cerror "Continue" "~A:~A: Expected an identifier as macro name after #define, not ~S"
                      (line-file line) (line-lino line) (token-text name))))
        (cerror "Continue" "~A:~A: Missing macro name after #define" file lino))))

(defun undef (line environment)
  (let ((file (token-file (first line)))
        (lino (token-file (first line))))
    (pop line) (pop line)
    (if line
        (progn
          (let ((name (pop line)))
            (if (identifierp name)
                (environment-macro-undefine environment-macro-undefine (token-text name))
                (cerror "Continue" "~A:~A: Expected an identifier as macro name after #undef, not ~S"
                        (line-file name) (line-lino name) (token-text name))))
          (when line
            (cerror "Continue" "~A:~A: Didn't expect anything after the macro name after #undef, not ~S"
                    (line-file (first line)) (line-lino (first line)) (token-text (first line)))))
        (cerror "Continue" "~A:~A: Missing macro name after #undef" file lino))))


(defun include (line include-level environment)
  )
(defun import (line include-level environment)
  )
(defun ifdef (line lines if-level environment)
  )
(defun ifndef (line lines if-level environment)
  )
(defun cpp-if (line lines if-level environment)
  )
(defun cpp-line (line lines if-level environment)
  )
(defun pragma (line environment)
  )
(defun cpp-error (line environment)
  )
(defun cpp-warning (line environment)
  )

(defun cpp-macro-expand)
(defun process (tokenized-lines environment &key (if-level 0) (include-level 0))
  "
TOKENIZED-LINES: a list of list of tokens (one sublist per input line).
ENVIRONMENT:     an object with the ENVIRONMENT-MACRO-DEFINITION accessor,
                 where the macros, keyed by their name (string), are stored.
RETURN:          the C-pre-processed source in form of list of list of tokens
                 (one sublist per output line).
"
  (loop
    :with output = '()
    :while tokenized-lines
    :do (let ((line (pop tokenized-lines)))
          (if (and (sharpp (first line))
                   (identifierp (second line)))
              (scase (token-text (second line))
                (("define")  (define line environment))
                (("undef")   (undef  line environment))
                (("include") (nreconc (include line include-level environment) output))
                (("import")  (nreconc (import  line include-level environment) output))
                (("ifdef")   (setf tokenized-lines (ifdef  line tokenized-lines if-level environment)))
                (("ifndef")  (setf tokenized-lines (ifndef line tokenized-lines if-level environment)))
                (("if")      (setf tokenized-lines (cpp-if line tokenized-lines if-level environment)))
                (("elif" "else" "endif"))
                (("line")    (setf tokenized-lines (cpp-line line tokenized-lines environment)))
                (("pragma")  (pragma      line environment))
                (("error")   (cpp-error   line environment))
                (("warning") (cpp-warning line environment))
                (("ident" "sccs"))
                (otherwise (cerror "Continue" "~A:~A: invalid directive ~A"
                                   (line-file line) (line-lino line) (token-text (second line)))))
              ;; (multiple-value-setq (tokenized-lines output) (cpp-macro-expand line tokenized-lines output environment))
              (push line output)))
    :finally (return (nreverse output))))



#-(and) (progn

          (let ((file "tests/define.h"))
            (with-open-file (in file)
              (let ((environment (make-hash-table :test 'equal)))
                (process (read-cpp-tokens in
                                          :file-name file
                                          :substitute-trigraphs t
                                          :warn-on-trigraph nil)
                         environment)
                (print-hashtable environment))))
          
          
          
          
          (let ((file "tests/trigraphs.c"))
            (with-open-file (in file)
              (read-cpp-tokens in
                               :file-name file
                               :substitute-trigraphs t
                               :warn-on-trigraph nil)))

          (let ((file "tests/comment.c"))
            (with-open-file (in file)
              (read-cpp-tokens in
                               :file-name file
                               :substitute-trigraphs t
                               :warn-on-trigraph nil)))

          (let ((file "tests/test.c"))
            (with-open-file (in file)
              (read-cpp-tokens in
                               :file-name file
                               :substitute-trigraphs t
                               :warn-on-trigraph nil)))

          (let ((file #P"~/src/macosx/emacs-24.5/src/lisp.c"))
            (with-open-file (in file)
              (read-cpp-tokens in
                               :file-name file
                               :substitute-trigraphs t
                               :warn-on-trigraph nil)))
          )






;;; ---

(defun test/substitute-trigraphs ()
  (assert (equal (substitute-trigraphs (make-numbered-line :text (copy-seq "hello world")))
                 '("hello world" 1 "-")))
  (assert (equal (substitute-trigraphs (make-numbered-line :text (copy-seq "??( brackets ??)???<braces??>??=??'a??!??-; (??\\?) (a==b???-c:'??/n')")))
                 '("[ brackets ]?{braces}#^a|~; (??\\?) (a==b?~c:'\\n')" 1 "-")))
  :success)

(defun test/number-lines ()
  (assert (equal (number-lines '("a" "b" "c") "file.c" :start 2)
                 '(("a" 2 #1="file.c") ("b" 3 #1#) ("c" 4 #1#))))
  :success)

(defun test/merge-continued-lines ()
  (assert (equal (merge-continued-lines '(("first line" 1 #1="file.c")
                                   ("abc\\" 2 #1#)
                                   ("def\\" 3 #1#)
                                   ("ghi" 4 #1#)
                                   ("middle line" 5 #1#)
                                   ("abc \\" 6 #1#)
                                   ("def \\  " 7 #1#)
                                   ("ghi" 8 #1#)
                                   ("last line" 9 #1#))
                                 :warn-spaces-in-continued-lines nil)
                 '(("first line" 1 #2="file.c") ("abcdefghi" 2 #2#) ("middle line" 5 #2#) ("abc def ghi" 6 #2#) ("last line" 9 #2#))))
  :success)

(defun test/remove-comments-in-line ()
  (flet ((check (text &optional (state :top) comment-start)
           (multiple-value-list (remove-comments-in-line comment-start
                                                         (make-numbered-line :text text
                                                                             :lino 42 :file "hw.c")
                                                         state t))))
    (assert (equal (check "Hello world")
                   '(("Hello world" 42 "hw.c") :top)))
    ;; comment in string
    (assert (equal (check "Hello \"salut /* le */ monde\" world")
                   '(("Hello \"salut /* le */ monde\" world" 42 "hw.c") :top)))
    (assert (equal (check "Hello \"salut \\\"/* le */\\\" monde\" world")
                   '(("Hello \"salut \\\"/* le */\\\" monde\" world" 42 "hw.c") :top)))
    (assert (typep (nth-value 1 (ignore-errors (check "Hello \"salut"))) 'error))
    (assert (typep (nth-value 1 (ignore-errors (check "Hello \"salut\\"))) 'error))
    ;; comment in characters
    (assert (equal (check "Hello 'salut /* le */ monde' world")
                   '(("Hello 'salut /* le */ monde' world" 42 "hw.c") :top)))
    (assert (equal (check "Hello 'salut \\'/* le */\\' monde' world")
                   '(("Hello 'salut \\'/* le */\\' monde' world" 42 "hw.c") :top)))
    (assert (typep (nth-value 1 (ignore-errors (check "Hello 'salut"))) 'error))
    (assert (typep (nth-value 1 (ignore-errors (check "Hello 'salut\\"))) 'error))
    ;; single line comment
    (assert (equal (check "Hello//monde*/world")   '(("Hello" 42 "hw.c") :top)))
    ;; monoline block comment
    (assert (equal (check "Hello/*monde*/world")   '(("Hello world" 42 "hw.c") :top)))
    (assert (equal (check "Hello/*mon//de*/world") '(("Hello world" 42 "hw.c") :top)))
    (assert (equal (check "Hello/*mon/*de*/world") '(("Hello world" 42 "hw.c") :top)))
    ;; multiline block comment first line
    (assert (equal (check "Hello world/*salut") '(("Hello world" 42 "hw.c") :in-multiline-comment)))
    (assert (equal (check "Hello/*monde*/world/*salut") '(("Hello world" 42 "hw.c") :in-multiline-comment)))
    ;; multiline block comment in the middle lines
    (assert (equal (check "in the middle comment" :in-multiline-comment '("Hello world" 42 "hw.c"))
                   '(("Hello world" 42 "hw.c") :in-multiline-comment)))
    (assert (equal (check "in /* the // middle comment" :in-multiline-comment '("Hello world" 42 "hw.c"))
                   '(("Hello world" 42 "hw.c") :in-multiline-comment)))
    ;; multiline block comment in the end line
    (assert (equal (check "end comment */end line" :in-multiline-comment '("Hello world" 42 "hw.c"))
                   '(("Hello world end line" 42 "hw.c") :top)))
    (assert (equal (check "end comment */end/*fin*/line" :in-multiline-comment '("Hello world" 42 "hw.c"))
                   '(("Hello world end line" 42 "hw.c") :top)))
    (assert (equal (check "end // comment */end/*fin*/line" :in-multiline-comment '("Hello world" 42 "hw.c"))
                   '(("Hello world end line" 42 "hw.c") :top)))
    (assert (equal (check "end // comment */end/*fin*/line// c'est fini" :in-multiline-comment '("Hello world" 42 "hw.c"))
                   '(("Hello world end line" 42 "hw.c") :top)))
    :success))

(defun test/remove-comments ()
  (assert (equal (remove-comments '(("Line one" 1 #1="test.c")
                                    ("Line/**/two" 2 #1#)
                                    ("Line/*three*/3" 3 #1#)
                                    ("Line/*4*/four/*and*/some" 4 #1#)
                                    ("Line/*-*/five/*a//nd*/some" 5 #1#)
                                    ("Line/*/five/*a//nd some" 6 #1#)
                                    ("Line//6 */seven" 7 #1#)
                                    ("Line/*/eight/*a//nd some" 8 #1#)
                                    ("Line nine" 9 #1#)
                                    ("Line//9 */ten" 10 #1#)
                                    ("Line \"ele/*v*/en\"--" 11 #1#)
                                    ("Line 'tw//elv/*e*/'--" 12 #1#)))
                 '(("Line one" 1 #2="test.c")
                   ("Line two" 2 #2#)
                   ("Line 3" 3 #2#)
                   ("Line four some" 4 #2#)
                   ("Line five some" 5 #2#)
                   ("Line seven" 6 #2#)
                   ("Line ten" 8 #2#)
                   ("Line \"ele/*v*/en\"--" 11 #2#)
                   ("Line 'tw//elv/*e*/'--" 12 #2#))))
  :success)

(defun test/scan-identifier ()
  (assert (equal (multiple-value-list (scan-identifier '("   Hello world " 42 "t.c") 3 "_$" :accept-unicode-escapes t))
                 '("Hello" 8)))
  (assert (equal (multiple-value-list (scan-identifier '("   Hello _wo$rl42d "  42 "t.c") 9 "_$" :accept-unicode-escapes t))
                 '("_wo$rl42d" 18)))
  (assert (equal (multiple-value-list (scan-identifier '("   Hello _\\u0145teve "  42 "t.c") 9 "_$" :accept-unicode-escapes t))
                 '("_\\u0145teve" 20)))
  (assert (equal (multiple-value-list (scan-identifier '("   Hello _\\U0145BABEteve "  42 "t.c") 9 "_$" :accept-unicode-escapes t))
                 '("_\\U0145BABEteve" 24)))
  (assert (equal (multiple-value-list (scan-identifier '("   Hello _world\\u014 "  42 "t.c") 9 "_$" :accept-unicode-escapes t))
                 '("_world" 15)))
  (assert (equal (multiple-value-list (scan-identifier '("   Hello _world\\U0145BABXteve " 42 "t.c") 9 "_$" :accept-unicode-escapes t))
                 '("_world" 15)))
  :success)

(defun test/scan-number ()
  (assert (equal (multiple-value-list (scan-number '("   123 " 42 "t.c") 3))
                 '("123" 6)))
  (assert (equal (multiple-value-list (scan-number '("   0xBABE42 _wo$rl42d " 42 "t.c") 3))
                 '("0xBABE42" 11)))
  (assert (equal (multiple-value-list (scan-number '("   0xBABE42+42 " 42 "t.c") 3))
                 '("0xBABE42" 11)))
  (assert (equal (multiple-value-list (scan-number '("   0xE+12/42 " 42 "t.c") 3))
                 '("0xE+12" 9)))
  (assert (equal (multiple-value-list (scan-number '("   0.123_4e-56*32 " 42 "t.c") 3))
                 '("0.123_4e-56" 14)))
  (assert (equal (multiple-value-list (scan-number '("   .9999+.1111 " 42 "t.c") 3))
                 '(".9999" 8)))
  :success)

(defun test/scan-punctuation ()
  (assert (equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 0))
                 '("{" 1)))
  (assert (equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 1))
                 '("&=" 3)))
  (assert (equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 3))
                 '("==" 5)))
  (assert (equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 5))
                 '(">>=" 8)))
  (assert (equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 8))
                 '("=" 9)))
  (assert (equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 9))
                 '("..." 12)))
  :success)

(defun test/all ()
  (test/number-lines)
  (test/substitute-trigraphs)
  (test/merge-continued-lines)
  (test/remove-comments-in-line)
  (test/remove-comments)
  (test/scan-identifier)
  (test/scan-number)
  (test/scan-punctuation))

(test/all)

;;;; THE END ;;;;
