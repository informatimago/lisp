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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LANGUAGES.CPP")




;;; --------------------

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
                (cpp-warning line "found trigraph ??~A converted to ~A" (aref text (+ 2 i)) tri))
              (setf (aref text j) tri)
              (incf j)
              (incf i 3)
        :else
          :do (setf (aref text j) (aref text i))
              (incf j)
              (incf i)
        :finally (setf (line-text line) (subseq text 0 j)))))
  line)


;;; --------------------

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
                                           (cpp-warning (pseudo-token file lino) "spaces after line continuation character"))
                                         (push (subseq (line-text line) 0 continuedp) result)
                                         (setf line (pop lines))
                                         (if (null line)
                                             (progn
                                               (cpp-warning (pseudo-token file lino) "last line is a continued line")
                                               (setf continuedp nil))
                                             (multiple-value-setq (continuedp spacesp) (continued-line-p (line-text line))))
                                     :finally (push (line-text line) result)
                                              (return (nreverse result))))
                        :lino lino
                        :file file))
                     line)))))


;;; --------------------

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
                        (progn (cpp-error (pseudo-token file lino) "backslash in string literal at the end of the line")
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
                        (progn (cpp-error (pseudo-token file lino) "backslash in character literal at the end of the line")
                               (setf state :top))))
                   ((#\')
                    (incf i)
                    (setf state :top))
                   (otherwise (incf i))))))
        :finally (return (case state
                           (:in-string
                            (cpp-error (pseudo-token file lino) "unterminated string literal at the end of the line")
                            (values (concatenate-chunks (nreverse chunks)) :top))
                           (:in-character
                            (cpp-error (pseudo-token file lino) "unterminated character literal at the end of the line")
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
                   (cpp-error (pseudo-token file lino) "end of file before end of multiline comment")
                   (setf new-state :top))
                 (setf state new-state)
                 new-line))))

;;; --------------------

(defparameter *whitespaces* #(#\space #\tab #\vt #\page #\nul #\newline #\return #\linefeed))

(defun skip-spaces (text start)
  (loop
    :while (and (< start (length text))
                (find (aref text start) *whitespaces*))
    :do (incf start))
  start)

(defun skip-spaces-but-one (text start)
  (let ((start (skip-spaces text start)))
    (when (and (plusp start)
               (find (aref text (1- start)) *whitespaces*))
      (decf start))
    start))

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
                 (cpp-error line "unterminated ~:[string~;character~] literal ending with incomplete escape"
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
         (cpp-error line "invalid punctuation: ~S" ch)
         (values "?" (1+ start)))))))


(defun punctuatorp (ch)
  (find ch "!#%&()*+,-./:;<=>?[\\]^{|}~"))


(defun tokenize-line (line &key (accept-unicode-escapes nil)
                             (dollar-is-punctuation nil))
  (destructuring-bind (text lino file) line
    (loop
      :with first-identifier   := (if dollar-is-punctuation
                                      "_"
                                      "_$")
      :with start              := 0
      :with header             := 1 ; we track #import and #include to allow <header.h> delimited literals.
      :with record-space-token := nil ; we track #define to detect the same in NAME ( vs. NAME(
      :do (setf start (ecase record-space-token
                        ((nil)
                         (skip-spaces text start))
                        ((:before-name)
                         (setf record-space-token :after-name)
                         (skip-spaces text start))
                        (:after-name
                         (setf record-space-token nil)
                         (skip-spaces-but-one text start))))
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
                      (when (eql 2 header)
                        (when (and (null record-space-token)
                                   (string= "define" token))
                          (setf record-space-token :before-name))
                        (setf header (if (or (string= "include" token)
                                             (string= "import" token))
                                         3
                                         nil)))
                      (prog1 (make-identifier token start lino file)
                        (setf start end))))
                   ((char= ch #\space)
                    (prog1 (make-punctuation " " start lino file)
                      (incf start)))
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
                   ((and (eql 3 header) (char= #\< ch))
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
                      (incf start))))))))

;;; --------------------


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


;;;; --------------------
;;;; Processing directives
;;;; --------------------

(defmacro with-cpp-line (line &body body)
  (let ((vtoken (gensym)))
    `(let* ((,vtoken (pop ,line))
            (file (token-file ,vtoken))
            (lino (token-line ,vtoken)))
       (update-context *context* :file file :line lino :column (token-column ,vtoken) :token ,vtoken)
       (pop ,line)
       (locally
           ,@body))))

;;; --------------------
;;; #define
;;; --------------------

(defun parse-stringifies (line parameters)
  (loop
    :while line
    :if (sharpp (first line))
      :collect (let* ((sharp (pop line))
                      (file  (token-file sharp))
                      (lino  (token-line sharp)))
                 (let ((parameter (pop line)))
                   (if (or (null parameter)
                           (not (find-if (lambda (par)
                                           (string= (token-text parameter)
                                                    (token-text (if (and (listp par)
                                                                         (eq :ellipsis (first par)))
                                                                    (second par)
                                                                    par))))
                                         parameters)))
                       (progn
                         (cpp-error (pseudo-token file lino) "'#' is not followed by a macro parameter")
                         (if (null parameter)
                             sharp
                             parameter))
                       `(:stringify ,parameter))))
    :else
      :collect (pop line)))

(defun check-concatenates (line)
  ;; ^ ## A and A ## $ are invalid.
  ;; A ## ## B // valid because A ## empty = A
  (loop
    :while (and line (sharpsharpp (first line)))
    :do (cpp-error (first line) "'##' cannot appear at either end of a macro expansion")
        (pop line))
  (loop
    :while (and line (sharpsharpp (first (last line))))
    :do (cpp-error (first line) "'##' cannot appear at either end of a macro expansion")
        (setf line (butlast line)))
  line)

(defun parse-function-macro-definition-body (line parameters)
  (when line
    (check-concatenates (parse-stringifies line parameters))))

(defun parse-object-macro-definition-body (line)
  (when line
    (check-concatenates line)))

(defun parse-macro-definition (name line)
  (cond
    ((null line)
     (make-instance 'macro-definition/object
                    :name name
                    :expansion '()))
    ((openp (first line))
     (let ((file (token-file (first line)))
           (lino (token-line (first line))))
       (pop line)
       (let ((parameters (loop
                           :with result := '()
                           :for parameter := (first line)
                           :unless (closep parameter)
                             :do (let ((par (cond
                                              ((identifierp parameter)
                                               (pop line)
                                               (if (and line (ellipsisp (first line)))
                                                   (progn
                                                     (pop line)
                                                     (unless (and line (closep (first line)))
                                                       (cpp-error parameter "ellipsis should be the last macro parameter"))
                                                     (list :ellipsis parameter))
                                                   (progn
                                                     (unless (and line (or (commap (first line)) (closep (first line))))
                                                       (cpp-error "Missing a comma after parameter ~A"  (token-text parameter)))
                                                     parameter)))
                                              ((ellipsisp parameter)
                                               (pop line)
                                               (unless (and line (closep (first line)))
                                                 (cpp-error parameter "ellipsis should be the last macro parameter"))
                                               (list :ellipsis (make-identifier "__VA_ARGS__" 0 0 "-")))
                                              (t
                                               (cpp-error parameter "Expected a macro parameter name, not ~S" (token-text parameter))
                                               (unless (commap parameter)
                                                 (pop line))
                                               nil))))
                                   (when par (push par result)))
                           :while (and line (commap (first line)))
                           :do (pop line) ; comma
                           :finally (if (and line (closep (first line)))
                                        (pop line)
                                        (cpp-error (pseudo-token file lino) "Expected a closing parentheses after the parameter list"))
                                    (return (nreverse result)))))
         (make-instance 'macro-definition/function
                        :name name
                        :parameters parameters
                        :expansion (parse-function-macro-definition-body line parameters)))))
    ((spacep (first line))
     (pop line)
     (make-instance 'macro-definition/object
                    :name name
                    :expansion (parse-object-macro-definition-body line)))
    (t
     (make-instance 'macro-definition/object
                    :name name
                    :expansion (parse-object-macro-definition-body line)))))

(defmethod define ((context context))
  (with-cpp-line (context-current-line context)
    (if (context-current-line context)
        (let ((name (pop (context-current-line context))))
          (if (identifierp name)
              (let ((old-definition (environment-macro-definition (context-environment context) (token-text name)))
                    (new-definition (parse-macro-definition name (context-current-line context))))
                (when (environment-macro-definedp (context-environment context) (token-text name))
                  (unless (equal old-definition new-definition)
                    (cpp-warning name "Redefiniting the macro ~A with a different definition" (token-text name))))
                (setf (environment-macro-definition (context-environment context) (token-text name)) new-definition))
              (cpp-error (first (context-current-line context))
                         "Didn't expect anything after the macro name after #define, not ~S"
                         (token-text (first (context-current-line context))))))
        (cpp-error context "Missing macro name after #define"))))

;;; --------------------
;;; #undef
;;; --------------------

(defmethod undef ((context context))
  (with-cpp-line (context-current-line context)
   (if (context-current-line context)
       (let ((name (pop (context-current-line context))))
         (if (identifierp name)
             (environment-macro-undefine (context-environment context) (token-text name))
             (cpp-error name "Expected an identifier as macro name after #undef, not ~S" (token-text name)))
         (when (context-current-line context)
           (cpp-error (first (context-current-line context))
                      "Didn't expect anything after the macro name after #undef, not ~S"
                      (token-text (first (context-current-line context))))))
       (cpp-error context "Missing macro name after #undef"))))



;;; --------------------
;;; #include & #import
;;; --------------------

;; TODO: implement caching of already included files with #ifndef/#define and #import.

;; #include <> searches files only in *include-bracket-directories*
;;
;; #include "" searches files
;; in the current directory unless *include-disable-current-directory* is true,
;; then in *include-quote-directories*
;; and finally in *include-bracket-directories*.

(defun search-file-in-directories (include-file directories kind directive)
  (loop
    :with include-search-functions := (option *context* :include-search-functions)
    :for directory :in directories
    :for path := (if (keywordp directory)
                     (let ((search-function (cdr (assoc directory include-search-functions))))
                       (if search-function
                           (funcall search-function include-file kind directive)
                           (progn
                             (cpp-warning "No search function for key ~S" directory)
                             nil)))
                     (merge-pathnames include-file directory))
    :when (and path (or (eq t path) (probe-file path)))
      :do (return path)
    :finally (return nil)))

(defun include-directories (kind)
  (let ((include-disable-current-directory (option *context* :include-disable-current-directory))
        (include-quote-directories         (option *context* :include-quote-directories))
        (include-bracket-directories       (option *context* :include-bracket-directories)))
    (append (if (eq kind :quote)
                (remove-duplicates
                 (append (unless include-disable-current-directory
                           (list (make-pathname :name nil :type nil :version nil)))
                         include-quote-directories)
                 :test (function equal))
                '())
            (remove-duplicates include-bracket-directories :test (function equal)))))

(defmethod perform-include ((context context) include-file kind directive)
  (flet ((include (path)
           (read-and-process-file context path)))
    (let* ((include-directories (include-directories kind))
           (path                (search-file-in-directories include-file include-directories kind directive)))
      (cond ((eq t path) #|done|#)
            (path        (include path))
            (t           (cpp-error context
                                    "Cannot find a file ~C~A~C in the include directories ~S"
                                    (if (eq kind :quote) #\" #\<)
                                    include-file
                                    (if (eq kind :quote) #\" #\>)
                                    include-directories))))))

(defgeneric token-string (token)
  (:method ((token token)) (token-text token))
  (:method ((token string-literal-token))
    (with-input-from-string (in (token-text token))
      (read-c-string in (read-char in))))
  (:method ((token character-literal-token))
    (with-input-from-string (in (token-text token))
      (read-c-string in (read-char in)))))

;; (princ (token-string (make-instance 'string-literal-token :text "\"abc\\ndef\\t\\xe9t\\xe9\\a\"")))

(defun extract-path (directive line)
  (let ((token (first line)))
    (cond
      ((string-literal-p token)
       (let ((text (token-text token)))
         (cond ((zerop (length text))
                (cpp-error token "Invalid empty path")
                (values "" :quote (rest line)))
               ((char= #\< (aref text 0))
                (values (subseq text 1 (1- (length text))) :bracket (rest line)))
               (t
                (values (token-string token) :quote (rest line))))))
      ((open-bracket-p token)
       (pop line)
       (values (mapconcat (function token-text) (loop
                                                  :with item
                                                  :while line
                                                  :do (setf item (pop line))
                                                  :until (close-bracket-p item)
                                                  :collect item) "") :bracket line))
      (t
       (cpp-error (first line) "In directive ~A, invalid path ~S"
                  directive (mapconcat (function token-text) line ""))
       (values nil nil nil)))))




(defmethod include-common ((context context) directive)
  (with-cpp-line (context-current-line context)
    (if (context-current-line context)
        ;; macro-functions must stand on a single line after #include/#import.
        (let ((line (first (macro-expand-macros context (context-current-line context)
                                                '() '() nil '()))))
          (multiple-value-bind (path kind line) (extract-path directive line)
            (when path
              (perform-include context path kind directive))
            (when line
              (cpp-error (first line) "Didn't expect anything after the path after #~(~A~), not ~S"
                         directive (token-text (first line))))))        
        (cpp-error context "Missing path after #~(~A~)" directive)))
  context)

(defmethod include ((context context))
  (include-common context :include))

(defmethod import ((context context))
  (include-common context :import))


;;; --------------------
;;; #ifdef
;;; --------------------

(defun parse-single-macro-name (line where)
  (cond
    ((null line)
     (cpp-error *context* "Missing a macro name after ~A" where)
     nil)
    ((cddr line)
     (cpp-error (first line) "Unexpected tokens after macro name ~A after ~A" (token-text (first line)) where)
     nil)
    ((identifierp (first line))
     (first line))
    (t
     (cpp-error (first line) "Invalid macro name ~A after ~A" (token-text (first line)) where)
     nil)))

(defmacro define-cpp-line-predicate (name key)
  `(defun ,name (line)
     (and (cdr line)
          (sharpp (car line))
          (string= ,key (token-text (cadr line))))))
(define-cpp-line-predicate if-line-p     "if")
(define-cpp-line-predicate ifdef-line-p  "ifdef")
(define-cpp-line-predicate ifndef-line-p "ifndef")
(define-cpp-line-predicate elif-line-p   "elif")
(define-cpp-line-predicate else-line-p   "else")
(define-cpp-line-predicate endif-line-p  "endif")

(defmethod skip-if ((context context))
  ;; PRE: current line is #if #ifdef #ifndef #elif or #else
  ;; POST: current line is nil or #endif
  ;; skips until the matching #endif
  (let ((if-line (context-current-line context)))
    (setf (context-current-line context) nil)
    (incf (context-if-level context))
    (unwind-protect
         (loop
           :while (context-input-lines context)
           :do (let ((line (pop (context-input-lines context))))
                 (cond ((or (if-line-p line)
                            (ifdef-line-p line)
                            (ifndef-line-p line))
                        (skip-if context))
                       ((endif-line-p line)
                        (setf (context-current-line context) line)
                        (return t))))
           :finally (cpp-error if-line "End of file reached before a balanced #endif for #~A"
                               (token-text (second if-line)))
                    (return nil))
      (decf (context-if-level context)))))

(defmethod skip-branch ((context context))
  ;; skips a single branch
  ;; PRE:  current line is #if #ifdef #ifndef or #elif
  ;; POST: current line is nil, #elif #else or #endif
  (let ((if-line (context-current-line context)))
    (setf (context-current-line context) nil)
    (loop
      :while (context-input-lines context)
      :do (let ((line (pop (context-input-lines context))))
            (cond ((or (if-line-p line)
                       (ifdef-line-p line)
                       (ifndef-line-p line))
                   (skip-if context))
                  ((or (elif-line-p line)
                       (else-line-p line)
                       (endif-line-p line))
                   (setf (context-current-line context) line)
                   (return t))))
      :finally (cpp-error if-line "End of file reached before a balanced #endif for #~A"
                          (token-text (second if-line)))
               (return nil))))

(defmethod process-branch-and-skip ((context context) &optional no-else)
  ;; current line is #if #ifdef #ifndef or #elif
  ;; processes the branch,
  ;; and then skip the branches until #endif
  ;; if no-else, the signals an error if a #else or #elif is found.
  ;; (there should be no-else after an #else).
  (flet ((check-no-else ()
           (when no-else
             (cpp-error (context-current-line context) "Found a #~A line after a #else"
                        (token-text (second (context-current-line context)))))))
    (process-file context) ; shall read the input-lines till the #elif #else or #endif
    (loop
      :while (elif-line-p (context-current-line context))
      :do (check-no-else)
          (skip-branch context))
    (when (else-line-p (context-current-line context))
      (check-no-else)
      (skip-branch context))
    (unless (or (null (context-current-line context))
                (endif-line-p (context-current-line context)))
      (check-no-else))
    (loop
      :until (or (null (context-current-line context))
                 (endif-line-p (context-current-line context)))
      :do (skip-branch context))))

#-(and) (
         
         (let ((*context*  (make-instance 'context
                                          :current-line (list (make-punctuation "#") (make-identifier "ifdef") (make-identifier "YES"))
                                          :input-lines  (list (list (make-number "1"))
                                                              (list (make-punctuation "#") (make-identifier "else"))
                                                              (list (make-number "2"))
                                                              (list (make-punctuation "#") (make-identifier "endif")))
                                          :if-level 1)))
           (process-branch-and-skip *context*)
           (write-processed-lines (reverse (context-output-lines *context*))))

                  
         (let ((*context*  (make-instance 'context
                                          :input-lines  (list (list (make-punctuation "#") (make-identifier "define") (make-identifier "YES")  (make-number "1"))
                                                              (list (make-punctuation "#") (make-identifier "ifdef") (make-identifier "YES"))
                                                              (list (make-number "1"))
                                                              (list (make-punctuation "#") (make-identifier "else"))
                                                              (list (make-number "2"))
                                                              (list (make-punctuation "#") (make-identifier "endif"))))))
           (process-file *context*)
           (write-processed-lines (reverse (context-output-lines *context*))))

         (let ((*context*  (make-instance 'context
                                          :input-lines  (list (list (make-punctuation "#") (make-identifier "define") (make-identifier "YES")  (make-number "1"))
                                                              (list (make-punctuation "#") (make-identifier "ifdef") (make-identifier "NO"))
                                                              (list (make-number "1"))
                                                              (list (make-punctuation "#") (make-identifier "else"))
                                                              (list (make-number "2"))
                                                              (list (make-punctuation "#") (make-identifier "endif"))))))
           (process-file *context*)
           (write-processed-lines (reverse (context-output-lines *context*))))

         (let ((*context*  (make-instance 'context
                                          :input-lines  (list (list (make-punctuation "#") (make-identifier "define") (make-identifier "YES")  (make-number "1"))
                                                              (list (make-punctuation "#") (make-identifier "ifdef") (make-identifier "NO"))
                                                              (list (make-number "1"))
                                                              (list (make-punctuation "#") (make-identifier "elif") (make-identifier "defined") (make-identifier "YES"))
                                                              (list (make-number "2"))
                                                              (list (make-punctuation "#") (make-identifier "else"))
                                                              (list (make-number "3"))
                                                              (list (make-punctuation "#") (make-identifier "endif"))))))
           (process-file *context*)
           (write-processed-lines (reverse (context-output-lines *context*))))
         


         
         
         )


(defmethod skip-branch-and-process ((context context))
  ;; line is #if #ifdef #ifndef #elif
  ;; skip the branches,
  ;; and then process the next #elif or #else branch.
  ;; and then skip the end.
  ;; Note: #ifdef … #elif … #else … #endif is valid.
  (skip-branch context)
  (loop
    :while (and (elif-line-p (context-current-line context))
                (not (cpp-evaluate-expression context (cddr (context-current-line context)))))
    :do (skip-branch context))
  (if (elif-line-p (context-current-line context))
      (process-branch-and-skip context)
      (progn
        (loop
          :until (or (else-line-p (context-current-line context))
                     (endif-line-p (context-current-line context)))
          :do (skip-branch context))
        (when (else-line-p (context-current-line context))
          (process-branch-and-skip context :no-else)))))

(defmethod ifdef-common ((context context) flip directive)
  (incf (context-if-level context))
  (unwind-protect
       (let ((name (parse-single-macro-name (cddr (context-current-line context)) directive)))
         (cond
           ((null name)
            (skip-if context))
           ((funcall flip (environment-macro-definedp (context-environment context) (token-text name)))
            (process-branch-and-skip context))
           (t
            (skip-branch-and-process context))))
    (decf (context-if-level context))))

(defmethod ifdef ((context context))
  (ifdef-common context (function identity) "#ifdef"))

(defmethod ifndef ((context context))
  (ifdef-common context (function not) "#ifndef"))

(defun cpp-evaluate-expression (context line)
  (not (zerop (eval (parse-expression context
                                      (first (macro-expand-macros context line
                                                                  '() '() :allow-defined '())))))))

(defmethod cpp-if ((context context))
  (if (cpp-evaluate-expression context (cddr (context-current-line context)))
      (process-branch-and-skip context)
      (skip-branch-and-process context)))


;;; --------------------
;;; #line
;;; --------------------

(defmethod cpp-line ((context context))
  ;; TODO:
  context)

;;; --------------------
;;; #pragma
;;; --------------------

(defmethod pragma ((context context))
  ;; TODO: unrecognized pragmas could be passed along on the output for the compiler.
  (with-cpp-line (context-current-line context)
    (when (context-current-line context)
      (let ((key (pop (context-current-line context))))
        (when (identifierp key)
          (let ((interpreter (gethash (token-text key) (context-pragma-interpreters context))))
            (when interpreter
              (funcall interpreter context (context-current-line context)))))))))

;;; --------------------
;;; #error
;;; --------------------

(defmethod cpp-error-line ((context context))
  (cpp-message 'cpp-error (context-current-line context)))

;;; --------------------
;;; #warning
;;; --------------------

(defmethod cpp-warning-line ((context context))
  (cpp-message 'cpp-warning (context-current-line context)))

;;; --------------------

(defun cpp-message (operation line)
  (let ((directive (second line)))
    (with-cpp-line line
      (if line
          (funcall operation directive "~{~A~^ ~}" (mapcar (function token-text) line))
          (cpp-error directive "Missing an expression after #~(~A~)" directive)))))

;;; --------------------
;;; pre-processing files
;;; --------------------


(defmethod cpp-macro-expand ((context context))
  (multiple-value-bind (output input) (macro-expand-macros context
                                                           (context-current-line context)
                                                           (context-input-lines context)
                                                           (context-output-lines context)
                                                           nil
                                                           (context-macros-being-expanded context))
    (setf (context-output-lines context) output
          (context-input-lines context) input)
    context))

(defmethod process-file ((context context))
  "Processes all the INPUT-LINES, pushing onto the OUTPUT-LINES."
  (loop
    :while (context-input-lines context)
    :do (let ((line (pop (context-input-lines context))))
          (setf (context-current-line context) line)
          ;;DEBUG;; (print line)
          (if (sharpp (first line))
              (cond
                ((identifierp (second line))
                 (scase (token-text (second line))
                   (("define")  (define  context))
                   (("undef")   (undef   context))
                   (("include") (include context))
                   (("import")  (import  context))
                   (("ifdef")   (ifdef   context))
                   (("ifndef")  (ifndef  context))
                   (("if")      (cpp-if  context))
                   (("elif" "else" "endif")
                    (if (plusp (context-if-level context))
                        (return)
                        (cpp-error (second line) "#~A without #if" (token-text (second line)))))
                   (("line")    (cpp-line context))
                   (("pragma")  (pragma           context))
                   (("error")   (cpp-error-line   context))
                   (("warning") (cpp-warning-line context))
                   (("ident" "sccs"))
                   (otherwise (cpp-error line "invalid directive ~A" (token-text (second line))))))
                ((number-token-p (second line)) ;; skip # 1 "file"
                 (push line (context-output-lines context)))
                ((rest line)
                 (cpp-error line "invalid directive #~A" (token-text (second line))))
                (t ;; skip # alone.
                 ))
              (cpp-macro-expand context)))
    :finally (setf (context-current-line context) nil))
  context)

(defmethod read-and-process-file ((context context) path)
  (with-open-file (input path :external-format (option *context* :external-format))
    (context-push-file context path (read-cpp-tokens
                                     input
                                     :file-name (namestring path)
                                     :substitute-trigraphs            (option *context* :substitute-trigraphs)
                                     :warn-on-trigraph                (option *context* :warn-on-trigraph)
                                     :warn-spaces-in-continued-lines  (option *context* :warn-spaces-in-continued-lines)
                                     :single-line-comments            (option *context* :single-line-comments)
                                     :accept-unicode-escapes          (option *context* :accept-unicode-escapes)
                                     :dollar-is-punctuation           (option *context* :dollar-is-punctuation)))
    (process-file context)
    (context-pop-file context)))

(defun process-toplevel-file (path &key (options *default-options*))
  (let ((*context* (make-instance 'context :base-file path :file path :options options)))
    (read-and-process-file *context* path)
    (values (reverse (context-output-lines *context*)) *context*)))

(defun write-processed-lines (lines &optional (*standard-output* *standard-output*))
  (when lines
    (let ((*print-circle* nil))
     (loop
       :with file := nil
       :with lino := nil
       :for line :in lines
       :when line
         :do (if (and (equal file (token-file (first line)))
                      lino
                      (= (1+ lino) (token-line (first line))))
                 (incf lino)
                 (format t "#line ~D ~S~%"
                         (setf lino (token-line (first line)))
                         (setf file (token-file (first line)))))
             (format t "~{~A~^ ~}~%" (mapcar (function token-text) line))))))

(defmacro with-cpp-error-logging (&body body)
  `(handler-bind ((cpp-error (lambda (condition) 
                              (princ condition *error-output*) (terpri *error-output*)
                              (let ((restart (find-restart 'continue condition)))
                                (when restart (invoke-restart restart)))))
                 (cpp-warning (lambda (condition) 
                                (princ condition *error-output*) (terpri *error-output*)
                                (let ((restart (find-restart 'muffle-warning condition)))
                                  (when restart (invoke-restart restart))))))
    ,@body))

(defun cpp-e (path &optional includes)
  (with-cpp-error-logging
    (multiple-value-bind (lines context)
        (process-toplevel-file path :options (acons :include-quote-directories includes *default-options*))
      (terpri)
      (write-processed-lines lines)
      ;; (print-hashtable (context-environment context))
      context)))

;;; --------------------

#-(and) (progn

          (cpp-e "tests/test.c" '("tests/"))
          (cpp-e "tests/variadic.c" '("tests/"))
          (cpp-e "tests/built-ins.c" '("tests/"))
          (cpp-e "tests/comment.c" '("tests/"))
          (cpp-e "tests/concat.c" '("tests/"))
          (cpp-e "tests/interface.c" '("tests/"))
          (cpp-e "tests/shadow.c" '("tests/"))
          (cpp-e "tests/stringify.c" '("tests/"))
          (cpp-e "tests/substitute.c" '("tests/"))
          (cpp-e "tests/trigraphs.c" '("tests/"))
          (cpp-e "tests/errors.c" '("tests/"))


          ;; bugged
          (cpp-e "tests/recursive.c" '("tests/"))
          (cpp-e "tests/empty-macro.c" '("tests/"))
          
          
          (let ((file "tests/define.h"))
            (with-open-file (in file)
              (let ((environment (make-environment)))
                (process-file (read-cpp-tokens in
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

(defun test/scan-delimited-literal ()
  (assert (equal (multiple-value-list (scan-delimited-literal '("'e'" 42 "test.c") 0))
                 '("'e'" 3)))
  (assert (equal (multiple-value-list (scan-delimited-literal '("'\\x41'" 42 "test.c") 0))
                 '("'\\x41'" 6)))
  :success)

(defun text/skip-spaces ()
  (assert (equal (skip-spaces "    xyz()" 0) 4))
  (assert (equal (skip-spaces "    xyz()" 7) 7))
  (assert (equal (skip-spaces "    xyz ()" 7) 8))
  (assert (equal (skip-spaces-but-one "    xyz()" 0) 3))
  (assert (equal (skip-spaces-but-one "    xyz()" 7) 7))
  (assert (equal (skip-spaces-but-one "    xyz ()" 7) 7))
  :success)

(defun test/extract-path ()
  (assert (equal (multiple-value-list (extract-path "#include"
                                                    (list (make-instance 'string-literal-token
                                                                         :text "\"/usr/local/include/\\xe9t\\xe9.h\""))))
                 '("/usr/local/include/été.h" :quote nil)))
  (assert (equal (multiple-value-list (extract-path "#include"
                                                    (list (make-instance 'string-literal-token
                                                                         :text "</usr/local/include/\\xe9t\\xe9.h>"))))
                 '("/usr/local/include/\\xe9t\\xe9.h" :bracket nil)))
  (assert (equal (multiple-value-list (extract-path "#include"
                                                    (list (make-instance 'punctuation-token :text "<")
                                                          (make-instance 'string-literal-token
                                                                         :text "/usr/local/include")
                                                          (make-instance 'string-literal-token
                                                                         :text "/file.h")
                                                          (make-instance 'punctuation-token :text ">"))))
                 '("/usr/local/include/file.h" :bracket nil)))
  :success)

(defun test/parse-function-macro-call-arguments ()
  (let ((*context*       (make-instance 'context :base-file "test.c" :file "test.c"))
        (tokenized-lines (list (list (make-instance 'identifier-token :text "next_line"))))
        (after           (make-instance 'identifier-token :text "same_line"))
        (foo             (make-instance 'identifier-token :text "FOO"))
        (arg1            (make-instance 'identifier-token :text "arg1"))
        (arg2            (make-instance 'identifier-token :text "arg2"))
        (plus            (make-instance 'punctuation-token :text "+"))
        (minus           (make-instance 'punctuation-token :text "-"))
        (comma           (make-instance 'punctuation-token :text ","))
        (left            (make-instance 'punctuation-token :text "("))
        (right           (make-instance 'punctuation-token :text ")")))
    (flet ((check (result expected)
             (assert (equal result expected)
                     () "Assertion failed: equal ~%   result   = ~S~%   expected = ~S~%"
                     result expected)))
      (check (multiple-value-list
              (parse-function-macro-call-arguments foo (list left right after)
                                                   tokenized-lines))
             (list '(())
                   (list after)
                   tokenized-lines))

      (check (multiple-value-list
              (parse-function-macro-call-arguments foo (list left comma right after)
                                                   tokenized-lines))
             (list '(() ())
                   (list after)
                   tokenized-lines))

      (check (multiple-value-list
              (parse-function-macro-call-arguments foo (list left arg1 right after)
                                                   tokenized-lines))
             `(((,arg1))
               (,after)
               ,tokenized-lines))
      
      (check (multiple-value-list
              (parse-function-macro-call-arguments foo (list left arg1 comma arg2 right after)
                                                   tokenized-lines))
             `(((,arg1) (,arg2))
               (,after)
               ,tokenized-lines))
      
      (check (multiple-value-list
              (parse-function-macro-call-arguments foo (list left arg1 comma right after)
                                                   tokenized-lines))
             `(((,arg1) ())
               (,after)
               ,tokenized-lines))
      
      (check (multiple-value-list
              (parse-function-macro-call-arguments foo (list left arg1 plus arg2 comma arg1 minus arg2 right after)
                                                   tokenized-lines))
             `(((,arg1 ,plus ,arg2) (,arg1 ,minus ,arg2))
               (,after)
               ,tokenized-lines))
      
      (check (multiple-value-list
              (parse-function-macro-call-arguments foo (list left arg1 left arg2 right comma arg1 minus arg2 right after)
                                                   tokenized-lines))
             `(((,arg1 ,left ,arg2 ,right) (,arg1 ,minus ,arg2))
               (,after)
               ,tokenized-lines))
      
      (check (multiple-value-list
              (parse-function-macro-call-arguments foo (list left arg1 left  comma arg2  comma right
                                                             comma arg1 minus arg2 right after)
                                                   tokenized-lines))
             `(((,arg1 ,left ,comma ,arg2 ,comma ,right) (,arg1 ,minus ,arg2))
               (,after)
               ,tokenized-lines))))
  :success)


(defun test/all ()
  (test/read-c-string)
  (test/number-lines)
  (test/substitute-trigraphs)
  (test/merge-continued-lines)
  (test/remove-comments-in-line)
  (test/remove-comments)
  (test/scan-identifier)
  (test/scan-number)
  (test/scan-punctuation)
  (test/scan-delimited-literal)
  (text/skip-spaces)
  (test/extract-path)
  (test/parse-function-macro-call-arguments)
  (test/character-value)
  (test/integer-value))

(test/all)



;;;; THE END ;;;;
