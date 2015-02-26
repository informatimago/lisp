;;;;**************************************************************************
;;;;FILE:               source-text.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports functions to read and manipulate
;;;;    Common Lisp sources.  Most of the text source properties
;;;;    are kept (file position, line number, comments, feature
;;;;    tests, etc), while no package is created and no symbol is
;;;;    interned (TODO: but perhaps keywords?)
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-03-07 <PJB> Created.
;;;;BUGS
;;;;
;;;; Check the implementation of these features:
;;;;  - write classes for source objects (symbols, packages, feature tests, etc).
;;;;  - write reader macros.
;;;;  - write (port from pjb-emacs.el) the "source" walker  WALK-FORMS (walk-sexp), etc.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2007 - 2015
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


(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP-TEXT.SOURCE-TEXT"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER")
  (:shadowing-import-from
   "COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER"
   "READTABLE"
   "COPY-READTABLE" "MAKE-DISPATCH-MACRO-CHARACTER"
   "READ" "READ-PRESERVING-WHITESPACE"
   "READ-DELIMITED-LIST"
   "READ-FROM-STRING"
   "READTABLE-CASE" "READTABLEP"
   "SET-DISPATCH-MACRO-CHARACTER" "GET-DISPATCH-MACRO-CHARACTER"
   "SET-MACRO-CHARACTER" "GET-MACRO-CHARACTER"
   "SET-SYNTAX-FROM-CHAR"
   "WITH-STANDARD-IO-SYNTAX"
   "*READ-BASE*" "*READ-DEFAULT-FLOAT-FORMAT*" "*READ-EVAL*"
   "*READ-SUPPRESS*" "*READTABLE*")
  (:export
   "BUILD-LINE-INDEX"  "GET-LINE-AND-COLUMN"
   ;; ---- ;;
   "*SOURCE-READTABLE*"  "*SOURCE-SIGNAL-ERRORS*"
   "SOURCE-READ"
   "SOURCE-OBJECT"
   "SOURCE-OBJECT-FILE"
   "SOURCE-OBJECT-POSITION"
   "SOURCE-OBJECT-TEXT"
   "SOURCE-OBJECT-SUBFORM"
   "SOURCE-TOKEN"
   "SOURCE-TOKEN-TEXT"
   "SOURCE-TOKEN-TRAITS"
   "SOURCE-LEXICAL-ERROR"
   "SOURCE-LEXICAL-ERROR-ERROR"
   "MACRO-CHARACTER-MIXIN"
   "MACRO-CHARACTER"
   "DISPATCH-MACRO-CHARACTER-MIXIN"
   "DISPATCH-MACRO-ARGUMENT"
   "DISPATCH-MACRO-SUB-CHARACTER"
   "COMMENT"
   "COMMENT-TEXT"
   "SOURCE-SEMICOLON-COMMENT"
   "SOURCE-STRING"
   "SOURCE-STRING-VALUE"
   "SOURCE-SUBFORM"
   "SOURCE-QUOTE"
   "SOURCE-BACKQUOTE"
   "SOURCE-UNQUOTE"
   "SOURCE-SPLICE"
   "SOURCE-SEQUENCE"
   "SOURCE-SEQUENCE-ELEMENTS"
   "SOURCE-LIST"
   "SOURCE-LABEL-REFERENCE"
   "SOURCE-LABEL-REFERENCE-LABEL"
   "SOURCE-LABEL-DEFINITION"
   "SOURCE-LABEL-DEFINITION-LABEL"
   "SOURCE-LABEL-DEFINITION-FORM"
   "SOURCE-FEATURE"
   "SOURCE-NOT-FEATURE"
   "SOURCE-READ-EVAL"
   "SOURCE-TOKEN"
   "SOURCE-SHARP-PIPE-COMMENT"
   "SOURCE-FUNCTION"
   "SOURCE-VECTOR"
   "SOURCE-BIT-VECTOR"
   "SOURCE-CHARACTER"
   "SOURCE-CHARACTER"
   "SOURCE-ARRAY"
   "SOURCE-NUMBER"
   "SOURCE-NUMBER-VALUE"
   "SOURCE-BASE-NUMBER"
   "SOURCE-BASE-NUMBER-BASE"
   "SOURCE-BASE-NUMBER-SPECIFIC"
   "SOURCE-COMPLEX"
   "SOURCE-PATHNAME"
   "SOURCE-STRUCTURE"
   ;; ---- ;;
   "SOURCE-ATOM-P" "MAP-SOURCE-STREAM" "MAP-SOURCE-FILE"
   )
  (:documentation "
    This package exports functions to read and manipulate
    Common Lisp sources.  Most of the text source properties
    are kept (file position, line number, comments, feature
    tests, etc), while no package is created and no symbol is
    interned.

    Copyright Pascal J. Bourguignon 2007 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-TEXT.SOURCE-TEXT")


;;; ---------------------------------------- ;;;

(defun build-line-index (file-path &key (external-format :default))
  "
DO:                 Build an index of the line positions in the file.
FILE-PATH:          The pathname of a text file.
EXTERNAL-FORMAT:    Passed to OPEN. Default: :default.
RETURN:             A vector of file positions of the beginning of each line.
"
  (with-open-file (input file-path :external-format external-format)
    (let ((line-positions (make-array 0
                                      :adjustable t :fill-pointer 0
                                      :element-type '(integer 0))))
      (loop
         :for pos = (file-position input)
         :do        (vector-push-extend pos line-positions)
         :while     (peek-char #\newline input nil nil)
         :do        (read-char input)
         :finally   (return line-positions)))))



;;; Copied from utility.lisp to avoid package loops

(defun dichotomy-search (vector value compare &key
                         (start 0) (end (length vector))
                         (key (function identity)))
  "
PRE:	entry is the element to be searched in the table.
        (<= start end)
RETURN: (values found index order)
POST:	(<= start index end)
        +-------------------+----------+-------+----------+----------------+
        | Case              |  found   | index |  order   |     Error      |
        +-------------------+----------+-------+----------+----------------+
        | x < a[min]        |   FALSE  |  min  |  less    |      0         |
        | a[i] = x          |   TRUE   |   i   |  equal   |      0         |
        | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |      0         |
        | a[max] < x        |   FALSE  |  max  |  greater |      0         |
        +-------------------+----------+-------+----------+----------------+
"
  (let* ((curmin start)
         (curmax end)
         (index    (truncate (+ curmin curmax) 2))
         (order  (funcall compare value (funcall key (aref vector index)))) )
    (loop :while (and (/= 0 order) (/= curmin index)) :do
       (if (< order 0)
           (setf curmax index)
           (setf curmin index))
       (setf index (truncate (+ curmin curmax) 2))
       (setf order  (funcall compare value (funcall key (aref vector index)))))
    (when (and (< start index) (< order 0))
      (setf order 1)
      (decf index))
    (assert
     (or (< (funcall compare value (funcall key (aref vector index))) 0)
         (and (> (funcall compare value (funcall key (aref vector index))) 0)
              (or (>= (1+ index) end)
                  (< (funcall compare value
                              (funcall key (aref vector (1+  index)))) 0)))
         (= (funcall compare value (funcall key (aref vector index))) 0)))
    (values (= order 0) index order)))


(defun get-line-and-column (line-positions pos)
  (multiple-value-bind (foundp index order)
      (dichotomy-search line-positions pos (lambda (a b) (cond ((< a b) -1)
                                                               ((> a b)  1)
                                                               (t        0))))
    (declare (ignore foundp order))
    ;; The case x < a[min] cannot occur since (= 0 (aref line-positions 0))
    ;; and (<= 0 pos)
    (values index (- pos (aref line-positions index)))))




;;; ---------------------------------------- ;;;

(defvar *stream* nil
  "The source stream.")
(defvar *file*   nil
  "The pathname of the source file.")
(defvar *start*  nil
  "The start file position of the current reader dispatch macro character.")
(defvar *macro-character* nil
  "The current dispatch macro character.")


;;; ---------------------------------------- ;;;


;;; ---------------------------------------- ;;;

(defclass source-object ()
  ((file     :accessor source-object-file
             :initarg :file
             :initform  nil
             :type (or null string pathname))
   (position :accessor source-object-position
             :initarg :position
             :initform nil
             :type (or null integer)
             :documentation "This are file-positions. 
We don't keep track of line/column, since this can be done by reading
the source file again, as a character file instead of a sexp file..")
   (text     :accessor source-object-text
             :initarg :text
             :initform nil
             :type (or null string))))


(defmethod print-object ((self source-object) stream)
  (if *print-readably*
      (format stream "~A" (source-object-text self))
      (print-unreadable-object (self stream :type t :identity t)))
  self)


;;; ---------------------------------------- ;;;

(defun read-string-between-file-positions (stream start end)
  "
PRE:    (eq 'character (stream-element-type stream)) 
        and START and END are file positions of this STREAM.
RETURN: A string containing the characters read between the START 
        and END file positions in the STREAM.
POST:   (= end (file-position stream))
"
  (let ((buffer (make-array (- end start) :element-type 'character
                            :fill-pointer 0)))
    (file-position stream start)
    (loop
       :while (< (file-position stream) end)
       :do (vector-push (read-char stream) buffer)
       ;; We could use copy-seq to return a simple-string,
       ;; but it's not worth it.
       :finally (unless (= (file-position stream) end)
                  (warn "While reading beetween file positions, ~
                         reached a different file position: ~A < ~A"
                        end (file-position stream)))
       (return buffer))))

(defvar *source-signal-errors* nil
  "
NIL ==> return source-lexical-error objects
T   ==> signal the errors.
")


(defun read-source-object (stream start class make-arguments)
  "
DO:      Read a source-object instance, of class CLASS, and re-read
         the STREAM from START to the file position after reading the
         source-object instance, as SOURCE-OBJECT-TEXT.
START:   The file position in STREAM of the first character of the
         source object.  The current file position may be beyond this
         START position, when we're called from a reader macro (1)
         or a reader dispatch macro (2+).
MAKE-ARGUMENTS: A function returning a plist of keyword and values
                to be passed as argument to (make-instance class ...)
                This function should be doing the reading of the
                source-object.
RETURN:  A new source-object instance of class CLASS, or an instance
         of SOURCE-LEXICAL-ERROR in case of error (READER-ERROR).
"
  (let* ((inst   (handler-case  (apply (function make-instance) class
                                       :file (ignore-errors (pathname stream))
                                       :position start
                                       (funcall make-arguments))
                   (end-of-file (err)
                     (error err))
                   (error (err)
                     (if *source-signal-errors*
                         (error err)
                         (make-instance 'source-lexical-error
                           :file (ignore-errors (pathname stream))
                           :position start
                           :error err))))))
    (setf (source-object-text inst) (read-string-between-file-positions
                                     stream start (file-position stream)))
    inst))


(defmacro building-source-object (stream start class
                                  &rest args &key &allow-other-keys)
  "
USAGE: (building-source-object stream *start* 
              'source-object-class 
              :attribute (source-read stream t nil t)
              :other-attribute (read-line stream t nil t) #| ... |#)
RETURN: the source-object-class instance build.
DO:     Keep track of the file position to seek back and read again the 
        source, for the text attribute of the instance.
"
  `(read-source-object ,stream ,start ,class (lambda () (list ,@args))))



;;; ---------------------------------------- ;;;

(defclass source-token (source-object)
  ((token  :accessor source-token-text
           :initarg :token)
   (traits :accessor source-token-traits
           :initarg :traits)))


(defun source-parse-token (token)
    "
DO:      Parse the lisp reader token and return a source token object.
RETURN:  okp ; the parsed lisp object if okp, or an error message if (not okp)
"
  (values t (building-source-object *stream*  *start* 'source-token
                                    :token  (com.informatimago.common-lisp.lisp-reader.reader::token-text   token)
                                    :traits (com.informatimago.common-lisp.lisp-reader.reader::token-traits token))))



;;; ---------------------------------------- ;;;

(defclass source-lexical-error (source-object)
  ((error :accessor source-lexical-error-error
          :initarg :error
          :initform nil
          :type (or null condition))))

(defmethod print-object ((self source-lexical-error) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (self stream :type t :identity t)
        (format stream "in file ~S at position ~S: ~A"
                (source-object-file self)
                (source-object-position self)
                (source-lexical-error-error self))))
  self)


;;; ---------------------------------------- ;;;

(defmacro building-reader-macro-source-object (stream macro-character
                                               class &rest args
                                               &key &allow-other-keys)
  "
DO:              Calls BUILDING-SOURCE-OBJECT, keeping track of the file
                 position of the MACRO-CHARACTER.
MACRO-CHARACTER: The macro character that has been read (as passed
                 to the reader macro).
"
  `(building-source-object ,stream *start* ,class
                           :macro-character ,macro-character ,@args))

;;; ---------------------------------------- ;;;

(defclass macro-character-mixin ()
  ((macro-character :accessor macro-character
                    :initarg :macro-character)))

(defclass dispatch-macro-character-mixin (macro-character-mixin)
  ((argument      :accessor dispatch-macro-argument
                  :initarg :argument)
   (sub-character :accessor dispatch-macro-sub-character
                  :initarg :sub-character)))



;;;---------------------------------------------
;;; STANDARD READER MACRO FUNCTIONS
;;;---------------------------------------------

(defgeneric comment-text (comment)
  (:documentation "The text of the comment."))

(defclass comment (source-object)
  ((comment :accessor comment-text :initarg :comment))
  (:documentation "Represents a source comment."))


;;; ---------------------------------------- ;;;

(defclass  source-semicolon-comment   (comment macro-character-mixin)  ())

(defun source-reader-macro-line-comment (stream ch)
  "Source reader ; macro reader."
  (building-reader-macro-source-object
   stream ch
   'source-semicolon-comment 
   :comment (read-line stream nil "")))

;;; ---------------------------------------- ;;;

(defclass source-string (source-object macro-character-mixin)
  ((value :accessor source-string-value
          :initarg :value
          :initform nil
          :type (or null string))))

(defun source-reader-macro-string (stream delim)
  "Source reader \" macro reader."
  (building-reader-macro-source-object
   stream delim
   'source-string  
   :value (flet ((error-eof ()
                   (com.informatimago.common-lisp.lisp-reader.reader::serror
                    'simple-end-of-file stream
                    "input stream ~S ends within a string" stream)))
            (loop
               :named read-string
               :with rst    = (readtable-syntax-table *readtable*)
               :with string = (make-array 64 :element-type 'character
                                          :adjustable t :fill-pointer 0)
               :for ch      = (read-char stream nil nil t)
               :do (cond
                     ((null ch)
                      (error-eof))
                     ((eql ch delim)
                      (return-from read-string string))
                     ((= (com.informatimago.common-lisp.lisp-reader.reader::character-syntax
                          (com.informatimago.common-lisp.lisp-reader.reader::character-description rst ch))
                         com.informatimago.common-lisp.lisp-reader.reader::+cs-single-escape+)
                      (let ((next (read-char stream nil nil)))
                        (when (null next)
                          (error-eof))
                        (vector-push-extend next string)))
                     (t (vector-push-extend ch   string)))))))


;;; ---------------------------------------- ;;;

(defclass source-subform (source-object macro-character-mixin)
  ((subform :accessor source-object-subform
            :initarg :subform)))


;;; ---------------------------------------- ;;;

(defclass source-quote (source-subform macro-character-mixin) ())


(defun source-reader-macro-quote (stream ch)
  "Source reader ' macro reader."
  (building-reader-macro-source-object
   stream ch
   'source-quote 
   :subform (source-read stream t nil t)))


;;; ---------------------------------------- ;;;

(defclass source-backquote (source-subform macro-character-mixin) ())


(defun source-reader-macro-backquote (stream ch)
  "Source reader ` macro reader."
  (building-reader-macro-source-object
   stream ch
   'source-backquote
   :subform (source-read stream t nil t)))


;;; ---------------------------------------- ;;;

(defclass source-unquote (source-subform macro-character-mixin) ())
(defclass source-splice  (source-subform macro-character-mixin) ())

(defun source-reader-macro-comma (stream ch)
  "Source reader , macro reader."
  (building-reader-macro-source-object
   stream ch
   (if (char= #\@ (peek-char nil stream t nil t))
       'source-splice
       'source-unquote)
   :subform (source-read stream t nil t)))


;;; ---------------------------------------- ;;;

(defclass source-sequence (source-object)
  ((elements :accessor source-sequence-elements
             :initarg  :elements
             :initform nil
             :type     sequence
             :documentation "
Works for conses, lists, and invalid stuff put in parentheses, like ( . . )
Dots are represented by a source-object, 
so they can appear even in invalid syntaxes.
")))

(defclass source-list (source-sequence macro-character-mixin) ())


(defun source-reader-macro-left-parenthesis (stream ch)
  "Source reader ( macro reader."
  (building-reader-macro-source-object
   stream ch
   'source-list
   :elements (loop
                :until (char= #\) (peek-char t stream t nil t))
                :collect (source-read stream t nil t)
                :finally (read-char stream t nil t))))


;;; ---------------------------------------- ;;;

(defun source-reader-macro-error-start (stream ch)
  ;; Actually exits thru the error handler...
  (building-reader-macro-source-object
   stream ch
   'source-lexical-error  
   :error (com.informatimago.common-lisp.lisp-reader.reader::serror
           'simple-reader-error stream
           "an object cannot start with ~C" ch)))


;;; ---------------------------------------- ;;;


(defun make-source-dispatch-macro-character (macro-character
                                             &optional (readtable *readtable*))
  "
PRE:   MACRO-CHARACTER is a reader dispatch macro character.
POST:  The dispatching reader macro function for the MACRO-CHARACTER
       is replaced by a function that encapsulates the original 
       dispatching reader macro function, in a binding to *START*
       of the file position of the MACRO-CHARACTER.
"
  (multiple-value-bind (dispatch non-terminating-p)
      (get-macro-character macro-character readtable)
    (set-macro-character macro-character 
                         (lambda (stream macro-character)
                           (unread-char macro-character stream)
                           (let ((*start* (file-position stream))
                                 (*macro-character* macro-character))
                             (read-char stream)
                             (funcall dispatch stream macro-character)))
                         non-terminating-p
                         readtable)))

(defmacro building-reader-dispatch-macro-source-object (stream argument sub-char
                                                        class &rest initargs
                                                        &key &allow-other-keys)
  `(building-source-object ,stream *start* ,class
                           :macro-character *macro-character*
                           :argument        ,argument
                           :sub-character   ,sub-char
                           ,@initargs))


;;;---------------------------------------------
;;; STANDARD READER DISPATCH MACRO FUNCTIONS
;;;---------------------------------------------

(defclass source-label-reference (source-object dispatch-macro-character-mixin)
  ((label :accessor source-label-reference-label
          :initarg :label
          :type (integer 0))))

(defun source-reader-dispatch-macro-label-reference   (stream arg sub-char)
  "Source reader ## dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-label-reference
   :label (or arg
              (com.informatimago.common-lisp.lisp-reader.reader::serror
               'simple-reader-error stream
               "a number must be given between # and #"))))

;;; ---------------------------------------- ;;;

(defclass source-label-definition (source-object dispatch-macro-character-mixin)
  ((label :accessor source-label-definition-label
          :initarg :label
          :type (integer 0))
   (form  :accessor source-label-definition-form
          :initarg :form)))



(defun source-reader-dispatch-macro-label-definition  (stream arg sub-char)
  "Source reader #= dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-label-definition 
   :label (or arg
              (com.informatimago.common-lisp.lisp-reader.reader::serror
               'simple-reader-error stream
               "a number must be given between # and ="))
   :form  (source-read stream t nil t)))


;;; ---------------------------------------- ;;;

(defclass source-feature     (source-subform dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-feature           (stream arg sub-char)
  "Source reader #+ dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-feature
   :subform (let ((*package*  (find-package "KEYWORD"))
                  (*read-suppress* nil))
              (source-read stream t nil t))))


(defclass source-not-feature (source-subform dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-not-feature       (stream arg sub-char)
  "Source reader #- dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-not-feature 
   :subform (let ((*package*  (find-package "KEYWORD"))
                  (*read-suppress* nil))
              (source-read stream t nil t))))


;;; ---------------------------------------- ;;;

(defclass source-read-eval (source-subform dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-read-eval         (stream arg sub-char)
  "Source reader #. dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-read-eval 
   :subform (source-read stream t nil t)))


;;; ---------------------------------------- ;;;

;;; We cannot decide between some symbol and integer, or the type of
;;; floating point before knowing *read-case* and *READ-DEFAULT-FLOAT-FORMAT*
;;;

;; (defclass source-symbol (source-token)
;;   ((name      :accessor source-symbol-name
;;               :initarg :name)
;;    (qualifier :accessor source-symbol-qualifier
;;               :initarg :qualifier
;;               :type (member nil :external :internal)
;;               :initform nil)
;;    (package   :accessor source-symbol-package
;;               :initarg :package
;;               :initform nil
;;               :documentation "
;; Either a string containing the package name, when it's explicitely qualified,
;; or nil when the symbol has no package (#:example), or the symbol *PACKAGE*
;; for unqualified symbols.
;; ")))
;; 
;; 
;; (defclass source-number (source-token)
;;   ((value   :accessor source-number-value
;;             :initarg :value)))


(defun source-reader-dispatch-macro-uninterned        (stream arg sub-char)
  "Source reader #: dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-token 
   :token (multiple-value-bind (tokenp token)
              (com.informatimago.common-lisp.lisp-reader.reader::read-token
               stream t nil t nil nil *readtable*)
            (if tokenp
                (make-instance 'source-token
                  ;; TODO: Here, we know we have a symbol whatever...
                  :file (pathname stream)
                  :position *start*
                  :token token)
                (com.informatimago.common-lisp.lisp-reader.reader::serror
                 'simple-reader-error stream
                 "token expected after #:")))))


;;; ---------------------------------------- ;;;

(defun source-reader-dispatch-macro-unreadable        (stream arg sub-char)
  "Source reader #< dispatch macro reader."
  (declare (ignore sub-char arg))
  ;; TODO: see if we can do something to read #< ...
  (com.informatimago.common-lisp.lisp-reader.reader::serror
   'simple-reader-error stream
   "objects printed as #<...> cannot be read back in"))


;;; ---------------------------------------- ;;;

(defclass source-sharp-pipe-comment (comment dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-comment           (stream arg sub-char)
  "Source reader #| dispatch macro reader."
  ;; #|...|# is treated as a comment by the reader. It must be balanced
  ;; with respect to other occurrences of #| and |#, but otherwise may
  ;; contain any characters whatsoever.
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-token 
   :comment
   (loop
      :with comment = (make-array 0
                                  :adjustable t :fill-pointer 0
                                  :element-type 'character)
      :with level = 1
      :with state = :normal
      :until (zerop level)
      :do (let ((ch (read-char stream t nil t)))
            (vector-push-extend ch comment)
            (case state
              ((:normal) (case ch
                           ((#\#)              (setf state :sharp))
                           ((#\|)              (setf state :pipe))))
              ((:sharp)  (case ch
                           ((#\#))
                           ((#\|) (incf level) (setf state :normal))
                           (otherwise          (setf state :normal))))
              ((:pipe)   (case ch
                           ((#\#) (decf level) (setf state :normal))
                           ((#\|))
                           (otherwise          (setf state :normal))))))
      :finally (progn (decf (fill-pointer comment) 2)
                      (return comment)))))


;;; ---------------------------------------- ;;;

(defclass source-function (source-subform dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-function          (stream arg sub-char)
  "Source reader #' dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-function 
   :subform (source-read stream t nil t)))


;;; ---------------------------------------- ;;;

(defclass source-vector (source-sequence dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-vector            (stream arg sub-char)
  "Source reader #( dispatch macro reader."
  ;; If an unsigned decimal integer appears between the # and (, it
  ;; specifies explicitly the length of the vector. The consequences are
  ;; undefined if the number of objects specified before the closing )
  ;; exceeds the unsigned decimal integer. If the number of  objects
  ;; supplied before the closing ) is less than the unsigned decimal
  ;; integer but greater than zero, the last object is used to fill all
  ;; remaining elements of the  vector. The consequences are undefined if
  ;; the unsigned decimal integer is non-zero and number of objects
  ;; supplied before the closing ) is zero. For example,
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-vector
   :elements  (loop
                :until (char= #\) (peek-char t stream t nil t))
                :collect (source-read stream t nil t)
                :finally (read-char stream t nil t))))


;;; ---------------------------------------- ;;;

(defclass source-bit-vector (source-sequence dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-bit-vector        (stream arg sub-char)
  "Source reader #* dispatch macro reader.
URL: <http://www.lispworks.com/documentation/HyperSpec/Body/02_dhd.htm>
"
  ;; Syntax: #*<<bits>>
  ;; 
  ;; A simple bit vector is constructed containing the indicated bits (0's
  ;; and 1's), where the leftmost bit has index zero and the subsequent
  ;; bits have increasing indices.
  ;; 
  ;; Syntax: #<<n>>*<<bits>>
  ;; 
  ;; With an argument n, the vector to be created is of length n. If the
  ;; number of bits is less than n but greater than zero, the last bit is
  ;; used to fill all remaining bits of the bit vector.
  ;; 
  ;; The notations #* and #0* each denote an empty bit vector.
  ;; 
  ;; Regardless of whether the optional numeric argument n is provided, the
  ;; token that follows the asterisk is delimited by a normal token
  ;; delimiter. However, (unless the  value of *read-suppress* is true) an
  ;; error of type reader-error is signaled if that  token is not composed
  ;; entirely of 0's and 1's, or if n was supplied and the token is
  ;; composed of more than n bits, or if n is greater than one, but no bits
  ;; were specified.  Neither a single escape nor a multiple escape is
  ;; permitted in this token.
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-bit-vector
   :elements (com.informatimago.common-lisp.lisp-reader.reader::reader-dispatch-macro-bit-vector stream arg sub-char)))


;;; ---------------------------------------- ;;;

(defclass source-character (source-object dispatch-macro-character-mixin)
  ((character :accessor source-character
              :initarg :character)))


(defun source-reader-dispatch-macro-char              (stream arg sub-char)
  "Source reader #\\ dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-character
   :character (read-char stream t nil t)))


;;; ---------------------------------------- ;;;

(defclass source-array (source-subform dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-array             (stream arg sub-char)
  "Source reader #A dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-array
   :subform (source-read stream t nil t)))


;;; ---------------------------------------- ;;;

(defclass source-number (source-object)
  ((value     :accessor source-number-value
              :initarg :value)))

(defclass source-base-number (source-number dispatch-macro-character-mixin)
  ((base      :accessor source-base-number-base
              :initarg :base)
   (specificp :accessor source-base-number-specific
              :initarg :specificp)))

(defun source-reader-dispatch-macro-binary            (stream arg sub-char)
  "Source reader #B dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-base-number
   :base 2.
   :specificp t
   :value (com.informatimago.common-lisp.lisp-reader.reader::read-rational-in-base
           stream arg sub-char 2.)))


(defun source-reader-dispatch-macro-octal             (stream arg sub-char)
  "Source reader #O dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-base-number
   :base 8.
   :specificp t
   :value (com.informatimago.common-lisp.lisp-reader.reader::read-rational-in-base
           stream arg sub-char 8.)))


(defun source-reader-dispatch-macro-hexadecimal       (stream arg sub-char)
  "Source reader #X dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-base-number
   :base 16.
   :specificp t
   :value (com.informatimago.common-lisp.lisp-reader.reader::read-rational-in-base
           stream arg sub-char 16.)))


(defun source-reader-dispatch-macro-radix             (stream arg sub-char)
  "Source reader #R dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-base-number
   :base arg
   :specificp nil
   :value (if arg
              (com.informatimago.common-lisp.lisp-reader.reader::read-rational-in-base
               stream nil sub-char arg)
              (com.informatimago.common-lisp.lisp-reader.reader::serror
               stream "the number base must be given between # and ~A"
                      sub-char))))

;;; ---------------------------------------- ;;;

(defclass source-complex (source-subform dispatch-macro-character-mixin) ())


(defun source-reader-dispatch-macro-complex           (stream arg sub-char)
  "Source reader #C dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-base-complex
   :subform (let ((c (source-read stream t nil t)))
              (if (and (com.informatimago.common-lisp.lisp-reader.reader::proper-list-p c)
                       (= 2 (length c))
                       (every (function realp) c))
                  c
                  (com.informatimago.common-lisp.lisp-reader.reader::serror
                   'simple-reader-error stream
                   "bad syntax for complex number: #C~S" c)))))


;;; ---------------------------------------- ;;;

(defclass source-pathname (source-subform dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-pathname          (stream arg sub-char)
  "Source reader #P dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-pathname
   :subform (source-read stream t nil t) ))


;;; ---------------------------------------- ;;;

(defclass source-structure (source-subform dispatch-macro-character-mixin) ())

(defun source-reader-dispatch-macro-structure         (stream arg sub-char)
  "Source reader #S dispatch macro reader."
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-structure
   :subform (source-read stream t nil t)))


;;; ---------------------------------------- ;;;


(defun source-reader-dispatch-macro-error-invalid (stream sub-char arg)
  (building-reader-dispatch-macro-source-object
   stream arg sub-char
   'source-lexical-error
   :error (com.informatimago.common-lisp.lisp-reader.reader::serror
           'simple-reader-error stream
           "objects printed as # in view of *PRINT-LEVEL* cannot be read back in")))


;;; ---------------------------------------- ;;;

(defun make-source-readtable ()
  "
RETURN: A new readtable where all the reader macros are set to
        return source objects instead of lisp objects.
"
  (let ((readtable (copy-readtable nil)))
    (macrolet ((smc (&rest clauses)
                 `(progn
                    ,@(mapcar (lambda (clause)
                                `(set-macro-character
                                  ,(first clause)
                                  (function ,(second clause))
                                  ,(third clause)
                                  readtable))
                              clauses))))
      (smc
       (#\; source-reader-macro-line-comment     nil)
       (#\" source-reader-macro-string           nil)
       (#\' source-reader-macro-quote            nil)
       (#\` source-reader-macro-backquote        nil)
       (#\, source-reader-macro-comma            nil)
       (#\( source-reader-macro-left-parenthesis nil)
       (#\) source-reader-macro-error-start      nil)))
    (macrolet ((dmc (&rest clauses)
                 `(progn
                    ,@(mapcar (lambda (clause)
                                `(set-dispatch-macro-character
                                  ,(first  clause)
                                  ,(second clause)
                                  (function ,(third clause))
                                  readtable))
                              clauses))))
      (make-dispatch-macro-character #\# t readtable)
      (dmc
       (#\# #\SPACE   source-reader-dispatch-macro-error-invalid)
       (#\# #\NEWLINE source-reader-dispatch-macro-error-invalid)
       (#\# #\# source-reader-dispatch-macro-label-reference)
       (#\# #\' source-reader-dispatch-macro-function)
       (#\# #\( source-reader-dispatch-macro-vector)
       (#\# #\* source-reader-dispatch-macro-bit-vector)
       (#\# #\+ source-reader-dispatch-macro-feature)
       (#\# #\- source-reader-dispatch-macro-not-feature)
       (#\# #\. source-reader-dispatch-macro-read-eval)
       (#\# #\: source-reader-dispatch-macro-uninterned)
       (#\# #\< source-reader-dispatch-macro-unreadable)
       (#\# #\= source-reader-dispatch-macro-label-definition)
       (#\# #\A source-reader-dispatch-macro-array)
       (#\# #\B source-reader-dispatch-macro-binary)
       (#\# #\C source-reader-dispatch-macro-complex)
       (#\# #\O source-reader-dispatch-macro-octal)
       (#\# #\P source-reader-dispatch-macro-pathname)
       (#\# #\R source-reader-dispatch-macro-radix)
       (#\# #\S source-reader-dispatch-macro-structure)
       (#\# #\X source-reader-dispatch-macro-hexadecimal)
       (#\# #\\ source-reader-dispatch-macro-char)
       (#\# #\| source-reader-dispatch-macro-comment)
       ;; clisp extensions:
       ;; (#\# #\! reader-dispatch-macro-executable)
       ;; (#\# #\" reader-dispatch-macro-clisp-pathname)
       ;; (#\# #\, reader-dispatch-macro-load-eval)
       ;; (#\# #\Y SYSTEM::CLOSURE-READER)
       ))
    (setf (readtable-parse-token readtable) (function source-parse-token))
    readtable))


(defvar *source-readtable* (make-source-readtable)
  "The source readtable.")

(defun source-read (&optional input-stream
                    (eof-error-p t) (eof-value nil)
                    (recursive-p nil) (preserve-whitespace-p nil))
  (let ((*read-suppress* nil)           ; we do want the source! 
        (*readtable* *source-readtable*)
        (*stream* input-stream)
        (*file*  (ignore-errors (pathname input-stream))))
    (unless preserve-whitespace-p 
      (peek-char t input-stream nil nil t))
    (let ((*start* (file-position input-stream)))
      ;; (read input-stream eof-error-p eof-value recursive-p)
      ;; We want to allow all-dots tokens.
      (com.informatimago.common-lisp.lisp-reader.reader::read-1  input-stream eof-error-p eof-value
                       recursive-p preserve-whitespace-p nil t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric source-atom-p (self)
  (:method ((self source-object))  (declare (ignorable self)) t)
  (:method ((self source-list))    (declare (ignorable self)) nil))


(defgeneric map-subforms (fun self)
  (:method (fun (self source-object))   (funcall fun self) (values))
  (:method (fun (self source-array))    (funcall fun self) (values))
  (:method (fun (self source-complex))  (funcall fun self) (values))
  (:method (fun (self source-pathname)) (funcall fun self) (values))
  (:method (fun (self source-subform))
    (funcall fun self)
    (map-subforms fun (source-object-subform self)))
  (:method (fun (self source-label-definition))
    (funcall fun self)
    (map-subforms fun (source-label-definition-form self)))
  (:method (fun (self source-list))
    (funcall fun self)
    (dolist (item (source-sequence-elements self) (values))
      (map-subforms fun item))))


(defun map-source-stream (fun source-stream
                          &key (deeply t) (only-atoms nil))
  (loop
     :for top-level-form = (source-read source-stream nil
                                        source-stream t)
     :until (eq top-level-form source-stream)
     :if deeply
     :do (map-subforms
          (if only-atoms
              (lambda (x) (when (source-atom-p x) (funcall fun x)))
              fun)
          top-level-form)
     :else
     :do (when (or (not  only-atoms) (source-atom-p top-level-form))
           (funcall fun top-level-form))))


(defun map-source-file (fun source-file  
                        &key (deeply t) (only-atoms nil)
                        (external-format :default))
  "
FUN:    A function (source-object) 
        source-object:  An instance of source-object parsed from a source file.
        When atoms is true, FUN is called only on source-objects not 
        representing cons cells (lists).
"
  (with-open-file (src source-file :external-format external-format)
    (map-source-stream fun src :deeply deeply :only-atoms only-atoms)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; To be tested:
;;; 
;;; '(#1=#2=#3=#4=a #2# #3# #4#)

#|| 

(use-package :com.informatimago.common-lisp.lisp-text.source-text)

(defparameter *some*
  (with-open-file (src "source-text.lisp")
    (loop
       :for item = (source-read src nil src)
       :until (eq item src)
       :collect item
       :until (typep item 'source-lexical-error)
       :do (princ "**") (let ((*print-readably* nil)) (print item))
       :do (princ "==") (let ((*print-readably* t  )) (prin1 item)))))

(defparameter *some*
  (with-input-from-string (src "(a b c) (d e f)")
    (loop
       :for item = (source-read src nil src)
       :until (eq item src)
       :collect item
       :until (typep item 'source-lexical-error)
       :do (princ "**") (let ((*print-readably* nil)) (print item))
       :do (princ "==") (let ((*print-readably* t  )) (prin1 item)))))


(with-input-from-string (src "  ")
  (com.informatimago.common-lisp.lisp-reader.reader::read-token
   src nil :eof t nil nil reader::*readtable*))

(with-input-from-string (src "  ")
  (com.informatimago.common-lisp.lisp-reader.READER::READ-0/1 src nil :eof nil NIL NIL T))

(with-input-from-string (src "  ")
  (source-read src nil :eof))
 
(defparameter *some*
  (with-open-file (src "source-text.lisp")
    (loop
       :for item = (source-text:source-read src nil src)
       :until (eq item src)
       :collect item
       :until (typep item 'source-lexical-error)
       :do (let ((*print-readably* t)) (prin1 item)))))

(with-input-from-string (src "#+(and)(a b c #-(or) d e)")
  (source-read src nil :eof))

(with-input-from-string (src "( a . b . c ... d #+(and)(a b c) #-(or)(d e))")
  (source-read src nil :eof))

(setf *some*
      (with-input-from-string (src "'(#1=#2=#3=#4=a #2# #3# #4#)")
        (source-read src nil :eof)))

||#

;; (with-input-from-string (src "(a (b c #(1 2 3) (e f) g) h) (1 2 3)")
;;   (let ((*print-readably* t))
;;     (map-source-stream (lambda (x) (terpri) (prin1 x))  src)))
;; 
;; (with-input-from-string (src "(a (b c (1 2 3) (e f) g) h) (1 2 3)")
;;   (let ((*print-readably* t))
;;     (map-source-stream (lambda (x) (terpri) (prin1 x))  src
;;                        :deeply t :only-atoms nil)))
;; 
;; (with-input-from-string (src "(a (b c #(1 2 3) (e f) g) h) (1 2 3)")
;;   (map-source-stream (lambda (x)
;;                        (terpri)
;;                        (let ((*print-readably* t)) (prin1 x)))
;;                      src :deeply t :only-atoms nil))


;;;; The End ;;;;
