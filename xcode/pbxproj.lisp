;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pbxproj.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A program to read Xcode .pbxproj files.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-12-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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

(defpackage "COM.INFORMATIMAGO.XCODE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.RDP"
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER")
  (:export "READ-PBXPROJ"
           "WRITE-PBXPROJ"))
(in-package "COM.INFORMATIMAGO.XCODE")



;;----------------------------------------------------------------------
;; pbxproj scanner
;;----------------------------------------------------------------------

(defclass pbxproj-scanner (rdp-scanner)
  ((bom :initform nil :accessor pbxproj-bom)))


(defun eofp (object) (null object))
(defvar *eof* nil)

(defun unquoted-string-char-p (ch)
   (or (alphanumericp ch) (find ch ".$_/")))

(defmethod scan-next-token ((scanner pbxproj-scanner) &optional parser-data)
  (declare (ignore parser-data))
  (when (zerop (scanner-line scanner))
    (setf (pbxproj-bom scanner) (readline scanner)))
  (setf (scanner-current-token scanner)
        (flet ((scan-unquoted-string (scanner)
                 (loop
                   :with string = (make-array 8
                                              :element-type 'character
                                              :initial-element #\space
                                              :adjustable t :fill-pointer 0)
                   :for ch = (getchar scanner)
                   :while (unquoted-string-char-p ch)
                   :do (vector-push-extend ch string (length string))
                   :finally (progn
                              (ungetchar scanner ch)
                              (return (make-instance 'token  :kind 'string :text string
                                                     :column (scanner-column scanner)
                                                     :line (scanner-line scanner)))))))
          (let ((ch (nextchar scanner)))
            (case ch
              ((nil)
               *eof*)
              ((#\{)
               (getchar scanner)
               (make-instance 'token :kind 'left-brace :text "{"
                              :column (scanner-column scanner)
                              :line (scanner-line scanner)))
              ((#\;)
               (getchar scanner)
               (make-instance 'token :kind 'semi-colon :text ";"
                              :column (scanner-column scanner)
                              :line (scanner-line scanner)))
              ((#\})
               (getchar scanner)
               (make-instance 'token :kind 'right-brace :text "}"
                              :column (scanner-column scanner)
                              :line (scanner-line scanner)))
              ((#\()
               (getchar scanner)
               (make-instance 'token :kind 'left-paren :text "("
                              :column (scanner-column scanner)
                              :line (scanner-line scanner)))
              ((#\,)
               (getchar scanner)
               (make-instance 'token :kind 'comma :text ","
                              :column (scanner-column scanner)
                              :line (scanner-line scanner)))
              ((#\))
               (getchar scanner)
               (make-instance 'token :kind 'right-paren :text ")"
                              :column (scanner-column scanner)
                              :line (scanner-line scanner)))
              ((#\=)
               (getchar scanner)
               (make-instance 'token :kind 'equal :text "="
                              :column (scanner-column scanner)
                              :line (scanner-line scanner)))
              ((#\")
               (getchar scanner)
               (loop
                 :named :eat-string
                 :with column = (scanner-column scanner)
                 :with line   = (scanner-line   scanner)
                 :with string = (make-array 8
                                            :element-type 'character
                                            :initial-element #\space
                                            :adjustable t :fill-pointer 0)
                 :for ch = (getchar scanner)
                 :while (and ch (char/= ch #\"))
                 :do (vector-push-extend (if (char= #\\ ch)
                                             (let ((ch (getchar scanner)))
                                               (case ch
                                                 ((#\n) #\newline)
                                                 ((#\t) #\tab)
                                                 ((#\v) #\vt)
                                                 ((#\b) #\bell)
                                                 ((#\r) #\return)
                                                 ;; TODO: Perhaps scan octal character codes?
                                                 (otherwise ch)))
                                             ch)
                                         string (length string))
                 :finally (let ((token (make-instance 'token :kind 'string :text string
                                                      :column column :line line)))
                            (unless ch
                              (error 'scanner-error
                                     :line line :column column
                                     :current-token token
                                     :scanner scanner
                                     :format-control "End of file while reading a string."))
                            (return-from :eat-string token))))
              ((#\/)
               (getchar scanner)
               (if (char= #\* (nextchar scanner))
                   (progn ; comment
                     (getchar scanner) 
                     (loop
                       :named :eat-comment
                       :with column = (scanner-column scanner)
                       :with line   = (scanner-line   scanner)
                       :for ch = (getchar scanner)
                       :while (and ch (not (and (eql #\* ch) (eql #\/ (nextchar scanner)))))
                       :finally (progn
                                  (unless ch
                                    (error 'scanner-error
                                           :line line :column column
                                           :scanner scanner
                                           :format-control "End of file while reading a comment."))
                                  (getchar scanner)
                                  (return-from :eat-comment (scan-next-token scanner)))))
                   (progn
                     (ungetchar scanner #\/)
                     (scan-unquoted-string scanner))))
              ((#\space #\newline #\vt #\tab)
               (getchar scanner)
               (scan-next-token scanner))
              (otherwise
               (if (unquoted-string-char-p ch)
                   (scan-unquoted-string scanner)
                   (progn
                     (getchar scanner) ; let's eat it so that error recovery skips it.
                     (error 'scanner-error
                            :line (scanner-line scanner)
                            :column (scanner-column scanner)
                            :scanner scanner
                            :format-control "Unexpected character '~C' (code ~D)."
                            :format-arguments (list ch (char-code ch))))))))))
  (setf (scanner-current-text scanner) (token-text (scanner-current-token scanner)))
  (scanner-current-token scanner))


(defmethod advance-line  ((scanner pbxproj-scanner))
  (scan-next-token scanner))


(defmethod scanner-end-of-source-p ((scanner pbxproj-scanner))
  (eofp (scanner-current-token scanner)))

(defmethod word-equal ((a symbol) (b token)) (eql a (token-kind b)))
(defmethod word-equal ((a token) (b symbol)) (eql (token-kind a) b))

;; (when (find-method (function scanner-current-token) '()  '(rdp-scanner) nil)
;;   (remove-method (function scanner-current-token) (find-method (function scanner-current-token) '()  '(rdp-scanner))))

(defmethod accept ((scanner pbxproj-scanner) expected)
  (let ((token (scanner-current-token scanner)))
   (if (word-equal expected token)
       (prog1 (list (token-kind token)
                    (token-text token)
                    (token-column token))
         (scan-next-token scanner))
       (call-next-method))))



;;----------------------------------------------------------------------
;; pbxproj parser
;;----------------------------------------------------------------------

#-mocl
(when (and (find-package "COM.INFORMATIMAGO.RDP")
           (find-symbol "*BOILERPLATE-GENERATED*" "COM.INFORMATIMAGO.RDP")
           (boundp (find-symbol "*BOILERPLATE-GENERATED*" "COM.INFORMATIMAGO.RDP")))
  (setf (symbol-value (find-symbol "*BOILERPLATE-GENERATED*" "COM.INFORMATIMAGO.RDP")) nil))

#-mocl
(defgrammar pbxproj
    :scanner pbxproj-scanner
    :terminals ((string "…")
                (equal "=")
                (left-brace "{")
                (semi-colon ";")
                (right-brace "}")
                (left-paren "(")
                (comma ",")
                (right-paren ")"))
    :start file
    :rules ((--> file    object)
            (--> object  left-brace slots right-brace
                 :action $2)
            (--> slots   (rep slot semi-colon :action $1)
                 :action (cons 'object $1))
            (--> slot    string equal data
                 :action (list (second $1) (second $3)))
            (--> data    (alt (seq string :action (second $1))
                              (seq object :action $1)
                              (seq list   :action $1)))
            (--> list    left-paren (rep data comma :action (second $1)) right-paren
                 :action (cons 'list $2))))

#+mocl
(let ((com.informatimago.rdp::*linenum* 0)
      (#2=#:|grammar148137|
       (make-grammar :name
                     'pbxproj
                     :terminals
                     '((string "…") (equal "=") (left-brace "{") (semi-colon ";") (right-brace "}")
                       (left-paren "(") (comma ",") (right-paren ")"))
                     :start
                     'file
                     :rules
                     '((file (seq (object) #28=((list* 'file . #1=($0)))))
                       #5=(object (seq (left-brace slots right-brace) #23=($2)))
                       (slots (seq ((rep ((seq (slot semi-colon) #26=($1))))) #27=((cons 'object $1))))
                       #25=(slot (seq (string equal data) #24=((list (second $1) (second $3)))))
                       #20=(data
                            (seq
                             ((alt
                               ((seq (string) #3=((second $1))) (seq (object) #6=($1))
                                (seq (list) #15=($1)))))
                             #16=((list* 'data . #1#))))
                       #14=(list (seq (left-paren (rep ((seq (data comma) #21=((second $1))))) right-paren)
                                  #22=((cons 'list $2)))))
                     :scanner
                     'pbxproj-scanner
                     :skip-spaces
                     't)))
  (setf (gethash (grammar-name #2#) com.informatimago.rdp::*grammars*) #2#)
  (com.informatimago.rdp::compute-all-terminals #2#)
  (com.informatimago.rdp::compute-all-non-terminals #2#)
  (com.informatimago.rdp::compute-first-follow #2#)
  nil
  'pbxproj-scanner
  (progn (defun pbxproj/parse-data #17=(scanner)
           "(data (seq ((alt ((seq (string) ((second $1))) (seq (object) ($1)) (seq (list) ($1))))) ((list* 'data $0))))"
           (com.informatimago.rdp::with-non-terminal
             data
             (let (($1
                    (cond ((word-equal #4=(scanner-current-token scanner) 'string)
                           (let (($1 (accept scanner 'string)))
                             (let (($0 (list $1)) (string $1) (string.1 $1))
                               (declare (ignorable $0 string.1 string))
                               . #3#)))
                          ((word-equal #4# 'left-brace)
                           (let (($1
                                  (if (word-equal #4# 'left-brace)
                                      (pbxproj/parse-object . #7=(scanner))
                                      (error #8='unexpected-token-error
                                             :line
                                             #9=(scanner-line scanner)
                                             :column
                                             #10=(scanner-column scanner)
                                             :scanner
                                             scanner
                                             :non-terminal-stack
                                             #11=(copy-list *non-terminal-stack*)
                                             :format-control
                                             #12="Unexpected token ~S~%~S~%~{~A --> ~S~}"
                                             :format-arguments
                                             (list #13=(scanner-current-token scanner)
                                                   *non-terminal-stack*
                                                   '#5#)))))
                             (let (($0 (list $1)) (object $1) (object.1 $1))
                               (declare (ignorable $0 object.1 object))
                               . #6#)))
                          ((word-equal #4# 'left-paren)
                           (let (($1
                                  (if (word-equal #4# 'left-paren)
                                      (pbxproj/parse-list . #7#)
                                      (error #8#
                                             :line
                                             #9#
                                             :column
                                             #10#
                                             :scanner
                                             scanner
                                             :non-terminal-stack
                                             #11#
                                             :format-control
                                             #12#
                                             :format-arguments
                                             (list #13# *non-terminal-stack* '#14#)))))
                             (let (($0 (list $1)) (list $1) (list.1 $1))
                               (declare (ignorable $0 list.1 list))
                               . #15#))))))
               (let (($0 (list $1))) (declare (ignorable $0)) . #16#)))))
  (progn (defun pbxproj/parse-list #17#
           "(list (seq (left-paren (rep ((seq (data comma) ((second $1))))) right-paren) ((cons 'list $2))))"
           (com.informatimago.rdp::with-non-terminal
             list
             (let (($1 (accept scanner 'left-paren))
                   ($2
                    (loop :while (member #18=(scanner-current-token scanner)
                                         '(left-brace left-paren string)
                                         . #19=(:test #'word-equal))
                          :collect (let (($1
                                          (if (member #18# '(string left-paren left-brace) . #19#)
                                              (pbxproj/parse-data . #7#)
                                              (error #8#
                                                     :line
                                                     #9#
                                                     :column
                                                     #10#
                                                     :scanner
                                                     scanner
                                                     :non-terminal-stack
                                                     #11#
                                                     :format-control
                                                     #12#
                                                     :format-arguments
                                                     (list #13# *non-terminal-stack* '#20#))))
                                         ($2 (accept scanner 'comma)))
                                     (let (($0 (list $1 $2)) (data $1) (data.1 $1) (comma $2) (comma.1 $2))
                                       (declare (ignorable $0 comma.1 comma data.1 data))
                                       . #21#))))
                    ($3 (accept scanner 'right-paren)))
                   (let (($0 (list $1 $2 $3))
                         (left-paren $1)
                         (left-paren.1 $1)
                         (right-paren $3)
                         (right-paren.1 $3))
                     (declare (ignorable $0 right-paren.1 right-paren left-paren.1 left-paren))
                     . #22#)))))
         (progn (defun pbxproj/parse-object #17#
                  "(object (seq (left-brace slots right-brace) ($2)))"
                  (com.informatimago.rdp::with-non-terminal
                    object
                    (let (($1 (accept scanner 'left-brace))
                          ($2 (when (word-equal #4# 'string) (pbxproj/parse-slots scanner)))
                          ($3 (accept scanner 'right-brace)))
                      (let (($0 (list $1 $2 $3))
                            (left-brace $1)
                            (left-brace.1 $1)
                            (slots $2)
                            (slots.1 $2)
                            (right-brace $3)
                            (right-brace.1 $3))
                        (declare
                         (ignorable $0 right-brace.1 right-brace slots.1 slots left-brace.1 left-brace))
                        . #23#)))))
         (progn (defun pbxproj/parse-slot #17#
                  "(slot (seq (string equal data) ((list (second $1) (second $3)))))"
                  (com.informatimago.rdp::with-non-terminal
                    slot
                    (let (($1 (accept scanner 'string))
                          ($2 (accept scanner 'equal))
                          ($3
                           (if (member #18# '(string left-paren left-brace) . #19#)
                               (pbxproj/parse-data . #7#)
                               (error #8#
                                      :line
                                      #9#
                                      :column
                                      #10#
                                      :scanner
                                      scanner
                                      :non-terminal-stack
                                      #11#
                                      :format-control
                                      #12#
                                      :format-arguments
                                      (list #13# *non-terminal-stack* '#20#)))))
                      (let (($0 (list $1 $2 $3))
                            (string $1)
                            (string.1 $1)
                            (equal $2)
                            (equal.1 $2)
                            (data $3)
                            (data.1 $3))
                        (declare (ignorable $0 data.1 data equal.1 equal string.1 string))
                        . #24#)))))
         (progn (defun pbxproj/parse-slots #17#
                  "(slots (seq ((rep ((seq (slot semi-colon) ($1))))) ((cons 'object $1))))"
                  (com.informatimago.rdp::with-non-terminal
                    slots
                    (let (($1
                           (loop :while (word-equal #4# 'string)
                                 :collect (let (($1
                                                 (if (word-equal #4# 'string)
                                                     (pbxproj/parse-slot . #7#)
                                                     (error #8#
                                                            :line
                                                            #9#
                                                            :column
                                                            #10#
                                                            :scanner
                                                            scanner
                                                            :non-terminal-stack
                                                            #11#
                                                            :format-control
                                                            #12#
                                                            :format-arguments
                                                            (list #13# *non-terminal-stack* '#25#))))
                                                ($2 (accept scanner 'semi-colon)))
                                            (let (($0 (list $1 $2))
                                                  (slot $1)
                                                  (slot.1 $1)
                                                  (semi-colon $2)
                                                  (semi-colon.1 $2))
                                              (declare (ignorable $0 semi-colon.1 semi-colon slot.1 slot))
                                              . #26#)))))
                          (let (($0 (list $1))) (declare (ignorable $0)) . #27#)))))
                (progn (defun pbxproj/parse-file #17#
                         "(file (seq (object) ((list* 'file $0))))"
                         (com.informatimago.rdp::with-non-terminal
                           file
                           (let (($1
                                  (if (word-equal #4# 'left-brace)
                                      (pbxproj/parse-object . #7#)
                                      (error #8#
                                             :line
                                             #9#
                                             :column
                                             #10#
                                             :scanner
                                             scanner
                                             :non-terminal-stack
                                             #11#
                                             :format-control
                                             #12#
                                             :format-arguments
                                             (list #13# *non-terminal-stack* '#5#)))))
                             (let (($0 (list $1)) (object $1) (object.1 $1))
                               (declare (ignorable $0 object.1 object))
                               . #28#)))))
                (progn (defun parse-pbxproj (com.informatimago.rdp::source)
                         "
SOURCE: When the grammar has a scanner generated, or a scanner class
        name, SOURCE can be either a string, or a stream that will be
        scanned with the generated scanner.  Otherwise, it should be a
        SCANNER instance.
"
                         (com.informatimago.rdp::with-non-terminal
                           pbxproj
                           (let ((scanner
                                  (make-instance 'pbxproj-scanner :source com.informatimago.rdp::source)))
                             (advance-line scanner)
                             (prog1 (pbxproj/parse-file scanner)
                                    (unless (scanner-end-of-source-p scanner)
                                      (error 'parser-end-of-source-not-reached
                                             :line
                                             (scanner-line scanner)
                                             :column
                                             (scanner-column scanner)
                                             :grammar
                                             (grammar-named 'pbxproj)
                                             :scanner
                                             scanner
                                             :non-terminal-stack
                                             (copy-list *non-terminal-stack*))))))))
                'pbxproj)



(defun read-pbxproj (path)
  (with-open-file (stream path)
    (parse-pbxproj  stream)))


;;;; THE END ;;;;
