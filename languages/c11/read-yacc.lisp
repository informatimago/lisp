;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               read-yacc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Parses yacc files, to generate the grammar in sexp form.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-02 <PJB> Created.
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.YACC.PARSER")

(defun remove-comments (text &key (single-line-comments t))
  (flet ((concatenate-chunks (chunks)
           (mapconcat (function identity) chunks " ")))
    (declare (inline concatenate-chunks))
    (loop
      :with length := (length text)
      :with state  := :top
      :with chunks := '()
      :with start  := 0
      :with i := 0
      :while (< i length)
      :do (let ((ch (aref text i)))
            (ecase state
              (:top
               (case ch
                 ((#\")     (incf i) (setf state :in-string))
                 ((#\')     (incf i) (setf state :in-character))
                 ((#\/)
                  (incf i)
                  (when (< i length)
                    (let ((ch (aref text i)))
                      (case ch
                        ((#\/) ;;single line comment
                         (when single-line-comments
                           (return-from remove-comments
                             (values (concatenate-chunks (nreverse (cons (subseq text start (1- i)) chunks)))
                                     state))))
                        ((#\*)
                         (incf i)
                         (push (subseq text start (- i 2)) chunks)
                         (setf state :in-multiline-comment
                               start length))))))
                 (otherwise (incf i))))
              (:in-multiline-comment
               (case ch
                 ((#\*)
                  (incf i)
                  (when (< i length)
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
                  (if (< i length)
                      (incf i)
                      (progn (cerror "Continue" "backslash in string literal at the end of the line")
                             (setf state :top))))
                 ((#\")
                  (incf i)
                  (setf state :top))
                 (otherwise (incf i))))
              (:in-character
               (case ch
                 ((#\\)
                  (incf i)
                  (if (< i length)
                      (incf i)
                      (progn (cerror "Continue" "backslash in character literal at the end of the line")
                             (setf state :top))))
                 ((#\')
                  (incf i)
                  (setf state :top))
                 (otherwise (incf i))))))
      :finally (return (case state
                         (:in-string
                          (cerror "Continue" "unterminated string literal at the end of the line")
                          (values (concatenate-chunks (nreverse chunks)) :top))
                         (:in-character
                          (cerror "Continue" "unterminated character literal at the end of the line")
                          (values (concatenate-chunks (nreverse chunks)) :top))
                         (:top
                          (values (concatenate-chunks (nreverse (if (< start length)
                                                                    (cons (subseq text start) chunks)
                                                                    chunks)))
                                  state))
                         (:in-multiline-comment
                          (values (concatenate-chunks (nreverse chunks))
                                  state)))))))

(defun test/remove-comments ()
 (assert (equal (multiple-value-list (remove-comments "hello \"world/*\" /*comment*/ /*another/*comment*/ boy // the end */ really"))
                '("hello \"world/*\"     boy "
                  :top)))
  :success)

(defun read-delimited-string (stream ch)
  (let ((buffer (make-array 80 :element-type 'character :initial-element #\space
                               :fill-pointer 0 :adjustable t)))
    (flet ((save (ch)
             (vector-push-extend ch buffer (length buffer))))
      (declare (inline save))
      (loop
        :with terminator = ch
        :for ch = (read-char stream nil nil)
        :while ch
        :do  (cond
               ((char= terminator ch)
                (loop-finish))
               ((char= #\\ ch)
                (let ((ch (read-char stream nil nil)))
                  (unless ch
                    (error "unterminated ~:[string~;character~] literal ending with incomplete escape"
                           (char= terminator #\')))
                  (save ch)))
               (t
                (save ch)))
        :finally (return buffer)))))


(defparameter *yacc-readtable* (let ((rt (copy-readtable nil)))
                                 (setf (readtable-case rt) :preserve)
                                 (remove-all-macro-characters rt)
                                 (set-macro-character #\" 'read-delimited-string nil rt)
                                 (set-macro-character #\' 'read-delimited-string nil rt)
                                 (set-macro-character #\: 'read-colon nil rt)
                                 rt))

(defun read-header (stream)
  (let ((*readtable*  *yacc-readtable*))
    (loop
      :with tokens := '()
      :with start := nil
      :for line = (let ((line (read-line stream nil nil)))
                    (when line
                      (string-trim *whitespaces* (remove-comments line))))
      :while (and line (string/= "%%" line))
      :when (plusp (length line))
        :do (with-input-from-string (in line)
              (let ((directive (read in)))
                (case directive
                  ((|%token|)
                   (setf tokens (nconc (loop
                                         :for token := (read in nil nil)
                                         :while token
                                         :do (check-type token symbol)
                                         :collect token) tokens)))
                  ((|%start|)
                   (setf start (read in nil nil))
                   (check-type start symbol))
                  (otherwise
                   (format *error-output* "Unexpected yacc directive: ~S~%" directive)))))
      :finally (return (list :start start :tokens tokens)))))


(define-parser *Yacc-parser*
  (:start-symbol rules)
  (:terminals (lchar \: \| identifier \;))
  (rules        (rule)
                (rule rules #'cons))
  (rule         (identifier \: alternatives \;
                            (lambda (i c as s)
                              (declare (ignore c s))
                              (cons i (mapcar (lambda (a)
                                                (if (and (listp a) (= 1 (length a)))
                                                    (first a)
                                                    a))
                                              as)))))
  (alternatives (rhs)
                (rhs \| alternatives
                     (lambda (r p a)
                       (declare (ignore p))
                       (if r
                           (cons r a)
                           a))))
  (rhs          ()
                (term rhs #'cons))
  (term         lchar
                identifier))

(defun make-lexer (scanner)
  (lambda ()
    (let ((token (scan-next-token scanner)))
      (if (eq (token-kind token) 'com.informatimago.common-lisp.parser.scanner::<END\ OF\ SOURCE>)
          (values nil nil)
          (values (token-kind token) token)))))

(defun read-rules (stream)
  (let* ((lines   (remove "" (mapcar (lambda (line) (string-trim *whitespaces* line))
                                   (stream-to-string-list stream))
                        :test (function string=)))
         (end     (member "%%" lines :test (function string=)))
         (scanner (make-instance 'c11-scanner :source (remove-comments (mapconcat (function identity) (ldiff lines end) " "))))
         (lexer   (make-lexer scanner)))
    #-(and)
    (parse-with-lexer lexer *yacc-parser*)
    (maptree (lambda (token)
               (ecase (token-kind token)
                 ((identifier) (intern (token-text token)))
                 ((lchar)      (intern (string (code-char (character-value (token-text token))))))))
             (print (parse-with-lexer lexer *yacc-parser*)))))

(defun read-footer (stream)
  (declare (ignore stream))
  (values))

(defun read-yacc (stream name)
  (let* ((header (read-header stream))
         (rules  (read-rules  stream)))
    (pprint `(define-parser ,name
               (:start-symbol ,(getf header :start (first (first rules))))
               (:terminals ,(getf header :tokens))
               ,@rules))))

#-(and) (progn
         
          (in-package "COM.INFORMATIMAGO.LANGUAGES.YACC.PARSER")
          (with-open-file (stream #P"~/src/public/lisp/languages/c11/c11-parser.yacc")
            (read-yacc stream "c11"))
         
          )

;;;; THE END ;;;;
