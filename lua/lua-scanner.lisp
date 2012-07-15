;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               lua-scanner.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a LUA scanner and parser.
;;;;    http://www.lua.org/manual/5.2/manual.html#3.1
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-14 <PJB> Created.  Implemented the scanner.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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


(in-package "COM.INFORMATIMAGO.LUA.SCANNER")


;; We need to write a specific scanner because of long brackets. :-(



;; Lua is a case-sensitive language: and is a reserved word, but And
;; and AND are two different, valid names. As a convention, names
;; starting with an underscore followed by uppercase letters (such as
;; _VERSION) are reserved for internal global variables used by Lua.

(defparameter *lua-keywords*
  (loop
    :with h = (make-hash-table :test (function equal))
    :for kw :in '("and"       "break"     "do"        "else"      "elseif"
                  "end"       "false"     "for"       "function"  "if"
                  "in"        "local"     "nil"       "not"       "or"
                  "repeat"    "return"    "then"      "true"      "until"
                  "while")
    :do (setf (gethash kw h) (intern kw :keyword))
    :finally (return h)))

;; (com.informatimago.common-lisp.cesarum.utility:print-hashtable *lua-keywords*)

(defparameter *lua-special-tokens*
  '("+"     "-"     "*"     "/"     "%"     "^"     "#"
    "=="    "~="    "<="    ">="    "<"     ">"     "="
    "("     ")"     "{"     "}"     "["     "]"
    ";"     ":"     ","     "."     ".."    "..."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LUA Tokens
;;;

(defclass lua-token (token)
  ())

(defclass tok-special (lua-token)
  ())

(defclass tok-keyword (lua-token)
  ())

(defclass tok-identifier (lua-token)
  ())

(defgeneric token-value (token)
  (:method ((token token))
    (token-kind token)))

(defclass tok-string (lua-token)
  ((value     :accessor token-value
              :initarg :value
              :initform ""
              :type     string)))

(defclass tok-number (lua-token)
  ((value     :accessor token-value
              :initarg :value
              :initform 0.0d0
              :type     real)))

(defclass tok-comment (lua-token)
  ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LUA Scanner
;;;

(defclass lua-scanner (scanner)
  ((keep-comments :accessor lua-scanner-keep-comments
                  :initform nil
                  :initarg :keep-comments
                  :type boolean
                  :documentation "When T, comments are returned as tokens,
when NIL, comments are skipped as spaces."))
  (:documentation "A scanner for LUA."))


(defmethod process-escape ((scanner lua-scanner) text value)
  (let ((ch (getchar scanner)))
    (write-char ch text)
    (let ((ch (case ch
                ((#\a)
                 #+bell #\Bell
                 ;; assume ASCII:
                 #-bell (ignore-errors (code-char 7)))
                ((#\b)
                 #+backspace #\Backspace
                 ;; assume ASCII:
                 #-backspace (ignore-errors (code-char 8)))
                ((#\f)
                 #+page #\Page
                 ;; assume ASCII:
                 #-page (ignore-errors (code-char 12)))
                ((#\n #\Newline)
                 #\Newline)
                ((#\r)
                 #+return #\Return
                 ;; assume ASCII:
                 #-return (ignore-errors (code-char 13)))
                ((#\t)
                 #+tab #\Tab
                 ;; assume ASCII:
                 #-tab (ignore-errors (code-char 9)))
                ((#\v)
                 #+vt #\vt
                 ;; assume ASCII:
                 #-vt (ignore-errors (code-char 11)))
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                 (let ((digits (list ch)))
                   (when (digit-char-p (nextchar scanner))
                     (let ((ch  (getchar scanner)))
                       (write-char ch text)
                       (push ch digits))
                     (when (digit-char-p (nextchar scanner))
                       (let ((ch  (getchar scanner)))
                         (write-char ch text)
                         (push ch digits))))
                   (let ((code (parse-integer (coerce (nreverse digits) 'string))))
                     ;; Note: we should
                     ;; catch the errors
                     ;; and continue
                     ;; scanning the
                     ;; string, before
                     ;; resignaling the
                     ;; error.
                     (if (<= 0 code 255)
                       (handler-case (code-char code)
                         (error ()
                                (error 'scanner-error
                                       :line    (scanner-line scanner)
                                       :column  (scanner-column scanner)
                                       :state    (scanner-state  scanner)
                                       :current-token (scanner-current-token scanner)
                                       :scanner scanner
                                       :format-control "Invalid character code in escape: ~A (unsupported character code) in string ~S."
                                       :format-arguments (list code (get-output-stream-string text)))))
                       (error 'scanner-error
                              :line    (scanner-line scanner)
                              :column  (scanner-column scanner)
                              :state    (scanner-state  scanner)
                              :current-token (scanner-current-token scanner)
                              :scanner scanner
                              :format-control "Invalid character code in escape \\~A in string ~S."
                              :format-arguments (list code (get-output-stream-string text)))))))
                (otherwise
                 ch))))
      (when ch (write-char ch value)))))


(defmethod scan-string-starting-with ((scanner lua-scanner) delimiter)
  ;; "('([^'\\]|\\([abfnrtv\\\"']|[0-9]([0-9][0-9]?)?)*')
  ;; |(\"([^'\\]|\\([abfnrtv\\\"']|[0-9]([0-9][0-9]?)?)*\")"
  (let (text value)
    (setf text
          (with-output-to-string (text)
            (write-char delimiter text)
            (setf value
                  (with-output-to-string (value)
                    (loop
                      :for ch = (getchar scanner)
                      :while ch
                      :do (write-char ch text)
                      :until (char= ch delimiter)
                      :do (case ch
                            ((#\\)
                             (process-escape scanner text value))
                            ((#\Newline)
                             (error 'scanner-error
                                    :line    (scanner-line scanner)
                                    :column  (scanner-column scanner)
                                    :state    (scanner-state  scanner)
                                    :current-token (scanner-current-token scanner)
                                    :scanner scanner
                                    :format-control "Unescaped newline in string: ~S."
                                    :format-arguments (list (get-output-stream-string text))))
                            (otherwise
                             (write-char ch value)))
                      :finally (unless ch
                                 (error 'scanner-error
                                        :line    (scanner-line scanner)
                                        :column  (scanner-column scanner)
                                        :state    (scanner-state  scanner)
                                        :current-token (scanner-current-token scanner)
                                        :scanner scanner
                                        :format-control "Reached end-of-file with unterminated string starting with: ~S."
                                        :format-arguments (list (get-output-stream-string text)))))))))
    (values text value)))


(defmethod scan-string-starting-with-long-bracket ((scanner lua-scanner) bracket)
  (let (text value)
    (setf text
          (with-output-to-string (text)
            (write-char bracket text)
            (setf value
                  (with-output-to-string (value)
                    (loop
                      :named scan-string
                      :with olevel = (loop
                                       :for ch = (nextchar scanner)
                                       :while (char= ch #\=)
                                       :do (write-char (getchar scanner) text)
                                       :count 1 :into level
                                       :finally
                                       (write-char (getchar scanner) text)
                                       (unless (char= ch #\[)
                                         (error 'scanner-error
                                                :line    (scanner-line scanner)
                                                :column  (scanner-column scanner)
                                                :state    (scanner-state  scanner)
                                                :current-token (scanner-current-token scanner)
                                                :scanner scanner
                                                :format-control "Invalid token: ~S (missing a '['?)"
                                                :format-arguments (list (get-output-stream-string text))))
                                       (return level))
                      :for ch = (getchar scanner)
                      :while ch
                      :do (write-char ch text)
                      :do (case ch
                            ((#\\)
                             (process-escape scanner text value))
                            ((#\])
                             (loop
                               :for ch = (nextchar scanner)
                               :while (char= ch #\=)
                               :do (write-char (getchar scanner) text)
                               :count 1 :into level
                               :finally
                               (write-char (getchar scanner) text)
                               (if (and (char= ch #\])
                                        (= olevel level))
                                 (return-from scan-string)
                                 (format value "]~V,,,'=<~>~C" level ch))))
                            (otherwise
                             (write-char ch value)))
                      :finally (unless ch
                                 (error 'scanner-error
                                        :line    (scanner-line scanner)
                                        :column  (scanner-column scanner)
                                        :state    (scanner-state  scanner)
                                        :current-token (scanner-current-token scanner)
                                        :scanner scanner
                                        :format-control "Reached end-of-file with unterminated string starting with: ~S."
                                        :format-arguments (list (get-output-stream-string text)))))))))
    (values text value)))


(defun make-extensible-string (&optional (default-allocation 32))
  (make-array default-allocation
              :element-type 'character
              :adjustable t
              :fill-pointer 0))


(defmethod scan-identifier ((scanner lua-scanner))
  (with-output-to-string (name)
    (write-char (getchar scanner) name)
    (loop
      :for ch = (nextchar scanner)
      :while (and ch (or (alphanumericp ch) (char= ch #\_)))
      :do (write-char (getchar scanner) name))))


(defmethod scan-number ((scanner lua-scanner))
  (flet ((invalid-char (ch what number)
           (error 'scanner-error
                  :line    (scanner-line scanner)
                  :column  (scanner-column scanner)
                  :state    (scanner-state  scanner)
                  :current-token (scanner-current-token scanner)
                  :scanner scanner
                  :format-control "Invalid ~A character ~S in number starting with: ~S."
                  :format-arguments (list what ch number)))
         (incomplete-floating-point-number (number)
           (error 'scanner-error
                  :line    (scanner-line scanner)
                  :column  (scanner-column scanner)
                  :state    (scanner-state  scanner)
                  :current-token (scanner-current-token scanner)
                  :scanner scanner
                  :format-control "Incomplete floating-point number starting with: ~S."
                  :format-arguments (list number))))
    (let ((ch  (getchar scanner)))
      (if (char= #\x (nextchar scanner))
        ;; hexadecimal integer
        (let ((text (with-output-to-string (number)
                      (loop
                        :for ch = (nextchar scanner)
                        :while (and ch (digit-char-p ch 16.))
                        :do (write-char (getchar scanner) number)
                        :finally (when (and ch (alphanumericp ch))
                                   (invalid-char ch "hexadecimal"
                                                 (concatenate
                                                     'string "0x"
                                                     (get-output-stream-string number))))))))
          (values text (parse-integer text :radix 16)))
        ;; integer or floating-point
        (let* ((*read-default-float-format* 'double-float)
               (*read-base* 10.)
               (*read-eval* nil)
               (text (with-output-to-string (number)
                       (write-char ch number)
                       (loop
                         :with state = :before-dot
                         :for ch = (nextchar scanner)
                         :while ch
                         :do (case state
                               (:before-dot
                                (case ch
                                  ((#\.)
                                   (setf state :after-dot)
                                   (write-char (getchar scanner) number))
                                  ((#\e #\E)
                                   (setf state :after-exponent)
                                   (write-char (getchar scanner) number))
                                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                   (write-char (getchar scanner) number))
                                  (otherwise
                                   (when (and ch (alphanumericp ch))
                                     (invalid-char ch "integer"
                                                   (get-output-stream-string number)))
                                   (loop-finish))))
                               (:after-dot
                                (case ch
                                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                   (setf state :in-fraction)
                                   (write-char (getchar scanner) number))
                                  (otherwise
                                   (invalid-char ch (if (char-equal ch #\e)
                                                      "floating-point (missing a 0 before the exponent?)"
                                                      "floating-point")
                                                 (get-output-stream-string number)))))
                               (:in-fraction
                                (case ch
                                  ((#\e #\E)
                                   (setf state :after-exponent)
                                   (write-char (getchar scanner) number))
                                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                   (write-char (getchar scanner) number))
                                  (otherwise
                                   (when (and ch (alphanumericp ch))
                                     (invalid-char ch "floating-point"
                                                   (get-output-stream-string number)))
                                   (loop-finish))))
                               (:after-exponent
                                (case ch
                                  ((#\- #\+)
                                   (setf state :after-exponent-sign)
                                   (write-char (getchar scanner) number))
                                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                   (setf state :in-exponent)
                                   (write-char (getchar scanner) number))
                                  (otherwise
                                   (when (and ch (alphanumericp ch))
                                     (invalid-char ch "floating-point exponent"
                                                   (get-output-stream-string number)))
                                   (loop-finish))))
                               (:after-exponent-sign
                                (case ch
                                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                   (setf state :in-exponent)
                                   (write-char (getchar scanner) number))
                                  (otherwise
                                   (when (alphanumericp ch)
                                     (invalid-char ch "floating-point exponent"
                                                   (get-output-stream-string number)))
                                   (loop-finish))))
                               (:in-exponent
                                (case ch
                                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                   (write-char (getchar scanner) number))
                                  (otherwise
                                   (when (alphanumericp ch)
                                     (invalid-char ch "floating-point exponent"
                                                   (get-output-stream-string number)))
                                   (loop-finish)))))
                         :finally (case state
                                    ((:after-dot :after-exponent :after-exponent-sign)
                                     (incomplete-floating-point-number
                                      (get-output-stream-string number))))))))
          (values text (read-from-string text)))))))


(defmethod scan-next-token ((scanner lua-scanner) &optional parser-data)
  (declare (ignore parser-data))
  (skip-spaces scanner)
  (setf (scanner-current-token scanner)
        (let ((column (scanner-column scanner))
              (line   (scanner-column scanner))
              (ch     (getchar scanner)))
          (flet ((invalid-char (ch)
                   (error 'scanner-error
                          :line    line
                          :column  column
                          :state    (scanner-state  scanner)
                          :current-token (scanner-current-token scanner)
                          :scanner scanner
                          :format-control "Invalid character ~S"
                          :format-arguments (list ch))))
            (case ch
              ((nil) nil)
              ((#\' #\")
               (multiple-value-bind (text value)
                   (scan-string-starting-with scanner ch)
                 (make-instance 'tok-string :column column :line line
                                :kind :string :text text :value value)))
              ((#\[)
               ;; [ [[…]] [=[…]=] [=…=[…]=…=]
               (let ((next (nextchar scanner)))
                 (case next
                   ((#\[ #\=)
                    (multiple-value-bind (text value)
                        (scan-string-starting-with-long-bracket scanner ch)
                      (make-instance 'tok-string :column column :line line
                                     :kind :string :text text :value value)))
                   (otherwise
                    (make-instance 'tok-special :column column :line line
                                   :kind :left-bracket :text "[")))))
              ((#\< #\= #\> #\~)
               (let ((next (nextchar scanner)))
                 (case next
                   ((#\=)
                    (let ((name (make-array 2 :element-type 'character
                                            :initial-contents (list ch (getchar scanner)))))
                      (make-instance 'tok-special :column column :line line
                                     :kind (case ch
                                             (#\< :le)
                                             (#\= :eq)
                                             (#\> :ge)
                                             (#\~ :ne))
                                     :text name)))
                   (otherwise
                    (when (char= ch #\~)
                      (invalid-char ch))
                    (let ((name (string ch)))
                      (make-instance 'tok-special :column column :line line
                                     :kind (case ch
                                             (#\< :lt)
                                             (#\= :assign)
                                             (#\> :gt))
                                     :text name))))))
              ((#\-)
               ;; - -- --[[…]] --[=[…]=] --[=…=[…]=…=]
               (let ((next (nextchar scanner)))
                 (if (char= next #\-)
                   (progn
                     (getchar scanner)
                     (if (char= (nextchar scanner) #\[)
                       (multiple-value-bind (text value)
                           (scan-string-starting-with-long-bracket scanner ch)
                         (if (lua-scanner-keep-comments scanner)
                           (make-instance 'tok-string :column column :line line
                                          :kind :string :text text :value value)
                           (scan-next-token scanner)))
                       (let ((comment (progn (ungetchar scanner ch)
                                             (ungetchar scanner ch)
                                             (readline scanner))))
                         (if (lua-scanner-keep-comments scanner)
                           (make-instance 'tok-comment :column column :line line
                                          :kind :comment :text comment)
                           (scan-next-token scanner)))))
                   (make-instance 'tok-special :column column :line line
                                  :kind :minus :text "-"))))
              ((#\+  #\* #\/ #\% #\^ #\# #\( #\) #\{ #\} #\] #\; #\: #\,)
               (make-instance 'tok-special :column column :line line
                              :kind (case ch
                                      (#\+ :plus)
                                      (#\* :times)
                                      (#\/ :divide)
                                      (#\% :modulo)
                                      (#\^ :power)
                                      (#\# :sharp)
                                      (#\( :left-paren)
                                      (#\) :right-paren)
                                      (#\{ :left-brace)
                                      (#\} :right-brace)
                                      (#\] :right-bracket)
                                      (#\; :semicolon)
                                      (#\: :colon)
                                      (#\, :comma))
                              :text (string ch)))
              ((#\.)
               ;; . .. ...
               (if (char= (nextchar scanner) #\.)
                 (progn
                   (getchar scanner)
                   (if (char= (nextchar scanner) #\.)
                     (progn
                       (getchar scanner)
                       (make-instance 'tok-special :column column :line line
                                      :kind :tridot :text "..."))
                     (make-instance 'tok-special :column column :line line
                                    :kind :duodot :text "..")))
                 (make-instance 'tok-special :column column :line line
                                :kind :unidot :text ".")))
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
               (ungetchar scanner ch)
               (multiple-value-bind (text value) (scan-number scanner)
                 (make-instance 'tok-number :column column :line line
                                :kind :number :text text :value value)))
              (otherwise
               ;; LUA alpha chars depend on the locale (isalpha).
               ;; We assume the CL implementation uses the locale for alpha-char-p.
               (if (and ch (or (alpha-char-p ch) (char= #\_ ch)))
                 (progn
                   (ungetchar scanner ch)
                   (let* ((name (scan-identifier scanner))
                          (kw (gethash name *lua-keywords*)))
                     (make-instance (if kw
                                      'tok-keyword
                                      'tok-identifier)
                       :column column :line line
                       :kind (or kw :identifier)
                       :text name)))
                 (invalid-char ch))))))))


(defun test/scan-stream (src)
  (loop
    :with scanner = (make-instance 'lua-scanner :source src)
    :for token = (scan-next-token scanner)
    :while token
    :do (progn
          (format t "~&~20A ~32S ~32S ~A~%"
                  (token-kind  (scanner-current-token scanner))
                  (token-value (scanner-current-token scanner))
                  (token-text  (scanner-current-token scanner))
                  (type-of (scanner-current-token scanner)))
          (finish-output))))

(defun test/scan-file (path)
  (with-open-file (src path)
    (test/scan-stream src)))

(defun test/scan-string (source)
  (with-input-from-string (src source)
    (test/scan-stream src)))

;; (test/scan-file #P "~/mission.lua")


;;;; THE END ;;;;

