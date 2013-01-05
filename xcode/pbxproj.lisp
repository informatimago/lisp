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
                   :while (or (alphanumericp ch) (find ch "._/"))
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
                   (progn
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
               (if (or (alphanumericp ch) (char= ch #\.))
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

(when (find-method (function scanner-current-token) '()  '(rdp-scanner) nil)
  (remove-method (function scanner-current-token) (find-method (function scanner-current-token) '()  '(rdp-scanner))))

(defmethod accept ((scanner rdp-scanner) expected)
  (let ((token (scanner-current-token scanner)))
   (if (word-equal expected token)
       (prog1 (list (token-kind token)
                    (token-text token)
                    (token-column token))
         (scan-next-token scanner))
       (call-next-method))))


(defun test/scan-stream (src)
  (loop
    :with scanner = (make-instance 'pbxproj-scanner :source src :state 0)
    ;; :initially (progn
    ;;              (advance-line scanner)
    ;;              (format t "~2%;; ~A~%;; ~A~%"
    ;;                      (scanner-buffer scanner)
    ;;                      (scanner-current-token scanner)))
    :do (progn
          (scan-next-token scanner)
          (format t "~&~3A ~20A ~20S ~3A ~3A ~20A ~A~%"
                  (scanner-state scanner)
                  (token-kind (scanner-current-token scanner))
                  (token-text (scanner-current-token scanner))
                  (eofp (scanner-current-token scanner))
                  (eofp (scanner-current-token scanner))
                  "-" ;; (scanner-previous-token-kind scanner) 
                  (type-of (scanner-current-token scanner)))
          (finish-output))
    :while (scanner-current-token scanner)))

(defun test/scan-file (path)
  (with-open-file (src path)
    (test/scan-stream src)))

(defun test/scan-string (source)
  (with-input-from-string (src source)
    (test/scan-stream src)))



;;----------------------------------------------------------------------
;; pbxproj parser
;;----------------------------------------------------------------------

(when (and (find-package "COM.INFORMATIMAGO.RDP")
           (find-symbol "*BOILERPLATE-GENERATED*" "COM.INFORMATIMAGO.RDP")
           (boundp (find-symbol "*BOILERPLATE-GENERATED*" "COM.INFORMATIMAGO.RDP")))
  (setf (symbol-value (find-symbol "*BOILERPLATE-GENERATED*" "COM.INFORMATIMAGO.RDP")) nil))


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




(defun test/parse-stream (src)
  (let ((scanner (make-instance 'pbxproj-scanner :source src :state 0)))
    (parse-pbxproj scanner)))


(defun test/parse-string (source)
  (with-input-from-string (src source)
    (test/parse-stream src)))

;; (test/scan-file  #P"~/works/abalone-macosx/Abalone-10.7/Abalone.xcodeproj/project.pbxproj")

(with-open-file (stream #P"~/works/abalone-macosx/Abalone-10.7/Abalone.xcodeproj/project.pbxproj")
 (parse-pbxproj  stream))
