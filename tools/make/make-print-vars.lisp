;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               make-dump-makefile-vars.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Extracts the variable names from variable definitions from a makefile,
;;;;    and prints a rule to print the value of each variable.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2023-04-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2023 - 2023
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

(defpackage "COM.INFORMATIMAGO.TOOLS.MAKEFILE.PRINT-VARS"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.TOOLS.MAKE")
  (:export "MAIN"))
(in-package "COM.INFORMATIMAGO.TOOLS.MAKEFILE.PRINT-VARS")

(defun split-tokens (line rest-of-lines)
  ;; TODO: use a proper Makefile token parser!!!
  (loop
    :with eof := (length line)
    :with start := 0
    :with end := 0
    :while (< end eof)
    :collect (loop 
               :with state := :in-token
               :until (eql state :got-token)
               :do (let ((ch (aref line end)))
                     (cond
                       ((find ch "!\"%&'()*,-./0123456789;<=>@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~")
                        (incf end))
                       ((char= ch #\\)
                        (incf end 2))
                       ((char= ch #\$)
                        (incf end)
                        (cond
                          ((<= eof end)
                           (setf state :got-token))
                          ((char= (aref line end) #\$)
                           (incf end)
                           (setf state :got-token))
                          ((char= (aref line end) #\()
                           (incf end)
                           (loop
                             :while (and (<= eof end) rest-of-lines)
                             :do (setf line (concatenate 'string line (pop rest-of-lines))
                                       eof (length line)))
                           (when (<= eof end)
                             (error "EOF in $(…) expression~%~S" line))
                           (loop
                             :while (char/= (aref line end) #\))
                             :do (incf end)
                             :finally (if (< end eof)
                                          (progn
                                            (incf end)
                                            (setf state :got-token))
                                          )))
                          (t 
                           (incf end))))
                       ((find ch "+?")  ; += ?=
                        (cond
                          ((<= eof (1+ end))
                           (incf end)
                           (setf state :got-token))
                          ((char= (aref line (1+ end)) #\=)
                           (when (= start end)
                             (incf end 2))
                           (setf state :got-token))
                          (t
                           (incf end))))
                       ((char= ch #\:)  ; : or :=
                        (cond
                          ((<= eof (1+ end))
                           (incf end))
                          ((char= (aref line (1+ end)) #\=)
                           (when (= start end)
                             (incf end 2)))
                          (t
                           (when (= start end)
                             (incf end))))
                        (setf state :got-token)) 
                       ((char= ch #\=)
                        (when (= start end)
                          (incf end))
                        (setf state :got-token))
                       ((find ch #(#\space #\tab))
                        (if (< start end)
                            (setf state :got-token)
                            (loop
                              :while (and (< end eof)
                                          (find (aref line end) #(#\space #\tab)))
                              :do (incf end)
                              :finally (setf start end))))
                       ((char= ch #\#)
                        ;; if # is at the start of a token,
                        ;; then it's a comment to the end of the line
                        ;; else it's a constituent character.
                        (if (= start end)
                            (setf end eof)
                            (incf end)))
                       (t (error "Invalid character in makefile: ~C (~D)~%~S"
                                 ch (char-code ch)
                                 line))))
               :do (when (<= eof end)
                     (setf state :got-token))
               :finally (return (values (prog1 (subseq line start end)
                                          (setf start end))
                                        rest-of-lines)))))

(defun join-continuation-lines (lines)
  (loop
    :with new-lines := '()
    :with big-line := '()
    :for line :in lines
    :if (and (plusp (length line))
             (char= #\\ (aref line (1- (length line)))))
    :do (push (subseq line 0 (1- (length line))) big-line)
        (push " " big-line)
    :else
    :do (if big-line
            (progn
              (push line big-line)
              (push (format nil "~{~A~}" (nreverse big-line)) new-lines)
              (setf big-line '()))
            (push line new-lines))
    :end
    :finally (return (nreverse new-lines))))

(defun process-makefile (lines handler)
  (let ((lines (join-continuation-lines lines)))
    (loop
      :while lines
      :do (let ((line (pop lines)))
            (unless (prefixp "#" line)
              (let ((tokens (split-tokens line)))
                (cond ((null tokens))
                      ((string= "define" (first tokens))
                       (funcall handler :define
                                (cons line
                                      (loop :for line := (pop lines)
                                            :collect line
                                            :until (prefixp "endef" line)))))
                      ((member ":" (rest tokens)
                               :test (function string=))
                       (funcall handler :rule (cons line
                                                    (loop :for line := (pop lines)
                                                          :collect line
                                                          :while (or (prefixp "#" line)
                                                                     (prefixp "	" line)
                                                                     (prefixp " " line))))))
                      ((member (elt tokens 1)
                               '(":=" "=" "+=" "?=")
                               :test (function string=))
                       (funcall handler :variable tokens))
                      (t
                       (error "Unexpected Makefile line: ~S~%~S" line tokens))))))))
  (values))

(defun extract-variables-from-makefile (lines)
  (let ((variables '())
        (i 0)
        (last-line nil))
    (process-makefile
     lines
     (lambda (kind lines)
       ;; (print (list kind lines))
       (incf i)
       (setf last-line (first lines))
       (when (zerop (mod i 100000))
         (format *trace-output* "# ~D: ~A~%" i last-line)
         (finish-output *trace-output*))
       (when (eq kind :variable)
         (push (string-trim " " (subseq (first lines) 0 (or (search ":=" (first lines))
                                                            (search "=" (first lines))
                                                            0)))
               variables))))
    (sort variables
          (lambda (a b)
            (let ((ap (position #\. a))
                  (bp (position #\. b)))
              (if (and ap bp)
                  (let ((ra (subseq a ap))
                        (rb (subseq b bp)))
                    (if (string= ra rb)
                        (string< a b)
                        (string< ra rb)))
                  (string< a b)))))))

(defun quoted (string)
  (with-output-to-string (*standard-output*)
	(princ "'")
    (loop :for ch :across string
          :do (case ch
                ((#\\ #\') (princ "\\") (princ ch))
                (otherwise (princ ch))))
    (princ "'")))

(defun regexp-quoted (string)
  (with-output-to-string (*standard-output*)
    (loop :for ch :across string
          :do (case ch
                ((#\[ #\( #\. #\\ #\? #\+ #\^ #\$) (princ "\\") (princ ch))
                (otherwise (princ ch))))))

(defun generate-print-vars-rule (variables output)
  (format output "# -*- mode:makefile -*-~%")
  (format output "vars:~%~:{	@printf '%s = %s\\n' ~A \"$(~A)\"~%~}~%"
	      (mapcar (lambda (var) (list (quoted var) var)) variables)))

(defun generate-variable-mangling (variables output)
  (format output "# -*- mode:sed -*-~%")
  (format output "~:{s/\\<~A\\>/~A/g~%~}"
	      (mapcar (lambda (var) (list (regexp-quoted var) (gentemp "V"))) variables)))

(defun usage (pname)
  (format t "~A usage:~%" pname)
  (format t "    ~A -h|--help~%" pname)
  (format t "    ~A -h|--help~%" pname)
  (format t "    ~A [-m|--mangle]   $path_to_makefile > $path_to_makefile-vars_include~%" pname)
  (format t "    ~A [-m|--mangle] < $path_to_makefile > $path_to_makefile-vars_include~%" pname))

(defconstant +ex-ok+ 0)
(defconstant +ex-usage+ 64)
(defconstant +ex-oserr+ 71)

(defun main (pname &rest arguments)
  (handler-case
      (let ((action (function generate-print-vars-rule))) 
        ;; check options:
        (cond
          ((or (member "-h" arguments :test (function string=))
               (member "--help" arguments :test (function string=)))
           (usage pname)
           (return-from main +ex-ok+))
          
          ((or (string= "-m" (first arguments))
	           (string= "--mangle" (first arguments)))
           (pop arguments)
           (setf action (function generate-variable-mangling))))
        ;; check remaining unknown options:
        (when (find-if (lambda (argument)
                         (prefixp "-" argument))
                       arguments)
          (error "Invalid argument: ~A" (find-if (lambda (argument)
                                                   (prefixp "-" argument))
                                                 arguments))
          (return-from main +EX-USAGE+))
        ;; perform action:
        (funcall action
                 (extract-variables-from-makefile
                  (cond
                    ((endp arguments)
                     (stream-to-string-list *standard-input*))
                    ((endp (rest arguments))
                     (let ((makefile-namestring (first arguments)))
                       (string-list-text-file-contents makefile-namestring)))
                    (t
                     (error "Too many arguments."))))
                 *standard-output*))
    (CCL::SIMPLE-STREAM-ERROR (err)
      (format *error-output* "ERROR: ~A~%" err)
      (finish-output *error-output*)
      (return-from main +ex-oserr+))
    (error (err)
      (print (type-of err) *error-output*)
      (format *error-output* "ERROR: ~A~%" err)
      (finish-output *error-output*)
      (return-from main +ex-usage+)))
  +ex-ok+)

;; #-testing
;; (ccl:quit (apply (function main)
;;                  (first (uiop:raw-command-line-arguments))
;;                  (uiop:command-line-arguments)))

               
