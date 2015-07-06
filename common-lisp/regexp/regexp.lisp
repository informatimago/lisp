;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               regexp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Regexp Portability Layer.
;;;;
;;;;    This package provides a common API over cl-ppcre or #+clisp regexp.
;;;;
;;;;    On clisp, the user can choose to use the REGEXP package instead of
;;;;    CL-PPCRE, by adjoining :use-regexp instead of :use-ppcre to
;;;;    *features* (see the beginning of this file). 
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-02 <PJB> Extracted from rdp.lisp
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
 
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP"
  (:use "COMMON-LISP")
  (:export "*ENGINE*" "ENGINES" "SELECT")
  ;; The function API, dispatches on *ENGINE*:
  (:export "SPLIT-STRING" "STRING-MATCH" "MATCH-STRING"
           "MATCH-BEGINNING" "MATCH-END" "REGEXP-MATCH-ANY"
           "REGEXP-COMPILE" "REGEXP-QUOTE-EXTENDED")
  ;; The generic function API, first argument must be an engine designator.
  ;; (Users may implement methods for their own engines)
  (:export "-SPLIT-STRING" "-STRING-MATCH" "-MATCH-STRING"
           "-MATCH-BEGINNING" "-MATCH-END" "-REGEXP-MATCH-ANY"
           "-REGEXP-COMPILE" "-REGEXP-QUOTE-EXTENDED")
  (:documentation
   "

Regexp Portability Layer.

This package provides a common API over cl-ppcre or #+clisp regexp.

Use:
      (select :ppcre) ; or
      #+clisp (select :regexp) ; when the REGEXP package is available.

to choose between the two currently available regexp engines.

License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2015 - 2015
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>
"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.REGEXP.REGEXP")

#+clisp (eval-when (:compile-toplevel :load-toplevel :execute)
          (when (find-package "REGEXP")
            (pushnew :use-regexp *features*)))

(defvar *engine* :ppcre
  "Current possible values are :ppcre or :regexp.  It is possible to
dynamically bind this variable temporarily to switch the engine.")

(defun engines ()
  "RETURN: A list of designators for the available regexp engines."
  (append '(:ppcre)
          #+use-regexp '(:regexp) 
          '(#|:posix :emacs|#)))

(defun select (engine)
  "Sets the `*ENGINE*`."
  (assert (member engine (engines))  (engine)
          "The selected engine must be one of ~{~S~^ ~}" (engines))
  (setf *engine* engine))



(defgeneric -split-string (engine string regexp)
  (:method ((engine (eql :ppcre)) string regexp)
    (cl-ppcre:split regexp string))
  #+use-regexp
  (:method ((engine (eql :regexp)) string regexp)
    (regexp:regexp-split regexp string)))

(defun split-string (string regexp)
  (-split-string *engine* string regexp))


(defvar *string-match-results* '())

#+use-regexp
(defun nsubseq (sequence start &optional (end nil))
  "
RETURN:  When the SEQUENCE is a vector, the SEQUENCE itself, or a dispaced
         array to the SEQUENCE.
         When the SEQUENCE is a list, it may destroy the list and reuse the
         cons cells to make the subsequence.
"
  (if (vectorp sequence)
      (if (and (zerop start) (or (null end) (= end (length sequence))))
          sequence
          (make-array (- (if end
                             (min end (length sequence))
                             (length sequence))
                         start)
                      :element-type (array-element-type sequence)
                      :displaced-to sequence
                      :displaced-index-offset start))
      (let ((result (nthcdr start sequence)))
        (when end
          (setf (cdr (nthcdr (- end start -1) sequence)) nil))
        result)))

(defgeneric -string-match (engine regexp string &key start end)
  (:method ((engine (eql :ppcre)) regexp string &key (start 0) (end nil))
    (setf *string-match-results*
          (let ((results (multiple-value-list
                          (if (stringp regexp)
                              (cl-ppcre:scan regexp       string :start start :end (or end (length string)))
                              (cl-ppcre:scan (cdr regexp) string :start start :end (or end (length string)))))))
            (if (equal '(nil) results)
                nil
                (destructuring-bind (as ae ss es) results
                  (list as ae
                        (concatenate 'vector (vector as) ss)
                        (concatenate 'vector (vector ae) es)))))))
  #+use-regexp
  (:method ((engine (eql :regexp)) regexp string &key (start 0) (end nil))
    (setf *string-match-results*
          (let ((results (if (stringp regexp)
                             (multiple-value-list
                              (regexp:match regexp string :start start :end end :extended t :ignore-case nil :newline t :nosub nil))
                             (regexp:regexp-exec (cdr regexp) string :start start :end end :return-type 'list))))
            (if (equal '(nil) results)
                nil
                results)))))

(defun string-match (regexp string &key (start 0) (end nil))
  (-string-match *engine* regexp string :start start :end end))


(defgeneric -match-string (engine index string &optional match-results)
  (:method ((engine (eql :ppcre)) index string &optional (match-results *string-match-results*))
    (let ((start (ignore-errors (aref (elt match-results 2) index)))
          (end   (ignore-errors (aref (elt match-results 3) index))))
      (when (and start end)
        (subseq string start end))))
  #+use-regexp
  (:method ((engine (eql :regexp)) index string &optional (match-results *string-match-results*))
    (let ((m (elt match-results index)))
      (when m (regexp:match-string string m)))))

(defun match-string (index string &optional (match-results *string-match-results*))
  (-match-string *engine* index string match-results))


(defgeneric -match-beginning (engine index &optional match-results)
  (:method ((engine (eql :ppcre)) index &optional (match-results *string-match-results*))
    (ignore-errors (aref (elt match-results 2) index)))
  #+use-regexp
  (:method ((engine (eql :regexp)) index &optional (match-results *string-match-results*))
    (let ((m (elt match-results index)))
      (when m (regexp:match-start m)))))

(defun match-beginning (index &optional (match-results *string-match-results*))
  (-match-beginning *engine* index match-results))


(defgeneric -match-end (engine index &optional match-results)
  (:method ((engine (eql :ppcre)) index &optional (match-results *string-match-results*))
    (ignore-errors (aref (elt match-results 3) index)))
  #+use-regexp
  (:method ((engine (eql :regexp)) index &optional (match-results *string-match-results*))
    (let ((m (elt match-results index)))
      (when m (regexp:match-start m)))))

(defun match-end (index &optional (match-results *string-match-results*))
  (-match-end *engine* index match-results))


(defgeneric -regexp-match-any (engine groupsp)
  (:method ((engine (eql :ppcre)) groupsp)
    (if groupsp "(.*)" ".*"))
  #+use-regexp
  (:method ((engine (eql :regexp)) groupsp)
    (if groupsp "(.*)" ".*")))

(defun regexp-match-any (groupsp)
  (-regexp-match-any *engine* groupsps))


(defgeneric -regexp-compile (engine regexp)
  (:method ((engine (eql :ppcre)) groupsp)
    (cl-ppcre:create-scanner regexp
                             :case-insensitive-mode nil
                             :multi-line-mode nil
                             :extended-mode nil
                             :destructive nil))
  #+use-regexp
  (:method ((engine (eql :regexp)) groupsp)
    (regexp:regexp-compile regexp
                           :extended t
                           :ignore-case nil
                           :newline t
                           :nosub nil)))

(defun regexp-compile (regexp)
  (-regexp-compile *engine* regexp))


(defgeneric -regexp-quote-extended (engine string)
  (:method ((engine (eql :ppcre)) string)
    (cl-ppcre:quote-meta-chars string))
  #+use-regexp
  (:method ((engine (eql :regexp)) string)
    ;; #+clisp regexp:regexp-quote doesn't quote extended regexps...
    ;;        (regexp:regexp-quote "(abc .*" t) --> "(abc \\.\\*"  instead of "\\(abc \\.\\*"
    (let* ((special-characters "^.[$()|*+?{\\")
           (increase (count-if (lambda (ch) (find ch special-characters)) string)))
      (if (zerop increase)
          string
          (let ((result (make-array (+ (length string) increase)
                                    :element-type 'character)))
            (loop
              :with i = -1
              :for ch :across string
              :do (if (find ch special-characters)
                      (setf (aref result (incf i)) #\\
                            (aref result (incf i)) ch)
                      (setf (aref result (incf i)) ch)))
            result)))))

(defun regexp-quote-extended (string)
  (-regexp-quote-extended *engine* string))

;;;; THE END ;;;;

