;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               logical-pathname.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Parses and validates a logical pathname namestring.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-11-01 <PJB> Extracted from COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM.
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
(defpackage "COM.INFORMATIMAGO.CLEXT.LOGICAL-PATHNAME"
  (:use "COMMON-LISP"
        "CL-PPCRE"
        "SPLIT-SEQUENCE")
  (:export "PARSE-LOGICAL-PATHNAME"))
(in-package "COM.INFORMATIMAGO.CLEXT.LOGICAL-PATHNAME")


(defun re-compile (re &key extended)
  (cl-ppcre:create-scanner re :extended-mode extended))

(defun re-exec (re string &key (start 0) (end nil))
  (multiple-value-bind (mstart mend starts ends)
      (cl-ppcre:scan re string
                     :start start
                     :end (or end (length string)))
    (and mstart mend
         (values-list (cons (list mstart mend)
                            (map 'list (lambda (s e)
                                         (if (or s e)
                                             (list s e)
                                             nil))
                              starts ends))))))


(defun re-match-string (string match)
  (subseq string (first match) (second match)))

(defun re-match (regexp string)
  (re-exec (re-compile regexp :extended t) string))



(defparameter *logical-pathname-regexp*
  (let ((host "(([-A-Z0-9]+):)?")
        (dire "(;)?(([-*A-Z0-9]+;|\\*\\*;)*)")
        (name "([-*A-Z0-9]+)?")
        (type "(.([-*A-Z0-9]+)(.([0-9]+|newest|NEWEST|\\*))?)?"))
    #-(and)
    (concatenate 'string "^" host dire name type "$")
    (re-compile (concatenate 'string "^" host dire name type "$")
                :extended t)))


(defun parse-logical-pathname (string &key (start 0) (end nil))
  "
RETURN: a list containing the pathname components: (host directory name type version)
"
  ;; TODO: implement junk-allowed
  ;; TODO: return new position.
  (flet ((wild (item part wild-inferiors-p)
           (cond ((string= "*"  item) :wild)
                 ((and wild-inferiors-p (string= "**" item)) :wild-inferiors)
                 ((search  "**" item) 
                  (error "Invalid ~A part: ~S; ~
                                \"**\" inside a wildcard-world is forbidden."
                         part item))
                 ((position #\* item) (list :wild-word item))
                 (t item))))
    (multiple-value-bind (all
                          dummy0 host 
                          relative directories dummy1
                          name
                          dummy2 type dummy3 version) 
        (re-exec *logical-pathname-regexp* string :start start :end end)
      (declare (ignore dummy0 dummy1 dummy2 dummy3))
      (if all
          (list (and host        (re-match-string string host))
                (if relative :relative :absolute)
                (and directories
                     (mapcar
                      (lambda (item) (wild item "directory" t))
                      (butlast (split-sequence #\; (re-match-string
                                                    string directories)))))
                (and name
                     (let ((item (re-match-string string name)))
                       (wild item "name" nil)))
                (and type        
                     (let ((item (re-match-string string type)))
                       (wild item "type" nil)))
                (and version
                     (let ((version (re-match-string string version)))
                       (cond 
                         ((string= "*" version) :wild)
                         ((string-equal "NEWEST" version) :newest)
                         (t (parse-integer version :junk-allowed nil))))))
          (error "Syntax error parsing logical pathname ~S"
                 (subseq string start end))))))

#-(and)
(mapc
 (lambda (path) (print (ignore-errors (parse-logical-pathname path))))
 '("SYS:KERNEL;PATH;LOGICAL.LISP"
   "SYS:;KERNEL;PATH;LOGICAL.LISP"
   "SYS:;KERNEL;**;LOGICAL.LISP"
   "SYS:;KERNEL;**;LO*L.LISP"
   "SYS:kernel;path/logical.lisp"))


;;;; THE END ;;;;
