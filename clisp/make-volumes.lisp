;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               make-volumes.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;NOWEB:              T
;;;;DESCRIPTION
;;;;    
;;;;    This package exports a function to tally a directory tree 
;;;;    and make if it 'volumes' of a given maximum size.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-06-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2003 - 2015
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
;;;;****************************************************************************

(defpackage "COM.INFORMATIMAGO.CLISP.MAKE-VOLUMES"
  (:documentation
   "This package exports a function to tally a directory tree and make if it
    'volumes' of a given maximum size.")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.CLISP.SUSV3")
  (:export  "MAIN" "MAKE-VOLUMES"))
(in-package  "COM.INFORMATIMAGO.CLISP.MAKE-VOLUMES")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TREE 
;;


(defun deep-copy-tree (tree)
  "
NOTE:           COPY-TREE ONLY DUPLICATES THE CONS, NOT THE OBJECTS.
                THIS IS UNFORTUNATE, BECAUSE WE OFTEN NEED TO DUPLICATE
                THE OBJECTS (STRINGS, ARRAY) TOO, BECAUSE OF THE
                MUTABLE/IMMUTABLE PROBLEM.
DO:             MAKES A COPY OF THE TREE, COPYING THE LEAF OBJECTS TOO.
"
  (cond
    ((consp tree)   (cons (deep-copy-tree (car tree))
                          (deep-copy-tree (cdr tree))))
    ((vectorp tree) (copy-seq tree))
    (t              tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AN ENUMERATOR IS A FUNCTION THAT RETURNS EACH TIME IT'S CALLED THE
;; NEXT ITEM OF A (POSSIBLY VIRTUAL AND POSSIBLY INFINITE) SEQUENCE AND T.
;; WHEN NO ITEM REMAINS, IT RETURNS (VALUES NIL NIL).
;; ENUMERATORS CAN BE CONCATENATED WITH APPEND-ENUMERATORS.
;; (OF COURSE APPENDING AN ENUMERATOR AFTER AN INFINITE ENUMERATOR WOULD
;; BE USELESS).


(defun make-list-enumerator (list)
  "
RETURN:         A ENUMERATOR FUNCTION FOR THE LIST.
NOTE:           THE ENUMERATOR FUNCTION RETURNS IN TURN EACH ELEMENT OF THE
                LIST AS FIRST VALUE AND A BOOLEAN T UNLESS THE END OF THE
                LIST IS REACHED AS SECOND VALUE.
"
  (lambda ()
    (multiple-value-prog1
        (values (car list) (not (null list)))
      (setq list (cdr list)))))



(defun append-enumerators (&rest enumerators)
  "
RETURN:        An enumerator that enumerates all the enumerators in turn.
"
  (lambda ()
    (block :meta-enum
      (loop
         (if (null enumerators)
             (return-from :meta-enum (values nil nil))
             (multiple-value-bind (val ind) (funcall (car enumerators))
               (if ind
                   (return-from :meta-enum (values val ind))
                   (pop enumerators))))))))



(defun collect-enumerator (enumerator)
  (do ((result '())
       (done nil))
      (done result)
    (multiple-value-bind (val ind) (funcall enumerator)
      (if ind
          (push val result)
          (setq done t)))))



(defun map-enumerator (lambda-expr enumerator)
  (do ((result '())
       (done nil))
      (done (nreverse result))
    (multiple-value-bind (val ind) (funcall enumerator)
      (if ind
          (push (funcall lambda-expr val) result)
          (setq done t)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UNIX PATHNAMES

(defun basename (unix-path)
  "
UNIX-PATH:  A STRING CONTAINING A UNIX PATH.
RETURN:         THE BASENAME, THAT IS, THE LAST COMPONENT OF THE PATH.
                TRAILING '/'S ARE REMOVED FIRST.
"
  (do* ((end (do ((end (1- (length unix-path)) (1- end)))
                 ((or (< end 0)
                      (char/= (character "/") (char unix-path end)))
                  (1+ end))))
        (start (1- end) (1- start)))
       ((or (< start 0)
            (char= (character "/") (char unix-path start)))
        (subseq unix-path (1+ start) end))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; XSI:



(defun unix-fs-node-name (node) ;; --> NAME
  (declare (ignore node)))
(defun unix-fs-node-kind (node)
  (declare (ignore node)))
(defun unix-fs-node-directory-path (node) ;; --> "/DIR/PATH"
  (declare (ignore node)))
(defun unix-fs-node-path (node) ;; --> /DIR/PATH/NAME
  (declare (ignore node)))

(defun logical-pathname-namestring-p (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun parse-logical-pathname-namestring (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun parse-unix-pathname-namestring (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun safe-make-pathname (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))
(defun safe-directory (&rest arguments) (declare (ignore arguments)) (error "NOT IMPLEMENTED YET"))


(defun make-volumes (root-dir)
  (let ((root-apath
         ;; TODO: WHAT IF NOT UNIX?
         (if (logical-pathname-namestring-p  root-dir)
             (parse-logical-pathname-namestring root-dir)
             (parse-unix-pathname-namestring root-dir)))
        pathspec)
    (when (eq :error root-apath)
      (error "BAD PATHNAME ~S." root-dir))
    (setq pathspec (safe-make-pathname
                    :host      (aget root-apath :host)
                    :device    (aget root-apath :device)
                    :directory (append (aget root-apath :directory)
                                       (list :wild-inferiors))
                    :name :wild
                    :type :wild
                    :version nil
                    :case :common))
    (format t "PATHSPEC=~S~%" pathspec)
    (safe-directory pathspec)))

#||
(LOAD "PACKAGES:COM;INFORMATIMAGO;ENCOURS;MAKE-VOLUMES")
(IN-PACKAGE "COM.INFORMATIMAGO.CLISP.MAKE-VOLUMES")
(MAKE-VOLUMES "/tmp/")
||#

;;;; THE END ;;;;
