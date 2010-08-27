;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               compile.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Replaces the Makefile.
;;;;    
;;;;    Usage:   (load "compile.lisp")
;;;;
;;;;    will compile all outdated files.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-13 <PJB> Added generation of ASD file and use of ASDF.
;;;;    2004-07-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

;; (defpackage "COM.INFORMATIMAGO.COMMON-LISP.COMPILE"
;;   (:use "COMMON-LISP")
;;   (:export "MAIN"))
;; (in-package "COM.INFORMATIMAGO.COMMON-LISP.COMPILE")


;;; Not used yet:
(defvar *prefix* "/usr/local/")
(defvar *module* "common-lisp")
(defvar *package-path* "com/informatimago/common-lisp")
;;; ----

(defun logger (ctrl &rest args)
  (format *trace-output* "~&;;;;~%;;;; ~?~%;;;;~%" ctrl args))
(logger "*** COMPILING COM.INFORMATIMAGO.COMMON-LISP ***")


(load "init.lisp")
;; package.lisp is loaded by init.lisp.
;;(package:load-package :com.informatimago.common-lisp.make-depends)
(setf package:*package-verbose* nil)

;; Load make-depends dependencies:
(defvar *make-depends-dependencies*
   '(package source-form utility ecma048 list string character-sets
     ascii stream file html make-depends))

(logger "COMPILE.LISP LOADING DEPENDENCIES")
(dolist (file *make-depends-dependencies*)
  (load (make-pathname
         :name (string file) :type "LISP" :version nil :case :common
         :defaults (or *load-pathname* *default-pathname-defaults*))))

(load "PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF.LISP")
(logger "COMPILE.LISP LOADED DEPENDENCIES")

(push (function package:package-system-definition)
      asdf:*system-definition-search-functions*)


(defparameter *sources*
  '(
	package 

	source-form                     ; used by READER and UTILITY, etc.
    reader                          ; used by SOURCE-TEXT
	source-text

    version
    script
    
	utility
    ascii                               ; = iso646-006 (US)
    ecma048                             ; = iso6429
	list
    dll
	queue
    array
	string 
	stream
    
    file ;; file uses stream
	peek-stream

    scanner
    parser 

    ;; avl
    ;; bbtree
    llrbtree
	dictionary 
    bset     
	brelation 
	graf 
	graph 
	
	graph-dot    
	graph-diagram 

	combination
    
	pmatch 
	picture  

    memory
    heap
	activity  
    message-queue

	float-binio  
	data-encoding 
    cons-to-ascii
	tree-to-ascii  
	tree-to-diagram 
	regexp-posix
	regexp-emacs

	rfc2822
    rfc3548
	iso639a 
	iso3166
    iso4217
    
    character-sets ; (previously IATA-CHARACTER-SETS) with implementation specific stuff..

    html-iso8879-1 
	html
    hquery
	htrans
	database 
	
    parse-html
    cache                               ; a generic disk-based cache.

	aliases 
	passwd 
    group
	primes 
    tea
    raiden
	make-depends 
    cxx                   ; Simple C++ Parser for call graph analysis.

	csv                             ; Coma-Separated-Values files.
    iban                            ; Internation Bank Account Number.
    rib                             ; Relevés d'Identité Bancaires.
    
    invoice            ; my personal accounting and invoicing package.
    
	browser  ; a file browser (and cd/pwd/pushd/popd/ls/cat/more cmds.

	ed                        ; a simple editor, for 
                                        ; common-lisp implementations
                                        ; lacking a COMMON-LISP:EDIT function...

    interactive
    )) 

(defparameter *source-type* "lisp")


(defun version++ (&optional path)
  "
DO:      Increment the version compilation number.
         The version is persistent, stored in a file named VERSION.DAT
         in the same directory as *LOAD-PATHNAME*, or at PATH.
RETURN:  The version as a string \"major.minor.compilation\"
"
  (flet ((read-version (file)
           (loop
              :for line = (read-line file nil nil)
              :for =pos = (when line (position (character "=") line))
              :while line
              :when =pos
              :collect (list (intern (string-upcase (subseq line 0 =pos)) "KEYWORD")
                             (read-from-string (subseq line (1+ =pos)))))))
    (let* ((default-path       (or *load-pathname* *default-pathname-defaults*))
           (version.path       (or path 
                                   (make-pathname :name "VERSION" :type "DAT"
                                                  :version :newest
                                                  :defaults default-path)))
           (version             (with-open-file (file version.path
                                                      :direction :input
                                                      :if-does-not-exist :error)
                                  (read-version file)))
           (version.major           (or (second (assoc :major       version)) 0))
           (version.minor           (or (second (assoc :minor       version)) 0))
           (version.compilation (1+ (or (second (assoc :compilation version)) 0)))
           (new-version `((:major       ,version.major)
                          (:minor       ,version.minor)
                          (:compilation ,version.compilation))))
      (with-open-file (file version.path
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
        (format file "~(~:{~A=~A~%~}~)" new-version))
      (values (format nil "~A.~A.~A"
                      version.major version.minor version.compilation)
              version.major version.minor version.compilation))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate the asdf system file, loading the sources.

(logger "GENERATING THE ASDF SYSTEM FILE")

(handler-bind ((warning #'muffle-warning))
  (com.informatimago.common-lisp.make-depends:generate-asd
   :com.informatimago.common-lisp *sources* *source-type*
   :version (version++)
   :implicit-dependencies '("package")
   :vanillap t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now, we generate a summary.html page.
;;;
(logger "GENERATING THE SUMMARY.HTML")
(handler-bind ((warning #'muffle-warning))
  (com.informatimago.common-lisp.make-depends:generate-summary
   *sources*
   :verbose nil
   :source-type *source-type*
   :summary-path "summary.html"
   :character-set "UTF-8"
   :repository-url (lambda (pp)
                     (format nil ;; "http://darcs.informatimago.com~
                             ;;  /darcs/public/lisp/~(~A/~A~).lisp"
                             "com/informatimago/~(~A/~A~).lisp"
                             (car (last (pathname-directory pp)))
                             (pathname-name pp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cleanup before asdf:load-op:
;;; we delete the package to let asdf:load-op load them cleanly.
;;;

(logger "CLEANING THE LOADED PACKAGES")

(defun package-use*-package-p (p q)
  "
RETURN: Whether the package P uses the package Q, or a package 
        that uses the package Q.
NOTE:   By definition, (PACKAGE-USE*-PACKAGE-P X X)
"
  (setf p (find-package p)
        q (find-package q))
  (loop
     :with processed = '()
     :with used = (list p)
     :while used
     ;; :do (print (list used processed))
     :do (let ((current (pop used)))
           (if (eq current q)
               (return-from package-use*-package-p t)
               (progn
                 (push current processed)
                 (dolist (new (package-use-list current))
                   (unless (member new processed)
                     (pushnew new used))))))
     :finally (return-from package-use*-package-p nil)))


(defun topological-sort (nodes lessp)
  "
RETURN: A list of NODES sorted topologically according to 
        the partial order function LESSP.
        If there are cycles (discounting reflexivity), 
        then the list returned won't contain all the NODES.
"
  (loop
     :with sorted = '()
     :with incoming = (map 'vector (lambda (to)
                                     (loop
                                        :for from :in nodes
                                        :when (and (not (eq from to))
                                                   (funcall lessp from to))
                                        :sum 1))
                           nodes)
     :with q = (loop
                  :for node :in nodes
                  :for inco :across incoming
                  :when (zerop inco)
                  :collect node) 
     :while q
     :do (let ((n (pop q)))
           (push n sorted)
           (loop
              :for m :in nodes
              :for i :from 0
              :do (when (and (and (not (eq n m))
                                  (funcall lessp n m))
                             (zerop (decf (aref incoming i))))
                    (push m q))))
     :finally (return (nreverse sorted))))


;; (defun print-graph (nodes edge-predicate)
;;   (flet ((initiale (package)
;;            (if (< (length "COM.INFORMATIMAGO.COMMON-LISP.")
;;                   (length (package-name package)))
;;                (subseq (package-name package)
;;                        (length "COM.INFORMATIMAGO.COMMON-LISP.")
;;                        (1+ (length "COM.INFORMATIMAGO.COMMON-LISP.")))
;;                (subseq (package-name package) 0 1))))
;;     (let* ((nodes (coerce nodes 'vector))
;;            (width (ceiling (log (length nodes) 10))))
;;       (loop
;;          :for i :from 0
;;          :for node :across nodes
;;          :initially (format t "~2%")
;;          :do (format t " ~VD: ~A~%" width i node)
;;          :finally (format t "~2%"))
;;       (loop
;;          :for j :from 0 :below (length nodes)
;;          :initially (format t " ~VD " width "")
;;          :do (format t " ~VD" width j)
;;          :finally (format t "~%"))
;;       (loop
;;          :for i :from 0 :below (length nodes)
;;          :do (loop
;;                 :for j :from 0 :below (length nodes)
;;                 :initially (format t "~A ~VD:"  (initiale (aref nodes i)) width i)
;;                 :do (format t " ~VD"
;;                             width
;;                             (if (funcall edge-predicate
;;                                          (aref nodes i) (aref nodes j))
;;                                 (concatenate 'string
;;                                   (initiale (aref nodes i))
;;                                   (initiale (aref nodes j)))
;;                                  ""))
;;                 :finally (format t "~%"))
;;          :finally (format t "~%")))))



;;; With topological-sort, we mustn't use a total order function like this one:
;; (defun package<= (p q)
;;   (cond ((eq p q) t)
;;         ((package-use*-package-p p q)
;;          (assert (not (package-use*-package-p q p))
;;                  (p q) "A circle could happen but it should not.")
;;          t)                                ; p<q
;;         ((package-use*-package-p q p) nil) ; p>q
;;         (t (string<= (package-name p) (package-name q)))))



(dolist (p (let* ((nodes
                    (delete-if-not
                     (lambda (p)
                       (let ((prefix "COM.INFORMATIMAGO.COMMON-LISP."))
                         (and (<  (length prefix) (length (package-name p)))
                              (string= prefix (package-name p)
                                       :end2 (length prefix)))))
                     (copy-list (list-all-packages))))
                   (sorted
                    (topological-sort nodes
                                      (function package-use*-package-p)))
                   (cyclic (set-difference nodes sorted)))
              (when cyclic
                (format t "Cyclic nodes = ~S~%" cyclic))
              (nconc cyclic sorted)))
  (delete-package p))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finally, we compile and load the system
;;;

(logger "COMPILING THE ASDF SYSTEM")
(setf asdf:*compile-file-warnings-behaviour* :ignore)
(let ((*load-verbose* t)
      (*compile-verbose* t)
      (asdf::*verbose-out* t))
  (asdf:operate 'asdf:load-op :com.informatimago.common-lisp))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
