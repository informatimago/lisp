;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               xkcd.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Builds an inverted index of xkcd.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-10-22 <PJB> Created.
;;;;BUGS
;;;;
;;;;    Note: xmls-tools and parse-html don't agree on the format of
;;;;          sexps representing xml/html attributes.
;;;;    
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(require :xmls-tools #P"~/src/public/lisp/future/xmls-tools.lisp")
(defpackage "COM.INFORMATIMAGO.XKCD"
  (:use "COMMON-LISP"
        "XMLS-TOOLS"
        "QL-HTTP"
        "COM.INFORMATIMAGO.COMMON-LISP.HTML-PARSER.PARSE-HTML"
        "COM.INFORMATIMAGO.COMMON-LISP.HTML-GENERATOR.HTML-ENTITIES"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.CLEXT.CHARACTER-SETS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "UPDATE-XKCD-INDEX"
           "XKCD")
  (:documentation "
Builds an inverted index of xkcd.com comics, and let you search them by keyword.

Use XKCD to perform a keyword search on xkcd.com comics.
The first search will download all the xkcd html pages to build the index,
which is cached in ~/.cache/xkcd/.
The ulterior searches only hit the site once a day.

Example:

     (xkcd \"standard\" :verbose t) ; the first time,
     (xkcd \"standard\")            ; in normal use.

Copyright Pascal J. Bourguignon 2014 - 2014
AGPL3
"))
(in-package "COM.INFORMATIMAGO.XKCD")

(defparameter *verbose* nil
  "Print messages while processing.")

(defparameter *utf-8* (character-set-to-lisp-encoding :utf-8)
  "The external-format for utf-8.")

(defparameter *last-day* 0
  "A monotonically increasing integer representing the day of
publication of the last xkcd.com comic.")

(defparameter *raw-index* '()
  "An a-list mapping xkcd indexes to a list of title and alt texts.")

(defparameter *inverted-index* nil
  "A hash-table mapping words to xkcd indexes.")

(defun xkcd-url (index)
  (format nil "http://xkcd.com/~D/" index))

(defun entity-text (entity)
  (etypecase entity
    (null   '(""))
    (cons   (mapcan (function entity-text) (cddr entity)))
    (string (list entity))))


(defun fetch-xkcd-raw-index (&optional (min 1) (max))
  "Fetches the xkcd pages from min to max (or to the first 404
\(excluding index 404) and push new entires to *RAW-INDEX*."
  (flet ((prepare-text (text)
           (format nil "~{~A~^ ~}" (mapcar (function melt-entities) (entity-text text)))))
    (loop
      :with path = #p"/tmp/xkcd"
      :for i :from min
      :for url = (xkcd-url i)
      :while (or (null max) (<= i max))
      :do (handler-case
              (progn
                (fetch url path
                       :if-exists :supersede
                       :follow-redirects t
                       :quietly (not *verbose*))
                (let* ((document (find :html (parse-html-file
                                              path
                                              :verbose nil
                                              :external-format *utf-8*)
                                       :key (lambda (item) (and (listp item) (first item)))))
                       ;; (ctitle (third (first (remove-if-not (lambda (entity)
                       ;;                                        (string= "ctitle" (getf (second entity) :id)))
                       ;;                                      (find-children-tagged document :div)))))
                       )
                  (dolist (element (find-children-tagged document :img))
                    (let ((title (getf (element-attributes element) :title))
                          (alt   (getf (element-attributes element) :alt)))
                      (when  title
                        (pushnew (list i
                                       ;; (when ctitle (prepare-text ctitle))
                                       (when alt    (prepare-text alt))
                                       (when title  (prepare-text title)))
                                 *raw-index*
                                 :test (function equal)))))))
            (unexpected-http-status (err)
              (unless (and (= i 404) (= 404 (unexpected-http-status-code err)))
                (error err)))))))


(defun tokenize (text)
  "Returns a list of unique downcased words in text"
  (remove-duplicates (loop
                       :with start = 0
                       :with len = (length text)
                       :for s = (position-if #'alphanumericp text :start start)
                       :for e = (and s (position-if-not #'alphanumericp text :start (1+ s)))
                       :while s
                       :collect (string-downcase (subseq text s e))
                       :while (and e (< e len))
                       :do (setq start e))
                     :test (function string=)))


(defun xkcd-file (name)
  (let ((path (merge-pathnames (format nil ".cache/xkcd/~A.sexp" name) (user-homedir-pathname))))
    (ensure-directories-exist path)
    path))

(defmacro define-persistent-variable (name pathname &key external-format default-value)
  (let ((vval (gensym)))
    `(progn
       (defun ,name ()
         (sexp-file-contents ,pathname :external-format ,external-format
                                       :if-does-not-exist ,default-value))
       (defun (setf ,name) (,vval)
         (setf (sexp-file-contents ,pathname :external-format ,external-format
                                             :if-does-not-exist :create
                                             :if-exists :supersede) ,vval)))))

(define-persistent-variable last-day       (xkcd-file "last-day")
  :external-format *utf-8* :default-value 0)
(define-persistent-variable raw-index      (xkcd-file "raw-index")
  :external-format *utf-8* :default-value nil)
(define-persistent-variable inverted-index (xkcd-file "inverted-index")
  :external-format *utf-8* :default-value '(hash-table :test equal))


(defun current-day ()
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time (get-universal-time) 0)
    (declare (ignore se mi ho))
    (+ (* (+ (* ye 100) mo) 100) da)))

(defun update-inverted-index ()
  (let ((index (make-hash-table :test (function equal) :size (length *raw-index*))))
    (loop :for (i ctitle title alt) :in *raw-index*
          :do (loop :for word :in (nconc (tokenize ctitle) (tokenize title) (tokenize alt))
                    :do (pushnew i (gethash word index '()))))
    (setf (inverted-index) (hash-table-to-sexp (setf *inverted-index* index)))))

(defun update-xkcd-index ()
  "
DO:  Load the indexes from the .cache; if the last data is older than
     current day, then try to fetch a new item, and re-compute the
     inverse index.
"
  (setf *last-day*       (or *last-day*       (last-day))
        *raw-index*      (or *raw-index*      (raw-index))
        *inverted-index* (or *inverted-index* (sexp-to-hash-table (inverted-index))))
  (when (< *last-day* (current-day))
    (let ((old-length (length *raw-index*))
          (min (1+ (reduce (function max) *raw-index* :initial-value 0 :key (function first)))))
      (handler-case (fetch-xkcd-raw-index min)
        (unexpected-http-status () nil))
      (unless (= (length *raw-index*) old-length)
        (setf (raw-index) *raw-index*)
        (update-inverted-index)
        (setf *last-day* (setf (last-day) (current-day)))))))


(defun raw-search (words)
  (sort (remove-duplicates (mapcan (lambda (word) (copy-list (gethash word *inverted-index*))) words))
        (function <)))

(defun xkcd-at (index)
  (find index *raw-index* :key (function first)))

(defun xkcd (words &key (verbose nil))
  "
DO:     Prints a list of xkcd.com urls (along with alt text and title),
        that contain at least one of the words.
RETURN: The list of urls.
"
  (let ((*verbose* verbose))
    (update-xkcd-index))
  (mapcar (lambda (i)
            (destructuring-bind (i alt title) (xkcd-at i)
              (let ((url (xkcd-url i)))
                (format t "~%~@{~@[- ~A~%~]~}~%"
                        url
                        (when alt    (nsubseq (string-justify-left alt    *print-right-margin* 2) 2))
                        (when title  (nsubseq (string-justify-left title  *print-right-margin* 2) 2)))
                url)))
          (etypecase words
            (symbol (raw-search (tokenize (string words))))
            (string (raw-search (tokenize words)))
            (list   (raw-search (mapcan (function tokenize) words))))))

;;;; THE END ;;;;
