;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               filenames.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This files implements CL filenames: pathnames, logical-pathnames,
;;;;    translations.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-01-14 <PJB> Extracted from 'virtual-fs.lisp'.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 19. Filenames
;;; http://www.lispworks.com/documentation/HyperSpec/Body/19_.htm

(define-condition simple-file-error (file-error simple-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~?"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))))



(defparameter *logical-pathname-regexp*
  (let ((host "(([-A-Z0-9]+):)?")
        (dire "(;)?(([-*A-Z0-9]+;|\\*\\*;)*)")
        (name "([-*A-Z0-9]+)?")
        (type "(.([-*A-Z0-9]+)(.([0-9]+|newest|NEWEST|\\*))?)?"))
    (re-compile (concatenate 'string "^" host dire name type "$")
                :extended t)))


(defun parse-logical-pathname (string &key (start 0) (end nil))
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
    (destructuring-bind (all
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
          (error "Syntax error parsing pathname ~S" string)))))

(defun concat* (type list)
  (let* ((totlen  (reduce (lambda (length item) (+ (length item) length))
                          list :initial-value 0))
         (result  (cond
                    ((or (eq type 'string)
                         (and (consp type) (eq 'string (first type))))
                     (make-string totlen))
                    ((or (eq type 'vector)
                         (and (consp type) (eq 'vector (first type)))
                         (eq type 'array)
                         (and (consp type) (eq 'array (first type))))
                     (make-array totlen))
                    ((eq type 'list)
                     (make-list totlen))
                    (t (error "Invalid sequence type: ~S" type)))))
    (loop
      :for item :in list
      :and start = 0 :then (+ start (length item))
      :do (replace result item :start1 start)
      :finally (return result))))

(defun match-wild-word-p (item wild)
  (re-match
   (concat* 'string
            (cons "^"
                  (nconc
                   (loop
                     :for chunks :on (split-sequence #\* wild)
                     :collect (car chunks) :when (cdr chunks) :collect ".*")
                   (list "$"))))
   item))


;;;---------------------------------------------------------------------
;;; PATHNAME
;;;---------------------------------------------------------------------
;;;
;;; Note: we implement pathnames just as CL logical pathnames.
;;; pathnames are 'physical pathnames' in our virtual file system.
;;; logical pathnames go thru logical pathname translations (have a
;;; logical host instead of a 'physical' file system host).
;;;


(defclass pathname ()
  ((host      :accessor %pathname-host
              :initarg :host
              :initform nil)
   (device    :accessor %pathname-device
              :initarg :device
              :initform :unspecific)
   (directory :accessor %pathname-directory
              :initarg :directory
              :initform nil)
   (name      :accessor %pathname-name
              :initarg :name
              :initform nil)
   (type      :accessor %pathname-type
              :initarg :type
              :initform nil)
   (version   :accessor %pathname-version
              :initarg :version
              :initform nil))
  (:documentation "A physical pathname."))

(defmethod print-object ((self pathname) stream)
  (format stream "~:[~;#P\"~]~A~0@*~:[~;\"~]" *print-escape* (namestring self))
  self)


(defun install-pathname-reader-macro (&optional (readtable *readtable*))
  (set-dispatch-macro-character #\# #\p
                                (lambda (stream disp char)
                                  (declare (ignore disp char))
                                  (pathname (read stream t nil t)))
                                readtable))


(defun reset-readtable ()
  (setq *readtable* (copy-readtable nil)))




#+emacs (put 'define-pathname-attribute 'lisp-indent-function 1)
(defmacro define-pathname-attribute (name &optional docstring)
  `(defun ,(intern (format nil "PATHNAME-~A" name)) (pathname &key (case :local))
     (declare (ignore case))
     ,@(when docstring (list docstring))
     (,(intern (format nil "%PATHNAME-~A" name)) (pathname pathname))))

(define-pathname-attribute host
  "Pathname Host Component: The name of the file system on which the
file resides, or the name of a logical host.")

(define-pathname-attribute device
  "Pathname Device Component: Corresponds to the ``device'' or ``file
structure'' concept in many host file systems: the name of a logical
or physical device containing files.")

(define-pathname-attribute directory
  "Pathname Directory Component: Corresponds to the ``directory'' concept
in many host file systems: the name of a group of related files. ")

(define-pathname-attribute name
  "Pathname Name Component: The ``name'' part of a group of files that
can be thought of as conceptually related.")

(define-pathname-attribute type
  "Pathname Type Component: Corresponds to the ``filetype'' or
``extension'' concept in many host file systems. This says what kind
of file this is. This component is always a string, nil, :wild, or
:unspecific.")

(define-pathname-attribute version
  "Pathname Version Component: Corresponds to the ``version number''
concept in many host file systems.  The version is either a positive
integer or a symbol from the following list: nil, :wild, :unspecific,
or :newest (refers to the largest version number that already exists
in the file system when reading a file, or to a version number greater
than any already existing in the file system when writing a new
file). Implementations can define other special version symbols.")



(defun dump-pathname (path)
  (format t "~&~A~%~
             ~{~&HOST      = ~S~
               ~&DEVICE    = ~S~
               ~&DIRECTORY = ~S~
               ~&NAME      = ~S~
               ~&TYPE      = ~S~
               ~&VERSION   = ~S~
               ~&~}"
          (class-name (class-of path))
          (mapcar (lambda (f) (funcall f path :case :common))
                  (list (function pathname-host)
                        (function pathname-device)
                        (function pathname-directory)
                        (function pathname-name)
                        (function pathname-type)
                        (function pathname-version)))))




(defclass logical-pathname (pathname)
  ()
  (:documentation "A logical pathname."))

;; Notice only the class changes from a physical PATHNAME to a LOGICAL-PATHNAME.



(defun pathnamep (object) (typep object 'pathname))



(defgeneric pathname (pathspect))


(defmethod pathname ((pathspec t))
  (assert-type pathspec '(or string file-stream pathname)))


(defmethod pathname ((pathspec pathname))
  (call-next-method)
  pathspec)


(defmethod pathname ((pathspec string))
  (call-next-method)
  (destructuring-bind (host relative directory name type version)
      (parse-logical-pathname pathspec)
    ;; (print (list host relative directory name type version))
    (make-instance (cond
                     ((eql :wild host)       'pathname)
                     ((logical-host-p host)  'logical-pathname)
                     (t                      'pathname))
                   :host host :directory (cons relative directory)
                   :name name :type type :version version)))




(defun present-item (item)
  (cond ((null item) item)
        ((listp item) (second item))
        ((eq :wild item) "*")
        ((eq :wild-inferiors item) "**")
        (t item)))


(defun namestring (pathname)
  (let ((pathname (pathname pathname)))
    (format nil "~A:~:[~;;~]~{~A;~}~:[~;~:*~A~]~
                    ~:[~;.~:*~A~:[~;.~:*~A~]~]"
            (pathname-host pathname)
            (eq :relative (first (pathname-directory pathname)))
            (mapcar (function present-item) (rest (pathname-directory pathname)))
            (present-item (pathname-name pathname))
            (present-item (pathname-type pathname))
            (present-item (pathname-version pathname)))))


(defun file-namestring (pathname)
  (let ((pathname (pathname pathname)))
    (format nil "~:[~;~:*~A~]~:[~;.~:*~A~:[~;.~:*~A~]~]"
            (present-item (pathname-name pathname))
            (present-item (pathname-type pathname))
            (present-item (pathname-version pathname)))))


(defun directory-namestring (pathname)
  (let ((pathname (pathname pathname)))
    (format nil "~:[~;;~]~{~A;~}"
            (eq :relative (first (pathname-directory pathname)))
            (mapcar (function present-item) (rest (pathname-directory pathname))))))


(defun host-namestring (pathname)
  (let ((pathname (pathname pathname)))
    (format nil "~@[~A~]" (pathname-host pathname))))


(defun enough-namestring (pathname &optional defaults)
  (declare (ignore pathname defaults))
  (error "enough-namestring not implemented yet"))





(defun check-host (host)
  (cond
    ((null host)              (name *default-file-system*))
    ((eql :wild host)         host)
    ((file-system-named host) host)
    (t                        (error "Invalid host ~S" host))))


(defun make-pathname (&key host device directory name type version (case :local)
                        (defaults nil defaults-p))
  (declare (ignore case))
  (cond ((stringp directory)  (setf directory (list :absolute directory)))
        ((eq :wild directory) (setf directory (list :absolute :wild-inferiors))))
  (let ((host (check-host (or host (if defaults-p
                                       (and defaults (pathname-host      defaults))
                                       (pathname-host *default-pathname-defaults*))))))
    (make-instance (cond
                     ((eql :wild host)       'pathname)
                     ((logical-host-p host)  'logical-pathname)
                     (t                      'pathname))
                   :host        host
                   :device      (or device    (and defaults (pathname-device    defaults)))
                   :directory   (or directory (and defaults (pathname-directory defaults)))
                   :name        (or name      (and defaults (pathname-name      defaults)))
                   :type        (or type      (and defaults (pathname-type      defaults)))
                   :version     (or version   (and defaults (pathname-version   defaults))))))





(defparameter *logical-pathname-translations*
  (make-hash-table :test (function equal)))


(defun logical-host-p (host)
  "
RETURN: whether HOST is a logical hosts.
"
  (nth-value 1 (gethash host *logical-pathname-translations*)))


(defun logical-pathname-translations (host)
  "
RETURN: The logical pathname translations for the HOST.
"
  (assert-type host 'string)
  (gethash host *logical-pathname-translations*))


(defun (setf logical-pathname-translations) (value host)
  (assert-type host 'string)
  (assert (and (proper-list-p value)
               (every (lambda (item)
                        (and (proper-list-p item)
                             (typep (first  item) '(or string logical-pathname))
                             (typep (second item) '(or string pathname))))
                      value)))
  (setf (gethash host  *logical-pathname-translations*) value))


(defun load-logical-pathname-translations (host)
  (assert-type host 'string)
  (if (nth-value 1 (logical-pathname-translations host))
      nil
      (with-open-file (input (make-pathname :host "SYS"
                                            :directory '(:absolute "SITE")
                                            :name host
                                            :type "TRANSLATIONS"
                                            :version :newest)
                        :if-does-not-exist nil)
        (if input
            (setf (logical-pathname-translations host) (read input nil nil))
            (error "No logical pathname translation file found for host ~S"
                   host)))))


(defun logical-pathname (pathspec)
  (let ((path (pathname pathspec)))
    (if (logical-pathname-p path)
        path
        (error "~S: pathspec ~S is not a logical pathname."
               'logical-pathname pathspec))))


(defun parse-namestring (thing &optional host
                                 (default-pathname *default-pathname-defaults*)
                         &key (start 0) (end nil) (junk-allowed nil))
  (let ((default-host (and host (check-host host))))
    (etypecase thing
      (file-stream
       (parse-namestring  (pathname thing) default-host default-pathname
                          :start start :end end :junk-allowed junk-allowed))
      (pathname
       (if (equal (pathname-host thing :case :common) default-host)
           (values thing start)
           (error 'simple-type-error
                  :format-control "~S: pathname has a different host ~S than given host ~S"
                  :format-arguments (list 'parse-namestring
                                          (pathname-host thing :case :common)
                                          default-host))))
      (string
       (if (string= thing "" :start1 start :end1 end)
           (values (make-instance 'pathname :host nil :directory nil :name nil :type nil :version nil)
                   start)
           ;; TODO: implement junk-allowed
           (let ((result (ignore-errors (parse-logical-pathname thing :start start :end end))))
             (if result
                 (destructuring-bind (host relative directory name type version) result
                   (when (and host default-host)
                     (unless (equal host default-hosts)
                       (error 'simple-type-error
                              :format-control "~S: pathname has a different host ~S than given host ~S"
                              :format-arguments (list 'parse-namestring host default-host))))
                   (let ((host (or host default-hosts (pathname-host default-pathname :case :common))))
                     (values
                      (make-instance (cond
                                       ((eql :wild host)       'pathname)
                                       ((logical-host-p host)  'logical-pathname)
                                       (t                      'pathname))
                                     :host host :directory (cons relative directory)
                                     :name name :type type :version version)
                      (or end (length thing)))))
                 (values nil start))))))))


(defun wild-pathname-p (pathname &optional field-key)
  (assert-type pathname '(or pathname string file-stream))
  (let ((pathname (pathname pathname)))
    (flet ((wild-p (item)
             (or (eq item :wild)
                 (eq item :wild-inferiors)
                 (and (consp item)
                      (eq (first item) :wild-word)))))
      (if (null field-key)
          (or (wild-pathname-p pathname :host)
              (wild-pathname-p pathname :device)
              (wild-pathname-p pathname :directory)
              (wild-pathname-p pathname :name)
              (wild-pathname-p pathname :type)
              (wild-pathname-p pathname :version))
          (ecase field-key
            (:host    (wild-p (pathname-host    pathname)))
            (:device  (wild-p (pathname-device  pathname)))
            (:directory (some (function wild-p)
                              (cdr (pathname-directory pathname))))
            (:name    (wild-p (pathname-name    pathname)))
            (:type    (wild-p (pathname-type    pathname)))
            (:version (wild-p (pathname-version pathname))))))))






(defun match-item-p (item wild &optional match-wild-word-p)
  (or (eq wild :wild)
      (and (consp wild) (eq (first wild) :wild-word)
           match-wild-word-p (match-wild-word-p item (second wild)))
      (eq item wild)
      (and (stringp item) (stringp wild) (string= item wild))))

(defun match-directory-items-p (item wild)
  (or (null item)
      (null wild)
      (if (eq (first wild) :wild-inferiors)
          (loop
            :for rest :on item
              :thereis (match-directory-items-p rest (rest wild)))
          (and (match-item-p (first item) (first wild) t)
               (match-directory-items-p (rest item) (rest wild))))))


(defun pathname-match-p (pathname wildcard)
  (assert-type pathname '(or pathname string file-stream))
  (assert-type wildcard '(or pathname string file-stream))
  (let* ((pathname (pathname pathname))
         (wildcard (merge-pathnames (pathname wildcard)
                                    (load-time-value (make-pathname
                                                      :host :wild
                                                      :device :wild
                                                      :directory :wild
                                                      :name :wild
                                                      :type :wild
                                                      :version :wild)))))
    (and (match-item-p (pathname-host    item) (pathname-host    wild) t)
         (match-item-p (pathname-device  item) (pathname-device  wild) t)
         (match-item-p (pathname-name    item) (pathname-name    wild) t)
         (match-item-p (pathname-type    item) (pathname-type    wild) t)
         (match-item-p (pathname-version item) (pathname-version wild) nil)
         (or (and (eq :absolute (first (pathname-directory wild)))
                  (eq :relative (first (pathname-directory item)))
                  (eq :wild-inferiors (second  (pathname-directory wild))))
             (and (eq (first (pathname-directory wild))
                      (first (pathname-directory item)))
                  (match-directory-items-p (rest (pathname-directory item))
                                           (rest (pathname-directory wild))))))))



(defun translate-logical-pathname (pathname &key)
  (warn "translate-logical-pathname not implemented yet")
  (pathname pathname))


(defun pathname-components (pathname)
  (list (pathname-host      pathname)
        (pathname-device    pathname)
        (pathname-directory pathname)
        (pathname-name      pathname)
        (pathname-type      pathname)
        (pathname-version   pathname)))

(defun translate-pathname (source from-wildcard to-wildcard &key)
  (assert-type source        '(or string pathname file-stream))
  (assert-type from-wildcard '(or string pathname file-stream))
  (assert-type to-wildcard   '(or string pathname file-stream))
  (error "NOT IMPLEMENTED YET")
  #-(and)
  (let ((source        (pathname-components (pathname source)))
        (from-wildcard (pathname-components (pathname from-wildcard)))
        (to-wildcard   (pathname-components (pathname to-wildcard))))
    (loop
      :for dirp    :in '(nil nil t nil nil nil)
      :for s-compo :in source
      :for f-compo :in from-wildcard
      :for t-compo :in to-wildcard
      :collect :to-be-done)))



(defun join (sep strlist)
  (if strlist
      (cl:with-output-to-string (out)
        (cl:princ (first strlist) out)
        (dolist (str (rest strlist))
          (cl:princ sep out)
          (cl:princ str out)))
      ""))

(defun test ()
  (let* (;; (source "CRACKBOOMHUH")
         (source "FOOZIMBAR")
         (from      (split-sequence #\* "FOO*BAR"))
         (to        (split-sequence #\* "Z(O)OM*ZOOM"))
         (from-re   (join "(.*)" (mapcar (lambda (item) (re-quote item :extended t)) from)))
         (matches   (re-match from-re source)))
    (assert (= (length  from) (length to)))
    (if matches
        (cl:with-output-to-string (out)
          (pop matches)
          (cl:princ (first to) out)
          (dolist (item (rest to))
            (let ((range (pop matches)))
              (cl:princ (subseq source (first range) (second range)) out))
            (cl:princ item out)))
        source)))




(defun delete-back (dir)
  (loop
    :with changed := t
    :while changed
    :do (loop
          :for cur := dir :then (cdr cur)
            :initially (setf changed nil)
          :do (when (and (or (stringp (cadr cur)) (eq :wild (cadr cur)))
                         (eq :back (caddr cur)))
                (setf (cdr cur) (cdddr cur)
                      changed t))))
  dir)


(defun merge-pathnames (pathname
                        &optional (default-pathname *default-pathname-defaults*)
                          (default-version :newest))
  (let ((pathname (pathname pathname)))
    (make-pathname
     :host    (or (pathname-host pathname) (pathname-host default-pathname))
     :device  (if (and (stringp (pathname-host pathname))
                       (stringp (pathname-host default-pathname))
                       (member (pathname-device pathname) '(:unspecific nil))
                       (string= (pathname-host pathname)
                                (pathname-host default-pathname)))
                  (pathname-device default-pathname)
                  (or (pathname-device pathname) :unspecific))
     :directory (if (eq :relative (car (pathname-directory pathname)))
                    (delete-back
                     (append (pathname-directory default-pathname)
                             (copy-list (cdr (pathname-directory pathname)))))
                    (or (pathname-directory pathname)
                        (pathname-directory default-pathname)))
     :name    (or (pathname-name pathname) (pathname-name default-pathname))
     :type    (or (pathname-type pathname) (pathname-type default-pathname))
     :version (cond ((pathname-name pathname)
                     (or (pathname-version pathname) default-version))
                    ((null default-version)
                     (pathname-version pathname))
                    (t
                     (or (pathname-version pathname)
                         (pathname-version default-pathname)))))))


;;;; THE END ;;;;
