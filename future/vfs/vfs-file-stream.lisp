;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               vfs-file-stream.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines the file stream operators
;;;;    for the Virtual File System backend.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-01-14 <PJB> Extracted from 'virtual-fs.lisp'.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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


(in-package "COM.INFORMATIMAGO.COMMON-LISP.VIRTUAL-FILE-SYSTEM")


(defclass file-stream (stream)
  ((pathname   :accessor pathname 
               :initarg :pathname)
   (file       :accessor %file-stream-file
               :initarg :file)
   (contents   :accessor %file-stream-contents
               :initarg :contents)
   (position   :accessor %file-stream-position
               :initarg :position
               :initform 0
               :type    (integer 0))
   (override-p :accessor %override-p
               :initarg  :override-p)))


(defclass file-input-stream (file-stream)
  ())

(defclass file-output-stream (file-stream)
  ())

(defclass file-io-stream (file-input-stream  file-output-stream)
  ())


(defgeneric print-object-fields (self stream))
(defmethod print-object-fields ((self file-stream) stream)
  (call-next-method)
  (format stream " :PATHNAME ~S :POSITION ~A"
          (pathname self) (%file-stream-position self)))


(defmethod print-object ((self file-stream) stream)
  (print-unreadable-object (self stream :type nil :identity t)
    (format stream "~A" 'file-stream)
    (print-object-fields self stream))
  self)


(define-stream-methods file-stream
  (stream-external-format (%stream-external-format stream))
  (file-length   (length (contents (%file-stream-contents stream))))
  (file-string-length
   (etypecase object
     (character 1)
     (string    (length object))))
  (close
   (prog1 (%open-stream-p stream)
     (when (%open-stream-p stream)
       (setf (%open-stream-p stream) nil)
       (when (%override-p stream)
         (let* ((path (pathname stream))
                (dir   (directory-entry path)))
           (delete-file path)
           (add-entry dir (%file-stream-file stream))))))))





(defun open (filespec &key (direction :input)
                        (element-type 'character)
                        (if-exists nil)
                        (if-does-not-exist nil)
                        (external-format :default))
  
  (check-type direction (member :input :output :io :probe))
  (check-type if-exists (member :error :new-version :rename :rename-and-delete
                                :overwrite :append :supersede nil))
  (check-type if-does-not-exist (member :error :create nil))
  (check-type external-format (member :default))
  

  (let ((path (resolve-pathspec filespec)))
    (labels ((make-stream (file openp inputp outputp overridep position)
               (let* ((contents (newest file)))
                 (make-instance (if inputp
                                    (if outputp
                                        'file-io-stream
                                        'file-input-stream)
                                    (if outputp
                                        'file-output-stream
                                        'file-stream))
                     :open-p openp
                     :element-type (element-type contents)
                     :external-format :default ; ???
                     :input-p inputp
                     :output-p outputp
                     :override-p overridep
                     :pathname (truename path)
                     :file file
                     :contents contents
                     :position (ecase position
                                 (:start 0)
                                 (:end   (length (contents contents)))))))

             (new-file (path element-type)
               (create-file-at-path path :create-version-p t :element-type element-type))

             (new-file/unlinked (path element-type)
               (let ((entry (make-instance 'fs-file
                                :name (pathname-name path) :type (pathname-type path))))
                 (create-new-version entry :element-type element-type)
                 entry))

             (new-version (file element-type)
               (create-new-version file :element-type element-type))

             (copy-new-version (contents)
               (let ((new-contents (newest (create-new-version (file contents)
                                                               :element-type (element-type contents)))))
                 (setf (contents new-contents) (copy-array (contents contents)
                                                           :copy-fill-pointer t
                                                           :copy-adjustable t))
                 (file new-contents)))

             (rename-old (file &optional (index 0))
               (let ((old (make-pathname :type (format nil "~A-OLD-~3,'0D"
                                                       (pathname-type file)
                                                       (incf index))
                                         :defaults file)))
                 (if (file-entry old)
                     (rename-old file index)
                     (rename-file file old)))))
      
      (let ((contents (file-entry path)))
        
        ;; filespec  ¬N.T     ¬N.T.3<NEWEST ¬N.T.3>NEWEST  N.T.3=NEWEST  N.T.3<NEWEST
        ;;   N.T     no exist     newest        newest       newest         newest
        ;;   N.T.3   no exist     no exist      no exist     newest      old version
        ;;                     create newest  create newest
        (ecase direction
          
          ((:input :probe)
           (if contents
               (progn
                 ;; TODO: use CERROR to ignre the requested element-type
                 (assert (equal (element-type contents) element-type)
                         () "~A: the element-type requested ~S must be identical to the element-type ~S of the file ~S"
                         'open element-type (element-type contents)  path)
                 (make-stream (file contents) (eql direction :input) t nil  nil :start))
               (ecase if-does-not-exist
                 ((:error)
                  (error (make-condition
                          'simple-file-error
                          :pathname path
                          :format-control "~A: file ~S does not exist"
                          :format-arguments (list 'open path))))
                 ((:create)
                  (make-stream (new-file path element-type) (eql direction :input) t nil  nil :start))
                 ((nil)
                  (return-from open nil)))))


          ((:output :io)
           (if contents
               ;; file exists:
               (ecase if-exists
                 ((:error)
                  (error (make-condition
                          'simple-file-error
                          :pathname path
                          :format-control "~A: file ~S already exists"
                          :format-arguments (list 'open path))))
                 ((:new-version)
                  (make-stream (new-version (file contents) element-type)
                               t (eql direction :io) t  nil :start))
                 ((:rename)
                  (rename-old path)
                  (make-stream (new-file path element-type)
                               t (eql direction :io) t  nil :start))
                 ((:rename-and-delete)
                  (let ((old nil))
                    (unwind-protect
                         (progn
                           (setf old (rename-old path))
                           (make-stream (new-file path element-type)
                                        t (eql direction :io) t  nil :start))
                      (when old
                        (delete-file old)))))
                 ((:overwrite)
                  (make-stream (if (eql contents (newest (file contents)))
                                   (file contents)
                                   (copy-new-version contents))
                               t (eql direction :io) t nil :start))
                 ((:append)
                  (make-stream (if (eql contents (newest (file contents)))
                                   (file contents)
                                   (copy-new-version contents))
                               t (eql direction :io) t nil :end))
                 ((:supersede)
                  (make-stream (new-file/unlinked path element-type)
                               t (eql direction :io) t  nil :start))
                 ((nil)
                  (return-from open nil)))
               ;; file does not exist:
               (ecase if-does-not-exist
                 ((:error)
                  (error (make-condition
                          'simple-file-error
                          :pathname path
                          :format-control "~A: file ~S does not exist"
                          :format-arguments (list 'open path))))
                 ((:create)
                  (make-stream (new-file path element-type) t (eql direction :io) t  t :start))
                 ((nil)
                  (return-from open nil))))))))))





(defun  !read-element (stream eof-error-p eof-value)
  (with-accessors ((contents %file-stream-contents)
                   (position %file-stream-position)) stream
    (if (< position (length (contents contents)))
        (aref (contents contents) (prog1 position (incf position)))
        (if eof-error-p
            (error 'end-of-file :stream stream)
            eof-value))))


(defun !write-element (stream sequence start end)
  (with-accessors ((contents %file-stream-contents)
                   (position %file-stream-position)) stream
    (let* ((end          (or end (length sequence)))
           (size         (- end start))
           (new-position (+ position size))
           (data         (contents contents)))
      (if (< new-position *maximum-file-size*)
          (progn
            (unless (< new-position (array-dimension data 0))
              (setf data (adjust-array data
                                       (max new-position (* 2 (array-dimension data 0)))
                                       :element-type (array-element-type data)
                                       :initial-element (if (subtypep (array-element-type data)
                                                                      'character)
                                                            #\space
                                                            0)
                                       :fill-pointer (fill-pointer data))
                    (contents contents) data))
            (when (< (fill-pointer data) new-position)
              (setf (fill-pointer data) new-position))
            (replace data sequence :start1 position :start2 start :end2 end)
            (setf position new-position))
          (error 'simple-stream-error
                 :stream stream
                 :format-control "data too big to be written on stream ~S (~A+~A=~A>~A)"
                 :format-arguments (list stream
                                         position size new-position *maximum-file-size*))))))

(defun whitespacep (ch)
  (position  ch #(#\Space #\Newline #\Tab #\Linefeed #\Page #\Return)))


(define-stream-methods file-input-stream
  (file-position
   (call-next-method)
   (with-accessors ((contents %file-stream-contents)
                    (position %file-stream-position)) stream
     (if position-spec
         (if (< -1 position-spec (length (contents contents)))
             (setf position position-spec)
             (error 'simple-stream-error
                    :stream stream
                    :format-control "~A: invalid position ~A on stream ~S"
                    :format-arguments (list 'file-position position-spec stream))))
     position))

  (read-byte         (!read-element       stream eof-error-p eof-value))
  (read-char         (!read-element input-stream eof-error-p eof-value))
  (read-char-no-hang (!read-element input-stream eof-error-p eof-value))
  (peek-char
   (flet ((eof ()
            (if eof-error-p
                (error 'end-of-file :stream stream)
                eof-value)))
     (with-accessors ((contents %file-stream-contents)
                      (position %file-stream-position)) stream
       (case peek-type
         ((nil))
         ((t)
          (setf position (or (position-if-not (function whitespacep)
                                              (contents contents)
                                              :start position)
                             (length (contents contents)))))
         (otherwise
          (setf position (or (position peek-type
                                       (contents contents)
                                       :start position)
                             (length (contents contents))))))
       (if (< position (length (contents contents)))
           (aref (contents contents) position)
           (eof)))))
  (unread-char
   (with-accessors ((contents %file-stream-contents)
                    (position %file-stream-position)) input-stream
     (when (plusp position)
       (decf position))))
  (read-line
   (with-accessors ((contents %file-stream-contents)
                    (position %file-stream-position)) input-stream
     (let ((start position)
           (end   (or (position #\newline (contents contents) :start position)
                      (length (contents contents)))))
       (prog1 (subseq (contents contents) start (if (< end (length (contents contents)))
                                                    (1- end)
                                                    end))
         (setf position end)))))
  (read-sequence
   (with-accessors ((contents %file-stream-contents)
                    (position %file-stream-position)) stream
     (let ((start position)
           (end   (or (position #\newline (contents contents) :start position)
                      (length (contents contents)))))
       (prog1 (subseq (contents contents) start (if (< end (length (contents contents)))
                                                    (1- end)
                                                    end))
         (setf position end)))))
  (listen
   (with-accessors ((contents %file-stream-contents)
                    (position %file-stream-position)) input-stream
     (< position (length (contents contents)))))
  (clear-input
   #|nothing|#
   nil))


(defvar *newline* (string #\newline))

(define-stream-methods file-output-stream
  (file-position
   (call-next-method)
   (with-accessors ((contents %file-stream-contents)
                    (position %file-stream-position)) stream
     (if position-spec
         (if (< -1 position-spec *maximum-file-size*)
             (setf position position-spec)    
             (error 'simple-stream-error
                    :stream stream
                    :format-control "~A: invalid position ~A on stream ~S"
                    :format-arguments (list 'file-position position-spec stream))))
     position))
  
  (write-byte     (!write-element stream        (vector byte)      0 1)
                  byte)
  (write-char     (!write-element output-stream (string character) 0 1)
                  character)
  (terpri         (!write-element output-stream *newline*          0 (length *newline*))
                  nil)
  (fresh-line     (with-accessors ((contents %file-stream-contents)
                                   (position %file-stream-position)) stream
                    (unless (and (plusp position)
                                 (char= #\newline (aref (contents contents) (1- position))))
                      (!write-element output-stream *newline*  0 (length *newline*))
                      #\newline)))
  (write-string   (!write-element output-stream string start end)
                  string)
  (write-line     (!write-element output-stream string start end)
                  (!write-element output-stream *newline* 0 (length *newline*))
                  string)
  (write-sequence (!write-element stream sequence start end)
                  sequence)
  (clear-output   #|nothing|# nil)
  (force-output   #|nothing|# nil)
  (finish-output  #|nothing|# nil))


;;;; THE END ;;;;

