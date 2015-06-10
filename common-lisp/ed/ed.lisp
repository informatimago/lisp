;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               ed.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    See defpackage documentation string.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-31 <PJB> Made a few corrections.
;;;;    2003-12-19 <PJB> Created.
;;;;BUGS
;;;;
;;;;    Not complete.
;;;;    (Still waiting on regexp package...).
;;;;
;;;;    In: 1,20!(do-something-with *input*)
;;;;    *input* is not bound to the 20 lines, and the output is not
;;;;    *inserted in the buffer.
;;;; 
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

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.ED.ED"
  (:use "COMMON-LISP")
  #+mocl (:shadowing-import-from "COM.INFORMATIMAGO.MOCL.KLUDGES.MISSING"
                                 "*TRACE-OUTPUT*"
                                 "*LOAD-VERBOSE*"
                                 "*LOAD-PRINT*"
                                 "ARRAY-DISPLACEMENT"
                                 "CHANGE-CLASS"
                                 "COMPILE"
                                 "COMPLEX"
                                 "ENSURE-DIRECTORIES-EXIST"
                                 "FILE-WRITE-DATE"
                                 "INVOKE-DEBUGGER" "*DEBUGGER-HOOK*"
                                 "LOAD"
                                 "LOGICAL-PATHNAME-TRANSLATIONS"
                                 "MACHINE-INSTANCE"
                                 "MACHINE-VERSION"
                                 "NSET-DIFFERENCE"
                                 "RENAME-FILE"
                                 "SUBSTITUTE-IF"
                                 "TRANSLATE-LOGICAL-PATHNAME"
                                 "PRINT-NOT-READABLE"
                                 "PRINT-NOT-READABLE-OBJECT")
  (:shadow "ED")
  (:export "ED")
  (:documentation
   "
This package exports an implementation of the COMMON-LISP ED function
following the user manual of ed(1).


ed(1) in COMMON-LISP.

Real men do it with magnets.  ed is for girly men.

This is a clone of the unix ed(1) editor written in pure Common-Lisp.
Since Common-Lisp does not define any process management functions,
all !command forms are taken as Lisp forms instead of sh(1) commands.
These forms are executed within a (LAMBDA (*INPUT*) command) with
the *INPUT* argument bound to a mutable list of inmutable input strings,
one per line.  The result of the returning form in command must be
a list of string, the list of resulting lines to be inserted into the
buffer or to be printed on the terminal.

For the rest, the aim is to be 100% user-interface compatible with ed(1).

Ed, man! !man ed: <http://www.gnu.org/fun/jokes/ed.msg.html>


Can you imagine that some Common-Lisp implementations DON'T provide
any editor (in conformity with the Common-Lisp specifications)?
Not complete (waiting for a REGEXP package). But otherwise
functional enough. 



License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2003 - 2015

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
(in-package "COM.INFORMATIMAGO.COMMON-LISP.ED.ED")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +debug+       t)
  (defparameter *show-debug* nil))


(defmacro dbg (&body body)
  (when +debug+ 
    `(when *show-debug* (let ((*standard-output* *trace-output*)) ,@body))))

;;(WHEN +DEBUG+
;;(shadow 'handler-case)
;;(defmacro handler-case (form &rest args) form))



;;--------
;; buffer:
;;--------

(defstruct buffer
  (path          nil)
  (lines         '(0)) ; (index-of-current-line . lines)
  (marks         '( )) ; ((ch . linum) ...)
  (old-lines     '(0)) ; (index-of-current-line . lines)
  (old-marks     '( )) ; ((ch . linum) ...)
  (cut-lines     '( )) ; (lines ...)
  (show-errors   nil)
  (got-error     nil)
  (last-error    "")
  (show-prompt   nil)
  (prompt-string "")
  (command       nil)
  (modified      nil)
  (print         nil)
  (quit          nil))

;; (setq b (buffer-read "~/test.txt"))

(defmacro toggle (place &environment env)
  (multiple-value-bind (vars vals stores setter getter) (get-setf-expansion place env)
    `(let* (,@(mapcar (function list) vars vals)
            (,(car stores) ,getter))
       (prog1
           (setq ,(car stores) (not ,(car stores)))
         ,setter))))


;;-------
;; lines:
;;-------

(defun (setf buffer-current-linum) (new-linum buffer) (setf (car (buffer-lines buffer)) new-linum))
(defun buffer-current-linum  (buffer) (car (buffer-lines buffer)))
(defun buffer-length         (buffer) (length (cdr (buffer-lines buffer))))
(defun buffer-nth-line (buffer linum) (car (buffer-line-cons buffer linum)))

(defun buffer-line-cons (buffer linum)
  "Return the cons cells where the line LINUM of the BUFFER is stored."
  (if (< linum 0)
      nil
      (nthcdr linum (buffer-lines buffer))))

;;-------
;; marks:
;;-------

(defun buffer-set-mark (buffer ch linum)
  (let ((ass (assoc ch (buffer-marks buffer))))
    (if ass
        (setf (cdr ass) linum)
        (push (cons ch linum) (buffer-marks buffer))))
  (values))


(defun buffer-get-mark (buffer ch)
  (cdr (assoc ch (buffer-marks buffer))))


(defun buffer-offset-marks (buffer from offset)
  (if (< offset 0)
      (let ((mindel (+ from offset)))
        (setf (buffer-marks buffer)
              (mapcan (lambda (ass)
                        (cond
                          ((< (cdr ass) mindel) (list ass))
                          ((< (cdr ass) from)   nil)
                          (t  (incf (cdr ass) offset) (list ass))))
                      (buffer-marks buffer))))
      (map nil (lambda (ass) (when (<= from (cdr ass)) (incf (cdr ass) offset)))
           (buffer-marks buffer))))


(defun copy-marks (marks)
  (mapcar (lambda (ass) (cons (car ass) (cdr ass))) marks))


;;----------------
;; error messages:
;;----------------

(defun buffer-clear-error (buffer)
  (setf (buffer-got-error buffer) nil))


(defun buffer-set-error (buffer message)
  (setf (buffer-last-error buffer) message
        (buffer-got-error  buffer) t))


;;-------------
;; undo buffer:
;;-------------

(defun buffer-save-undo (buffer)
  (setf (buffer-modified buffer) t)
  (setf (buffer-old-marks buffer) (copy-marks (buffer-marks buffer))
        (buffer-old-lines buffer) (copy-seq (buffer-lines buffer))))


(defun buffer-swap-undo (buffer)
  (psetf (buffer-old-marks buffer) (buffer-marks buffer)
         (buffer-marks buffer)     (buffer-old-marks buffer)
         (buffer-old-lines buffer) (buffer-lines buffer)
         (buffer-lines buffer)     (buffer-old-lines buffer)))


(defun buffer-erase (buffer)
  ;; no undo!
  (setf (buffer-lines     buffer) '(0)
        (buffer-marks     buffer) nil
        (buffer-old-lines buffer) '(0)
        (buffer-old-marks buffer) nil
        (buffer-cut-lines buffer) nil
        (buffer-modified  buffer) t))



(defun buffer-read (path)
  (let ((buffer (make-buffer :path path)))
    (do-read buffer path 0)
    buffer))


(defun buffer-from-string (text)
  (let ((buffer(make-buffer)))
    (do-paste buffer 0
              (do ((newline (format nil "~%"))
                   (lines ()) (position 0) (nextpos 0))
                  ((>= nextpos (length text)) (nreverse lines))
                (setq position (search newline text :start2 nextpos))
                (if position
                    (progn
                      (push (subseq text nextpos position) lines)
                      (setq nextpos (+ position (length newline))))
                    (progn
                      (push (subseq text nextpos) lines)
                      (setq nextpos (length text))))))
    buffer))



(defun limit (value min max)
  (if (<= min value max)
      value
      nil))


(defun address->linum (buffer address &optional (min 1))
  (cond
    ((null address) nil)
    ((eq address :curr)  (buffer-current-linum buffer))
    ((eq address :first) 1)
    ((eq address :last)  (buffer-length buffer))
    ((eq address :next)  (limit (1+ (buffer-current-linum buffer))
                                1 (buffer-length buffer)))
    ((eq address :prev)  (limit (1- (buffer-current-linum buffer))
                                min (buffer-length buffer)))
    ((not (consp address)) nil)
    ((eq (car address) :next) (limit (+ (buffer-current-linum buffer)
                                        (cdr address))
                                     1 (buffer-length buffer)))
    ((eq (car address) :prev) (limit (- (buffer-current-linum buffer)
                                        (cdr address))
                                     min (buffer-length buffer)))
    ((eq (car address) :linum) (limit (cdr address)
                                      min (buffer-length buffer)))
    ((eq (car address) :mark) (limit (buffer-get-mark buffer (cdr address))
                                     1 (buffer-length buffer)))
    ((eq (car address) :regexp)
     ;; TODO: regexp not implemented yet.
     (format *terminal-io* "REGEXP NOT IMPLEMENTED YET.~%"))))


(defmacro with-addresses ((buffer . addresses) &body body)
  `(let ,(mapcar (lambda (vam)
                   `(,(first vam)
                      (address->linum ,buffer ,(second vam) ,(third vam))))
                 addresses)
     (if (or ,@(mapcar (lambda (vam) `(null ,(first vam))) addresses)
             ,@(when (<= 2 (length addresses))
                     `((< ,(first (second addresses))
                          ,(first (first  addresses))))))
         (buffer-set-error ,buffer "Invalid address")
         (progn ,@body))))


(defmacro unless-modified (buffer &body body)
  `(if (buffer-modified ,buffer)
       (progn
         (setf (buffer-modified ,buffer) nil)
         (buffer-set-error ,buffer "Warning: file modified"))
       (progn
         ,@body)))


;; (progn (PJB-CL+INDENT with-addresses 1)(PJB-CL+INDENT unless-modified 1))

(defun do-insert (buffer linum)
  (dbg (format t "DO-INSERT(~S ~S) ~%" "buffer" linum))
  (do ((place (buffer-line-cons buffer linum) (cdr place))
       (line  (read-line *terminal-io* nil ".")
              (read-line *terminal-io* nil "."))
       (curr  linum (1+ curr)))
      ((string= line ".")
       (progn
         (setf (buffer-current-linum buffer) curr)
         (buffer-offset-marks buffer linum (- curr linum))))
    (setf (cdr place) (cons line (cdr place)))))


(defun do-paste (buffer linum new-lines)
  (dbg (format t "DO-PASTE(~S ~S [~D lines]) ~%"
               "buffer" linum (length new-lines)))
  (let* ((insert-point (buffer-line-cons buffer linum))
         (after        (cdr insert-point)))
    (setf (buffer-current-linum buffer) (+ linum (length new-lines)))
    (setf (cdr insert-point) new-lines)
    (setf (cdr (last new-lines)) after)))


(defun do-cut (buffer from last)
  "
RETURN: The list of cut lines.
"
  (dbg (format t "DO-CUT(~S ~S ~S) ~%" "buffer" from last))
  (let* ((delete-point (buffer-line-cons buffer (1- from)))
         (after-point  (buffer-line-cons buffer  last))
         (result       (cdr delete-point)))
    (setf (buffer-current-linum buffer)
          (if (null (cdr after-point)) from (1+ from)))
    (setf (cdr delete-point) (cdr after-point)
          (cdr after-point) nil)
    (buffer-offset-marks buffer from (- last from -1))
    result))


(defun do-print-lines (buffer curr last func)
  (dbg (format t " DO-PRINT-LINES(~S ~S ~S [a function]) ~%" "buffer" curr last))
  (setf (buffer-print buffer) nil)
  (do* ((curr curr (1+ curr))
        (lines (buffer-line-cons buffer curr) (cdr lines)))
       ((> curr last)
        (setf (buffer-current-linum buffer) last))
    (funcall func curr (car lines))))


(defun do-write (buffer path mode from last)
  (dbg (format t "DO-WRITE(~S ~S ~S ~S ~S) ~%" "buffer" path mode from last))
  (handler-case
      (with-open-file (output path :direction :output :if-exists mode
                              :if-does-not-exist :create)
        (do ((line (buffer-line-cons buffer from) (cdr line))
             (curr from (1+ curr)))
            ((> curr last))
          (format output "~A~%" (car line))))
    (error (err) (buffer-set-error buffer (format nil "~S" err)))))


(defun do-read (buffer path linum)
  (dbg (format t "DO-READ(~S ~S ~S) ~%" "buffer" path linum))
  (handler-case
      (do-paste buffer linum
                (with-open-file (input path :direction :input
                                       :if-does-not-exist :error)
                  (do ((lines '() (cons line lines))
                       (line t))
                      ((null line) (nreverse (cdr lines)))
                    (setf line (read-line input nil nil)))))
    (error (err) (buffer-set-error buffer (format nil "~S" err)))))


(defun filter-command-output (output)
  (if (and (listp output) (every (function stringp) output))
      output
      (list (format nil "~S" output))))


;; (defmacro hc (form &rest rest) form)

(defun do-command (buffer &key (input nil) (output :print))
  (dbg (format t "DO-COMMAND(~S ~S ~S) ~%" "buffer" input output))
  (if (buffer-command buffer)
      (handler-case
          (progn
            (format *terminal-io* "~A~%" (buffer-command buffer))
            (let* ((*package* (find-package "COM.INFORMATIMAGO.COMMON-LISP.ED.ED"))
                   (results   (eval `((lambda (*input*)
                                        (declare (ignorable *input*))
                                        ,(read-from-string 
                                          (format nil "(progn  ~A)"
                                                  (buffer-command buffer))))
                                      (list ,@input)))))
              (case output
                ((:print)
                 (map nil (lambda (line) (format *terminal-io* "~A~%" line))
                      (filter-command-output results))
                 (format *terminal-io* "!~%")
                 (values))
                ((:result)
                 (format *terminal-io* "!~%")
                 results)
                (otherwise
                 (buffer-set-error buffer "Internal error: DO-COMMAND")
                 (values)))))
        (error (err)
          (buffer-set-error buffer (format nil "~S" err))
          (values)))
      (progn
        (buffer-set-error buffer "No previous command")
        (values))))


(defun cmd-comment (buffer from to arg)
  (declare (ignore from to arg))
  (dbg (format t "CMD-COMMENT: ~%"))
  ;; (.,.)#  Begins a comment;  the rest of the line, up to a newline,
  ;;         is ignored.  If a line address followed by a semicolon is
  ;;         given,  then  the current address is set to that address.
  ;;         Otherwise, the current address is unchanged.
  (setf (buffer-print buffer) nil))


(defun cmd-append (buffer from to arg)
  (declare (ignore from arg))
  (dbg (format t "CMD-APPEND: ~%"))
  ;; (.)a    Appends text to the  buffer  after  the  addressed  line,
  ;;         which  may  be  the address 0 (zero).  Text is entered in
  ;;         input mode.  The current address  is  set  to  last  line
  ;;         entered.
  (with-addresses (buffer (linum to 0))
    (buffer-save-undo buffer)
    (do-insert buffer linum)))


(defun cmd-insert (buffer from to arg)
  (declare (ignore from arg))
  (dbg (format t "CMD-INSERT: ~%"))
  ;; (.)i    Inserts text in the buffer before the current line.  Text
  ;;         is entered in input mode.  The current address is set  to
  ;;         the last line entered.
  (with-addresses (buffer (linum to 1))
    (buffer-save-undo buffer)
    (do-insert buffer (max 0 (1- linum)))))


(defun cmd-delete-lines (buffer from to arg)
  (declare (ignore arg))
  (dbg (format t "CMD-DELETE-LINES: ~%"))
  ;; (.,.)d  Deletes the addressed lines from the buffer.  If there is
  ;;         a  line after the deleted range, then the current address
  ;;         is set to this line. Otherwise the current address is set
  ;;         to the line before the deleted range.
  (with-addresses (buffer (curr from 1) (last to 1))
    (buffer-save-undo buffer)
    (setf (buffer-cut-lines buffer) (do-cut buffer curr last))))

                  
(defun cmd-copy (buffer from to arg)
  (declare (ignore arg))
  (dbg (format t "CMD-COPY: ~%"))
  ;; (.,.)y  Copies (yanks) the addressed lines  to  the  cut  buffer.
  ;;         The  cut  buffer  is  overwritten by subsequent `y', `s',
  ;;         `j', `d',  or  `c'  commands.   The  current  address  is
  ;;         unchanged.
  ;; no undo
  (with-addresses (buffer (curr from 1) (last to 1))
    ;; We don't need to make copies of the line string
    ;; because they're considered immutable.
    ;; When edited (changed, substituted),
    ;; a new copy replaces the old one.
    (setf (buffer-cut-lines buffer)
          (subseq (buffer-lines buffer) curr (1+ last)))))

 
(defun cmd-paste (buffer from to arg)
  (declare (ignore from arg))
  (dbg (format t "CMD-PASTE: ~%"))
  ;; (.)x    Copies (puts) the contents of the cut buffer to after the
  ;;         addressed  line.   The current address is set to the last
  ;;         line copied.
  (with-addresses (buffer (curr to 0))
    (buffer-save-undo buffer)
    (do-paste buffer curr (copy-seq (buffer-cut-lines buffer)))))


(defun cmd-undo (buffer from to arg)
  (declare (ignore from to arg))
  (dbg (format t "CMD-UNDO: ~%"))
  ;; u       Undoes the last command and restores the current  address
  ;;         to  what  it was before the command.  The global commands
  ;;         `g', `G', `v', and `V'.  are treated as a single  command
  ;;         by undo.  `u' is its own inverse.
  (buffer-swap-undo buffer))



(defun cmd-copy-lines (buffer from to arg)
  (dbg (format t "CMD-COPY-LINES: ~%"))
  ;; (.,.)t(.)
  ;;         Copies (i.e., transfers) the addressed lines to after the
  ;;         right-hand destination address, which may be the  address
  ;;         0  (zero).   The  current address is set to the last line
  ;;         copied.
  (with-addresses (buffer (curr from 1) (last to 1) (target arg 0))
    (buffer-save-undo buffer)
    (do-paste buffer arg (subseq (buffer-lines buffer) curr (1+ last)))))


(defun cmd-move-lines (buffer from to arg)
  (dbg (format t "CMD-MOVE-LINES: ~%"))
  ;; (.,.)m(.)
  ;;         Moves lines in the buffer.  The addressed lines are moved
  ;;         to after the right-hand destination address, which may be
  ;;         the address 0 (zero).  The current address is set to  the
  ;;         last line moved.
  (with-addresses (buffer (curr from 1) (last to 1) (target arg 0))
    (buffer-save-undo buffer)
    (do-paste buffer arg (do-cut buffer curr last))))


(defun cmd-change-lines (buffer from to arg)
  (dbg (format t "CMD-CHANGE-LINES: ~%"))
  ;; (.,.)c  Changes  lines  in  the  buffer.  The addressed lines are
  ;;         deleted from the buffer, and text is  appended  in  their
  ;;         place.   Text  is  entered  in  input  mode.  The current
  ;;         address is set to last line entered.
  (with-addresses (buffer (curr from 1) (last to 1) (target arg 0))
    (buffer-save-undo buffer)
    (setf (buffer-cut-lines buffer) (do-cut buffer curr last))
    (do-insert buffer curr)))


(defun cmd-join-lines (buffer from to arg)
  (declare (ignore arg))
  (dbg (format t "CMD-JOIN-LINES: ~%"))
  ;; (.,.+1)j
  ;;         Joins  the  addressed  lines.   The  addressed  lines are
  ;;         deleted from the buffer and replaced  by  a  single  line
  ;;         containing their joined text.  The current address is set
  ;;         to the resultant line.
  (with-addresses (buffer (curr from 1) (last to 1))
    (buffer-save-undo buffer)
    (setf (buffer-cut-lines buffer) (do-cut buffer curr last))
    (do-paste buffer (max 0 (1- curr))
              (apply (function concatenate) 'string
                     (buffer-cut-lines buffer)))))


(defun cmd-mark (buffer from to arg)
  (declare (ignore from))
  (dbg (format t "CMD-MARK: ~%"))
  ;; (.)klc  Marks a line with a lower case letter lc.  The  line  can
  ;;         then  be  addressed as 'lc (i.e., a single quote followed
  ;;         by lc ) in subsequent commands.  The mark is not  cleared
  ;;         until the line is deleted or otherwise modified.
  (with-addresses (buffer (curr to 1))
    (buffer-set-mark buffer arg curr)))


(defun cmd-print-line-number (buffer from to arg)
  (declare (ignore from arg))
  (dbg (format t "CMD-PRINT-LINE-NUMBER: ~%"))
  ;; ($)=    Prints the line number of the addressed line.
  (with-addresses (buffer (curr to 1))
    (format *terminal-io* "~D~%" curr)))


(defun cmd-scroll-lines (buffer from to arg)
  (declare (ignore from arg))
  (dbg (format t "CMD-SCROLL-LINES: ~%"))
  ;; (.+1)zn Scrolls n lines at a time starting at addressed line.  If
  ;;         n is not specified, then the current window size is used.
  ;;         The current address is set to the last line printed.
  (with-addresses (buffer (curr to 1))
    ;; TODO: IMPLEMENT SCROLL!
    (do-print-lines buffer curr (buffer-length buffer)
                    (lambda (linum line)
                      (declare (ignore linum))
                      (format *terminal-io* "~A~%" line))) ))


(defun cmd-print-lines (buffer from to arg)
  (declare (ignore arg))
  (dbg (format t "CMD-PRINT-LINES: ~%"))
  ;;     (.+1)newline
  ;;             Prints the addressed line, and sets the  current  address
  ;;             to that line.
  ;;     (.,.)p  Prints the addressed lines.    If invoked from  a  termi-
  ;;             nal, ed pauses at the end of each page until a newline is
  ;;             entered.  The current address is set  to  the  last  line
  ;;             printed.
  (with-addresses (buffer (curr from 1) (last to 1))
    (do-print-lines buffer curr last
                    (lambda (linum line)
                      (declare (ignore linum))
                      (format *terminal-io* "~A~%" line)))))


(defun cmd-print-lines-and-numbers (buffer from to arg)
  (declare (ignore arg))
  (dbg (format t "CMD-PRINT-LINES-AND-NUMBERS: ~%"))
  ;;     (.,.)n  Prints the addressed lines along with their line numbers.
  ;;             The current address is set to the last line printed.
  ;;
  (with-addresses (buffer (curr from 1) (last to 1))
    (do-print-lines buffer curr last
                    (lambda (linum line)
                      (format *terminal-io* "~6D  ~A~%" linum line)))))

   
(defun cmd-print-lines-unambiguously (buffer from to arg)
  (declare (ignore arg))
  (dbg (format t "CMD-PRINT-LINES-UNAMBIGUOUSLY: ~%"))
  ;;     (.,.)l  Prints  the  addressed  lines  unambiguously.  If invoked
  ;;             from a terminal, ed pauses at the end of each page  until
  ;;             a  newline is entered.  The current address is set to the
  ;;             last line printed.
  (with-addresses (buffer (curr from 1) (last to 1))
    (do-print-lines
        buffer curr last
        (lambda (linum line)
          (declare (ignore linum))
          (do ((i 0 (1+ i))
               (ch))
              ((>= i (length line)) (format *terminal-io* "$~%"))
            (setq ch (char line i))
            (if (graphic-char-p ch)
                (format *terminal-io* "~C" ch)
                (let ((ass (assoc (char-code ch)
                                  '((7 . "a") (8 . "b") (9 . "t")
                                    (10 . "l") (11 . "v") (12 . "f")
                                    (13 . "r")))))
                  (if ass
                      (format *terminal-io* "\\~A" (cdr ass))
                      (format *terminal-io* "\\~3,'0O" (char-code ch))))))))))


(defun cmd-substitute (buffer from to arg)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (dbg (format t "CMD-SUBSTITUTE: ~%"))
  ;; (.,.)s/re/replacement/
  ;; (.,.)s/re/replacement/g
  ;; (.,.)s/re/replacement/n
  ;;         Replaces  text  in the addressed lines matching a regular
  ;;         expression re with replacement.   By  default,  only  the
  ;;         first  match  in  each  line  is  replaced.   If  the `g'
  ;;         (global)  suffix  is  given,  then  every  match  to   be
  ;;         replaced.   The  `n' suffix, where n is a postive number,
  ;;         causes only the nth match to be replaced.  It is an error
  ;;         if no substitutions are performed on any of the addressed
  ;;         lines.   The  current  address  is  set  the  last   line
  ;;         affected.
  ;;
  ;;         re  and  replacement  may  be  delimited by any character
  ;;         other than space and newline (see the `s' command below).
  ;;         If one or two of the last delimiters is omitted, then the
  ;;         last line affected is printed as though the print  suffix
  ;;         `p' were specified.
  ;;
  ;;         An  unescaped  `&' in replacement is replaced by the cur-
  ;;         rently matched text.  The character sequence `\m',  where
  ;;         m  is a number in the range [1,9], is replaced by the mth
  ;;         backreference  expression  of  the  matched   text.    If
  ;;         replacement  consists  of  a single `%', then replacement
  ;;         from the last substitution  is  used.   Newlines  may  be
  ;;         embedded  in replacement if they are escaped with a back-
  ;;         slash (\).
  ;;
  ;; (.,.)s  Repeats the last substitution.  This form of the `s' com-
  ;;         mand  accepts  a  count suffix `n', or any combination of
  ;;         the characters `r', `g', and `p'.  If a count suffix  `n'
  ;;         is  given,  then only the nth match is replaced.  The `r'
  ;;         suffix causes the regular expression of the  last  search
  ;;         to  be used instead of the that of the last substitution.
  ;;         The `g' suffix toggles the global suffix of the last sub-
  ;;         stitution.   The  `p'  suffix toggles the print suffix of
  ;;         the last substitution The current address is set  to  the
  ;;         last line affected.
  (format *terminal-io* "NOT IMPLEMENTED YET.~%"))
    
        
(defun cmd-edit-matching (buffer from to arg)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (dbg (format t "CMD-EDIT-MATCHING: ~%"))
  ;; (1,$)g/re/command-list
  ;;         Applies  command-list  to  each  of  the  addressed lines
  ;;         matching a regular expression re.  The current address is
  ;;         set  to the line currently matched before command-list is
  ;;         executed.  At the end of the  `g'  command,  the  current
  ;;         address is set to the last line affected by command-list.
  ;;
  ;;         Each command in command-list must be on a separate  line,
  ;;         and  every line except for the last must be terminated by
  ;;         a backslash (\).  Any commands are  allowed,  except  for
  ;;         `g',  `G', `v', and `V'.  A newline alone in command-list
  ;;         is equivalent to a `p' command.
  ;;
  (format *terminal-io* "NOT IMPLEMENTED YET.~%"))


(defun cmd-edit-not-matching (buffer from to arg)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (dbg (format t "CMD-EDIT-NOT-MATCHING: ~%"))
  ;; (1,$)v/re/command-list
  ;;         Applies  command-list  to each of the addressed lines not
  ;;         matching a regular expression re.  This is similar to the
  ;;         `g' command.
  ;;
  (format *terminal-io* "NOT IMPLEMENTED YET.~%"))


(defun cmd-user-edit-matching (buffer from to arg)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (dbg (format t "CMD-USER-EDIT-MATCHING: ~%"))
  ;; (1,$)G/re/
  ;;         Interactively edits the addressed lines matching a  regu-
  ;;         lar  expression  re.  For each matching line, the line is
  ;;         printed, the current address is  set,  and  the  user  is
  ;;         prompted  to enter a command-list.  At the end of the `G'
  ;;         command, the current address is  set  to  the  last  line
  ;;         affected by (the last) command-list.
  ;;
  ;;         The format of command-list is the same as that of the `g'
  ;;         command.  A newline alone acts as a null command list.  A
  ;;         single `&' repeats the last non-null command list.
  (format *terminal-io* "NOT IMPLEMENTED YET.~%"))


(defun cmd-user-edit-not-matching (buffer from to arg)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (dbg (format t "CMD-USER-EDIT-NOT-MATCHING: ~%"))
  ;; (1,$)V/re/
  ;;         Interactively  edits  the  addressed lines not matching a
  ;;         regular expression re.  This is similar to the  `G'  com-
  ;;         mand.
  (format *terminal-io* "NOT IMPLEMENTED YET.~%"))


(defun file-or-command-arg (arg)
  "
  -->  :empty 
  -->  :path           ; path    ; exists (t/nil)
  -->  :command        ; command
  -->  :command-append ; command
  -->  :invalid        ; error-message
"
  (if (string= "" arg)
      (values :empty)
      (progn
        (let ((pos (skip-spaces arg)))
          (when (and pos (< 0 pos)) (setq arg (subseq arg pos))))
        (cond
          ((or (string= "" arg)
               (handler-case (prog1 nil (probe-file arg)) (error nil t)))
           (values :invalid  "Invalid filename"))
          ((and (<= 2 (length arg)) (string= "!!" arg :end2 2))
           (values :command-append (subseq arg 2)))
          ((and (<= 1 (length arg)) (string= "!"  arg :end2 1))
           (values :command (subseq arg 1)))
          (t
           (values :path arg (probe-file arg)))))))



(defun cmd-write-or-append (buffer from to arg mode)
  (with-addresses (buffer (curr from 1) (last to 1))
    (multiple-value-bind (kind path exists) (file-or-command-arg arg)
      (declare (ignore exists))
      (case kind
        ((:empty)
         (if (buffer-path buffer)
             (do-write buffer (buffer-path buffer) mode curr last)
             (buffer-set-error buffer "No current filename")))
        ((:path)
         (unless (buffer-path buffer)
           (setf (buffer-path buffer) path))
         (do-write buffer path mode curr last))
        ((:comand)
         (setf (buffer-command buffer) path)
         (do-command buffer
           :input (subseq (buffer-lines buffer) curr (1+ last))))
        ((:comand-append)
         (setf (buffer-command buffer)
               (concatenate 'string (buffer-command buffer) path))
         (do-command buffer
           :input (subseq (buffer-lines buffer) curr (1+ last))))
        ((:invalid) (buffer-set-error buffer path))
        (otherwise
         (buffer-set-error buffer "Internal error: FILE-OR-COMMAND-ARG"))))))


(defun cmd-append-file (buffer from to arg)
  (dbg (format t "CMD-APPEND-FILE: ~%"))
  ;; (1,$)W file
  ;;         Appends  the addressed lines to the end of file.  This is
  ;;         similar to the `w' command, expect that the previous con-
  ;;         tents  of  file is not clobbered.  The current address is
  ;;         unchanged.
  (cmd-write-or-append buffer from to arg :append))


(defun cmd-write-file (buffer from to arg)
  (dbg (format t "CMD-WRITE-FILE: ~%"))
  ;; (1,$)w file
  ;;         Writes  the  addressed  lines to file.  Any previous con-
  ;;         tents of file is lost without warning.  If  there  is  no
  ;;         default  filename,  then  the  default filename is set to
  ;;         file, otherwise it is unchanged.  If no filename is spec-
  ;;         ified,  then  the  default filename is used.  The current
  ;;         address is unchanged.
  ;;
  ;; (1,$)w !command
  ;;         Writes  the  addressed  lines  to  the  standard input of
  ;;         `!command', (see the !command below).  The default  file-
  ;;         name and current address are unchanged.
  (cmd-write-or-append buffer from to arg :supersede))


(defun cmd-write-file-quit (buffer from to arg)
  (dbg (format t "CMD-WRITE-FILE-QUIT: ~%"))
  ;; (1,$)wq file
  ;;         Writes the addressed lines to file, and then  executes  a
  ;;         `q' command.
  ;;
  (cmd-write-file buffer from to arg)
  (unless (buffer-got-error buffer)
    (cmd-quit buffer from to arg)))


(defun cmd-edit-or-read (buffer arg linum)
  "
LINUM:  NIL ==> Edit, NUMBERP ==> Read
"
  (multiple-value-bind (kind path exists) (file-or-command-arg arg)
    (declare (ignore exists))
    (case kind
      ((:empty)
       (if (buffer-path buffer)
           (progn
             (unless linum (buffer-erase buffer))
             (do-read buffer (buffer-path buffer) (or linum 0)))
           (buffer-set-error buffer "No current filename")))
      ((:path)
       (setf (buffer-path buffer) path)
       (unless linum (buffer-erase buffer))
       (do-read buffer path (or linum 0)))
      ((:comand)
       (setf (buffer-command buffer) path)
       (unless linum (buffer-erase buffer))
       (do-paste buffer (or linum 0)
                 (filter-command-output (do-command buffer :output :result))))
      ((:comand-append)
       (setf (buffer-command buffer)
             (concatenate 'string (buffer-command buffer) path))
       (unless linum (buffer-erase buffer))
       (do-paste buffer (or linum 0)
                 (filter-command-output (do-command buffer :output :result))))
      ((:invalid) (buffer-set-error buffer path))
      (otherwise
       (buffer-set-error buffer "Internal error: FILE-OR-COMMAND-ARG")))))


(defun cmd-read-file (buffer from to arg)
  (declare (ignore from))
  (dbg (format t "CMD-READ: ~%"))
  ;; ($)r file
  ;;         Reads  file  to after the addressed line.  If file is not
  ;;         specified, then the default filename is used.   If  there
  ;;         was  no  default  filename prior to the command, then the
  ;;         default filename is set to file.  Otherwise, the  default
  ;;         filename is unchanged.  The current address is set to the
  ;;         last line read.
  ;;
  ;; ($)r !command
  ;;         Reads to after the addressed line the standard output  of
  ;;         `!command',  (see the !command below).  The default file-
  ;;         name is unchanged.  The current address  is  set  to  the
  ;;         last line read.
  (with-addresses (buffer (curr to 1))
    (cmd-edit-or-read buffer arg curr)))


(defun cmd-edit-file-unconditionally (buffer from to arg)
  (declare (ignore from to))
  (dbg (format t "CMD-EDIT-FILE-UNCONDITIONALLY: ~%"))
  ;; E file  Edits  file  unconditionally.   This  is similar to the e
  ;;         command, except  that  unwritten  changes  are  discarded
  ;;         without  warning.  The current address is set to the last
  ;;         line read.
  (cmd-edit-or-read buffer arg nil)
  (unless (buffer-got-error buffer)
    (setf (buffer-modified buffer) nil)
    (format *terminal-io* "~D~%" (buffer-current-linum buffer))))

  
(defun cmd-edit-file (buffer from to arg)
  (declare (ignore from to))
  (dbg (format t "CMD-EDIT-FILE: ~%"))
  ;; e file  Edits  file,  and  sets the default filename.  If file is
  ;;         not specified, then the  default filename is  used.   Any
  ;;         lines  in  the  buffer are deleted before the new file is
  ;;         read.  The current address is set to the last line  read.
  ;;
  ;; e !command
  ;;         Edits  the  standard  output of `!command', (see !command
  ;;         below).  The default filename is unchanged.  Any lines in
  ;;         the  buffer  are  deleted before the output of command is
  ;;         read.  The current address is set to the last line  read.
  (unless-modified buffer
     (cmd-edit-or-read buffer arg nil)))


(defun cmd-set-default-filename (buffer from to arg)
  (declare (ignore from to))
  (dbg (format t "CMD-SET-DEFAULT-FILENAME: ~%"))
  ;; f file  Sets the default filename to file.  If file is not speci-
  ;;         fied, then the default unescaped filename is printed.
  (when (string/= "" arg)
    (let ((pos (skip-spaces arg)))
      (when (and pos (< 0 pos)) (setq arg (subseq arg pos))))
    (if (or (string= "" arg)
            (handler-case (prog1 nil (probe-file arg)) (error nil t)))
        (buffer-set-error buffer "Invalid filename")
        (setf (buffer-path buffer) arg)))
  (format *terminal-io* "~A~%" (buffer-path buffer)))


(defun cmd-print-last-error (buffer from to arg)
  (declare (ignore from to arg))
  (dbg (format t "CMD-PRINT-LAST-ERROR: ~%"))
  ;; h       Prints an explanation of the last error.
  (format *terminal-io* "~A~%" (buffer-last-error buffer)))


(defun cmd-toggle-error-explanations (buffer from to arg)
  (declare (ignore from to arg))
  (dbg (format t "CMD-TOGGLE-ERROR-EXPLANATIONS: ~%"))
  ;; H       Toggles  the printing of error explanations.  By default,
  ;;         explanations are not printed.  It is recommended that  ed
  ;;         scripts begin with this command to aid in debugging.
  (toggle (buffer-show-errors buffer))
  (unless (string= "" (buffer-last-error buffer))
    (format *terminal-io* "~A~%" (buffer-last-error buffer))))


(defun cmd-toggle-command-prompt (buffer from to arg)
  (declare (ignore from to arg))
  (dbg (format t "CMD-TOGGLE-COMMAND-PROMPT: ~%"))
  ;; P       Toggles  the  command prompt on and off.  Unless a prompt
  ;;         was specified by with command-line option -p string,  the
  ;;         command prompt is by default turned off.
  (toggle (buffer-show-prompt buffer)))


(defun cmd-quit (buffer from to arg)
  (declare (ignore from to arg))
  (dbg (format t "CMD-QUIT: ~%"))
  ;; q       Quits ed.
  (unless-modified buffer
     (setf (buffer-quit buffer) t)))


(defun cmd-quit-unconditionnaly (buffer from to arg)
  (declare (ignore from to arg))
  (dbg (format t "CMD-QUIT-UNCONDITIONNALY: ~%"))
  ;; Q       Quits  ed unconditionally.  This is similar to the q com-
  ;;         mand, except that unwritten changes are discarded without
  ;;         warning.
  (setf (buffer-quit buffer) t))


(defun cmd-subshell (buffer from to arg)
  (declare (ignore from to))
  (dbg (format t "CMD-SUBSHELL: ~%"))
  ;; !command
  ;;         Executes  command  via  sh(1).  If the first character of
  ;;         command is `!', then it is replaced by text of the previ-
  ;;         ous  `!command'.   ed  does not process command for back-
  ;;         slash (\) escapes.  However, an unescaped `%' is replaced
  ;;         by  the  default  filename.   When the shell returns from
  ;;         execution, a `!'  is printed to the standard output.  The
  ;;         current line is unchanged.
  (setf (buffer-command buffer)
        (if (char= (character "!") (char arg 0))
            (concatenate 'string (buffer-command buffer) (subseq arg 1))
            arg))
  (do-command buffer))

;; remaining free commands: "@%()_[]{};:'\"<>ABCDFIJKLMNORSTUXYZbo"

(defparameter *commands*
  '( ;;cmd from   to    argument     p   function
    ("t"  :curr  :curr  :curr        t   cmd-copy-lines)
    ("m"  :curr  :curr  :curr        t   cmd-move-lines)
    ("c"  :curr  :curr  nil          t   cmd-change-lines)
    ("#"  :curr  :curr  nil          t   cmd-comment)
    ("y"  :curr  :curr  nil          t   cmd-copy)
    ("d"  :curr  :curr  nil          t   cmd-delete-lines)
    ("p"  :curr  :curr  nil          t   cmd-print-lines)
    ("n"  :curr  :curr  nil          t   cmd-print-lines-and-numbers)
    ("l"  :curr  :curr  nil          t   cmd-print-lines-unambiguously)
    ("s"  :curr  :curr  substitution t   cmd-substitute)
    ("j"  :curr  :next  nil          t   cmd-join-lines)
    ("g"  :first :last  regexp       nil cmd-edit-matching)
    ("v"  :first :last  regexp       nil cmd-edit-not-matching)
    ("G"  :first :last  regexp       t   cmd-user-edit-matching)
    ("V"  :first :last  regexp       t   cmd-user-edit-not-matching)
    ("W"  :first :last  string       nil cmd-append-file)
    ("w"  :first :last  string       nil cmd-write-file)
    ("k"  nil    :curr  character    t   cmd-mark)
    ("a"  nil    :curr  nil          t   cmd-append)
    ("i"  nil    :curr  nil          t   cmd-insert)
    ("x"  nil    :curr  nil          t   cmd-paste)
    ("="  nil    :last  nil          t   cmd-print-line-number)
    ("r"  nil    :last  string       nil cmd-read-file)
    (nil  nil    :next  nil          nil cmd-print-lines)
    ("z"  nil    :next  number       t   cmd-scroll-lines)
    ("h"  nil    nil    nil          t   cmd-print-last-error)
    ("q"  nil    nil    nil          t   cmd-quit)
    ("Q"  nil    nil    nil          t   cmd-quit-unconditionnaly)
    ("P"  nil    nil    nil          t   cmd-toggle-command-prompt)
    ("H"  nil    nil    nil          t   cmd-toggle-error-explanations)
    ("u"  nil    nil    nil          t   cmd-undo)
    ("e"  nil    nil    string       nil cmd-edit-file)
    ("E"  nil    nil    string       nil cmd-edit-file-unconditionally)
    ("f"  nil    nil    string       nil cmd-set-default-filename)
    ("!"  nil    nil    string       nil cmd-subshell)
    ("wq" :first :last  string       nil cmd-write-file-quit)))


(dbg
 (push '("D"  nil    nil    nil          nil cmd-toggle-debug) *commands*)
 (defun cmd-toggle-debug (buffer from to arg)
   ;; !command
   ;;         Executes  command  via  sh(1).  If the first character of
   ;;         command is `!', then it is replaced by text of the previ-
   ;;         ous  `!command'.   ed  does not process command for back-
   ;;         slash (\) escapes.  However, an unescaped `%' is replaced
   ;;         by  the  default  filename.   When the shell returns from
   ;;         execution, a `!'  is printed to the standard output.  The
   ;;         current line is unchanged.
   (declare (ignore buffer from to arg))
   (dbg (format t "CMD-toggle-debug: ~%"))
   (toggle *show-debug*)))



(defun skip-spaces (command &optional (start 0))
  "
RETURN: The index of the next non white space character in command,
        starting from position, or nil if end of string.
"
  (do ((start start (1+ start)))
      ((or (>= start (length command))
           (char/= (character " ") (char command start)))
       (when (< start (length command)) start))))

;;addresses -->   address
;;              | address ',' address
;;              | ',' | '%' , ';' .
;;
;;
;;address -->   '.' | '$' | number
;;            | '-' | '^' | '-' number | '^' number
;;                  | '+' | '+' number | space number
;;                  | '/' re '/' | '?' re '?' | '//' | '??'
;;                  | "'" lc .
;;
;;lc --> 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'
;;      |'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'.
;;
;;re --> [first] ( ch | '\' ch | '[' char-class ']' | '[' '^' char-class ']'
;;                | '^' | '$' | '\(' re '\)' | re '*'
;;                | re '\{' num ',' num '\}'
;;                | re '\{' num ','  '\}'
;;                | re '\{' num '\}'
;;                | '\<' | '\>' | '\`' | "\'" | re '\?' | re '\+'
;;                | '\b' | '\B' | '\w' | '\W' .
;;first --> '*' .
;;
;;char-class --> '[:alnum:]'|'[:cntrl:]'|'[:lower:]'|'[:space:]'
;;              |'[:alpha:]'|'[:digit:]'|'[:print:]'|'[:upper:]'
;;              |'[:blank:]'|'[:graph:]'|'[:punct:]'|'[:xdigit:]'
;;              |'[.'col-elm'.]'|'[='col-elm'=]'.
;;col-elm --> .

  
(defun parse-address (command position)
  ;;address -->   '.' | '$' | number
  ;;            | '-' | '^' | '-' number | '^' number
  ;;                  | '+' | '+' number | space number
  ;;                  | '/' re '/' | '?' re '?' | '//' | '??'
  ;;                  | "'" lc .
  (declare (integer position))
  (let ((address nil) (ch))
    (flet ((parse-optional-number
               ()
             (setq position (skip-spaces command (1+ position)))
             (when (and position
                        (setq ch (char command position))
                        (digit-char-p ch))
               (multiple-value-bind (value pos)
                   (parse-integer command :start position :junk-allowed t)
                 (when value
                   (setq address (cons address value)))
                 (setq position pos)))))
      (setq position (skip-spaces command position))
      (when position
        (setq ch (char command position))
        (cond
          ((char= ch (character ".")) (setq address :curr) (incf position))
          ((char= ch (character "$")) (setq address :last) (incf position))
          ((digit-char-p ch)
           (multiple-value-bind (value pos)
               (parse-integer command :start position :junk-allowed t)
             (when value
               (setq address (cons :linum value)))
             (setq position pos)))
          ((or (char= ch (character "^")) (char= ch (character "-")))
           (setq address :prev)
           (parse-optional-number))
          ((or (char= ch (character " ")) (char= ch (character "+")))
           (setq address :next)
           (parse-optional-number))
          ((or (char= ch (character "/")) (char= ch (character "?")))
           ;; TODO: regexp
           ;; eat regexp:
           (setq address
                 (cons :regexp 
                       (do ((terminator ch)
                            (end position (1+ end)))
                           ((or (>= end (length command))
                                (char= terminator (char command end)))
                            (prog1 (subseq command position (1+ end))
                              (setq position (1+ end))))
                         (declare (integer end))))))
          ((char= ch (character "'"))
           (incf position)
           ;; TODO: when there is an error here it's: "Invalid mark character"
           ;; TODO: not: "Invalid address" !
           (when (< position (length command))
             (setq address (cons :mark (char command position)))
             (incf position)))
          )))
    (values address position)))


(defun parse-and-run-command (buffer command)
  (let ((position (skip-spaces command))
        (cmd) (ch) (from) (to) (arg) (print nil))
    (buffer-clear-error buffer)
    (macrolet ((set-error (message) `(buffer-set-error buffer ,message))
               (got-error () `(buffer-got-error buffer)))
      (when position
        (setq ch (char command position))
        (cond
          ((or (char= (character ",") ch)  (char= (character "%") ch))
           (setq from :first to :last)
           (setq position (skip-spaces command (1+ position))))
          ((char= (character ";") ch)
           (setq from :curr  to :last)
           (setq position (skip-spaces command (1+ position))))
          ((not (alpha-char-p ch))
           (multiple-value-setq (to position) (parse-address command position))
           (if (eq to :error)
               (set-error "Invalid address")
               (progn
                 (setq position (skip-spaces command position))
                 (when position
                   (setq ch (char command position))
                   (when (char= (character ",") ch)
                     (setq from to)
                     (multiple-value-setq (to position)
                       (parse-address command (1+ position)))
                     (if (eq to :error)
                         (set-error "Invalid address")
                         (setq position (skip-spaces command position))))))))))
      (dbg (format t "PARC: from= ~S to= ~S position= ~S got-error= ~S~%"
                   from to position (got-error)))
      (unless (got-error)
        (if (null position)
            (setq cmd (assoc nil *commands*))
            (setq cmd (assoc (subseq command position (1+ position)) *commands*
                             :test (function string=))
                  position (1+ position)))
        (dbg (format t "PARC: command key= ~S ~%      cmd= ~S~%"
                     (when position (subseq command (1- position) position))
                     (nconc (butlast cmd)
                            (list (symbol-name (car (last cmd)))))))
        (let ((defr (second cmd))
              (deto (third  cmd))
              (argk (fourth cmd))
              (accp (fifth  cmd))
              (cmdf (sixth  cmd)))
          (unless from (setq from defr))
          (unless to   (setq to   deto))
          (case argk
            ((nil))
            ((character)
             (if (and position (< position (length command))
                      (alpha-char-p (char command position))
                      (lower-case-p (char command position)))
                 (setf arg (char command position))
                 (set-error "Invalid mark character")))
            ((number)
             (when position
               (multiple-value-setq (arg position)
                 (parse-integer command :start position :junk-allowed t))
               (unless arg
                 (set-error "Invalid address"))))
            ((string)
             (when position
               (setq arg (subseq command position)
                     position (length command))))
            ((regexp)
             )
            ((substitution)
             )
            ((:curr)
             (when position
               (multiple-value-setq (arg position)
                 (parse-address command position))
               (cond
                 ((eq arg :error)  (set-error "Invalid address"))
                 ((null arg) (setq arg :curr)))))
            (otherwise
             (set-error "Internal error: *command* table.")))
          (dbg (format t "PARC: from= ~S to= ~S position= ~S got-error= ~S~%"
                       from to position (got-error))
               (format t "      arg= ~S ~%" arg))
          (unless (got-error)
            (if (and accp position (< position (length command))
                     (char= (character "p") (char command position)))
                (setq  print t)
                (when (and position (skip-spaces command position))
                  (set-error "Invalid command suffix")))
            (unless (got-error)
              (dbg (format t "PARC: calling (~A ~S ~S ~S ~S ~S)~%"
                           cmdf "BUFFER" from to arg print))
              (setf (buffer-print buffer) print)
              (funcall cmdf buffer from to arg))))))))


(defun edit (buffer)
  (format *terminal-io* "~&")
  (setf (buffer-quit buffer) nil)
  (format *terminal-io* "~D~%" (buffer-length buffer))
  (loop
     (let ((command (read-line *terminal-io* nil nil)))
       (unless command (return))
       (dbg (format t "EDIT: read command ~S~%" command))
       (setf (buffer-print buffer) nil)
       (buffer-clear-error buffer)
       (parse-and-run-command buffer command)
       (dbg (format t "EDIT: parc returned (~S ~S ~S)~%"
                    (buffer-print buffer)
                    (buffer-got-error buffer)
                    (buffer-quit buffer)))
       (if (buffer-got-error buffer)
           (if (buffer-show-errors buffer)
               (format *terminal-io* "~A~%" (buffer-last-error buffer))
               (format *terminal-io* "?~%"))
           (when (buffer-print buffer)
             (let ((current (buffer-current-linum buffer)))
               (if (limit current 1 (buffer-length buffer))
                   (format *terminal-io* "~A~%" (buffer-nth-line buffer current))
                   (progn
                     (buffer-set-error buffer "Invalid address")
                     (if (buffer-show-errors buffer)
                         (format *terminal-io* "~A~%" (buffer-last-error buffer))
                         (format *terminal-io* "?~%")))))))
       (if (buffer-quit buffer)
           (return)
           (when (buffer-show-prompt buffer)
             (format *terminal-io* "~A~%" (buffer-prompt-string buffer)))))))


;; ed &optional x => implementation-dependent
;;
;;
;;
;; Arguments and Values:
;;
;;
;; x---nil, a pathname, a string, or a function name. The default is nil.
;;
;;
;; Description:
;;
;;
;; ed invokes the editor if the implementation provides a resident editor.
;;
;; If x is nil, the editor is entered. If the editor had been previously
;; entered, its prior state is resumed, if possible.
;;
;; If x is a pathname or string, it is taken as the pathname designator
;; for a file to be edited.
;;
;; If x is a function name, the text of its definition is edited. The
;; means by which the function text is obtained is implementation-defined.


(defparameter *current-buffer* (make-buffer))


(defun ed (&optional x)
  "
DO:  Invokes the ed(1)-like editor.
X:   NIL, a pathname, a string, or a function name. The default is NIL.
"
  (cond
    ((null x)
     (edit *current-buffer*))
    ((or (pathnamep x) (stringp x))
     (setq *current-buffer* (buffer-read x))
     (edit *current-buffer*))
    ((symbolp x)
     (let ((fle (function-lambda-expression (fdefinition x))))
       (setq *current-buffer*
             (buffer-from-string
              (if (eq 'lambda (car fle))
                  (format nil "~S~%"  (cons 'defun (cons x (cdr fle))))
                  (format nil "~S is not a function.~%" x))))
       (edit *current-buffer*)))
    ;; TODO: If x is a function name, ...
    (t
     (error "Invalid argument ~S." x))))




(defvar test-text
  "One little boy and
Two little girls, climbed up a
Tree near the sky.
Four birds landed on that tree.
Five eggs were layed each on one child head.
") 

;; (progn (ext:cd "/home/pascal/src/lisp/encours/") (load "ed.lisp"))

;;;; THE END ;;;;
