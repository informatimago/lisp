;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               ed.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    ed(1) in COMMON-LISP.
;;;;
;;;;    Real men do it with magnets.  ed is for girly men.
;;;;
;;;;    This is a clone of the unix ed(1) editor written in pure Common-Lisp.
;;;;    Since Common-Lisp does not define any process management functions,
;;;;    all !command forms are taken as Lisp forms instead of sh(1) commands.
;;;;    These forms are executed within a (LAMBDA (*INPUT*) command) with
;;;;    the *INPUT* argument bound to a mutable list of inmutable input strings,
;;;;    one per line.  The result of the returning form in command must be
;;;;    a list of string, the list of resulting lines to be inserted into the
;;;;    buffer or to be printed on the terminal.
;;;;
;;;;    For the rest, the aim is to be 100% user-interface compatible with ed(1).
;;;;
;;;;    <a href=http://www.gnu.org/fun/jokes/ed.msg.html>Ed, man! !man ed</a>
;;;;
;;;;    Can you imagine that some Common-Lisp implementations DON'T provide
;;;;    any editor (in conformity with the Common-Lisp specifications)?
;;;;    Not complete (waiting for a REGEXP package). But otherwise
;;;;    functional enough. 
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2003-12-19 <PJB> Created.
;;;;BUGS
;;;;
;;;;    Not complete.
;;;;    (Still waiting on regexp package...).
;;;; 
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
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

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ED"
  (:DOCUMENTATION
   "This package exports an implementation of the COMMON-LISP ED function
    following the user manual of ed(1).

    Copyright Pascal J. Bourguignon 2003 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:USE "COMMON-LISP")
  (:SHADOW "ED")
  (:EXPORT "ED"))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.ED")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (DEFCONSTANT +DEBUG+ T)
  (defparameter show-debug t))


(DEFMACRO DBG (&BODY BODY)
  (WHEN +DEBUG+ 
    `(when show-debug (LET ((*STANDARD-OUTPUT* *TRACE-OUTPUT*)) ,@BODY))))

;;(WHEN +DEBUG+
;;(shadow 'handler-case)
;;(defmacro handler-case (form &rest args) form))



(DEFSTRUCT BUFFER
  (PATH         NIL)
  (LINES        '(0)) ;; (index-of-current-line . lines)
  (MARKS        '( )) ;; ((ch . linum) ...)
  (OLD-LINES    '(0)) ;; (index-of-current-line . lines)
  (OLD-MARKS    '( )) ;; ((ch . linum) ...)
  (CUT-LINES    '( )) ;; (lines ...)
  (MESSAGE      '(NIL nil ""))
  (PROMPT       '(NIL . "*"))
  (command      nil)
  (MODIFIED     NIL)
  (print        NIL)
  (QUIT         NIL)
  ) ;;BUFFER

;; (setq b (buffer-read "~/test.txt"))

(DEFMACRO TOGGLE (PLACE) `(SETF ,PLACE (NOT ,PLACE)))


;;-------
;; lines:
;;-------

(DEFMACRO BUFFER-CURRENT-LINUM  (BUFFER) `(CAR (BUFFER-LINES ,BUFFER)))
(DEFUN    BUFFER-LENGTH         (BUFFER) (LENGTH (CDR (BUFFER-LINES BUFFER))))
(DEFUN    BUFFER-NTH-LINE (BUFFER LINUM) (CAR (BUFFER-LINE-CONS BUFFER LINUM)))

(DEFUN BUFFER-LINE-CONS (BUFFER LINUM)
  (IF (< LINUM 0)
      NIL
      (NTHCDR  LINUM (BUFFER-LINES BUFFER)))
  ) ;;BUFFER-LINE-CONS

;;-------
;; marks:
;;-------

(DEFUN BUFFER-SET-MARK (BUFFER CH LINUM)
  (LET ((ASS (ASSOC CH (BUFFER-MARKS BUFFER))))
    (IF ASS
        (SETF (CDR ASS) LINUM)
        (PUSH (CONS CH LINUM) (BUFFER-MARKS BUFFER))))
  (VALUES)) ;;BUFFER-SET-MARK


(DEFUN BUFFER-GET-MARK (BUFFER CH)
  (CDR (ASSOC CH (BUFFER-MARKS BUFFER)))
  ) ;;BUFFER-GET-MARK


(DEFUN BUFFER-OFFSET-MARKS (BUFFER FROM OFFSET)
  (IF (< OFFSET 0)
      (LET ((MINDEL (+ FROM OFFSET)))
        (SETF (BUFFER-MARKS BUFFER)
              (MAPCAN (LAMBDA (ASS)
                        (COND
                          ((< (CDR ASS) MINDEL) (LIST ASS))
                          ((< (CDR ASS) FROM)   NIL)
                          (T  (INCF (CDR ASS) OFFSET) (LIST ASS))))
                      (BUFFER-MARKS BUFFER))))
      (MAP NIL (LAMBDA (ASS) (WHEN (<= FROM (CDR ASS)) (INCF (CDR ASS) OFFSET)))
           (BUFFER-MARKS BUFFER)))
  ) ;;BUFFER-OFFSET-MARKS


(DEFUN COPY-MARKS (MARKS)
  (MAPCAR (LAMBDA (ASS) (CONS (CAR ASS) (CDR ASS))) MARKS)
  ) ;;COPY-MARKS


;;-------------
;; undo buffer:
;;-------------

(DEFUN BUFFER-SAVE-UNDO (BUFFER)
  (SETF (BUFFER-MODIFIED BUFFER) T)
  (SETF (BUFFER-OLD-MARKS BUFFER) (COPY-MARKS (BUFFER-MARKS BUFFER))
        (BUFFER-OLD-LINES BUFFER) (COPY-SEQ (BUFFER-LINES buffer)))
  ) ;;BUFFER-SAVE-UNDO


(DEFUN BUFFER-SWAP-UNDO (BUFFER)
  (PSETF (BUFFER-OLD-MARKS BUFFER) (BUFFER-MARKS BUFFER)
         (BUFFER-MARKS BUFFER)     (BUFFER-OLD-MARKS BUFFER)
         (BUFFER-OLD-LINES BUFFER) (BUFFER-LINES BUFFER)
         (BUFFER-LINES BUFFER)     (BUFFER-OLD-LINES BUFFER))
  ) ;;BUFFER-SWAP-UNDO


;;----------------
;; error messages:
;;----------------

(defmacro buffer-show-errors (buffer) `(first  (buffer-message ,buffer)))
(defmacro buffer-got-error   (buffer) `(second (buffer-message ,buffer)))
(defmacro buffer-last-error  (buffer) `(third  (buffer-message ,buffer)))

  
(defun buffer-clear-error (buffer)
  (setf (buffer-got-error buffer) nil)
  ) ;;buffer-clear-error


(defun buffer-set-error (buffer message)
  (setf (buffer-last-error buffer) message
        (buffer-got-error  buffer) t)
  ) ;;buffer-set-error



;;--------
;; prompt:
;;--------

(defmacro buffer-show-prompt   (buffer) `(car (buffer-prompt ,buffer)))
(defmacro buffer-prompt-string (buffer) `(cdr (buffer-prompt ,buffer)))





(defun buffer-erase (buffer)
  ;; no undo!
  (setf (buffer-lines     buffer) '(0)
        (buffer-marks     buffer) nil
        (buffer-old-lines buffer) '(0)
        (buffer-old-marks buffer) nil
        (buffer-cut-lines buffer) nil
        (buffer-modified  buffer) t)
  ) ;;buffer-erase



(DEFUN BUFFER-READ (PATH)
  (let ((buffer (MAKE-BUFFER :PATH PATH)))
    (do-read buffer path 0)
    buffer)
  ) ;;BUFFER-READ


(DEFUN BUFFER-FROM-STRING (TEXT)
  (let ((buffer(make-buffer)))
    (do-paste buffer 0
              (DO ((NEWLINE (FORMAT NIL "~%"))
                   (LINES ()) (POSITION 0) (NEXTPOS 0))
                  ((>= NEXTPOS (LENGTH TEXT)) (NREVERSE LINES))
                (SETQ POSITION (SEARCH NEWLINE TEXT :START2 NEXTPOS))
                (IF POSITION
                    (PROGN
                      (PUSH (SUBSEQ TEXT NEXTPOS POSITION) LINES)
                      (SETQ NEXTPOS (+ POSITION (LENGTH NEWLINE))))
                    (PROGN
                      (PUSH (SUBSEQ TEXT NEXTPOS) LINES)
                      (SETQ NEXTPOS (LENGTH TEXT))))))
    buffer)
  ) ;;BUFFER-FROM-STRING



(DEFUN LIMIT (VALUE MIN MAX)
  (IF (<= MIN VALUE MAX)
      VALUE
      NIL)) ;;LIMIT


(DEFUN ADDRESS->LINUM (BUFFER ADDRESS &OPTIONAL (MIN 1))
  (COND
    ((NULL ADDRESS) NIL)
    ((EQ ADDRESS :CURR)  (BUFFER-CURRENT-LINUM BUFFER))
    ((EQ ADDRESS :FIRST) 1)
    ((EQ ADDRESS :LAST)  (BUFFER-LENGTH BUFFER))
    ((EQ ADDRESS :NEXT)  (LIMIT (1+ (BUFFER-CURRENT-LINUM BUFFER))
                                1 (BUFFER-LENGTH BUFFER)))
    ((EQ ADDRESS :PREV)  (LIMIT (1- (BUFFER-CURRENT-LINUM BUFFER))
                                MIN (BUFFER-LENGTH BUFFER)))
    ((NOT (CONSP ADDRESS)) NIL)
    ((EQ (CAR ADDRESS) :NEXT) (LIMIT (+ (BUFFER-CURRENT-LINUM BUFFER)
                                        (CDR ADDRESS))
                                     1 (BUFFER-LENGTH BUFFER)))
    ((EQ (CAR ADDRESS) :PREV) (LIMIT (- (BUFFER-CURRENT-LINUM BUFFER)
                                        (CDR ADDRESS))
                                     MIN (BUFFER-LENGTH BUFFER)))
    ((EQ (CAR ADDRESS) :LINUM) (LIMIT (CDR ADDRESS)
                                      MIN (BUFFER-LENGTH BUFFER)))
    ((EQ (CAR ADDRESS) :MARK) (LIMIT (BUFFER-GET-MARK BUFFER (CDR ADDRESS))
                                     1 (BUFFER-LENGTH BUFFER)))
    ((EQ (CAR ADDRESS) :REGEXP)
     ;; TODO: regexp not implemented yet.
     (format *terminal-io* "REGEXP NOT IMPLEMENTED YET.~%")
     ))) ;;ADDRESS->LINUM


(defmacro with-addresses ((buffer . addresses) &body body)
  `(LET ,(mapcar (lambda (vam)
                   `(,(first vam)
                      (ADDRESS->LINUM ,buffer ,(second vam) ,(third vam))))
                 addresses)
     (IF (or ,@(mapcar (lambda (vam) `(null ,(first vam))) addresses)
             ,@(when (<= 2 (length addresses))
                     `((< ,(first (second addresses))
                          ,(first (first  addresses))))))
         (buffer-set-error ,buffer "Invalid address")
         (PROGN ,@body)))
  ) ;;with-addresses


(defmacro unless-modified (buffer &body body)
  `(if (buffer-modified ,buffer)
       (progn
         (setf (buffer-modified ,buffer) nil)
         (buffer-set-error ,buffer "Warning: file modified"))
       (progn
         ,@body))
  ) ;;unless-modified


;; (progn (PJB-CL+INDENT with-addresses 1)(PJB-CL+INDENT unless-modified 1))

(DEFUN DO-INSERT (BUFFER LINUM)
  (DBG (FORMAT T "DO-INSERT(~S ~S) ~%" "buffer" linum))
  (DO ((PLACE (BUFFER-LINE-CONS BUFFER LINUM) (CDR PLACE))
       (LINE  (READ-LINE *TERMINAL-IO* NIL ".")
              (READ-LINE *TERMINAL-IO* NIL "."))
       (CURR  LINUM (1+ CURR)))
      ((STRING= LINE ".")
       (PROGN
         (SETF (BUFFER-CURRENT-LINUM BUFFER) CURR)
         (BUFFER-OFFSET-MARKS BUFFER LINUM (- CURR LINUM))))
    (SETF (CDR PLACE) (CONS LINE (CDR PLACE))))
  ) ;;DO-INSERT


(defun do-paste (buffer linum new-lines)
  (DBG (FORMAT T "DO-PASTE(~S ~S [~D lines]) ~%"
               "buffer" linum (length new-lines)))
  (let* ((insert-point (buffer-line-cons buffer linum))
         (after        (cdr insert-point)))
    (setf (buffer-current-linum buffer) (+ linum (length new-lines)))
    (setf (cdr insert-point) new-lines)
    (setf (cdr (last new-lines)) after))
  ) ;;do-paste


(defun do-cut (buffer from last)
  "
RETURN: The list of cut lines.
"
  (DBG (FORMAT T "DO-CUT(~S ~S ~S) ~%" "buffer" from last))
  (let* ((delete-point (buffer-line-cons buffer (1- from)))
         (after-point  (buffer-line-cons buffer  last))
         (result       (cdr delete-point)))
    (setf (buffer-current-linum buffer)
          (if (null (cdr after-point)) from (1+ from)))
    (setf (cdr delete-point) (cdr after-point)
          (cdr after-point) Nil)
    (BUFFER-OFFSET-MARKS BUFFER from (- LAST from -1))
    result)
  ) ;;do-cut


(DEFUN DO-PRINT-LINES (BUFFER curr last FUNC)
  (DBG (FORMAT T " DO-PRINT-LINES(~S ~S ~S [a function]) ~%" "buffer" curr last))
  (setf (buffer-print buffer) nil)
  (DO* ((CURR CURR (1+ CURR))
        (LINES (BUFFER-LINE-CONS BUFFER CURR) (CDR LINES)))
       ((> CURR LAST)
        (SETF (BUFFER-CURRENT-LINUM BUFFER) LAST))
    (FUNCALL FUNC CURR (CAR LINES)))
  ) ;;DO-PRINT-LINES


(defun do-write (buffer path mode from last)
  (DBG (FORMAT T "DO-WRITE(~S ~S ~S ~S ~S) ~%" "buffer" path mode from last))
  (handler-case
      (with-open-file (output path :direction :output :if-exists mode
                              :if-does-not-exist :create)
        (do ((line (buffer-line-cons buffer from) (cdr line))
             (curr from (1+ curr)))
            ((> curr last))
          (format output "~A~%" (car line))))
    (error (err) (buffer-set-error buffer (format nil "~S" err))))
  ) ;;do-write


(defun do-read (buffer path linum)
  (DBG (FORMAT T "DO-READ(~S ~S ~S) ~%" "buffer" path linum))
  (handler-case
      (do-paste buffer linum
                (with-open-file (input path :direction :input
                                       :if-does-not-exist :error)
                  (do ((lines '() (cons line lines))
                       (line t))
                      ((null line) (nreverse (cdr lines)))
                    (setf line (read-line input nil nil)))))
    (error (err) (buffer-set-error buffer (format nil "~S" err))))
  ) ;;do-read


(defun filter-command-output (output)
  (if (and (listp output) (every (function stringp) output))
      output
      (list (format nil "~S" output)))
  ) ;;filter-command-output


;; (defmacro hc (form &rest rest) form)

(defun do-command (buffer &key (input nil) (output :print))
  (DBG (FORMAT T "DO-COMMAND(~S ~S ~S) ~%" "buffer" input output))
  (if (buffer-command buffer)
      (handler-case
          (progn
            (format *terminal-io* "~A~%" (buffer-command buffer))
            (let ((results  (eval `((lambda (*input*)
                                      ,(read-from-string 
                                        (format nil "(progn  ~A )"
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
        (values)))
  ) ;;do-command


(DEFUN CMD-COMMENT (BUFFER FROM TO ARG)
  (DECLARE (IGNORE FROM TO ARG))
  (DBG (FORMAT T "CMD-COMMENT: ~%"))
  ;; (.,.)#  Begins a comment;  the rest of the line, up to a newline,
  ;;         is ignored.  If a line address followed by a semicolon is
  ;;         given,  then  the current address is set to that address.
  ;;         Otherwise, the current address is unchanged.
  (setf (buffer-print buffer) nil)
  ) ;;CMD-COMMENT


(DEFUN CMD-APPEND (BUFFER FROM TO ARG)
  (declare (ignore from arg))
  (DBG (FORMAT T "CMD-APPEND: ~%"))
  ;; (.)a    Appends text to the  buffer  after  the  addressed  line,
  ;;         which  may  be  the address 0 (zero).  Text is entered in
  ;;         input mode.  The current address  is  set  to  last  line
  ;;         entered.
  (with-addresses (buffer (linum to 0))
    (BUFFER-SAVE-UNDO BUFFER)
    (DO-INSERT BUFFER LINUM))
  ) ;;CMD-APPEND


(DEFUN CMD-INSERT (BUFFER FROM TO ARG)
  (declare (ignore from arg))
  (DBG (FORMAT T "CMD-INSERT: ~%"))
  ;; (.)i    Inserts text in the buffer before the current line.  Text
  ;;         is entered in input mode.  The current address is set  to
  ;;         the last line entered.
  (with-addresses (buffer (linum to 1))
    (BUFFER-SAVE-UNDO BUFFER)
    (DO-INSERT BUFFER (1- LINUM)))
  ) ;;CMD-INSERT


(DEFUN CMD-DELETE-LINES (BUFFER FROM TO ARG)
  (declare (ignore arg))
  (DBG (FORMAT T "CMD-DELETE-LINES: ~%"))
  ;; (.,.)d  Deletes the addressed lines from the buffer.  If there is
  ;;         a  line after the deleted range, then the current address
  ;;         is set to this line. Otherwise the current address is set
  ;;         to the line before the deleted range.
  (with-addresses (buffer (curr from 1) (last to 1))
    (BUFFER-SAVE-UNDO BUFFER)
    (SETF (BUFFER-CUT-LINES BUFFER) (do-cut buffer curr last)))
  ) ;;CMD-DELETE-LINES

                  
(DEFUN CMD-COPY (BUFFER FROM TO ARG)
  (declare (ignore arg))
  (DBG (FORMAT T "CMD-COPY: ~%"))
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
    (SETF (BUFFER-CUT-LINES BUFFER)
          (subseq (buffer-lines buffer) curr (1+ last))))
  ) ;;CMD-COPY

 
(DEFUN CMD-PASTE (BUFFER FROM TO ARG)
  (declare (ignore from arg))
  (DBG (FORMAT T "CMD-PASTE: ~%"))
  ;; (.)x    Copies (puts) the contents of the cut buffer to after the
  ;;         addressed  line.   The current address is set to the last
  ;;         line copied.
  (with-addresses (buffer (curr to 0))
    (BUFFER-SAVE-UNDO BUFFER)
    (do-paste buffer curr (copy-seq (buffer-cut-lines buffer))))
  ) ;;CMD-PASTE


(DEFUN CMD-UNDO (BUFFER FROM TO ARG)
  (declare (ignore from to arg))
  (DBG (FORMAT T "CMD-UNDO: ~%"))
  ;; u       Undoes the last command and restores the current  address
  ;;         to  what  it was before the command.  The global commands
  ;;         `g', `G', `v', and `V'.  are treated as a single  command
  ;;         by undo.  `u' is its own inverse.
  (buffer-swap-undo buffer)
  ) ;;CMD-UNDO



(DEFUN CMD-COPY-LINES (BUFFER FROM TO ARG)
  (DBG (FORMAT T "CMD-COPY-LINES: ~%"))
  ;; (.,.)t(.)
  ;;         Copies (i.e., transfers) the addressed lines to after the
  ;;         right-hand destination address, which may be the  address
  ;;         0  (zero).   The  current address is set to the last line
  ;;         copied.
  (with-addresses (buffer (curr from 1) (last to 1) (target arg 0))
    (BUFFER-SAVE-UNDO BUFFER)
    (do-paste buffer arg (subseq (buffer-lines buffer) curr (1+ last))))
  ) ;;CMD-COPY-LINES


(DEFUN CMD-MOVE-LINES (BUFFER FROM TO ARG)
  (DBG (FORMAT T "CMD-MOVE-LINES: ~%"))
  ;; (.,.)m(.)
  ;;         Moves lines in the buffer.  The addressed lines are moved
  ;;         to after the right-hand destination address, which may be
  ;;         the address 0 (zero).  The current address is set to  the
  ;;         last line moved.
  (with-addresses (buffer (curr from 1) (last to 1) (target arg 0))
    (BUFFER-SAVE-UNDO BUFFER)
    (do-paste buffer arg (do-cut buffer curr last)))
  ) ;;CMD-MOVE-LINES


(DEFUN CMD-CHANGE-LINES (BUFFER FROM TO ARG)
  (DBG (FORMAT T "CMD-CHANGE-LINES: ~%"))
  ;; (.,.)c  Changes  lines  in  the  buffer.  The addressed lines are
  ;;         deleted from the buffer, and text is  appended  in  their
  ;;         place.   Text  is  entered  in  input  mode.  The current
  ;;         address is set to last line entered.
  (with-addresses (buffer (curr from 1) (last to 1) (target arg 0))
    (BUFFER-SAVE-UNDO BUFFER)
    (setf (buffer-cut-lines buffer) (do-cut buffer curr last))
    (do-insert buffer curr))
  ) ;;CMD-CHANGE-LINES


(DEFUN CMD-JOIN-LINES (BUFFER FROM TO ARG)
  (declare (ignore arg))
  (DBG (FORMAT T "CMD-JOIN-LINES: ~%"))
  ;; (.,.+1)j
  ;;         Joins  the  addressed  lines.   The  addressed  lines are
  ;;         deleted from the buffer and replaced  by  a  single  line
  ;;         containing their joined text.  The current address is set
  ;;         to the resultant line.
  (with-addresses (buffer (curr from 1) (last to 1))
    (BUFFER-SAVE-UNDO BUFFER)
    (setf (buffer-cut-lines buffer) (do-cut buffer curr last))
    (do-paste buffer (1- curr)
              (apply (function concatenate) 'string
                     (buffer-cut-lines buffer))))
  ) ;;CMD-JOIN-LINES


(DEFUN CMD-MARK (BUFFER FROM TO ARG)
  (declare (ignore from))
  (DBG (FORMAT T "CMD-MARK: ~%"))
  ;; (.)klc  Marks a line with a lower case letter lc.  The  line  can
  ;;         then  be  addressed as 'lc (i.e., a single quote followed
  ;;         by lc ) in subsequent commands.  The mark is not  cleared
  ;;         until the line is deleted or otherwise modified.
  (with-addresses (buffer (curr to 1))
    (BUFFER-SET-MARK BUFFER arg curr))
  ) ;;CMD-MARK


(DEFUN CMD-PRINT-LINE-NUMBER (BUFFER FROM TO ARG)
  (declare (ignore from arg))
  (DBG (FORMAT T "CMD-PRINT-LINE-NUMBER: ~%"))
  ;; ($)=    Prints the line number of the addressed line.
  (with-addresses (buffer (curr to 1))
    (format *terminal-io* "~D~%" curr))
  ) ;;CMD-PRINT-LINE-NUMBER


(DEFUN CMD-SCROLL-LINES (BUFFER FROM TO ARG)
  (declare (ignore from arg))
  (DBG (FORMAT T "CMD-SCROLL-LINES: ~%"))
  ;; (.+1)zn Scrolls n lines at a time starting at addressed line.  If
  ;;         n is not specified, then the current window size is used.
  ;;         The current address is set to the last line printed.
  (with-addresses (buffer (curr to 1))
    ;; TODO: IMPLEMENT SCROLL!
    (DO-PRINT-LINES BUFFER curr (buffer-length buffer)
                    (LAMBDA (LINUM LINE)
                      (DECLARE (IGNORE LINUM))
                      (FORMAT *TERMINAL-IO* "~A~%" LINE))) )
  ) ;;CMD-SCROLL-LINES


(DEFUN CMD-PRINT-LINES (BUFFER FROM TO ARG)
  (declare (ignore arg))
  (DBG (FORMAT T "CMD-PRINT-LINES: ~%"))
  ;;     (.+1)newline
  ;;             Prints the addressed line, and sets the  current  address
  ;;             to that line.
  ;;     (.,.)p  Prints the addressed lines.    If invoked from  a  termi-
  ;;             nal, ed pauses at the end of each page until a newline is
  ;;             entered.  The current address is set  to  the  last  line
  ;;             printed.
  (with-addresses (buffer (curr from 1) (last to 1))
    (DO-PRINT-LINES BUFFER curr last
                    (LAMBDA (LINUM LINE)
                      (DECLARE (IGNORE LINUM))
                      (FORMAT *TERMINAL-IO* "~A~%" LINE))))
  ) ;;CMD-PRINT-LINES


(DEFUN CMD-PRINT-LINES-AND-NUMBERS (BUFFER FROM TO ARG)
  (declare (ignore arg))
  (DBG (FORMAT T "CMD-PRINT-LINES-AND-NUMBERS: ~%"))
  ;;     (.,.)n  Prints the addressed lines along with their line numbers.
  ;;             The current address is set to the last line printed.
  ;;
  (with-addresses (buffer (curr from 1) (last to 1))
    (DO-PRINT-LINES BUFFER curr last
                    (LAMBDA (LINUM LINE)
                      (FORMAT *TERMINAL-IO* "~6D  ~A~%" LINUM LINE))))
  ) ;;CMD-PRINT-LINES-AND-NUMBERS

   
(DEFUN CMD-PRINT-LINES-UNAMBIGUOUSLY (BUFFER FROM TO ARG)
  (declare (ignore arg))
  (DBG (FORMAT T "CMD-PRINT-LINES-UNAMBIGUOUSLY: ~%"))
  ;;     (.,.)l  Prints  the  addressed  lines  unambiguously.  If invoked
  ;;             from a terminal, ed pauses at the end of each page  until
  ;;             a  newline is entered.  The current address is set to the
  ;;             last line printed.
  (with-addresses (buffer (curr from 1) (last to 1))
    (DO-PRINT-LINES
        BUFFER curr last
        (LAMBDA (LINUM LINE)
          (DECLARE (IGNORE LINUM))
          (DO ((I 0 (1+ I))
               (CH))
              ((>= I (LENGTH LINE)) (FORMAT *TERMINAL-IO* "$~%"))
            (SETQ CH (CHAR LINE I))
            (IF (GRAPHIC-CHAR-P CH)
                (format *TERMINAL-IO* "~C" ch)
                (LET ((ASS (ASSOC (CHAR-CODE CH)
                                  '((7 . "a") (8 . "b") (9 . "t")
                                    (10 . "l") (11 . "v") (12 . "f")
                                    (13 . "r")))))
                  (IF ASS
                      (FORMAT *TERMINAL-IO* "\\~A" (CDR ASS))
                      (FORMAT *TERMINAL-IO* "\\~3,'0O" (CHAR-CODE CH)))))))))
  ) ;;CMD-PRINT-LINES-UNAMBIGUOUSLY


(DEFUN CMD-SUBSTITUTE (BUFFER FROM TO ARG)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (DBG (FORMAT T "CMD-SUBSTITUTE: ~%"))
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
  (format *terminal-io* "NOT IMPLEMENTED YET.~%")
  ) ;;CMD-SUBSTITUTE
    
        
(DEFUN CMD-EDIT-MATCHING (BUFFER FROM TO ARG)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (DBG (FORMAT T "CMD-EDIT-MATCHING: ~%"))
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
  (format *terminal-io* "NOT IMPLEMENTED YET.~%")
  ) ;;CMD-EDIT-MATCHING


(DEFUN CMD-EDIT-NOT-MATCHING (BUFFER FROM TO ARG)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (DBG (FORMAT T "CMD-EDIT-NOT-MATCHING: ~%"))
  ;; (1,$)v/re/command-list
  ;;         Applies  command-list  to each of the addressed lines not
  ;;         matching a regular expression re.  This is similar to the
  ;;         `g' command.
  ;;
  (format *terminal-io* "NOT IMPLEMENTED YET.~%")
  ) ;;CMD-EDIT-NOT-MATCHING


(DEFUN CMD-USER-EDIT-MATCHING (BUFFER FROM TO ARG)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (DBG (FORMAT T "CMD-USER-EDIT-MATCHING: ~%"))
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
  (format *terminal-io* "NOT IMPLEMENTED YET.~%")
  ) ;;CMD-USER-EDIT-MATCHING


(DEFUN CMD-USER-EDIT-NOT-MATCHING (BUFFER FROM TO ARG)
  (declare (ignore buffer from to arg)) ;; TODO: implement this function.
  (DBG (FORMAT T "CMD-USER-EDIT-NOT-MATCHING: ~%"))
  ;; (1,$)V/re/
  ;;         Interactively  edits  the  addressed lines not matching a
  ;;         regular expression re.  This is similar to the  `G'  com-
  ;;         mand.
  (format *terminal-io* "NOT IMPLEMENTED YET.~%")
  ) ;;CMD-USER-EDIT-NOT-MATCHING


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
           (values :path arg (probe-file arg))))))
  ) ;;file-or-command-arg



(DEFUN cmd-write-or-append (BUFFER FROM TO ARG mode)
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
         (buffer-set-error buffer "Internal error: FILE-OR-COMMAND-ARG")))))
  ) ;;cmd-write-or-append


(DEFUN CMD-APPEND-FILE (BUFFER FROM TO ARG)
  (DBG (FORMAT T "CMD-APPEND-FILE: ~%"))
  ;; (1,$)W file
  ;;         Appends  the addressed lines to the end of file.  This is
  ;;         similar to the `w' command, expect that the previous con-
  ;;         tents  of  file is not clobbered.  The current address is
  ;;         unchanged.
  (cmd-write-or-append buffer from to arg :append)
  ) ;;CMD-APPEND-FILE


(DEFUN CMD-WRITE-FILE (BUFFER FROM TO ARG)
  (DBG (FORMAT T "CMD-WRITE-FILE: ~%"))
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
  (cmd-write-or-append buffer from to arg :supersede)
  ) ;;CMD-WRITE-FILE


(DEFUN CMD-WRITE-FILE-QUIT (BUFFER FROM TO ARG)
  (DBG (FORMAT T "CMD-WRITE-FILE-QUIT: ~%"))
  ;; (1,$)wq file
  ;;         Writes the addressed lines to file, and then  executes  a
  ;;         `q' command.
  ;;
  (cmd-write-file buffer from to arg)
  (unless (buffer-got-error buffer)
    (cmd-quit buffer from to arg))
  ) ;;CMD-WRITE-FILE-QUIT


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
       (buffer-set-error buffer "Internal error: FILE-OR-COMMAND-ARG"))))
  ) ;;cmd-edit-or-read


(DEFUN CMD-READ-FILE (BUFFER FROM TO ARG)
  (declare (ignore from))
  (DBG (FORMAT T "CMD-READ: ~%"))
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
    (cmd-edit-or-read buffer arg curr))
  ) ;;CMD-READ-FILE


(DEFUN CMD-EDIT-FILE-UNCONDITIONALLY (BUFFER FROM TO ARG)
  (declare (ignore from to))
  (DBG (FORMAT T "CMD-EDIT-FILE-UNCONDITIONALLY: ~%"))
  ;; E file  Edits  file  unconditionally.   This  is similar to the e
  ;;         command, except  that  unwritten  changes  are  discarded
  ;;         without  warning.  The current address is set to the last
  ;;         line read.
  (cmd-edit-or-read buffer arg nil)
  (unless (buffer-got-error buffer)
    (setf (buffer-modified buffer) nil)
    (format *terminal-io* "~D~%" (buffer-current-linum buffer)))
  ) ;;CMD-EDIT-FILE-UNCONDITIONALLY

  
(DEFUN CMD-EDIT-FILE (BUFFER FROM TO ARG)
  (declare (ignore from to))
  (DBG (FORMAT T "CMD-EDIT-FILE: ~%"))
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
                   (cmd-edit-or-read buffer arg nil))
  ) ;;CMD-EDIT-FILE


(DEFUN CMD-SET-DEFAULT-FILENAME (BUFFER FROM TO ARG)
  (declare (ignore from to))
  (DBG (FORMAT T "CMD-SET-DEFAULT-FILENAME: ~%"))
  ;; f file  Sets the default filename to file.  If file is not speci-
  ;;         fied, then the default unescaped filename is printed.
  (when (string/= "" arg)
    (let ((pos (skip-spaces arg)))
      (when (and pos (< 0 pos)) (setq arg (subseq arg pos))))
    (if (or (string= "" arg)
            (handler-case (prog1 nil (probe-file arg)) (error nil t)))
        (buffer-set-error buffer "Invalid filename")
        (setf (buffer-path buffer) arg)))
  (format *terminal-io* "~A~%" (buffer-path buffer))
  ) ;;CMD-SET-DEFAULT-FILENAME


(DEFUN CMD-PRINT-LAST-ERROR (BUFFER FROM TO ARG)
  (declare (ignore from to arg))
  (DBG (FORMAT T "CMD-PRINT-LAST-ERROR: ~%"))
  ;; h       Prints an explanation of the last error.
  (FORMAT *TERMINAL-IO* "~A~%" (buffer-last-error buffer))
  ) ;;CMD-PRINT-LAST-ERROR


(DEFUN CMD-TOGGLE-ERROR-EXPLANATIONS (BUFFER FROM TO ARG)
  (DECLARE (IGNORE FROM TO ARG))
  (DBG (FORMAT T "CMD-TOGGLE-ERROR-EXPLANATIONS: ~%"))
  ;; H       Toggles  the printing of error explanations.  By default,
  ;;         explanations are not printed.  It is recommended that  ed
  ;;         scripts begin with this command to aid in debugging.
  (TOGGLE (buffer-show-errors BUFFER))
  (unless (string= "" (buffer-last-error buffer))
    (format *terminal-io* "~A~%" (buffer-last-error buffer)))
  ) ;;CMD-TOGGLE-ERROR-EXPLANATIONS


(DEFUN CMD-TOGGLE-COMMAND-PROMPT (BUFFER FROM TO ARG)
  (DECLARE (IGNORE FROM TO ARG))
  (DBG (FORMAT T "CMD-TOGGLE-COMMAND-PROMPT: ~%"))
  ;; P       Toggles  the  command prompt on and off.  Unless a prompt
  ;;         was specified by with command-line option -p string,  the
  ;;         command prompt is by default turned off.
  (TOGGLE (BUFFER-show-PROMPT BUFFER))
  ) ;;CMD-TOGGLE-COMMAND-PROMPT


(DEFUN CMD-QUIT (BUFFER FROM TO ARG)
  (declare (ignore from to arg))
  (DBG (FORMAT T "CMD-QUIT: ~%"))
  ;; q       Quits ed.
  (unless-modified buffer
                   (setf (buffer-quit buffer) t))
  ) ;;CMD-QUIT


(DEFUN CMD-QUIT-UNCONDITIONNALY (BUFFER FROM TO ARG)
  (DECLARE (IGNORE FROM TO ARG))
  (DBG (FORMAT T "CMD-QUIT-UNCONDITIONNALY: ~%"))
  ;; Q       Quits  ed unconditionally.  This is similar to the q com-
  ;;         mand, except that unwritten changes are discarded without
  ;;         warning.
  (setf (buffer-quit buffer) t)
  ) ;;CMD-QUIT-UNCONDITIONNALY


(DEFUN CMD-SUBSHELL (BUFFER FROM TO ARG)
  (DECLARE (IGNORE FROM TO))
  (DBG (FORMAT T "CMD-SUBSHELL: ~%"))
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
  (do-command buffer)
  ) ;;CMD-SUBSHELL



(DEFPARAMETER *COMMANDS*
  '( ;;cmd from   to    argument     p   function
    ("t"  :CURR  :CURR  :CURR        T   CMD-COPY-LINES)
    ("m"  :CURR  :CURR  :CURR        T   CMD-MOVE-LINES)
    ("c"  :CURR  :CURR  NIL          T   CMD-CHANGE-LINES)
    ("#"  :CURR  :CURR  NIL          T   CMD-COMMENT)
    ("y"  :CURR  :CURR  NIL          T   CMD-COPY)
    ("d"  :CURR  :CURR  NIL          T   CMD-DELETE-LINES)
    ("p"  :CURR  :CURR  NIL          T   CMD-PRINT-LINES)
    ("n"  :CURR  :CURR  NIL          T   CMD-PRINT-LINES-AND-NUMBERS)
    ("l"  :CURR  :CURR  NIL          T   CMD-PRINT-LINES-UNAMBIGUOUSLY)
    ("s"  :CURR  :CURR  SUBSTITUTION T   CMD-SUBSTITUTE)
    ("j"  :CURR  :NEXT  NIL          T   CMD-JOIN-LINES)
    ("g"  :FIRST :LAST  REGEXP       NIL CMD-EDIT-MATCHING)
    ("v"  :FIRST :LAST  REGEXP       NIL CMD-EDIT-NOT-MATCHING)
    ("G"  :FIRST :LAST  REGEXP       T   CMD-USER-EDIT-MATCHING)
    ("V"  :FIRST :LAST  REGEXP       T   CMD-USER-EDIT-NOT-MATCHING)
    ("W"  :FIRST :LAST  STRING       NIL CMD-APPEND-FILE)
    ("w"  :FIRST :LAST  STRING       NIL CMD-WRITE-FILE)
    ("k"  NIL    :CURR  CHARACTER    T   CMD-MARK)
    ("a"  NIL    :CURR  NIL          T   CMD-APPEND)
    ("i"  NIL    :CURR  NIL          T   CMD-INSERT)
    ("x"  NIL    :CURR  NIL          T   CMD-PASTE)
    ("="  NIL    :LAST  NIL          T   CMD-PRINT-LINE-NUMBER)
    ("r"  NIL    :LAST  STRING       NIL CMD-READ-FILE)
    (NIL  NIL    :NEXT  NIL          NIL CMD-PRINT-LINES)
    ("z"  NIL    :NEXT  NUMBER       T   CMD-SCROLL-LINES)
    ("h"  NIL    NIL    NIL          T   CMD-PRINT-LAST-ERROR)
    ("q"  NIL    NIL    NIL          T   CMD-QUIT)
    ("Q"  NIL    NIL    NIL          T   CMD-QUIT-UNCONDITIONNALY)
    ("P"  NIL    NIL    NIL          T   CMD-TOGGLE-COMMAND-PROMPT)
    ("H"  NIL    NIL    NIL          T   CMD-TOGGLE-ERROR-EXPLANATIONS)
    ("u"  NIL    NIL    NIL          T   CMD-UNDO)
    ("e"  NIL    NIL    STRING       NIL CMD-EDIT-FILE)
    ("E"  NIL    NIL    STRING       NIL CMD-EDIT-FILE-UNCONDITIONALLY)
    ("f"  NIL    NIL    STRING       NIL CMD-SET-DEFAULT-FILENAME)
    ("!"  NIL    NIL    STRING       NIL CMD-SUBSHELL)
    ("wq" :FIRST :LAST  STRING       NIL CMD-WRITE-FILE-QUIT)
    )) ;;*COMMANDS*


(dbg
 (push '("D"  nil    nil    nil          nil cmd-toggle-debug) *commands*)
 (DEFUN cmd-toggle-debug (BUFFER FROM TO ARG)
   ;; !command
   ;;         Executes  command  via  sh(1).  If the first character of
   ;;         command is `!', then it is replaced by text of the previ-
   ;;         ous  `!command'.   ed  does not process command for back-
   ;;         slash (\) escapes.  However, an unescaped `%' is replaced
   ;;         by  the  default  filename.   When the shell returns from
   ;;         execution, a `!'  is printed to the standard output.  The
   ;;         current line is unchanged.
   (declare (ignore buffer from to arg))
   (DBG (FORMAT T "CMD-toggle-debug: ~%"))
   (toggle show-debug)
   ) ;;cmd-toggle-debug
 )



(DEFUN SKIP-SPACES (COMMAND &OPTIONAL (START 0))
  "
RETURN: The index of the next non white space character in command,
        starting from position, or nil if end of string.
"
  (DO ((START START (1+ START)))
      ((OR (>= START (LENGTH COMMAND))
           (CHAR/= (CHARACTER " ") (CHAR COMMAND START)))
       (WHEN (< START (LENGTH COMMAND)) START)))
  ) ;;SKIP-SPACES

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

  
(DEFUN PARSE-ADDRESS (COMMAND POSITION)
  ;;address -->   '.' | '$' | number
  ;;            | '-' | '^' | '-' number | '^' number
  ;;                  | '+' | '+' number | space number
  ;;                  | '/' re '/' | '?' re '?' | '//' | '??'
  ;;                  | "'" lc .
  (declare (integer position))
  (LET ((ADDRESS NIL) (CH))
    (FLET ((PARSE-OPTIONAL-NUMBER
               ()
             (SETQ POSITION (SKIP-SPACES COMMAND (1+ POSITION)))
             (WHEN (AND POSITION
                        (SETQ CH (CHAR COMMAND POSITION))
                        (DIGIT-CHAR-P CH))
               (MULTIPLE-VALUE-BIND (VALUE POS)
                   (PARSE-INTEGER COMMAND :START POSITION :JUNK-ALLOWED T)
                 (WHEN VALUE
                   (SETQ ADDRESS (CONS ADDRESS VALUE)))
                 (SETQ POSITION POS)))))
      (SETQ POSITION (SKIP-SPACES COMMAND POSITION))
      (WHEN POSITION
        (SETQ CH (CHAR COMMAND POSITION))
        (COND
          ((CHAR= CH (CHARACTER ".")) (SETQ ADDRESS :CURR) (INCF POSITION))
          ((CHAR= CH (CHARACTER "$")) (SETQ ADDRESS :LAST) (INCF POSITION))
          ((DIGIT-CHAR-P CH)
           (MULTIPLE-VALUE-BIND (VALUE POS)
               (PARSE-INTEGER COMMAND :START POSITION :JUNK-ALLOWED T)
             (WHEN VALUE
               (SETQ ADDRESS (CONS :LINUM VALUE)))
             (SETQ POSITION POS)))
          ((OR (CHAR= CH (CHARACTER "^")) (CHAR= CH (CHARACTER "-")))
           (SETQ ADDRESS :PREV)
           (PARSE-OPTIONAL-NUMBER))
          ((OR (CHAR= CH (CHARACTER " ")) (CHAR= CH (CHARACTER "+")))
           (SETQ ADDRESS :NEXT)
           (PARSE-OPTIONAL-NUMBER))
          ((OR (CHAR= CH (CHARACTER "/")) (CHAR= CH (CHARACTER "?")))
           ;; TODO: regexp
           ;; eat regexp:
           (SETQ ADDRESS
                 (CONS :REGEXP 
                       (DO ((TERMINATOR CH)
                            (END POSITION (1+ END)))
                           ((OR (>= END (LENGTH COMMAND))
                                (CHAR= TERMINATOR (CHAR COMMAND END)))
                            (PROG1 (SUBSEQ COMMAND POSITION (1+ END))
                              (SETQ POSITION (1+ END))))
                         (declare (integer end))))))
          ((CHAR= CH (CHARACTER "'"))
           (INCF POSITION)
           ;; TODO: when there is an error here it's: "Invalid mark character"
           ;; TODO: not: "Invalid address" !
           (WHEN (< POSITION (LENGTH COMMAND))
             (SETQ ADDRESS (CONS :MARK (CHAR COMMAND POSITION)))
             (INCF POSITION)))
          )))
    (VALUES ADDRESS POSITION))
  ) ;;PARSE-ADDRESS


(DEFUN PARSE-AND-RUN-COMMAND (BUFFER COMMAND)
  (LET ((POSITION (SKIP-SPACES COMMAND))
        (CMD) (CH) (FROM) (TO) (ARG) (PRINT NIL))
    (buffer-clear-error buffer)
    (MACROLET ((SET-ERROR (MESSAGE) `(buffer-set-error buffer ,message))
               (GOT-ERROR () `(buffer-got-ERROR buffer)))
      (WHEN POSITION
        (SETQ CH (CHAR COMMAND POSITION))
        (COND
          ((OR (CHAR= (CHARACTER ",") CH)  (CHAR= (CHARACTER "%") CH))
           (SETQ FROM :FIRST TO :LAST)
           (SETQ POSITION (SKIP-SPACES COMMAND (1+ POSITION))))
          ((CHAR= (CHARACTER ";") CH)
           (SETQ FROM :CURR  TO :LAST)
           (SETQ POSITION (SKIP-SPACES COMMAND (1+ POSITION))))
          ((NOT (ALPHA-CHAR-P CH))
           (MULTIPLE-VALUE-SETQ (TO POSITION) (PARSE-ADDRESS COMMAND POSITION))
           (IF (EQ TO :ERROR)
               (SET-ERROR "Invalid address")
               (PROGN
                 (SETQ POSITION (SKIP-SPACES COMMAND POSITION))
                 (WHEN POSITION
                   (SETQ CH (CHAR COMMAND POSITION))
                   (WHEN (CHAR= (CHARACTER ",") CH)
                     (SETQ FROM TO)
                     (MULTIPLE-VALUE-SETQ (TO POSITION)
                       (PARSE-ADDRESS COMMAND (1+ POSITION)))
                     (IF (EQ TO :ERROR)
                         (SET-ERROR "Invalid address")
                         (SETQ POSITION (SKIP-SPACES COMMAND POSITION))))))))))
      (DBG (FORMAT T "PARC: from= ~S to= ~S position= ~S got-error= ~S~%"
                   FROM TO POSITION (GOT-ERROR)))
      (UNLESS (GOT-ERROR)
        (IF (NULL POSITION)
            (SETQ CMD (ASSOC NIL *COMMANDS*))
            (SETQ CMD (ASSOC (SUBSEQ COMMAND POSITION (1+ POSITION)) *COMMANDS*
                             :TEST (FUNCTION STRING=))
                  POSITION (1+ POSITION)))
        (DBG (FORMAT T "PARC: command key= ~S ~%      cmd= ~S~%"
                     (WHEN POSITION (SUBSEQ COMMAND (1- POSITION) POSITION))
                     (nconc (butlast CMD)
                            (list (symbol-name (car (last cmd)))))))
        (LET ((DEFR (SECOND CMD))
              (DETO (THIRD  CMD))
              (ARGK (FOURTH CMD))
              (ACCP (FIFTH  CMD))
              (CMDF (SIXTH  CMD)))
          (UNLESS FROM (SETQ FROM DEFR))
          (UNLESS TO   (SETQ TO   DETO))
          (CASE ARGK
            ((NIL))
            ((CHARACTER)
             (IF (AND POSITION (< POSITION (LENGTH COMMAND))
                      (ALPHA-CHAR-P (CHAR COMMAND POSITION))
                      (LOWER-CASE-P (CHAR COMMAND POSITION)))
                 (SETF ARG (CHAR COMMAND POSITION))
                 (SET-ERROR "Invalid mark character")))
            ((NUMBER)
             (WHEN POSITION
               (MULTIPLE-VALUE-SETQ (ARG POSITION)
                 (PARSE-INTEGER COMMAND :START POSITION :JUNK-ALLOWED T))
               (UNLESS ARG
                 (SET-ERROR "Invalid address"))))
            ((STRING)
             (WHEN POSITION
               (SETQ ARG (SUBSEQ COMMAND POSITION)
                     POSITION (LENGTH COMMAND))))
            ((REGEXP)
             )
            ((SUBSTITUTION)
             )
            ((:CURR)
             (WHEN POSITION
               (MULTIPLE-VALUE-SETQ (ARG POSITION)
                 (PARSE-ADDRESS COMMAND POSITION))
               (COND
                 ((EQ ARG :ERROR)  (SET-ERROR "Invalid address"))
                 ((NULL ARG) (SETQ ARG :CURR)))))
            (OTHERWISE
             (SET-ERROR "Internal error: *command* table.")))
          (DBG (FORMAT T "PARC: from= ~S to= ~S position= ~S got-error= ~S~%"
                       FROM TO POSITION (GOT-ERROR))
               (FORMAT T "      arg= ~S ~%" ARG))
          (UNLESS (GOT-ERROR)
            (IF (AND ACCP POSITION (< POSITION (LENGTH COMMAND))
                     (CHAR= (character "p") (CHAR COMMAND POSITION)))
                (SETQ  PRINT T)
                (WHEN (AND POSITION (SKIP-SPACES COMMAND POSITION))
                  (SET-ERROR "Invalid command suffix")))
            (UNLESS (GOT-ERROR)
              (DBG (FORMAT T "PARC: calling (~A ~S ~S ~S ~S ~S)~%"
                           CMDF "BUFFER" FROM TO ARG PRINT))
              (setf (buffer-print buffer) print)
              (FUNCALL CMDF BUFFER FROM TO ARG)))))))
  ) ;;PARSE-AND-RUN-COMMAND


(DEFUN EDIT (BUFFER)
  (FORMAT *TERMINAL-IO* "~&")
  (setf (buffer-quit buffer) nil)
  (FORMAT *TERMINAL-IO* "~D~%" (BUFFER-LENGTH BUFFER))
  (LOOP
     (LET ((COMMAND (READ-LINE *TERMINAL-IO* NIL NIL)))
       (UNLESS COMMAND (RETURN))
       (DBG (FORMAT T "EDIT: read command ~S~%" COMMAND))
       (setf (buffer-print buffer) nil)
       (buffer-clear-error buffer)
       (PARSE-AND-RUN-COMMAND BUFFER COMMAND)
       (DBG (FORMAT T "EDIT: parc returned (~S ~S ~S)~%"
                    (buffer-print buffer)
                    (buffer-got-error buffer)
                    (buffer-QUIT buffer)))
       (IF (buffer-got-error buffer)
           (IF (buffer-show-errors buffer)
               (FORMAT *TERMINAL-IO* "~A~%" (buffer-last-error buffer))
               (FORMAT *TERMINAL-IO* "?~%"))
           (WHEN (buffer-print buffer)
             (let ((current (BUFFER-CURRENT-linum buffer)))
               (if (LIMIT current 1 (buffer-length buffer))
                   (format *terminal-io* "~A~%" (buffer-nth-line buffer current))
                   (progn
                     (buffer-set-error buffer "Invalid address")
                     (IF (buffer-show-errors buffer)
                         (FORMAT *TERMINAL-IO* "~A~%" (buffer-last-error buffer))
                         (FORMAT *TERMINAL-IO* "?~%")))))))
       (IF (buffer-quit buffer)
           (RETURN)
           (WHEN (BUFFER-show-PROMPT BUFFER)
             (FORMAT *TERMINAL-IO* "~A~%" (buffer-PROMPT-string BUFFER))))))
  ) ;;EDIT


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


(DEFVAR *CURRENT-BUFFER* (MAKE-BUFFER))


(DEFUN ED (&OPTIONAL X)
  (COND
    ((NULL X)
     (EDIT *CURRENT-BUFFER*))
    ((OR (PATHNAMEP X) (STRINGP X))
     (SETQ *CURRENT-BUFFER* (BUFFER-READ X))
     (EDIT *CURRENT-BUFFER*))
    ((SYMBOLP X)
     (LET ((FLE (FUNCTION-LAMBDA-EXPRESSION (FDEFINITION X))))
       (SETQ *CURRENT-BUFFER*
             (BUFFER-FROM-STRING
              (IF (EQ 'LAMBDA (CAR FLE))
                  (FORMAT NIL "~S~%"  (CONS 'DEFUN (CONS X (CDR FLE))))
                  (FORMAT NIL "~S is not a function.~%" x))))
       (EDIT *CURRENT-BUFFER*)))
    ;; TODO: If x is a function name, ...
    (T
     (ERROR "Invalid argument ~S." X)))
  ) ;;ED




(defvar test-text
  "One little boy and
Two little girls, climbed up a
Tree near the sky.
Four bird lander on that tree.
Five eggs were layed each on one child head.
") ;;test-text

;; (progn (ext:cd "/home/pascal/src/lisp/encours/") (load "ed.lisp"))

;;;; ed.lisp                          --                     --          ;;;;
