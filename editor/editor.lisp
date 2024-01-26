;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               editor.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A screen editor.   clispmacs
;;;;
;;;;    About emacs in CL:
;;;;    http://clocc.cvs.sourceforge.net/clocc/clocc/src/cllib/elisp.lisp?view=markup
;;;;    http://common-lisp.net/viewvc/phemlock/phemlock/src/elisp/hemlock-shims.lisp?revision=1.1.1.1&view=markup&sortby=author&pathrev=HEAD
;;;;    http://www.emacswiki.org/emacs/EmacsCommonLisp
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-04-30 <PJB> Corrected a couple of little bugs. Added BUGS entries.
;;;;    2007-09-11 <PJB> Added the language feature, and some docstrings.
;;;;    2006-07-10 <PJB> Created.
;;;;BUGS
;;;;    - When entering the mini-buffer (prompt-window?), the status
;;;;      line above disappears.
;;;;    - implement multi-windows (C-x 2, C-x 3, C-x 1)
;;;;    - redisplay: we can see the cursor moving over the terminal
;;;;    - redisplay: implement minimal updating.
;;;;    - implement pathname and buffer-name completion.
;;;;    - doesn't take into account changes of terminal size (SIGWINCH).
;;;;    - doesn't handle TAB completion (or at least ignore TAB).
;;;;    - breaking into the debugger (eg. on C-x C-e) is not handled in the editor,
;;;;      and some restart may exit from the editor.
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2006 - 2016
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
(in-package "COM.INFORMATIMAGO.EDITOR")

(defvar *debug-on-error*    nil
  "Non-nil means enter debugger if a unhandled error is signaled.")

(defvar *debug-on-quit*     nil
  "Non-nil means enter debugger if a unhandled quit is signaled (C-g, for example).")

(defvar *debug-on-message*  nil
  "If non-nil, debug if a message matching this regexp is displayed.")

(defvar *log*            nil "Debugging stream.")

(defvar *frame-list*     '() "The list of frames.")
(defvar *current-frame*  nil "The current frame.")
(defvar *buffer-list*    '() "The list of buffers")
(defvar *current-window* nil "The current window.")
(defvar *scratch-buffer-default-contents*
  ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

"
  "The default contents for the *scratch* buffer.")

(defvar *last-command-char* nil
  "Last input event that was part of a command.")

(defvar *this-command* nil
  "The command now being executed.
The command can set this variable; whatever is put here
will be in `last-command' during the following command.")

(defvar *last-command* nil
  "The last command executed.
Normally a symbol with a function definition, but can be whatever was found
in the keymap, or whatever the variable `this-command' was set to by that
command.

The value `mode-exit' is special; it means that the previous command
read an event that told it to exit, and it did so and unread that event.
In other words, the present command is the event that made the previous
command exit.

The value `kill-region' is special; it means that the previous command
was a kill command.")

(defvar *prefix-arg* nil
  "The value of the prefix argument for the next editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.

You cannot examine this variable to find the argument for this command
since it has been set to nil by the time you can look.
Instead, you should use the variable `current-prefix-arg', although
normally commands can get this prefix argument with (interactive \"P\").")

(defvar *current-prefix-arg* nil
  "The value of the prefix argument for this editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.
This is what `(interactive \"P\")' returns.")



(defvar *kill-whole-line* nil
  "*If non-nil, `kill-line' with no arg at beg of line kills the whole line.")

(defvar *yank* nil)



#-mocl
(declaim (ftype function message))
(defun error (datum &rest arguments)
  (cond
    (*debug-on-error* (apply (function cl:error) datum arguments))
    ((stringp datum) (message datum))
    (t (message "~A" (apply (function format) datum arguments)))))


(defmacro with-current-window (window &body body)
  `(let ((*current-window* ,window))
     ,@body))


(defun read-something (prompt other-args validatef postf)
  (loop
     :for something = (apply (function read-from-minibuffer) prompt other-args)
     :then (apply (function read-from-minibuffer)
                  prompt :initial-contents something other-args)
     :do (if (funcall validatef something)
             (return (funcall postf something))
             (with-current-window (frame-mini-window *current-frame*)
               (insert " [No match]")
               ;; TODO: Replace this sleep with an interuptible wait!
               (redisplay) (sleep 1)))))


(defun read-number-from-minibuffer (prompt)
  (read-something prompt nil
                  (lambda (something)
                    (with-input-from-string
                        (*standard-output* something)
                      (let* ((*read-eval* nil))
                        (numberp (read)))))
                  (lambda (something)
                    (with-input-from-string
                        (*standard-output* something)
                      (let* ((*read-eval* nil))
                        (read))))))


(defun nsubseq (sequence start &optional (end nil))
  "Same as CL:SUBSEQ, but with vectors, use a displaced array instead of
copying the vector."
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
      (subseq sequence start end)))

(defmacro in-order (start end)
  `(unless (< ,start ,end) (rotatef ,start ,end)))

(defun interactive-item (item)
  (loop
     :for start :from 0
     :for prompt = (nsubseq item (1+ start))
     :while (and (< start (length item))
                 (or (char= #\* (aref item start))
                     (char= #\@ (aref item start))))
     :do (when (char= #\* (aref item start))
           (when (buffer-read-only-p (current-buffer))
             (error "Buffer is read-only ~S" (current-buffer))))
     :finally
     (return
       (when (< start (length item))
         (case (aref item start)
           ((#\a)                       ; read a function name
            (read-something prompt nil
                            (lambda (name) (fboundp (find-symbol
                                                     (string-upcase name))))
                            (compose (function find-symbol)
                                     (function string-upcase))))

           ((#\b)                       ; read an existing buffer
            (read-something prompt nil
                            (function get-buffer)
                            (function identity)))

           ((#\B)                       ; read a buffer name
            (read-from-minibuffer prompt))
           ((#\c)                 ; read a character - no input method
            #\a
            )
           ((#\C)                       ; read a command name
            (read-something prompt nil
                            (lambda (name)
                              (and (fboundp (find-symbol
                                             (string-upcase name)))
                                   (interactivep (find-symbol
                                                  (string-upcase name)))))
                            (compose (function find-symbol)
                                     (function string-upcase))))
           ((#\d)                  ; value of point as number - no i/o
            (point))
           ((#\D)                       ; Directory name.
            (read-something prompt nil
                            (lambda (name)
                              #+clisp
                              (ext:probe-directory name)
                              #-clisp
                              (progn (warn "How to probe for directory ~S in ~S" name (lisp-implementation-type)) t))
                            (function identity)))
           ((#\e) ; Parametrized event (i.e., one that's a list) that invoked this command. If used more than once, the Nth `e' returns the Nth parameterized event. This skips events that are integers or symbols.
            )
           ((#\f)                       ; Existing file name.
            (read-something prompt nil
                            (function probe-file)
                            (function identity)))
           ((#\F)                    ; Possibly nonexistent file name.
            (read-something prompt nil
                            (function pathname)
                            (function identity)))
           ((#\G) ; Possibly nonexistent file name, defaulting to just directory name.
            (read-something prompt nil
                            (function pathname)
                            (function identity)))
           ((#\i)        ; Ignored, i.e. always nil.  Does not do I/O.
            nil)
           ((#\k) ; Key sequence (downcase the last event if needed to get a definition).
            )
           ((#\K) ; Key sequence to be redefined (do not downcase the last event).
            )
           ((#\m)         ; Value of mark as number.  Does not do I/O.
            (mark))
           ((#\M)    ; Any string.  Inherits the current input method.
            (read-from-minibuffer prompt))
           ((#\n)                      ; Number read using minibuffer.
            (read-number-from-minibuffer prompt))
           ((#\N)      ; Raw prefix arg, or if none, do like code `n'.
            (or *current-prefix-arg*
                (read-number-from-minibuffer prompt)))
           ((#\p)  ; Prefix arg converted to number.  Does not do I/O.
            (cond
              ((null     *current-prefix-arg*) 1)
              ((integerp *current-prefix-arg*) *current-prefix-arg*)
              ((listp    *current-prefix-arg*) (first *current-prefix-arg*))
              ((eq '-    *current-prefix-arg*) -1)
              (t                               0)))
           ((#\P)          ; Prefix arg in raw form.  Does not do I/O.
            *current-prefix-arg*)
           ((#\r) ; Region: point and mark as 2 numeric args, smallest first.  Does no I/O.
            (let ((start (point))
                  (end   (or (mark) (point))))
              (in-order start end)
              (list     start end)))
           ((#\s) ; Any string.  Does not inherit the current input method.
            (read-from-minibuffer prompt))
           ((#\S)                       ; Any symbol.
            (read-something prompt nil
                            (constantly t)
                            (compose (function intern)
                                     (function string-upcase))))
           ((#\U) ; Mouse up event discarded by a previous k or K argument.
            nil)
           ((#\v)     ; Variable name: symbol that is user-variable-p.
            )
           ((#\x)            ; Lisp expression read but not evaluated.
            (read-something prompt nil
                            (lambda (something)
                              ;; TODO: more sophisticated test (EOS).
                              (handler-case
                                  (progn (read-from-string something) t)
                                (cl:error () nil)))
                            (function read-from-string)))
           ((#\X)                ; Lisp expression read and evaluated.
            (read-something prompt nil
                            (lambda (something)
                              ;; TODO: more sophisticated test (EOS).
                              (handler-case
                                  (progn (read-from-string something) t)
                                (cl:error () nil)))
                            (lambda (something) (eval (read-from-string something)))))
           ((#\z)                       ; Coding system.
            )
           ((#\Z)               ; Coding system, nil if no prefix arg.
            )
           (otherwise (error "Bad interactive specifier ~S" (aref item start))))))))



(defun call-interactively (fun)
  (let ((interspec (second (interactivep fun))))
    (apply fun
           (cond
             ((null interspec) '())
             ((stringp interspec)
              (mapcar (function interactive-item)
                      (split-sequence #\newline interspec)))
             (t
              (eval interspec))))))



;;;---------------------------------------------------------------------
;;; Keymaps
;;;---------------------------------------------------------------------

(defparameter *special-keys*
  '(
    :F1 :f2 :F3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :F12
    :INSERT
    :DELETE
    :HOME
    :END
    :CENTER
    :PGUP
    :PGDN
    :LEFT :RIGHT :UP :DOWN))



(defstruct (keymap (:constructor %make-keymap)
                   (:conc-name %keymap-)
                   (:predicate keymapp))
  table)

(defun make-keymap ()
  "Returns a new empty keymap."
  (let ((keymap (%make-keymap :table (make-hash-table :test (function equal)))))
    (keymap-bind-key keymap '(:control #\g) 'keyboard-quit)
    keymap))


(defun keymap-map (function keymap)
  "Calls FUNCTION with each (KEY BINDING) couples in the KEYMAP."
  (maphash function (%keymap-table keymap)))


(defun keymap-copy (keymap &key shallow)
  "Returns a copy of the KEYMAP.
If SHALLOW is true, then subkeymaps are not copied too."
  (let ((copy (make-keymap)))
    (keymap-map (lambda (key binding)
                  (keymap-bind-key copy key
                                   (if (and (not shallow) (keymapp binding))
                                       (keymap-copy binding)
                                       binding)))
                keymap)
    copy))


(defun keymap-bind-key (keymap key binding)
  "Binds a KEY to a BINDING in the KEYMAP.
KEY:        must be either a character, one of the *SPECIAL-KEYS*,
            or a list ([:control] [:meta] KEY)
BINDING:    must be either a symbol (naming a command),
            a command function,
            or another keymap."
  (setf (gethash key (%keymap-table keymap)) binding))


(defun keymap-binding  (keymap &rest keys)
  "Returns the binding for the KEYS, and the remaining keys."
  (let ((binding (if (atom (first keys))
                     (or (gethash (first keys)        (%keymap-table keymap))
                         (gethash (list (first keys)) (%keymap-table keymap)))
                     (gethash (first keys) (%keymap-table keymap)))))
    (if (and (keymapp binding) (rest keys))
        (apply (function keymap-binding) binding (rest keys))
        (values binding (rest keys)))))



(defparameter *default-keymap*
  (let ((def-map (make-keymap))
        (c-x-map (make-keymap))
        (c-h-map (make-keymap))
        (fn-map  (make-keymap)))
    (loop

      ;; Note: all printable iso-8859-1 characters, which is a subset
      ;; of unicode between 32 and 255, excluding control codes.
      ;; This may ensure only base-char are used.

      :for key :across #.(concatenate 'string
                                      " !\"#$%&'()*+,-./0123456789:;<=>?"
                                      "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                                      "`abcdefghijklmnopqrstuvwxyz{|}~"
                                      " ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿"
                                      "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß"
                                      "àáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")
      :do (keymap-bind-key def-map key 'self-insert-command))
    (keymap-bind-key def-map #\return        'new-line)
    (keymap-bind-key def-map #\newline       'new-line)
    (keymap-bind-key def-map #\tab           'self-insert-command)
    (keymap-bind-key def-map #\Rubout        'delete-backward-char)
    (keymap-bind-key def-map '(:control #\d) 'delete-char)
    (loop
      :for key :across "0123456789"
      :do (keymap-bind-key def-map (list :meta key) 'digit-argument))
    (keymap-bind-key def-map '(:meta    #\-) 'negative-argument)
    (keymap-bind-key def-map '(:meta    #\x) 'execute-extended-command)
    (keymap-bind-key def-map '(:meta    #\:) 'eval-expression)
    (keymap-bind-key def-map '(:control #\@) 'set-mark)
    (keymap-bind-key def-map '(:control #\-) 'negative-argument)
    (keymap-bind-key def-map '(:control #\u) 'universal-argument)
    (keymap-bind-key def-map '(:control #\l) 'redisplay)
    (keymap-bind-key def-map '(:control #\a) 'beginning-of-line)
    (keymap-bind-key def-map '(:control #\e) 'end-of-line)
    (keymap-bind-key def-map '(:control #\f) 'forward-char)
    (keymap-bind-key def-map '(:control #\b) 'backward-char)
    (keymap-bind-key def-map '(:control #\n) 'next-line)
    (keymap-bind-key def-map '(:control #\p) 'previous-line)
    (keymap-bind-key def-map '(:control #\k) 'kill-line)
    (keymap-bind-key def-map '(:control #\u) 'universal-argument)
    (keymap-bind-key def-map '(:control #\y) 'yank)
    (keymap-bind-key def-map '(:control :meta #\f) 'forward-sexp)
    (keymap-bind-key def-map '(:control :meta #\b) 'backward-sexp)
    (keymap-bind-key def-map '(:control :meta #\k) 'kill-sexp)
    (keymap-bind-key def-map '(:control #\v) 'scroll-up)
    (keymap-bind-key def-map '(:meta    #\v) 'scroll-down)

    (keymap-bind-key def-map '(:control #\h) c-h-map)
    (keymap-bind-key c-h-map '#\h            'view-hello-file)
    (keymap-bind-key c-h-map '(:control #\h) 'help-for-help)
    (keymap-bind-key c-h-map '#\f            'describe-function)
    (keymap-bind-key c-h-map '#\v            'describe-variable)
    (keymap-bind-key c-h-map '#\k            'describe-key)
    (keymap-bind-key c-h-map '#\w            'where-is)

    (keymap-bind-key def-map '(:control #\x) c-x-map)
    (keymap-bind-key c-x-map '#\b            'switch-to-buffer)
    (keymap-bind-key c-x-map '#\f            'find-file)
    (keymap-bind-key c-x-map '#\k            'kill-buffer)
    (keymap-bind-key c-x-map '(:control #\s) 'save-buffer)
    (keymap-bind-key c-x-map '(:control #\b) 'list-buffers)
    (keymap-bind-key c-x-map '(:control #\c) 'editor-quit)
    (keymap-bind-key c-x-map '(:control #\d) 'my-debug)
    (keymap-bind-key c-x-map  #\D            'cl-debugger)
    (keymap-bind-key c-x-map '(:control #\e) 'eval-last-sexp)
    (keymap-bind-key c-x-map '(:control #\f) 'find-file)

    (keymap-bind-key def-map '(:control :meta #\[) fn-map) ; temporary kludge
    (keymap-bind-key def-map '(:meta #\[)          fn-map)
    (keymap-bind-key fn-map  '#\A            'previous-line)
    (keymap-bind-key fn-map  '#\B            'next-line)
    (keymap-bind-key fn-map  '#\C            'forward-char)
    (keymap-bind-key fn-map  '#\D            'backward-char)

    (keymap-bind-key fn-map  '#\c            'previous-line) ; ??
    (keymap-bind-key fn-map  '#\e            'forward-char)  ; ??
    (keymap-bind-key fn-map  '#\d            'backward-char) ; ??

    def-map))

(defvar *keymap* nil)


#-clisp
(defun make-xterm-io-stream (&key display geometry)
  (error "(~S ~S ~S) Not implemented on ~A"
         'make-xterm-io-stream display geometry (lisp-implementation-type)))

(defvar *old-terminal-io*)
(defun cl-debugger ()
  (declare (interactive))
  (let* ((io                (make-xterm-io-stream :geometry "100x24+0+0"))
         (*old-terminal-io* *terminal-io*)
         (*debug-io*        io)
         (*terminal-io*     io))
    (unwind-protect
         (invoke-debugger
          (make-condition 'simple-error
                          :format-control "Debugger invoked interactively"))
      (close io))))


;;;---------------------------------------------------------------------
;;; Help commands
;;;---------------------------------------------------------------------

(defun view-hello-file ()
  (declare (interactive))
  (switch-to-buffer (get-buffer-create "*Hello*"))
  (erase-buffer)
  (insert "Hello!~%")
  (insert "Salut!~%")
  (insert "¡Hola!~%"))


(defun help-for-help ()
  (declare (interactive))
  (switch-to-buffer (get-buffer-create "*Help*"))
  (erase-buffer)
  (insert "
You have typed C-h, the help character.  Type a Help option:
(Use SPC or DEL to scroll through this text.  Type q to exit the Help command.)

a PATTERN   Show commands whose name matches the PATTERN (a list of words
              or a regexp).  See also the `apropos' command.
b           Display all key bindings.
c KEYS      Display the command name run by the given key sequence.
C CODING    Describe the given coding system, or RET for current ones.
d PATTERN   Show a list of functions, variables, and other items whose
              documentation matches the PATTERN (a list of words or a regexp).
e           Go to the *Messages* buffer which logs echo-area messages.
f FUNCTION  Display documentation for the given function.
F COMMAND   Show the on-line manual's section that describes the command.
g           Display information about the GNU project.
h           Display the HELLO file which illustrates various scripts.
i           Start the Info documentation reader: read on-line manuals.
I METHOD    Describe a specific input method, or RET for current.
k KEYS      Display the full documentation for the key sequence.
K KEYS      Show the on-line manual's section for the command bound to KEYS.
l           Show last 300 input keystrokes (lossage).
L LANG-ENV  Describes a specific language environment, or RET for current.
m           Display documentation of current minor modes and current major mode,
              including their special commands.
n           Display news of recent Emacs changes.
p TOPIC     Find packages matching a given topic keyword.
r           Display the Emacs manual in Info mode.
s           Display contents of current syntax table, plus explanations.
S SYMBOL    Show the section for the given symbol in the on-line manual
              for the programming language used in this buffer.
t           Start the Emacs learn-by-doing tutorial.
v VARIABLE  Display the given variable's documentation and value.
w COMMAND   Display which keystrokes invoke the given command (where-is).
.           Display any available local help at point in the echo area.

C-a         Information about Emacs.
C-c         Emacs copying permission (GNU General Public License).
C-d         Instructions for debugging GNU Emacs.
C-e         External packages and information about Emacs.
C-f         Emacs FAQ.
C-m         How to order printed Emacs manuals.
C-n         News of recent Emacs changes.
C-o         Emacs ordering and distribution information.
C-p         Info about known Emacs problems.
C-t         Emacs TODO list.
C-w         Information on absence of warranty for GNU Emacs.
"))


(defun where-is (cname)
  (declare (interactive "CWhere is command: "))
  (switch-to-buffer (get-buffer-create "*Help*"))
  (erase-buffer)
  (insert "~S is a command~%" cname))

(defun describe-function (fname)
  (declare (interactive "aDescribe Function: "))
  (switch-to-buffer (get-buffer-create "*Help*"))
  (erase-buffer)
  (insert "~S is a function~%" fname))

(defun describe-variable (vname)
  (declare (interactive "vDescribe Variable: "))
  (switch-to-buffer (get-buffer-create "*Help*"))
  (erase-buffer)
  (insert "~S is a variable~%" vname))

(defun describe-key (kname)
  (declare (interactive "kDescribe Key: "))
  (switch-to-buffer (get-buffer-create "*Help*"))
  (erase-buffer)
  (insert "~S is a key sequence~%" kname))



;;;---------------------------------------------------------------------
;;; Files
;;;---------------------------------------------------------------------

(defclass file ()
  ((pathname :initarg :pathname :accessor file-pathname)))

(defgeneric file-name (file))
(defgeneric file-contents (file))
(defgeneric (setf file-contents) (new-contents file))

(defmethod file-name ((self file))
  (file-namestring (file-pathname self)))


(defmethod (setf file-contents) (new-contents (self file))
  (ensure-directories-exist (file-pathname self))
  (with-open-file (out (file-pathname self)
                      :direction :output
                      :external-format :default
                      :if-does-not-exist :create
                      :if-exists :supersede)
    (write-sequence new-contents out)))


(defmethod file-contents ((self file))
  (with-open-file (in (file-pathname self)
                      :direction :input
                      :external-format :default
                      :if-does-not-exist nil)
    (if in
        (let* ((busize (or (ignore-errors (file-length in)) 4096))
               (eltype (stream-element-type in))
               (initel (if (subtypep eltype 'integer) 0 #\Space))
               (buffer (make-array busize
                                   :element-type eltype
                                   :initial-element initel
                                   :adjustable t :fill-pointer t))
               (start 0)
               (max-extend 65536))
          (loop
             (let ((end (read-sequence buffer in :start start)))
               (when (< end busize)
                 ;; we got eof, or have read enough
                 (setf (fill-pointer buffer) end)
                 (return-from file-contents (copy-seq buffer)))
               ;; no eof; extend the buffer
               (setf busize
                     (if  (<= (* 2 busize) max-extend)
                          (* 2 busize)
                          (+ busize max-extend))
                     start end))
             (adjust-array buffer busize
                           :initial-element initel
                           :fill-pointer t)))
        "")))



;;;---------------------------------------------------------------------
;;; Buffers
;;;---------------------------------------------------------------------


(defun compose (f &rest others)
  (if (null others)
      f
      (lambda (x)
        (funcall f (funcall (apply (function compose) others) x)))))

;; (defun buffer-list ()
;;   (mapcar (function window-buffer)
;;           (apply (function append)
;;                  (mapcar(function frame-window-list) *frame-list*))))




(defmacro with-buffer (buffer &body body)
  `(with-current-window (make-instance 'context :buffer (get-buffer ,buffer))
     (unwind-protect
          (progn ,@body)
       (marker-delete (context-point *current-window*))
       (when (context-mark *current-window*)
         (marker-delete (context-mark *current-window*))))))



;; [window]-1--------1-[context]-*------/-1-[buffer]-1--------*-[marker]
;;     |                   |                                        |
;;     2+                  +-0/1----------------------------------2-+
;;     |
;;     +-----------1-[frame]


;; BUFFERS contains the text and can edit it (insert/delete).
;; A BUFFER can have MARKERS.
;; A BUFFER can be saved to or loaded from a FILE.

;; MARKERS are positions in the text of a BUFFER that move along when
;; text is inserted or deleted before the MARKER position.
;; A MARKER has a gravity toward the begin or the end of the BUFFER,
;; which is taken into account when the insertion or deletion involves
;; exactly the position of the MARKER.

;; Editing CONTEXTs held the current editing point and the mark, both
;; MARKERs of the BUFFER of the CONTEXT.

;; WINDOWS display part of the contents of a BUFFER, thru an editing
;; CONTEXT.

;; We need these three objects to allow the following features:
;; - When there are several WINDOWS open on the same BUFFER, each
;;   WINDOW show a different point position and editing thru the
;;   different WINDOWS happens at the different point in the BUFFER.
;; - A WINDOW can display different BUFFERs at different times
;;   (SWITCH-TO-BUFFER).
;; - When a BUFFER is not visible in any WINDOW, it keeps a CONTEXT
;;   that shows again when we switch back to it in a WINDOW.
;;   (note: GNUemacs saves only one CONTEXT).

;; FRAMES are terminals or GUI windows.
;; They may display several editor WINDOWS.
;; There is usually a mini-window at the bottom of the frame
;; that shows the contents of the mini-buffer.


(defclass buffer ()
  ((name          :accessor buffer-name        :initarg :name)
   (lines         :accessor buffer-lines       :type dll)
   (markers       :accessor buffer-markers     :initform '())
   (mark          :accessor buffer-saved-mark  :initarg :mark  :initform nil
                  :documentation "Saved mark in the buffer.")
   (point         :accessor buffer-saved-point :initarg :point :initform nil
                  :documentation "Saved insertion point in the buffer.")
   (read-only-p   :accessor buffer-read-only-p :initform nil)
   (changed-p     :accessor buffer-changed-p   :initform nil)
   (file          :accessor buffer-file        :initform nil)))

(defclass marker ()
  ((buffer   :accessor marker-buffer  :initarg :buffer
             :accessor marker-valid-p)
   (point    :accessor marker-point   :initarg :point)
   (gravity  :accessor marker-gravity :initarg :gravity :initform :end
             :type (member :begin :end))))

(defclass frame ()
  ((title         :accessor frame-title  :initarg :title :initform "")
   (window-list   :accessor frame-window-list :initform '())
   (mini-window   :accessor frame-mini-window)
   (mini-buffer   :accessor frame-mini-buffer)
   (prompt-window :accessor frame-prompt-window)
   (prompt-buffer :accessor frame-prompt-buffer)
   (width         :accessor frame-width  :initarg :width)
   (height        :accessor frame-height :initarg :height)
   (screen        :accessor frame-screen :initarg :screen)))

(defgeneric context-buffer (window))
(defgeneric context-point (window))
(defgeneric context-mark (window))
(defgeneric context-save (window))
(defgeneric (setf context-point) (value window))
(defgeneric (setf context-mark) (value window))

(defclass context ()
  ((buffer    :accessor context-buffer     :initarg :buffer)
   (mark      :accessor context-mark       :initarg :mark   :initform nil
              :documentation "Mark in the buffer.")
   (point     :accessor context-point      :initarg :point  :initform nil
              :documentation "Insertion point in the buffer.")
   (edited-p  :accessor context-edited-p   :initform nil)))

(defclass window ()
  ((frame     :accessor window-frame      :initarg :frame)
   (context   :accessor window-context    :initarg :context :initform nil)
   (top       :accessor window-top        :initarg :top)
   (left      :accessor window-left       :initarg :left)
   (height    :accessor window-height     :initarg :height)
   (width     :accessor window-width      :initarg :width)
   (changed-p :accessor window-changed-p  :initform nil)
   (top-row   :accessor window-top-row    :initform 0
              :documentation
              "Number of the line displayed on the top row of the window.")
   (name      :accessor window-name       :initarg :name)))

(defclass window-with-status-bar (window)
  ()
  (:documentation
   "This is a normal window, which displays a status bar
at the bottom.  Normally, only the bottom-most window, displaying the
mini-buffer is a plain window without a status bar."))

(defgeneric window-bottom (window))
(defgeneric window-right (window))
(defgeneric window-visible-line-count (self))
(defgeneric window-visible-line-count (self))
(defgeneric window-move-cursor-to (self &key line column))

(defmethod window-bottom ((self window))
  (+ (window-top self) (window-height self)))
(defmethod window-right ((self window))
  (+ (window-left self) (window-width self)))

(defmethod window-visible-line-count ((self window))
  (window-height self))
(defmethod window-visible-line-count ((self window-with-status-bar))
  (1- (window-height self)))

(defmethod window-move-cursor-to ((self window) &key (line 0) (column 0))
  (let ((frame (window-frame self)))
    (format *log* "move cursor to x:~A, y:~A~%"
            (+ (window-left self) column)
            (+ (window-top  self) line))
    (set-screen-cursor-position (frame-screen frame)
                                (+ (window-top  self) line)
                                (+ (window-left self) column))))


;; +-----------------------------------------------------+
;; |0,0   |                                              |frame-width
;; |      |                                              |
;; |      |                                              |
;; |      |                                              |
;; |      v                                              |
;; |   window-top                                        |
;; |  ---------------+---------------+                   |
;; |      |          |0,0  |         |                   |
;; |      |          |     |         |                   |
;; |      |          |     |line     |                   |
;; |      |          |     |         |                   |
;; |      |          |     |         |                   |
;; |      v          |     |         |                   |
;; |   window-bottom |     v         |                   |
;; | ----------------+---------------+                   |
;; |                 |window-left    |window-width       |
;; |---------------->|-------------->|                   |
;; |                                                     |
;; +-----------------------------------------------------+
;; |frame-height                                         |



;; Forward context methods:

(defmethod context-buffer ((self window))
  (and (window-context self) (context-buffer (window-context self))))
(defmethod context-point  ((self window))
  (and (window-context self) (context-point  (window-context self))))
(defmethod context-mark   ((self window))
  (and (window-context self) (context-mark   (window-context self))))
(defmethod (setf context-point) (value (self window))
  (setf (context-point  (window-context self)) value))
(defmethod (setf context-mark)  (value (self window))
  (setf (context-mark   (window-context self)) value))



;;;---------------------------------------------------------------------
;;; Buffers & Markers
;;;---------------------------------------------------------------------

(defmethod initialize-instance :after ((self buffer) &key &allow-other-keys)
  (push self *buffer-list*)
  (setf (buffer-lines self)  (dll))
  (dll-insert (buffer-lines self) nil "")
  self)

(defmethod print-object ((self buffer) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":name ~S" (buffer-name self)))
  stream)

(defmethod initialize-instance :after ((self marker) &key &allow-other-keys)
  (buffer-add-marker (marker-buffer self) self))

(defmethod print-object ((self marker) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (if (marker-valid-p self)
        (format stream ":buffer ~S :point ~A :gravity ~S"
                (buffer-name (marker-buffer self))
                (marker-point self)
                (marker-gravity self))
        (format stream ":valid-p nil")))
  stream)


(defmethod initialize-instance :after ((self context) &key &allow-other-keys)
  (setf (context-mark  self) (or (context-mark self)
                                 (buffer-saved-mark (context-buffer self)))
        (context-point self) (or (context-point self)
                                 (buffer-saved-point (context-buffer self))
                                 (make-instance 'marker
                                     :buffer (context-buffer self)
                                     :point  0
                                     :gravity :end)))
  self)

(defmethod context-save ((self context))
  (setf (buffer-saved-mark (context-buffer self)) (context-mark self)
        (buffer-saved-point (context-buffer self)) (context-point self)))


(defgeneric marker-delete (marker))

(defmethod marker-delete ((self marker))
  (when (marker-valid-p self)
    (buffer-remove-marker (marker-buffer self) self))
  (setf (marker-buffer self) nil)
  (values))

(defun current-buffer ()
  "Return the current buffer as a Lisp object."
  (context-buffer *current-window*))


(defun buffer-line-count (buffer)
  (dll-length (buffer-lines buffer)))

(defun buffer-or-error (buffer-or-name)
  (or (get-buffer buffer-or-name)
      (error "There is no buffer named ~S" buffer-or-name)))


(defgeneric buffer-substring (buffer-designator start end))
(defgeneric buffer-contents (buffer))
(defgeneric buffer-delete-region (buffer-name start end))

(defmethod buffer-substring ((buffer-name string) start end)
  (buffer-substring (buffer-or-error buffer-name) start end))
(defmethod buffer-substring ((self buffer) start end)
  (in-order start end)
  (assert (<= start end))
  (with-output-to-string (out)
    (multiple-value-bind (srow scolumn sline) (buffer-line-of-point self start)
      (declare (ignore srow))
      (multiple-value-bind (erow ecolumn eline) (buffer-line-of-point self end)
        (declare (ignore erow))
        (cond
          ((null sline))
          ((eq sline eline)
           ;; one line:
           (format out "~A" (nsubseq (dll-node-item sline) scolumn ecolumn)))
          (t
           (when (plusp scolumn)
             ;; partial first line
             (format out "~A~%"  (nsubseq (dll-node-item sline) scolumn))
             (setf sline (dll-node-next sline)))
           (loop
              :until   (eq sline eline)
              :do      (format out "~A~%" (dll-node-item sline))
              :do      (setf sline (dll-node-next sline))
              :finally (when eline
                         (format out "~A~%"
                                 (nsubseq (dll-node-item eline)
                                          0 ecolumn))))))))))

(defmethod buffer-contents (buffer)
  (buffer-substring buffer 0 (buffer-size buffer)))

(defmethod buffer-delete-region ((buffer-name string) start end)
  (buffer-delete-region (buffer-or-error buffer-name) start end))
(defmethod buffer-delete-region ((self buffer) start end)
  (in-order start end)
  (assert (<= start end))
  (when (< start end)
    (multiple-value-bind (srow scolumn sline) (buffer-line-of-point self start)
      (declare (ignore srow))
      (multiple-value-bind (erow ecolumn eline) (buffer-line-of-point self end)
        (declare (ignore erow))
        (if (eq sline eline)
            ;; one line:
            (setf (dll-node-item sline)
                  (concatenate 'string
                    (nsubseq (dll-node-item sline) 0 scolumn)
                    (nsubseq (dll-node-item sline) ecolumn)))
            (progn
              (when (plusp scolumn)
                ;; partial first line
                (setf (dll-node-item sline)
                      (nsubseq (dll-node-item sline) 0 scolumn))
                (setf sline (dll-node-next sline)))
              (loop
                 :until   (eq sline eline)
                 :do (let ((next (dll-node-next sline)))
                       (dll-delete sline (buffer-lines self))
                       (setf sline next))
                 :finally (when sline
                            (setf (dll-node-item sline)
                                  (nsubseq (dll-node-item sline) ecolumn))))))))
    (setf (buffer-changed-p self) t)
    (buffer-move-markers-down self start (- end start))))


(defgeneric buffer-will-insert (buffer-designator)
  (:method ((buffer-name string))
    (buffer-will-insert (buffer-or-error buffer-name)))
  (:method ((self buffer))
    (when (buffer-read-only-p self)
      (error "Buffer is read-only: ~S" self))))


(defgeneric buffer-did-insert (buffer-designator)
  (:method ((buffer-name string))
    (buffer-did-insert (buffer-or-error buffer-name)))
  (:method ((self buffer))
    self))


(defgeneric buffer-insert (buffer-designator point text)
  (:method ((buffer-name string) point text)
    (buffer-insert (buffer-or-error buffer-name) point text))
  (:method ((self buffer) (point marker) text)
    (buffer-insert self (marker-point point) text))
  (:method ((self buffer) point text)
    (format *log* "buffer-insert buffer=~S point=~A text=~S~%" self point text)
    (when (plusp (length text))
      (let ((lines (split-sequence #\newline text)))
        (format *log* "~{line: ~S~%~}" lines)
        (multiple-value-bind (row column current-line)
            (buffer-line-of-point self point)
          (declare (ignore row))
          (cond
            ((null current-line)       ; adding at the end of the buffer
             (loop
               :with new-node = (dll-last-node (buffer-lines self))
               :for new-line :in lines
               :do  (setf new-node (dll-insert (buffer-lines self)
                                               new-node new-line))))
            ((null (cdr lines))           ; one line
             (let ((item (first lines)))
               (setf (dll-node-item current-line)
                     (concatenate 'string
                       (nsubseq (dll-node-item current-line) 0 column)
                       item
                       (nsubseq (dll-node-item current-line) column)))))
            (t                            ; at least two lines
             (loop
               :with before = (nsubseq (dll-node-item current-line) 0 column)
               :with after  = (nsubseq (dll-node-item current-line) column)
               :with new-node = current-line
               :for new-lines :on (rest lines)
               :until (null (cdr new-lines))
               :initially (setf (dll-node-item current-line)
                                (concatenate 'string
                                  before
                                  (first lines)))
               (format *log* "inserting ~S~%" (first lines))
               :do     (setf new-node (dll-insert (buffer-lines self)
                                                  new-node
                                                  (car new-lines)))
               (format *log* "inserting ~S~%" (car new-lines))
               :finally
               (format *log* "inserting ~S~%" after)
               (dll-insert (buffer-lines self) new-node after))))))
      (setf (buffer-changed-p self) t)
      (buffer-move-markers-up self point (length text)))
    (clear-output *log*)
    (+ point (length text))))



(defgeneric buffer-point-of-line (buffer-designator line-number)
  (:method ((buffer-name string) line-number)
    (buffer-point-of-line (buffer-or-error buffer-name) line-number))
  (:method ((self buffer) line-number)
    (loop
      :for line = (dll-first-node (buffer-lines self))
      :then (dll-node-next line)
      :for point = 0
      :then (+ point 1 (length (dll-node-item (dll-node-previous line))))
      :repeat line-number
      :while (dll-node-next line)
      :finally (return point))))


(defgeneric buffer-line-of-point (buffer-designator point)
  (:documentation     "
RETURN: row; column; the line containing point, or NIL if point is at end
                     of buffer on a new line.
")
  (:method ((buffer-name string) point)
    (buffer-line-of-point (buffer-or-error buffer-name) point))
  (:method ((self buffer) point)
    (when (typep point 'marker)
      (setf point (marker-point point)))
    (loop
      :for line = (dll-first-node (buffer-lines self))
        :then (dll-node-next line)
      :for bol = 0 :then eol
      :for eol = (if line (+ 1 (length (dll-node-item line))) 0)
        :then (if line (+ eol 1 (length (dll-node-item line))) eol)
      :for row :from 0
      :while (and line (<= eol point))
      :finally (return (values row (max 0 (- point bol)) line)))))


(defgeneric buffer-size (buffer-designator)
  (:method ((buffer-name string))
    (buffer-size (buffer-or-error buffer-name)))
  (:method ((self buffer))
    (1- (loop
          :for line = (dll-first-node (buffer-lines self))
          :then (dll-node-next line)
          :while line
          :sum (1+ (length (dll-node-item line)))))))



(defgeneric buffer-add-marker (self marker))
(defgeneric buffer-remove-marker (self marker))
(defgeneric buffer-move-markers-up (self start offset))
(defgeneric buffer-move-markers-down (self start offset))


(defmethod buffer-add-marker    ((self buffer) (marker marker))
  (push marker (buffer-markers self))
  (setf (marker-buffer marker) self)
  marker)

(defmethod buffer-remove-marker ((self buffer) (marker marker))
  (assert (eq self (marker-buffer marker)))
  (setf (buffer-markers self) (delete marker (buffer-markers self))
        (marker-buffer marker) nil)
  self)

(defmethod buffer-move-markers-up ((self buffer) start offset)
  (dolist (marker (buffer-markers self))
    (format *log* "mup buffer=~S marker=~S start=~A offset=~A~%"
            self marker start offset)
    (format *log* "mup start=~A ~[<~;=~;~] point=~A ~%"
            start (cond ((< start (marker-point marker)) 0)
                        ((= start (marker-point marker)) 1)
                        (t 2))
            (marker-point marker))
    (clear-output *log*)
    (format *log* "mup new point = ~A ~%" (marker-point marker))
    (when (or (< start (marker-point marker))
              (and (= start (marker-point marker))
                   (eq :end (marker-gravity marker))))
      (incf (marker-point marker) offset)))
  self)

(defmethod buffer-move-markers-down ((self buffer) start offset)
  (let ((end (+ start offset)))
    (dolist (marker (buffer-markers self))
      (cond
        ((<= end   (marker-point marker)) (decf (marker-point marker) offset))
        ((<= start (marker-point marker)) (setf (marker-point marker) start)))))
  self)


(defun get-buffer (designator)
  (if (typep designator 'buffer)
      designator
      (find designator *buffer-list*
            :test (function string=) :key (function buffer-name))))

(defun get-buffer-create (designator)
  (or (get-buffer designator) (make-instance 'buffer :name designator)))



(defun point ()
  (marker-point (context-point *current-window*)))

(defun mark ()
  (and (context-mark *current-window*)
       (marker-point (context-mark *current-window*))))

(defun set-mark (point)
  (declare (interactive "d"))
  (if (context-mark *current-window*)
      (setf (marker-point (context-mark *current-window*)) point)
      (setf (context-mark *current-window*) (make-instance 'marker
                                                :buffer (current-buffer)
                                                :point point))))

(defun goto-char (target)
  (declare (interactive "n"))
  (assert (<= 0 target (buffer-size (context-buffer *current-window*))))
  (setf (marker-point (context-point *current-window*)) target))

(defun point-min ()
  0)

(defun point-max ()
  (buffer-size (current-buffer)))


(defun forward-char (&optional n)
  (declare (interactive "p"))
  (let* ((buffer  (context-buffer *current-window*))
         (size    (buffer-size buffer)))
    (goto-char (if (<= (+ (point) n) size)
                   (+ (point) n)
                   size))))


(defun backward-char (&optional n)
  (declare (interactive "p"))
  (goto-char (if (minusp (- (point) n))
                 0
                 (- (point) n))))



(defun beginning-of-line (&optional n)
  (declare (interactive "p"))
  (declare (ignore n))
  ;; TODO: repeat
  (let ((buffer  (context-buffer *current-window*)))
    (multiple-value-bind (row col line)
        (buffer-line-of-point buffer (context-point  *current-window*))
      (declare (ignore col line))
      (goto-char (buffer-point-of-line buffer row)))))


(defun end-of-line (&optional n)
  (declare (interactive "p"))
  (declare (ignore n))
  ;; TODO: repeat
  (let ((buffer  (context-buffer *current-window*)))
    (multiple-value-bind (row col line)
        (buffer-line-of-point buffer (context-point  *current-window*))
      (declare (ignore col))
      (goto-char (if (or (null line) (null (dll-node-next line)))
                     (buffer-size buffer)
                     (+ (buffer-point-of-line buffer row)
                        (length (dll-node-item line))))))))


(declaim (inline clip))
(defun clip (min value max)
  (cond ((< value min) min)
        ((< max value) max)
        (t value)))

(defun increment-line (n successor-line)
  (let ((buffer  (context-buffer *current-window*)))
    (multiple-value-bind (row col line)
        (buffer-line-of-point buffer (context-point  *current-window*))
      (let ((line (or line (dll-last-node (buffer-lines buffer)))))
        (goto-char
         (+ (buffer-point-of-line buffer (clip 0 (+ row n) (buffer-line-count buffer)))
            (loop
              :for next = (funcall successor-line line)
              :repeat n
              :while next
              :do (setf line next)
              :finally (return (min col (length (dll-node-item line)))))))))))

(defun next-line (&optional (n 1))
  (declare (interactive "p"))
  (increment-line n (function dll-node-next)))

(defun previous-line (&optional (n 1))
  (declare (interactive "p"))
  (increment-line (- n) (function dll-node-previous)))


(defun erase-buffer ()
  "Delete the entire contents of the current buffer.
Any narrowing restriction in effect (see `narrow-to-region') is removed,
so the buffer is truly empty after this."
  (declare (interactive))
  (let* ((buffer (current-buffer))
         (size   (buffer-size buffer)))
    (setf (buffer-lines buffer)  (dll))
    (dll-insert (buffer-lines buffer) nil "")
    (buffer-move-markers-down buffer 0 size))
  (dolist (marker (buffer-markers (current-buffer)))
    (assert (zerop (marker-point marker))))
  (values))




(defun my-debug ()
  (declare (interactive))
  (dolist (window (apply (function append)
                         (mapcar (function frame-window-list) *frame-list*)))
    (with-current-window window
      (erase-buffer)
      (insert "~S ~S" *current-window* (current-buffer)))))


(defun message (ctrl-string &rest args)
  "Inserts a formated string in the *Messages* buffer,
and displays it in the mini-window."
  (let ((text (apply (function format) nil ctrl-string args)))
    (with-buffer (get-buffer-create "*Messages*")
      (goto-char (point-max))
      (insert "~A~%" text))
    (with-current-window (frame-mini-window *current-frame*)
      (switch-to-buffer  (frame-mini-buffer *current-frame*))
      (erase-buffer)
      (insert "~A" text))
    (when *log* (write-line text *log*))))



(defun backward-sexp (n)
  (declare (interactive "p"))
  (cond
    ((minusp n) (forward-sexp (- n)))
    ((plusp  n)
     (let ((previous-text (buffer-substring (current-buffer) 0 (point))))
       (if (plusp (length previous-text))
           (with-input-from-string (src previous-text)
             (loop
               :for previous = nil :then current
               :for current  = 0   :then (file-position src)
               :for sexp = (handler-case (read src nil src)
                             (cl:error () src))
               :until (eq sexp src)
               :finally (if previous
                          (goto-char previous)
                          (error "Cannot read previous S-expressions"))))
           (error "Beginning of buffer"))))))


(defun forward-sexp (n)
  (declare (interactive "p"))
  (cond
    ((minusp n) (backward-sexp (- n)))
    ((plusp  n) (goto-char (with-input-from-string
                               (src (buffer-substring
                                     (current-buffer) (point)
                                     (buffer-size (current-buffer))))
                             (loop
                                :for sexp = (read src nil src)
                                :repeat n
                                :until (eq sexp src)
                                :finally (return  (file-position src))))))))


(defun show-results (results insert-in-buffer-p)
  (if insert-in-buffer-p
      (insert "~%-->~{~S ~^;~%   ~}" results)
      (message "~{~S~^ ;~}" results)))

(defun eval-expression (expression &optional insert-results-p)
  (declare (interactive "xEval: "))
  (show-results (multiple-value-list (eval expression)) insert-results-p))


(defun eval-last-sexp (result-in-buffer-p)
  (declare (interactive "P"))
  (let* ((end     (point))
         (start   (prog1 (progn (backward-sexp 1) (point)) (goto-char end))))
    (show-results (multiple-value-list
                   (eval (read-from-string
                          (buffer-substring (current-buffer) start end))))
                  result-in-buffer-p)))


(defun execute-extended-command (command)
  (declare (interactive "CM-x "))
  (insert "~S" command)
  (call-interactively command))


(defun buffer-for-file (path)
  "
RETURN:   The buffer associated with the file at PATH,
          or NIL if it doesn't exist.
"
  (find-if (lambda (buffer)
             (and (buffer-file buffer)
                  ;; we cannot use truename since that works only on existing files.
                  (equalp path (file-pathname (buffer-file buffer)))))
           *buffer-list*))


(defun find-file (path)
  (declare (interactive  "FFind file: "))
  (let ((buffer (buffer-for-file path)))
    (if buffer
        (switch-to-buffer buffer)
        (let ((file (make-instance 'file :pathname path)))
          (switch-to-buffer (file-name file))
          (setf (buffer-file (current-buffer)) file)
          (insert "~A" (file-contents file))
          (goto-char 0)))))


(defun save-buffer ()
  (declare (interactive))
  (let* ((buffer (current-buffer))
         (file   (buffer-file buffer)))
    (setf (file-contents file) (buffer-contents  buffer))
    (message "Wrote ~A" (file-pathname file))))


;;;---------------------------------------------------------------------
;;; Frames and Windows
;;;---------------------------------------------------------------------

(defvar *counter* 0)

(defmethod initialize-instance :after ((self frame) &key &allow-other-keys)
  (setf
   ;; mini-buffer
   (frame-mini-buffer self) (get-buffer-create
                             (format nil " *Minibuf~D*" (incf *counter*)))
   (frame-mini-window self) (make-instance 'window
                                :name "mini"
                                :frame self
                                :context (make-instance 'context
                                             :buffer (frame-mini-buffer self))
                                :top  (1- (frame-height self))
                                :height 1
                                :left 0
                                :width (frame-width self))
   ;; prompt-buffer
   (frame-prompt-buffer self) (get-buffer-create
                               (format nil " *Prompt~D*" *counter*))
   (frame-prompt-window self) (make-instance 'window
                                  :name "prompt"
                                  :frame self
                                  :context (make-instance 'context
                                               :buffer (frame-prompt-buffer self))
                                  :top  (1- (frame-height self))
                                  :height 1
                                  :left 0
                                  :width 0)
   ;; main window
   *current-window*         (make-instance 'window-with-status-bar
                                :name "main"
                                :frame self
                                :context (make-instance 'context
                                             :buffer (get-buffer-create
                                                      "*scratch*"))
                                :top 0
                                :height (1- (frame-height self))
                                :left 0
                                :width (frame-width self)))
  self)


(defmethod print-object ((self frame) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":title ~S" (frame-title self)))
  stream)


(defmethod initialize-instance :after ((self window) &key &allow-other-keys)
  (push self (frame-window-list (window-frame self)))
  self)

(defmethod print-object ((self window) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (when (window-context self)
      (format stream ":left ~D :bottom ~D :width ~D :height ~D :buffer ~S"
              (window-left  self) (window-bottom self)
              (window-width self) (window-height self)
              (if (buffer-file (context-buffer self))
                  (file-name (buffer-file (context-buffer self)))
                  (buffer-name (context-buffer self))))))
  stream)


(defparameter *mininum-minibuffer-width* 10
  "BUG: Perhaps it should be relative to the screen width? Perhaps can't work with screen to narrow?")


(defvar *recursive-edit* nil)
(defun recursive-edit ()
  (let ((*recursive-edit* t))
    (catch 'abort-recursive-edit
      (return-from recursive-edit
        (catch 'end-recursive-edit
          (redisplay)
          (keyboard-loop))))
    (throw 'keyboard-quit nil)))



(defun read-char-exclusive (&key PROMPT INHERIT-INPUT-METHOD)
  (error "not implemented yet (~S ~S ~S)"
         'read-char-exclusive PROMPT INHERIT-INPUT-METHOD))


(defun read-from-minibuffer (prompt &key initial-contents read
                             keymap inherit-input-method
                             history keep-all default-value)
  (declare (ignore default-value keep-all history inherit-input-method));TODO: handle them.
  (setf prompt (or prompt ""))
  (with-current-window (frame-mini-window *current-frame*)
    (let ((*keymap* (keymap-copy (or keymap *keymap*) :shallow t))
          (done  (lambda ()
                   (declare (interactive))
                   (throw 'end-recursive-edit nil)))
          (abort (lambda ()
                   (declare (interactive))
                   (throw 'abort-recursive-edit nil))))
      (keymap-bind-key *keymap* #\return done)
      (keymap-bind-key *keymap* #\newline done)
      (keymap-bind-key *keymap* '(:control #\g) abort)
      (erase-buffer)
      (with-current-window (frame-prompt-window *current-frame*)
        (erase-buffer)
        (insert "~A" prompt))
      (let ((mlen (- (frame-width *current-frame*) *mininum-minibuffer-width*))
            (plen (length prompt)))
        (when (< mlen plen)
          (setf plen mlen))
        (setf (window-left  (frame-prompt-window *current-frame*)) 0
              (window-width (frame-prompt-window *current-frame*)) plen
              (window-left  *current-window*) plen
              (window-width *current-window*) (- (frame-width *current-frame*)
                                                 plen)))
      (unwind-protect
           (progn
             (when initial-contents
               (insert "~A" initial-contents))
             (recursive-edit)
             (if read
                 (read-from-string (buffer-contents (current-buffer)))
                 (buffer-contents (current-buffer))))
        (setf (window-left  (frame-prompt-window *current-frame*)) 0
              (window-width (frame-prompt-window *current-frame*)) 0
              (window-left  *current-window*) 0
              (window-width *current-window*) (frame-width *current-frame*))))))



(defun list-buffers (&optional file-only-p)
  (declare (interactive "P"))
  (declare (ignore file-only-p)) ;for now
  (switch-to-buffer "*Buffer List*")
  (erase-buffer)
  (insert "~20A ~8A  ~18A ~A~%"
          "Buffer" "Size" "Mode" "File")
  (dolist (buffer *buffer-list*)
    (insert "~20A ~8D  ~18A ~A~%"
            (buffer-name buffer)
            (buffer-line-count buffer)
            "Lisp"
            (if (buffer-file buffer)
                (file-pathname (buffer-file buffer))
                ""))))


(defun switch-to-buffer (buffer)
  "Select BUFFER in the current window.
If BUFFER does not identify an existing buffer,
then this command creates a buffer with that name."
  (declare (interactive "BSwitch to buffer: "))
  (context-save (window-context *current-window*))
  (setf (window-context *current-window*)
        (make-instance 'context :buffer (get-buffer-create buffer))))



(defun kill-buffer (&optional buffer)
  (declare (interactive "bKill buffer:"))
  ;; TODO: query buffer with current-buffer as default.
  (setf buffer (get-buffer (or buffer (current-buffer))))
  (setf *buffer-list* (delete buffer *buffer-list*))
  (when (eq buffer (current-buffer))
    (switch-to-buffer (first *buffer-list*)))
  (dolist (frame *frame-list*)
    (dolist (window (frame-window-list frame))
      (when (eq buffer (context-buffer window))
        (let ((*current-window* window))
          (switch-to-buffer (first *buffer-list*)))))))



(defun insert (ctrl-string &rest args)
  (buffer-will-insert (current-buffer))
  (buffer-insert (current-buffer) (context-point *current-window*)
                 (apply (function format) nil ctrl-string args))
  (buffer-did-insert (current-buffer)))


;; OLD:
;; +-----------------------------------------------------+
;; |0,0   |                                              |frame-width
;; |      |                                              |
;; |      |                                              |
;; |      |                                              |
;; |      |                                              |
;; |      |           window-height                      |
;; |      |          +---------------+                   |
;; |      |          |     ^         |                   |
;; |      |          |     |         |                   |
;; |      |          |     |line     |                   |
;; |      |          |     |         |                   |
;; |      |          |     |         |                   |
;; |      v          |     |         |                   |
;; |   window-bottom |0,0  |         |window-width       |
;; | ----------------+---------------+                   |
;; |                 |window-left                        |
;; |---------------->|                                   |
;; |                                                     |
;; +-----------------------------------------------------+
;; |frame-height                                         |


;; NEW:

;;;---------------------------------------------------------------------
;;; Display Engine
;;;---------------------------------------------------------------------

(defgeneric display-line (window line))
(defgeneric display (window))

(defmethod display-line ((self window) line)
  (format (screen-stream (frame-screen (window-frame self))) "~VA"
          (window-width self)
          (nsubseq line 0 (min (window-width self) (length line)))))


(defmethod display ((self window))
  (loop
    :with screen = (frame-screen (window-frame self))
    :with width  = (window-width self)
    :with buffer = (context-buffer (window-context self))
    :for row :from (window-top-row self)
    :for line = (dll-node-nth (window-top-row self) (buffer-lines buffer))
      :then (dll-node-next line)
    :repeat (print (min (window-visible-line-count self)
                        (- (buffer-line-count buffer)
                           (window-top-row self)))
                   *log*)
    :do (window-move-cursor-to self :line row)
    :do (let ((line (dll-node-item line)))
          (screen-format screen "~VA" width (nsubseq line 0 (min width (length line))))
          (clear-screen-to-eol screen))))



(defun scroll-up   (&optional n)
  (declare (interactive "p"))
  (message "n=~S" n)
  (setf (window-top-row *current-window*)
        (min
         (mod (+ (window-top-row *current-window*)
                 (etypecase n
                   (null    (window-height *current-window*))
                   (cons    (first n))
                   (integer n)))
              (window-height *current-window*))
          (dll-length (buffer-lines (context-buffer (window-context *current-window*)))))))


(defun scroll-down (&optional n)
  (declare (interactive "P"))
  (scroll-up (etypecase n
               (null      n)
               (cons      (list (- (first n))))
               (integer   (- n)))))



(defmethod display ((self window-with-status-bar))
  ;; 1- display the status bar:
  (window-move-cursor-to self :line (window-bottom self))
  (let ((screen  (frame-screen (window-frame self))))
    (unwind-protect
         (progn
           (screen-highlight-on screen)
           (window-move-cursor-to self :line (1- (window-height self)))
           (screen-format screen
                          "~VA" (window-width self)
                          (let* ((lines (dll-length (buffer-lines (current-buffer))))
                                 (status (format nil "--:--  ~A  ~D% L~D (~:(~{~A~^ ~}~))"
                                                 (buffer-name (context-buffer self))
                                                 (truncate
                                                  (/ (window-top-row *current-window*)
                                                     (1+ lines))
                                                  1/100)
                                                 lines
                                                 '(lisp))))
                            (subseq  status 0 (min (window-width self) (length status))))))
      (screen-highlight-off screen)))
  ;; 2- display the contents
  (call-next-method))


(defun redisplay ()
  (declare (interactive))
  ;; (dolist (frame *frame-list*)
  ;;   (with-current-screen (frame-screen frame)))
  (unwind-protect
       (progn
         (format *log* "redisplay: clear-screen~%")
         (screen-cursor-off *current-screen*)
         (clear-screen      *current-screen*)
         (dolist (window (frame-window-list *current-frame*))
           (format *log* "redisplay: display window ~A~%" (window-name window))
           (display window))
         (multiple-value-bind (row column)
             (buffer-line-of-point (context-buffer *current-window*)
                                   (context-point  *current-window*))
           (format *log* "redisplay: move cursor to x:~A, y:~A~%"
                   column
                   (- row (window-top-row *current-window*)))
           (window-move-cursor-to
            *current-window*
            :line  (- row (window-top-row *current-window*))
            :column column)))
    ;; (finish-output (screen-stream *current-screen*))
    (screen-cursor-on *current-screen*)
    (format *log* "redisplay: done~%")
    (finish-output *log*)))



;;;---------------------------------------------------------------------
;;; Miscellaneous Commands
;;;---------------------------------------------------------------------


(defun not-implemented-yet ()
  (declare (interactive))
  (message "Not implemented yet."))


(defun test-command ()
  (declare (interactive))
  (insert "~%Test ~D~%" (incf *counter*)))


;;;---------------------------------------------------------------------
;;; Editor
;;;---------------------------------------------------------------------

(defun editor-reset ()
  (setf *current-screen*     nil
        *buffer-list*       '()
        *current-frame*     nil
        *frame-list*        '()
        *current-window*    nil
        *counter*           0
        *last-command-char* nil
        *keymap*            (keymap-copy *default-keymap*))
  (values))

(defun editor-initialize (screen)
  (multiple-value-bind (height width) (screen-size screen)
    (setf *current-frame* (make-instance 'frame
                            :screen screen :width width :height height
                            :title "editor")
          *frame-list* (list *current-frame*)))
  (insert *scratch-buffer-default-contents*)
  (redisplay))


(defun editor-terminate ()
  (format t "~&Good bye!~%")
  (values))


(defun command-character (keyboard-event)
  (etypecase keyboard-event
      (character keyboard-event)
      (list      (car (last keyboard-event)))))


(defun yank (repeat-count)
  (declare (interactive "p"))
  (if (minusp repeat-count)
      (let ((start (point)))
        (yank (- repeat-count))
        (goto-char start))
      (loop
         :repeat repeat-count
         :do (insert "~A" *yank*))))

(defun kill-region (start end)
  (declare (interactive "r"))
  (setf *yank* (buffer-substring (current-buffer) start end))
  (unless (buffer-read-only-p (current-buffer))
    (buffer-delete-region (current-buffer) start end)
    (setf *this-command* 'kill-region)))

(defun delete-char (&optional n)
  (declare (interactive "p"))
  (cond
    ((minusp n) (delete-backward-char (- n)))
    ((plusp  n) (kill-region (point) (+ (point) n)))))

(defun delete-backward-char (&optional n)
  (declare (interactive "p"))
  (cond
    ((minusp n) (delete-char (- n)))
    ((plusp  n) (kill-region (point) (- (point) n)))))

(defun kill-sexp (n)
  (declare (interactive "p"))
  (kill-region (point) (progn (forward-sexp n) (point))))

(defun kill-line (repeat-count)
  "Kill the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.
With zero argument, kills the text before point on the current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To kill a whole line, when point is not at the beginning, type \
\\[beginning-of-line] \\[kill-line] \\[kill-line].

If `*kill-whole-line*' is non-nil, then this command kills the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always kill a whole line
by typing \\[beginning-of-line] \\[kill-line].

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-line].

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (declare (interactive "*P"))
  (declare (ignorable repeat-count))
  (kill-region (point) (progn (end-of-line) (1+ (point))))
  #+ (or) (let ((start (point)))
    (if (zerop (length (string-trim
                        #(#\space #\tab)
                        (buffer-substring (current-buffer)
                                          start
                                          (progn (end-of-line) (point))))))
        ;; TODO
        )))


(defun new-line (repeat-count)
  "Inserts a new line."
  (declare (interactive "p"))
  (insert "~V%" repeat-count))

(defun self-insert-command (repeat-count)
  "Inserts the last command character."
  (declare (interactive "p"))
  (loop
     :with datum = (command-character *last-command-char*)
     :repeat repeat-count
     :do (insert "~A" datum)))


(defun digit-argument (prefix)
  "Part of the numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (declare (interactive "P"))
  (let ((digit (digit-char-p (command-character *last-command-char*))))
    (setf *prefix-arg*
          (cond ((integerp prefix)
                 (+ (* prefix 10) (if (minusp prefix) (- digit)  digit)))
                ((eql '- prefix)(if (zerop digit) '- (- digit)))
                (t digit))))
  ;; (setq universal-argument-num-events (length (this-command-keys)))
  ;; (ensure-overriding-map-is-bound)
  )

(defun universal-argument ()
  "Begin a numeric argument for the following command.
Digits or minus sign following C-u make up the numeric argument.
C-u following the digits or minus sign ends the argument.
C-u without digits or minus sign provides 4 as argument.
Repeating C-u without digits or minus sign
 multiplies the argument by 4 each time.
For some commands, just C-u by itself serves as a flag
which is different in effect from any particular numeric argument.
These commands include C-@ and M-x start-kbd-macro."
  (declare (interactive))
  (setf *prefix-arg* (list 4))
  ;; (setq universal-argument-num-events (length (this-command-keys)))
  ;; (ensure-overriding-map-is-bound)
  )

(defun negative-argument (arg)
    "Begin a negative numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
    (declare (interactive "P"))
    (setf *prefix-arg* (cond ((integerp arg) (- arg))
                             ((eq arg '-)    nil)
                             (t              '-)))
    ;; (setq universal-argument-num-events (length (this-command-keys)))
    ;; (ensure-overriding-map-is-bound))
    )


(defun editor-quit ()
  "Quit the editor."
  (declare (interactive))
  (throw 'editor-quit nil))

(defun keyboard-quit ()
  "Reset the keyboard state"
  (declare (interactive))
  (throw 'keyboard-quit nil))

(defun keyboard-modifiers (bits)
  (loop
     :for bit = 1  :then (* 2 bit)
     :for modifier :in  '(:control :meta :super :hyper)
     :unless (zerop (logand bit bits)) :collect modifier))


(defvar *current-keymap*   nil)
(defvar *current-sequence* '())

(defun editor-reset-key ()
  (setf *current-keymap*   *keymap*
        *current-sequence* '()))

(defun editor-process-key (key)
  (let ((binding (keymap-binding *current-keymap* key)))
    (push key *current-sequence*)
    (cond
      ((keymapp binding)
       (format *log* "editor-process-key -> keymap ~{~A ~}~%"
               (reverse *current-sequence*))
       (setf *current-keymap* binding))
      ((or (and (symbolp binding)
                (fboundp binding)
                (interactivep binding))
           (and (functionp binding)
                (interactivep binding)))
       (format *log* "editor-process-key -> binding ~{~A ~} --> ~S~%"
               (reverse *current-sequence*) binding)
       (setf *last-command-char*  (first *current-sequence*)
             *this-command*       binding
             *current-prefix-arg* *prefix-arg*
             *prefix-arg*         nil)
       (call-interactively binding)
       (setf *last-command* *this-command*)
       (editor-reset-key))
      ((null binding)
       (beep))
      (t (message "~{~A ~} is bound to a non-command: ~S~%"
                  (reverse *current-sequence*) binding)
         (editor-reset-key)))))



(defvar *handler-window-height* 10)
(defvar *handler-window-current* 0)


(defun handler-window-position ()
  (multiple-value-bind (width height) (screen-size *current-screen*)
    (declare (ignore width))
    (truncate (- height *handler-window-height*) 2)))

(defun handler-window-current-position ()
  (+ *handler-window-current* (handler-window-position)))

(defun handler-window-initialize ()
  (loop :for y = (handler-window-position)
        :repeat *handler-window-height*
        :do (set-screen-cursor-position *current-screen* y 0)
            (clear-screen-to-eol *current-screen*))
  (setf *handler-window-current* 0))

(defun handler-window-writeln (format-control &rest arguments)
  (when (<= *handler-window-height* *handler-window-current*)
    (setf *handler-window-current* 0))
  (set-screen-cursor-position *current-screen* (handler-window-current-position) 0)
  (ignore-errors
   (screen-write-string *current-screen* (apply (function format) nil format-control arguments)))
  (incf *handler-window-current*))

(defun beep ())

(defgeneric screen-read-line (screen))

(defmethod screen-read-line ((screen screen))
  (restart-bind ((continue-reading (lambda () (throw 'continue-read (values)))
                                   :report-function (reportly "Continue reading line.")))
    (let ((line (make-array 80 :element-type 'character :adjustable t :fill-pointer 0))
          (meta-seen-p nil))
      (loop
        (catch 'continue-read
          (let ((chord (keyboard-chord-no-hang *current-screen*)))
            (when chord
              (let ((key       (chord-character chord))
                    (modifiers (append (when meta-seen-p
                                         (setf meta-seen-p nil)
                                         '(:meta))
                                       (symbolic-modifiers (chord-modifiers chord)))))
                (cond
                  ((eql #\escape key) (setf meta-seen-p t))
                  (modifiers (beep))
                  ((eql #\rubout key)
                   (when (plusp (fill-pointer line))
                     (decf (fill-pointer line))
                     (multiple-value-bind (column line) (screen-cursor-position *current-screen*)
                       (set-screen-cursor-position *current-screen* (1- column) line)
                       (screen-write-string *current-screen* " "))))
                  ((find key #(#\Newline #\Return #\Linefeed))
                   (return-from screen-read-line line))
                  (t
                   (screen-write-string *current-screen* (string key))
                   (vector-push-extend key line)))))))))))

(defvar *condition*)

(defun handle-editor-error (condition)
  (let* ((restarts (compute-restarts condition))
         (last-r   (1- (length restarts))))
    (flet ((print-restart-list ()
             (setf last-r (loop
                            :for r :in restarts
                            :for i :from 0
                            :do (handler-window-writeln "~D: (~10A) ~A" i (restart-name r) r)
                            :until (eq (restart-name r) 'abort)
                            :finally (return i)))))
      (let ((restart
              (loop
                :for n = (progn
                           (handler-window-initialize)
                           (handler-window-writeln "~A" condition)
                           (print-restart-list)
                           (handler-window-writeln "Option: ")
                           (prog1 (ignore-errors
                                   (read-from-string (screen-read-line *current-screen*)))
                             (handler-window-writeln "")))
                :until (and (typep n 'integer) (<= 0 n last-r))
                :finally (return (nth n restarts)))))
        (handler-window-writeln "~S" (list 'restart '= (restart-name restart)))
        (handler-window-writeln "~S" (list '*debugger-hook* '= *debugger-hook*))
        (let ((*condition* condition))
          (handler-bind ((cl:error (function invoke-debugger)))
            (redisplay)
            (invoke-restart-interactively restart)))))))


(defun reportly (string)
  (lambda (stream) (format stream "~A" string)))


(defun keyboard-loop ()
  (handler-bind ((cl:error (function handle-editor-error)))
    (restart-bind ((debug    (lambda () (invoke-debugger *condition*))
                             :report-function (reportly "Invoke the debugger."))
                   (continue (lambda () (throw 'keyboard-quit (values)))
                             :report-function (reportly "Continue editing."))
                   (abort    (lambda () (throw 'editor-quit (values)))
                             :report-function (reportly "Quit the editor.")))
      (catch 'editor-quit
        (loop
          (catch 'keyboard-quit
            (loop
              :with redisplayed = t
              :with meta-seen-p = nil
              :for chord = (keyboard-chord-no-hang *current-screen*)
                :initially (editor-reset-key) (redisplay)
              :do (if chord
                      (let ((key       (chord-character chord))
                            (modifiers (append (when meta-seen-p
                                                 (setf meta-seen-p nil)
                                                 '(:meta))
                                               (symbolic-modifiers (chord-modifiers chord)))))
                        (format *log* "chord = ~S   meta-seen-p = ~S   key = ~S   modifiers = ~S~%"
                                chord meta-seen-p key modifiers)
                        (if (eql #\escape key)
                            (setf meta-seen-p t)
                            (progn
                              (editor-process-key
                               (if modifiers
                                   (append modifiers (list key))
                                   key))
                              (setf redisplayed nil)))
                        ;; (message "key=~S modifiers=~S" key modifiers)
                        )
                      (unless redisplayed
                        (redisplay)
                        (setf redisplayed t))))))))))


#+mocl
(defun shadow-synonym-stream (stream synonym)
  (declare (ignore synonym))
  stream)

#-mocl
(defun shadow-synonym-stream (stream synonym)
  (if (and (typep stream 'synonym-stream)
           (eq synonym (synonym-stream-symbol stream)))
      (symbol-value synonym)
      stream))




(defun screen-editor (&key log (screen-class 'charms-screen))
  (with-open-stream (*log* (typecase log
                             ((member :xterm) (make-xterm-io-stream :geometry "100x24+0+0"))
                             ((or string pathname)  (open log
                                                          :direction :output
                                                          :if-exists :append
                                                          :if-does-not-exist :create))
                             (file  log)
                             (otherwise (make-broadcast-stream))))
    (let ((*error-output* *log*)
          (*trace-output* *log*)
          (screen (make-instance screen-class)))
      (screen-initialize-for-terminal screen (getenv "TERM"))
      (editor-reset)
      (with-screen screen
        (editor-initialize *current-screen*)
        (unwind-protect (keyboard-loop)
          (set-screen-cursor-position *current-screen*
                                      0 (screen-size *current-screen*))
          (clear-screen *current-screen*))
        (editor-terminate)))))



(defun editor () (screen-editor :log "/tmp/editor.log"))
(defun ed (&rest args) (apply (function screen-editor) args))

#-mocl
(defun reload ()
  (in-package "CL-USER")
  (funcall (when (find-package "QL")
             (intern "QUICKLOAD" (find-package "QL"))
             (function identity))
           :com.informatimago.editor)
  (in-package "EDITOR"))


(in-package "COMMON-LISP-USER")

#-mocl
(progn
 (print '(COM.INFORMATIMAGO.EDITOR::reload))
 (print '(COM.INFORMATIMAGO.EDITOR:screen-editor))
 (print '(COM.INFORMATIMAGO.EDITOR:ed)))

;;;; THE END ;;;;

