;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               parser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A parser for GNU makefiles
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2023-04-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2023 - 2023
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
(in-package "COM.INFORMATIMGO.TOOLS.MAKE.PARSER")

;;;---------------------------------------------------------------------

(defgeneric parse-makefile (makefile &key &allow-other-keys))

;;;---------------------------------------------------------------------


(defstruct floc
  filename
  lineno
  offset)

(defstruct ebuffer
  (buffer "")
  stream
  floc)

(defstruct vmodifiers
  assign
  define
  underfine
  override
  private
  export)

(deftype make-word-type ()
  '(member :bogus :eol :static :variable
    :colon :dcolon :semicolon
    :varassign :ampcolon :ampdcolon))

(defstruct conditionals
  if-cmds
  allocated
  ignoring
  seen-else)

(defvar *conditionals* (make-conditionals))
(defvar *default-include-directories* '())
(defvar *include-directories* '())
(defvar *max-incl-len* 0)
(defvar *reading-file* nil "Current floc")
(defvar *read-files* nil "goaldep")

(defconstant *command-prefix* "	" "TAB")


(defun readline (ebuf)
  ;; Note: readstring is implemented with with-input-from-string in (parse-makefile (string))
  (loop
    :with nlines := 0
    :for line := (read-line (ebuffer-stream ebuf) nil nil)
    :while line
    :do
    ;; This only happens when the first thing on the line is a
    ;; '\0'.  It is a pretty hopeless case, but (wonder of
    ;; wonders) Athena lossage strikes again!  (xmkmf puts NULs in
    ;; its makefiles.)  There is nothing really to be done; we
    ;; synthesize a newline so the following line doesn't appear
    ;; to be part of this line.
    (when (find #\null line)
      (warn "NUL character seen; rest of line ignored")
      (setf line (subseq line 0 (position #\null line))))

    ;; We got a newline, so add one to the count of lines.
    (incf nlines)

    ;; Check to see if the line was really ended with CRLF; if so ignore
    ;; the CR.
    #-(or windows dos emx)
    (setf line (delete #\return line))

    (let ((blen (length line)))
      (setf line (string-right-trim "\\" line))
      (setf (ebuffer-buffer ebuf)
            (if (oddp (- blen (length line)))
                (if (ebuffer-buffer ebuf)
                    (concatenate 'string (ebuffer-buffer ebuf) line)
                    line)
                line)))
    :finally
    ;;   /* If we found some lines, return how many.
    ;;      If we didn't, but we did find _something_, that indicates we read the last
    ;;      line of a file with no final newline; return 1.
    ;;      If we read nothing, we're at EOF; return -1.  */
    (return (cond
              ((plusp nlines) (values nlines line))
              ((ebuffer-buffer ebuf) (values 1 line))
              (t (values -1 nil))))))


(defun test/readline ()
  (assert (equal (multiple-value-list
           (with-input-from-string (makefile "
NAME = make-print-vars
TEST_MAKEFILE ?= /build/pbourguignon/work/build.devel/.makedump

all:$(NAME)
	   $(NAME) --help \
	&& $(NAME) --mangle $(TEST_MAKEFILE) > m.sed \
	&& $(NAME)          $(TEST_MAKEFILE) > $(TEST_MAKEFILE)-vars

$(NAME):$(NAME).lisp
	ccl < generate-exe.lisp >$(NAME).log 2>$(NAME).err || ( cat $(NAME).err ; exit 200 )

clean:
	-rm -f *.lx64fsl
	-rm -f *.out *.err
	-rm -f system-index.txt
")
             (let ((ebuf (make-ebuffer
                          :stream makefile
                          :floc (make-floc :filename "/mem/string/makefile"
                                           :lineno 1
                                           :offset 0))))
               (values (readline ebuf) (ebuffer-buffer ebuf)))))
                 '(1 "NAME = make-print-vars")))
  :success)

;; Note: we don't want to hardwire with #+ those values,
;; to be able to read foreign files (cross tools)

(defconstant +MAP-NUL+      #x0001)
(defconstant +MAP-BLANK+    #x0002)
(defconstant +MAP-NEWLINE+  #x0004)
(defconstant +MAP-COMMENT+  #x0008)
(defconstant +MAP-SEMI+     #x0010)
(defconstant +MAP-EQUALS+   #x0020)
(defconstant +MAP-COLON+    #x0040)
(defconstant +MAP-VARSEP+   #x0080)
(defconstant +MAP-PIPE+     #x0100)
(defconstant +MAP-DOT+      #x0200)
(defconstant +MAP-COMMA+    #x0400)
(defconstant +MAP-USERFUNC+ #x2000)
(defconstant +MAP-VARIABLE+ #x4000)
(defconstant +MAP-DIRSEP+   #x8000)

#+vms (defconstant +MAP-VMSCOMMA+ +MAP-COMMA+)
#-vms (defconstant +MAP-VMSCOMMA+ #x0000)

(defconstant +MAP-SPACE+ (logior +MAP-BLANK+ +MAP-NEWLINE+))

#+(and dos (not cygwin)) (defconstant +PATH-SEPARATOR-CHAR+ #\;)
#+(and dos (not cygwin)) (defconstant +MAP-PATHSEP+         +MAP-SEMI+)

#+unix (defconstant +PATH-SEPARATOR-CHAR+ #\:)
#+unix (defconstant +MAP-PATHSEP+         +MAP-COLON+)

;; (defconstant +PATH-SEPARATOR-CHAR+ #\;)
;; (defconstant +MAP-PATHSEP+         +MAP-SEMI+)
;;
;; (defconstant +PATH-SEPARATOR-CHAR+ #\,)
;; (defconstant +MAP-PATHSEP+         +MAP-COMMA+)

(defvar *whitespaces* #(#\space #\tab #\page #\vt
                        #\newline #\linefeed #\return))
(defun whitespacep (ch) (find ch *whitespaces*))
(defun trim-starting-spaces (string)
  (string-left-trim *whitespaces* string))


(defparameter *stopchar-map*
  (let ((map (make-array 128 :element-type 'fixnum :initial-element 0))
        (maxcode 127))
    (flet ((enter (ch flag)
             (assert (<= (char-code ch) maxcode))
             (setf (aref map (char-code ch)) flag)))
      (enter #\Null   +MAP-NUL+)
      (enter #\#      +map-comment+)
      (enter #\;      +map-semi+)
      (enter #\=      +map-equals+)
      (enter #\:      +map-colon+)
      (enter #\|      +map-pipe+)
      (enter #\.      (logior +map-dot+ +map-userfunc+))
      (enter #\(      +map-varsep+)
      (enter #\{      +map-varsep+)
      (enter #\}      +map-varsep+)
      (enter #\)      +map-varsep+)
      (enter #\$      +map-variable+)
      (enter #\_      +map-userfunc+)
      (enter #\-      +map-userfunc+)
      (enter #\space  +map-blank+)
      (enter #\tab    +map-blank+)
      (enter #\/      +map-dirsep+)
      #+vms (enter #\:      +map-dirsep+)
      #+vms (enter #\]      +map-dirsep+)
      #+vms (enter #\>      +map-dirsep+)
      #+dos (enter #\\      +map-dirsep+))
    (loop :for i :from 1 :to maxcode
          :do (cond ((and (whitespacep (code-char i))
                          (zerop (logand (aref map i) +map-blank+)))
                     (setf (aref map i) (logior  (aref map i) +map-newline+)))
                    ((alphanumericp (code-char i))
                     (setf (aref map i) (logior (aref map i) +map-userfunc+)))))
    map))

(declaim (inline any-set stop-set isblank isspace isdirsep has-drivespec
                 end-of-token  map-stopchar))

(defun map-stopchar (ch)
  (let ((code (char-code ch)))
    (if (<= (length *stopchar-map*) code)
        0
        (aref *stopchar-map* code))))

(defun any-set       (a b)      (not (zerop (logand a b))))
(defun stop-set      (ch mask)  (any-set (map-stopchar ch) mask))
(defun isblank       (ch)       (stop-set ch +map-blank+))
(defun isspace       (ch)       (stop-set ch +map-space+))
(defun isdirsep      (ch)       (stop-set ch +map-dirsep+))
(defun has-drivespec (ch)       (stop-set ch +map-dirsep+))
(defun end-of-token  (ch)       (stop-set ch (logior +map-space+ +map-nul+)))


(defun split-tokens (source)

  ;; char* find_next_token(const char** ptr,size_t* lengthpr);
  ;; find-if = NEXT_TOKEN()

  (loop
    :with end := 0
    :with eos := (length source)
    :for start := (or (position-if (lambda (ch) (not (isspace ch))) source)
                      eos)
    :then (or (position-if (lambda (ch) (not (isspace ch))) source :start end)
              eos)
    :while  (< start eos)
    :do (setf end (or (position-if (function end-of-token) source :start start)
                      eos))
    :collect (subseq source start end)))



;; (defconstant +PARSEFS-NONE+    #x0000)
;; (defconstant +PARSEFS-NOSTRIP+ #x0001)
;; (defconstant +PARSEFS-NOAR+    #x0002)

;; (defconstant +PARSEFS-NOGLOB+  #x0004)
;; (defconstant +PARSEFS-EXISTS+  #x0008)
;; (defconstant +PARSEFS-NOCACHE+ #x0010)
;; (defconstant +PARSEFS-ONEWORD+ #x0020)
;; (defconstant +PARSEFS-WAIT+    #x0040)

(deftype flag ()
  '(member :nostrip :noar :noglob :exists :nocache :oneword :wait))

(defun find-map-unquote (source start stopmap)
  ;; Search STRING for an unquoted STOPMAP.
  ;; Backslashes quote elements from STOPMAP and backslash.
  ;; Quoting backslashes are removed from STRING by compacting it into itself.
  ;; Returns a pointer to the first unquoted STOPCHAR if there is one, or nil if
  ;; there are none.
  ;;
  ;; If MAP_VARIABLE is set, then the complete contents of variable references
  ;; are skipped, even if they contain STOPMAP characters.

  ;; Returns: the token text and the end position in source.
  (setf stopmap (logior stopmap +map-nul+))
  (let* ((eos (length source))
         (end start))

    ;; NOTE: current implementation in make (4.+) read.c says that $()
    ;;       has higher priority than \x :
    ;; - the code tests for $ before testing for a previous \
    ;; - the result is that:
    ;;     c=n
    ;;     s=\$(c)
    ;;   reads as s=\n (after $(c) expansion)

    (loop
      :do (setf end (or (position-if (lambda (ch) (stop-set ch stopmap))
                                     source :start end)
                        eos))
      :while (< end eos)
      ;;  If we stopped due to a variable reference, skip over its contents.
      :do (cond
            ((char= #\$ (aref source end))
             (incf end)
             (when (< end eos)
               (let ((openparen (aref source end)))
                 (case openparen
                   ((#\( #\{)
                    (let ((closeparen (if (char= #\( openparen)
                                          #\)
                                          #\}))
                          (pcount 1))
                      (incf end)
                      (loop
                        :while (and (< end eos) (plusp pcount))
                        :do (cond
                              ((char= (aref source end) openparen)
                               (incf pcount))
                              ((char= (aref source end) closeparen)
                               (decf pcount)))
                            (incf end))))))
               ;; Skipped the variable reference: look for STOPCHARS again.
               ))


            ((and (< start end) (char= (aref source (1- end)) #\\))
             ;; Search for more backslashes.
             (let ((i (- end 2)))
               (loop :while (and (<= start i)
                                 (char= (aref source i) #\\))
                     :do (decf i))
               (incf i)
               (multiple-value-bind (backslash-count escape-count)
                   (truncate (- end i) 2)
                 (replace source source
                          :start2 start :end2 (- end backslash-count)
                          :start1 (+ start backslash-count))
                 (incf start backslash-count)
                 (when (zerop escape-count)
                   ;; All the backslashes quoted each other; the STOPCHAR was
                   ;; unquoted.
                   (return-from find-map-unquote
                     (values end (subseq source start end)))))
               ;; The STOPCHAR was quoted by a backslash.  Look for another.
               (incf end)))
            (t
             (return-from find-map-unquote
               (values end (subseq source start end))))))
    (values end nil)))

(defun test/find-map-unquote ()
  (assert (equal (multiple-value-list (find-map-unquote "foo bar baz" 4 +MAP-BLANK+))
                 '(7 "bar")))
  (assert (equal (multiple-value-list (find-map-unquote "foo bar" 4 +MAP-BLANK+))
                 '(7 nil)))

  ;; reads $(…):
  (assert (equal (multiple-value-list
                  (find-map-unquote "foo bar$(baz and,quux) bozo"
                                    4 +MAP-BLANK+))
                 '(12 "bar$(baz")))
  (assert (equal (multiple-value-list
                  (find-map-unquote "foo bar$(baz and,quux) bozo"
                                    4 (logior +MAP-BLANK+ +map-variable+)))
                 '(22 "bar$(baz and,quux)")))
  (assert (equal (multiple-value-list
                  (find-map-unquote "foo bar\\\\$(baz and,quux) bozo"
                                    4 (logior +MAP-BLANK+ +map-variable+)))
                 '(24  "bar\\\\$(baz and,quux)")))
  (assert (equal (multiple-value-list
                  (find-map-unquote "foo bar\\$(baz and,quux) bozo"
                                    4 (logior +MAP-BLANK+ +map-variable+)))
                 '(23 "bar\\$(baz and,quux)")))
  ;; reads \\x:
  (assert (equal (multiple-value-list
                  (find-map-unquote "foo bar\\quux bozo"
                                    4 (logior +MAP-BLANK+ +map-variable+)))
                 '(12 "bar\\quux")))
  (assert (equal (multiple-value-list
                  (find-map-unquote "foo bar\\\\quux bozo"
                                    4 (logior +MAP-BLANK+ +map-variable+)))
                 '(13 "bar\\\\quux")))
  (assert (equal (multiple-value-list
                  (find-map-unquote "foo bar\\ quux bozo"
                                    4 (logior +MAP-BLANK+ +map-variable+)))
                 '(13 "bar\\ quux")))
  (assert (equal (multiple-value-list
                  (find-map-unquote "foo bar\\\\ quux bozo"
                                    4 (logior +MAP-BLANK+ +map-variable+)))
                 '(9 "bar\\")))
  :success)




(defun parse-file-seq (source stopmap prefix flags)
  "
PREFIX: added to each parsed file.
STOPMAP: an integer mask of +MAP-…+
FLAGS: a list of
RETURN: a list of namestrings.
"
  (let ((findmap (logior stopmap +map-vmscomma+ +map-nul+))
        (found-wait nil)
        (gl (make-glob)))
    (unless (member :oneword flags)
      (setf findmap (logior findmap +map-blank+)))
    (setf stopmap (logior stopmap +map-nul+)) ; Always stop on NUL.
    (unless (member :noglob flags)
      (dir-setup-glob gl))

    ;; Skip whitespace; at the end of the string or STOPCHAR we're done.
    (loop
      :with end := 0
      :with eos := (length source)
      :for start := (or (position-if (lambda (ch) (not (isspace ch))) source)
                        eos)
      :then (or (position-if (lambda (ch) (not (isspace ch))) source :start end)
                eos)
      :while (or (< start eos) (stop-set (aref source start) stopmap))
      :do

      (setf end (find-map-unquote source start findmap))
      (setf end (or (position-if (function end-of-token) source :start end)
                    eos))
      :collect (subseq source start end))

    (loop
      :for

      )
    ))

(defvar *command-prefix* nil
  "NIL => Commands start with a TAB; otherwise they start with that string.")

(defmethod parse-makefile ((ebuf ebuffer) &key set-default &allow-other-keys)
  (let (collapsed
        commands
        commands-started
        (commands-index 0)
        targets-started
        ignoring
        in-ignored-define
        no-targets
        also-make-targets
        filenames
        depstr
        (nlines 0)
	    two-colon
        (prefix *command-prefix*)
	    pattern
        pattern-percent
        fstart
        (fi (make-floc))
        token)
    (flet ((record-waiting-files ()
	         (when filenames
	           (setf (floc-lineno fi) targets-started
                     (floc-offset fi) 0)
               (record-files filenames also-make-targets
                             pattern pattern-percent depstr
                             commands-started commands-index commands
                             two-colon prefix fi)
               (setf filenames nil))
             (setf commands-index 0
                   no-targets nil
                   pattern nil
                   also-make-targets nil)))

      (setf pattern-percent nil
            commands-started t
            targets-started  t
	        fstart (ebuffer-floc ebuf)
            (floc-filename fi) (floc-filename (ebuffer-floc ebuf)))

      ;; Loop over lines in the file.
      ;; The strategy is to accumulate target names in FILENAMES, dependencies
      ;; in DEPS and commands in COMMANDS.  These are used to define a rule
      ;; when the start of the next rule (or eof) is encountered.
      ;;
      ;; When you see a "continue" in the loop below, that means we are moving on
      ;; to the next line.  If you see record_waiting_files(), then the statement
      ;; we are parsing also finishes the previous rule.
      (loop
        :named parse
        :do (block continue
              (flet ((next () (return-from continue)))
	            (let ((linelen 0)
                      line
                      (wlen 0)
                      p p2
                      (vmod (make-vmodifiers)))
                  ;; At the top of this loop, we are starting a brand new line.
                  ;; Grab the next line to be evaluated

                  (incf (floc-lineno ebuf) nlines)
                  (setf nlines (readline ebuf))
		          ;; If there is nothing left to eval, we're done.
	              (when (minusp nlines)
                    (return-from parse))

                  (setf line (ebuffer-buffer ebuf))

			      ;; Note: in CL we don't need to check the UTF-8 BOM, since
			      ;; it's done by the stream external-format coding.

	              ;; If this line is empty, skip it.
                  (when (zerop (length line))
                    (next))
                  (setf linelen (length line))

                  ;; Check for a shell command line first.
                  ;;    If it is not one, we can stop treating cmd_prefix specially.
    	          (when (char= *command-prefix* (aref line 0))

                    ;; Ignore the commands in a rule with no targets.
	                (when no-targets
                      (next))
                    (when filenames
                      (when ignoring
                        ;; Yep, this is a shell command, and we don't care.
                        (next))
                      (when (null commands)
                        (setf commands-started (floc-lineno (ebuffer-floc ebuf))))

                      ;; Append this command line to the line being accumulated.
                      ;;    Skip the initial command prefix character.
			          (push (subseq line 1) commands)
                      (next)))
                  ;; This line is not a shell command line.  Don't worry about whitespace.
                  ;;    Get more space if we need it; we don't need to preserve the current
                  ;;    contents of the buffer.

		          (setf collapsed line)
	              (setf collapsed (collapse-continuations collapsed))
	              (setf collapsed (remove-comments collapsed))
                  (setf collapsed (trim-starting-spaces collapsed))
                  ;; See if this is a variable assignment.  We need to do this early, to
                  ;;    allow variables with names like 'ifdef', 'export', 'private', etc.
                  (setf p (parse-variable-assignment collapsed 0 vmod))
                  (when (vmodifiers-assign vmod)
                    (let (v
                          (origin (if (vmodifiers-override vmod)
                                      :override
                                      :file)))
                      ;; If we're ignoring then we're done now.
                      (when ignoring
                        (when (vmodifiers-define vmod)
                          (setf in-ignored-define t))
                        (next))

                      ;; Variable assignment ends the previous rule.
                      (record-waiting-files)
                      (when (vmodifiers-undefine vmod)
                        (do-undefine p origin ebuf)
                        (next))
                      (if (vmodifiers-define vmod)
                          (setf v (do-define p origin ebuf))
                          (setf v (try-variable-definition fstart p origin 0)))
                      (assert v)
                      (unless (eq :default (vmodifiers-export vmod))
                        (setf (variable-export v) (vmodifier-export vmod)))
                      (when (vmodifiers-preivate vmod)
                        (setf (variable-private-var v) t))
                      ;; This line has been dealt with.
                      (next)))

                  ;; If this line is completely empty, ignore it.
                  (when (zerop (length p))
                    (next))

                  (multiple-value-setq (token p2) (end-of-token p))
                  (setf p2 (trim-starting-spaces p2))
                  ;; If we're in an ignored define, skip this line (but maybe get out).
                  (when in-ignored-define
                    ;; See if this is an endef line (plus optional comment).
                    (if (and (string= "endef" token)
                             (stop-set p2 '(or map-comment map-nul)))
                        (setf in-ignored-define nil))
                    (next))
                  ;; Check for conditional state changes.
                  (let ((i (conditional-line p token fstart)))
                    (case i
                      ((-2))
                      ((-1) (error "invalid syntax in conditional ~S" fstart))
                      (otherwise
                       (setf ignoring i)
                       (next))))
                  ;; Nothing to see here... move along.
                  (when ignoring
                    (next))
                  ;; Manage the "export" keyword used outside of variable assignment
                  ;;    as well as "unexport".
                  (let ((exporting (string= "export" token)))
                    (when (or exporting
                              (string= "unexport" token))

                      ;; Export/unexport ends the previous rule.
                      (record-waiting-files)
                      ;; (un)export by itself causes everything to be (un)exported.
                      (if (zerop (length p2))
                          (setf export-all-variables exporting)
                          (let (l cp ap)
                            ;; Expand the line so we can use indirect and constructed
                            ;;    variable names in an (un)export command.
                            (setf cp (allocated-variable-expand p2))
                            (loop :for p :in (split-tokens cp)
                                  (let ((v (lookup-variable p)))
                                    (unless v
                                      (setf v (define-variable-global p "" :file 0 fstart)))
                                    (setf (variable-export v) (if exporting :export :noexport))))))
                      (next)))

                  ;; Handle the special syntax for vpath.
                  (when (string= "vpath" token)
                    (let (cp
                          (vpath nil))
                      ;; vpath ends the previous rule.
                      (record-waiting-files)
                      (setf cp (variable-expand p2))
                      (let ((tokens (split-tokens cp)))
                        (when tokens
                          (setf vpath (pop tokens)))
                        (construct-vpath-list vpath tokens)))
                    (next))

                  ;; Handle include and variants.
                  (when (or (string= "include" token)
                            (string= "-include" token)
                            (string= "sinclude" token))
                    ;; We have found an 'include' line specifying a nested
                    ;; makefile to be read at this point.  */
                    (let (save new-conditionals files
                               (noerror (char/= #\i (aref token 0))))
                      ;; Include ends the previous rule.
                      (record-waiting-files)
                      (setf p (allocated-variable-expand p2))
                      ;; If no filenames, it's a no-op.
                      (unless p (next))
                      ;; Parse the list of file names.  Don't expand archive references!
                      (setf files (parse-file-seq p))
                      )
                    (next))

                  ;; Handle the special syntax for define.
                  (when (string= "define" token)
                    (let (cp
                          (name nil)
                          (value nil))
                      ;; define ends the previous rule.
                      (record-waiting-files)
                      (setf cp (allocated-variable-expand p2))
                      (setf cp (trim-starting-spaces cp))
                      (setf name (pop (split-tokens cp)))
                      (setf value (trim-starting-spaces cp))
                      (define-variable-global name value :file 0 fstart))
                    (next))
                  
                  )))))))

;;;---------------------------------------------------------------------

(defmethod parse-makefile ((makefile pathname) &key &allow-other-keys)
  (with-open-file (input makefile)
    (parse-makefile input)))

(defmethod parse-makefile ((makefile-contents string) &key &allow-other-keys)
  (with-input-from-string (input makefile-contents)
    (parse-makefile input)))

(defmethod parse-makefile ((makefile stream) &key &allow-other-keys)
  (let ((ebuf (make-ebuffer
               :stream makefile
               :floc (make-floc :filename (namestring (pathname makefile))
                                :lineno 1
                                :offset 0))))
    (parse-makefile ebuf)))

;;;---------------------------------------------------------------------

(defun test/all ()
  (test/find-map-unquote)
  (test/readline))

(test/all)
