
(defstruct floc
  filename
  lineno
  offset)

(defstruct ebuffer
  buffer
  current-line-pos
  next-line-pos
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

(defun trim-starting-spaces (string)
  (left-trim-string string #(#\space #\tab #\page #\vt)))

(defun parse-makefile (ebuf set-default)
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
        (fi (make-floc)))
    (flet ((record-waiting-files ()
	         (when filenames
	           (setf (floc-lineno fi) targets-started
                     (floc-offset fi) 0)
               (record-files filenames also-make-targets
                             pattern pattern-percen destr
                             commands-started commands-index commands
                             two-colon prefix fi)
               (setf filenames nil))
             (setf commands-index 0
                   no-targets nil
                   pattern nil
                   also-make-targets nil)))

      (setf pattern-precen nil
            commands-started t
            targets-started  t
	        fstart (ebuffer->flock ebuf)
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
              (flet ((next () (next)))
	            (let ((linelen 0)
                      line
                      (wlen 0)
                      p p2
                      (vmod (make-vmodifiers)))
                  ;; At the top of this loop, we are starting a brand new line.
                  ;; Grab the next line to be evaluated

                  (incf (floc-lineno ebuf) nlines)
                  (setf nlines (readlines ebuf))
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
    	          (when (char= command-prefix (aref line 0))

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
                        (setf variable-private-var v) t)
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
                            (setf vpat (pop tokens)))
                        (construct-vpah-list vpath tokens)))
                    (next))

                  

                  )))))))
