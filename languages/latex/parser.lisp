;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               parser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A simple latex parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-02-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2015
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

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LATEX"
  (:use "COMMON-LISP")
  (:shadow "SET"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LATEX")


(defun latex-env-name (text)
  (let* ((left  (position #\{ text))
         (right (position #\} text :start left)))
    (subseq text (1+ left) right)))

(defun test/latex-env-name ()
  (assert (string= (latex-env-name "{Hello} {world}!") "Hello"))
  (assert (string= (latex-env-name "Hello {world}!") "world"))
  (assert (string= (latex-env-name "{world}") "world"))
  (assert (string= (latex-env-name "{world") "world"))
  (assert (null (ignore-errors (latex-env-name "world")))))

(defun escape-quotes (text)
  (with-output-to-string (*standard-output*)
    (loop :for ch :across text :do
      (when (char= #\" ch) (princ "\\"))
      (princ ch))))

(defun test/escape-quotes ()
  (assert (string= (escape-quotes "Hello\"World\"!") "Hello\\\"World\\\"!"))
  (assert (string= (escape-quotes "Hello '\"' World!") "Hello '\\\"' World!"))
  (assert (string= (escape-quotes "Hello 'world'!") "Hello 'world'!"))
  (assert (string= (escape-quotes "Hello world!") "Hello world!")))

(defun get-macro-name (text)
  "Return the suffix of TEXT starting after the last backslash."
  (subseq text (1+ (or (position #\\ text :from-end t) -1))))

(defun test/get-macro-name ()
  (assert (string= (get-macro-name "\\Hello") "Hello"))
  (assert (string= (get-macro-name "\\Hello\\World") "World"))
  (assert (string= (get-macro-name "Hello") "Hello")))


(defparameter *max-include-depth* 10)


(defun set     (chars)          `(set     ,chars))
(defun set-not (chars)          `(set-not ,chars))
(defun alt     (&rest options)  `(alt     ,@options))
(defun opt     (re)             `(opt     ,re))
(defun rep+    (re)             `(rep+    ,re))
(defun rep*    (re)             `(rep*    ,re))
(defun seq     (&rest res)      `(seq     ,@res))


(defmacro scan-rules ((token-variable stream-variable)
                      &body rules)
  (let ((vstate (gensym "state"))
        (vscan-buffer (gensym "buffer")))
    `(let ((,vstate         nil)
           (,token-variable nil)
           (,vscan-buffer   (make-array 8 :element-type 'character :adjustable t :fill-pointer 0)))
       (flet ((goto (state)
                (setf ,vstate state))
              (scan-current-stream ()
                ,stream-variable)
              ((setf scan-current-stream) (new-stream)
                (setf ,stream-variable new-stream))
              (scan-current-buffer ()
                ,vscan-buffer)
              ((setf scan-current-buffer) (new-buffer)
                (assert (and (stringp new-buffer) (adjustable-array-p new-buffer) (array-has-fill-pointer-p new-buffer)))
                (setf ,vscan-buffer new-buffer))
              (scan-reset-current-buffer ()
                (setf ,vscan-buffer (make-array 8 :element-type 'character :adjustable t :fill-pointer 0))))
         (macrolet ((when-state (state &body body)
                                `(when (eq ,vstate state)
                                   ,@body))
                    (rule (regexp &body body)
                          `(when (setf ,token-variable (scan-match-regexp ,stream-variable ,regexp))
                             ,@body)))
             ,@rules)))))



(defun scan (stream &key (read-tex-dir #P".")
                      (external-format :default)
                      (if-include-fails :continue) ; or :error or some value to return from SCAN.
                      )
  (let ((token-text               "")
        (include-stack            '())

        (chapter-flag             nil)
        (section-flag             nil)
        (subsection-flag          nil)
        (subsubsection-flag       nil)
        (aparagraph-flag          nil)
        (inline-quote-flag        nil)
        (inline-math-flag         nil)
        (display-math-flag        nil)
        (table-count              0)
        (array-count              0)
        (saved-math-state         nil)
        (brace-count              nil)
        (list-environment-count   0)

        (list-stack               nil)
        (current-list             nil)
        
        (states '(:math :macro :picture))
        
        (A-Z                      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (CONTROL-M                #\return)
        (CONTROL-L                #\page)
        (ALPHABET                 (set A-Z))
        (ACCENT-ACUTE             "\\'")
        (ACCENT-GRAV              "\\`")
        (ACCENT-UMLAUT            "\\\"" )
        (ACCENT-CIRCUMFLEX        "\\^")
        (ACCENT                   (alt ACCENT-CIRCUMFLEX ACCENT-GRAV ACCENT-ACUTE ACCENT-UMLAUT))
        (FILENAME-CHAR            (set A-Z))
        (LETTER                   (alt (set A-Z) #\- ACCENT))
        (DIGIT                    (set "0123456789"))
        (NUMBER                   (rep+ DIGIT))
        (MATH-NUMBER              (seq (rep* DIGIT) (opt ".") NUMBER))
        (TEXT-NUMBER              (seq NUMBER (rep* (seq (opt ",") MATH-NUMBER))))
        (TIME-NUMBER              (seq (rep+ DIGIT) (opt ":") (rep+ digit)))
        (ALPHABET-STRING          (rep* LETTER))
        (WHITESPACE               (alt #\space #\tab #\newline))
        (H-SPACE                  (alt #\space #\tab))
        (NEWLINE                  #\newline)
        (BLANK-LINE               (seq (rep* H-SPACE) NEWLINE) )
        (PARAGRAPH-SEPARATOR      (seq NEWLINE (rep+ BLANK-LINE)))
        (PUNCT                    (set "-=_^/.,:;!?\"()@[]|<>*+'`"))
        (BACKSLASH                #\\)
        (LATEX-SPECIAL-CHAR       (set "#$%&~_^{}\\"))
        (MATH-OP                  (set "+*/'<>=-"))
        (POS-TAB-ARG              (seq "[" (rep+ (set "hbtp")) "]"))
        (OPT-TAB-ARG              (seq "{" (rep* (set "lrc|")) "}"))
        (BEGIN-ENV-OPEN           (seq BACKSLASH "begin" (rep* H-SPACE) "{"))
        (BEGIN-ENV-CLOSE          (seq "}"(rep* WHITESPACE)))
        (BEGIN-DISPLAY-MATH       "$$")
        (END-DISPLAY-MATH         "$$")

        )
    (scan-rules (token-text stream)

      (labels ((get-tex-filename-without-braces (text)
                 "Return the tex file path designated by the token-text \"filename\".
               Use variable READ-TEX-DIR."
                 (let ((dirname read-tex-dir)
                       (filename text))
                   (concatenate 'string (or dirname "") filename (if (find #\. filename) "" ".tex"))))
               (get-tex-filename (text)
                 "Return the tex file path designated by the token-text \"{filename}\".
               Use variable READ-TEX-DIR."
                 (get-tex-filename-without-braces (latex-env-name text)))
               (open-list (&rest prefix)
                 (push current-list list-stack)
                 (setf current-list (reverse prefix)))
               (close-list ()
                 (let ((new-list (nreverse current-list)))
                   (setf current-list (pop list-stack))
                   (push new-list current-list)
                   new-list))
               (add-as-string (text)
                 (push text current-list))
               (add-sexp (sexp)
                 (push sexp current-list))
               (clean-up-quotes ()
                 (when inline-quote-flag
                   (setf inline-quote-flag nil)
                   (close-list)))
               (list-env-p (text)
                 (member text '("list"
                                "exercises"
                                "Exercises"
                                "closeitemize"
                                "alphlist"
                                "Alphlist"
                                "chapterex") :test (function string=)))
               (push-include (filename)
                 (if (<= (length include-stack) *max-include-depth* )
                   (error "Includes nested too deeply.")
                   (let ((infile (open filename
                                       :external-format external-format
                                       :if-does-not-exist (if (eq if-include-fails :error)
                                                            :error
                                                            nil))))
                     (if infile
                       (case if-include-fails
                         (:continue (goto nil))
                         (:error    (error "Could not open include file ~S" filename))
                         (otherwise (return-from scan if-include-fails)))
                       (progn
                         (push (cons (scan-current-stream) (scan-current-buffer)) include-stack)
                         (setf (scan-current-stream) infile)
                         (scan-reset-current-buffer)
                         (format *trace-output* "Reading input from include file ~S~%" filename)
                         (goto :initial)))}))
               (pop-include ()
                 (when include-stack
                   (close (scan-current-stream))
                   (setf (scan-current-stream) (car (car include-stack))
                         (scan-current-buffer) (cdr (car include-stack)))
                   (pop include-stack)
                   t))
               (terminate-includes ()
                 (loop
                   :while include-stack
                   :do (pop-include)))
               (aparagraph-closer (closer)
                 (lambda ()
                     (when aparagraph-flag
                       (funcall closer)
                       (close-list)
                       (setf aparagraph-flag nil))))
               (subsubsection-closer (closer)
                 (lambda ()
                     (when subsubsection-flag
                       (funcall closer)
                       (close-list)
                       (setf subsubsection-flag nil))))
               (subsection-closer (closer)
                 (lambda ()
                     (when subsection-flag
                       (funcall closer)
                       (close-list)
                       (setf subsection-flag nil))))
               (section-closer (closer)
                 (lambda ()
                  (when section-flag
                    (funcall closer)
                    (clean-up-quotes)
                    (close-list)
                    (setf section-flag nil))))
               (open-group (flag tag close-subgroup)
                 (if flag
                   (progn
                     (funcall close-subgroup)
                     (close-list)
                     (open-list tag)
                     nil)
                   (progn
                     (clean-up-quotes)
                     (open-list tag)
                     t))))
        (unwind-protect
            (progn
              
              (rule (seq "\\end " (rep* H-SPACE))
                    (error "space after \\end can cause problems"))

              (rule "\\input"
                    (goto :include))

              (rule "\\include"
                    (goto :include))

              (when-state :include
                (rule (rep* H-SPACE)
                      #|eat the whitespace|#)
                (rule (seq "{" (rep+ (set-not (alt "}" WHITESPACE))) "}")
                      (let ((filename (get-tex-filename token-text)))
                        (push-include filename)))
                (rule (rep+ filename-char)
                      (let ((filename (get-tex-filename-without-braces token-text)))
                        (push-include filename))))

              (when-state :<<EOF>>
                (unless (pop-include)
                  (terminate)))

              ;; Dectalk abbreviations:
              (rule (alt "Jan." "Feb." "Mar." "Apr." "Aug." "Sep." "Oct." "Nov." "Dec.")
                    (add-as-string token-text))

              (rule "i.e."
                    (add-sexp '(cs "ie")))
              (rule "e.g."
                    (add-sexp '(cs "eg")))

              ;; Misc abbreviations:
              (rule (alt "Corp." "etc.")
                    (add-as-string token-text))

              ;; Initials:
              (rule (seq ALPHABET ".")
                    ;; A letter followed by a period is probably an initial.
                    (add-as-string token-text))

              ;; Salutations:
              (rule (alt "Prof." "Mr." "Mrs." "St." "PhD." "Phd." "Dr." )
                    (add-as-string token-text))

              ;; Days of the week:
              (rule (alt "Sun." "Mon." "Tue." "Wed." "Thu." "Fri." "Sat.")
                    (add-as-string token-text))

              ;; tex commands that are stripped  
              (rule (alt "\\hspace*" 
                         "\\bigskip" 
                         "\\noalign" 
                         "\\bigl" 
                         "\\bigr" 
                         "\\biggl" 
                         "\\biggr" 
                         "\\bigggl" 
                         "\\bigggr" 
                         (seq "\\thispagestyle{" (rep* (set-not #\})) "}"  )
                         "\\medskip" 
                         "\\advance" 
                         "\\leftskip" 
                         "\\rightskip" 
                         (seq "by" (rep+ digit) "pc")  
                         (seq "by" (rep+ digit) "pt") 
                         (seq "by" (rep+ digit) "in")
                         "\\protect" 
                         "\\smallskip" 
                         "\\displaystyle" 
                         "\\left" 
                         "\\right" 
                         "\\boldmath" 
                         "\\unboldmath" 
                         "\\raggedright" 
                         "\\vfil" 
                         "\\vfill" 
                         "\\hfil" 
                         "\\hfill" 
                         "\\scriptstyle" 
                         (seq "\\vskip" (rep* H-space) (opt (seq (rep+ digit) "pt")))
                         (seq "\\hspace*" (rep* H-space) (opt (seq (rep+ digit) "in")))
                         (seq "\\vspace*" (rep* H-space) (opt (seq (rep+ digit) "pt")))
                         (seq "\\vspace*" (rep* H-space) (opt (seq (rep+ digit) "in")))
                         (seq "\\kern*"  (rep* H-space) (opt (seq (rep+ digit) "pt")))
                         "\\eject" 
                         "\\maketitle" 
                         "\\noindent" 
                         "\\goodbreak" 
                         "\\," 
                         "\\!" 
                         "\\:" 
                         "\\"   
                         "\\hline" 
                         "\\ " 
                         "\\/" 
                         "\\mathstrut" 
                         "\\pagebreak" 
                         "\\linebreak" 
                         )
                    #|nothing|#)

              (rule "\\@"
                    (add-sexp '(cs "@")))
              (rule "\\ul"
                    (add-sexp '(cs "em")))
              
              ;; Some special verb commands
              (rule "\\verb|[|"
                    (add-as-string "["))
              (rule "\\verb|{|"
                    (add-as-string "{"))

              ;; special tex and latex characters */
              (rule (alt "--" "---")
                    (add-as-string token-text))
              (rule "~"
                    (add-as-string " ")); or ignore?

              (rule (alt "\\cr" "\\headrow" "\\newrow" (seq BACKSLASH BACKSLASH))
                    (if (or (plusp table-count) (plusp array-count))
                      (progn
                        (close-list)   ; End current array element.
                        (close-list)   ; End current row.
                        (open-list)    ; Now start next row
                        (open-list)))  ; and start its first element
                    (add-sexp '(newline)))
              (rule (seq backslash latex-special-char)
                    ;; print it after stripping off the backslash
                    (add-as-string (escape-quote (subseq token-text 1))))
              (rule "&"
                    (if (or (plusp table-count) (plusp array-count))
                      (progn
                        (close-list)
                        (open-list))
                      ;; output field separator so that we can handle tex matrix command
                      (add-sexp '(field-separator))))
              
              (when-state :math
                (rule "^" (add-as-string "^"))
                (rule "_" (add-as-string "_")))

              ;; Skip pictures
              (rule (seq BEGIN-ENV-OPEN "picture" BEGIN-ENV-CLOSE)
                    (goto :picture))

              (rule (seq (rep* whitespace) backslash "end{picture}" (rep* h-space))
                    (goto nil))

              (when-state :picture
                (rule (rep* (any))
                      #|ignore|#))
              
              ;; kluges for handling macro definitions
              (rule (seq backslash "def" (rep* whitespace) backslash (rep* letter))
                    ;; handle macro definitions
                    (add-sexp `(cs "def" ,(get-macro-name token-text)))
                    ;; start arg list
                    (open-list 'arglist)
                    (goto :macro))

              (rule (seq "#" digit)
                    ;; Convert #1 to arg1 etc.
                    (add-as-string (format nil "arg~C" (aref token-text 1))))

              (when-state :macro
                (rule "{"
                      ;; ending of arg list seen so mark it and remember
                      (close-list)
                      (open-list 'block)
                      (goto nil)))

              ;; braces start groups

              (when-state :math
                (rule "{"
                      (incf brace-count)
                      (open-list 'subformula)))

              (rule "{"
                    (incf brace-count)
                    (open-list 'block))

              (rule "}"
                    (decf brace-count)
                    (close-list)
                    (when (zerop brace-count)
                      ;; reset state to math if necessary
                      (case saved-math-state
                        ((nil))
                        ((:inline-math)
                         (setf inline-math-flag t
                               saved-math-state nil)
                         (goto :math))
                        ((:display-math)
                         (setf display-math-flag t
                               saved-math-state nil)
                         (goto :math))
                        (otherwise
                         (error "Unknown saved-math-state ~S" saved-math-state)))))

              ;; hbox and mbox change state 
              (rule (alt (seq backslash "fbox" (rep* h_space) "{")
                         (seq backslash "hbox" (rep* h_space) "{")
                         (seq backslash "mbox" (rep* h_space) "{"))
                    (setf brace-count 1) ; reset brace count
                    ;;  When brace_count reaches 0 we have seen the
                    ;;  matching close brace and can close the hbox.
                    ;;  Both hbox and mbox marked as mbox and will be
                    ;;  processed as if they were user defined macros
                    ;;  ie: using define-text-object  
                    (clean-up-quotes)
                    ;; first set up state
                    (cond
                      (display-math-flag
                       (setf display-math-flag nil
                             inline-math-flag nil ; kludge
                             saved-math-state :display-math)
                       (goto nil))
                      (inline-math-flag
                       (setf display-math-flag nil ; kludge
                             inline-math-flag nil 
                             saved-math-state :inline-math)
                       (goto nil)))
                    (add-sexp `(cs "mbox"))
                    (open-list 'block))

              ;; Brackets in latex
              (when-state :math
                (rule "[" (add-as-string "["))
                (rule "]" (add-as-string "]")))

              (rule "[" ; optional args in a block
                    (open-list 'block))
              
              (rule "]"
                    (close-list))

              ;; paragraph breaks
              (rule (alt (seq backslash "par")
                         (seq backslash "paragraph")
                         PARAGRAPH-SEPARATOR)
                    ;; Don't put parbreaks inside math.
                    (unless (or inline-math-flag display-math-flag)
                      (clean-up-quotes)
                      ;; Paragraph delimiter is a newline followed by
                      ;; an arbitrary number blank lines, where a
                      ;; blank is defined as a line with an arbitrary
                      ;; amount of optional h-space followed by a
                      ;; newline.  Close apar if one opened.
                      (when aparagraph-flag
                        (close-list)
                        (setf aparagraph-flag nil))
                      (add-sexp 'parbreak)))

              ;; Beginning and ending math mode

              (rule "\\["
                    (clean-up-quotes)
                    (setf display-math-flag t)
                    (open-list 'display-math)
                    (goto :math))

              (rule "\\]"
                    (setf display-math-flag nil)
                    (close-list)
                    (goto nil))
              
              (rule "$$"
                    (when inline-math-flag
                      (error "Display math started inside inline math? Probably an inline math was closed and immediately opened. Check the latex file."))
                    (if display-math-flag
                      (progn
                        (setf display-math-flag nil)
                        (close-list)
                        (goto nil))
                      (progn
                        (clean-up-quotes)
                        (setf display-math-flag t)
                        (open-list 'display-math)
                        (goto :math))))

              (rule "\\("
                    (setf inline-math-flag t)
                    (open-list 'inline-math)
                    (goto :math))

              (rule "\\)"
                    (setf inline-math-flag nil)
                    (close-list)
                    (goto :math))

              (rule "$"
                    (if inline-math-flag
                      (progn
                        (setf inline-math-flag nil)
                        (close-list)
                        (goto nil))
                      (progn
                        (setf inline-math-flag t)
                        (open-list 'inline-math)
                        (goto :math))))

              ;; Math operators
              (when-state :math

                (rule "'" ; catch single quote in math mode
                      (add-as-string "prime"))

                (rule (seq "''" (rep* h-space)) ; catch double prime in math mode
                      (add-as-string "double-prime"))

                (rule MATH-OP
                      (add-as-string token-text)))

              ;; tex comment
              (rule (seq "%" (rep* (any))) ; Latex comments run to the end of the line.
                    ;; Not doing anything with comments, so throw them away.
                    ;; (open-list 'comment)
                    ;; (add-as-string (escape-quotes token-text))
                    ;; (close-list)
                    )

              ;; begin various environments

              (rule (seq begin-env-open (alt "document" "abstract" "center") begin-env-close)
                    (open-list (intern (latex-env-name token-text))))

              (rule (seq begin-env-open (alt "quote" "quotation" "verbatim") begin-env-close)
                    (clean-up-quotes)
                    (open-list (intern (latex-env-name token-text))))

              (rule (seq begin-env-open (alt "description" "deflist" "enumerate" "itemize") begin-env-close)
                    (clean-up-quotes)
                    (incf list-environment-count)
                    (open-list (intern (latex-env-name token-text)))
                    ;; Generate dummy item
                    ;; this is to allow \item to be handled cleanly
                    (open-list 'item))

              (rule (seq (rep* whitespace) backslash "item")
                    (clean-up-quotes)
                    ;; begin a new item after ending previous item
                    (close-list)
                    (when (zerop list-environment-count)
                      (error "An item was found outside known list environment."))
                    ;; Note this is a quick fix,
                    ;; and will leave a null list as the first item of each enumerated list
                    (open-list 'item))

              ;; begin equation
              (rule (seq begin-env-open "equation" begin-env-close)
                    (clean-up-quotes)
                    (open-list 'equation)
                    (setf display-math-flag t)
                    (goto :math))

              ;; begin eqnarray
              (rule (seq begin-env-open (alt "eqnarray*" "eqnarray" "eqalign" "eqalign*") begin-env-close)
                    ;; starting an eqnarray or an eqalign
                    (incf array-count)
                    (setf display-math-flag t)
                    (goto :math)
                    (open-list (intern (string-trim "*" (latex-env-name token-text))))
                    (open-list) ; start the first eqnarray row
                    (open-list)) ; start the first eqnarray element

              (rule (seq begin-env-open (opt "tabular" "array") "}"
                         (opt POS-TAB-ARG) (opt OPT_TAB_ARG) (rep* WHITESPACE))
                    (clean-up-quotes)
                    (let ((op (intern (string-trim "*" (latex-env-name token-text)))))
                      (if (eq op 'tabular)
                        (incf table-count)
                        (incf array-count))
                      (open-list op))
                    (open-list) ; start the first table row
                    (open-list)) ; start the first table element

              ;; begin cases
              
              ;; Cases handled like table environment.  Allow for text
              ;; inside math mode by saving state. If this works, use
              ;; similar approach for mbox hbox etc. 

              (rule (seq begin-env-open "cases" "}" (opt POS-TAB-ARG) (rep* WHITESPACE))
                    (clean-up-quotes)
                    (cond
                      (display-math-flag
                       (setf display-math-flag nil
                             saved-math-state :display-math)
                       (goto nil))
                      (inline-math-flag
                       (setf inline-math-flag nil
                             saved-math-state :inline-math)
                       (goto nil)))
                    (incf table-count)
                    (open-list 'cases)
                    (open-list) ; start the first table row
                    (open-list)) ; start the first table element

              (rule (alt (seq (rep* blank-line) begin-env-open "slide}{}")
                         (seq begin-env-open "slide" begin-env-close))
                    (clean-up-quotes)
                    ;; starting a latex slide
                    (open-list 'slide))

              (rule (seq begin-env-open "displaymath" begin-env-close)
                    (clean-up-quotes)
                    (setd display-math-flag t)
                    (open-list 'display-math)
                    (goto :math))

              ;; unrecognized environment               
              (rule (alt (seq begin-env-open alphabet-string "}" (opt pos-tab-arg) (opt pos-tab-arg))
                         (seq begin-env-open (rep* (set-not "}")) "}")) ; environment names can have more that alphabets
                    (clean-up-quotes)
                    ;; Some new environments maybe declared as
                    ;; enumerable by adding their name to
                    ;; list_env_names.  Handle this by checking if the
                    ;; env name present in the table.
                    (let ((name (latex-env-name token-text)))
                      (if (list-env-p name)
                        (progn
                          (open-list 'new-environment name)
                          ;; expect items in this env
                          (incf list-environment-count)
                          ;; generate dummy item
                          ;; this is to allow \item to be handled cleanly
                          (open-list 'item))
                        (open-list 'new-environment name))))

              (rule (seq (rep* whitespace) backslash (alt "grieschapter" "chapter*" "chapterx" "chapter"))
                    (setf chapter-flag (open-group chapter-flag 'chapter
                                                   (section-closer
                                                    (subsection-closer
                                                     (subsubsection-closer
                                                      (lambda ())))))))
              (rule (seq (rep* whitespace) backslash (alt "griessection" "section*" "section"))
                    (setf section-flag (open-group section-flag 'section
                                                   (subsection-closer
                                                    (subsubsection-closer
                                                     (lambda ()))))))
              (rule (seq (rep* whitespace) backslash (alt "subsection*" "subsection"))
                    (setf subsection-flag (open-group subsection-flag 'subsection
                                                      (subsubsection-closer
                                                       (lambda ())))))
              (rule (seq (rep* whitespace) backslash (alt "subsubsection*" "subsubsection"))
                    (setf subsubsection-flag (open-group subsubsection-flag 'subsubsection
                                                         (lambda ()))))

              
              ;; absolute sectioning constructs
              (rule (seq (rep* whitespace) backslash "achapter")
                    (setf chapter-flag (open-group chapter-flag 'achapter
                                                   (section-closer
                                                    (subsection-closer
                                                     (subsubsection-closer
                                                      (aparagraph-closer
                                                       (lambda ()))))))))              
              (rule (seq (rep* whitespace) backslash  "asection")
                    (setf section-flag (open-group section-flag 'asection
                                                   (subsection-closer
                                                    (subsubsection-closer
                                                     (aparagraph-closer
                                                      (lambda ())))))))
              (rule (seq (rep* whitespace) backslash "asubsection")
                    (setf subsection-flag (open-group subsection-flag 'asubsection
                                                      (subsubsection-closer
                                                       (aparagraph-closer
                                                        (lambda ()))))))
              (rule (seq (rep* whitespace) backslash "asubsubsection")
                    (setf subsubsection-flag (open-group subsubsection-flag 'asubsubsection
                                                         (aparagraph-closer
                                                          (lambda ())))))
              (rule (seq (rep* whitespace) backslash "apar")
                    (setf aparagraph-flag (open-group aparagraph-flag 'apar
                                                      (lambda ()))))

              ;; end various environments

              (rule (seq (rep* whitespace) backslash "end{abstract}" (rep* h-space))
                    (clean-up-quotes)
                    (when aparagraph-flag
                      (close-list)
                      (setf aparagraph-flag nil))
                    (close-list))

              (rule (seq (rep* whitespace) backslash "end{" (alt "center" "quote" "quotation") "}" (rep* h-space))
                    (clean-up-quotes)
                    (close-list))

              (rule (seq (rep* whitespace) backslash "end{equation}" (rep* h-space))
                    (clean-up-quotes)
                    (close-list)
                    (setf display-math-flag nil)
                    (goto nil))

              (rule (seq (rep* whitespace) backslash "end{"
                         (alt "eqnarray*" "eqnarray" "eqalign*" "eqalign") "}" (rep* h-space))
                    (decf array-count)
                    (close-list)
                    (close-list)
                    (close-list)
                    (setf display-math-flag nil)
                    (goto nil))

              (rule (seq (rep* whitespace) backslash "end{array}" (rep* h-space))
                    (decf array-count)
                    (close-list)
                    (close-list)
                    (close-list))

              (rule (seq (rep* whitespace) backslash "end{tabular}" (rep* h-space))
                    (decf table-count)
                    (close-list)
                    (close-list)
                    (close-list))

              ;;  Cases handled like tabular
              
              (rule (seq (rep* whitespace) backslash "end{tabular}" (rep* h-space))
                    (decf table-count)
                    (close-list)
                    (close-list)
                    (close-list)
                    (case saved-math-state
                      ((nil))
                      ((:inline-math)
                       (setf inline-math-flag t
                             saved-math-state nil)
                       (goto :math))
                      ((:display-math)
                       (setf display-math-flag t
                             saved-math-state nil)
                       (goto :math))
                      (otherwise (error "Unknown saved-math-state ~S" saved-math-state))))

              (rule (seq (rep* whitespace) backslash "end{" (alt "enumerate" "description" "itemize") "}" (rep* h-space))
                    (clean-up-quotes)
                    (decf list-environment-count)
                    (close-list)
                    (close-list))

              (rule (seq (rep* whitespace) backslash "end{document}")
                    (clean-up-quotes)
                    (when aparagraph-flag
                      (setf aparagraph-flag nil)
                      (close-list))
                    (when subsubsection-flag
                      (setf subsubsection-flag nil)
                      (close-list))
                    (when subsection-flag
                      (setf subsection-flag nil)
                      (close-list))
                    (when section-flag
                      (setf section-flag nil)
                      (close-list))
                    (when chapter-flag
                      (setf chapter-flag nil)
                      (close-list))
                    (close-list))

              (rule (seq (rep* whitespace) backslash "end{slide}")
                    (clean-up-quotes)
                    (close-list))

              (rule (seq (rep* whitespace) backslash "end{displaymath}")
                    (close-list)
                    (setf display-math-flag nil)
                    (goto nil))

              (rule (seq backslash "end{" (alt (seq alphabet-string)
                                               (seq (rep* (set-not "}")))) (opt "*") "}")
                    (clean-up-quotes)
                    (let ((name (latex-env-name token-text)))
                      (if (list-env-p name)
                        (progn
                          (decf list-environment-count)
                          (close-list)
                          (close-list))
                        (close-list))))              

              ;; tex control sequences eg macro names
              
              (rule (seq backslash (set-not a-z))
                    ;; handle single characters with a backslash in front eg \. etc.
                    (add-as-string (escape-quotes (subseq token-text 1))))

              (when-state :math
                (rule (seq backslash (rep* alphabet))
                      (open-list 'math-cs (subseq token-text 1))
                      (close-list)))

              (rule (seq backslash (rep* alphabet))
                    (open-list 'cs (subseq token-text 1))
                    (close-list))

              ;; be smart about numbers

              (when-state :math
                (rule (seq backslash math-number)
                      (open-list 'math-number token-text)
                      (close-list)))

              (rule time-number
                    ;; Dectalk speaks time numbers correctly
                    (add-as-string token-text))

              (rule text-number
                    (open-list 'text-number token-text)
                    (close-list))
              
              ;; words handled according to mode
              (when-state :math
                (rule (seq "''" (rep* h-space))
                      (add-as-string "double-prime")))

              (rule (alt alphabet
                         (seq alphabet (rep* letter) (opt "'") (rep+ alphabet)))
                    (if (or inline-math-flag display-math-flag)
                      ;; In math mode, the string should be broken up
                      ;; into strings of one character ie "a+b" is "a"
                      ;; "+" "b" since TeX allows for only plain
                      ;; single letter variables
                      (loop :for ch :across token-text
                        :do (add-as-string (string ch)))
                      ;; Convert text to strings.  Not escape quotes
                      ;; since umlaut now handled as a letter if this
                      ;; causes trouble, reintroduce escape_quote as
                      ;; in.
                      (add-as-string token-text)))

              (rule (seq (rep* h-space) "\"")
                    (if inline-quote-flag
                      (reject)
                      (progn
                        (setf inline-quote-flag t)
                        (open-list 'inline-quote))))

              (rule (seq (rep* h-space) "``")
                    (unless inline-quote-flag
                      (setf inline-quote-flag t)
                      (open-list 'inline-quote)))

              (rule (seq (alt "\"" "''") (rep* h-space))
                    ;; matching " here is a concession
                    (if inline-quote-flag
                      (progn ; Marking matched inline-quote
                        (add-as-string "''")
                        (close-list)
                        (setf inline-quote-flag nil))
                      (progn ; This does not match a quotation, so just put it in the text:
                        (add-as-string (escape-quotes token-text)))))
              
              (rule PUNCT
                    (add-as-string (escape-quotes token-text)))
              
              (rule (rep* h-space)
                    (add-as-string token-text))

              ;; Trap things that are not caught and echo to stderr
              (rule (alt control-m control-l)
                    #|ignore|#)

              (rule "#"
                    (add-as-string "#"))
              
              (rule (any)
                    (error "This escaped ~S" token-text)))
          (terminate-includes))))))


(defun read-token (stream)
  (let ((ch (read-char stream)))
    (case ch
      ((#\\))
      ((#\%))
      (otherwise
       (loop
         :with text = (make-array 4 :element-type 'character :adjustable t :fill-pointer 0)
         :while (and (char/= #\\ ch) (char/= #\% ch))
         :do (vector-push-extend ch text (length text))
         :finally (unread-char ch stream) (return text))))))


(with-open-file (latex #P"~/library/informatique/standards-and-protocol/cplusplus-draft/source/grammar.tex")
  (loop
    :for token = (read-token latex)
    :while token
    :do (print token)))


(defun getenv (var)
  #+clisp (ext:getenv var)
  #+ccl   (ccl:getenv var)
  #-(or clisp ccl) (error "Please implement getenv in ~A" (lisp-implementation-type)))

(scan stream (getenv "READ_TEX_DIR"))
