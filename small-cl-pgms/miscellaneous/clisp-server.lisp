#!/usr/local/bin/clisp -q -ansi  -on-error debug
;;;;**************************************************************************
;;;;FILE:               clisp-server.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This script implements a CL server and client.
;;;;
;;;;    The server (with the --server option)  waits for connections
;;;;    on port +PORT+ and then read one form, evaluates it, and sends 
;;;;    back the data printed and the multiple value resulting from 
;;;;    the form, or the error if raised.
;;;;
;;;;    The client either sends to the server the forms given on 
;;;;    the command line, or enter a REPL mode (with the --repl option),
;;;;    from which it reads forms, and sends them to the server.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-04-09 <PJB> 
;;;;BUGS
;;;;
;;;;    - The current protocol disconnect between each form evaluated.
;;;;    - We should use the SWANK protocol 
;;;;      (fetch swank client from McClimDesktop).
;;;;    - The restricted cl package is not free of security leaks yet.
;;;;    - It's planed to implement a virtual file system from which
;;;;      the file and stream operators would work.
;;;;    - It's planed to have a multi-server client, where the same forms
;;;;      are sent to various implementations of CL and the results
;;;;      sent back are compared and implementation dependent results
;;;;      are highlighed.
;;;;
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2006 - 2015
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
;;;;**************************************************************************
(eval-when (:compile-topleve :load-toplevel :execute)
  (setf custom:*load-echo* t))

(defpackage "RESTRICTED-COMMON-LISP"
  (:nicknames "RCL")
  (:use "COMMON-LISP")
  (:shadow "LIST-ALL-PACKAGES" "FIND-PACKAGE"))

(in-package "RESTRICTED-COMMON-LISP")

(defconstant *rcl-exports*
  '(
    &allow-other-keys &aux &body  &environment &key &optional &rest &whole

    ;; *BREAK-ON-SIGNALS*
    *compile-file-pathname*
    *compile-file-truename*
    *compile-print*
    *compile-verbose*
    ;; *DEBUG-IO*
    ;; *DEBUGGER-HOOK*
    *default-pathname-defaults*
    *error-output*
    *features*
    *gensym-counter*
    ;; *LOAD-PATHNAME*
    *load-print*
    ;; *LOAD-TRUENAME*
    *load-verbose*
    *macroexpand-hook*
    ;; *MODULES*
    *package*
    *print-array*
    *print-base*
    *print-case*
    *print-circle*
    *print-escape*
    *print-gensym*
    *print-length*
    *print-level*
    *print-lines*
    *print-miser-width*
    *print-pprint-dispatch*
    *print-pretty*
    *print-radix*
    *print-readably*
    *print-right-margin*
    ;; *QUERY-IO*
    *random-state*
    ;; *READ-BASE*
    ;; *READ-DEFAULT-FLOAT-FORMAT*
    ;; *READ-EVAL*
    ;; *READ-SUPPRESS*
    ;; *READTABLE*
    *standard-input*
    *standard-output*
    ;; *TERMINAL-IO*
    ;; *TRACE-OUTPUT*

    * ** ***
    + ++ +++
    -
    / // ///
    = /= < <= > >=
    1+ 1-


    abort
    abs
    acons
    acos
    acosh
    add-method
    adjoin
    adjust-array
    adjustable-array-p
    allocate-instance
    alpha-char-p
    alphanumericp
    and
    append
    apply
    apropos
    apropos-list
    aref
    arithmetic-error
    arithmetic-error-operands
    arithmetic-error-operation
    array
    array-dimension
    array-dimension-limit
    array-dimensions
    array-displacement
    array-element-type
    array-has-fill-pointer-p
    array-in-bounds-p
    array-rank
    array-rank-limit
    array-row-major-index
    array-total-size
    array-total-size-limit
    arrayp
    ash
    asin
    asinh
    assert
    assoc
    assoc-if
    assoc-if-not
    atan
    atanh
    atom
    base-char
    base-string
    bignum
    bit
    bit-and
    bit-andc1
    bit-andc2
    bit-eqv
    bit-ior
    bit-nand
    bit-nor
    bit-not
    bit-orc1
    bit-orc2
    bit-vector
    bit-vector-p
    bit-xor
    block
    boole
    boole-1
    boole-2
    boole-and
    boole-andc1
    boole-andc2
    boole-c1
    boole-c2
    boole-clr
    boole-eqv
    boole-ior
    boole-nand
    boole-nor
    boole-orc1
    boole-orc2
    boole-set
    boole-xor
    boolean
    both-case-p
    boundp
    break
    broadcast-stream
    broadcast-stream-streams
    built-in-class
    butlast
    byte
    byte-position
    byte-size
    caaaar
    caaadr
    caaar
    caadar
    caaddr
    caadr
    caar
    cadaar
    cadadr
    cadar
    caddar
    cadddr
    caddr
    cadr
    call-arguments-limit
    call-method
    call-next-method
    car
    case
    catch
    ccase
    cdaaar
    cdaadr
    cdaar
    cdadar
    cdaddr
    cdadr
    cdar
    cddaar
    cddadr
    cddar
    cdddar
    cddddr
    cdddr
    cddr
    cdr
    ceiling
    cell-error
    cell-error-name
    cerror
    change-class
    char
    char-code
    char-code-limit
    char-downcase
    char-equal
    char-greaterp
    char-int
    char-lessp
    char-name
    char-not-equal
    char-not-greaterp
    char-not-lessp
    char-upcase
    char/=
    char<
    char<=
    char=
    char>
    char>=
    character
    characterp
    check-type
    cis
    class
    class-name
    class-of
    clear-input
    clear-output
    close
    clrhash
    code-char
    coerce
    compilation-speed
    compile
    compile-file
    compile-file-pathname
    compiled-function
    compiled-function-p
    compiler-macro
    compiler-macro-function
    complement
    complex
    complexp
    compute-applicable-methods
    compute-restarts
    concatenate
    concatenated-stream
    concatenated-stream-streams
    cond
    condition
    conjugate
    cons
    consp
    constantly
    constantp
    continue
    control-error
    copy-alist
    copy-list
    copy-pprint-dispatch
    copy-readtable
    copy-seq
    copy-structure
    copy-symbol
    copy-tree
    cos
    cosh
    count
    count-if
    count-if-not
    ctypecase
    debug
    decf
    declaim
    declaration
    declare
    decode-float
    decode-universal-time
    defclass
    defconstant
    defgeneric
    define-compiler-macro
    define-condition
    define-method-combination
    define-modify-macro
    define-setf-expander
    define-symbol-macro
    defmacro
    defmethod
    defpackage
    defparameter
    defsetf
    defstruct
    deftype
    defun
    defvar
    delete
    delete-duplicates
    ;; DELETE-FILE
    delete-if
    delete-if-not
    delete-package
    denominator
    deposit-field
    describe
    describe-object
    destructuring-bind
    digit-char
    digit-char-p
    ;; DIRECTORY
    directory-namestring
    disassemble
    division-by-zero
    do
    do*
    do-all-symbols
    do-external-symbols
    do-symbols
    documentation
    dolist
    dotimes
    double-float
    double-float-epsilon
    double-float-negative-epsilon
    dpb
    ;; DRIBBLE
    dynamic-extent
    ecase
    echo-stream
    echo-stream-input-stream
    echo-stream-output-stream
    ;; ED
    eighth
    elt
    encode-universal-time
    end-of-file
    endp
    enough-namestring
    ;; ENSURE-DIRECTORIES-EXIST
    ensure-generic-function
    eq
    eql
    equal
    equalp
    error
    etypecase
    ;; EVAL
    eval-when
    evenp
    every
    exp
    export
    expt
    extended-char
    fboundp
    fceiling
    fdefinition
    ffloor
    fifth
    file-author
    file-error
    file-error-pathname
    file-length
    file-namestring
    file-position
    file-stream
    file-string-length
    file-write-date
    fill
    fill-pointer
    find
    ;; FIND-ALL-SYMBOLS
    find-class           ; IS PROBABLY A SECURITY LEAK (Gray streams?)
    find-if
    find-if-not
    find-method
    find-package                        ; shadowed
    find-restart
    ;; FIND-SYMBOL
    finish-output
    first
    fixnum
    flet
    float
    float-digits
    float-precision
    float-radix
    float-sign
    floating-point-inexact
    floating-point-invalid-operation
    floating-point-overflow
    floating-point-underflow
    floatp
    floor
    fmakunbound
    force-output
    format
    formatter
    fourth
    fresh-line
    fround
    ftruncate
    ftype
    funcall
    function
    function-keywords
    function-lambda-expression
    functionp
    gcd
    generic-function
    gensym
    gentemp
    get
    get-decoded-time
    get-dispatch-macro-character
    get-internal-real-time
    get-internal-run-time
    get-macro-character
    get-output-stream-string
    get-properties
    get-setf-expansion
    get-universal-time
    getf
    gethash
    go
    graphic-char-p
    handler-bind
    handler-case
    hash-table
    hash-table-count
    hash-table-p
    hash-table-rehash-size
    hash-table-rehash-threshold
    hash-table-size
    hash-table-test
    host-namestring
    identity
    if
    ignorable
    ignore
    ignore-errors
    imagpart
    import
    ;; IN-PACKAGE
    incf
    initialize-instance
    inline
    input-stream-p
    ;; INSPECT
    integer
    integer-decode-float
    integer-length
    integerp
    interactive-stream-p
    ;; INTERN
    internal-time-units-per-second
    intersection
    invalid-method-error
    invoke-debugger
    invoke-restart
    invoke-restart-interactively
    isqrt
    keyword
    keywordp
    labels
    lambda
    lambda-list-keywords
    lambda-parameters-limit
    last
    lcm
    ldb
    ldb-test
    ldiff
    least-negative-double-float
    least-negative-long-float
    least-negative-normalized-double-float
    least-negative-normalized-long-float
    least-negative-normalized-short-float
    least-negative-normalized-single-float
    least-negative-short-float
    least-negative-single-float
    least-positive-double-float
    least-positive-long-float
    least-positive-normalized-double-float
    least-positive-normalized-long-float
    least-positive-normalized-short-float
    least-positive-normalized-single-float
    least-positive-short-float
    least-positive-single-float
    length
    let
    let*
    lisp-implementation-type
    lisp-implementation-version
    list
    list*
    list-all-packages                   ; shadowed
    list-length
    listen
    listp
    ;; LOAD
    load-logical-pathname-translations
    load-time-value
    locally
    log
    logand
    logandc1
    logandc2
    logbitp
    logcount
    logeqv
    logical-pathname
    logical-pathname-translations
    logior
    lognand
    lognor
    lognot
    logorc1
    logorc2
    logtest
    logxor
    long-float
    long-float-epsilon
    long-float-negative-epsilon
    long-site-name
    loop
    loop-finish
    lower-case-p
    machine-instance
    machine-type
    machine-version
    macro-function
    macroexpand
    macroexpand-1
    macrolet
    make-array
    make-broadcast-stream
    make-concatenated-stream
    make-condition
    make-dispatch-macro-character
    make-echo-stream
    make-hash-table
    make-instance
    make-instances-obsolete
    make-list
    make-load-form
    make-load-form-saving-slots
    make-method
    ;; MAKE-PACKAGE
    ;; MAKE-PATHNAME
    make-random-state
    make-sequence
    make-string
    make-string-input-stream
    make-string-output-stream
    make-symbol
    make-synonym-stream
    make-two-way-stream
    makunbound
    map
    map-into
    mapc
    mapcan
    mapcar
    mapcon
    maphash
    mapl
    maplist
    mask-field
    max
    member
    member-if
    member-if-not
    merge
    ;; MERGE-PATHNAMES
    method
    method-combination
    method-combination-error
    method-qualifiers
    min
    minusp
    mismatch
    mod
    most-negative-double-float
    most-negative-fixnum
    most-negative-long-float
    most-negative-short-float
    most-negative-single-float
    most-positive-double-float
    most-positive-fixnum
    most-positive-long-float
    most-positive-short-float
    most-positive-single-float
    muffle-warning
    multiple-value-bind
    multiple-value-call
    multiple-value-list
    multiple-value-prog1
    multiple-value-setq
    multiple-values-limit
    name-char
    ;; NAMESTRING
    nbutlast
    nconc
    next-method-p
    nil
    nintersection
    ninth
    no-applicable-method
    no-next-method
    not
    notany
    notevery
    notinline
    nreconc
    nreverse
    nset-difference
    nset-exclusive-or
    nstring-capitalize
    nstring-downcase
    nstring-upcase
    nsublis
    nsubst
    nsubst-if
    nsubst-if-not
    nsubstitute
    nsubstitute-if
    nsubstitute-if-not
    nth
    nth-value
    nthcdr
    null
    number
    numberp
    numerator
    nunion
    oddp
    ;; OPEN
    open-stream-p
    optimize
    or
    otherwise
    output-stream-p
    package
    package-error
    package-error-package
    package-name
    package-nicknames
    package-shadowing-symbols
    package-use-list
    package-used-by-list
    packagep
    pairlis
    parse-error
    parse-integer
    parse-namestring
    pathname
    pathname-device
    pathname-directory
    pathname-host
    pathname-match-p
    pathname-name
    pathname-type
    pathname-version
    pathnamep
    peek-char
    phase
    pi
    plusp
    pop
    position
    position-if
    position-if-not
    pprint
    pprint-dispatch
    pprint-exit-if-list-exhausted
    pprint-fill
    pprint-indent
    pprint-linear
    pprint-logical-block
    pprint-newline
    pprint-pop
    pprint-tab
    pprint-tabular
    prin1
    prin1-to-string
    princ
    princ-to-string
    print
    print-not-readable
    print-not-readable-object
    print-object
    print-unreadable-object
    ;; PROBE-FILE
    proclaim
    prog
    prog*
    prog1
    prog2
    progn
    program-error
    progv
    provide
    psetf
    psetq
    push
    pushnew
    quote
    random
    random-state
    random-state-p
    rassoc
    rassoc-if
    rassoc-if-not
    ratio
    rational
    rationalize
    rationalp
    read
    read-byte
    read-char
    read-char-no-hang
    read-delimited-list
    read-from-string
    read-line
    read-preserving-whitespace
    read-sequence
    reader-error
    readtable
    readtable-case
    readtablep
    real
    realp
    realpart
    reduce
    reinitialize-instance
    rem
    remf
    remhash
    remove
    remove-duplicates
    remove-if
    remove-if-not
    remove-method
    remprop
    rename-file
    rename-package
    replace
    require
    rest
    restart
    restart-bind
    restart-case
    restart-name
    return
    return-from
    revappend
    reverse
    room
    rotatef
    round
    row-major-aref
    rplaca
    rplacd
    safety
    satisfies
    sbit
    scale-float
    schar
    search
    second
    sequence
    serious-condition
    set
    set-difference
    set-dispatch-macro-character
    set-exclusive-or
    set-macro-character
    set-pprint-dispatch
    set-syntax-from-char
    setf
    setq
    seventh
    shadow
    shadowing-import
    shared-initialize
    shiftf
    short-float
    short-float-epsilon
    short-float-negative-epsilon
    short-site-name
    signal
    signed-byte
    signum
    simple-array
    simple-base-string
    simple-bit-vector
    simple-bit-vector-p
    simple-condition
    simple-condition-format-arguments
    simple-condition-format-control
    simple-error
    simple-string
    simple-string-p
    simple-type-error
    simple-vector
    simple-vector-p
    simple-warning
    sin
    single-float
    single-float-epsilon
    single-float-negative-epsilon
    sinh
    sixth
    sleep
    slot-boundp
    slot-exists-p
    slot-makunbound
    slot-missing
    slot-unbound
    slot-value
    software-type
    software-version
    some
    sort
    space
    special
    special-operator-p
    speed
    sqrt
    stable-sort
    standard
    standard-char
    standard-char-p
    standard-class
    standard-generic-function
    standard-method
    standard-object
    step
    storage-condition
    store-value
    stream
    stream-element-type
    stream-error
    stream-error-stream
    stream-external-format
    streamp
    string
    string-capitalize
    string-downcase
    string-equal
    string-greaterp
    string-left-trim
    string-lessp
    string-not-equal
    string-not-greaterp
    string-not-lessp
    string-right-trim
    string-stream
    string-trim
    string-upcase
    string/=
    string<
    string<=
    string=
    string>
    string>=
    stringp
    structure
    structure-class
    structure-object
    style-warning
    sublis
    subseq
    subsetp
    subst
    subst-if
    subst-if-not
    substitute
    substitute-if
    substitute-if-not
    subtypep
    svref
    sxhash
    symbol
    symbol-function
    symbol-macrolet
    symbol-name
    symbol-package
    symbol-plist
    symbol-value
    symbolp
    synonym-stream
    synonym-stream-symbol
    t
    tagbody
    tailp
    tan
    tanh
    tenth
    terpri
    the
    third
    throw
    time
    trace
    translate-logical-pathname
    translate-pathname
    tree-equal
    truename
    truncate
    two-way-stream
    two-way-stream-input-stream
    two-way-stream-output-stream
    type
    type-error
    type-error-datum
    type-error-expected-type
    type-of
    typecase
    typep
    unbound-slot
    unbound-slot-instance
    unbound-variable
    undefined-function
    unexport
    unintern
    union
    unless
    unread-char
    unsigned-byte
    untrace
    unuse-package
    unwind-protect
    update-instance-for-different-class
    update-instance-for-redefined-class
    upgraded-array-element-type
    upgraded-complex-part-type
    upper-case-p
    use-package
    use-value
    user-homedir-pathname
    values
    values-list
    variable
    vector
    vector-pop
    vector-push
    vector-push-extend
    vectorp
    warn
    warning
    when
    wild-pathname-p
    with-accessors
    with-compilation-unit
    with-condition-restarts
    with-hash-table-iterator
    with-input-from-string
    ;; WITH-OPEN-FILE
    ;; WITH-OPEN-STREAM
    with-output-to-string
    ;; WITH-PACKAGE-ITERATOR
    with-simple-restart
    with-slots
    with-standard-io-syntax
    write
    write-byte
    write-char
    write-line
    write-sequence
    write-string
    write-to-string
    y-or-n-p
    yes-or-no-p
    zerop
    ))

(export *rcl-exports*)

(defpackage "RESTRICTED-COMMON-LISP-USER"
  (:nicknames "RCL-USER")
  (:use  "RESTRICTED-COMMON-LISP"))

(in-package "RESTRICTED-COMMON-LISP")

(defvar *all-packages* (mapcar (function cl:find-package)
                               '("RESTRICTED-COMMON-LISP"
                                 "RESTRICTED-COMMON-LISP-USER"
                                 "KEYWORD"
                                 #+(and clisp regexp) "REGEXP")))
(defun list-all-packages () (copy-list *all-packages*))
(defun find-package (name)
  (let ((name (cl:find-package name)))
    (and (member name *all-packages*) name)))


(in-package "COMMON-LISP-USER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARSE-SEXP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cuts an input stream into strings containing one token.
;;;
;;; (with-input-from-string (in "a 1 \"s\" pac:sym | hi | \\abc #|com|# ;com
;;;  (1 2 3) #2A((a b) (c d)) #x123 #'fun")
;;;   (loop :for s = (parse-sexp in nil nil) :while s  :collect s))
;;; --> 
;;; ("a " "1 " "\"s\"" " pac:sym " "| hi |" " \\abc " "#|com|# ;com
;;;  (1 2 3)" " #2A((a b) (c d)) " "#x123 #'fun")

(define-condition sexp-end-of-file (end-of-file simple-condition)
  ())

#+(or)
(defconstant +character-syntax-types+
  ;; Other characters are constituents.
  (:whitespace      .  #( #\Space #\Newline #\Tab #\Return #\Linefeed #\Page ))
  (:multiple-escape             . "|")
  (:non-terminating-macro-char  . "#")
  (:single-escape               . "\\")
  (:terminating-macro-char      . "\"'(),;`"))

(defconstant +white-spaces+  
  '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page))
(defconstant +digits+ 
  '#.(loop :for i :from 0 :to 9 :collect (digit-char i)))

(defun parse-sexp (&optional (stream *standard-input*)
                   (eof-error-p t) (eof-value nil))
  "Read character on STREAM and collect them in a string,
parsing up to one sexp. 
If an EOF occurs during the parsing of an object, 
a SEXP-END-OF-FILE condition is raised.
If an EOF occurs before a sexp is read, 
then either a SEXP-END-OF-FILE condition is raised 
or the EOF-VALUE is returned according to EOF-ERROR-P.
Spaces, and comments are read and collected in the output string.
The syntax for strings, comment and lists is the default CL reader one.
No reader macro is processed.  #2A(a b c) is parsed as two tokens:
#2A and (a b c).
"
  ;; sexp ::= token
  ;; token ::= string | text | semi-colon-comment | sharp-pipe-comment
  ;; text ::= /([^ ]|\.)+/
  ;; string ::= /"([^"]|\.)*"/
  ;; semi-colon-comment ::= /;.*$/
  ;; sharp-pipe-comment ::= #| ( text-comment | sharp-pipe-comment )* |#
  (with-output-to-string (text)
    (loop
       :named parser
       :with state = :init
       :with stack = '()
       :for ch = (peek-char nil stream nil nil)
       :do (labels 
               ((accept (ch) (princ (read-char stream) text))
                (rerror (eof-error-p ctrlstr &rest args)
                  (if eof-error-p
                      (error (make-instance 'sexp-end-of-file
                               :format-control (concatenate 'string "~A: " 
                                                            ctrlstr)
                               :format-arguments (cons 'parse-sexp args)
                               :stream stream))
                      (return-from parse-sexp eof-value)))
                (eof-error (where)
                  (rerror t "input stream ~S ends within ~A" stream where))
                (end (token)
                  (case token
                    ((:eof) 
                     (rerror eof-error-p
                             "input stream ~S has reached its end"
                             stream))
                    ((:eof-object)   (eof-error "an object"))
                    ((:eof-string)   (eof-error "a string"))
                    ((:eof-sharp)    (eof-error
                                      "a read macro beginning with #\\#"))
                    ((:eof-comment)  (eof-error "a comment #| ... |#"))
                    ((:eof-single)   (eof-error 
                                      "a token after single escape character"))
                    ((:eof-multiple) (eof-error 
                                      "a token after multiple escape character"))
                    ((:close) 
                     (rerror t "READ from ~S: an object cannot start with #\\)"
                             stream))
                    (otherwise (loop-finish))))
                (shift (new-state) (setf state new-state))
                (spop () (funcall (pop stack))))
             (macrolet ((spush (return new-state)
                          `(progn (push (lambda () ,(if (keywordp return)
                                                        `(shift ,return)
                                                        return)) stack)
                                  (shift ,new-state))))
               (ecase state 
                 ((:init)
                  (case ch
                    (#.+white-spaces+ (accept ch))
                    ((#\;)         (accept ch) (spush :init     :semi-comment))
                    ((#\")         (accept ch) (spush (end :string) :string))
                    ((#\()         (accept ch) (spush (end :list)   :list))
                    ((#\))         (end :close))
                    ((#\' #\, #\`) (accept ch))
                    ((#\#)         (accept ch) (shift :init-sharp))
                    ((nil)         (end :eof))
                    ((#\\)         (spush (end :token) :token))
                    ((#\|)         (spush (end :token) :token))
                    (otherwise     (accept ch) (spush (end :token) :token))))
                 ((:init-sharp)
                  (case ch
                    ((#\|)       (accept ch) (spush :init        :comment))
                    ((nil)       (end :eof-sharp))
                    (otherwise   (accept ch) (spush (end :token) :sharp-token))))
                 ((:list-sharp)
                  (case ch
                    ((#\|)       (accept ch) (spush :list :comment))
                    ((nil)       (end :eof-sharp))
                    (otherwise   (accept ch) (spush :list :sharp-token))))
                 ((:semi-comment)
                  (case ch
                    ((#\newline) (accept ch) (spop))
                    ((nil)       (end (if (null stack) :eof :eof-object)))
                    (otherwise   (accept ch))))
                 ((:string)
                  (case ch
                    ((#\\)       (accept ch) (shift :string-escape))
                    ((#\")       (accept ch) (spop))
                    ((nil)       (end :eof-string))
                    (otherwise   (accept ch))))
                 ((:string-escape) 
                  (case ch
                    ((nil)       (end :eof-string))
                    (otherwise   (accept ch) (shift :string))))
                 ((:comment)
                  (case ch
                    ((#\|)       (accept ch) (shift :comment-end))
                    ((#\#)       (accept ch) (shift :comment-sharp))
                    ((nil)       (end :eof-comment))
                    (otherwise   (accept ch))))
                 ((:comment-end)
                  (case ch
                    ((#\#)       (accept ch) (spop))
                    ((nil)       (end :eof-comment))
                    (otherwise   (accept ch) (shift :comment))))
                 ((:comment-sharp)
                  (case ch
                    ((#\|)       (accept ch) (spush :comment  :comment))
                    ((nil)       (end :eof-comment))
                    (otherwise   (accept ch) (shift :comment))))
                 ((:list)
                  (case ch
                    (#.+white-spaces+ (accept ch))
                    ((#\;)         (accept ch) (spush :list :semi-comment))
                    ((#\")         (accept ch) (spush :list :string))
                    ((#\()         (accept ch) (spush :list :list))
                    ((#\#)         (accept ch) (shift :list-sharp))
                    ((#\))         (accept ch) (spop))
                    ((#\' #\, #\`) (accept ch))
                    ((nil)         (end :eof-object))
                    (otherwise     (accept ch) (spush :list :token))))
                 ((:token)
                  (case ch
                    (#.+white-spaces+ (accept ch) (spop))
                    ((#\;)            (accept ch) (spush (spop) :semi-comment))
                    ((#\\)            (accept ch) (spush :token :single-escape))
                    ((#\|)            (accept ch) (shift :multiple-escape))
                    ((#\")            (spop))
                    ((#\( #\))        (spop))
                    ((#\' #\, #\`)    (spop))
                    ((nil)            (spop))
                    (otherwise (accept ch))))
                 ((:sharp-token)
                  (case ch
                    (#.+digits+       (accept ch))
                    ((#\: #\@)        (accept ch) (shift :sharp-colon-at))
                    ((nil)            (end :eof-object))
                    (otherwise        (accept ch) (shift :sharp-rest))))
                 ((:sharp-colon-at)
                  (case ch
                    ((#\: #\@)        (accept ch))
                    ((nil)            (end :eof-object))
                    (otherwise        (accept ch) (shift :sharp-rest))))
                 ((:sharp-rest)
                  (case ch
                    (#.+white-spaces+ (accept ch) (spop))
                    ((#\\)     (accept ch) (spush :sharp-rest :single-escape))
                    ((#\|)     (accept ch) (spush :sharp-rest :multiple-escape))
                    ((#\()     (accept ch) (spush :sharp-rest :list))
                    ((#\))     (spop))
                    ((#\")     (spop))
                    ((nil)     (spop))
                    (otherwise (accept ch))))
                 ((:single-escape)
                  (case ch
                    ((nil) (end :eof-single))
                    (otherwise (accept ch) (spop))))
                 ((:multiple-escape)
                  (case ch
                    ((nil) (end :eof-multiple))
                    ((#\|) (accept ch) (spop))
                    ((#\\) (accept ch) (spush :multiple-escape :single-escape))
                    (otherwise (accept ch))))))))))


#||

(with-input-from-string (in "  ; comment
#| 1 1
tralala |#
 2  hello   world! #\\GREEK_SMALL_LETTER_LAMDA #2a((a b) (c d ) ) zzz")
  (loop :for s = (parse-sexp in nil nil) :while s :collect s))

(with-input-from-string (in "  (  a ( d e f ) c )  ")
  (loop :for s = (parse-sexp in nil nil) :while s :collect s))
(with-input-from-string (in "(a (d e f) c)")
  (loop :for s = (parse-sexp in nil nil) :while s :collect s))


(mapcar 'read-from-string
        (with-input-from-string (in "  ; comment
#| 1 1
tralala |#
 2  hello   world! #\\GREEK_SMALL_LETTER_LAMDA #2a((a #| b)#|hi|#
 (1|# 3) (c d ) ) zzz #x123ABC\"def\"")
          (loop :for s = (parse-sexp in nil nil) :while s  :collect s)))

||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant +port+ 19000)

(defun remote-eval (form-text)
  (with-open-stream (socket (socket:socket-connect +port+))
    (when (eql :output (socket:socket-status socket))
      (let ((*print-circle* t)
            (*print-readably* t))
        (princ form-text socket) (finish-output socket)
        (let ((results (read socket nil socket)))
          (cond ((atom results)
                 (format t "~&Got invalid result message: ~S~%"
                         results))
                ((eql :ok (car results))
                 (unless (= 0 (length (second results)))
                   (format t "~&Printed:  ~A" (second results)))
                 (unless (null (third results))
                   (format t "~&Returned: ~{~A~^ ;~%          ~}~%"
                           (third results))))
                ((eql :error (car results))
                 (format t "~&ERROR: ~A~%" (cdr results)))
                (t (format t "~&Got invalid result message: ~S~%"
                           results))))))))

(defun read-new-value ()
  (format *query-io* "Enter a new value: ")
  (multiple-value-list (eval (read *query-io*))))

(defmacro with-restricted-dynamic-environment (&body body)
  `(with-output-to-string (*standard-output*)
     (with-input-from-string (*standard-input* "")
       (let* ((*print-circle* t)
              (*print-readably* nil)
              (*error-output* *standard-output*)
              (*trace-output* *standard-output*)
              (*query-io* (make-two-way-stream *standard-input*
                                               *standard-output*))
              (*terminal-io* *query-io*)
              (*package* (find-package "RCL-USER"))
              (*read-eval* nil)
              (-))
         (restart-case (progn ,@body)
           (abort          ()      :report "Aborted")
           (continue       ()      :report "Continued")
           (muffle-warning ()      :report "Warning muffled")
           (store-value    (value) :report "Value stored"
                                   :interactive read-new-value)
           (use-value      (value) :report "Value used"
                                   :interactive read-new-value))))))

(defun check-form/packages (form &key (in-packs '(:rcl-user :keyword)) 
                                 (out-packs '(:rcl :regexp)))
  (cond ((symbolp form)
         (dolist (pack in-packs)
           (multiple-value-bind (sym status)  
               (find-symbol (symbol-name form) pack)
             (when status
               (return-from check-form/packages nil))))
         (dolist (pack out-packs)
           (multiple-value-bind (sym status)  
               (find-symbol (symbol-name form) pack)
             (when (eql status :external)
               (return-from check-form/packages nil))))
         (symbol-package form))
        ((atom form) nil)
        (t (or (check-form/packages (car form))
               (check-form/packages (cdr form))))))

(defun server-rep (socket logout)
  (let* ((values)
         (output
          (with-restricted-dynamic-environment
              ;; (print (list logout *standard-output*))
              (let ((form (read socket nil socket)))
                (if (eql form socket)
                    (format logout "~&Got EOF~%")
                    (let ((pack (check-form/packages form)))
                      (if pack
                          (error "Unknown package ~S" pack)
                          (progn
                            (format logout "~&Got request to evaluate: ~S~%"
                                    form)
                            (setf values  (let ((- form))
                                            (multiple-value-list
                                             (eval form))))))))))))
    (let ((*print-circle* t)
          (*print-readably* nil))
      (print (list :ok output (mapcar (lambda (x) (format nil "~S" x)) values))
             socket)
      (terpri socket)
      (finish-output socket))))


(defun main ()
  (cond
    ((or (null ext:*args*)
         (member "--help"  ext:*args* :test (function string=)))
     (let ((name (pathname-name (load-time-value *load-pathname*))))
       (format t "~%~A --server &  # to start the server~%~
                 ~:*~A --repl      # to start a local REPL communicating~%~
                   ~VA             # with the server~%~
                ~3:*~A <sexp>      # to send one S-EXP to the server~2%"
               name (length name) "")))
    ((string= "--server" (or (first ext:*args*) ""))
     (let ((server (socket:socket-server +port+))
           (log-output *terminal-io*))
       (unwind-protect
            (loop
               (handler-case
                   (with-open-stream (socket (socket:socket-accept server))
                     (multiple-value-bind (remote-host remote-port)
                         (socket:socket-stream-peer socket)
                       (format log-output "~&Accepted connection from ~A ~A~%"
                               remote-host remote-port))
                     (handler-case
                         (server-rep socket log-output)
                       (t (err)
                         (let ((*print-circle* t)
                               (*print-readably* nil))
                           (print (cons :error (format nil "~A" err)) socket)
                           (terpri socket) (finish-output socket)))))
                 #+clisp
                 (system::interrupt-condition (err) 
                   (format log-output "~&Got error: ~A~%" err)
                   (format log-output "Exiting.~%")
                   (ext:quit))
                 (t (err) 
                   (format log-output "~&Got error: ~A~%" err))))
         (socket:socket-server-close server))))
    ((string= "--repl" (or (first ext:*args*) ""))
     (loop
        (format t "~2%Welcome to the remote ~A REPL~2%" (lisp-implementation-type))
        (format t "Exit by typing: :QUIT or :EXIT alone.~2%")
        (handler-case
            (loop
               :for form-text = (progn
                                  (format t "~&LISP> ") (finish-output)
                                  (parse-sexp *standard-input* nil nil))
               :until (or (null form-text)
                          (member  (string-trim +white-spaces+ form-text)
                                   '(":QUIT" ":EXIT" "(QUIT)" "(EXIT)")
                                   :test (function string-equal)))
               :do (remote-eval form-text)
               :finally (return-from main))
          (error (err) (format *error-output* "~%~A~%" err)))))
    (t
     (dolist (form-text ext:*args*)
       (princ form-text)
       (remote-eval form-text)))))

(main)


;; (funcall (first (find-all-symbols "OPEN")) "/tmp/a")
