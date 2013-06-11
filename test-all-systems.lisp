(ql:register-local-projects)

(defparameter *systems*
  '(
    :com.informatimago.xcode
    :com.informatimago.rdp.example
    :com.informatimago.rdp.basic.example
    :com.informatimago.rdp.basic
    :com.informatimago.rdp
    :com.informatimago.lispdoc
    :com.informatimago.linc
    :com.informatimago.languages.lua
    :com.informatimago.languages.cxx
    :com.informatimago.common-lisp.unix
    :com.informatimago.common-lisp.tools.make-depends
    :com.informatimago.common-lisp.telnet
    :com.informatimago.common-lisp.rfc3548
    :com.informatimago.common-lisp.rfc2822
    :com.informatimago.common-lisp.regexp
    :com.informatimago.common-lisp.picture
    :com.informatimago.common-lisp.parser
    :com.informatimago.common-lisp.lisp.stepper
    :com.informatimago.common-lisp.lisp.ibcl
    :com.informatimago.common-lisp.lisp-text
    :com.informatimago.common-lisp.lisp-sexp
    :com.informatimago.common-lisp.lisp-reader
    :com.informatimago.common-lisp.lisp
    :com.informatimago.common-lisp.invoice
    :com.informatimago.common-lisp.interactive
    :com.informatimago.common-lisp.http
    :com.informatimago.common-lisp.html-parser
    :com.informatimago.common-lisp.html-generator
    :com.informatimago.common-lisp.html-base
    :com.informatimago.common-lisp.heap
    :com.informatimago.common-lisp.graphviz
    :com.informatimago.common-lisp.ed
    :com.informatimago.common-lisp.diagram
    :com.informatimago.common-lisp.data-encoding
    :com.informatimago.common-lisp.csv
    :com.informatimago.common-lisp.cesarum
    :com.informatimago.common-lisp.bank
    :com.informatimago.common-lisp.arithmetic
    :com.informatimago.common-lisp.apple-file
    :com.informatimago.common-lisp
    :com.informatimago.clmisc
    #-sbcl               :com.informatimago.clext
    #+clisp              :com.informatimago.susv3
    #+clisp              :com.informatimago.clisp
    #+(and ccl darwin)   :com.informatimago.objcl            ; macosx even.
    #+(and ccl darwin)   :com.informatimago.cocoa-playground ; macosx even.
    ))

(dolist (sys *systems*)
  (handler-case
   (ql:quickload sys)
    (error (err)
      (format t "~2%Error while loading system ~A~%~A~%" sys err))))

#|
SBCL:

cl-user> (load #P "~/src/public/lisp/test-all-systems.lisp")
; loading #P"~/src/public/lisp/test-all-systems.lisp"
To load "com.informatimago.xcode":
  Load 1 ASDF system:
    com.informatimago.xcode
; Loading "com.informatimago.xcode"

To load "com.informatimago.rdp.example":
  Load 1 ASDF system:
    com.informatimago.rdp.example
; Loading "com.informatimago.rdp.example"

To load "com.informatimago.rdp.basic.example":
  Load 1 ASDF system:
    com.informatimago.rdp.basic.example
; Loading "com.informatimago.rdp.basic.example"
[package com.informatimago.rdp.basic.example]
; 
; caught error:
;   (during macroexpansion of (defgrammar example ...))
;   The function com.informatimago.rdp::first-rhs is undefined.
; 
; compilation unit aborted
;   caught 1 fatal ERROR condition
;   caught 1 ERROR condition


Error while loading system com.informatimago.rdp.basic.example
Error while invoking #<compile-op (:verbose nil) {100689C3E3}> on #<cl-source-file "com.informatimago.rdp.basic.example" "example-basic">
To load "com.informatimago.rdp.basic":
  Load 1 ASDF system:
    com.informatimago.rdp.basic
; Loading "com.informatimago.rdp.basic"

To load "com.informatimago.rdp":
  Load 1 ASDF system:
    com.informatimago.rdp
; Loading "com.informatimago.rdp"

To load "com.informatimago.objcl":
  Load 1 ASDF system:
    com.informatimago.objcl
; Loading "com.informatimago.objcl"
[package com.informatimago.simple-test]...........
[package com.informatimago.objective-c.lower].....
[package com.informatimago.objective-cl]..
; 
; caught error:
;   read error during compile-file: Package NS does not exist.Line: 445, Column: 32, File-Position: 18351Stream: #<sb-sys:fd-stream for "file /home/pjb/src/git/public/lisp/objcl/objcl.lisp" {1003E729D3}>
; 
; compilation unit aborted
;   caught 2 fatal ERROR conditions
;   caught 1 ERROR condition


Error while loading system com.informatimago.objcl
Error while invoking #<compile-op (:verbose nil) {100800DE33}> on #<cl-source-file "com.informatimago.objcl" "objcl">
To load "com.informatimago.lispdoc":
  Load 1 ASDF system:
    com.informatimago.lispdoc
; Loading "com.informatimago.lispdoc"


WARNING: closer-weak: WEAK-OR-RELATION should be implemented as primitive in SBCL

; 
; compilation unit aborted
;   caught 1 fatal ERROR condition


Error while loading system com.informatimago.lispdoc
p1 should be collected (iteration 1)
To load "com.informatimago.linc":
  Load 1 ASDF system:
    com.informatimago.linc
; Loading "com.informatimago.linc"

To load "com.informatimago.languages.lua":
  Load 1 ASDF system:
    com.informatimago.languages.lua
; Loading "com.informatimago.languages.lua"
......
To load "com.informatimago.languages.cxx":
  Load 1 ASDF system:
    com.informatimago.languages.cxx
; Loading "com.informatimago.languages.cxx"

To load "com.informatimago.common-lisp.unix":
  Load 1 ASDF system:
    com.informatimago.common-lisp.unix
; Loading "com.informatimago.common-lisp.unix"

To load "com.informatimago.common-lisp.tools.make-depends":
  Load 1 ASDF system:
    com.informatimago.common-lisp.tools.make-depends
; Loading "com.informatimago.common-lisp.tools.make-depends"
[package com.informatimago.common-lisp.tools.make-depends].
.
; 
; caught error:
;   read error during compile-file: Package PACKAGE does not exist.Line: 579, Column: 44, File-Position: 24135Stream: #<sb-sys:fd-stream for "file /home/pjb/src/git/public/lisp/tools/make-depends.lisp" {100978F0E3}>
; 
; compilation unit aborted
;   caught 2 fatal ERROR conditions
;   caught 1 ERROR condition


Error while loading system com.informatimago.common-lisp.tools.make-depends
Error while invoking #<compile-op (:verbose nil) {1008F6D093}> on #<cl-source-file "com.informatimago.common-lisp.tools.make-depends" "make-depends">
To load "com.informatimago.common-lisp.telnet":
  Load 1 ASDF system:
    com.informatimago.common-lisp.telnet
; Loading "com.informatimago.common-lisp.telnet"
[package com.informatimago.common-lisp.telnet].

; file: /home/pjb/src/git/public/lisp/common-lisp/telnet/telnet.lisp
; in: defclass option
;     (DEFCLASS COM.INFORMATIMAGO.COMMON-LISP.TELNET:OPTION NIL
;               ((COM.INFORMATIMAGO.COMMON-LISP.TELNET::CODE :INITARG :CODE :READER
;                 COM.INFORMATIMAGO.COMMON-LISP.TELNET:OPTION-CODE)
;                (COM.INFORMATIMAGO.COMMON-LISP.TELNET::NAME :INITARG :NAME)
;                (COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII:US :INITFORM :NO
;                 :TYPE COM.INFORMATIMAGO.COMMON-LISP.TELNET::SIDE-OPTION-STATE
;                 :ACCESSOR COM.INFORMATIMAGO.COMMON-LISP.TELNET::OPT-US)
;                (COM.INFORMATIMAGO.COMMON-LISP.TELNET::USQ :INITFORM :EMPTY :TYPE
;                 COM.INFORMATIMAGO.COMMON-LISP.TELNET::SIDE-OPTION-QUEUE :ACCESSOR
;                 COM.INFORMATIMAGO.COMMON-LISP.TELNET::OPT-USQ)
;                (COM.INFORMATIMAGO.COMMON-LISP.TELNET::HIM :INITFORM :NO :TYPE
;                 COM.INFORMATIMAGO.COMMON-LISP.TELNET::SIDE-OPTION-STATE :ACCESSOR
;                 COM.INFORMATIMAGO.COMMON-LISP.TELNET::OPT-HIM)
;                (COM.INFORMATIMAGO.COMMON-LISP.TELNET::HIMQ :INITFORM :EMPTY :TYPE
;                 COM.INFORMATIMAGO.COMMON-LISP.TELNET::SIDE-OPTION-QUEUE :ACCESSOR
;                 COM.INFORMATIMAGO.COMMON-LISP.TELNET::OPT-HIMQ)))
; 
; caught error:
;   (during macroexpansion of (defclass option ...))
;   In DEFCLASS option, the slot name us is a constant.
...
......; 
      ; compilation unit aborted
      ;   caught 1 fatal ERROR condition
      ;   caught 1 ERROR condition


Error while loading system com.informatimago.common-lisp.telnet
Error while invoking #<compile-op (:verbose nil) {1004175033}> on #<cl-source-file "com.informatimago.common-lisp.telnet" "telnet">
To load "com.informatimago.common-lisp.rfc3548":
  Load 1 ASDF system:
    com.informatimago.common-lisp.rfc3548
; Loading "com.informatimago.common-lisp.rfc3548"

To load "com.informatimago.common-lisp.rfc2822":
  Load 1 ASDF system:
    com.informatimago.common-lisp.rfc2822
; Loading "com.informatimago.common-lisp.rfc2822"

To load "com.informatimago.common-lisp.regexp":
  Load 1 ASDF system:
    com.informatimago.common-lisp.regexp
; Loading "com.informatimago.common-lisp.regexp"

To load "com.informatimago.common-lisp.picture":
  Load 1 ASDF system:
    com.informatimago.common-lisp.picture
; Loading "com.informatimago.common-lisp.picture"

To load "com.informatimago.common-lisp.parser":
  Load 1 ASDF system:
    com.informatimago.common-lisp.parser
; Loading "com.informatimago.common-lisp.parser"

To load "com.informatimago.common-lisp.lisp.stepper":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp.stepper
; Loading "com.informatimago.common-lisp.lisp.stepper"
[package com.informatimago.common-lisp.lisp.stepper.internal]
[package com.informatimago.common-lisp.lisp.stepper].
........
To load "com.informatimago.common-lisp.lisp.ibcl":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp.ibcl
; Loading "com.informatimago.common-lisp.lisp.ibcl"
[package com.informatimago.common-lisp.lisp.source].
..................................................
[package com.informatimago.common-lisp.lisp.cl-saving-defines].
..................................................
[package com.informatimago.common-lisp.lisp.image-based-common-lisp]
[package com.informatimago.common-lisp.lisp.image-based-common-lisp-user].
..
To load "com.informatimago.common-lisp.lisp-text":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp-text
; Loading "com.informatimago.common-lisp.lisp-text"

To load "com.informatimago.common-lisp.lisp-sexp":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp-sexp
; Loading "com.informatimago.common-lisp.lisp-sexp"

To load "com.informatimago.common-lisp.lisp-reader":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp-reader
; Loading "com.informatimago.common-lisp.lisp-reader"

To load "com.informatimago.common-lisp.lisp":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp
; Loading "com.informatimago.common-lisp.lisp"
[package com.informatimago.common-lisp.generic-common-lisp].
.; 
 ; compilation unit aborted
 ;   caught 2 fatal ERROR conditions


Error while loading system com.informatimago.common-lisp.lisp
Lock on package COMMON-LISP violated when proclaiming sequence as a class while in package COM.INFORMATIMAGO.COMMON-LISP.GENERIC-COMMON-LISP.
See also:
  The SBCL Manual, Node "Package Locks"
  The ANSI Standard, Section 11.1.2.1.2
To load "com.informatimago.common-lisp.invoice":
  Load 1 ASDF system:
    com.informatimago.common-lisp.invoice
; Loading "com.informatimago.common-lisp.invoice"

To load "com.informatimago.common-lisp.interactive":
  Load 1 ASDF system:
    com.informatimago.common-lisp.interactive
; Loading "com.informatimago.common-lisp.interactive"

To load "com.informatimago.common-lisp.http":
  Load 1 ASDF system:
    com.informatimago.common-lisp.http
; Loading "com.informatimago.common-lisp.http"

To load "com.informatimago.common-lisp.html-parser":
  Load 1 ASDF system:
    com.informatimago.common-lisp.html-parser
; Loading "com.informatimago.common-lisp.html-parser"

To load "com.informatimago.common-lisp.html-generator":
  Load 1 ASDF system:
    com.informatimago.common-lisp.html-generator
; Loading "com.informatimago.common-lisp.html-generator"

To load "com.informatimago.common-lisp.html-base":
  Load 1 ASDF system:
    com.informatimago.common-lisp.html-base
; Loading "com.informatimago.common-lisp.html-base"

To load "com.informatimago.common-lisp.heap":
  Load 1 ASDF system:
    com.informatimago.common-lisp.heap
; Loading "com.informatimago.common-lisp.heap"

To load "com.informatimago.common-lisp.graphviz":
  Load 1 ASDF system:
    com.informatimago.common-lisp.graphviz
; Loading "com.informatimago.common-lisp.graphviz"

To load "com.informatimago.common-lisp.ed":
  Load 1 ASDF system:
    com.informatimago.common-lisp.ed
; Loading "com.informatimago.common-lisp.ed"

To load "com.informatimago.common-lisp.diagram":
  Load 1 ASDF system:
    com.informatimago.common-lisp.diagram
; Loading "com.informatimago.common-lisp.diagram"

To load "com.informatimago.common-lisp.data-encoding":
  Load 1 ASDF system:
    com.informatimago.common-lisp.data-encoding
; Loading "com.informatimago.common-lisp.data-encoding"

To load "com.informatimago.common-lisp.csv":
  Load 1 ASDF system:
    com.informatimago.common-lisp.csv
; Loading "com.informatimago.common-lisp.csv"

To load "com.informatimago.common-lisp.cesarum":
  Load 1 ASDF system:
    com.informatimago.common-lisp.cesarum
; Loading "com.informatimago.common-lisp.cesarum"

To load "com.informatimago.common-lisp.bank":
  Load 1 ASDF system:
    com.informatimago.common-lisp.bank
; Loading "com.informatimago.common-lisp.bank"

To load "com.informatimago.common-lisp.arithmetic":
  Load 1 ASDF system:
    com.informatimago.common-lisp.arithmetic
; Loading "com.informatimago.common-lisp.arithmetic"

To load "com.informatimago.common-lisp.apple-file":
  Load 1 ASDF system:
    com.informatimago.common-lisp.apple-file
; Loading "com.informatimago.common-lisp.apple-file"
[package com.informatimago.common-lisp.apple-file.apple-file].
..........
To load "com.informatimago.common-lisp":
  Load 1 ASDF system:
    com.informatimago.common-lisp
; Loading "com.informatimago.common-lisp"



Error while loading system com.informatimago.cocoa-playground
Error while trying to load definition for system com.informatimago.cocoa-playground from pathname /home/pjb/src/git/public/lisp/objcl/com.informatimago.cocoa-playground.asd: Invalid initialization argument: :maitainer in call for class #<standard-class asdf:system>.
See also:
  The ANSI Standard, Section 7.1.2
To load "com.informatimago.clmisc":
  Load 1 ASDF system:
    com.informatimago.clmisc
; Loading "com.informatimago.clmisc"

t
cl-user>
|#


#|
CLISP

CL-USER> (load #P "~/src/public/lisp/test-all-systems.lisp")
;; Loading file /home/pjb/src/public/lisp/test-all-systems.lisp ...
To load "com.informatimago.xcode":
  Load 1 ASDF system:
    com.informatimago.xcode
; Loading "com.informatimago.xcode"

To load "com.informatimago.rdp.example":
  Load 1 ASDF system:
    com.informatimago.rdp.example
; Loading "com.informatimago.rdp.example"


Error while loading system COM.INFORMATIMAGO.RDP.EXAMPLE
(EQUAL
 (PARSE-EXAMPLE
  "
    const abc = 123,
          pi=3.141592e+0; 
    var a,b,c; 
    procedure gcd; 
    begin 
        while a # b do 
        begin
             if a<b then b:=b-a ; 
             if a>b then a:=a-b 
        end 
    end;
begin 
    a:=42;
    b:=30.0;
    call gcd
end.")
 '(BLOCK (((IDENT "abc" 11) (INTEGER "123" 17)) ((IDENT "pi" 32) (REAL "3.141592e+0" 35))) ((IDENT "a" 57) (IDENT "b" 59) (IDENT "c" 61)) ((PROCEDURE (IDENT "gcd" 79) (BLOCK NIL NIL NIL ((WHILE (("#" "#" 112) (+ ((IDENT "a" 110))) (+ ((IDENT "b" 114)))) ((IF (("<" "<" 151) (+ ((IDENT "a" 150))) (+ ((IDENT "b" 152)))) (SETF (IDENT "b" 159) (+ ((IDENT "b" 162)) (("-" "-" 163) ((IDENT "a" 164)))))) (IF ((">" ">" 186) (+ ((IDENT "a" 185))) (+ ((IDENT "b" 187)))) (SETF (IDENT "a" 194) (+ ((IDENT "a" 197)) (("-" "-" 198) ((IDENT "b" 199)))))))))))) ((SETF (IDENT "a" 235) (+ ((INTEGER "42" 238)))) (SETF (IDENT "b" 246) (+ ((REAL "30.0" 249)))) (CALL (IDENT "gcd" 264))))) must evaluate to a non-NIL value.
To load "com.informatimago.rdp.basic.example":
  Load 1 ASDF system:
    com.informatimago.rdp.basic.example
; Loading "com.informatimago.rdp.basic.example"
[package com.informatimago.rdp.basic.example]

Error while loading system COM.INFORMATIMAGO.RDP.BASIC.EXAMPLE
FUNCALL: undefined function COM.INFORMATIMAGO.RDP::FIRST-RHS

To load "com.informatimago.rdp.basic":
  Load 1 ASDF system:
    com.informatimago.rdp.basic
; Loading "com.informatimago.rdp.basic"

To load "com.informatimago.rdp":
  Load 1 ASDF system:
    com.informatimago.rdp
; Loading "com.informatimago.rdp"

To load "com.informatimago.lispdoc":
  Load 1 ASDF system:
    com.informatimago.lispdoc
; Loading "com.informatimago.lispdoc"

To load "com.informatimago.linc":
  Load 1 ASDF system:
    com.informatimago.linc
; Loading "com.informatimago.linc"

To load "com.informatimago.languages.lua":
  Load 1 ASDF system:
    com.informatimago.languages.lua
; Loading "com.informatimago.languages.lua"

To load "com.informatimago.languages.cxx":
  Load 1 ASDF system:
    com.informatimago.languages.cxx
; Loading "com.informatimago.languages.cxx"

To load "com.informatimago.common-lisp.unix":
  Load 1 ASDF system:
    com.informatimago.common-lisp.unix
; Loading "com.informatimago.common-lisp.unix"

To load "com.informatimago.common-lisp.tools.make-depends":
  Load 1 ASDF system:
    com.informatimago.common-lisp.tools.make-depends
; Loading "com.informatimago.common-lisp.tools.make-depends"
[package com.informatimago.common-lisp.tools.make-depends]

Error while loading system COM.INFORMATIMAGO.COMMON-LISP.TOOLS.MAKE-DEPENDS
READ from #<CLOSED INPUT BUFFERED FILE-STREAM CHARACTER #P"/home/pjb/src/git/public/lisp/tools/make-depends.lisp" @579>: there is no package with name "PACKAGE"

To load "com.informatimago.common-lisp.telnet":
  Load 1 ASDF system:
    com.informatimago.common-lisp.telnet
; Loading "com.informatimago.common-lisp.telnet"

To load "com.informatimago.common-lisp.rfc3548":
  Load 1 ASDF system:
    com.informatimago.common-lisp.rfc3548
; Loading "com.informatimago.common-lisp.rfc3548"

To load "com.informatimago.common-lisp.rfc2822":
  Load 1 ASDF system:
    com.informatimago.common-lisp.rfc2822
; Loading "com.informatimago.common-lisp.rfc2822"

To load "com.informatimago.common-lisp.regexp":
  Load 1 ASDF system:
    com.informatimago.common-lisp.regexp
; Loading "com.informatimago.common-lisp.regexp"

To load "com.informatimago.common-lisp.picture":
  Load 1 ASDF system:
    com.informatimago.common-lisp.picture
; Loading "com.informatimago.common-lisp.picture"

To load "com.informatimago.common-lisp.parser":
  Load 1 ASDF system:
    com.informatimago.common-lisp.parser
; Loading "com.informatimago.common-lisp.parser"

To load "com.informatimago.common-lisp.lisp.stepper":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp.stepper
; Loading "com.informatimago.common-lisp.lisp.stepper"

To load "com.informatimago.common-lisp.lisp.ibcl":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp.ibcl
; Loading "com.informatimago.common-lisp.lisp.ibcl"

To load "com.informatimago.common-lisp.lisp-text":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp-text
; Loading "com.informatimago.common-lisp.lisp-text"

To load "com.informatimago.common-lisp.lisp-sexp":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp-sexp
; Loading "com.informatimago.common-lisp.lisp-sexp"

To load "com.informatimago.common-lisp.lisp-reader":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp-reader
; Loading "com.informatimago.common-lisp.lisp-reader"

To load "com.informatimago.common-lisp.lisp":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp
; Loading "com.informatimago.common-lisp.lisp"
[package com.informatimago.common-lisp.generic-common-lisp]

Error while loading system COM.INFORMATIMAGO.COMMON-LISP.LISP
DEFMETHOD((CLOS:VALIDATE-SUPERCLASS (:BEFORE) CLASS CLASS)): (#<PACKAGE CLOS> #<PACKAGE COMMON-LISP> #<PACKAGE CLOS> #<PACKAGE CLOS>) is locked
To load "com.informatimago.common-lisp.invoice":
  Load 1 ASDF system:
    com.informatimago.common-lisp.invoice
; Loading "com.informatimago.common-lisp.invoice"

To load "com.informatimago.common-lisp.interactive":
  Load 1 ASDF system:
    com.informatimago.common-lisp.interactive
; Loading "com.informatimago.common-lisp.interactive"

To load "com.informatimago.common-lisp.http":
  Load 1 ASDF system:
    com.informatimago.common-lisp.http
; Loading "com.informatimago.common-lisp.http"

To load "com.informatimago.common-lisp.html-parser":
  Load 1 ASDF system:
    com.informatimago.common-lisp.html-parser
; Loading "com.informatimago.common-lisp.html-parser"

To load "com.informatimago.common-lisp.html-generator":
  Load 1 ASDF system:
    com.informatimago.common-lisp.html-generator
; Loading "com.informatimago.common-lisp.html-generator"

To load "com.informatimago.common-lisp.html-base":
  Load 1 ASDF system:
    com.informatimago.common-lisp.html-base
; Loading "com.informatimago.common-lisp.html-base"

To load "com.informatimago.common-lisp.heap":
  Load 1 ASDF system:
    com.informatimago.common-lisp.heap
; Loading "com.informatimago.common-lisp.heap"

To load "com.informatimago.common-lisp.graphviz":
  Load 1 ASDF system:
    com.informatimago.common-lisp.graphviz
; Loading "com.informatimago.common-lisp.graphviz"

To load "com.informatimago.common-lisp.ed":
  Load 1 ASDF system:
    com.informatimago.common-lisp.ed
; Loading "com.informatimago.common-lisp.ed"

To load "com.informatimago.common-lisp.diagram":
  Load 1 ASDF system:
    com.informatimago.common-lisp.diagram
; Loading "com.informatimago.common-lisp.diagram"

To load "com.informatimago.common-lisp.data-encoding":
  Load 1 ASDF system:
    com.informatimago.common-lisp.data-encoding
; Loading "com.informatimago.common-lisp.data-encoding"

To load "com.informatimago.common-lisp.csv":
  Load 1 ASDF system:
    com.informatimago.common-lisp.csv
; Loading "com.informatimago.common-lisp.csv"

To load "com.informatimago.common-lisp.cesarum":
  Load 1 ASDF system:
    com.informatimago.common-lisp.cesarum
; Loading "com.informatimago.common-lisp.cesarum"

To load "com.informatimago.common-lisp.bank":
  Load 1 ASDF system:
    com.informatimago.common-lisp.bank
; Loading "com.informatimago.common-lisp.bank"

To load "com.informatimago.common-lisp.arithmetic":
  Load 1 ASDF system:
    com.informatimago.common-lisp.arithmetic
; Loading "com.informatimago.common-lisp.arithmetic"

To load "com.informatimago.common-lisp.apple-file":
  Load 1 ASDF system:
    com.informatimago.common-lisp.apple-file
; Loading "com.informatimago.common-lisp.apple-file"


Error while loading system COM.INFORMATIMAGO.COMMON-LISP.APPLE-FILE
(EQUALP (MAPCAR (LAMBDA (FORMAT) (MAPCAR (LAMBDA (FORK) (APPLE-FILE-FORK-PATHNAME "test.single" FORMAT FORK)) '(:INFO :DATA :RESOURCE))) '(:APPLE-SINGLE :APPLE-DOUBLE :APPLE-TRIPLE)) '((#P"test.single" #P"test.single" #P"test.single") (#P"\\._test.single" #P"test.single" #P"\\._test.single") (#P"test.info" #P"test.data" #P"test.rsrc"))) must evaluate to a non-NIL value.
To load "com.informatimago.common-lisp":
  Load 1 ASDF system:
    com.informatimago.common-lisp
; Loading "com.informatimago.common-lisp"

To load "com.informatimago.clmisc":
  Load 1 ASDF system:
    com.informatimago.clmisc
; Loading "com.informatimago.clmisc"

To load "com.informatimago.clext":
  Load 1 ASDF system:
    com.informatimago.clext
; Loading "com.informatimago.clext"

To load "com.informatimago.susv3":
  Load 1 ASDF system:
    com.informatimago.susv3
; Loading "com.informatimago.susv3"
[package com.informatimago.clisp.fifo-stream]

Error while loading system COM.INFORMATIMAGO.SUSV3
in #:|90 103 (DEFMETHOD CLOSE (# &KEY ABORT) ...)-7-1-1-1|  in lines 90..103 : Constant ABORT cannot be bound.
To load "com.informatimago.clisp":
  Load 1 ASDF system:
    com.informatimago.clisp
; Loading "com.informatimago.clisp"
[package com.informatimago.clisp.fifo-stream]

Error while loading system COM.INFORMATIMAGO.CLISP
in #:|90 103 (DEFMETHOD CLOSE (# &KEY ABORT) ...)-7-1-1-1|  in lines 90..103 : Constant ABORT cannot be bound.
;; Loaded file /home/pjb/src/public/lisp/test-all-systems.lisp
#P"/home/pjb/src/git/public/lisp/test-all-systems.lisp"
CL-USER>
|#


#|
CCL

; SLIME 2013-04-02
cl-user> (load #P "~/src/public/lisp/test-all-systems.lisp")
To load "com.informatimago.xcode":
  Load 1 ASDF system:
    com.informatimago.xcode
; Loading "com.informatimago.xcode"
[package com.informatimago.rdp]...................
[package com.informatimago.xcode].
To load "com.informatimago.rdp.example":
  Load 1 ASDF system:
    com.informatimago.rdp.example
; Loading "com.informatimago.rdp.example"
[package com.informatimago.rdp.example]...........
[package com.informatimago.rdp.example-without-action].


Error while loading system com.informatimago.rdp.example
Failed assertion: (equal (com.informatimago.rdp.example:parse-example "
    const abc = 123,
          pi=3.141592e+0; 
    var a,b,c; 
    procedure gcd; 
    begin 
        while a # b do 
        begin
             if a<b then b:=b-a ; 
             if a>b then a:=a-b 
        end 
    end;
begin 
    a:=42;
    b:=30.0;
    call gcd
end.") '(block (((com.informatimago.rdp.example::ident "abc" 11) (integer "123" 17)) ((com.informatimago.rdp.example::ident "pi" 32) (real "3.141592e+0" 35))) ((com.informatimago.rdp.example::ident "a" 57) (com.informatimago.rdp.example::ident "b" 59) (com.informatimago.rdp.example::ident "c" 61)) ((com.informatimago.rdp.example::procedure (com.informatimago.rdp.example::ident "gcd" 79) (block nil nil nil ((com.informatimago.rdp.example::while (("#" "#" 112) (+ ((com.informatimago.rdp.example::ident "a" 110))) (+ ((com.informatimago.rdp.example::ident "b" 114)))) ((if (("<" "<" 151) (+ ((com.informatimago.rdp.example::ident "a" 150))) (+ ((com.informatimago.rdp.example::ident "b" 152)))) (setf (com.informatimago.rdp.example::ident "b" 159) (+ ((com.informatimago.rdp.example::ident "b" 162)) (("-" "-" 163) ((com.informatimago.rdp.example::ident "a" 164)))))) (if ((">" ">" 186) (+ ((com.informatimago.rdp.example::ident "a" 185))) (+ ((com.informatimago.rdp.example::ident "b" 187)))) (setf (com.informatimago.rdp.example::ident "a" 194) (+ ((com.informatimago.rdp.example::ident "a" 197)) (("-" "-" 198) ((com.informatimago.rdp.example::ident "b" 199)))))))))))) ((setf (com.informatimago.rdp.example::ident "a" 235) (+ ((integer "42" 238)))) (setf (com.informatimago.rdp.example::ident "b" 246) (+ ((real "30.0" 249)))) (com.informatimago.rdp.example::call (com.informatimago.rdp.example::ident "gcd" 264)))))
To load "com.informatimago.rdp.basic.example":
  Load 1 ASDF system:
    com.informatimago.rdp.basic.example
; Loading "com.informatimago.rdp.basic.example"
[package com.informatimago.rdp.basic.example]

Error while loading system com.informatimago.rdp.basic.example
Undefined function com.informatimago.rdp::first-rhs called with arguments (#<grammar example #x3020021A5F3D> ((com.informatimago.rdp:seq ("const" com.informatimago.rdp.basic.example::ident "=" number (com.informatimago.rdp:rep ((com.informatimago.rdp:seq ("," com.informatimago.rdp.basic.example::ident "=" number) ("NCAR=A4:NCDR=NIL:CALL CONS" "NCAR=A2:NCDR=RES:CALL CONS")))) ";") ("NCAR=A4:NCDR=NIL:CALL CONS" "NCAR=A2:NCDR=RES:CALL CONS" "NCAR=RES:NCDR=A5:CALL CONS")))) .
To load "com.informatimago.rdp.basic":
  Load 1 ASDF system:
    com.informatimago.rdp.basic
; Loading "com.informatimago.rdp.basic"

To load "com.informatimago.rdp":
  Load 1 ASDF system:
    com.informatimago.rdp
; Loading "com.informatimago.rdp"

To load "com.informatimago.lispdoc":
  Load 1 ASDF system:
    com.informatimago.lispdoc
; Loading "com.informatimago.lispdoc"
[package com.informatimago.clext.closer-weak].....
[package com.informatimago.clext.closer-weak-user].
..................................................
[package split-sequence]..........................
[package com.informatimago.lispdoc]....
To load "com.informatimago.linc":
  Load 1 ASDF system:
    com.informatimago.linc
; Loading "com.informatimago.linc"
[package com.informatimago.linc]..................
[package com.informatimago.linc.c]........
To load "com.informatimago.languages.lua":
  Load 1 ASDF system:
    com.informatimago.languages.lua
; Loading "com.informatimago.languages.lua"
[package com.informatimago.languages.lua.scanner].
[package com.informatimago.languages.lua.parser]..
.
To load "com.informatimago.languages.cxx":
  Load 1 ASDF system:
    com.informatimago.languages.cxx
; Loading "com.informatimago.languages.cxx"
[package com.informatimago.languages.cxx.cxx]
To load "com.informatimago.common-lisp.unix":
  Load 1 ASDF system:
    com.informatimago.common-lisp.unix
; Loading "com.informatimago.common-lisp.unix"

To load "com.informatimago.common-lisp.tools.make-depends":
  Load 1 ASDF system:
    com.informatimago.common-lisp.tools.make-depends
; Loading "com.informatimago.common-lisp.tools.make-depends"
[package com.informatimago.common-lisp.tools.make-depends]
Read error between positions 23606 and 24137 in /home/pjb/src/git/public/lisp/tools/make-depends.lisp.

Error while loading system com.informatimago.common-lisp.tools.make-depends
There is no package named "PACKAGE" .
To load "com.informatimago.common-lisp.telnet":
  Load 1 ASDF system:
    com.informatimago.common-lisp.telnet
; Loading "com.informatimago.common-lisp.telnet"
[package com.informatimago.common-lisp.telnet]...
To load "com.informatimago.common-lisp.rfc3548":
  Load 1 ASDF system:
    com.informatimago.common-lisp.rfc3548
; Loading "com.informatimago.common-lisp.rfc3548"

To load "com.informatimago.common-lisp.rfc2822":
  Load 1 ASDF system:
    com.informatimago.common-lisp.rfc2822
; Loading "com.informatimago.common-lisp.rfc2822"

To load "com.informatimago.common-lisp.regexp":
  Load 1 ASDF system:
    com.informatimago.common-lisp.regexp
; Loading "com.informatimago.common-lisp.regexp"

To load "com.informatimago.common-lisp.picture":
  Load 1 ASDF system:
    com.informatimago.common-lisp.picture
; Loading "com.informatimago.common-lisp.picture"

To load "com.informatimago.common-lisp.parser":
  Load 1 ASDF system:
    com.informatimago.common-lisp.parser
; Loading "com.informatimago.common-lisp.parser"

To load "com.informatimago.common-lisp.lisp.stepper":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp.stepper
; Loading "com.informatimago.common-lisp.lisp.stepper"

To load "com.informatimago.common-lisp.lisp.ibcl":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp.ibcl
; Loading "com.informatimago.common-lisp.lisp.ibcl"
[package com.informatimago.common-lisp.lisp.source]
[package com.informatimago.common-lisp.lisp.cl-saving-defines].
[package com.informatimago.common-lisp.lisp.image-based-common-lisp]
[package com.informatimago.common-lisp.lisp.image-based-common-lisp-user]
To load "com.informatimago.common-lisp.lisp-text":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp-text
; Loading "com.informatimago.common-lisp.lisp-text"

To load "com.informatimago.common-lisp.lisp-sexp":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp-sexp
; Loading "com.informatimago.common-lisp.lisp-sexp"

To load "com.informatimago.common-lisp.lisp-reader":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp-reader
; Loading "com.informatimago.common-lisp.lisp-reader"

To load "com.informatimago.common-lisp.lisp":
  Load 1 ASDF system:
    com.informatimago.common-lisp.lisp
; Loading "com.informatimago.common-lisp.lisp"
[package com.informatimago.common-lisp.generic-common-lisp]

Error while loading system com.informatimago.common-lisp.lisp
Class option :documentation is not one of (:name)
To load "com.informatimago.common-lisp.invoice":
  Load 1 ASDF system:
    com.informatimago.common-lisp.invoice
; Loading "com.informatimago.common-lisp.invoice"

To load "com.informatimago.common-lisp.interactive":
  Load 1 ASDF system:
    com.informatimago.common-lisp.interactive
; Loading "com.informatimago.common-lisp.interactive"

To load "com.informatimago.common-lisp.http":
  Load 1 ASDF system:
    com.informatimago.common-lisp.http
; Loading "com.informatimago.common-lisp.http"

To load "com.informatimago.common-lisp.html-parser":
  Load 1 ASDF system:
    com.informatimago.common-lisp.html-parser
; Loading "com.informatimago.common-lisp.html-parser"

To load "com.informatimago.common-lisp.html-generator":
  Load 1 ASDF system:
    com.informatimago.common-lisp.html-generator
; Loading "com.informatimago.common-lisp.html-generator"

To load "com.informatimago.common-lisp.html-base":
  Load 1 ASDF system:
    com.informatimago.common-lisp.html-base
; Loading "com.informatimago.common-lisp.html-base"

To load "com.informatimago.common-lisp.heap":
  Load 1 ASDF system:
    com.informatimago.common-lisp.heap
; Loading "com.informatimago.common-lisp.heap"

To load "com.informatimago.common-lisp.graphviz":
  Load 1 ASDF system:
    com.informatimago.common-lisp.graphviz
; Loading "com.informatimago.common-lisp.graphviz"

To load "com.informatimago.common-lisp.ed":
  Load 1 ASDF system:
    com.informatimago.common-lisp.ed
; Loading "com.informatimago.common-lisp.ed"

To load "com.informatimago.common-lisp.diagram":
  Load 1 ASDF system:
    com.informatimago.common-lisp.diagram
; Loading "com.informatimago.common-lisp.diagram"

To load "com.informatimago.common-lisp.data-encoding":
  Load 1 ASDF system:
    com.informatimago.common-lisp.data-encoding
; Loading "com.informatimago.common-lisp.data-encoding"

To load "com.informatimago.common-lisp.csv":
  Load 1 ASDF system:
    com.informatimago.common-lisp.csv
; Loading "com.informatimago.common-lisp.csv"

To load "com.informatimago.common-lisp.cesarum":
  Load 1 ASDF system:
    com.informatimago.common-lisp.cesarum
; Loading "com.informatimago.common-lisp.cesarum"

To load "com.informatimago.common-lisp.bank":
  Load 1 ASDF system:
    com.informatimago.common-lisp.bank
; Loading "com.informatimago.common-lisp.bank"

To load "com.informatimago.common-lisp.arithmetic":
  Load 1 ASDF system:
    com.informatimago.common-lisp.arithmetic
; Loading "com.informatimago.common-lisp.arithmetic"

To load "com.informatimago.common-lisp.apple-file":
  Load 1 ASDF system:
    com.informatimago.common-lisp.apple-file
; Loading "com.informatimago.common-lisp.apple-file"
[package com.informatimago.common-lisp.apple-file.apple-file].
..
To load "com.informatimago.common-lisp":
  Load 1 ASDF system:
    com.informatimago.common-lisp
; Loading "com.informatimago.common-lisp"

To load "com.informatimago.clmisc":
  Load 1 ASDF system:
    com.informatimago.clmisc
; Loading "com.informatimago.clmisc"

To load "com.informatimago.clext":
  Load 1 ASDF system:
    com.informatimago.clext
; Loading "com.informatimago.clext"

To load "com.informatimago.objcl":
  Load 1 ASDF system:
    com.informatimago.objcl
; Loading "com.informatimago.objcl"


Error while loading system com.informatimago.objcl
Module cocoa was not provided by any function on ccl:*module-provider-functions*.


Error while loading system com.informatimago.cocoa-playground
Error while trying to load definition for system com.informatimago.cocoa-playground from pathname /home/pjb/src/git/public/lisp/objcl/com\.informatimago\.cocoa-playground.asd: :maitainer is an invalid initarg to reinitialize-instance for #<standard-class asdf:system>.
Valid initargs: (:name :version :description :long-description :in-order-to :do-first :parent :pathname :around-compile :encoding :properties :components :if-component-dep-fails :default-component-class :author :maintainer :licence :license :source-file :defsystem-depends-on).
;Compiler warnings for "home:src;git;public;lisp;common-lisp;apple-file;apple-file.lisp.newest" :
;   In com.informatimago.common-lisp.apple-file.apple-file:apple-file-afp-directory-id: Undefined function com.informatimago.common-lisp.apple-file.apple-file::file-directory-id
;   In (com.informatimago.common-lisp.apple-file.apple-file::close-apple-file (com.informatimago.common-lisp.apple-file.apple-file::apple-file)) inside an anonymous lambda form: Undefined function com.informatimago.common-lisp.apple-file.apple-file::apple-file-header-info-stream
;   In com.informatimago.common-lisp.apple-file.apple-file::check-ranges: Undefined function com.informatimago.common-lisp.apple-file.apple-file::report-collision
;Compiler warnings for "home:src;git;public;lisp;common-lisp;telnet;telnet.lisp.newest" :
;   In (ccl::report-condition (com.informatimago.common-lisp.telnet:telnet-option-warning t)) inside an anonymous lambda form: Undefined function com.informatimago.common-lisp.telnet::telnet-protocol-warning-option
;Compiler warnings for "home:src;git;public;lisp;languages;linc;linc.lisp.newest" :
;   In com.informatimago.linc::generate-preprocessor: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-preprocessor: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-preprocessor: Undefined function com.informatimago.linc::generate-expression
;   In (com.informatimago.linc::generate (t)) inside an anonymous lambda form: Undefined function com.informatimago.linc::generate-expression
;   In (com.informatimago.linc::generate (t)) inside an anonymous lambda form: Undefined function com.informatimago.linc::generate-statement
;   In (com.informatimago.linc::generate (t)) inside an anonymous lambda form: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-declaration: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-declaration: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-declaration: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-declaration: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-declaration: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-declaration: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-declaration: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-declaration: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-declaration: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::repl: Undefined function com.informatimago.linc::generate-statement
;   In com.informatimago.linc::repl: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-definition: Undefined function com.informatimago.linc::generate-statement
;   In com.informatimago.linc::generate-definition: Undefined function com.informatimago.linc::generate-identifier
;   In com.informatimago.linc::generate-definition: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-parameter: Undefined function com.informatimago.linc::generate-expression
;   In com.informatimago.linc::generate-parameter: Undefined function com.informatimago.linc::generate-identifier
;   In com.informatimago.linc::generate-parameter: Undefined function com.informatimago.linc::generate-expression
;Compiler warnings for "home:src;git;public;lisp;languages;linc;c-syntax.lisp.newest" :
;   In (com.informatimago.linc::generate (com.informatimago.linc::namespace)) inside an anonymous lambda form: Undefined function com.informatimago.linc::generate-expression
#P"/home/pjb/src/git/public/lisp/test-all-systems.lisp"
cl-user>
|#
