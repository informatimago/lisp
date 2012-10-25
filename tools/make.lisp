
(defvar *modules* '(common-lisp clext clmisc sbcl clisp susv3))

(defmacro define-implementation (name &key feature executable default-options
                                      load-option
                                      eval-option
                                      quit-option ; to quit once arguments processing is done
                                      quit-expression)
  `'(,name ,feature ,executable ,default-options ,load-option ,eval-option ,quit-option ,quit-expression))


(define-implementation abcl
  :feature :abcl
  :executable "abcl"
  :default-options ()
  :load-option ("--load" <arg>) 
  :eval-option ("--eval" <arg>)
  :quit-option ("--eval" "(quit)")
  :quit-expression "(extensions:quit)")

(define-implementation allegro
  :feature :allegro
  :executable "alisp"
  :default-options ("-batch" "-q")
  :load-option ("-L" <arg>) 
  :eval-option ()
  :quit-option "-kill"
  :quit-expression "(excl:exit)") ; dumps an "; Exiting" message...

(define-implementation ccl
  :feature :ccl
  :executable "ccl"
  :default-options ("--batch" "--o-init")
  :load-option ("--load" <arg>)
  :eval-option ("--eval" <arg>)
  :quit-option "--quit"
  :quit-expression "(ccl:quit)")

(define-implementation clisp
  :feature :clisp
  :executable "clisp"
  :default-options ("-ansi" "-q" "-norc" "-Kfull"  "-Efile" "UTF-8"
                            #|"-on-error" "debug"|#)
  :load-option (<arg>)
  :eval-option ("-x" <arg>)
  :quit-option ()
  :quit-expression "(ext:quit)")

(define-implementation ecl
  :feature :ecl
  :executable "ecl"
  :default-options ("-norc")
  :load-option ("-shell" <arg>)
  :eval-option ("-eval" <arg>)
  :quit-option ("-quit")
  :quit-expression "(ext:quit)")

sbcl          := sbcl
cmucl         := cmucl
openmcl       := openmcl

abcl_flags    := 
allegro_flags := 
ccl_flags     := 
clisp_flags   := -ansi -q -norc -kfull -e iso-8859-1 -efile utf-8 -eterminal utf-8  -on-error debug
cmucl_flags   := -noinit -nositeinit 
ecl_flags     := -norc 
openmcl_flags := 
sbcl_flags    := --noinform --sysinit /dev/null --userinit /dev/null 
