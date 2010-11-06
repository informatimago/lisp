
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

SBCL          := sbcl
CMUCL         := cmucl
OPENMCL       := openmcl

ABCL_FLAGS    := 
ALLEGRO_FLAGS := 
CCL_FLAGS     := 
CLISP_FLAGS   := -ansi -q -norc -Kfull -E iso-8859-1 -Efile UTF-8 -Eterminal UTF-8  -on-error debug
CMUCL_FLAGS   := -noinit -nositeinit 
ECL_FLAGS     := -norc 
OPENMCL_FLAGS := 
SBCL_FLAGS    := --noinform --sysinit /dev/null --userinit /dev/null 
