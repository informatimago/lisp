all:generate

generate:index.html access-rights clean

index.html:init.lisp index.lisp Makefile
	clisp -norc -q -ansi  -E iso-8859-1  -x '(prog1 (values) (setf *load-verbose* nil) (load "init.lisp") (load "index.lisp"))'|tr -d '\015'|sed -e 's/^;.*//' -e '/^NIL$$/d' > index.html

access-rights:
	publish .

clean:
	-rm -rf  *~
cleanall:clean
	-rm -rf index.html

#(with-open-file (*STANDARD-OUTPUT* "index.html" :direction :output :if-exists :supersede :if-does-not-exist :create)
#### Makefile                         --                     --          ####
