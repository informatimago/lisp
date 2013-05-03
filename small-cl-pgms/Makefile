all::help
HELPFORMAT="make %-20s \# %s\n"

help::
	@printf $(HELPFORMAT) generate "Generate the index.html page."
generate:index.html access-rights clean

index.html:init.lisp index.lisp Makefile
	clisp -norc -q -ansi  -E iso-8859-1  -x '(prog1 (values) (setf *load-verbose* nil) (load "init.lisp") (load "index.lisp"))'|tr -d '\015'|sed -e 's/^;.*//' -e '/^NIL$$/d' > index.html

help::
	@printf $(HELPFORMAT) access-rights "Sets the access-rights for publication."
access-rights:
	publish .

help::
	@printf $(HELPFORMAT) clean "Erase backup and fasl files."
clean:
	-rm -rf  *~ *.lx*fsl

help::
	@printf $(HELPFORMAT) cleanall "make clean and erase targets."
cleanall:clean
	-rm -rf index.html what-implementation

#(with-open-file (*STANDARD-OUTPUT* "index.html" :direction :output :if-exists :supersede :if-does-not-exist :create)

help::
	@printf $(HELPFORMAT) what-implementation "Builds the what-implementation executable."
wi what-implementation:what-implementation.lisp
	echo '(load (compile-file "what-implementation.lisp"))' | ccl -n 

help::
	@printf $(HELPFORMAT) install "Install the what-implementation executable."
install:what-implementation
	mv what-implementation /usr/local/bin/

#### THE END ####

