LISP=sbcl --noinform --noprogrammer # --no-sysinit 

all: package

package: 
	echo "(asdf:operate 'asdf:load-op 'cclan)" "(cclan:cvs-tag 'cliki)"  "(cclan:make-tar-file 'cliki)" |$(LISP)

