all:  
	@echo "# choose:"
	@echo "make linux"

linux:objc.lisp

INCLUDES= -I. -I/usr/lib/gcc/x86_64-linux-gnu/4.7/include/
OPTS=-fPIC -g -c  


objc.lisp:objc.i Makefile
	swig $(INCLUDES) -Dobjc_EXPORT=extern -cffi -generate-typedef -swig-lisp objc.i 
	( sed \
	 	-e 1i\\ -e '(in-package "COM.INFORMATIMAGO.OBJC.GNU")' \
		-e 's/&body/cl:&/g' \
	 	-e 's/#oL/0/g' \
	 	-e 's/\([0-9]\+\)U\?L\+/\1/g' \
	 	-e 's/(cl:- \([0-9]\+\) 1)/(cl:- -\1 1)/g' \
		-e '/(cl:defconstant _C_/d' \
	 	< objc.lisp  ;\
	sed \
		-n \
		-e 's/#define \(_C_[^ ]*\) *'\''\(.\)'\'' *$$/(cl:defconstant \1 #\\\2)/p' \
		< /usr/lib/gcc/x86_64-linux-gnu/4.7/include/objc/runtime.h \
	) > objc.new && mv objc.new objc.lisp

# objc_wrap.o:objc_wrap.c



#### THE END ####

