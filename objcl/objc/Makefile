all:objc.lisp

# Darwin:
SWIG=/usr/local/bin/swig
OBJC_INCLUDES=
DEFINES=-D__OBJC2__=1 -DOBJC2_UNAVAILABLE=


# Linux:
#SWIG=/usr/bin/swig
#SWIG=/data/languages/swig/bin/swig
# OBJC_INCLUDES=-I/usr/lib/gcc/x86_64-linux-gnu/4.4/include
DEFINES=



INCLUDES=-I/usr/include $(OBJC_INCLUDES)
SWIG_OPTIONS=-cffi -swig-lisp -generate-typedef

objc-raw.lisp:objc.i Makefile
	$(SWIG) $(SWIG_OPTIONS) $(INCLUDES) $(DEFINES) objc.i
	sed \
	< objc.lisp > objc-raw.lisp
	rm objc.lisp

objc.lisp:objc-raw.lisp Makefile lispify-objc
	./lispify-objc < objc-raw.lisp > objc.lisp

clean:
	- rm -f objc-raw.lisp


CC=/opt/llvm/bin/objc -isysroot /
CFLAGS=
#CFLAGS=-v
#CFLAGS=-fblocks 
# -I/opt/llvm/lib/objc/3.3/include
LDFLAGS=-L/opt/llvm/lib -lobjc
ast:ast.c Makefile
	@ true $(CC) $(CFLAGS) -I. -g3 -ggdb3 -E -o /dev/stdout ast.c
	@ $(CC) $(CFLAGS) -I. -g3 -ggdb3 -o ast ast.c $(LDFLAGS)

test-ast:ast
	@ LD_LIBRARY_PATH=/opt/llvm/lib:$LD_LIBRARY_PATH ./ast --test example.c

run-ast:ast
	@ LD_LIBRARY_PATH=/opt/llvm/lib:$LD_LIBRARY_PATH ./ast example.c

#### THE END ####

