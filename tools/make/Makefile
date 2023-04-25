NAME = make-print-vars
TEST_MAKEFILE ?= /build/pbourguignon/work/build.devel/.makedump

all:$(NAME)
	   $(NAME) --help \
	&& $(NAME) --mangle $(TEST_MAKEFILE) > m.sed \
	&& $(NAME)          $(TEST_MAKEFILE) > $(TEST_MAKEFILE)-vars

$(NAME):$(NAME).lisp
	ccl < generate-exe.lisp >$(NAME).log 2>$(NAME).err || ( cat $(NAME).err ; exit 200 )

clean:
	-rm -f *.lx64fsl
	-rm -f *.out *.err
	-rm -f system-index.txt
