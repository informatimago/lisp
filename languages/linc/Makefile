all:help
help:
	@printf "make clean\n"
clean:
	-rm -rf *.o *.dSYM $(TEST_PROGRAMS) $(TEST_INTERMEDIARIES)
TEST_PROGRAMS = \
	test-c-array test-c-empty-struct-union test-c-struct-bit \
	test-expressions test-include test-statements test-types
TEST_INTERMEDIARIES = \
	test-expressions.c	test-include.c	test-statements.c	test-types.c
