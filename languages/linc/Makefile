
TEST_SEXPC = \
	test-expressions.sexpc \
	test-include.sexpc \
	test-statements.sexpc \
	test-types.sexpc \
	test-macros.sexpc

TEST_C = \
	test-c-array.c \
	test-c-empty-struct-union.c \
	test-c-struct-bit.c

TEST_C_GEN = \
	$(TEST_SEXPC:.sexpc=.c)

TEST_O = \
	$(TEST_C:.c=.o) \
	$(TEST_C_GEN:.c=.o)

TEST_PROGRAMS = \
	$(TEST_O:.o=)


all:$(TEST_PROGRAMS)

help:
	@printf "make clean\n"

clean:
	-rm -rf *.o *.dSYM $(TEST_PROGRAMS) $(TEST_O) $(TEST_C_GEN)
	-rm -rf test-include-dummy.c

$(TEST_C_GEN):$(TEST_SEXPC)
	ccl --load generate.lisp --eval '(quit)'

.c.o: ; $(CC) -Werror -Wall -c -o $@ $^

test-c-array:test-c-array.o
test-c-empty-struct-union:test-c-empty-struct-union.o
test-c-struct-bit:test-c-struct-bit.o
test-expressions:test-expressions.o
test-include:test-include.o
test-statements:test-statements.o
test-types:test-types.o

test-include.o:test-include.c
	cmp test-include.c test-include.expected
	echo 'int main(){return 0;}' > test-include-dummy.c
	$(CC) -Werror -Wall -c -o test-include.o test-include-dummy.c
