all:
empty-macro:
	gcc -E -o - empty-macro.c
include-macro:
	gcc -I.. -E -o - include-macro.h
priority:
	gcc -E -o - priority.h
concat:
	gcc -E -o - concat.c
stringify:
	gcc -E -o - stringify.c
recursive:
	gcc -E -o - recursive.c
variadic:
	gcc -E -o - variadic.c
shadow:
	gcc -E -o - shadow.c
substitute:
	gcc -E -o - substitute.c
built-ins:
	gcc -E -o - built-ins.c
test:
	gcc -E -o - test.c
trigraphs:
	gcc -E -o - -trigraphs trigraphs.c
errors:
	gcc -E -o - errors.c
line:
	gcc -E -o - line.c
ifdef:
	gcc -E -o - ifdef.c | cat -s
if:
	gcc -E -o - -Wundef if.c | cat -s
if-embedded:
	gcc -E -o - if-embedded.c | cat -s
line:
	gcc -E -o - line.c | cat -s

