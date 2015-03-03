
all:hw1 hw2 hw3

hw1:hw.lisp hello.lisp word.lisp
	ll -o hw1  hello.lisp word.lisp hw.lisp



hw2:hw.lisp hello.ol word.ol
	ll -o hw2 hw.lisp -l hello.ol -l  word.ol 

hw3:hello.lisp libhw.ol
	ll -o hw3 hello.lisp libhw.ol

libhw.ol: hello.lisp word.lisp
	ll -o libhw.ol hello.lisp word.lisp

hello.ol:hello.lisp
	ll -c -o hello.ol hello.lisp

word.ol:word.lisp
	ll -c word.lisp
