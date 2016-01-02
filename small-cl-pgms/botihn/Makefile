all:botihn
botihn: com.informatimago.small-cl-pgms.botihn.asd  botihn.lisp generate-application.lisp
	ccl -norc < generate-application.lisp

install:botihn
	install -m 755 botihn /usr/local/sbin/botihn
