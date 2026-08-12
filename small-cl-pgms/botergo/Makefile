PREFIX=/usr/local
all::
clean::
install::

all::botergo

botergo: com.informatimago.small-cl-pgms.botergo.asd botergo.lisp generate-application.lisp
	ccl -norc < generate-application.lisp

install::botergo
	install -m 755 botergo $(PREFIX)/sbin/botergo

clean::
	- rm -f botergo

.PHONY: all clean install
#### THE END ####
