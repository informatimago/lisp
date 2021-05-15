PREFIX=/usr/local
all::
doc::
clean::
install::

all::botvot

botvot: com.informatimago.small-cl-pgms.botvot.asd  botvot.lisp generate-application.lisp
	ccl --no-init --load generate-application.lisp --eval '(ccl:quit)'

install::botvot
	install -m 755 botvot /usr/local/sbin/botvot

# pandoc -f org -t asciidoc < botvot.org > botvot-fr.asc

.PHONY:doc html
#### THE END ####
