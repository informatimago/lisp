PREFIX=/usr/local
all::
doc::
install::

CSS="/default.css"
RSTHTMLOPT=\
	--leave-comments \
	--link-stylesheet --stylesheet=$(CSS)


all::botihn
botihn: com.informatimago.small-cl-pgms.botihn.asd  botihn.lisp generate-application.lisp
	ccl -norc < generate-application.lisp

doc::
	pandoc -f rst -t asciidoc < botihn.txt >botihn-fr.asc
	rst2html $(RSTHTMLOPT) < botihn.txt > botihn-fr.html

#### THE END ####
