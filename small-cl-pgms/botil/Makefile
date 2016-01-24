PREFIX=/usr/local
all::
doc::
clean::
install::

CSS="/default.css"
RSTHTMLOPT=\
	--leave-comments \
	--link-stylesheet --stylesheet=$(CSS)


all::botil

botil: com.informatimago.small-cl-pgms.botil.asd  botil.lisp generate-application.lisp
	ccl -norc < generate-application.lisp

disabled-doc::html
	pandoc -f rst -t asciidoc < botil.txt >botil-fr.asc

clean::
	- rm -f botil-fr.html botil-fr.html.in

html::botil-fr.html

html.in::botil-fr.html.in

botil-fr.html.in:botil.txt
	rst2html $(RSTHTMLOPT) < botil.txt | ../../tools/html-unwrap-document.lisp > botil-fr.html.in
botil-fr.html:botil-fr.html.in
	../../tools/html-wrap-document.lisp < $< > $@ || rm $@

.PHONY:doc html
#### THE END ####
