PREFIX=/usr/local
all::
doc::
clean::
install::

CSS="/default.css"
RSTHTMLOPT=\
	--leave-comments \
	--link-stylesheet --stylesheet=$(CSS)


all::botihn

botihn: com.informatimago.small-cl-pgms.botihn.asd  botihn.lisp generate-application.lisp
	ccl -norc < generate-application.lisp

install:botihn
	install -m 755 botihn /usr/local/sbin/botihn

doc::html
	pandoc -f rst -t asciidoc < botihn.txt >botihn-fr.asc

clean::
	- rm -f botihn-fr.html botihn-fr.html.in

html::botihn-fr.html

html.in::botihn-fr.html.in

botihn-fr.html.in:botihn.txt
	rst2html $(RSTHTMLOPT) < botihn.txt | ../../tools/html-unwrap-document.lisp > botihn-fr.html.in
botihn-fr.html:botihn-fr.html.in
	../../tools/html-wrap-document.lisp < $< > $@ || rm $@

.PHONY:doc html
#### THE END ####
