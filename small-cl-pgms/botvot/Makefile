PREFIX=/usr/local
all::
doc::
clean::
install::

CSS="/default.css"
RSTHTMLOPT=\
	--leave-comments \
	--link-stylesheet --stylesheet=$(CSS)


all::botvot

botvot: com.informatimago.small-cl-pgms.botvot.asd  botvot.lisp generate-application.lisp
	ccl -norc < generate-application.lisp

install::botvot
	install -m 755 botvot /usr/local/sbin/botvot

doc::html
	pandoc -f rst -t asciidoc < botvot.txt > botvot-fr.asc

clean::
	- rm -f botvot-fr.html botvot-fr.html.in botvot-fr.asc

html::botvot-fr.html

html.in::botvot-fr.html.in

botvot-fr.html.in:botvot.txt
	rst2html $(RSTHTMLOPT) < botvot.txt | ../../tools/html-unwrap-document.lisp > botvot-fr.html.in
botvot-fr.html:botvot-fr.html.in
	../../tools/html-wrap-document.lisp < $< > $@ || rm $@

.PHONY:doc html
#### THE END ####
