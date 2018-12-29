PREFIX=/usr/local
all::
doc::
install::

include ../implementations.mk

CSS="/default.css"
RSTHTMLOPT=\
	--leave-comments \
	--link-stylesheet --stylesheet=$(CSS)

doc::documentation
documentation::html pipe-article-fr.asc pipe-article-en.asc
html::pipe-article-fr.html pipe-article-en.html
html.in::pipe-article-fr.html.in pipe-article-en.html.in

clean::
	-@rm -f pipe-article-fr.asc pipe-article-en.asc pipe-article-fr.html.in pipe-article-en.html.in pipe-article-fr.html pipe-article-en.html

pipe-article-fr.asc:pipe-article-fr.txt
	pandoc -f rst -t asciidoc < pipe-article-fr.txt >pipe-article-fr.asc
pipe-article-en.asc:pipe-article-en.txt
	pandoc -f rst -t asciidoc < pipe-article-en.txt >pipe-article-en.asc
pipe-article-fr.html.in:pipe-article-fr.txt
	rst2html $(RSTHTMLOPT) < pipe-article-fr.txt |../tools/html-unwrap-document.lisp > pipe-article-fr.html.in
pipe-article-en.html.in:pipe-article-en.txt
	rst2html $(RSTHTMLOPT) < pipe-article-en.txt |../tools/html-unwrap-document.lisp > pipe-article-en.html.in
pipe-article-fr.html:pipe-article-fr.html.in
	../tools/html-wrap-document.lisp < pipe-article-fr.html.in > pipe-article-fr.html
pipe-article-en.html:pipe-article-en.html.in
	../tools/html-wrap-document.lisp < pipe-article-en.html.in > pipe-article-en.html

.PHONY:doc documentation html html.in

#### THE END ####
