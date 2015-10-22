PREFIX=/usr/local
all::
doc::
install::

CSS="/default.css"
RSTHTMLOPT=\
	--leave-comments \
	--link-stylesheet --stylesheet=$(CSS)

doc::
	pandoc -f rst -t asciidoc < pipe-article-fr.txt >pipe-article-fr.asc
	pandoc -f rst -t asciidoc < pipe-article-en.txt >pipe-article-en.asc

doc::
	rst2html $(RSTHTMLOPT) < pipe-article-fr.txt |../tools/html-unwrap-document.lisp > pipe-article-fr.html.in
	rst2html $(RSTHTMLOPT) < pipe-article-en.txt |../tools/html-unwrap-document.lisp > pipe-article-en.html.in

#### THE END ####
