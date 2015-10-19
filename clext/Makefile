PREFIX=/usr/local
all::
doc::
install::

CSS="/default.css"
STYLE=--link-stylesheet --stylesheet=$(CSS)

doc::
	pandoc -f rst -t asciidoc < pipe-article-fr.txt >pipe-article-fr.asc
	pandoc -f rst -t asciidoc < pipe-article-en.txt >pipe-article-en.asc

doc::
	rst2html $(STYLE) < pipe-article-fr.txt >pipe-article-fr.html
	rst2html $(STYLE) < pipe-article-en.txt >pipe-article-en.html

#### THE END ####
