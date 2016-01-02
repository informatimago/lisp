all:documentation.pdf

documentation.pdf:documentation.rst
	rst2pdf documentation.rst

documentation.html:documentation.rst
	rst2html documentation.rst > documentation.html

showpdf show-pdfs:documentation.pdf
	open documentation.pdf


html:documentation.html
.PHONY:show-pdf showpdf html
