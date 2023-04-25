all:make-print-vars
	   make-print-vars --help \
	&& make-print-vars --mangle /build/pbourguignon/work/build.devel/.makedump > m.sed \
	&& make-print-vars          /build/pbourguignon/work/build.devel/.makedump > /build/pbourguignon/work/build.devel/.makedump-vars


make-print-vars:make-print-vars.lisp
	ccl < generate-exe.lisp >make-print-vars.log 2>make-print-vars.err || ( cat make-print-vars.err ; exit 200 )

