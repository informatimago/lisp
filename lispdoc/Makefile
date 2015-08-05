all:
	( echo '(ql:quickload :com.informatimago.lispdoc)' ;\
	  echo '(com.informatimago.lispdoc.run:doc)' ;\
	  echo '(ccl::quit)' ) | ccl 

textdoc:
	( echo '(ql:quickload :com.informatimago.lispdoc)' ;\
	  echo '(com.informatimago.lispdoc.run:doc :doc-directory "/tmp/doc/" :target-class (quote com.informatimago.lispdoc.generate.text:text-documentation))' ;\
	  echo '(ccl::quit)' ) | ccl 
