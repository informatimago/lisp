;;; -*- Lisp -*-
(defpackage :cliki-system (:use #:cl #:asdf))
(in-package :cliki-system)

(defsystem cliki
  :depends-on (ARANEIDA net-telent-date xmls diff cl-ppcre)
  :version "0.4.4"
  :components ((:file "defpackage")               
	       (:file "utilities" :depends-on ("defpackage"))
	       (:file "cliki-instance-class" :depends-on ("defpackage"))
	       (:file "cliki-instance" :depends-on
		      ("utilities" "cliki-instance-class"))
	       (:file "cliki-page-class" :depends-on ("defpackage"))
	       (:file "cliki-page" :depends-on ("cliki-page-class" "cliki-instance-class" "utilities"))
	       (:file "cliki-request-class" :depends-on ("defpackage"))
	       (:file "cliki-request" :depends-on
		      ("cliki-instance-class" "cliki-request-class"))
	       (:file "edit-handler-class" 
		      :depends-on ("defpackage" "utilities" "recent-changes"))
	       (:file "edit-handler" :depends-on ("cliki-request-class"
						  "cliki-page-class"
						  "view"
						  "edit-handler-class"))
	       (:file "authed-cliki-class" :depends-on ("cliki-instance"
							"edit-handler"))
	       (:file "authed-cliki" :depends-on ("authed-cliki-class"))
	       (:file "index"
		      :depends-on ("cliki-page" "cliki-instance"
						"cliki-request"))
	       (:file "hyperspec" :depends-on ("defpackage"))
               ;(:file "link-checker") :depends-on ("indexing"))
               (:file "view" :depends-on
		      ("hyperspec" "utilities" "cliki-request"))
               (:file "recent-changes"
		      :depends-on ("view" "cliki-instance"  "cliki-request"))
	       (:file "view-source"
		      :depends-on ("cliki-instance" "cliki-request"))
               (:file "buffered-output-stream-class"
		      :depends-on ("defpackage"))
	       (:file "buffered-output-stream" :depends-on
		      ("buffered-output-stream-class"))
               (:file "elided-stream" :depends-on ("buffered-output-stream"))
	       (:file "strip-html-stream" :depends-on ("buffered-output-stream"))
               (:file "search" :depends-on ("index" "elided-stream" "strip-html-stream"))
               (:file "handlers" :depends-on
                      ("view" "utilities" "index" "view-source" "search"))
	       (:file "cliki-skin" :depends-on ("cliki-instance-class"))
	       (:file "cliki-net"  :depends-on ("cliki-instance"))
	       (:file "versions" :depends-on ("defpackage" "cliki-page"))
	       (:static-file "example" :pathname "example.lisp")
	       (:static-file "TODO")
	       (:static-file "make-TODO" :pathname "make-TODO.pl")
	       (:static-file "NEWS")
	       (:static-file "README")))


