(in-package :CL-USER)
(defpackage "CLIKI"
  (:export cliki-view-handler cliki-edit-handler cliki-instance cliki-view
	   cliki-url-root
	   with-page-surround
	   cliki-css-text cliki-pages cliki-page-header cliki-page-footer
	   cliki-user-name short-forms

	   authed-cliki-handler authed-cliki-edit-handler

	   write-a-href request-cliki cliki-data-directory
	   html-for-keyword form-element-for-keyword
	   parse-form-element-for-keyword
	   request-cliki cliki-default-page-name
	   cliki-page find-page page-pathname
	   compute-index-for-page add-to-index-for-page make-index-for-page
	   search-term-relevance search-term-summary
	   edit-handler

	   check-page-save-allowed cliki-page-save-rejected
	   
	   elided-stream)
	   
  (:import-from #+sbcl "SB-GRAY" #+cmu "EXT"
                "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM")
  (:use "NET.TELENT.DATE" "COMMON-LISP" "SB-GRAY" "ARANEIDA" "SB-BSD-SOCKETS"))

;;; language for inline searches and stuff
(defpackage "CLIKI-PAGES" (:use #| nil |# ))

;;; used for collection of markup routines
(defpackage "CLIKI-HTML")
