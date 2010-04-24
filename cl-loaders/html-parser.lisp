;;;; -*- coding:utf-8 -*-

(let ((path
       "/local/share/lisp/packages/edu/mit/ai/cl-http-70-23/html-parser/v10/"))
  (setf (logical-pathname-translations "HTML-PARSER")
        `(("**;*.*.*"  path)))

  (mapc (lambda (f) (load (compile-file (concatenate 'string path f ".lisp"))))
        '(
          "packages"
          "tokenizer"
          "plist"
          "defs"
          "patmatch"
          "rewrite-engine"
          "rewrite-rules"
          "html-tags"
          "html-reader"
          "html-parser"
          "html-utilities"))
  )

;;;; html-parser.lisp                 -- 2003-09-15 04:54:44 -- pascal   ;;;;
