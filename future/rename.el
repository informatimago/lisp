;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rename.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;; (mapcar (lambda (pat)
;;;;           (update-informatimago-in-files
;;;;            (file-expand-wildcards pat)))
;;;;         '("/home/pjb/src/public/lisp/clext/*.lisp"
;;;;           "/home/pjb/src/public/lisp/cl-posix/*.lisp"
;;;;           "/home/pjb/src/public/lisp/clisp/*.lisp"
;;;;           "/home/pjb/src/public/lisp/clmisc/*.lisp"
;;;;           "/home/pjb/src/public/lisp/sbcl/*.lisp"
;;;;           "/home/pjb/src/public/lisp/susv3/*.lisp"))
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(require 'cl)

(defmacro defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))

(defparameter *informatimago-replacements*
  '(("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(passwd\\|group\\|aliases\\)\\>" "\\1unix.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(rfc3548\\)\\>" "\\1rfc3548.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(rfc2822\\)\\>" "\\1rfc2822.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(regexp-posix\\|regexp-emacs\\)\\>" "\\1regexp.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(tree-to-ascii\\|picture\\|cons-to-ascii\\)\\>" "\\1picture.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(scanner\\|parser\\)\\>" "\\1parser.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(web-cache\\|rfc2047\\|repl\\|iso646\\|iso15924\\|isbn\\|graf\\|dictionary\\|database\\|avl\\)\\>" "\\1obsolete-or-incomplepte.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(make-depends\\)\\>" "\\1make-depends.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(generic-cl\\)\\>" "\\1lisp.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(source-text\\)\\>" "\\1lisp-text.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(source-form\\)\\>" "\\1lisp-sexp.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(reader\\)\\>" "\\1lisp-reader.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(invoice\\)\\>" "\\1invoice.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(interactive\\|browser\\)\\>" "\\1interactive.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(htrans\\|hquery\\)\\>" "\\1http.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(parse-html\\)\\>" "\\1html-parser.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(html\\)\\>" "\\1html-generator.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(html401\\|html-entities\\)\\>" "\\1html-base.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(memory\\|heap\\)\\>" "\\1heap.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(graph-dot\\)\\>" "\\1graphviz.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(stream\\|peek-stream\\|float-binio\\|file\\|cache\\)\\>" "\\1file.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(ed\\)\\>" "\\1ed.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(tree-to-diagram\\)\\>" "\\1diagram.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(data-encoding\\|data-encoding-test\\)\\>" "\\1data-encoding.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(cxx\\)\\>" "\\1cxx.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(csv\\)\\>" "\\1csv.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(version\\|utility\\|update-iso3166\\|tea\\|string\\|raiden\\|queue\\|pmatch\\|package\\|message-queue\\|llrbtree\\|list\\|iso639a\\|iso4217\\|iso3166\\|iana-character-sets\\|graph\\|ecma048\\|dll\\|dictionary\\|date\\|combination\\|character-sets\\|bset\\|brelation\\|ascii\\|array\\|activity\\)\\>" "\\1cesarum.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(rib\\|iban\\)\\>" "\\1bank.\\2")
    ("\\<\\(com\\.informatimago\\.common-lisp\\.\\)\\(primes\\)\\>" "\\1arithmetic.\\2")))





(defmacro* with-file (file-and-options &body body)
  "
find-file or find-file-literally, process body, and optionally save the buffer
and kill it.
save is not done if body exits exceptionnaly.
kill is always done as specified.
FILE-AND-OPTION: either an atom evaluated to a path,
                 or (path &key (save t) (kill t) (literal nil))
"
  (if (atom file-and-options)
      `(with-file (,file-and-options) ,@body)
      ;; destructuring-bind is broken, we cannot give anything else than nil
      ;; as default values:
      (destructuring-bind (path &key (save nil savep) (kill nil killp)
                                (literal nil literalp))
          file-and-options
        (unless savep (setf save t))
        (unless killp (setf kill t))
        `(unwind-protect
              (progn
                (,(if literal 'find-file-literally 'find-file) ,path)
                (prog1 (save-excursion ,@body)
                  ,(when save `(save-buffer 1))))
           ,(when kill
                  `(kill-buffer (current-buffer)))))))


(defun multifile-replace-regexp (regexp to-string files &optional delimited)
  "Replace the all occurences of `regexp' by `to-string' in all the `files'.
`delimited', if non-nil, means replace only matches surrounded by word boundaries.
 "
  (dolist (file files)
    (with-file (file :save t :kill nil)
      (message "Processing %S" file)
      (beginning-of-buffer)
      (replace-regexp regexp to-string delimited))))



(defun update-informatimago-in-files (files)
  (dolist (replacement *informatimago-replacements*)
    (multifile-replace-regexp (first replacement) (second replacement) files)))








(defun generate-replacements ()
  ;; This function uses stuff in
  ;; http://www.informatimago.com/develop/emacs/index.html
  ;; http://git.informatimago.com/viewgit/index.php?a=summary&p=public/emacs
  (let* ((files 
          '(
            ("common-lisp" "arithmetic" "primes.lisp")

            ("common-lisp" "bank" "iban.lisp")
            ("common-lisp" "bank" "rib.lisp")

            ("common-lisp" "cesarum" "activity.lisp")
            ("common-lisp" "cesarum" "array.lisp")
            ("common-lisp" "cesarum" "ascii.lisp")
            ("common-lisp" "cesarum" "brelation.lisp")
            ("common-lisp" "cesarum" "bset.lisp")
            ("common-lisp" "cesarum" "character-sets.lisp")
            ("common-lisp" "cesarum" "combination.lisp")
            ("common-lisp" "cesarum" "date.lisp")
            ("common-lisp" "cesarum" "dictionary.lisp")
            ("common-lisp" "cesarum" "dll.lisp")
            ("common-lisp" "cesarum" "ecma048.lisp")
            ("common-lisp" "cesarum" "graph.lisp")
            ("common-lisp" "cesarum" "iana-character-sets.lisp")
            ("common-lisp" "cesarum" "iso3166.lisp")
            ("common-lisp" "cesarum" "iso4217.lisp")
            ("common-lisp" "cesarum" "iso639a.lisp")
            ("common-lisp" "cesarum" "list.lisp")
            ("common-lisp" "cesarum" "llrbtree.lisp")
            ("common-lisp" "cesarum" "message-queue.lisp")
            ("common-lisp" "cesarum" "package.lisp")
            ("common-lisp" "cesarum" "pmatch.lisp")
            ("common-lisp" "cesarum" "queue.lisp")
            ("common-lisp" "cesarum" "raiden.lisp")
            ("common-lisp" "cesarum" "string.lisp")
            ("common-lisp" "cesarum" "tea.lisp")
            ("common-lisp" "cesarum" "update-iso3166.lisp")
            ("common-lisp" "cesarum" "utility.lisp")
            ("common-lisp" "cesarum" "version.lisp")

            ("common-lisp" "csv" "csv.lisp")
            ("common-lisp" "cxx" "cxx.lisp")
            ("common-lisp" "data-encoding" "data-encoding-test.lisp")
            ("common-lisp" "data-encoding" "data-encoding.lisp")
            ("common-lisp" "diagram" "tree-to-diagram.lisp")
            ("common-lisp" "ed" "ed.lisp")
            ("common-lisp" "file" "cache.lisp")
            ("common-lisp" "file" "file.lisp")
            ("common-lisp" "file" "float-binio.lisp")
            ("common-lisp" "file" "peek-stream.lisp")
            ("common-lisp" "file" "stream.lisp")
            ("common-lisp" "graphviz" "graph-dot.lisp")
            ("common-lisp" "heap" "heap.lisp")
            ("common-lisp" "heap" "memory.lisp")
            ("common-lisp" "html-base" "html-entities.lisp")
            ("common-lisp" "html-base" "html401.lisp")
            ("common-lisp" "html-generator" "html.lisp")
            ("common-lisp" "html-parser" "parse-html.lisp")
            ("common-lisp" "http" "hquery.lisp")
            ("common-lisp" "http" "htrans.lisp")
            ("common-lisp" "interactive" "browser.lisp")
            ("common-lisp" "interactive" "interactive.lisp")
            ("common-lisp" "invoice" "invoice.lisp")
            ("common-lisp" "lisp-reader" "reader.lisp")
            ("common-lisp" "lisp-sexp" "source-form.lisp")
            ("common-lisp" "lisp-text" "source-text.lisp")
            ("common-lisp" "lisp" "generic-cl.lisp")
            ("common-lisp" "make-depends" "make-depends.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "avl.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "database.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "dictionary.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "graf.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "isbn.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "iso15924.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "iso646.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "repl.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "rfc2047.lisp")
            ("common-lisp" "obsolete-or-incomplepte" "web-cache.lisp")
            ("common-lisp" "parser" "parser.lisp")
            ("common-lisp" "parser" "scanner.lisp")
            ("common-lisp" "picture" "cons-to-ascii.lisp")
            ("common-lisp" "picture" "picture.lisp")
            ("common-lisp" "picture" "tree-to-ascii.lisp")
            ("common-lisp" "regexp" "regexp-emacs.lisp")
            ("common-lisp" "regexp" "regexp-posix.lisp")
            ("common-lisp" "rfc2822" "rfc2822.lisp")
            ("common-lisp" "rfc3548" "rfc3548.lisp")
            ("common-lisp" "unix" "aliases.lisp")
            ("common-lisp" "unix" "group.lisp")
            ("common-lisp" "unix" "passwd.lisp")
            ))
         (paths (mapcar (lambda (item)
                          (format "/home/pjb/src/public/lisp/%s/%s/%s"
                                  (first item) (second item) (third item)))
                        files)))
    (mapcar (lambda (class)
              (let* ((lib      (first  (first class)))
                     (sublib   (second (first class)))
                     (packages (mapcar (lambda (item) (pathname-name* (third item))) class))
                     (regexp (with-output-to-string
                                 (princ (format "\\<\\(com\\.informatimago\\.%s\\.\\)\\(" lib))
                               (princ (first packages))
                               (dolist (package (rest packages))
                                 (princ "\\|")
                                 (princ package))
                               (princ "\\)\\>"))))
                (list regexp (concat  "\\1" sublib ".\\2"))))
            (equivalence-classes files (lambda (a b) (and (string= (second a) (second b))
                                                          (string= (first  a) (first  b))))))))

;;;; THE END ;;;;
