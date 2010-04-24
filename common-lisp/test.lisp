;;;; -*- coding:utf-8 -*-


(setf *print-circle* nil)
(pprint (macroexpand-1 (quote
(DEFINE-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PARSE-HTML"
  (:DOCUMENTATION  "This package exports functions to parse HTML pages.")
;;;  (:SHADOW MAP)
  (:FROM "COMMON-LISP"                                  :IMPORT :ALL)
  ;; (:FROM "COM.HP.ZEBU" :IMPORT "KB-DOMAIN")
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING"
         :IMPORT "STRING-REPLACE" "SPLIT-STRING" "UNSPLIT-STRING")
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.LIST"           :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY"        :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.PEEK-STREAM"    :IMPORT :ALL)
  (:FROM "COM.INFORMATIMAGO.COMMON-LISP.HTML-ISO8879-1" :IMPORT :ALL)
  (:EXPORT
;;;            A ABBR ACRONYM ADDRESS APPLET AREA B BASE BASEFONT BDO BIG
;;;            BLOCKQUOTE BODY BR BUTTON CAPTION CENTER CITE CODE COL
;;;            COLGROUP DD DEL DFN DIR DIV DL DT EM FIELDSET FONT FORM
;;;            FRAME FRAMESET H1 H2 H3 H4 H5 H6 HEAD HR HTML I IFRAME IMG
;;;            INPUT INS ISINDEX KBD LABEL LEGEND LI LINK MAP MENU META
;;;            NOFRAMES NOSCRIPT OBJECT OL OPTGROUP OPTION P PARAM PRE Q S
;;;            SAMP SCRIPT SELECT SMALL SPAN STRIKE STRONG STYLE SUB SUP
;;;            TABLE TBODY TD TEXTAREA TFOOT TH THEAD TITLE TR TT U UL VAR
   "PARSE-HTML-FILE" "PARSE-HTML-STRING" "UNPARSE-HTML"
   "HTML-TAG" "HTML-ATTRIBUTES" "HTML-CONTENTS" "HTML-ATTRIBUTE" ))

)))

(pprint (macroexpand-1 (quote
 )))


(PROGN (IN-PACKAGE "COMMON-LISP-USER")
 (DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PARSE-HTML"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.HTML-ISO8879-1"
   "COM.INFORMATIMAGO.COMMON-LISP.PEEK-STREAM"
   "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "COM.INFORMATIMAGO.COMMON-LISP.LIST"
   "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.STRING")
  (:EXPORT "HTML-ATTRIBUTE" "HTML-CONTENTS" "HTML-ATTRIBUTES" "HTML-TAG"
   "UNPARSE-HTML" "PARSE-HTML-STRING" "PARSE-HTML-FILE")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.STRING" "UNSPLIT-STRING"
   "SPLIT-STRING" "STRING-REPLACE")
  (:DOCUMENTATION "This package exports functions to parse HTML pages."))
 (IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PARSE-HTML"))



(in-package "PACKAGE")
(DEFMACRO DEFINE-PACKAGE (NAME &REST DECLARATIONS)
  "
DO:         Declares a package.
            This includes loading the packages depended on,
            adding nicknames to the packages used under these nicknames,
            defining the package, and going into it (with IN-PACKAGE).
"
  (SETQ NAME (STRING NAME))
  (MULTIPLE-VALUE-BIND (DEPENDENCIES RENAMES DEFPACK-ARGS)
      (PARSE-PACKAGE-DECLARATIONS DECLARATIONS)
    (let* ((used-packages
            (nconc
             (reduce (function nconc)
                     (mapcar (function cdr)
                             (remove :use defpack-args
                                     :test (complement (function eql))
                                     :key (function first))))
             (mapcar (function second) (remove :import-from defpack-args
                                               :test (complement (function eql))
                                               :key (function first)))))
           (also-use-packages
            (set-difference (remove-if  (function BUILT-IN-p) dependencies)
                            used-packages :test (function string=))))
      `(progn
         (in-package "COMMON-LISP-USER")
         ,@(when also-use-packages
                 `((declaim (declaration common-lisp-user::also-use-packages)
                             (common-lisp-user::also-use-packages
                              ,@also-use-packages))))
         ,@(when renames
                 `((eval-when (:compile-toplevel :load-toplevel :execute)
                      ,@(mapcar
                         (lambda (rename) `(PACKAGE:ADD-NICKNAME ,(CAR RENAME) ,(CDR RENAME)))
                         renames))))
         (DEFPACKAGE ,NAME ,@DEFPACK-ARGS)
         (IN-PACKAGE ,NAME)))))
(in-package "COMMON-LISP-USER")




(in-package "COMMON-LISP-USER")
(declaim (declaration also-use-packages)
         (also-use-packages
          "COM.INFORMATIMAGO.COMMON-LISP.ECMA048"))

(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.STRING"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY"
        "COMMON-LISP")
  (:EXPORT "LOCALIZE" "DEFTRANSLATION" "STRING-JUSTIFY-LEFT" "STRING-PAD"
           "PREFIXP" "SPLIT-NAME-VALUE" "STRING-REPLACE" "UNSPLIT-STRING"
           "SPLIT-STRING" "SPLIT-ESCAPED-STRING"
           "IMPLODE-STRING" "EXPLODE-STRING")
  (:DOCUMENTATION
   "This package exports some string processing functions.

    Copyright Pascal J. Bourguignon 2002 - 2005
    This package is provided under the GNU General Public License.
    See the source file for details."))

(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.STRING")


(load "make-depends.lisp")

(load "/net/users/pjb/src/public/lisp/clmisc/asdf-system-tarball-location.lisp")

(dolist (file (directory "*.lisp"))
  (COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS::scan-source file :load t))




(defparameter *cl-sources* '( package  source utility ecma048  list dll
	queue array string stream file peek-stream scanner parser  avl
	bset brelation dictionary graf graph  graph-dot graph-diagram
	combination pmatch picture  memory heap activity message-queue
	float-binio data-encoding cons-to-ascii tree-to-ascii
	tree-to-diagram regexp-posix regexp-emacs rfc2822 rfc3548 iso639a
	iso3166 iso4217 iana-character-sets html-iso8879-1 html htrans
	database  parse-html cache  aliases passwd
	group primes tea raiden make-depends cxx csv  iban  rib  invoice
	browser  ed  interactive ))


(defparameter *clisp-sources* (append '( syslog  #+ffi uffi  iotask
  rfc1413 ) (when (find-package "LINUX") '( raw-memory susv3 susv3-mc3
  susv3-xsi script shell xterm make-volumes ))))

(mapcar
 (lambda (src)
   (let ((file (make-pathname :name (string src) :type "LISP" :case :common)))
     (com.informatimago.common-lisp.make-depends::get-source-file file)))
        *cl-sources*)

(mapcar
 (lambda (src)
   (let ((file (make-pathname :name (string src) :type "LISP" :case :common
                              :directory '(:relative :up "CLISP"))))
     (com.informatimago.common-lisp.make-depends::get-source-file file)))
        *clisp-sources*)



