;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               character-sets.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Portability layer over character sets and external-formats.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-06 <PJB> Extracted from
;;;;                     com.informatimago.common-lisp.cesarum.character-sets.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.CLEXT.CHARACTER-SETS"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS")
  (:export
   "MAKE-EXTERNAL-FORMAT"
   "EXTERNAL-FORMAT-CHARACTER-ENCODING"
   "EXTERNAL-FORMAT-LINE-TERMINATION"
   "CHARACTER-SET-TO-LISP-ENCODING"
   "CHARACTER-SET-FOR-LISP-ENCODING"
   "CHARACTER-SET-TO-EMACS-ENCODING"
   "CHARACTER-SET-FROM-EMACS-ENCODING"
   "EMACS-ENCODING-TO-LISP-EXTERNAL-FORMAT")
  (:documentation "
This package exports functions to manage character-sets,
character encodings, coding systems and external format.
It's all the same, but everyone likes to have his own terms...

Copyright Pascal J. Bourguignon 2005 - 2012
This package is provided under the GNU General Public Licence.
See the source file for details.
"))
(in-package "COM.INFORMATIMAGO.CLEXT.CHARACTER-SETS")


(defparameter *aliases*
  '(
    ;; clisp, emacs:
    ("UNICODE-32-LITTLE-ENDIAN" "UTF-32-LE" "UTF-32LE")
    ("UNICODE-32-BIG-ENDIAN"    "UTF-32-BE" "UTF-32BE")
    ("UNICODE-16-LITTLE-ENDIAN" "UTF-16-LE" "UTF-16LE")
    ("UNICODE-16-BIG-ENDIAN"    "UTF-16-BE" "UTF-16BE")
    ;; clisp
    ("CP437-IBM" "CP437")
    ("CP852-IBM" "CP852")
    ("CP860-IBM" "CP860")
    ("CP861-IBM" "CP861")
    ("CP862-IBM" "CP862")
    ("CP863-IBM" "CP863")
    ("CP864-IBM" "CP864")
    ("CP865-IBM" "CP865")
    ("CP869-IBM" "CP869")
    ("CP874-IBM" "CP874")
    ;; emacs:
    ("VSCII" "VISCII")
    ;; Aliases for other implementations:
    ("LATIN1"  "ISO-8859-1")
    ("LATIN2"  "ISO-8859-2")
    ("LATIN3"  "ISO-8859-3")
    ("LATIN4"  "ISO-8859-4")
    ("LATIN5"  "ISO-8859-9")
    ("LATIN6"  "ISO-8859-10")
    ("LATIN8"  "ISO-8859-14")
    ("LATIN9"  "ISO-8859-15")

    ("LATIN-1"  "ISO-8859-1")
    ("LATIN-2"  "ISO-8859-2")
    ("LATIN-3"  "ISO-8859-3")
    ("LATIN-4"  "ISO-8859-4")
    ("LATIN-5"  "ISO-8859-9")
    ("LATIN-6"  "ISO-8859-10")
    ("LATIN-8"  "ISO-8859-14")
    ("LATIN-9"  "ISO-8859-15")
    )

  "A list of lists of aliases for character-set.")



(defun add-aliases-to-group (encoding-name-and-aliases aliases)
  "
ENCODING-NAME-AND-ALIASES:
            A list of name and aliases of character-sets.

ALIASES:    A list of lists of aliases, each sublist naming the same character-set.

RETURN:     A new list of name and aliases, with the ALIASES added, if
            they name the same character-set as ENCODING-NAME-AND-ALIASES.
"
  (let ((alias (find-if
                (lambda (alias)
                  (intersection encoding-name-and-aliases alias :test (function string-equal)))
                aliases)))
    (if alias
        (remove-duplicates (cons (car encoding-name-and-aliases)
                                 (union (cdr encoding-name-and-aliases) alias
                                        :test (function string-equal)))
                           :test (function string-equal))
        encoding-name-and-aliases)))




(defparameter *lisp-encodings*

  ;; #+ccl-1.9
  ;; ccl:list-character-encodings.  Use it
  ;; in ccl:describe-character-encodings

  #+(and ccl (not ccl-1.6))
  (mapcar (lambda (x) (mapcar (function string-upcase) x))
          '((:iso-8859-1 :iso_8859-1 :latin1 :l1 :ibm819 :cp819 :csisolatin1)
            (:iso-8859-2 :iso_8859-2 :latin-2 :l2 :csisolatin2)
            (:iso-8859-3 :iso_8859-3 :latin3 :l3 :csisolatin3)
            (:iso-8859-4 :iso_8859-4 :latin4 :l4 :csisolatin4)
            (:iso-8859-5 :iso_8859-5 :cyrillic :csisolatincyrillic :iso-ir-144)
            (:iso-8859-6 :iso_8859-6 :arabic :csisolatinarabic :iso-ir-127)
            (:iso-8859-7 :iso_8859-7 :greek :greek8 :csisolatingreek :iso-ir-126 :elot_928 :ecma-118)
            (:iso-8859-8 :iso_8859-8 :hebrew :csisolatinhebrew :iso-ir-138)
            (:iso-8859-9 :iso_8859-9 :latin5 :csisolatin5 :iso-ir-148)
            (:iso-8859-10 :iso_8859-10 :latin6 :csisolatin6 :iso-ir-157)
            (:iso-8859-11)
            (:iso-8859-13)
            (:iso-8859-14 :iso_8859-14 :iso-ir-199 :latin8 :l8 :iso-celtic)
            (:iso-8859-15 :iso_8859-15 :latin9)
            (:iso-8859-16 :iso_8859-16 :iso-ir-199 :latin8 :l8 :iso-celtic)
            (:macintosh :macos-roman :macosroman :mac-roman :macroman)
            (:ucs-2)
            (:ucs-2be)
            (:ucs-2le)
            (:us-ascii :csascii :cp637 :ibm637 :us :iso646-us :ascii :iso-ir-6)
            (:utf-16)
            (:utf-16be)
            (:utf-16le)
            (:utf-32 :utf-4)
            (:utf-32be :ucs-4be)
            (:utf-8)
            (:utf-32le :ucs-4le)
            (:windows-31j  :cp932 :cswindows31j)
            (:euc-jp :eucjp)))

  #+(and ccl ccl-1.6)
  (mapcar (lambda (x) (mapcar (function string-upcase) x))
          '((:iso-8859-1 :iso_8859-1  :latin1  :l1  :ibm819  :cp819  :csisolatin1)
            (:iso-8859-2 :iso_8859-2  :latin-2  :l2  :csisolatin2)
            (:iso-8859-3 :iso_8859-3  :latin3 :l3  :csisolatin3)
            (:iso-8859-4 :iso_8859-4  :latin4  :l4  :csisolatin4)
            (:iso-8859-5 :iso_8859-5  :cyrillic  :csisolatincyrillic  :iso-ir-144)
            (:iso-8859-6 :iso_8859-6  :arabic  :csisolatinarabic  :iso-ir-127)
            (:iso-8859-7 :iso_8859-7  :greek  :greek8  :csisolatingreek  :iso-ir-126  :elot_928  :ecma-118)
            (:iso-8859-8 :iso_8859-8  :hebrew  :csisolatinhebrew  :iso-ir-138)
            (:iso-8859-9 :iso_8859-9  :latin5  :csisolatin5  :iso-ir-148)
            (:iso-8859-10 :iso_8859-10  :latin6  :csisolatin6  :iso-ir-157)
            (:iso-8859-11)
            (:iso-8859-13)
            (:iso-8859-14 :iso_8859-14  :iso-ir-199  :latin8  :l8  :iso-celtic)
            (:iso-8859-15 :iso_8859-15  :latin9)
            (:iso-8859-16 :iso_8859-16  :iso-ir-199  :latin8  :l8  :iso-celtic)
            (:macintosh :macos-roman  :macosroman  :mac-roman  :macroman)
            (:ucs-2)
            (:ucs-2be)
            (:ucs-2le)
            (:us-ascii :csascii  :cp637 :ibm637  :us  :iso646-us  :ascii  :iso-ir-6)
            (:utf-16)
            (:utf-16be)
            (:utf-16le)
            (:utf-32 :utf-4)
            (:utf-32be :ucs-4be)
            (:utf-8)
            (:utf-32le :ucs-4le)
            (:windows-31j :cp932  :cswindows31j)
            (:euc-jp :eucjp)
            (:gb2312 :gb2312-80 :gb2312-1980 :euc-cn :euccn)
            (:cp936 :gbk :ms936 :windows-936)))

  #+clisp
  (let ((h (make-hash-table)))
    (do-external-symbols (s "CHARSET")
      (push (string-upcase s) (gethash (ext:encoding-charset s) h)))
    (let ((r '()))
      (maphash (lambda (k v) (declare (ignore k)) (push  v r)) h)
      r))

  #+cmu   '(("ISO-8859-1"))          ; :iso-latin-1-unix ;  what else?

  #+ecl   '(("ISO-8859-1")
            #+unicode ("UTF-8"))

  #+sbcl
  (etypecase sb-impl::*external-formats*
    (hash-table (let ((result '()))
                  (maphash (lambda (name encoding) (declare (ignore name)) (pushnew encoding result))
                           sb-impl::*external-formats*)
                  (mapcar (lambda (encoding)
                            (mapcar (function string-upcase)
                                    (slot-value encoding 'sb-impl::names)))
                          result)))
    (list (mapcar (lambda (x) (mapcar (function string-upcase) (first x)))
                  sb-impl::*external-formats*)))


  ;; From Java7:
  ;; Every implementation of the Java platform is required to support
  ;; the following standard charsets. Consult the release
  ;; documentation for your implementation to see if any other
  ;; charsets are supported. The behavior of such optional charsets
  ;; may differ between implementations.
  ;;
  ;; Other external formats are also possible with abcl.
  #+abcl
  (remove-duplicates
   (append '(("US-ASCII") ("ISO-8859-1") ("UTF-8") ("UTF-16BE") ("UTF-16LE") ("UTF-16"))
           (mapcar (lambda (encoding)
                     (let ((n (symbol-name encoding))
                           (u (string-upcase encoding)))
                       (if (string= n u)
                           (list n)
                           (list n u))))
                   (system:available-encodings)))
   :test (function equal))


  #-(or abcl ccl clisp cmu ecl sbcl)
  (progn
    (warn "What are the available external formats in ~A ?"
          (lisp-implementation-type))
    '(("US-ASCII")))

  "Give an a-list of name and list of aliases of encoding systems in
the current Common Lisp implementation.  Those names and aliases are strings.")



(defun fill-character-set-lisp-encoding ()
  "
DO:         Set the cs-lisp-encoding of the character-sets present in
            the current implementation.
"
  (dolist (lsl *lisp-encodings* (values))
    (let* ((aliases (add-aliases-to-group lsl *aliases*))
           (cs (some (function find-character-set) aliases)))
      (when cs
        ;; We don't add the aliases to the lisp-encoding, since this
        ;; list is used to make the implementation specific encodings
        ;; and external-formats.
        (setf (cs-lisp-encoding cs) lsl)))))






(defgeneric make-external-format (character-encoding &optional line-termination)
  (:documentation "Makes an implementation specific external-format.")

  (:method ((cs character-set) &optional line-termination)
    (if (cs-lisp-encoding cs)
        (let ((encoding         (first (cs-lisp-encoding cs)))
              (line-termination (or line-termination
                                    #+ccl ccl:*default-line-termination*
                                    #-ccl :unix)))
          (check-type line-termination (member :unix :mac :dos))

          #+ccl
          (ccl:make-external-format :domain nil
                                    :character-encoding (intern encoding "KEYWORD")
                                    :line-termination (case line-termination
                                                        ((:unix :dos) line-termination)
                                                        ((:mac) :macos)))

          #+clisp
          (ext:make-encoding :charset (symbol-value (intern encoding "CHARSET"))
                             :line-terminator line-termination
                             :input-error-action :error
                             :output-error-action :error)

          #+cmu
          (if (string-equal encoding "ISO-8859-1")
              :iso-latin-1-unix
              (progn #|should not occur|#
                (cerror 'character-set-error
                        :character-set cs
                        :format-control "The character-set ~S has no lisp-encoding in ~A"
                        :format-arguments (list (cs-name cs) (lisp-implementation-type)))
                :default))

          #+ecl
          (cond
            ((string-equal encoding "ISO-8859-1")
             :iso-8859-1)
            #+unicode
            ((string-equal encoding "UTF-8")
             :utf-8)
            (t  #|should not occur|#
             (cerror 'character-set-error
                     :character-set cs
                     :format-control "The character-set ~S has no lisp-encoding in ~A"
                     :format-arguments (list (cs-name cs) (lisp-implementation-type)))
             :default))

          #+sbcl
          (intern encoding "KEYWORD")

          #+abcl
          (intern encoding "KEYWORD")

          #-(or abcl ccl clisp cmu ecl sbcl)
          (values
           (find (lambda (cs) (member encoding (cs-lisp-encoding cs)
                                      :test (function string-equal)))
                 *character-sets*)
           :unix))
        (error 'character-set-error
               :character-set cs
               :format-control "The character-set ~S has no lisp-encoding in ~A"
               :format-arguments (list (cs-name cs) (lisp-implementation-type)))))

  (:method ((character-set-name string) &optional line-termination)
    (let ((cs (find-character-set character-set-name)))
      (if cs
          (make-external-format cs line-termination)
          (error 'character-set-error
                 :character-set (string character-set-name)
                 :format-control "There is no character-set named ~S"
                 :format-arguments (list (string character-set-name))))))

  (:method ((character-set symbol) &optional line-termination)
    (make-external-format (string character-set) line-termination)))


(defun external-format-character-encoding (external-format)
  #+ccl (ccl:external-format-character-encoding external-format)
  #+(and clisp unicode) (string (ext:encoding-charset external-format))
  #+cmu (string external-format)
  #+ecl (string external-format)
  #+sbcl (string external-format)
  #+abcl (string external-format)
  #-(or abcl ccl (and clisp unicode) cmu ecl sbcl)
  (error "~S: How to decode an external-format in ~A"
         'external-format-character-encoding
         (lisp-implementation-type)))


(defun external-format-line-termination (external-format)
  #+(or cmu ecl sbcl abcl) (declare (ignore external-format))
  #+ccl (ccl:external-format-line-termination external-format)
  #+(and clisp unicode) (string (ext:encoding-line-terminator external-format))
  #+cmu :unix
  #+ecl :unix
  #+sbcl :unix
  #+abcl :unix ; TODO: ???
  #-(or abcl ccl (and clisp unicode) cmu ecl sbcl)
  (error "~S: How to decode an external-format in ~A"
         'external-format-line-termination
         (lisp-implementation-type)))



(defun character-set-to-lisp-encoding (cs &key (line-termination :unix))
  "
RETURN: An implementation specific object representing the  encoding for
        the given character-set and line-termination.
SIGNAL: An error if line-termination is not (member :unix :mac :dos nil) or
        if cs has no emacs encoding.
"
  (assert (member line-termination '(:unix :mac :dos nil)))
  (make-external-format cs line-termination))


(defun character-set-for-lisp-encoding (encoding)
  "
ENCODING:  An implementation specific object representing an encoding.
           possibly with line-termination.
RETURN:    The character-set that correspond to this emacs-encoding ;
           the line-termination.
"
  (values (external-format-character-encoding encoding)
          (external-format-line-termination   encoding)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs coding systems
;;;

(defparameter *emacs-encodings*
  #||
  ;; emacs lisp code to generate the following list.
  (require 'cl)
  (sort*
   (mapcar
    (lambda (sl) (mapcar (lambda (sym) (upcase (symbol-name sym))) sl))
    (delete-duplicates
     (mapcar (lambda (coding-system)
               (or (coding-system-get coding-system 'alias-coding-systems)
                   (list coding-system)))
             (let ((coding-system-list '()))
               (mapatoms (lambda (sym) (when (and sym (coding-system-p sym))
                                    (push sym coding-system-list))))
               coding-system-list))
     :test (function equal)))
   (function string<) :key (function first))
  ||#
  (quote
   (("CHINESE-BIG5" "BIG5" "CN-BIG5" "CP950")
    ("CHINESE-HZ" "HZ-GB-2312" "HZ")
    ("CHINESE-ISO-8BIT" "CN-GB-2312" "EUC-CHINA" "EUC-CN" "CN-GB" "GB2312" "CP936")
    ("CHINESE-ISO-8BIT-WITH-ESC")
    ("COMPOUND-TEXT" "X-CTEXT" "CTEXT")
    ("COMPOUND-TEXT-WITH-EXTENSIONS" "X-CTEXT-WITH-EXTENSIONS" "CTEXT-WITH-EXTENSIONS")
    ("CP1125" "RUSCII" "CP866U")
    ("CP437")
    ("CP720")
    ("CP737")
    ("CP775")
    ("CP850")
    ("CP851")
    ("CP852")
    ("CP855")
    ("CP857")
    ("CP860")
    ("CP861")
    ("CP862")
    ("CP863")
    ("CP864")
    ("CP865")
    ("CP866")
    ("CP869")
    ("CP874")
    ("CTEXT-NO-COMPOSITIONS")
    ("CYRILLIC-ALTERNATIVNYJ" "ALTERNATIVNYJ")
    ("CYRILLIC-ISO-8BIT" "ISO-8859-5")
    ("CYRILLIC-ISO-8BIT-WITH-ESC")
    ("CYRILLIC-KOI8" "KOI8-R" "KOI8" "CP878")
    ("EMACS-MULE")
    ("EUC-TW" "EUC-TAIWAN")
    ("GEORGIAN-PS")
    ("GREEK-ISO-8BIT" "ISO-8859-7")
    ("GREEK-ISO-8BIT-WITH-ESC")
    ("HEBREW-ISO-8BIT" "ISO-8859-8" "ISO-8859-8-E" "ISO-8859-8-I")
    ("HEBREW-ISO-8BIT-WITH-ESC")
    ("IN-IS13194" "DEVANAGARI")
    ("IN-IS13194-WITH-ESC")
    ("ISO-2022-7BIT")
    ("ISO-2022-7BIT-LOCK" "ISO-2022-INT-1")
    ("ISO-2022-7BIT-LOCK-SS2" "ISO-2022-CJK")
    ("ISO-2022-7BIT-SS2")
    ("ISO-2022-8BIT-SS2")
    ("ISO-2022-CN" "CHINESE-ISO-7BIT")
    ("ISO-2022-CN-EXT")
    ("ISO-2022-JP" "JUNET")
    ("ISO-2022-JP-2")
    ("ISO-2022-KR" "KOREAN-ISO-7BIT-LOCK")
    ("ISO-8859-11")
    ("ISO-8859-6" "ARABIC-ISO-8BIT")
    ("ISO-LATIN-1" "ISO-8859-1" "LATIN-1")
    ("ISO-LATIN-1-WITH-ESC")
    ("ISO-LATIN-10" "ISO-8859-16" "LATIN-10")
    ("ISO-LATIN-2" "ISO-8859-2" "LATIN-2")
    ("ISO-LATIN-2-WITH-ESC")
    ("ISO-LATIN-3" "ISO-8859-3" "LATIN-3")
    ("ISO-LATIN-3-WITH-ESC")
    ("ISO-LATIN-4" "ISO-8859-4" "LATIN-4")
    ("ISO-LATIN-4-WITH-ESC")
    ("ISO-LATIN-5" "ISO-8859-9" "LATIN-5")
    ("ISO-LATIN-5-WITH-ESC")
    ("ISO-LATIN-6" "ISO-8859-10" "LATIN-6")
    ("ISO-LATIN-7" "ISO-8859-13" "LATIN-7")
    ("ISO-LATIN-8" "ISO-8859-14" "LATIN-8")
    ("ISO-LATIN-8-WITH-ESC")
    ("ISO-LATIN-9" "ISO-8859-15" "LATIN-9" "LATIN-0")
    ("ISO-LATIN-9-WITH-ESC")
    ("ISO-SAFE" "US-ASCII")
    ("JAPANESE-ISO-7BIT-1978-IRV" "ISO-2022-JP-1978-IRV" "OLD-JIS")
    ("JAPANESE-ISO-8BIT" "EUC-JAPAN-1990" "EUC-JAPAN" "EUC-JP")
    ("JAPANESE-ISO-8BIT-WITH-ESC")
    ("JAPANESE-SHIFT-JIS" "SHIFT_JIS" "SJIS" "CP932")
    ("KOI8-T" "CYRILLIC-KOI8-T")
    ("KOI8-U")
    ("KOREAN-ISO-8BIT" "EUC-KR" "EUC-KOREA" "CP949")
    ("KOREAN-ISO-8BIT-WITH-ESC")
    ("LAO")
    ("LAO-WITH-ESC")
    ("MAC-ROMAN")
    ("MIK")
    ("MULE-UTF-16" "UTF-16")
    ("MULE-UTF-16BE" "UTF-16BE")
    ("MULE-UTF-16BE-WITH-SIGNATURE" "UTF-16BE-WITH-SIGNATURE"
                                    "MULE-UTF-16-BE" "UTF-16-BE")
    ("MULE-UTF-16LE" "UTF-16LE")
    ("MULE-UTF-16LE-WITH-SIGNATURE" "UTF-16LE-WITH-SIGNATURE"
                                    "MULE-UTF-16-LE" "UTF-16-LE")
    ("MULE-UTF-8" "UTF-8")
    ("NEXT")
    ("NO-CONVERSION")
    ("PT154")
    ("RAW-TEXT")
    ("THAI-TIS620" "TH-TIS620" "TIS620" "TIS-620")
    ("THAI-TIS620-WITH-ESC")
    ("TIBETAN-ISO-8BIT" "TIBETAN")
    ("TIBETAN-ISO-8BIT-WITH-ESC")
    ("UNDECIDED")
    ("UTF-7")
    ("VIETNAMESE-TCVN" "TCVN" "TCVN-5712")
    ("VIETNAMESE-VIQR" "VIQR")
    ("VIETNAMESE-VISCII" "VISCII")
    ("VIETNAMESE-VSCII" "VSCII")
    ("W3M-EUC-JAPAN")
    ("W3M-ISO-LATIN-1")
    ("WINDOWS-1250" "CP1250")
    ("WINDOWS-1251" "CP1251" "CP1251")
    ("WINDOWS-1252" "CP1252" "CP1252")
    ("WINDOWS-1253" "CP1253")
    ("WINDOWS-1254" "CP1254")
    ("WINDOWS-1255" "CP1255")
    ("WINDOWS-1256" "CP1256")
    ("WINDOWS-1257" "CP1257")
    ("WINDOWS-1258" "CP1258")))
  "List of emacs encoding, grouped by aliases")



(defun fill-character-set-emacs-encoding ()
  "
DO:         Set the cs-emacs-encoding of the character-sets present in
            the current implementation.
"
  (dolist (ecsl *emacs-encodings* (values))
    (let ((cs (some (function find-character-set)
                    (add-aliases-to-group ecsl *aliases*))))
      (when cs
        (setf (cs-emacs-encoding cs) ecsl)))))




(defun character-set-to-emacs-encoding (cs &key (line-termination :unix))
  "
RETURN: A string naming the emacs encoding for the given character-set
        and line-termination.
SIGNAL: An error if line-termination is not (member :unix :mac :dos nil) or
        if cs has no emacs encoding.
"
  (assert (member line-termination '(:unix :mac :dos nil)))
  (unless  (cs-emacs-encoding cs)
    (error "The character-set ~A has no corresponding emacs encoding"
           (cs-name cs)))
  (format nil "~(~A~:[~;~:*-~A~]~)" (first (cs-emacs-encoding cs))
          line-termination))


(defun character-set-from-emacs-encoding (ecs)
  "
ECS:    A string or symbol naming the emacs encoding,
        possibly suffixed by a line-termination.
RETURN: The character-set that correspond to this emacs-encoding ;
        the line-termination.
"
  (let ((line-termination nil)
        (ecs (string ecs)))
    (cond
      ((suffixp "-unix" ecs :test (function char-equal))
       (setf ecs  (subseq ecs 0 (- (length ecs) 5))
             line-termination :unix))
      ((suffixp "-dos" ecs :test (function char-equal))
       (setf ecs (subseq ecs 0 (- (length ecs) 4))
             line-termination :dos))
      ((suffixp "-mac" ecs :test (function char-equal))
       (setf ecs (subseq ecs 0 (- (length ecs) 4))
             line-termination :mac)))
    (values
     (find (lambda (cs) (member ecs (cs-emacs-encoding cs)
                           :test (function string-equal)))
           *character-sets*)
     line-termination)))


(defun emacs-encoding-to-lisp-external-format (emacs-encoding)
  "
RETURN:  the external-format value corresponding to this EMACS-ENCODING.
"
  (multiple-value-bind (charset line-termination)
      (character-set-from-emacs-encoding emacs-encoding)
    (when charset
      (character-set-to-lisp-encoding charset :line-termination line-termination))))



(eval-when (:load-toplevel :execute)
  (fill-character-set-emacs-encoding)
  (fill-character-set-lisp-encoding))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The rest was used to generate the data in
;;; COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER-SETS
;;;

#+(and (and) clisp)
(defun compute-character-set-ranges ()
  "
DO:     Read the character-set file and build the *character-sets* list,
        then update the character sets with emacs encodings, lisp encodings,
        and character set ranges (found in clisp).
RETURN: *character-sets*
"
  (setf *character-sets* (read-character-sets-file "character-sets"))
  (fill-character-set-emacs-encoding)
  (fill-character-set-lisp-encoding)
  (dolist (cs *character-sets*)
    (when (cs-lisp-encoding cs)
      (let ((charset (find-symbol (first (cs-lisp-encoding cs)) "CHARSET")))
        (setf (cs-ranges cs)
              #+#.(cl:if #+mocl (cl:and (cl:find-package "SYSTEM")
                                        (cl:find-symbol "GET-CHARSET-RANGE" "SYSTEM"))
                         #-mocl (cl:ignore-errors
                                 (cl:find-symbol "GET-CHARSET-RANGE" "SYSTEM"))
                         '(:and) '(:or))
              (map 'vector (function char-code)
                   (system::get-charset-range charset))
              #-#.(cl:if #+mocl (cl:and (cl:find-package "SYSTEM")
                                        (cl:find-symbol "GET-CHARSET-RANGE" "SYSTEM"))
                         #-mocl (cl:ignore-errors
                                 (cl:find-symbol "GET-CHARSET-RANGE" "SYSTEM"))
                         '(:and) '(:or))
              (coerce
               (loop
                  :with charset = (symbol-value charset)
                  :with i = 0
                  :for start = (loop
                                  :until (or (< char-code-limit i)
                                             (typep (code-char i) charset))
                                  :do (incf i)
                                  :finally (return (when (<= i char-code-limit)
                                                     i)))
                  :while start
                  :nconc (list start
                               (loop
                                  :while (and (<= i char-code-limit)
                                              (typep (code-char i) charset))
                                  :do (incf i)
                                  :finally (return (1- i)))))
               'vector)))))
  *character-sets*)


;;; Provide a default value for  *CHARACTER-SETS*
#-(and)
(let ((*print-right-margin* 72))
  (pprint
   `(setf *character-sets*
      (list
       ,@(mapcar
          (lambda (cs)
            `(make-character-set
              :mib-enum ,(cs-mib-enum cs)
              :name ,(cs-name cs)
              :aliases ',(cs-aliases cs)
              :mime-encoding  ',(cs-mime-encoding cs)
              :source ',(cs-source cs)
              :comments ',(cs-comments cs)
              :references ',(cs-references cs)
              :ranges ,(cs-ranges cs)))
          (compute-character-set-ranges))))))


;;;; THE END ;;;;
