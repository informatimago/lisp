;;;; -*- coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               character-sets.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports functions to manage character-sets,
;;;;    character encodings, coding systems and external format.
;;;;    It's all the same, but each everybody likes to have his own terms...
;;;;    
;;;;    The base character set repertoire will be the IANA one, published at:
;;;;    http://www.iana.org/assignments/character-sets
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-05-18 <PJB> Merged with stuff from make-depend, and augmented.
;;;;    2005-08-31 <PJB> Created (iana-character-sets.lisp).
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2007
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


(IN-PACKAGE "COMMON-LISP-USER")
;; (DECLAIM (DECLARATION ALSO-USE-PACKAGES)
;;          (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.HTML"))
;; (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;   (COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:ADD-NICKNAME
;;    "COM.INFORMATIMAGO.COMMON-LISP.HTML" "HTML"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CHARACTER-SETS"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING")
  (:EXPORT
   "CHARACTER-SET"
   "CS-MIB-ENUM" "CS-NAME" "CS-ALIASES"
   "CS-SOURCE" "CS-COMMENTS" "CS-REFERENCES" "CS-RANGES"
   "CS-LISP-ENCODING" "CS-EMACS-ENCODING" "CS-MIME-ENCODING" 
   "READ-CHARACTER-SETS-FILE"
   "*CHARACTER-SETS*" "FIND-CHARACTER-SET"
   "CHARACTER-SET-TO-EMACS-ENCODING"
   "CHARACTER-SET-FROM-EMACS-ENCODING"
   "CHARACTER-SET-TO-LISP-ENCODING"
   "CHARACTER-SET-FROM-LISP-ENCODING"
   "CHARACTER-SET-TO-MIME-ENCODING"
   "EMACS-ENCODING-TO-LISP-EXTERNAL-FORMAT"
   "CHARACTER-IN-CHARACTER-SET-P")
  (:DOCUMENTATION
   "This package exports functions to manage character-sets,
    character encodings, coding systems and external format.
    It's all the same, but each everybody likes to have his own terms...
    
    Copyright Pascal J. Bourguignon 2005 - 2007
    This package is provided under the GNU General Public Licence.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CHARACTER-SETS")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Processing IANA charactet-sets file.
;;;;

 (defstruct (character-set (:conc-name cs-))
   mib-enum name aliases
   ;; Comments and other human readable information:
   source comments references
   ;; We associate each IANA character set with the cl-encoding and
   ;; the emacs encoding.
   ;;
   ;; We keep only the current Common Lisp implementation encoding, to
   ;; promote standardization of implementation encoding names around
   ;; this IANA character set registry. (We don't want to manage cross
   ;; CL implementations encodings, let's go thru the IANA registry).
   lisp-encoding
   ;; However, we keep also here the emacs encoding for its important
   ;; application in reading source files edited with emacs.
   emacs-encoding
   ;; The preferred name for MIME is stored here, otherwise name is used.
   mime-encoding
   ;; Character ranges: a vector of pairs of unicode character codes:
   ;;    [ (aref range (* 2 i))   (aref range (1+ (* 2 i))) ]
   ;; indicating which characters belong to the character set,
   ;; or NIL when unknown.
   ranges)



(defun read-character-sets-file (file)
  "
DO:     Parse the http://www.iana.org/assignments/character-sets file,
        and extracts the character-sets defined there.
RETURN: A list of character-set structures read from the file.
"
  (let ((lines
         (with-open-file (in file)
           (loop
              :for line = (read-line in nil nil)
              :while (and line (not  (prefixp "Character Set " line))))
           (loop ;; join continuation lines.
              :with lines = '()
              :for line = (read-line in nil nil)
              :while (and line (not (prefixp "REFERENCES" line)))
              :do (if (and (<= 1 (length line))
                           (char= #\space (aref line 0))
                           lines)
                      (setf (car lines) (concatenate 'string (car lines) " "
                                                     (string-trim " " line)))
                      (push line lines))
              :finally (return (nreverse lines))))))
    (flet ((trim (string start end)
             (declare (inline trim))
             (string-trim " " (subseq string start end))))
      (loop
         :with sets = '()
         :with cs = nil
         :for line :in lines
         :do (let ((spc (position #\space line))
                   (colon (position #\: line)))
               (if (and colon (or (not spc) (< colon spc)))
                   (let ((name (subseq line 0 colon))
                         (value (string-trim " " (subseq line (1+ colon)))))
                     (cond
                       ((string-equal name "Name")
                        (when cs
                          (push cs sets))
                        (let ((left-bra (position #\[ value))
                              (spc (position #\space value))
                              name comment reference)
                          (cond
                            ((and left-bra (or (null spc) (< left-bra spc)))
                             (setf name      (trim value 0 left-bra)
                                   reference (trim value left-bra nil)))
                            ((and left-bra spc (< spc left-bra))
                             (setf name (trim  value 0 spc)
                                   comment   (trim value spc left-bra)
                                   reference (trim value left-bra nil)))
                            (spc
                             (setf name    (trim value 0 spc)
                                   comment (trim value spc nil)))
                            (t
                             (setf name (string-trim " " value))))
                          (setf cs (make-character-set
                                    :name name
                                    :references (when reference
                                                  (list reference))))
                          (when comment
                            (cond
                              ((null comment))
                              ((search "preferred MIME " comment)
                               (setf (cs-mime-encoding cs)  name))
                              ((string/= "" comment)
                               (push comment (cs-comments cs)))))))
                       ((or (string-equal name "Alias")
                            (string-equal name "Aliases"))
                        (let ((spc (position #\space value))
                              alias comment)
                          (if spc
                              (setf alias (string-trim " " (subseq value 0 spc))
                                    comment line)
                              (setf alias value
                                    comment nil))
                          (when (search "preferred MIME " comment)
                            (setf (cs-mime-encoding cs)  alias))
                          (unless (string-equal "None" alias)
                            (push alias (cs-aliases cs))
                            (when (and comment (string/= "" comment))
                              (push comment (cs-comments cs))))))
                       ((string-equal name "Source")
                        (setf (cs-source cs) value))
                       ((string-equal name "MIBenum")
                        (setf (cs-mib-enum cs) (parse-integer value)))
                       ;; bug:
                       ((string-equal name "http")
                        (push line (cs-comments cs)))))
                   (when cs
                     (let ((comment  (string-trim " " line)))
                       (when  (string/= "" comment)
                         (push comment (cs-comments cs)))))))
         :finally (when cs (push cs sets)) (return sets)))))


(defvar *CHARACTER-SETS* nil "The list of IANA Character Sets.")


(defun find-character-set (name)
  "
RETURN: The character-set in *CHARACTER-SETS* that has NAME as name or alias,
        or some variation of NAME (removing non alphanumeric characters
        and prefixing 'cs'. 
"
  (flet ((memb (item list)
           (declare (inline memb))
           (member item list :test (function string-equal))))
    (let* ((name (string name))
           (result
            (find-if
            (lambda (cs)
              (or  (string-equal name (cs-name cs))
                   (memb name (cs-aliases cs))
                   (memb name (cs-lisp-encoding cs))
                   (memb name (cs-emacs-encoding cs))
                   (memb (concatenate 'string "cs"
                                      (remove-if-not (function alphanumericp)
                                                     name))
                           (cs-aliases cs))))
            *character-sets*)))
      ;; We move the found character set in front of the list to speed next
      ;; searches, assuming the application will often search several times
      ;; the same few character sets.
      (when result
        (setf *character-sets* (cons result (delete result *character-sets*))))
      result)))



(defun character-in-character-set-p (character character-set)
  (let ((code   (char-code character))
        (ranges (cs-ranges character-set)))
    (if ranges
        (loop
           :for i :from 0 :below (length ranges) :by 2
           :do (when (<= (aref ranges i) code (aref ranges (1+ i)))
                 (return (values code :known)))
           :finally (return (values nil :known)))
        (values t :unknown))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EMACS-ENCODINGS
;;;

(defparameter *emacs-encodings*
  #+emacs
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
  #-emacs
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



(defun add-aliases-to-group (group aliases)
  (let ((alias (find-if
                (lambda (alias)
                  (intersection group alias :test (function string-equal)))
                aliases)))
    (if alias
        (remove-duplicates (union group alias :test (function string-equal))
                           :test (function string-equal))
        group)))


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
    ;; TODO: Add aliases for other implementations if needed.
    ))


(defun fill-character-set-emacs-encoding ()
  (dolist (ecsl  *emacs-encodings*)
    (let ((cs (some (function find-character-set)
                    (add-aliases-to-group ecsl *aliases*))))
      (when cs
        (setf (cs-emacs-encoding cs) ecsl)))))


(defun character-set-to-emacs-encoding (cs &key (line-terminator :unix))
  "
RETURN: A string naming the emacs encoding for the given character-set
        and line-terminator.
SIGNAL: An error if line-terminator is not (member :unix :mac :dos nil) or
        if cs has no emacs encoding.
"
  (assert (member line-terminator '(:unix :mac :dos nil)))
  (unless  (cs-emacs-encoding cs)
    (error "The character-set ~A has no corresponding emacs encoding"
           (cs-name cs)))
  (format nil "~(~A~:[~;~:*-~A~]~)" (first (cs-emacs-encoding cs))
          line-terminator))


(defun character-set-from-emacs-encoding (ecs)
  "
ECS:  A string or symbol naming the emacs encoding,
        possibly suffixed by a line-terminator.
RETURN: The character-set that correspond to this emacs-encoding ;
       the line-terminator.
"
  (let ((line-terminator nil)
        (ecs (string ecs)))
    (cond
      ((suffixp "-unix" ecs :test (function char-equal))
       (setf ecs  (subseq ecs 0 (- (length ecs) 5))
             line-terminator :unix))
      ((suffixp "-dos" ecs :test (function char-equal))
       (setf ecs (subseq ecs 0 (- (length ecs) 4))
             line-terminator :dos))
      ((suffixp "-mac" ecs :test (function char-equal))
       (setf ecs (subseq ecs 0 (- (length ecs) 4))
             line-terminator :mac)))
    (values
     (find (lambda (cs) (member ecs (cs-emacs-encoding cs)
                           :test (function string-equal)))
           *character-sets*)
     line-terminator)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp Encodings
;;;

#+clisp
(defun group-charset-aliases ()
  "
RETURN: A list of sublists grouping character set aliases, ie. charset encodings
     that have the same charset."
  (let ((h (make-hash-table)))
    (do-external-symbols (s "CHARSET")
      (push (string-upcase s) (gethash (EXT:ENCODING-CHARSET s) h)))
    (let ((r '()))
      (maphash (lambda (k v) (declare (ignore k)) (push  v r)) h)
      r)))


(defparameter *lisp-encodings*
  #+clisp (group-charset-aliases)
  #+sbcl   (mapcar (lambda (x) (mapcar 'string-upcase (first x)))
                   SB-IMPL::*EXTERNAL-FORMATS*)
  #+cmu   '(("ISO-8859-1"))          ; :iso-latin-1-unix ;  what else?
  #-(or clisp sbcl cmu)
  (progn
    (warn "What are the available external formats in ~A ?"
          (lisp-implementation-type))
    '(("US-ASCII"))))


(defun fill-character-set-lisp-encoding ()
  (dolist (lsl *lisp-encodings*)
    (let ((cs (some (function find-character-set)
                    (add-aliases-to-group lsl *aliases*))))
      (when cs
        (setf (cs-lisp-encoding cs) lsl)))))




(defun character-set-to-lisp-encoding (cs &key (line-terminator :unix))
  "
RETURN: An implementation specific object representing the  encoding for
        the given character-set and line-terminator.
SIGNAL: An error if line-terminator is not (member :unix :mac :dos nil) or
        if cs has no emacs encoding.
"
  (assert (member line-terminator '(:unix :mac :dos nil)))
  (unless  (cs-lisp-encoding cs)
    (error "The character-set ~A has no corresponding lisp encoding"
           (cs-name cs)))
  #+clisp (let ((sym (find-symbol (first (cs-lisp-encoding cs)) "CHARSET")))
            (if sym
                (ext:make-encoding :charset (symbol-value sym)
                                   :line-terminator line-terminator)
                :default #|should not occur|#))
  #-clisp (warn "How should I handle the line-terminator in ~A?"
                (lisp-implementation-type))
  #+sbcl  (intern (first (cs-lisp-encoding cs)) "KEYWORD")
  #+cmu   (if (string-equal (first (cs-lisp-encoding cs)) "ISO-8859-1")
              :iso-latin-1-unix
              (error "I don't know a charset for ~A in ~A"
                     cs (lisp-implementation-type)))
  #-(or clisp sbcl cmu)
  (if (string-equal (first (cs-lisp-encoding cs)) "US-ASCII")
      :default
      (error "I don't know the coding systems of ~A"
             (lisp-implementation-type))))


(defun character-set-for-lisp-encoding (encoding)
  "
ENCODIGN:  An implementation specific object representing an encoding.
        possibly with line-terminator.
RETURN: The character-set that correspond to this emacs-encoding ;
       the line-terminator.
"
  #+clisp (values (EXT:ENCODING-charset encoding)
                  (EXT:ENCODING-line-terminator encoding))
  #-clisp (values
           (find (lambda (cs) (member encoding (cs-lisp-encoding cs)
                                 :test (function string-equal)))
                 *character-sets*)
           :unix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun encodings-not-in-iana (encodings)
  "
RETURN: a list of encoding groups that aren't in *CHARACTER-SETS*.
"
  (let ((result '()))
    (dolist (enc encodings result)
      (let ((cs (some (function find-character-set) enc)))
        (unless cs
          (push enc result))))))


(defun CHARACTER-SET-TO-MIME-ENCODING (cs)
  (or (cs-mime-encoding cs) (cs-name cs)))


(defun emacs-encoding-to-lisp-external-format (emacs-encoding)
  "
RETURN:  the external-format value corresponding to this EMACS-ENCODING.
"
  (multiple-value-bind (charset line-terminator)
      (character-set-from-emacs-encoding emacs-encoding)
    (when charset
      (character-set-to-lisp-encoding charset :line-terminator line-terminator))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

#+clisp
(defun compute-character-set-ranges ()
  "
DO:     Read the character-set file and build the *character-sets* list,
        then update the character sets with emacs encodings, lisp encodings,
        and character set ranges (found in clisp).
RETURN: *character-sets*
"
  (setf *CHARACTER-SETS* (READ-CHARACTER-SETS-FILE "character-sets"))
  (fill-character-set-emacs-encoding)
  (fill-character-set-lisp-encoding)
  (dolist (cs *character-sets*)
    (when (cs-lisp-encoding cs)
      (let ((charset (find-symbol (first (cs-lisp-encoding cs)) "CHARSET")))
        (setf (cs-ranges cs)
              #+#.(cl:if (cl:ignore-errors
                          (cl:find-symbol "GET-CHARSET-RANGE" "SYSTEM"))
                         '(:and) '(:or))
              (map 'vector (function char-code)
                   (SYSTEM::GET-CHARSET-RANGE charset))
              #-#.(cl:if (cl:ignore-errors
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
  *CHARACTER-SETS*)


;; Provide a default value for  *CHARACTER-SETS*
#-(and)
(let ((*print-right-margin* 72))
  (pprint
   `(setf *CHARACTER-SETS*
      (LIST
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


(SETF *CHARACTER-SETS*
 (LIST
  (MAKE-CHARACTER-SET :MIB-ENUM 2052 :NAME "IBM865" :ALIASES
   '("csIBM865" "865" "cp865") :MIME-ENCODING 'NIL :SOURCE
   '"IBM DOS 3.3 Ref (Abridged), 94X9575 (Feb 1987)" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 161 163 164 170 172 176 178 181 181 183 183 186 186 188
     189 191 191 196 199 201 201 209 209 214 214 216 216 220 220 223 226
     228 239 241 244 246 252 255 255 402 402 915 915 920 920 931 931 934
     934 937 937 945 945 948 949 960 960 963 964 966 966 8319 8319 8359
     8359 8729 8730 8734 8734 8745 8745 8776 8776 8801 8801 8804 8805
     8976 8976 8992 8993 9472 9472 9474 9474 9484 9484 9488 9488 9492
     9492 9496 9496 9500 9500 9508 9508 9516 9516 9524 9524 9532 9532
     9552 9580 9600 9600 9604 9604 9608 9608 9612 9612 9616 9619 9632
     9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 2051 :NAME "IBM864" :ALIASES
   '("csIBM864" "cp864") :MIME-ENCODING 'NIL :SOURCE
   '"IBM Keyboard layouts and code pages, PN 07G4586 June 1991"
   :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 36 38 127 160 160 162 164 166 166 171 173 176 177 183 183 187 189
     215 215 247 247 946 946 966 966 1548 1548 1563 1563 1567 1567 1600
     1600 1617 1617 1632 1642 8729 8730 8734 8734 8776 8776 9472 9472
     9474 9474 9484 9484 9488 9488 9492 9492 9496 9496 9500 9500 9508
     9508 9516 9516 9524 9524 9532 9532 9618 9618 9632 9632 65149 65149
     65152 65157 65163 65163 65165 65167 65169 65169 65171 65171 65173
     65173 65175 65175 65177 65177 65179 65179 65181 65181 65183 65183
     65185 65185 65187 65187 65189 65189 65191 65191 65193 65193 65195
     65195 65197 65197 65199 65199 65201 65201 65203 65203 65205 65205
     65207 65207 65209 65209 65211 65211 65213 65213 65215 65215 65217
     65217 65221 65221 65225 65233 65235 65235 65237 65237 65239 65239
     65241 65241 65243 65243 65245 65245 65247 65247 65249 65249 65251
     65251 65253 65253 65255 65255 65257 65257 65259 65261 65263 65267
     65269 65272 65275 65276))
  (MAKE-CHARACTER-SET :MIB-ENUM 38 :NAME #1="EUC-KR" :ALIASES
   '("csEUCKR") :MIME-ENCODING '#1# :SOURCE
   '"RFC-1557 (see also KS_C_5861-1992)" :COMMENTS 'NIL :REFERENCES
   '("[RFC1557,Choi]") :RANGES
   #(0 127 161 161 164 164 167 168 170 170 173 174 176 180 182 186 188
     191 198 198 208 208 215 216 222 223 230 230 240 240 247 248 254 254
     273 273 294 295 305 307 312 312 319 322 329 331 338 339 358 359 711
     711 720 720 728 731 733 733 913 929 931 937 945 961 963 969 1025
     1025 1040 1103 1105 1105 8213 8213 8216 8217 8220 8221 8224 8225
     8229 8230 8240 8240 8242 8243 8251 8251 8308 8308 8319 8319 8321
     8324 8364 8364 8451 8451 8457 8457 8467 8467 8470 8470 8481 8482
     8486 8486 8491 8491 8531 8532 8539 8542 8544 8553 8560 8569 8592
     8601 8658 8658 8660 8660 8704 8704 8706 8707 8711 8712 8715 8715
     8719 8719 8721 8721 8730 8730 8733 8734 8736 8736 8741 8741 8743
     8748 8750 8750 8756 8757 8764 8765 8786 8786 8800 8801 8804 8805
     8810 8811 8834 8835 8838 8839 8857 8857 8869 8869 8978 8978 9312
     9326 9332 9346 9372 9397 9424 9449 9472 9475 9484 9547 9618 9618
     9632 9633 9635 9641 9650 9651 9654 9655 9660 9661 9664 9665 9670
     9672 9675 9675 9678 9681 9733 9734 9742 9743 9756 9756 9758 9758
     9792 9792 9794 9794 9824 9825 9827 9829 9831 9834 9836 9837 12288
     12291 12296 12305 12307 12309 12353 12435 12449 12534 12593 12686
     12800 12828 12896 12923 12927 12927 13184 13188 13192 13258 13263
     13264 13267 13267 13270 13270 13272 13272 13275 13277 19968 19969
     19971 19971 19975 19979 19981 19981 19985 19985 19988 19990 19992
     19993 19998 19998 20013 20013 20018 20018 20024 20025 20027 20027
     20034 20035 20037 20037 20043 20043 20045 20047 20054 20054 20056
     20057 20061 20063 20075 20075 20077 20077 20083 20083 20086 20087
     20094 20094 20098 20098 20102 20102 20104 20104 20107 20108 20110
     20110 20112 20114 20116 20117 20120 20120 20123 20123 20126 20126
     20129 20130 20132 20134 20136 20136 20139 20142 20150 20150 20154
     20154 20160 20161 20164 20164 20167 20167 20170 20171 20173 20173
     20180 20185 20189 20189 20191 20191 20195 20197 20208 20208 20210
     20210 20214 20215 20219 20219 20225 20225 20233 20235 20237 20241
     20271 20271 20276 20276 20278 20278 20280 20280 20282 20282 20284
     20285 20291 20291 20294 20296 20301 20305 20309 20309 20313 20316
     20329 20329 20335 20336 20339 20339 20342 20342 20346 20346 20350
     20351 20353 20353 20355 20356 20358 20358 20360 20360 20362 20363
     20365 20365 20367 20367 20369 20369 20374 20374 20376 20376 20379
     20379 20381 20381 20398 20399 20405 20406 20415 20415 20418 20420
     20425 20426 20430 20430 20433 20433 20435 20436 20439 20439 20442
     20442 20445 20445 20447 20449 20462 20463 20465 20465 20467 20467
     20469 20469 20472 20472 20474 20474 20482 20482 20486 20486 20489
     20489 20491 20491 20493 20493 20497 20498 20502 20502 20505 20506
     20508 20508 20510 20511 20513 20513 20515 20516 20518 20520 20522
     20525 20539 20539 20547 20547 20551 20553 20559 20559 20565 20565
     20570 20570 20572 20572 20581 20581 20596 20598 20600 20600 20608
     20608 20613 20613 20621 20621 20625 20625 20632 20633 20652 20653
     20658 20659 20661 20661 20663 20663 20670 20670 20677 20677 20681
     20682 20687 20687 20689 20689 20693 20694 20698 20698 20702 20702
     20709 20709 20711 20711 20717 20717 20729 20729 20731 20731 20735
     20737 20740 20740 20742 20742 20745 20745 20754 20754 20767 20767
     20769 20769 20778 20778 20786 20786 20791 20791 20794 20794 20796
     20796 20800 20801 20803 20809 20811 20814 20818 20818 20828 20828
     20834 20834 20837 20837 20839 20846 20849 20849 20853 20856 20860
     20860 20864 20864 20870 20870 20874 20874 20877 20877 20882 20882
     20885 20885 20887 20887 20896 20896 20901 20901 20906 20906 20908
     20908 20918 20919 20925 20925 20932 20932 20934 20934 20937 20937
     20939 20941 20956 20958 20961 20961 20976 20977 20982 20982 20984
     20986 20989 20989 20992 20992 20995 20995 20998 21000 21002 21002
     21006 21006 21009 21009 21015 21015 21021 21021 21028 21029 21033
     21034 21038 21038 21040 21040 21046 21051 21059 21059 21063 21063
     21066 21069 21076 21076 21078 21078 21083 21083 21085 21085 21089
     21089 21097 21098 21103 21103 21106 21106 21109 21109 21117 21117
     21119 21119 21123 21123 21127 21129 21133 21133 21137 21138 21147
     21147 21151 21152 21155 21156 21161 21163 21182 21182 21185 21185
     21187 21187 21189 21189 21191 21191 21193 21193 21197 21197 21202
     21202 21205 21206 21208 21209 21211 21211 21213 21215 21218 21220
     21235 21235 21237 21237 21240 21240 21242 21243 21246 21247 21253
     21253 21256 21256 21261 21261 21263 21264 21269 21271 21273 21273
     21280 21281 21283 21283 21290 21290 21295 21295 21305 21305 21311
     21313 21315 21316 21319 21322 21325 21325 21329 21332 21335 21335
     21338 21338 21340 21340 21342 21342 21344 21344 21350 21350 21352
     21352 21359 21361 21364 21365 21367 21367 21373 21373 21375 21375
     21380 21380 21395 21395 21400 21400 21402 21402 21407 21408 21413
     21414 21421 21421 21435 21435 21443 21443 21448 21451 21453 21453
     21460 21460 21462 21463 21467 21467 21473 21477 21481 21485 21487
     21491 21496 21496 21507 21508 21512 21514 21516 21521 21531 21531
     21533 21533 21535 21536 21542 21542 21545 21545 21547 21547 21555
     21555 21560 21561 21563 21564 21566 21566 21570 21570 21576 21576
     21578 21578 21585 21585 21608 21608 21610 21610 21617 21617 21619
     21619 21621 21621 21627 21629 21632 21632 21638 21638 21644 21644
     21646 21646 21648 21648 21668 21668 21672 21672 21675 21676 21683
     21683 21688 21688 21693 21693 21696 21697 21700 21700 21704 21705
     21729 21729 21733 21733 21736 21736 21741 21742 21746 21746 21754
     21754 21764 21764 21766 21767 21774 21774 21776 21776 21788 21788
     21807 21807 21809 21809 21813 21813 21822 21822 21828 21828 21830
     21830 21839 21839 21843 21843 21846 21846 21854 21854 21859 21859
     21884 21884 21888 21888 21892 21892 21894 21895 21897 21898 21912
     21914 21916 21917 21927 21927 21929 21932 21934 21934 21957 21957
     21959 21959 21972 21972 21978 21978 21980 21980 21983 21983 21987
     21988 22013 22014 22022 22022 22025 22025 22036 22036 22039 22039
     22063 22063 22066 22066 22068 22068 22070 22070 22099 22099 22120
     22120 22123 22123 22132 22132 22150 22150 22181 22181 22188 22188
     22190 22190 22196 22196 22204 22204 22218 22218 22221 22221 22225
     22225 22234 22235 22238 22238 22240 22240 22256 22256 22265 22266
     22275 22276 22280 22280 22283 22283 22285 22285 22290 22291 22294
     22294 22296 22296 22303 22303 22312 22312 22317 22317 22320 22320
     22331 22331 22336 22336 22338 22338 22343 22343 22346 22346 22349
     22350 22352 22353 22369 22369 22372 22372 22374 22374 22378 22378
     22382 22382 22384 22384 22389 22389 22396 22396 22402 22402 22408
     22408 22411 22411 22419 22419 22432 22432 22434 22435 22467 22467
     22471 22472 22475 22475 22478 22478 22495 22496 22512 22512 22516
     22516 22519 22519 22521 22522 22524 22524 22528 22528 22530 22530
     22533 22534 22536 22538 22558 22558 22561 22561 22564 22564 22567
     22567 22570 22570 22575 22577 22580 22581 22586 22586 22602 22603
     22607 22607 22609 22609 22612 22612 22615 22616 22618 22618 22622
     22622 22625 22626 22628 22628 22645 22645 22649 22649 22652 22652
     22654 22654 22659 22659 22661 22661 22665 22665 22675 22675 22684
     22684 22686 22687 22696 22697 22702 22702 22707 22707 22714 22715
     22718 22718 22721 22721 22725 22725 22727 22727 22734 22734 22737
     22737 22739 22739 22741 22741 22744 22745 22750 22751 22756 22756
     22763 22764 22767 22767 22777 22779 22781 22781 22799 22799 22804
     22806 22809 22810 22812 22812 22818 22818 22823 22823 22825 22827
     22829 22830 22833 22833 22839 22839 22846 22846 22852 22852 22855
     22857 22862 22865 22868 22869 22871 22871 22874 22874 22880 22880
     22882 22882 22887 22887 22890 22894 22899 22900 22904 22904 22909
     22909 22914 22916 22922 22922 22931 22931 22934 22935 22937 22937
     22949 22949 22952 22952 22956 22956 22969 22969 22971 22971 22974
     22974 22979 22979 22982 22982 22985 22985 22987 22987 22992 22993
     22995 22996 23001 23002 23004 23005 23014 23014 23016 23016 23018
     23018 23020 23020 23022 23022 23032 23032 23035 23035 23039 23039
     23041 23041 23043 23043 23057 23057 23064 23064 23067 23068 23071
     23072 23077 23077 23081 23081 23094 23094 23100 23100 23105 23105
     23110 23110 23113 23113 23130 23130 23138 23138 23142 23142 23186
     23186 23194 23195 23204 23204 23233 23234 23236 23236 23241 23241
     23244 23244 23265 23265 23270 23270 23273 23273 23301 23301 23305
     23305 23307 23308 23318 23318 23338 23338 23360 23360 23363 23363
     23376 23377 23380 23381 23383 23384 23386 23386 23388 23389 23391
     23391 23395 23396 23401 23401 23403 23403 23408 23409 23413 23413
     23416 23416 23418 23418 23420 23420 23429 23429 23431 23433 23435
     23436 23439 23439 23443 23443 23445 23452 23458 23462 23468 23468
     23470 23470 23472 23472 23475 23478 23480 23481 23487 23488 23490
     23495 23500 23500 23504 23504 23506 23508 23511 23511 23518 23519
     23521 23522 23524 23529 23531 23532 23534 23535 23541 23542 23544
     23544 23546 23546 23553 23553 23556 23556 23559 23563 23565 23567
     23569 23569 23574 23574 23577 23577 23588 23588 23592 23592 23601
     23601 23608 23612 23614 23616 23621 23622 23624 23624 23627 23627
     23629 23630 23633 23633 23637 23637 23643 23643 23648 23648 23650
     23650 23652 23653 23660 23660 23663 23663 23665 23665 23673 23673
     23696 23697 23713 23713 23721 23721 23723 23724 23729 23729 23731
     23731 23733 23733 23735 23736 23738 23738 23742 23742 23744 23744
     23769 23769 23776 23776 23784 23784 23791 23792 23796 23796 23798
     23798 23803 23803 23805 23805 23815 23815 23821 23822 23825 23825
     23828 23828 23830 23831 23833 23833 23847 23847 23849 23849 23883
     23884 23888 23888 23913 23913 23916 23916 23919 23919 23943 23943
     23947 23947 23965 23965 23968 23968 23970 23970 23978 23978 23992
     23992 23994 23994 23996 23997 24013 24013 24018 24018 24022 24022
     24029 24030 24033 24034 24037 24040 24043 24043 24046 24046 24049
     24052 24055 24055 24061 24062 24066 24067 24070 24070 24076 24076
     24081 24081 24086 24086 24089 24089 24091 24091 24093 24093 24101
     24101 24107 24107 24109 24109 24115 24115 24118 24118 24120 24120
     24125 24125 24127 24128 24132 24133 24135 24135 24140 24140 24149
     24149 24159 24159 24161 24163 24178 24180 24183 24185 24187 24190
     24196 24196 24199 24199 24202 24202 24207 24207 24213 24213 24215
     24215 24218 24218 24220 24220 24224 24224 24230 24231 24235 24235
     24237 24237 24245 24248 24254 24254 24258 24258 24264 24266 24272
     24272 24275 24275 24278 24278 24282 24283 24287 24288 24290 24291
     24300 24300 24307 24307 24310 24311 24314 24315 24321 24321 24324
     24324 24330 24330 24335 24335 24337 24337 24339 24341 24343 24344
     24347 24347 24351 24351 24358 24359 24361 24361 24369 24369 24373
     24373 24378 24378 24380 24380 24392 24392 24394 24394 24396 24396
     24398 24398 24406 24407 24409 24409 24411 24411 24418 24418 24422
     24423 24425 24429 24432 24433 24439 24439 24441 24441 24444 24444
     24447 24449 24453 24453 24455 24455 24458 24460 24464 24466 24471
     24473 24478 24478 24480 24481 24488 24490 24494 24494 24501 24501
     24503 24503 24505 24505 24509 24509 24515 24515 24517 24517 24524
     24525 24534 24537 24544 24544 24555 24555 24565 24565 24573 24573
     24575 24575 24591 24591 24594 24594 24598 24598 24604 24605 24608
     24609 24613 24613 24615 24616 24618 24618 24623 24623 24641 24643
     24653 24653 24656 24656 24658 24658 24661 24661 24665 24665 24669
     24669 24674 24677 24680 24682 24684 24685 24687 24688 24709 24709
     24713 24713 24716 24717 24724 24724 24726 24726 24730 24731 24735
     24736 24739 24740 24743 24743 24752 24752 24754 24756 24758 24758
     24760 24760 24764 24765 24773 24773 24775 24775 24785 24785 24794
     24794 24796 24796 24799 24801 24816 24817 24819 24819 24822 24822
     24825 24827 24833 24833 24838 24838 24840 24841 24845 24847 24853
     24853 24858 24859 24863 24863 24871 24871 24880 24880 24884 24884
     24887 24887 24892 24892 24894 24895 24898 24898 24900 24900 24903
     24904 24906 24908 24915 24915 24917 24917 24920 24921 24925 24925
     24927 24927 24930 24932 24935 24936 24939 24939 24942 24942 24944
     24944 24950 24951 24957 24958 24961 24962 24970 24970 24974 24974
     24976 24977 24980 24980 24984 24986 24996 24996 24999 24999 25001
     25001 25003 25004 25006 25006 25010 25010 25014 25014 25018 25018
     25022 25022 25027 25027 25031 25035 25062 25062 25074 25074 25078
     25080 25082 25082 25084 25084 25087 25088 25095 25096 25098 25098
     25100 25102 25104 25106 25110 25110 25114 25114 25119 25119 25121
     25121 25130 25130 25134 25134 25136 25137 25140 25140 25142 25142
     25150 25153 25159 25161 25163 25163 25165 25165 25171 25171 25176
     25176 25198 25198 25201 25201 25206 25206 25209 25209 25212 25212
     25215 25216 25220 25220 25225 25226 25233 25234 25237 25237 25239
     25240 25243 25243 25259 25259 25265 25265 25269 25269 25273 25273
     25276 25277 25282 25282 25287 25289 25292 25293 25295 25296 25298
     25300 25302 25305 25307 25308 25324 25327 25329 25329 25331 25331
     25335 25335 25342 25343 25345 25345 25351 25351 25353 25353 25361
     25361 25387 25387 25391 25391 25402 25403 25405 25406 25417 25417
     25420 25420 25423 25424 25429 25429 25447 25448 25454 25454 25458
     25458 25463 25463 25466 25467 25471 25471 25475 25475 25480 25481
     25484 25484 25490 25490 25494 25494 25496 25496 25499 25499 25504
     25506 25509 25509 25511 25514 25536 25536 25540 25540 25542 25542
     25551 25552 25558 25558 25562 25563 25569 25569 25581 25582 25588
     25588 25590 25591 25613 25613 25615 25615 25620 25620 25622 25623
     25628 25628 25634 25634 25644 25645 25658 25658 25662 25662 25688
     25688 25696 25696 25705 25705 25711 25711 25720 25722 25736 25736
     25745 25747 25754 25754 25758 25758 25764 25765 25771 25771 25773
     25774 25776 25776 25778 25778 25787 25787 25793 25793 25796 25797
     25799 25799 25802 25802 25805 25806 25810 25810 25812 25812 25816
     25816 25818 25818 25825 25826 25829 25831 25836 25836 25842 25842
     25844 25844 25850 25850 25854 25854 25856 25856 25860 25860 25880
     25880 25885 25885 25891 25891 25898 25900 25903 25903 25910 25913
     25915 25915 25918 25919 25925 25925 25928 25928 25933 25935 25937
     25937 25942 25943 25950 25950 25954 25955 25958 25958 25964 25965
     25970 25970 25972 25973 25975 25976 25982 25982 25986 25987 25989
     25989 25991 25991 25996 25996 26000 26001 26007 26007 26009 26009
     26011 26012 26015 26015 26017 26017 26020 26021 26023 26023 26027
     26028 26031 26032 26039 26039 26041 26041 26044 26045 26049 26049
     26053 26053 26059 26060 26063 26063 26066 26066 26071 26071 26080
     26080 26083 26083 26085 26086 26088 26089 26092 26093 26097 26097
     26100 26100 26106 26109 26111 26111 26118 26119 26121 26122 26124
     26124 26126 26129 26131 26133 26142 26144 26149 26149 26151 26152
     26157 26157 26159 26161 26164 26164 26166 26166 26170 26171 26177
     26180 26185 26185 26187 26187 26191 26191 26201 26201 26203 26203
     26205 26207 26212 26217 26219 26219 26222 26223 26227 26228 26230
     26232 26234 26234 26244 26244 26247 26249 26254 26254 26256 26257
     26262 26264 26269 26269 26272 26272 26274 26274 26283 26283 26286
     26286 26290 26292 26297 26297 26299 26299 26302 26302 26308 26308
     26310 26311 26313 26313 26326 26326 26329 26329 26332 26333 26336
     26336 26342 26342 26352 26352 26354 26356 26359 26362 26364 26364
     26366 26368 26371 26371 26376 26377 26379 26379 26381 26381 26388
     26389 26391 26391 26395 26395 26397 26399 26406 26408 26410 26414
     26417 26417 26420 26420 26422 26422 26426 26426 26429 26429 26438
     26438 26441 26441 26446 26449 26451 26451 26454 26454 26460 26460
     26462 26463 26477 26477 26479 26481 26483 26483 26485 26485 26487
     26487 26491 26491 26494 26495 26503 26503 26505 26505 26507 26507
     26511 26512 26515 26515 26517 26517 26519 26519 26522 26522 26524
     26525 26543 26544 26547 26547 26550 26552 26558 26558 26564 26564
     26575 26580 26586 26586 26589 26589 26601 26601 26604 26604 26607
     26609 26611 26614 26619 26619 26622 26622 26642 26643 26646 26647
     26657 26658 26666 26666 26671 26671 26680 26681 26684 26685 26688
     26691 26696 26696 26702 26702 26704 26705 26707 26708 26733 26733
     26742 26742 26751 26751 26753 26753 26757 26757 26767 26767 26771
     26772 26775 26775 26781 26781 26783 26783 26785 26786 26791 26792
     26797 26797 26799 26801 26803 26803 26805 26806 26820 26821 26825
     26825 26827 26827 26829 26829 26834 26834 26837 26837 26839 26840
     26842 26842 26847 26848 26855 26856 26862 26862 26866 26866 26873
     26874 26880 26880 26885 26885 26893 26894 26898 26898 26919 26919
     26928 26928 26941 26941 26943 26943 26954 26954 26963 26965 26967
     26967 26969 26970 26974 26974 26976 26979 26984 26984 26987 26987
     26989 26991 26997 26997 26999 27001 27029 27029 27035 27036 27045
     27045 27047 27047 27054 27054 27060 27060 27067 27067 27073 27073
     27075 27075 27083 27085 27088 27088 27112 27112 27114 27114 27131
     27131 27133 27133 27135 27135 27138 27138 27146 27146 27153 27153
     27155 27155 27159 27159 27161 27161 27166 27167 27169 27169 27171
     27171 27189 27189 27192 27194 27197 27197 27204 27204 27208 27208
     27211 27211 27218 27219 27224 27225 27231 27231 27233 27233 27243
     27243 27264 27264 27268 27268 27273 27273 27277 27278 27287 27287
     27292 27292 27298 27299 27315 27315 27323 27323 27330 27331 27347
     27347 27354 27355 27382 27382 27387 27387 27396 27396 27402 27402
     27404 27404 27410 27410 27414 27414 27424 27425 27427 27427 27442
     27442 27450 27450 27453 27454 27462 27463 27468 27468 27470 27470
     27472 27472 27487 27487 27489 27494 27498 27498 27506 27506 27511
     27512 27515 27515 27519 27519 27523 27524 27526 27526 27529 27530
     27542 27542 27544 27544 27550 27550 27566 27567 27570 27570 27573
     27573 27575 27575 27578 27578 27580 27580 27583 27583 27585 27585
     27589 27590 27595 27595 27597 27597 27599 27599 27602 27604 27606
     27608 27611 27611 27627 27628 27656 27656 27663 27663 27665 27665
     27667 27667 27683 27683 27700 27700 27703 27704 27710 27710 27712
     27714 27726 27726 27728 27728 27733 27733 27735 27735 27738 27738
     27741 27744 27752 27752 27754 27754 27757 27757 27760 27760 27762
     27762 27766 27766 27770 27770 27773 27774 27777 27779 27781 27784
     27788 27788 27792 27792 27794 27798 27801 27803 27819 27819 27822
     27822 27827 27827 27832 27833 27835 27839 27841 27842 27844 27844
     27849 27850 27852 27852 27859 27859 27861 27861 27863 27863 27867
     27867 27873 27875 27877 27877 27880 27880 27883 27883 27886 27888
     27891 27891 27915 27916 27921 27921 27927 27927 27929 27929 27931
     27931 27934 27934 27941 27941 27943 27943 27945 27946 27954 27954
     27957 27958 27960 27961 27963 27963 27965 27966 27969 27969 27993
     27994 27996 27996 28003 28003 28006 28006 28009 28010 28012 28012
     28014 28014 28020 28020 28023 28025 28031 28031 28037 28037 28039
     28041 28044 28046 28049 28049 28051 28051 28053 28053 28079 28079
     28082 28082 28085 28085 28096 28096 28099 28103 28107 28107 28111
     28111 28113 28113 28120 28122 28126 28126 28129 28129 28136 28136
     28138 28139 28142 28142 28145 28145 28147 28147 28149 28149 28151
     28155 28183 28183 28185 28187 28191 28193 28195 28198 28203 28205
     28207 28207 28210 28210 28212 28212 28214 28214 28216 28216 28218
     28218 28220 28222 28227 28228 28234 28234 28237 28237 28246 28246
     28248 28248 28251 28252 28254 28255 28263 28263 28267 28267 28270
     28271 28274 28275 28282 28282 28304 28304 28310 28310 28316 28317
     28319 28319 28322 28322 28325 28325 28330 28331 28335 28335 28337
     28337 28342 28342 28346 28346 28354 28354 28356 28357 28361 28361
     28363 28364 28366 28366 28369 28369 28371 28372 28399 28399 28404
     28404 28408 28408 28414 28415 28417 28418 28422 28422 28431 28431
     28433 28433 28436 28437 28448 28448 28450 28451 28459 28460 28465
     28466 28472 28472 28479 28479 28481 28481 28497 28497 28500 28500
     28503 28504 28506 28507 28510 28511 28514 28514 28516 28516 28525
     28526 28528 28528 28538 28538 28540 28542 28545 28545 28548 28548
     28552 28552 28557 28558 28560 28560 28564 28564 28567 28567 28579
     28580 28583 28583 28590 28591 28593 28593 28595 28595 28601 28601
     28606 28606 28608 28611 28618 28618 28629 28629 28634 28634 28639
     28641 28644 28644 28649 28649 28651 28652 28655 28655 28657 28657
     28670 28670 28673 28673 28677 28678 28681 28681 28683 28683 28687
     28687 28689 28689 28693 28693 28696 28696 28698 28703 28707 28707
     28711 28712 28719 28719 28727 28727 28734 28734 28748 28748 28752
     28753 28760 28760 28765 28765 28771 28771 28779 28779 28784 28784
     28792 28792 28796 28797 28805 28805 28810 28810 28814 28814 28818
     28818 28824 28826 28833 28833 28836 28836 28843 28845 28847 28847
     28851 28851 28855 28857 28872 28872 28875 28875 28879 28879 28888
     28889 28893 28893 28895 28895 28913 28913 28921 28921 28925 28925
     28932 28932 28937 28937 28940 28940 28953 28954 28958 28958 28961
     28961 28966 28966 28976 28976 28982 28982 28999 28999 29001 29002
     29004 29004 29006 29006 29008 29008 29014 29014 29017 29017 29020
     29020 29022 29022 29028 29031 29033 29033 29036 29036 29038 29038
     29053 29053 29060 29060 29065 29066 29071 29071 29074 29074 29076
     29076 29081 29081 29087 29087 29090 29090 29100 29100 29105 29105
     29113 29114 29118 29118 29121 29121 29123 29123 29128 29129 29134
     29134 29136 29136 29138 29138 29140 29141 29151 29151 29157 29159
     29165 29166 29179 29180 29182 29184 29190 29190 29200 29200 29211
     29211 29226 29226 29228 29229 29232 29232 29234 29234 29237 29238
     29242 29243 29245 29246 29248 29248 29254 29256 29260 29260 29266
     29266 29272 29273 29275 29275 29277 29277 29279 29279 29281 29282
     29287 29287 29289 29289 29298 29298 29305 29305 29309 29309 29312
     29313 29346 29346 29351 29351 29356 29356 29359 29359 29376 29376
     29378 29378 29380 29380 29390 29390 29392 29392 29399 29399 29401
     29401 29409 29409 29417 29417 29432 29433 29436 29437 29450 29450
     29462 29462 29467 29469 29477 29477 29481 29483 29494 29495 29502
     29503 29508 29509 29520 29520 29522 29522 29527 29527 29544 29544
     29546 29546 29552 29552 29554 29554 29557 29557 29560 29560 29562
     29563 29572 29572 29574 29575 29577 29577 29579 29579 29582 29582
     29588 29588 29590 29592 29599 29599 29607 29607 29609 29609 29613
     29613 29618 29619 29625 29625 29632 29632 29634 29634 29641 29642
     29644 29645 29647 29647 29654 29654 29657 29657 29661 29662 29664
     29664 29667 29670 29673 29674 29677 29677 29687 29687 29689 29689
     29693 29694 29697 29697 29699 29699 29701 29703 29705 29705 29715
     29715 29723 29723 29728 29730 29733 29734 29736 29736 29738 29740
     29742 29744 29747 29750 29752 29752 29754 29754 29759 29761 29763
     29764 29771 29771 29781 29781 29783 29783 29785 29788 29790 29792
     29794 29794 29796 29797 29800 29802 29807 29807 29822 29822 29826
     29827 29831 29831 29833 29833 29835 29835 29848 29848 29852 29852
     29854 29855 29857 29857 29859 29859 29861 29861 29863 29864 29866
     29866 29872 29872 29874 29874 29877 29877 29881 29881 29885 29885
     29887 29887 29894 29894 29898 29898 29903 29903 29908 29908 29912
     29912 29914 29914 29916 29916 29920 29920 29922 29923 29926 29926
     29934 29934 29943 29943 29953 29953 29956 29956 29969 29969 29973
     29973 29976 29976 29978 29979 29983 29983 29987 29987 29989 29990
     29992 29992 29995 29996 30000 30003 30007 30008 30010 30010 30023
     30023 30028 30028 30031 30031 30033 30033 30035 30036 30041 30041
     30043 30045 30050 30050 30053 30054 30058 30058 30063 30064 30069
     30070 30072 30072 30074 30074 30079 30079 30086 30087 30090 30091
     30094 30095 30097 30097 30109 30109 30117 30117 30123 30123 30129
     30131 30133 30133 30136 30137 30140 30142 30146 30146 30149 30149
     30151 30151 30157 30157 30162 30162 30164 30165 30168 30169 30171
     30171 30178 30178 30192 30192 30194 30194 30196 30196 30202 30202
     30204 30204 30208 30208 30221 30221 30233 30233 30239 30242 30244
     30244 30246 30246 30267 30267 30274 30274 30284 30284 30286 30286
     30290 30290 30294 30294 30305 30305 30308 30308 30313 30313 30316
     30316 30320 30320 30322 30322 30328 30328 30331 30334 30340 30340
     30342 30343 30350 30350 30352 30352 30355 30355 30382 30382 30394
     30394 30399 30399 30402 30403 30406 30406 30408 30408 30410 30410
     30418 30418 30422 30422 30427 30428 30430 30431 30433 30433 30435
     30436 30439 30439 30446 30446 30450 30450 30452 30452 30456 30456
     30460 30460 30462 30462 30465 30465 30468 30468 30472 30473 30475
     30475 30494 30494 30496 30496 30505 30505 30519 30520 30522 30522
     30524 30524 30528 30528 30541 30541 30555 30555 30561 30561 30563
     30563 30566 30566 30571 30571 30585 30585 30590 30591 30603 30603
     30609 30609 30622 30622 30629 30629 30636 30637 30640 30640 30643
     30643 30651 30652 30655 30655 30679 30679 30683 30684 30690 30691
     30693 30693 30697 30697 30701 30703 30707 30707 30722 30722 30738
     30738 30757 30759 30764 30764 30770 30770 30772 30772 30789 30789
     30799 30799 30813 30813 30827 30828 30831 30831 30844 30844 30849
     30849 30855 30855 30860 30862 30865 30865 30871 30871 30883 30883
     30887 30887 30889 30889 30906 30908 30913 30913 30917 30917 30922
     30923 30926 30926 30928 30928 30952 30952 30956 30956 30959 30959
     30965 30965 30971 30971 30977 30977 30990 30990 30998 30998 31018
     31020 31034 31034 31038 31038 31040 31041 31047 31049 31056 31056
     31062 31063 31066 31070 31072 31072 31077 31077 31080 31080 31085
     31085 31098 31098 31103 31103 31105 31105 31117 31119 31121 31121
     31142 31143 31146 31146 31150 31150 31153 31153 31155 31155 31161
     31161 31165 31169 31177 31179 31185 31186 31189 31189 31192 31192
     31199 31199 31204 31204 31206 31207 31209 31209 31227 31227 31232
     31232 31237 31237 31240 31240 31243 31243 31245 31245 31252 31252
     31255 31255 31257 31258 31260 31260 31263 31264 31278 31278 31281
     31281 31286 31287 31291 31293 31295 31296 31302 31302 31305 31305
     31309 31310 31319 31319 31329 31330 31337 31337 31339 31339 31344
     31344 31348 31348 31350 31350 31353 31354 31357 31357 31359 31359
     31361 31361 31364 31364 31368 31368 31378 31379 31381 31381 31384
     31384 31391 31391 31401 31402 31406 31407 31418 31418 31428 31429
     31431 31431 31434 31435 31447 31447 31449 31449 31453 31453 31455
     31456 31459 31459 31461 31461 31466 31466 31469 31469 31471 31471
     31478 31478 31481 31482 31487 31487 31503 31503 31505 31505 31513
     31513 31515 31515 31518 31518 31520 31520 31526 31526 31532 31533
     31545 31545 31558 31558 31561 31561 31563 31565 31567 31570 31572
     31572 31574 31574 31584 31584 31596 31596 31598 31598 31605 31605
     31613 31613 31623 31623 31627 31627 31631 31631 31636 31637 31639
     31639 31642 31642 31645 31645 31649 31649 31661 31661 31665 31665
     31668 31668 31672 31672 31680 31681 31684 31684 31686 31687 31689
     31689 31698 31698 31712 31712 31716 31716 31721 31721 31751 31751
     31762 31762 31774 31774 31777 31777 31783 31783 31786 31787 31805
     31807 31811 31811 31820 31821 31840 31840 31844 31844 31852 31852
     31859 31859 31875 31875 31881 31881 31890 31890 31893 31893 31895
     31896 31903 31903 31909 31909 31911 31911 31918 31918 31921 31923
     31929 31929 31934 31934 31946 31946 31958 31958 31966 31968 31975
     31975 31995 31995 31998 31998 32000 32000 32002 32002 32004 32008
     32010 32011 32013 32013 32016 32016 32020 32020 32023 32027 32032
     32034 32043 32044 32046 32048 32051 32051 32053 32053 32057 32058
     32066 32070 32080 32080 32094 32094 32097 32098 32102 32102 32104
     32104 32106 32106 32110 32110 32113 32115 32118 32118 32121 32121
     32127 32127 32142 32143 32147 32147 32156 32156 32160 32160 32162
     32162 32172 32173 32177 32178 32180 32181 32184 32184 32186 32187
     32189 32191 32199 32199 32202 32203 32214 32214 32216 32216 32218
     32218 32221 32222 32224 32225 32227 32227 32232 32233 32236 32236
     32239 32239 32244 32244 32251 32251 32265 32266 32277 32277 32283
     32283 32285 32287 32289 32289 32291 32291 32299 32299 32302 32303
     32305 32305 32311 32311 32317 32318 32321 32321 32323 32323 32326
     32327 32338 32338 32340 32341 32350 32350 32353 32353 32361 32363
     32365 32365 32368 32368 32377 32377 32380 32380 32386 32386 32396
     32396 32399 32399 32403 32403 32406 32406 32408 32408 32411 32412
     32566 32566 32568 32568 32570 32570 32588 32588 32592 32592 32596
     32597 32618 32619 32622 32622 32624 32624 32626 32626 32629 32629
     32631 32631 32633 32633 32645 32645 32648 32648 32650 32650 32652
     32652 32654 32654 32660 32660 32666 32666 32670 32670 32676 32676
     32680 32681 32690 32690 32696 32697 32701 32701 32705 32705 32709
     32709 32714 32714 32716 32716 32718 32718 32722 32722 32724 32725
     32735 32737 32745 32745 32747 32747 32752 32752 32761 32761 32764
     32764 32768 32769 32771 32771 32773 32774 32777 32777 32780 32780
     32784 32784 32789 32789 32791 32792 32813 32813 32819 32819 32822
     32822 32829 32829 32831 32831 32835 32835 32838 32838 32842 32842
     32854 32854 32856 32856 32858 32858 32862 32862 32879 32880 32882
     32883 32887 32887 32893 32895 32900 32903 32905 32905 32907 32908
     32918 32918 32923 32923 32925 32925 32929 32930 32933 32933 32937
     32938 32943 32943 32945 32946 32948 32948 32954 32954 32963 32964
     32972 32972 32974 32974 32986 32987 32990 32990 32993 32993 32996
     32997 33009 33009 33012 33012 33016 33016 33021 33021 33026 33026
     33029 33032 33034 33034 33048 33048 33050 33051 33059 33059 33065
     33065 33067 33067 33071 33071 33081 33081 33086 33086 33099 33099
     33102 33102 33104 33105 33108 33109 33125 33126 33131 33131 33136
     33137 33144 33146 33151 33152 33160 33160 33162 33162 33167 33167
     33178 33178 33180 33181 33184 33184 33187 33187 33192 33192 33203
     33203 33205 33205 33210 33210 33213 33216 33218 33218 33222 33222
     33229 33229 33240 33240 33247 33247 33251 33251 33253 33253 33255
     33256 33258 33258 33261 33261 33267 33268 33274 33276 33278 33278
     33285 33285 33287 33288 33290 33290 33292 33293 33298 33298 33307
     33308 33310 33311 33313 33313 33322 33324 33333 33335 33337 33337
     33344 33344 33349 33349 33351 33351 33369 33369 33380 33380 33382
     33382 33390 33391 33393 33394 33398 33398 33400 33400 33406 33406
     33419 33419 33421 33422 33426 33426 33433 33434 33437 33437 33439
     33439 33445 33446 33449 33449 33452 33455 33457 33457 33459 33459
     33463 33465 33467 33469 33471 33471 33489 33490 33492 33493 33495
     33495 33499 33499 33502 33503 33505 33505 33509 33511 33521 33521
     33533 33534 33537 33541 33545 33545 33559 33559 33576 33576 33579
     33579 33583 33583 33585 33585 33588 33590 33592 33593 33600 33600
     33607 33607 33609 33610 33615 33615 33617 33618 33651 33651 33655
     33655 33659 33659 33673 33674 33678 33678 33686 33686 33688 33688
     33694 33694 33698 33698 33705 33707 33725 33725 33729 33729 33733
     33733 33737 33738 33740 33740 33747 33747 33750 33750 33756 33756
     33769 33769 33771 33771 33775 33778 33780 33780 33785 33785 33789
     33789 33795 33796 33802 33802 33804 33806 33833 33833 33836 33836
     33841 33841 33848 33848 33853 33853 33865 33865 33879 33879 33883
     33883 33889 33889 33891 33891 33894 33894 33899 33900 33903 33903
     33909 33909 33914 33914 33936 33936 33940 33940 33945 33945 33948
     33948 33953 33953 33970 33970 33976 33976 33979 33980 33983 33984
     33986 33986 33988 33988 33990 33990 33993 33993 33995 33995 33997
     33997 34001 34001 34010 34010 34028 34028 34030 34030 34036 34036
     34044 34044 34065 34065 34067 34068 34071 34072 34074 34074 34078
     34078 34081 34081 34083 34083 34085 34085 34092 34093 34095 34095
     34109 34109 34111 34111 34113 34113 34115 34115 34121 34121 34126
     34126 34131 34131 34137 34137 34147 34147 34152 34154 34157 34157
     34180 34180 34183 34183 34191 34191 34193 34193 34196 34196 34203
     34203 34214 34214 34216 34218 34223 34224 34234 34234 34241 34241
     34249 34249 34253 34255 34261 34261 34268 34269 34276 34277 34281
     34282 34295 34295 34298 34299 34303 34303 34306 34306 34310 34311
     34314 34314 34326 34327 34330 34330 34349 34349 34367 34367 34382
     34382 34384 34384 34388 34389 34395 34396 34398 34399 34407 34407
     34425 34425 34442 34442 34444 34444 34451 34451 34467 34468 34473
     34473 34503 34503 34507 34507 34516 34516 34521 34521 34523 34523
     34527 34527 34532 34532 34541 34541 34558 34558 34560 34560 34562
     34563 34568 34568 34584 34584 34586 34586 34588 34588 34638 34638
     34645 34645 34647 34647 34655 34655 34662 34662 34664 34664 34676
     34676 34678 34678 34680 34680 34690 34690 34701 34701 34719 34719
     34722 34722 34739 34739 34746 34746 34756 34756 34784 34784 34796
     34796 34799 34799 34802 34802 34809 34809 34811 34811 34814 34814
     34821 34821 34847 34847 34850 34851 34865 34865 34870 34870 34875
     34875 34880 34880 34886 34886 34892 34893 34898 34899 34903 34903
     34905 34905 34907 34907 34909 34909 34913 34915 34920 34920 34923
     34923 34928 34928 34930 34930 34935 34935 34942 34943 34945 34946
     34952 34952 34955 34955 34957 34957 34962 34962 34966 34967 34974
     34974 34987 34987 34996 34996 35009 35010 35023 35023 35028 35029
     35033 35033 35036 35037 35039 35039 35041 35041 35048 35048 35059
     35061 35064 35064 35069 35069 35079 35079 35088 35088 35090 35091
     35096 35097 35109 35109 35114 35114 35126 35126 35128 35128 35131
     35131 35137 35137 35140 35140 35167 35167 35172 35172 35178 35178
     35186 35186 35199 35199 35201 35201 35203 35203 35206 35207 35211
     35211 35215 35215 35219 35219 35222 35222 35233 35233 35241 35242
     35250 35250 35258 35258 35261 35261 35264 35264 35282 35282 35299
     35299 35316 35316 35320 35320 35328 35328 35330 35331 35336 35336
     35338 35338 35340 35340 35342 35342 35347 35347 35350 35352 35355
     35355 35357 35357 35359 35359 35363 35363 35365 35365 35370 35370
     35373 35373 35377 35377 35380 35380 35382 35382 35386 35387 35408
     35408 35412 35413 35419 35419 35422 35422 35424 35424 35426 35427
     35430 35430 35433 35433 35437 35438 35440 35443 35445 35445 35449
     35449 35461 35461 35463 35463 35468 35469 35475 35475 35477 35477
     35480 35480 35486 35486 35488 35489 35491 35494 35496 35496 35498
     35498 35504 35504 35506 35506 35513 35513 35516 35516 35518 35519
     35522 35522 35524 35524 35527 35527 35531 35531 35533 35533 35535
     35535 35538 35538 35542 35542 35547 35548 35553 35553 35558 35559
     35562 35563 35565 35566 35569 35569 35574 35576 35578 35578 35582
     35582 35584 35586 35588 35588 35598 35598 35600 35600 35604 35604
     35606 35607 35609 35611 35613 35613 35616 35616 35624 35624 35627
     35628 35635 35635 35641 35641 35649 35649 35657 35657 35662 35663
     35672 35672 35674 35674 35676 35676 35686 35686 35692 35692 35695
     35696 35700 35700 35703 35703 35709 35709 35712 35712 35722 35722
     35728 35728 35730 35731 35734 35734 35738 35738 35895 35895 35903
     35903 35905 35905 35910 35910 35912 35912 35914 35914 35916 35916
     35925 35925 35930 35930 35937 35937 35946 35947 35961 35962 35970
     35970 35978 35978 35980 35980 35997 35998 36000 36002 36007 36012
     36015 36016 36019 36020 36022 36024 36027 36029 36031 36036 36039
     36040 36042 36042 36049 36049 36051 36051 36058 36058 36060 36060
     36062 36062 36064 36064 36066 36068 36070 36070 36074 36074 36077
     36077 36084 36084 36091 36093 36100 36101 36103 36104 36106 36106
     36109 36109 36115 36115 36118 36118 36196 36196 36198 36198 36203
     36203 36208 36208 36211 36212 36215 36215 36229 36229 36234 36234
     36249 36249 36259 36259 36264 36264 36275 36275 36282 36282 36286
     36286 36294 36294 36299 36300 36303 36303 36315 36315 36317 36317
     36321 36321 36323 36323 36328 36328 36335 36335 36339 36339 36362
     36362 36367 36368 36382 36382 36394 36394 36400 36400 36405 36405
     36418 36418 36420 36420 36423 36426 36441 36441 36447 36448 36468
     36468 36470 36470 36481 36481 36487 36487 36490 36490 36493 36493
     36522 36524 36544 36544 36554 36557 36562 36562 36575 36575 36587
     36587 36600 36600 36603 36603 36606 36606 36611 36611 36613 36613
     36617 36617 36626 36629 36635 36639 36646 36647 36649 36650 36655
     36655 36659 36659 36664 36665 36667 36667 36670 36671 36676 36677
     36681 36681 36685 36686 36701 36701 36703 36703 36706 36706 36763
     36764 36771 36771 36774 36774 36776 36776 36781 36781 36783 36786
     36802 36802 36805 36805 36814 36814 36817 36817 36820 36820 36838
     36838 36842 36843 36845 36845 36848 36848 36850 36850 36855 36855
     36857 36857 36861 36861 36864 36867 36869 36870 36872 36872 36875
     36875 36877 36877 36879 36881 36884 36885 36887 36887 36889 36890
     36893 36899 36910 36910 36913 36914 36917 36917 36920 36920 36924
     36924 36926 36926 36929 36930 36935 36935 36938 36939 36941 36942
     36944 36945 36947 36949 36953 36953 36956 36958 36960 36961 36963
     36963 36969 36969 36973 36975 36978 36978 36981 36981 36983 36984
     36986 36986 36988 36989 36991 36996 36999 37000 37002 37002 37007
     37007 37009 37009 37013 37013 37017 37017 37026 37027 37030 37030
     37032 37032 37034 37034 37039 37041 37045 37045 37048 37048 37057
     37057 37066 37066 37086 37086 37089 37089 37096 37096 37101 37101
     37109 37109 37117 37117 37122 37122 37138 37138 37141 37141 37145
     37145 37159 37159 37165 37165 37170 37170 37193 37198 37202 37202
     37218 37218 37225 37226 37228 37228 37237 37237 37239 37240 37255
     37255 37257 37257 37259 37259 37261 37261 37266 37266 37276 37276
     37291 37292 37294 37295 37297 37297 37300 37301 37312 37312 37319
     37319 37321 37321 37323 37329 37335 37336 37340 37341 37347 37347
     37351 37351 37354 37354 37365 37365 37389 37389 37392 37394 37399
     37399 37406 37406 37428 37428 37434 37434 37439 37440 37445 37445
     37449 37449 37463 37463 37467 37467 37470 37470 37474 37474 37476
     37478 37504 37504 37507 37507 37509 37509 37521 37521 37523 37523
     37526 37526 37528 37528 37532 37532 37555 37555 37558 37559 37561
     37561 37580 37580 37583 37583 37586 37586 37604 37604 37610 37610
     37624 37624 37628 37628 37636 37636 37648 37648 37656 37656 37658
     37658 37662 37666 37668 37668 37670 37670 37672 37672 37675 37675
     37678 37679 37704 37704 37706 37707 37709 37709 37716 37716 37723
     37723 37742 37742 37749 37749 37756 37756 37758 37758 37772 37772
     37780 37780 37782 37782 37786 37786 37795 37795 37799 37799 37804
     37805 37808 37808 37827 37827 37841 37841 37854 37854 37857 37857
     37860 37860 37878 37878 37892 37892 37912 37912 37925 37925 37931
     37931 37941 37941 37944 37944 37956 37956 37969 37970 37979 37979
     38013 38013 38015 38015 38263 38263 38272 38272 38275 38275 38281
     38281 38283 38283 38287 38287 38289 38292 38296 38296 38307 38309
     38312 38312 38317 38317 38321 38321 38331 38332 38343 38343 38346
     38346 38356 38358 38364 38364 38369 38370 38428 38428 38433 38433
     38442 38442 38446 38446 38450 38450 38459 38459 38463 38464 38466
     38466 38468 38468 38475 38477 38480 38480 38491 38495 38498 38500
     38506 38506 38512 38512 38515 38515 38517 38520 38525 38525 38533
     38534 38538 38539 38541 38542 38548 38549 38552 38553 38555 38556
     38563 38563 38567 38568 38570 38570 38577 38577 38583 38583 38587
     38587 38592 38593 38596 38599 38601 38601 38603 38606 38613 38614
     38617 38617 38619 38620 38626 38627 38632 38634 38639 38640 38642
     38642 38646 38647 38649 38649 38651 38651 38656 38656 38662 38663
     38673 38673 38675 38675 38678 38678 38681 38681 38684 38684 38686
     38686 38695 38695 38704 38704 38706 38706 38713 38713 38717 38717
     38722 38722 38724 38724 38728 38728 38737 38737 38742 38742 38748
     38748 38750 38750 38753 38754 38761 38761 38765 38765 38772 38772
     38775 38775 38778 38778 38795 38795 38797 38797 38799 38799 38816
     38816 38824 38824 38827 38827 38829 38829 38854 38854 38859 38859
     38867 38867 38876 38876 38899 38899 38902 38902 38907 38907 38911
     38915 38917 38918 38920 38920 38922 38922 38924 38924 38928 38931
     38935 38936 38957 38957 38960 38960 38968 38969 38971 38971 38982
     38982 38988 38990 38996 38996 39000 39000 39002 39002 39006 39006
     39013 39013 39015 39015 39019 39019 39023 39023 39080 39080 39087
     39087 39089 39089 39108 39108 39111 39111 39131 39132 39135 39135
     39137 39138 39149 39151 39156 39156 39164 39166 39171 39171 39177
     39178 39180 39180 39184 39184 39187 39187 39192 39192 39198 39198
     39200 39200 39208 39208 39237 39237 39241 39241 39243 39245 39249
     39250 39252 39252 39255 39255 39318 39318 39321 39321 39325 39325
     39333 39333 39336 39336 39340 39342 39345 39345 39347 39348 39353
     39353 39361 39361 39376 39378 39381 39381 39385 39385 39389 39389
     39391 39391 39405 39405 39409 39409 39423 39423 39425 39425 39432
     39432 39438 39439 39449 39449 39467 39467 39472 39472 39478 39479
     39488 39488 39491 39491 39493 39493 39501 39501 39509 39509 39511
     39511 39514 39515 39519 39519 39522 39522 39525 39525 39529 39530
     39592 39592 39608 39608 39635 39636 39640 39640 39653 39653 39662
     39662 39706 39706 39719 39719 39722 39722 39729 39729 39740 39740
     39745 39749 39759 39759 39764 39764 39770 39770 39791 39791 39822
     39822 39825 39825 39839 39839 39851 39851 39854 39854 39881 39881
     39894 39894 39908 39908 39912 39912 39949 39949 39952 39952 39954
     39954 39957 39957 39973 39973 39986 39986 39995 39995 40007 40007
     40009 40009 40023 40023 40165 40165 40167 40167 40169 40169 40179
     40180 40182 40182 40201 40201 40219 40219 40230 40230 40232 40232
     40251 40251 40273 40273 40285 40285 40288 40289 40300 40300 40306
     40306 40361 40361 40367 40367 40372 40372 40388 40388 40407 40407
     40434 40434 40440 40442 40474 40474 40478 40478 40565 40565 40569
     40569 40573 40573 40575 40575 40594 40595 40599 40599 40605 40605
     40607 40607 40613 40613 40628 40629 40635 40635 40638 40638 40643
     40643 40653 40654 40657 40657 40660 40660 40664 40664 40667 40668
     40670 40670 40680 40680 40692 40692 40711 40712 40718 40718 40723
     40723 40736 40736 40763 40763 40778 40779 40782 40782 40786 40786
     40799 40799 40801 40801 40807 40807 40810 40810 40812 40812 40823
     40823 40845 40845 40848 40848 40853 40853 40860 40860 44032 55203
     63744 64011 65281 65374 65504 65507 65509 65510 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 2082 :NAME "VISCII" :ALIASES
   '("csVISCII") :MIME-ENCODING 'NIL :SOURCE '"RFC 1456" :COMMENTS 'NIL
   :REFERENCES '("[RFC1456]") :RANGES
   #(0 1 3 4 7 19 21 24 26 29 31 127 192 195 200 202 204 205 210 213 217
     218 221 221 224 227 232 234 236 237 242 245 249 250 253 253 258 259
     272 273 296 297 360 361 416 417 431 432 7840 7929 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 2084 :NAME #2="KOI8-R" :ALIASES
   '("csKOI8R") :MIME-ENCODING '#2# :SOURCE
   '"RFC 1489, based on GOST-19768-74, ISO-6937/8,  INIS-Cyrillic, ISO-5427."
   :COMMENTS 'NIL :REFERENCES '("[RFC1489]") :RANGES
   #(0 127 160 160 169 169 176 176 178 178 183 183 247 247 1025 1025
     1040 1103 1105 1105 8729 8730 8776 8776 8804 8805 8992 8993 9472
     9472 9474 9474 9484 9484 9488 9488 9492 9492 9496 9496 9500 9500
     9508 9508 9516 9516 9524 9524 9532 9532 9552 9580 9600 9600 9604
     9604 9608 9608 9612 9612 9616 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 2011 :NAME "IBM437" :ALIASES
   '("csPC8CodePage437" "437" "cp437") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 163 165 165 170 172 176 178 183 183 186 189 191 191 196
     199 201 201 209 209 214 214 220 220 223 226 228 239 241 244 246 247
     249 252 255 255 402 402 915 915 920 920 931 931 934 934 937 937 945
     945 948 949 956 956 960 960 963 964 966 966 8319 8319 8359 8359
     8729 8730 8734 8734 8745 8745 8776 8776 8801 8801 8804 8805 8976
     8976 8992 8993 9472 9472 9474 9474 9484 9484 9488 9488 9492 9492
     9496 9496 9500 9500 9508 9508 9516 9516 9524 9524 9532 9532 9552
     9580 9600 9600 9604 9604 9608 9608 9612 9612 9616 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 113 :NAME "GBK" :ALIASES
   '("windows-936" "MS936" "CP936") :MIME-ENCODING 'NIL :SOURCE
   '"Chinese IT Standardization Technical Committee   Please see: <http://www.iana.org/assignments/charset-reg/GBK>"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 164 164 167 168 176 177 183 183 215 215 224 225 232 234 236
     237 242 243 247 247 249 250 252 252 257 257 275 275 283 283 299 299
     324 324 328 328 333 333 363 363 462 462 464 464 466 466 468 468 470
     470 472 472 474 474 476 476 593 593 609 609 711 711 713 715 729 729
     913 929 931 937 945 961 963 969 1025 1025 1040 1103 1105 1105 8208
     8208 8211 8214 8216 8217 8220 8221 8229 8230 8240 8240 8242 8243
     8245 8245 8251 8251 8451 8451 8453 8453 8457 8457 8470 8470 8481
     8481 8544 8555 8560 8569 8592 8595 8598 8601 8712 8712 8719 8719
     8721 8721 8725 8725 8730 8730 8733 8736 8739 8739 8741 8741 8743
     8747 8750 8750 8756 8759 8765 8765 8776 8776 8780 8780 8786 8786
     8800 8801 8804 8807 8814 8815 8853 8853 8857 8857 8869 8869 8895
     8895 8978 8978 9312 9321 9332 9371 9472 9547 9552 9587 9601 9615
     9619 9621 9632 9633 9650 9651 9660 9661 9670 9671 9675 9675 9678
     9679 9698 9701 9733 9734 9737 9737 9792 9792 9794 9794 12288 12291
     12293 12311 12317 12318 12321 12329 12353 12435 12443 12446 12449
     12534 12540 12542 12549 12585 12832 12841 12849 12849 12963 12963
     13198 13199 13212 13214 13217 13217 13252 13252 13262 13262 13265
     13266 13269 13269 19968 40869 63788 63788 63865 63865 63893 63893
     63975 63975 63985 63985 64012 64015 64017 64017 64019 64020 64024
     64024 64031 64033 64035 64036 64039 64041 65072 65073 65075 65092
     65097 65106 65108 65111 65113 65126 65128 65131 65281 65374 65504
     65509 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 1014 :NAME "UTF-16LE" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE '"RFC 2781" :COMMENTS 'NIL :REFERENCES
   '("[RFC2781]") :RANGES #(0 65535))
  (MAKE-CHARACTER-SET :MIB-ENUM 114 :NAME "GB18030" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Chinese IT Standardization Technical Committee Please see: <http://www.iana.org/assignments/charset-reg/GB18030>"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 544 546 563 592 685 688 750 768 847 864 879 884 885 890 890 894
     894 900 906 908 908 910 929 931 974 976 1014 1024 1158 1160 1230
     1232 1269 1272 1273 1280 1295 1329 1366 1369 1375 1377 1415 1417
     1418 1425 1441 1443 1465 1467 1476 1488 1514 1520 1524 1548 1548
     1563 1563 1567 1567 1569 1594 1600 1621 1632 1773 1776 1790 1792
     1805 1807 1836 1840 1866 1920 1969 2305 2307 2309 2361 2364 2381
     2384 2388 2392 2416 2433 2435 2437 2444 2447 2448 2451 2472 2474
     2480 2482 2482 2486 2489 2492 2492 2494 2500 2503 2504 2507 2509
     2519 2519 2524 2525 2527 2531 2534 2554 2562 2562 2565 2570 2575
     2576 2579 2600 2602 2608 2610 2611 2613 2614 2616 2617 2620 2620
     2622 2626 2631 2632 2635 2637 2649 2652 2654 2654 2662 2676 2689
     2691 2693 2699 2701 2701 2703 2705 2707 2728 2730 2736 2738 2739
     2741 2745 2748 2757 2759 2761 2763 2765 2768 2768 2784 2784 2790
     2799 2817 2819 2821 2828 2831 2832 2835 2856 2858 2864 2866 2867
     2870 2873 2876 2883 2887 2888 2891 2893 2902 2903 2908 2909 2911
     2913 2918 2928 2946 2947 2949 2954 2958 2960 2962 2965 2969 2970
     2972 2972 2974 2975 2979 2980 2984 2986 2990 2997 2999 3001 3006
     3010 3014 3016 3018 3021 3031 3031 3047 3058 3073 3075 3077 3084
     3086 3088 3090 3112 3114 3123 3125 3129 3134 3140 3142 3144 3146
     3149 3157 3158 3168 3169 3174 3183 3202 3203 3205 3212 3214 3216
     3218 3240 3242 3251 3253 3257 3262 3268 3270 3272 3274 3277 3285
     3286 3294 3294 3296 3297 3302 3311 3330 3331 3333 3340 3342 3344
     3346 3368 3370 3385 3390 3395 3398 3400 3402 3405 3415 3415 3424
     3425 3430 3439 3458 3459 3461 3478 3482 3505 3507 3515 3517 3517
     3520 3526 3530 3530 3535 3540 3542 3542 3544 3551 3570 3572 3585
     3642 3647 3675 3713 3714 3716 3716 3719 3720 3722 3722 3725 3725
     3732 3735 3737 3743 3745 3747 3749 3749 3751 3751 3754 3755 3757
     3769 3771 3773 3776 3780 3782 3782 3784 3789 3792 3801 3804 3805
     3840 3911 3913 3946 3953 3979 3984 3991 3993 4028 4030 4044 4047
     4047 4096 4129 4131 4135 4137 4138 4140 4146 4150 4153 4160 4185
     4256 4293 4304 4344 4347 4347 4352 4441 4447 4514 4520 4601 4608
     4614 4616 4678 4680 4680 4682 4685 4688 4694 4696 4696 4698 4701
     4704 4742 4744 4744 4746 4749 4752 4782 4784 4784 4786 4789 4792
     4798 4800 4800 4802 4805 4808 4814 4816 4822 4824 4846 4848 4878
     4880 4880 4882 4885 4888 4894 4896 4934 4936 4954 4961 4988 5024
     5108 5121 5750 5760 5788 5792 5872 5888 5900 5902 5908 5920 5942
     5952 5971 5984 5996 5998 6000 6002 6003 6016 6108 6112 6121 6144
     6158 6160 6169 6176 6263 6272 6313 7680 7835 7840 7929 7936 7957
     7960 7965 7968 8005 8008 8013 8016 8023 8025 8025 8027 8027 8029
     8029 8031 8061 8064 8116 8118 8132 8134 8147 8150 8155 8157 8175
     8178 8180 8182 8190 8192 8274 8279 8279 8287 8291 8298 8305 8308
     8334 8352 8369 8400 8426 8448 8506 8509 8523 8531 8579 8592 9166
     9216 9254 9280 9290 9312 9470 9472 9747 9750 9751 9753 9853 9856
     9865 9985 9988 9990 9993 9996 10023 10025 10059 10061 10061 10063
     10066 10070 10070 10072 10078 10081 10132 10136 10159 10161 10174
     10192 10219 10224 11007 11904 11929 11931 12019 12032 12245 12272
     12283 12288 12351 12353 12438 12441 12543 12549 12588 12593 12686
     12688 12727 12784 12828 12832 12867 12881 12923 12927 13003 13008
     13054 13056 13174 13179 13277 13280 13310 13312 19893 19968 40869
     40960 42124 42128 42182 44032 55203 57344 64045 64048 64106 64256
     64262 64275 64279 64285 64310 64312 64316 64318 64318 64320 64321
     64323 64324 64326 64433 64467 64831 64848 64911 64914 64967 65008
     65020 65024 65039 65056 65059 65072 65094 65097 65106 65108 65126
     65128 65131 65136 65140 65142 65276 65279 65279 65281 65470 65474
     65479 65482 65487 65490 65495 65498 65500 65504 65510 65512 65518
     65529 65533))
  (MAKE-CHARACTER-SET :MIB-ENUM 2048 :NAME "IBM860" :ALIASES
   '("csIBM860" "860" "cp860") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 163 170 172 176 178 183 183 186 189 191 195 199 202 204
     205 209 213 217 218 220 220 223 227 231 234 236 237 241 245 247 247
     249 250 252 252 915 915 920 920 931 931 934 934 937 937 945 945 948
     949 956 956 960 960 963 964 966 966 8319 8319 8359 8359 8729 8730
     8734 8734 8745 8745 8776 8776 8801 8801 8804 8805 8992 8993 9472
     9472 9474 9474 9484 9484 9488 9488 9492 9492 9496 9496 9500 9500
     9508 9508 9516 9516 9524 9524 9532 9532 9552 9580 9600 9600 9604
     9604 9608 9608 9612 9612 9616 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 1019 :NAME "UTF-32LE" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"<http://www.unicode.org/unicode/reports/tr19/>" :COMMENTS 'NIL
   :REFERENCES '("[Davis]") :RANGES #(0 1114111))
  (MAKE-CHARACTER-SET :MIB-ENUM 1001 :NAME "ISO-10646-UCS-4" :ALIASES
   '("csUCS4") :MIME-ENCODING 'NIL :SOURCE
   '"the full code space. (same comment about byte order, these are 31-bit numbers."
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES #(0 1114111))
  (MAKE-CHARACTER-SET :MIB-ENUM 2087 :NAME "IBM775" :ALIASES
   '("csPC775Baltic" "cp775") :MIME-ENCODING 'NIL :SOURCE
   '"HP PCL 5 Comparison Guide (P/N 5021-0329) pp B-13, 1996" :COMMENTS
   'NIL :REFERENCES '("[HP-PCL5]") :RANGES
   #(0 127 160 160 162 164 166 167 169 169 171 174 176 179 181 183 185
     185 187 190 196 198 201 201 211 211 213 216 220 220 223 223 228 230
     233 233 243 243 245 248 252 252 256 257 260 263 268 269 274 275 278
     281 290 291 298 299 302 303 310 311 315 316 321 326 332 333 342 343
     346 347 352 353 362 363 370 371 377 382 8217 8217 8220 8222 8729
     8729 9472 9472 9474 9474 9484 9484 9488 9488 9492 9492 9496 9496
     9500 9500 9508 9508 9516 9516 9524 9524 9532 9532 9552 9553 9556
     9556 9559 9559 9562 9562 9565 9565 9568 9568 9571 9571 9574 9574
     9577 9577 9580 9580 9600 9600 9604 9604 9608 9608 9612 9612 9616
     9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 1013 :NAME "UTF-16BE" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE '"RFC 2781" :COMMENTS 'NIL :REFERENCES
   '("[RFC2781]") :RANGES #(0 65535))
  (MAKE-CHARACTER-SET :MIB-ENUM 11 :NAME "ISO_8859-8:1988" :ALIASES
   '("csISOLatinHebrew" "hebrew" #3="ISO-8859-8" "ISO_8859-8"
     "iso-ir-138")
   :MIME-ENCODING '#3# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: ISO-8859-8 (preferred MIME name)") :REFERENCES
   '("[RFC1345,KXS2]") :RANGES
   #(0 160 162 169 171 185 187 190 215 215 247 247 1488 1514 8206 8207
     8215 8215))
  (MAKE-CHARACTER-SET :MIB-ENUM 2004 :NAME "hp-roman8" :ALIASES
   '("csHPRoman8" "r8" "roman8") :MIME-ENCODING 'NIL :SOURCE
   '"LaserJet IIP Printer User's Manual,  HP part no 33471-90901, Hewlet-Packard, June 1989."
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5,RFC1345,KXS2]") :RANGES
   #(0 165 167 168 170 171 175 177 180 183 186 214 216 246 248 255 352
     353 376 376 402 402 710 710 715 715 732 732 8212 8212 8356 8356
     9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 2010 :NAME "IBM852" :ALIASES
   '("csPCp852" "852" "cp852") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 160 164 164 167 168 171 173 176 176 180 180 184 184 187
     187 193 194 196 196 199 199 201 201 203 203 205 206 211 212 214 215
     218 218 220 221 223 223 225 226 228 228 231 231 233 233 235 235 237
     238 243 244 246 247 250 250 252 253 258 263 268 273 280 283 313 314
     317 318 321 324 327 328 336 337 340 341 344 347 350 357 366 369 377
     382 711 711 728 729 731 731 733 733 9472 9472 9474 9474 9484 9484
     9488 9488 9492 9492 9496 9496 9500 9500 9508 9508 9516 9516 9524
     9524 9532 9532 9552 9553 9556 9556 9559 9559 9562 9562 9565 9565
     9568 9568 9571 9571 9574 9574 9577 9577 9580 9580 9600 9600 9604
     9604 9608 9608 9617 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 12 :NAME "ISO_8859-9:1989" :ALIASES
   '("csISOLatin5" "l5" "latin5" #4="ISO-8859-9" "ISO_8859-9"
     "iso-ir-148")
   :MIME-ENCODING '#4# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: ISO-8859-9 (preferred MIME name)") :REFERENCES
   '("[RFC1345,KXS2]") :RANGES
   #(0 207 209 220 223 239 241 252 255 255 286 287 304 305 350 351))
  (MAKE-CHARACTER-SET :MIB-ENUM 2054 :NAME "IBM869" :ALIASES
   '("csIBM869" "cp-gr" "869" "cp869") :MIME-ENCODING 'NIL :SOURCE
   '"IBM Keyboard layouts and code pages, PN 07G4586 June 1991"
   :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 160 163 163 166 169 171 173 176 180 187 187 189 189 901
     906 908 908 910 929 931 974 8213 8213 8216 8217 9472 9472 9474 9474
     9484 9484 9488 9488 9492 9492 9496 9496 9500 9500 9508 9508 9516
     9516 9524 9524 9532 9532 9552 9553 9556 9556 9559 9559 9562 9562
     9565 9565 9568 9568 9571 9571 9574 9574 9577 9577 9580 9580 9600
     9600 9604 9604 9608 9608 9617 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 39 :NAME #5="ISO-2022-JP" :ALIASES
   '("csISO2022JP") :MIME-ENCODING '#5# :SOURCE
   '"RFC-1468 (see also RFC-2237)" :COMMENTS 'NIL :REFERENCES
   '("[RFC1468,Murai]") :RANGES
   #(0 127 162 163 165 165 167 168 172 172 176 177 180 180 182 182 215
     215 247 247 913 929 931 937 945 961 963 969 1025 1025 1040 1103
     1105 1105 8208 8208 8213 8214 8216 8217 8220 8221 8224 8225 8229
     8230 8240 8240 8242 8243 8251 8251 8254 8254 8451 8451 8491 8491
     8592 8595 8658 8658 8660 8660 8704 8704 8706 8707 8711 8712 8715
     8715 8722 8722 8730 8730 8733 8734 8736 8736 8743 8748 8756 8757
     8765 8765 8786 8786 8800 8801 8806 8807 8810 8811 8834 8835 8838
     8839 8869 8869 8978 8978 9472 9475 9484 9484 9487 9488 9491 9492
     9495 9496 9499 9501 9504 9504 9507 9509 9512 9512 9515 9516 9519
     9520 9523 9524 9527 9528 9531 9532 9535 9535 9538 9538 9547 9547
     9632 9633 9650 9651 9660 9661 9670 9671 9675 9675 9678 9679 9711
     9711 9733 9734 9792 9792 9794 9794 9834 9834 9837 9837 9839 9839
     12288 12291 12293 12309 12316 12316 12353 12435 12443 12446 12449
     12534 12539 12542 19968 19969 19971 19971 19975 19979 19981 19982
     19984 19985 19988 19993 19998 19998 20001 20001 20006 20006 20010
     20010 20013 20013 20017 20018 20022 20022 20024 20025 20027 20028
     20031 20031 20034 20035 20037 20037 20043 20043 20045 20047 20053
     20057 20061 20063 20066 20066 20081 20081 20083 20083 20094 20094
     20096 20096 20098 20098 20101 20102 20104 20108 20110 20110 20113
     20114 20116 20117 20120 20121 20123 20124 20126 20130 20132 20134
     20136 20136 20139 20142 20144 20144 20147 20147 20150 20150 20154
     20154 20160 20162 20164 20164 20166 20167 20170 20171 20173 20175
     20180 20185 20189 20191 20195 20197 20205 20206 20208 20208 20210
     20210 20214 20215 20219 20219 20225 20225 20233 20234 20237 20241
     20250 20250 20252 20253 20271 20272 20276 20276 20278 20278 20280
     20280 20282 20282 20284 20285 20291 20291 20294 20295 20301 20305
     20307 20307 20309 20309 20311 20311 20313 20318 20329 20329 20335
     20336 20339 20339 20341 20342 20347 20348 20351 20351 20355 20355
     20358 20358 20360 20360 20363 20363 20365 20365 20367 20367 20369
     20369 20374 20374 20376 20376 20379 20379 20381 20381 20384 20385
     20395 20395 20397 20399 20405 20406 20415 20415 20418 20420 20426
     20426 20430 20430 20432 20433 20436 20436 20439 20440 20442 20443
     20445 20445 20447 20447 20449 20449 20451 20453 20462 20463 20467
     20467 20469 20470 20472 20472 20474 20474 20478 20478 20485 20486
     20489 20489 20491 20491 20493 20493 20495 20495 20497 20498 20500
     20500 20502 20502 20505 20506 20511 20511 20513 20513 20515 20518
     20520 20525 20534 20534 20537 20537 20547 20547 20551 20553 20559
     20560 20565 20566 20570 20570 20572 20572 20581 20581 20588 20588
     20594 20594 20596 20598 20600 20600 20605 20605 20608 20608 20613
     20613 20621 20621 20625 20625 20632 20634 20652 20653 20658 20661
     20663 20663 20670 20670 20674 20674 20677 20677 20681 20682 20685
     20685 20687 20687 20689 20689 20693 20694 20698 20698 20702 20702
     20707 20707 20709 20709 20711 20711 20717 20718 20725 20725 20729
     20729 20731 20731 20736 20738 20740 20740 20745 20745 20754 20754
     20756 20758 20760 20760 20762 20762 20767 20767 20769 20769 20778
     20778 20786 20786 20791 20791 20794 20796 20799 20801 20803 20809
     20811 20814 20816 20816 20818 20818 20820 20820 20826 20826 20828
     20828 20834 20834 20837 20837 20840 20846 20849 20849 20853 20856
     20860 20860 20864 20864 20866 20866 20869 20870 20873 20874 20876
     20877 20879 20883 20885 20887 20889 20889 20896 20896 20898 20898
     20900 20902 20904 20908 20912 20919 20925 20925 20932 20934 20937
     20937 20939 20941 20950 20950 20955 20957 20960 20961 20966 20967
     20969 20970 20973 20973 20976 20977 20981 20982 20984 20986 20989
     20990 20992 20992 20995 20996 20998 21000 21002 21003 21006 21006
     21009 21009 21012 21012 21015 21015 21021 21021 21028 21029 21031
     21031 21033 21034 21038 21038 21040 21040 21043 21043 21046 21051
     21059 21060 21063 21063 21066 21069 21071 21071 21076 21076 21078
     21078 21083 21083 21086 21086 21091 21093 21097 21098 21103 21109
     21117 21117 21119 21119 21123 21123 21127 21129 21133 21133 21137
     21138 21140 21140 21147 21147 21151 21152 21155 21155 21161 21165
     21169 21169 21172 21173 21177 21177 21180 21180 21182 21182 21185
     21185 21187 21187 21189 21189 21191 21191 21193 21193 21197 21197
     21202 21202 21205 21205 21207 21209 21213 21216 21218 21220 21222
     21223 21234 21235 21237 21237 21240 21242 21246 21247 21249 21250
     21253 21254 21256 21256 21261 21261 21263 21264 21269 21271 21273
     21274 21277 21277 21280 21281 21283 21283 21290 21290 21295 21295
     21297 21297 21299 21299 21304 21307 21311 21313 21315 21315 21317
     21322 21325 21325 21329 21332 21335 21336 21338 21338 21340 21340
     21342 21342 21344 21344 21350 21350 21353 21353 21358 21361 21363
     21365 21367 21368 21371 21371 21375 21375 21378 21378 21380 21380
     21398 21398 21400 21400 21402 21402 21407 21408 21413 21414 21416
     21417 21421 21422 21424 21424 21427 21427 21430 21430 21435 21435
     21442 21443 21448 21454 21460 21460 21462 21463 21465 21465 21467
     21467 21471 21471 21473 21477 21480 21491 21494 21496 21498 21498
     21505 21505 21507 21508 21512 21521 21531 21531 21533 21533 21535
     21536 21542 21542 21545 21545 21547 21550 21558 21558 21560 21561
     21563 21566 21568 21568 21570 21570 21574 21574 21576 21578 21582
     21582 21585 21585 21599 21599 21608 21608 21610 21610 21616 21617
     21619 21619 21621 21623 21627 21629 21632 21632 21636 21636 21638
     21638 21643 21644 21646 21648 21650 21650 21666 21666 21668 21669
     21672 21672 21675 21676 21679 21679 21682 21683 21688 21688 21692
     21694 21696 21698 21700 21700 21703 21705 21720 21720 21729 21730
     21733 21734 21736 21737 21741 21742 21746 21746 21754 21754 21757
     21757 21764 21764 21766 21767 21775 21776 21780 21780 21782 21782
     21806 21807 21809 21809 21811 21811 21816 21817 21822 21822 21824
     21824 21828 21830 21836 21836 21839 21839 21843 21843 21846 21847
     21852 21853 21859 21859 21883 21884 21886 21886 21888 21888 21891
     21892 21895 21895 21897 21899 21912 21914 21916 21919 21927 21932
     21934 21934 21936 21936 21942 21942 21956 21957 21959 21959 21972
     21972 21978 21978 21980 21980 21983 21983 21987 21988 22007 22007
     22009 22009 22013 22014 22022 22022 22025 22025 22036 22036 22038
     22040 22043 22043 22057 22057 22063 22063 22065 22066 22068 22068
     22070 22070 22072 22072 22082 22082 22092 22092 22094 22094 22096
     22096 22107 22107 22116 22116 22120 22120 22122 22124 22132 22132
     22136 22136 22138 22138 22144 22144 22150 22151 22154 22154 22159
     22159 22164 22164 22176 22176 22178 22178 22181 22181 22190 22190
     22196 22196 22198 22198 22204 22204 22208 22211 22216 22216 22222
     22222 22225 22225 22227 22227 22231 22232 22234 22235 22238 22238
     22240 22240 22243 22243 22254 22254 22256 22256 22258 22259 22265
     22266 22269 22269 22271 22272 22275 22276 22280 22281 22283 22283
     22285 22285 22287 22287 22290 22291 22294 22294 22296 22296 22300
     22300 22303 22303 22310 22312 22317 22317 22320 22320 22327 22328
     22331 22331 22336 22336 22338 22338 22343 22343 22346 22346 22350
     22353 22369 22369 22372 22372 22374 22374 22377 22378 22399 22399
     22402 22402 22408 22409 22411 22411 22419 22419 22432 22432 22434
     22436 22442 22442 22448 22448 22451 22451 22464 22464 22467 22467
     22470 22470 22475 22475 22478 22478 22482 22484 22486 22486 22492
     22492 22495 22496 22499 22499 22516 22516 22519 22519 22521 22522
     22524 22524 22528 22528 22530 22530 22533 22534 22538 22539 22549
     22549 22553 22553 22557 22557 22561 22561 22564 22564 22570 22570
     22575 22577 22580 22581 22586 22586 22589 22589 22592 22593 22602
     22603 22609 22610 22612 22612 22615 22618 22622 22622 22626 22626
     22633 22633 22635 22635 22640 22640 22642 22642 22645 22645 22649
     22649 22654 22654 22659 22659 22661 22661 22675 22675 22679 22679
     22684 22684 22687 22687 22696 22696 22699 22699 22702 22702 22707
     22707 22712 22715 22718 22718 22721 22721 22725 22725 22727 22727
     22730 22730 22732 22732 22737 22737 22739 22739 22741 22741 22743
     22745 22748 22748 22750 22751 22756 22757 22763 22764 22766 22770
     22775 22775 22777 22781 22786 22786 22793 22794 22799 22800 22805
     22806 22808 22812 22818 22818 22821 22821 22823 22823 22825 22830
     22833 22834 22839 22840 22846 22846 22852 22852 22855 22857 22862
     22865 22868 22869 22871 22872 22874 22874 22880 22880 22882 22882
     22885 22885 22887 22890 22892 22892 22894 22894 22899 22900 22904
     22904 22909 22909 22913 22916 22922 22922 22925 22925 22931 22931
     22934 22934 22937 22937 22939 22939 22941 22941 22947 22947 22949
     22949 22952 22952 22956 22956 22962 22962 22969 22969 22971 22971
     22974 22974 22982 22982 22985 22985 22987 22987 22992 22993 22995
     22996 23001 23002 23004 23004 23013 23014 23016 23016 23018 23019
     23030 23030 23035 23035 23039 23039 23041 23041 23043 23043 23049
     23049 23057 23057 23064 23064 23066 23066 23068 23068 23071 23072
     23077 23077 23081 23081 23087 23087 23093 23094 23100 23100 23104
     23105 23110 23110 23113 23113 23130 23130 23138 23138 23142 23142
     23146 23146 23148 23148 23167 23167 23186 23186 23194 23195 23228
     23230 23233 23234 23241 23241 23243 23244 23248 23248 23254 23255
     23265 23265 23267 23267 23270 23270 23273 23273 23290 23291 23305
     23305 23307 23308 23318 23318 23330 23330 23338 23338 23340 23340
     23344 23344 23346 23346 23350 23350 23358 23358 23360 23360 23363
     23363 23365 23365 23376 23377 23380 23381 23383 23384 23386 23389
     23391 23391 23395 23398 23401 23401 23403 23403 23408 23409 23411
     23411 23413 23413 23416 23416 23418 23418 23424 23424 23427 23427
     23429 23429 23431 23433 23435 23437 23439 23439 23445 23445 23447
     23453 23455 23455 23458 23462 23470 23470 23472 23472 23475 23478
     23480 23481 23487 23487 23490 23495 23497 23497 23500 23500 23504
     23504 23506 23508 23515 23515 23517 23519 23521 23522 23524 23529
     23531 23531 23534 23534 23536 23536 23539 23539 23541 23542 23544
     23544 23546 23546 23550 23551 23553 23554 23556 23563 23565 23567
     23569 23569 23571 23571 23574 23574 23578 23578 23584 23584 23586
     23586 23588 23588 23592 23592 23597 23597 23601 23601 23608 23617
     23621 23622 23624 23624 23626 23627 23629 23633 23635 23635 23637
     23637 23646 23646 23648 23649 23652 23653 23660 23660 23662 23663
     23665 23665 23670 23670 23673 23673 23692 23692 23696 23697 23700
     23700 23713 23713 23720 23721 23723 23724 23729 23729 23731 23731
     23734 23736 23739 23740 23742 23742 23749 23749 23751 23751 23769
     23769 23776 23777 23784 23786 23789 23789 23791 23792 23798 23798
     23802 23803 23805 23805 23815 23815 23819 23819 23822 23822 23825
     23825 23828 23835 23839 23839 23842 23842 23849 23849 23883 23884
     23886 23886 23888 23888 23890 23890 23900 23900 23913 23913 23916
     23916 23919 23919 23923 23923 23926 23926 23938 23938 23940 23940
     23943 23943 23947 23948 23952 23952 23965 23965 23970 23970 23980
     23980 23982 23982 23991 23991 23994 23994 23996 23997 24009 24009
     24012 24013 24018 24019 24022 24022 24027 24027 24029 24030 24033
     24033 24035 24035 24037 24040 24043 24043 24046 24046 24049 24053
     24055 24055 24059 24059 24061 24062 24066 24067 24070 24070 24075
     24076 24081 24081 24086 24086 24089 24091 24093 24093 24101 24101
     24107 24107 24109 24109 24111 24112 24115 24115 24118 24120 24125
     24125 24128 24128 24131 24133 24135 24135 24140 24140 24142 24142
     24148 24149 24151 24151 24159 24159 24161 24164 24178 24182 24184
     24191 24193 24193 24195 24196 24199 24199 24202 24202 24207 24207
     24213 24215 24218 24218 24220 24220 24224 24224 24230 24231 24235
     24235 24237 24237 24245 24248 24257 24259 24264 24266 24271 24272
     24275 24275 24278 24278 24282 24283 24285 24285 24287 24291 24296
     24297 24300 24300 24304 24305 24307 24308 24310 24312 24314 24316
     24318 24319 24321 24321 24323 24324 24329 24333 24335 24337 24339
     24344 24347 24347 24351 24351 24357 24359 24361 24361 24365 24365
     24367 24367 24369 24369 24373 24373 24375 24376 24380 24380 24382
     24382 24385 24385 24392 24392 24394 24394 24396 24396 24398 24398
     24401 24401 24403 24403 24406 24407 24409 24409 24412 24413 24417
     24418 24422 24422 24425 24429 24432 24433 24435 24435 24439 24439
     24441 24441 24444 24444 24447 24453 24455 24456 24458 24460 24464
     24467 24471 24473 24478 24478 24480 24481 24488 24490 24493 24494
     24499 24500 24505 24505 24508 24509 24515 24515 24517 24517 24524
     24525 24534 24537 24540 24541 24544 24544 24548 24548 24555 24555
     24560 24561 24565 24565 24568 24568 24571 24571 24573 24573 24575
     24575 24590 24592 24594 24594 24597 24598 24601 24601 24603 24605
     24608 24609 24613 24619 24623 24623 24625 24625 24634 24634 24641
     24643 24646 24646 24650 24651 24653 24653 24656 24656 24658 24658
     24661 24661 24665 24666 24671 24672 24674 24677 24680 24685 24687
     24688 24693 24693 24695 24695 24705 24705 24707 24708 24713 24713
     24715 24717 24722 24722 24724 24724 24726 24727 24730 24731 24735
     24736 24739 24739 24742 24743 24745 24746 24754 24758 24760 24760
     24764 24765 24773 24775 24785 24785 24787 24787 24792 24792 24794
     24794 24796 24796 24799 24801 24803 24803 24807 24808 24816 24817
     24819 24820 24822 24823 24825 24827 24832 24833 24835 24835 24838
     24838 24840 24841 24845 24847 24853 24853 24858 24859 24863 24863
     24865 24865 24871 24872 24876 24876 24884 24884 24892 24895 24898
     24898 24900 24900 24903 24904 24906 24910 24915 24915 24917 24917
     24920 24922 24925 24925 24927 24927 24930 24931 24933 24933 24935
     24936 24939 24939 24942 24945 24947 24951 24958 24958 24962 24962
     24967 24967 24970 24970 24974 24974 24976 24977 24980 24980 24982
     24982 24985 24986 24996 24996 24999 24999 25001 25001 25003 25004
     25006 25006 25010 25010 25014 25014 25018 25018 25022 25022 25027
     25027 25030 25037 25040 25040 25059 25059 25062 25062 25074 25074
     25076 25076 25078 25080 25082 25082 25084 25088 25096 25098 25100
     25102 25104 25106 25108 25108 25110 25110 25114 25115 25117 25119
     25121 25121 25126 25126 25130 25130 25134 25136 25138 25140 25144
     25144 25147 25147 25151 25153 25159 25161 25163 25163 25165 25166
     25171 25171 25173 25173 25176 25176 25179 25179 25182 25182 25184
     25184 25187 25187 25192 25192 25198 25198 25201 25201 25206 25206
     25209 25209 25212 25212 25214 25216 25218 25220 25225 25226 25233
     25240 25243 25244 25246 25246 25259 25260 25265 25265 25269 25269
     25273 25273 25275 25277 25282 25282 25285 25290 25292 25293 25295
     25300 25303 25305 25307 25309 25312 25313 25324 25327 25329 25329
     25331 25331 25333 25335 25342 25343 25345 25346 25351 25353 25356
     25356 25361 25361 25369 25369 25375 25375 25383 25384 25387 25387
     25391 25391 25402 25402 25405 25407 25417 25417 25420 25421 25423
     25424 25429 25429 25431 25431 25436 25436 25447 25449 25451 25451
     25454 25454 25458 25458 25462 25463 25466 25467 25472 25472 25475
     25475 25480 25481 25484 25484 25486 25487 25490 25490 25494 25494
     25496 25496 25499 25499 25503 25507 25509 25509 25511 25516 25522
     25522 25524 25525 25531 25531 25534 25534 25536 25536 25539 25540
     25542 25542 25545 25545 25551 25552 25554 25554 25558 25558 25562
     25563 25569 25569 25571 25571 25577 25577 25582 25582 25588 25588
     25590 25590 25594 25594 25606 25606 25613 25613 25615 25615 25619
     25619 25622 25623 25628 25628 25638 25638 25640 25640 25644 25645
     25652 25652 25654 25654 25658 25658 25662 25662 25666 25666 25678
     25678 25688 25688 25703 25703 25705 25705 25711 25711 25718 25718
     25720 25720 25722 25722 25731 25731 25736 25736 25746 25747 25749
     25749 25754 25754 25758 25758 25764 25765 25769 25769 25771 25771
     25773 25774 25776 25776 25778 25778 25785 25785 25787 25788 25793
     25794 25797 25797 25799 25799 25805 25805 25810 25810 25812 25812
     25816 25816 25818 25818 25824 25827 25830 25831 25836 25836 25839
     25839 25841 25842 25844 25844 25846 25846 25850 25850 25853 25854
     25856 25856 25861 25861 25880 25880 25884 25885 25891 25892 25898
     25900 25903 25903 25908 25913 25915 25915 25918 25919 25925 25925
     25928 25928 25933 25933 25935 25935 25937 25937 25941 25945 25949
     25950 25954 25955 25958 25958 25964 25964 25968 25968 25970 25970
     25972 25973 25975 25976 25986 25987 25991 25993 25996 25996 25998
     25998 26000 26001 26007 26007 26009 26009 26011 26012 26015 26015
     26017 26017 26020 26021 26023 26023 26027 26029 26031 26032 26039
     26039 26041 26041 26044 26045 26049 26049 26051 26054 26059 26060
     26063 26063 26066 26066 26071 26071 26073 26073 26075 26075 26080
     26082 26085 26089 26092 26093 26097 26097 26106 26107 26114 26115
     26118 26119 26122 26122 26124 26124 26126 26127 26131 26132 26140
     26140 26143 26144 26149 26149 26151 26152 26157 26157 26159 26159
     26164 26166 26172 26172 26175 26175 26177 26180 26185 26185 26187
     26187 26191 26191 26194 26194 26205 26207 26210 26210 26212 26212
     26214 26217 26222 26224 26228 26228 26230 26230 26234 26234 26241
     26241 26243 26244 26247 26249 26254 26254 26257 26257 26262 26264
     26269 26269 26274 26274 26278 26278 26283 26283 26286 26286 26292
     26292 26296 26297 26300 26300 26302 26302 26305 26305 26308 26308
     26311 26311 26313 26313 26326 26326 26329 26330 26332 26333 26336
     26336 26342 26342 26345 26345 26352 26352 26354 26357 26359 26361
     26364 26368 26371 26371 26376 26377 26379 26379 26381 26381 26383
     26383 26388 26391 26395 26395 26397 26399 26406 26408 26410 26414
     26417 26417 26420 26420 26422 26424 26426 26426 26429 26429 26431
     26431 26433 26433 26438 26438 26441 26441 26446 26449 26451 26451
     26454 26454 26457 26457 26460 26460 26462 26469 26474 26474 26477
     26477 26479 26483 26485 26485 26487 26487 26492 26492 26494 26495
     26501 26501 26503 26503 26505 26505 26507 26508 26512 26512 26517
     26517 26519 26519 26522 26522 26524 26525 26528 26530 26534 26534
     26537 26537 26543 26543 26547 26548 26550 26553 26561 26561 26564
     26564 26566 26566 26570 26570 26574 26577 26579 26580 26584 26584
     26586 26586 26589 26590 26594 26594 26596 26596 26599 26599 26601
     26601 26604 26604 26606 26607 26609 26609 26611 26613 26619 26619
     26622 26623 26626 26628 26643 26643 26646 26647 26654 26654 26657
     26658 26665 26667 26674 26674 26676 26676 26680 26681 26684 26685
     26688 26691 26694 26694 26696 26696 26701 26702 26704 26705 26707
     26708 26713 26713 26716 26717 26719 26719 26723 26723 26727 26727
     26740 26740 26742 26743 26750 26751 26753 26753 26755 26755 26757
     26757 26765 26765 26767 26767 26771 26772 26775 26775 26779 26779
     26781 26781 26783 26784 26786 26786 26790 26792 26797 26797 26799
     26801 26803 26803 26805 26806 26809 26810 26812 26812 26820 26820
     26822 26822 26825 26827 26829 26829 26834 26834 26836 26837 26839
     26840 26842 26842 26847 26849 26851 26851 26855 26855 26862 26863
     26866 26866 26873 26874 26880 26881 26884 26885 26888 26888 26891
     26895 26898 26898 26905 26908 26913 26915 26917 26918 26920 26920
     26922 26922 26928 26928 26932 26932 26934 26934 26937 26937 26941
     26941 26943 26943 26954 26954 26963 26965 26969 26970 26972 26974
     26976 26978 26986 26987 26989 26991 26995 26997 26999 27001 27004
     27006 27009 27010 27018 27018 27022 27022 27025 27025 27028 27029
     27035 27036 27040 27040 27047 27047 27054 27054 27057 27058 27060
     27060 27067 27067 27070 27071 27073 27073 27075 27075 27079 27079
     27082 27086 27088 27088 27091 27091 27096 27097 27101 27102 27111
     27112 27115 27115 27117 27117 27122 27122 27129 27129 27131 27131
     27133 27133 27135 27135 27138 27138 27141 27141 27146 27148 27154
     27156 27159 27159 27161 27161 27163 27163 27166 27167 27169 27171
     27177 27179 27182 27182 27189 27190 27192 27194 27197 27197 27204
     27204 27207 27208 27211 27211 27224 27225 27231 27231 27233 27234
     27238 27238 27250 27250 27256 27256 27263 27264 27268 27268 27277
     27278 27280 27280 27287 27287 27292 27292 27296 27296 27298 27299
     27306 27306 27308 27308 27310 27310 27315 27315 27320 27320 27323
     27323 27329 27331 27345 27345 27347 27347 27354 27355 27358 27359
     27368 27368 27370 27370 27386 27387 27396 27397 27402 27402 27410
     27410 27414 27414 27421 27421 27423 27425 27427 27427 27431 27431
     27442 27442 27447 27450 27453 27454 27459 27459 27463 27463 27465
     27465 27468 27468 27470 27470 27472 27472 27475 27476 27481 27481
     27483 27483 27487 27487 27489 27492 27494 27494 27497 27498 27503
     27503 27507 27508 27512 27513 27515 27515 27519 27520 27523 27524
     27526 27526 27529 27531 27533 27533 27541 27542 27544 27544 27550
     27550 27556 27556 27562 27563 27567 27567 27569 27573 27575 27575
     27578 27580 27583 27584 27589 27590 27595 27595 27597 27598 27602
     27604 27608 27608 27611 27611 27615 27615 27627 27628 27631 27631
     27635 27635 27656 27656 27663 27663 27665 27665 27667 27668 27671
     27671 27675 27675 27683 27684 27700 27700 27703 27704 27710 27710
     27712 27714 27726 27726 27728 27728 27733 27733 27735 27735 27738
     27738 27741 27744 27746 27746 27752 27752 27754 27754 27760 27760
     27762 27763 27770 27770 27773 27774 27777 27779 27784 27784 27788
     27789 27792 27792 27794 27795 27798 27798 27801 27803 27809 27810
     27819 27819 27822 27822 27825 27825 27827 27827 27832 27839 27841
     27841 27844 27845 27849 27850 27852 27852 27859 27859 27861 27861
     27863 27863 27865 27865 27867 27867 27869 27869 27873 27875 27877
     27877 27880 27880 27882 27882 27887 27889 27891 27891 27915 27916
     27922 27922 27927 27927 27929 27929 27931 27931 27934 27935 27941
     27941 27945 27947 27954 27955 27957 27958 27960 27960 27963 27963
     27965 27966 27969 27969 27972 27973 27993 27994 27996 27996 28003
     28004 28006 28006 28009 28010 28012 28012 28014 28014 28020 28020
     28023 28025 28037 28037 28040 28040 28044 28044 28046 28046 28051
     28051 28053 28053 28057 28057 28059 28060 28079 28079 28082 28082
     28085 28085 28088 28088 28092 28092 28096 28096 28101 28103 28107
     28108 28113 28114 28117 28117 28120 28121 28126 28126 28129 28129
     28132 28132 28134 28134 28136 28136 28138 28140 28142 28142 28145
     28145 28147 28147 28149 28149 28151 28151 28153 28155 28165 28165
     28167 28171 28179 28179 28181 28181 28185 28187 28189 28189 28191
     28193 28195 28198 28201 28201 28203 28207 28216 28216 28218 28218
     28222 28222 28227 28227 28234 28234 28237 28238 28246 28246 28248
     28248 28251 28251 28255 28255 28263 28263 28267 28267 28270 28271
     28274 28274 28278 28278 28286 28288 28290 28290 28300 28300 28303
     28304 28310 28310 28312 28312 28316 28317 28319 28319 28322 28322
     28325 28325 28330 28330 28335 28335 28338 28338 28342 28343 28346
     28346 28349 28349 28354 28354 28356 28357 28361 28361 28363 28364
     28369 28369 28371 28373 28381 28382 28396 28396 28399 28399 28402
     28402 28404 28404 28407 28408 28414 28415 28417 28418 28422 28422
     28425 28425 28431 28431 28433 28433 28435 28437 28448 28448 28450
     28451 28459 28460 28465 28466 28472 28472 28478 28479 28481 28481
     28485 28485 28500 28500 28504 28504 28507 28508 28511 28511 28516
     28516 28518 28518 28525 28528 28532 28532 28536 28536 28538 28538
     28540 28540 28544 28546 28548 28548 28550 28550 28558 28558 28561
     28561 28567 28567 28577 28577 28579 28580 28586 28586 28593 28593
     28595 28595 28601 28601 28608 28611 28614 28614 28628 28629 28632
     28632 28635 28635 28639 28641 28644 28644 28651 28652 28654 28655
     28657 28657 28659 28659 28662 28662 28666 28666 28670 28670 28673
     28673 28681 28681 28683 28683 28687 28687 28689 28689 28693 28693
     28696 28696 28698 28699 28701 28703 28710 28711 28716 28716 28720
     28720 28722 28722 28734 28734 28748 28748 28753 28753 28760 28760
     28771 28771 28779 28779 28783 28784 28792 28792 28796 28797 28809
     28810 28814 28814 28818 28818 28825 28825 28844 28847 28851 28851
     28856 28858 28872 28872 28875 28875 28879 28879 28889 28889 28893
     28893 28895 28895 28913 28913 28921 28921 28925 28925 28937 28937
     28948 28948 28953 28954 28956 28956 28961 28961 28966 28966 28982
     28982 28988 28988 29001 29001 29004 29004 29006 29006 29013 29014
     29017 29017 29026 29026 29028 29031 29033 29033 29036 29036 29038
     29038 29053 29053 29060 29060 29064 29064 29066 29066 29071 29071
     29076 29077 29081 29081 29087 29087 29096 29096 29100 29100 29105
     29105 29113 29113 29118 29118 29123 29123 29128 29129 29134 29134
     29136 29136 29138 29138 29140 29141 29143 29143 29151 29152 29157
     29159 29164 29166 29173 29173 29177 29177 29179 29180 29183 29183
     29190 29190 29197 29197 29200 29200 29211 29211 29224 29224 29226
     29226 29228 29229 29232 29232 29234 29234 29237 29238 29242 29248
     29254 29256 29259 29260 29266 29266 29272 29273 29275 29275 29277
     29277 29279 29279 29281 29282 29287 29287 29289 29289 29298 29298
     29300 29300 29305 29305 29309 29310 29312 29314 29319 29319 29330
     29330 29334 29334 29344 29344 29346 29346 29351 29351 29356 29356
     29359 29359 29362 29362 29366 29366 29369 29369 29378 29380 29382
     29382 29390 29390 29392 29392 29394 29394 29399 29399 29401 29401
     29403 29403 29408 29410 29417 29417 29420 29421 29431 29433 29436
     29437 29450 29450 29462 29463 29467 29469 29471 29471 29477 29477
     29481 29483 29486 29487 29492 29492 29494 29495 29502 29503 29508
     29509 29518 29519 29527 29527 29539 29539 29544 29544 29546 29546
     29552 29552 29554 29554 29557 29557 29560 29560 29562 29563 29572
     29572 29575 29575 29577 29577 29579 29579 29590 29590 29609 29609
     29618 29619 29627 29627 29632 29632 29634 29634 29640 29640 29642
     29642 29645 29646 29662 29662 29664 29664 29669 29669 29674 29674
     29677 29678 29681 29681 29688 29688 29694 29694 29699 29699 29701
     29702 29705 29705 29730 29730 29733 29733 29746 29750 29754 29754
     29759 29759 29761 29761 29781 29781 29785 29788 29790 29792 29795
     29796 29801 29802 29807 29808 29811 29811 29814 29814 29822 29822
     29827 29827 29835 29835 29854 29854 29858 29858 29863 29863 29872
     29872 29885 29885 29898 29898 29903 29903 29908 29908 29916 29916
     29920 29920 29922 29923 29926 29927 29929 29929 29934 29934 29936
     29938 29942 29944 29955 29957 29964 29966 29969 29969 29971 29971
     29973 29973 29976 29976 29978 29978 29980 29980 29982 29983 29987
     29987 29989 29990 29992 29992 29995 29996 30000 30003 30007 30008
     30010 30012 30020 30020 30022 30022 30025 30029 30031 30031 30033
     30033 30036 30036 30041 30045 30048 30048 30050 30050 30052 30055
     30057 30059 30061 30061 30064 30064 30067 30068 30070 30072 30079
     30079 30082 30082 30086 30087 30089 30091 30094 30095 30097 30097
     30100 30100 30106 30106 30109 30109 30115 30115 30117 30117 30123
     30123 30129 30131 30133 30133 30136 30137 30140 30142 30146 30147
     30149 30149 30151 30151 30154 30154 30157 30157 30162 30162 30164
     30165 30168 30169 30171 30171 30174 30174 30178 30179 30185 30185
     30192 30192 30194 30196 30202 30202 30204 30204 30206 30207 30209
     30209 30217 30217 30219 30219 30221 30221 30239 30242 30244 30244
     30247 30247 30256 30256 30260 30260 30267 30267 30274 30274 30278
     30280 30284 30284 30290 30290 30294 30294 30296 30296 30300 30300
     30305 30306 30311 30314 30316 30316 30320 30320 30322 30322 30326
     30326 30328 30328 30330 30334 30336 30336 30339 30340 30342 30344
     30347 30347 30350 30350 30352 30352 30355 30355 30358 30358 30361
     30362 30382 30382 30384 30384 30388 30388 30391 30394 30399 30399
     30402 30403 30406 30406 30408 30408 30410 30410 30413 30413 30418
     30418 30422 30423 30427 30428 30430 30431 30433 30433 30435 30437
     30439 30439 30442 30442 30446 30446 30450 30450 30452 30452 30456
     30456 30459 30459 30462 30462 30465 30465 30468 30468 30471 30473
     30475 30476 30491 30491 30494 30496 30500 30502 30505 30505 30519
     30520 30522 30522 30524 30524 30528 30528 30535 30535 30554 30555
     30561 30561 30563 30563 30565 30566 30568 30568 30571 30571 30585
     30585 30590 30591 30603 30603 30606 30606 30609 30609 30622 30622
     30624 30624 30629 30629 30636 30637 30640 30640 30643 30643 30646
     30646 30649 30649 30651 30653 30655 30655 30663 30663 30669 30669
     30679 30679 30682 30684 30690 30691 30693 30693 30695 30695 30697
     30697 30701 30703 30707 30707 30716 30716 30722 30722 30732 30732
     30738 30738 30740 30741 30752 30752 30757 30759 30770 30770 30772
     30772 30778 30778 30783 30783 30789 30789 30813 30813 30827 30828
     30831 30831 30834 30834 30836 30836 30844 30844 30849 30849 30854
     30855 30860 30862 30865 30865 30867 30867 30869 30869 30871 30871
     30874 30874 30883 30883 30887 30887 30889 30890 30895 30895 30901
     30901 30906 30906 30908 30908 30910 30910 30913 30913 30917 30918
     30922 30923 30928 30929 30932 30932 30938 30938 30951 30952 30956
     30956 30959 30959 30964 30964 30973 30973 30977 30977 30983 30983
     30990 30990 30993 30994 31001 31001 31014 31014 31018 31020 31034
     31034 31036 31036 31038 31038 31040 31041 31047 31049 31056 31056
     31059 31059 31061 31063 31066 31066 31069 31072 31074 31074 31077
     31077 31080 31080 31085 31085 31095 31095 31098 31098 31103 31105
     31108 31109 31114 31114 31117 31119 31133 31133 31142 31143 31146
     31146 31150 31150 31152 31152 31155 31155 31161 31162 31165 31169
     31177 31177 31179 31179 31185 31186 31189 31189 31192 31192 31199
     31199 31201 31201 31203 31204 31206 31207 31209 31209 31212 31212
     31216 31216 31227 31227 31232 31232 31240 31240 31243 31243 31245
     31246 31252 31252 31255 31258 31260 31260 31263 31264 31278 31278
     31281 31282 31287 31287 31291 31296 31298 31299 31302 31302 31305
     31305 31309 31312 31319 31319 31329 31331 31337 31337 31339 31339
     31344 31344 31348 31348 31350 31350 31353 31354 31357 31357 31359
     31359 31361 31361 31363 31364 31368 31368 31378 31379 31381 31384
     31391 31391 31401 31402 31406 31408 31414 31414 31418 31418 31423
     31423 31427 31429 31431 31432 31434 31435 31437 31437 31439 31439
     31442 31443 31445 31445 31449 31450 31452 31453 31455 31459 31461
     31462 31466 31466 31469 31469 31471 31472 31478 31478 31480 31482
     31487 31487 31490 31490 31492 31492 31494 31494 31496 31496 31498
     31499 31503 31503 31505 31505 31512 31513 31515 31515 31518 31518
     31520 31520 31525 31526 31528 31528 31532 31532 31539 31539 31541
     31542 31545 31545 31557 31558 31560 31561 31563 31565 31567 31570
     31572 31572 31574 31574 31581 31581 31589 31589 31591 31591 31596
     31596 31598 31598 31600 31601 31604 31605 31610 31610 31622 31623
     31627 31627 31629 31629 31631 31631 31634 31634 31636 31637 31639
     31642 31644 31645 31647 31647 31649 31649 31658 31658 31661 31661
     31665 31665 31668 31668 31672 31672 31680 31681 31684 31684 31686
     31687 31689 31689 31691 31692 31695 31695 31709 31709 31712 31712
     31716 31718 31721 31721 31725 31725 31731 31731 31734 31735 31744
     31744 31751 31751 31757 31757 31761 31764 31767 31767 31775 31775
     31777 31777 31779 31779 31783 31783 31786 31787 31799 31800 31805
     31808 31811 31811 31820 31821 31823 31824 31828 31828 31830 31830
     31832 31832 31839 31840 31844 31845 31852 31852 31859 31859 31861
     31861 31870 31870 31873 31875 31881 31881 31883 31883 31885 31885
     31888 31888 31890 31890 31893 31893 31895 31896 31899 31899 31903
     31903 31905 31906 31908 31909 31911 31912 31915 31915 31917 31918
     31921 31923 31929 31929 31933 31934 31936 31936 31938 31938 31941
     31941 31946 31946 31950 31950 31954 31954 31958 31958 31960 31960
     31964 31964 31966 31968 31970 31970 31975 31975 31983 31983 31986
     31986 31988 31988 31990 31990 31992 31992 31994 31995 31998 31998
     32000 32000 32002 32002 32004 32006 32010 32011 32013 32013 32016
     32016 32020 32021 32023 32028 32032 32034 32043 32044 32046 32048
     32050 32051 32053 32053 32057 32058 32063 32063 32066 32070 32075
     32076 32078 32080 32086 32086 32091 32091 32094 32094 32097 32099
     32102 32102 32104 32104 32110 32110 32113 32115 32117 32118 32121
     32121 32125 32125 32137 32137 32143 32143 32147 32147 32153 32156
     32159 32159 32162 32163 32171 32178 32180 32181 32184 32184 32186
     32187 32189 32191 32199 32199 32202 32203 32207 32207 32209 32210
     32213 32213 32216 32216 32218 32218 32220 32222 32224 32225 32228
     32228 32232 32233 32236 32236 32239 32239 32242 32242 32244 32244
     32251 32251 32257 32257 32260 32261 32265 32267 32274 32274 32283
     32283 32286 32287 32289 32291 32294 32294 32299 32299 32302 32302
     32305 32306 32309 32309 32311 32311 32313 32315 32317 32318 32321
     32321 32323 32323 32326 32326 32330 32331 32333 32333 32340 32342
     32345 32346 32349 32350 32358 32359 32361 32362 32365 32365 32368
     32368 32377 32377 32379 32381 32383 32383 32386 32387 32392 32393
     32396 32396 32398 32400 32402 32404 32406 32406 32411 32412 32566
     32566 32568 32568 32570 32570 32581 32581 32588 32590 32592 32593
     32596 32597 32600 32600 32607 32608 32615 32619 32622 32622 32624
     32624 32626 32626 32629 32629 32631 32633 32642 32643 32645 32648
     32650 32650 32652 32652 32654 32654 32660 32660 32666 32666 32669
     32670 32675 32676 32680 32681 32686 32687 32690 32690 32694 32694
     32696 32697 32701 32701 32705 32705 32709 32710 32714 32714 32716
     32716 32722 32722 32724 32725 32736 32737 32742 32742 32745 32745
     32747 32747 32752 32752 32755 32755 32761 32761 32763 32764 32768
     32769 32771 32774 32779 32780 32784 32784 32786 32786 32789 32789
     32791 32793 32796 32796 32801 32801 32808 32808 32819 32819 32822
     32822 32827 32827 32829 32829 32831 32831 32838 32838 32842 32842
     32850 32850 32854 32854 32856 32856 32858 32858 32862 32863 32865
     32866 32872 32872 32879 32880 32882 32884 32886 32887 32889 32889
     32893 32895 32900 32903 32905 32905 32907 32908 32915 32915 32918
     32918 32920 32920 32922 32923 32925 32925 32929 32930 32933 32933
     32937 32938 32940 32941 32943 32943 32945 32946 32948 32948 32954
     32954 32963 32964 32966 32966 32972 32972 32974 32974 32982 32982
     32985 32987 32989 32990 32993 32993 32996 32997 33007 33007 33009
     33009 33012 33012 33016 33016 33020 33021 33026 33026 33029 33034
     33050 33051 33059 33059 33065 33065 33071 33071 33073 33073 33075
     33075 33081 33081 33086 33086 33094 33094 33099 33099 33102 33102
     33104 33105 33107 33109 33119 33119 33125 33126 33131 33131 33134
     33134 33136 33137 33140 33140 33144 33146 33151 33152 33154 33155
     33160 33160 33162 33162 33167 33167 33171 33171 33173 33173 33178
     33178 33180 33181 33184 33184 33187 33188 33192 33193 33200 33200
     33203 33203 33205 33205 33208 33208 33210 33210 33213 33216 33218
     33218 33222 33222 33224 33225 33229 33229 33233 33233 33235 33235
     33240 33242 33247 33248 33251 33251 33253 33253 33255 33256 33258
     33258 33261 33261 33267 33268 33274 33276 33278 33278 33281 33282
     33285 33285 33287 33290 33292 33294 33296 33296 33298 33298 33302
     33304 33307 33308 33310 33311 33321 33324 33326 33326 33331 33331
     33333 33337 33344 33344 33351 33351 33368 33370 33373 33373 33375
     33375 33378 33378 33380 33380 33382 33382 33384 33384 33386 33387
     33390 33391 33393 33394 33398 33400 33406 33406 33419 33419 33421
     33421 33426 33426 33433 33433 33437 33437 33439 33439 33445 33446
     33451 33453 33455 33455 33457 33457 33459 33459 33464 33465 33467
     33467 33469 33469 33477 33477 33489 33492 33495 33495 33497 33497
     33499 33500 33502 33503 33505 33505 33507 33507 33509 33511 33515
     33515 33521 33521 33523 33524 33529 33531 33538 33542 33545 33545
     33550 33550 33558 33560 33564 33564 33571 33571 33576 33576 33579
     33579 33583 33583 33585 33586 33588 33590 33592 33593 33600 33600
     33605 33605 33609 33610 33615 33616 33618 33618 33624 33624 33651
     33651 33653 33653 33655 33655 33659 33660 33669 33669 33671 33671
     33673 33674 33678 33678 33683 33683 33686 33686 33690 33690 33694
     33696 33698 33698 33704 33704 33706 33707 33713 33713 33717 33717
     33725 33725 33729 33729 33733 33733 33738 33738 33740 33740 33742
     33742 33747 33747 33750 33750 33752 33752 33756 33756 33759 33760
     33769 33769 33771 33771 33775 33778 33780 33780 33783 33783 33787
     33787 33789 33789 33795 33796 33799 33799 33803 33806 33811 33811
     33824 33824 33826 33826 33833 33834 33836 33836 33841 33841 33845
     33845 33848 33848 33852 33853 33862 33862 33865 33865 33870 33870
     33879 33879 33883 33883 33889 33891 33894 33894 33897 33897 33899
     33903 33905 33905 33909 33909 33911 33911 33913 33914 33922 33922
     33924 33924 33931 33931 33936 33936 33940 33940 33945 33945 33948
     33948 33951 33951 33953 33953 33965 33965 33970 33970 33976 33977
     33979 33980 33983 33983 33985 33985 33988 33988 33990 33990 33993
     33995 33997 33997 34000 34001 34006 34006 34009 34010 34028 34028
     34030 34030 34036 34036 34044 34044 34047 34048 34054 34054 34065
     34065 34067 34069 34071 34072 34074 34074 34079 34079 34081 34081
     34086 34086 34092 34093 34101 34101 34109 34109 34112 34113 34115
     34115 34120 34123 34126 34126 34133 34133 34135 34136 34138 34138
     34147 34147 34152 34154 34157 34157 34167 34167 34174 34174 34176
     34176 34180 34180 34183 34184 34186 34186 34192 34193 34196 34196
     34199 34199 34201 34201 34203 34204 34212 34212 34214 34214 34216
     34220 34222 34223 34233 34234 34241 34241 34249 34249 34253 34253
     34255 34256 34261 34261 34268 34269 34276 34277 34281 34282 34295
     34295 34297 34299 34302 34302 34306 34306 34310 34311 34314 34315
     34323 34323 34326 34327 34330 34330 34338 34338 34349 34349 34351
     34352 34367 34367 34381 34382 34384 34384 34388 34389 34394 34394
     34396 34396 34398 34399 34407 34407 34411 34411 34417 34417 34425
     34425 34427 34427 34442 34444 34451 34451 34453 34453 34467 34468
     34473 34475 34479 34480 34486 34486 34500 34500 34502 34503 34505
     34505 34507 34507 34509 34510 34516 34516 34521 34521 34523 34523
     34526 34527 34532 34532 34537 34537 34540 34543 34552 34553 34555
     34555 34558 34558 34560 34560 34562 34563 34566 34566 34568 34570
     34573 34573 34577 34578 34584 34584 34586 34586 34588 34588 34597
     34597 34601 34601 34612 34612 34615 34615 34619 34619 34623 34623
     34633 34633 34635 34636 34638 34638 34643 34643 34645 34645 34647
     34647 34649 34649 34655 34656 34659 34659 34662 34662 34664 34664
     34666 34666 34670 34670 34676 34676 34678 34678 34680 34680 34687
     34687 34690 34690 34701 34701 34719 34719 34722 34722 34731 34731
     34735 34735 34739 34739 34746 34747 34749 34749 34752 34752 34756
     34756 34758 34759 34763 34763 34768 34768 34770 34770 34784 34784
     34799 34799 34802 34802 34806 34807 34809 34809 34811 34811 34814
     34814 34821 34821 34829 34831 34833 34833 34837 34838 34849 34851
     34855 34855 34865 34865 34870 34870 34873 34873 34875 34875 34880
     34880 34882 34882 34884 34884 34886 34886 34892 34893 34898 34899
     34903 34903 34905 34905 34907 34907 34909 34910 34913 34915 34920
     34920 34923 34923 34928 34928 34930 34930 34933 34933 34935 34935
     34941 34943 34945 34946 34952 34952 34955 34955 34957 34957 34962
     34962 34966 34967 34969 34969 34974 34974 34978 34978 34980 34980
     34987 34987 34990 34990 34992 34993 34996 34997 34999 34999 35007
     35007 35009 35013 35023 35023 35028 35029 35032 35033 35036 35037
     35039 35039 35041 35041 35048 35048 35058 35060 35064 35065 35068
     35070 35074 35074 35076 35076 35079 35079 35082 35082 35084 35084
     35088 35088 35090 35091 35101 35102 35109 35109 35114 35115 35126
     35126 35128 35128 35131 35131 35137 35137 35139 35140 35148 35149
     35158 35158 35166 35168 35172 35172 35174 35174 35178 35178 35181
     35181 35183 35183 35186 35186 35188 35188 35191 35191 35198 35199
     35201 35201 35203 35203 35206 35208 35210 35211 35215 35215 35219
     35219 35222 35224 35226 35226 35233 35233 35238 35239 35241 35242
     35244 35244 35247 35247 35250 35251 35258 35258 35261 35261 35263
     35264 35282 35282 35290 35290 35292 35293 35299 35299 35302 35303
     35316 35316 35320 35320 35328 35328 35330 35331 35336 35336 35338
     35338 35340 35340 35342 35342 35344 35344 35347 35347 35350 35352
     35355 35355 35357 35357 35359 35359 35363 35363 35365 35365 35370
     35370 35373 35373 35377 35377 35379 35380 35382 35382 35386 35388
     35393 35393 35398 35398 35400 35400 35408 35410 35412 35413 35419
     35419 35422 35422 35424 35424 35426 35427 35430 35430 35433 35433
     35435 35438 35440 35443 35452 35452 35458 35458 35460 35461 35463
     35463 35465 35465 35468 35469 35473 35473 35475 35475 35477 35477
     35480 35480 35482 35482 35486 35486 35488 35489 35491 35494 35496
     35496 35500 35501 35504 35504 35506 35506 35513 35513 35516 35516
     35519 35519 35522 35522 35524 35524 35527 35527 35531 35533 35535
     35535 35538 35538 35542 35542 35546 35548 35550 35550 35552 35554
     35556 35556 35558 35559 35563 35563 35565 35566 35569 35569 35571
     35571 35575 35576 35578 35578 35582 35582 35584 35586 35588 35588
     35591 35591 35596 35596 35598 35598 35600 35600 35604 35604 35606
     35607 35609 35611 35613 35613 35616 35617 35622 35622 35624 35624
     35627 35628 35635 35635 35641 35641 35646 35646 35649 35649 35657
     35657 35660 35660 35662 35663 35670 35670 35672 35672 35674 35676
     35679 35679 35686 35686 35691 35692 35695 35698 35700 35700 35703
     35703 35709 35709 35712 35712 35715 35715 35722 35722 35724 35724
     35726 35726 35728 35728 35730 35731 35734 35734 35737 35738 35895
     35895 35898 35898 35903 35903 35905 35905 35910 35910 35912 35912
     35914 35914 35916 35916 35918 35918 35920 35920 35925 35925 35930
     35930 35937 35938 35946 35948 35960 35962 35964 35964 35970 35970
     35973 35973 35977 35978 35980 35982 35988 35988 35992 35992 35997
     35998 36000 36002 36007 36016 36018 36020 36022 36024 36027 36029
     36031 36036 36039 36040 36042 36042 36045 36046 36049 36049 36051
     36051 36058 36060 36062 36062 36064 36064 36066 36068 36070 36070
     36074 36074 36077 36077 36090 36093 36100 36101 36103 36104 36106
     36107 36109 36109 36111 36112 36115 36116 36118 36118 36196 36196
     36198 36199 36203 36203 36205 36205 36208 36209 36211 36212 36215
     36215 36225 36225 36229 36229 36234 36234 36249 36249 36259 36259
     36264 36264 36275 36275 36282 36282 36286 36286 36290 36290 36299
     36300 36303 36303 36310 36310 36314 36315 36317 36317 36319 36319
     36321 36321 36323 36323 36328 36328 36330 36331 36335 36335 36339
     36339 36341 36341 36348 36348 36351 36351 36360 36362 36367 36368
     36381 36383 36394 36394 36400 36400 36404 36405 36418 36418 36420
     36420 36423 36426 36428 36428 36432 36432 36437 36437 36441 36441
     36447 36448 36451 36452 36466 36466 36468 36468 36470 36470 36476
     36476 36481 36481 36484 36485 36487 36487 36490 36491 36493 36493
     36497 36497 36499 36500 36505 36505 36513 36513 36522 36524 36527
     36529 36542 36542 36549 36550 36552 36552 36554 36557 36562 36562
     36571 36571 36575 36575 36578 36579 36587 36587 36600 36600 36603
     36606 36611 36611 36613 36613 36617 36618 36620 36620 36626 36629
     36633 36633 36635 36637 36639 36639 36646 36646 36649 36650 36655
     36655 36659 36659 36664 36665 36667 36667 36670 36671 36674 36674
     36676 36678 36681 36681 36684 36686 36695 36695 36700 36700 36703
     36703 36705 36708 36763 36764 36766 36767 36771 36771 36775 36776
     36781 36786 36791 36791 36794 36796 36799 36799 36802 36802 36804
     36805 36814 36814 36817 36817 36820 36820 36826 36826 36834 36834
     36837 36838 36841 36843 36845 36845 36847 36848 36852 36852 36855
     36858 36861 36861 36864 36865 36867 36867 36869 36870 36875 36875
     36877 36881 36883 36887 36889 36890 36893 36899 36903 36903 36910
     36910 36913 36914 36917 36918 36920 36921 36924 36924 36926 36926
     36929 36930 36933 36933 36935 36935 36937 36939 36941 36950 36952
     36953 36956 36956 36958 36958 36960 36961 36963 36963 36965 36965
     36968 36969 36973 36975 36978 36978 36981 36984 36986 36986 36988
     36989 36991 36996 36999 36999 37001 37002 37007 37007 37009 37009
     37027 37027 37030 37030 37032 37032 37034 37034 37039 37039 37041
     37041 37045 37045 37048 37048 37057 37057 37066 37066 37070 37070
     37083 37083 37089 37090 37092 37092 37096 37096 37101 37101 37109
     37109 37111 37111 37117 37117 37122 37122 37138 37138 37145 37145
     37165 37165 37168 37168 37170 37170 37193 37198 37202 37202 37204
     37204 37206 37206 37208 37208 37218 37219 37221 37221 37225 37226
     37228 37228 37234 37235 37237 37237 37239 37240 37250 37250 37255
     37255 37257 37257 37259 37259 37261 37261 37264 37264 37266 37266
     37271 37271 37276 37276 37282 37282 37284 37284 37290 37291 37295
     37295 37300 37301 37304 37304 37306 37306 37312 37313 37318 37321
     37323 37329 37334 37334 37336 37336 37339 37341 37343 37343 37345
     37345 37347 37347 37350 37351 37365 37366 37372 37372 37375 37375
     37389 37390 37393 37393 37396 37397 37406 37406 37417 37417 37420
     37420 37428 37428 37431 37431 37439 37439 37444 37445 37448 37449
     37451 37451 37456 37456 37463 37463 37466 37467 37470 37470 37474
     37474 37476 37476 37478 37478 37489 37489 37502 37502 37504 37504
     37507 37507 37509 37509 37521 37521 37523 37523 37525 37526 37528
     37528 37530 37532 37549 37549 37559 37559 37561 37561 37583 37583
     37586 37586 37604 37604 37609 37610 37613 37613 37618 37619 37624
     37624 37626 37626 37628 37628 37638 37638 37647 37648 37656 37658
     37664 37664 37666 37667 37670 37670 37672 37672 37675 37676 37678
     37679 37682 37682 37685 37685 37690 37691 37700 37700 37707 37707
     37709 37709 37716 37716 37718 37718 37723 37724 37728 37728 37740
     37740 37742 37742 37749 37749 37756 37756 37758 37758 37772 37772
     37780 37780 37782 37783 37786 37786 37799 37799 37804 37806 37808
     37808 37817 37817 37827 37827 37832 37832 37840 37841 37846 37848
     37853 37853 37857 37857 37860 37861 37864 37864 37891 37891 37895
     37895 37904 37904 37907 37908 37912 37914 37921 37921 37931 37931
     37941 37942 37944 37944 37946 37946 37953 37953 37956 37956 37969
     37971 37978 37979 37982 37982 37984 37984 37986 37986 37994 37994
     38000 38000 38005 38005 38007 38007 38012 38015 38017 38017 38263
     38263 38272 38272 38274 38275 38279 38279 38281 38283 38287 38287
     38289 38289 38291 38292 38294 38294 38296 38297 38304 38304 38306
     38309 38311 38312 38317 38317 38322 38322 38329 38329 38331 38332
     38334 38334 38339 38339 38343 38343 38346 38346 38348 38349 38356
     38358 38360 38360 38364 38364 38369 38370 38373 38373 38428 38428
     38433 38433 38440 38440 38442 38442 38446 38447 38450 38450 38459
     38459 38463 38464 38466 38466 38468 38468 38475 38477 38479 38480
     38491 38495 38498 38502 38506 38506 38508 38508 38512 38512 38514
     38515 38517 38520 38522 38522 38525 38525 38533 38534 38536 38536
     38538 38539 38541 38543 38548 38549 38551 38553 38555 38556 38560
     38560 38563 38563 38567 38568 38570 38570 38576 38578 38580 38580
     38582 38585 38587 38588 38592 38593 38596 38599 38601 38601 38603
     38606 38609 38609 38613 38614 38617 38617 38619 38620 38626 38627
     38632 38632 38634 38635 38640 38640 38642 38642 38646 38647 38649
     38649 38651 38651 38656 38656 38660 38660 38662 38664 38666 38666
     38669 38671 38673 38673 38675 38675 38678 38678 38681 38681 38684
     38684 38686 38686 38692 38692 38695 38695 38698 38698 38704 38704
     38706 38706 38712 38713 38717 38718 38722 38722 38724 38724 38726
     38726 38728 38729 38738 38738 38742 38742 38745 38745 38748 38748
     38750 38750 38752 38754 38756 38756 38758 38758 38760 38761 38763
     38763 38765 38765 38769 38769 38772 38772 38777 38778 38780 38780
     38785 38785 38788 38790 38795 38795 38797 38797 38799 38800 38808
     38808 38812 38812 38816 38816 38819 38819 38822 38822 38824 38824
     38827 38827 38829 38829 38835 38836 38851 38851 38854 38854 38856
     38856 38859 38859 38867 38867 38876 38876 38893 38894 38898 38899
     38901 38902 38907 38907 38911 38911 38913 38915 38917 38918 38920
     38920 38924 38924 38927 38931 38935 38936 38938 38938 38945 38945
     38948 38948 38956 38957 38964 38964 38967 38968 38971 38973 38982
     38982 38987 38991 38996 38997 39000 39000 39003 39003 39006 39006
     39015 39015 39019 39019 39023 39025 39027 39028 39080 39080 39082
     39082 39087 39087 39089 39089 39094 39094 39107 39108 39110 39110
     39131 39132 39135 39135 39138 39138 39145 39145 39147 39147 39149
     39151 39154 39154 39156 39156 39164 39166 39171 39171 39173 39173
     39177 39178 39180 39180 39184 39184 39186 39188 39192 39192 39197
     39198 39200 39201 39204 39204 39208 39208 39212 39212 39214 39214
     39229 39230 39234 39234 39237 39237 39241 39241 39243 39244 39248
     39250 39253 39253 39255 39255 39318 39321 39333 39333 39336 39336
     39340 39342 39347 39348 39356 39356 39361 39361 39364 39366 39368
     39368 39376 39378 39381 39381 39384 39384 39387 39387 39389 39389
     39391 39391 39394 39394 39405 39406 39409 39410 39416 39416 39419
     39419 39423 39423 39425 39425 39429 39429 39438 39439 39442 39443
     39449 39449 39464 39464 39467 39467 39472 39472 39479 39479 39486
     39486 39488 39488 39490 39491 39493 39493 39501 39501 39509 39509
     39511 39511 39514 39515 39519 39519 39522 39522 39524 39525 39529
     39531 39592 39592 39597 39597 39600 39600 39608 39608 39612 39612
     39616 39616 39620 39620 39631 39631 39633 39633 39635 39636 39640
     39640 39646 39647 39650 39651 39654 39654 39658 39659 39661 39663
     39665 39665 39668 39668 39671 39671 39675 39675 39686 39686 39704
     39704 39706 39706 39711 39711 39714 39715 39717 39717 39719 39722
     39726 39727 39729 39730 39739 39740 39745 39749 39757 39759 39761
     39761 39764 39764 39768 39768 39770 39770 39791 39791 39796 39796
     39811 39811 39822 39822 39825 39827 39830 39831 39839 39840 39848
     39848 39850 39851 39853 39854 39860 39860 39865 39865 39872 39872
     39878 39878 39881 39882 39887 39887 39889 39890 39892 39892 39894
     39894 39899 39899 39905 39908 39912 39912 39920 39922 39925 39925
     39940 39940 39942 39942 39944 39946 39948 39949 39952 39952 39954
     39957 39963 39963 39969 39969 39972 39973 39981 39984 39986 39986
     39993 39995 39998 39998 40006 40008 40018 40018 40023 40023 40026
     40026 40032 40032 40039 40039 40054 40054 40056 40056 40165 40165
     40167 40167 40169 40169 40171 40172 40176 40176 40179 40180 40182
     40182 40195 40195 40198 40201 40206 40206 40210 40210 40213 40213
     40219 40219 40223 40223 40227 40227 40230 40230 40232 40232 40234
     40236 40251 40251 40254 40255 40257 40257 40260 40260 40262 40262
     40264 40264 40272 40273 40281 40281 40284 40286 40288 40289 40292
     40292 40300 40300 40303 40303 40306 40306 40314 40314 40327 40327
     40329 40329 40335 40335 40346 40346 40356 40356 40361 40361 40363
     40363 40367 40367 40370 40370 40372 40372 40376 40376 40378 40379
     40385 40386 40388 40388 40390 40390 40399 40399 40403 40403 40409
     40409 40422 40422 40429 40429 40431 40431 40434 40434 40440 40442
     40445 40445 40474 40475 40478 40478 40565 40565 40568 40569 40573
     40573 40575 40575 40577 40577 40584 40584 40587 40588 40593 40595
     40597 40597 40599 40599 40605 40605 40607 40607 40613 40614 40617
     40618 40621 40621 40632 40636 40638 40639 40644 40644 40652 40656
     40658 40658 40660 40660 40664 40665 40667 40670 40672 40672 40677
     40677 40680 40680 40687 40687 40692 40692 40694 40695 40697 40697
     40699 40701 40711 40712 40718 40718 40723 40723 40725 40725 40736
     40737 40748 40748 40763 40763 40766 40766 40778 40779 40782 40783
     40786 40786 40788 40788 40799 40803 40806 40807 40810 40810 40812
     40812 40818 40818 40822 40823 40845 40845 40853 40853 40860 40861
     40864 40864 65281 65281 65283 65286 65288 65292 65294 65373 65507
     65507 65509 65509))
  (MAKE-CHARACTER-SET :MIB-ENUM 2009 :NAME "IBM850" :ALIASES
   '("csPC850Multilingual" "850" "cp850") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 255 305 305 402 402 8215 8215 9472 9472 9474 9474 9484
     9484 9488 9488 9492 9492 9496 9496 9500 9500 9508 9508 9516 9516
     9524 9524 9532 9532 9552 9553 9556 9556 9559 9559 9562 9562 9565
     9565 9568 9568 9571 9571 9574 9574 9577 9577 9580 9580 9600 9600
     9604 9604 9608 9608 9617 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 7 :NAME "ISO_8859-4:1988" :ALIASES
   '("csISOLatin4" "l4" "latin4" #6="ISO-8859-4" "ISO_8859-4"
     "iso-ir-110")
   :MIME-ENCODING '#6# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: ISO-8859-4 (preferred MIME name)") :REFERENCES
   '("[RFC1345,KXS2]") :RANGES
   #(0 160 164 164 167 168 173 173 175 176 180 180 184 184 193 198 201
     201 203 203 205 206 212 216 218 220 223 223 225 230 233 233 235 235
     237 238 244 248 250 252 256 257 260 261 268 269 272 275 278 281 290
     291 296 299 302 303 310 312 315 316 325 326 330 333 342 343 352 353
     358 363 370 371 381 382 711 711 729 729 731 731))
  (MAKE-CHARACTER-SET :MIB-ENUM 2047 :NAME "IBM857" :ALIASES
   '("csIBM857" "857" "cp857") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 207 209 220 223 239 241 252 255 255 286 287 304 305 350
     351 9472 9472 9474 9474 9484 9484 9488 9488 9492 9492 9496 9496
     9500 9500 9508 9508 9516 9516 9524 9524 9532 9532 9552 9553 9556
     9556 9559 9559 9562 9562 9565 9565 9568 9568 9571 9571 9574 9574
     9577 9577 9580 9580 9600 9600 9604 9604 9608 9608 9617 9619 9632
     9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 40 :NAME #7="ISO-2022-JP-2" :ALIASES
   '("csISO2022JP2") :MIME-ENCODING '#7# :SOURCE '"RFC-1554" :COMMENTS
   'NIL :REFERENCES '("[RFC1554,Ohta]") :RANGES
   #(0 275 278 290 292 299 302 333 336 382 461 476 501 501 711 711 713
     713 720 720 728 731 733 733 890 890 900 902 904 906 908 908 910 929
     931 974 1025 1036 1038 1103 1105 1116 1118 1119 8208 8208 8213 8214
     8216 8217 8220 8221 8224 8225 8229 8230 8240 8240 8242 8243 8251
     8251 8254 8254 8308 8308 8319 8319 8321 8324 8364 8364 8367 8367
     8451 8451 8457 8457 8467 8467 8470 8470 8481 8482 8486 8486 8491
     8491 8531 8532 8539 8542 8544 8555 8560 8569 8592 8601 8658 8658
     8660 8660 8704 8704 8706 8707 8711 8712 8715 8715 8719 8719 8721
     8722 8730 8730 8733 8734 8736 8736 8741 8741 8743 8748 8750 8750
     8756 8759 8764 8765 8776 8776 8780 8780 8786 8786 8800 8801 8804
     8807 8810 8811 8814 8815 8834 8835 8838 8839 8857 8857 8869 8869
     8978 8978 9312 9326 9332 9397 9424 9449 9472 9547 9618 9618 9632
     9633 9635 9641 9650 9651 9654 9655 9660 9661 9664 9665 9670 9672
     9675 9675 9678 9681 9711 9711 9733 9734 9742 9743 9756 9756 9758
     9758 9792 9792 9794 9794 9824 9825 9827 9829 9831 9834 9836 9837
     9839 9839 12288 12291 12293 12311 12316 12316 12353 12435 12443
     12446 12449 12534 12539 12542 12549 12585 12593 12686 12800 12828
     12832 12841 12896 12923 12927 12927 13184 13188 13192 13258 13263
     13264 13267 13267 13270 13270 13272 13272 13275 13277 19968 19973
     19975 19982 19984 19999 20001 20008 20010 20018 20020 20022 20024
     20037 20039 20041 20043 20058 20060 20067 20070 20070 20072 20073
     20075 20075 20077 20077 20080 20081 20083 20087 20089 20089 20094
     20096 20098 20098 20101 20102 20104 20130 20132 20137 20139 20144
     20146 20147 20149 20150 20153 20155 20159 20167 20169 20171 20173
     20177 20179 20187 20189 20197 20200 20200 20202 20211 20213 20215
     20219 20219 20221 20227 20232 20242 20245 20256 20258 20258 20260
     20263 20266 20267 20270 20286 20288 20288 20290 20291 20294 20297
     20299 20320 20323 20325 20327 20327 20329 20330 20332 20332 20334
     20337 20339 20351 20353 20358 20360 20372 20374 20379 20381 20385
     20387 20387 20389 20399 20402 20402 20405 20407 20409 20409 20411
     20422 20424 20436 20439 20440 20442 20454 20456 20458 20461 20467
     20469 20470 20472 20472 20474 20474 20476 20482 20484 20487 20489
     20500 20502 20511 20513 20526 20528 20528 20530 20531 20533 20534
     20537 20540 20542 20542 20544 20547 20549 20554 20556 20556 20558
     20563 20565 20567 20569 20570 20572 20572 20575 20576 20578 20579
     20581 20583 20586 20586 20588 20589 20592 20594 20596 20600 20603
     20603 20605 20609 20611 20614 20616 20616 20618 20618 20621 20628
     20630 20630 20632 20636 20638 20643 20645 20645 20647 20650 20652
     20653 20655 20656 20658 20661 20663 20663 20665 20667 20669 20670
     20672 20672 20674 20677 20679 20679 20681 20682 20684 20689 20691
     20694 20696 20696 20698 20698 20700 20703 20706 20713 20716 20719
     20721 20723 20725 20726 20729 20731 20734 20740 20742 20745 20747
     20750 20752 20752 20754 20754 20756 20767 20769 20769 20771 20771
     20775 20776 20778 20778 20780 20781 20783 20783 20785 20789 20791
     20796 20799 20824 20826 20826 20828 20828 20831 20831 20834 20834
     20836 20846 20848 20849 20851 20857 20859 20862 20864 20870 20872
     20883 20885 20889 20891 20893 20896 20902 20904 20909 20911 20920
     20922 20928 20930 20930 20932 20937 20939 20941 20943 20943 20945
     20947 20949 20950 20952 20952 20955 20958 20960 20962 20964 20967
     20969 20971 20973 21000 21002 21003 21005 21006 21009 21019 21021
     21021 21024 21024 21026 21026 21028 21029 21031 21035 21037 21038
     21040 21043 21045 21053 21055 21061 21063 21063 21065 21069 21071
     21073 21076 21080 21082 21089 21091 21095 21097 21098 21102 21109
     21111 21113 21117 21117 21119 21123 21125 21125 21127 21130 21132
     21133 21136 21144 21146 21165 21167 21185 21187 21193 21195 21197
     21199 21202 21204 21209 21211 21226 21228 21228 21232 21243 21246
     21251 21253 21254 21256 21256 21258 21261 21263 21265 21267 21267
     21269 21281 21283 21283 21285 21299 21301 21301 21304 21327 21329
     21340 21342 21353 21355 21365 21367 21371 21373 21375 21378 21385
     21387 21390 21395 21398 21400 21402 21405 21405 21407 21414 21416
     21419 21421 21424 21426 21432 21434 21435 21437 21437 21439 21443
     21445 21445 21448 21455 21457 21467 21469 21491 21493 21501 21505
     21508 21510 21510 21512 21523 21525 21527 21530 21531 21533 21537
     21539 21539 21542 21551 21553 21558 21560 21561 21563 21566 21568
     21568 21570 21572 21574 21579 21581 21593 21595 21596 21598 21599
     21602 21602 21604 21604 21606 21611 21613 21614 21616 21624 21627
     21629 21631 21638 21640 21641 21643 21650 21652 21654 21657 21661
     21663 21663 21665 21679 21681 21684 21687 21698 21700 21700 21702
     21706 21708 21717 21719 21722 21724 21730 21733 21738 21740 21743
     21746 21747 21750 21750 21754 21754 21756 21761 21764 21769 21772
     21777 21780 21782 21787 21788 21792 21792 21794 21796 21799 21799
     21802 21804 21806 21811 21813 21817 21819 21825 21827 21831 21833
     21834 21836 21837 21839 21841 21843 21843 21845 21848 21850 21854
     21856 21857 21859 21863 21866 21866 21868 21870 21877 21880 21883
     21884 21886 21892 21894 21899 21902 21903 21905 21908 21911 21914
     21916 21919 21923 21924 21927 21934 21936 21939 21941 21943 21945
     21945 21947 21947 21949 21951 21953 21953 21955 21959 21961 21961
     21963 21966 21969 21972 21974 21976 21978 21983 21985 21990 21992
     21996 21999 21999 22002 22003 22005 22007 22009 22009 22013 22017
     22021 22022 22024 22026 22028 22034 22036 22036 22038 22041 22043
     22043 22046 22047 22051 22052 22055 22055 22057 22057 22060 22061
     22063 22073 22075 22077 22079 22084 22086 22086 22089 22089 22091
     22096 22099 22100 22103 22105 22107 22108 22110 22110 22112 22116
     22118 22118 22120 22125 22127 22127 22129 22130 22132 22134 22136
     22136 22138 22140 22144 22144 22148 22152 22154 22156 22158 22159
     22163 22165 22169 22170 22173 22176 22178 22179 22181 22185 22187
     22191 22193 22193 22195 22196 22198 22199 22204 22204 22206 22206
     22208 22211 22213 22213 22216 22225 22227 22228 22231 22248 22251
     22251 22253 22254 22256 22263 22265 22266 22269 22276 22278 22285
     22287 22287 22289 22291 22293 22294 22296 22296 22298 22301 22303
     22304 22306 22314 22316 22320 22323 22324 22327 22331 22333 22336
     22338 22338 22341 22343 22346 22346 22348 22354 22359 22359 22362
     22370 22372 22379 22381 22385 22387 22391 22393 22396 22398 22399
     22401 22406 22408 22409 22411 22412 22418 22421 22423 22423 22425
     22436 22438 22446 22448 22448 22450 22452 22456 22456 22461 22461
     22464 22464 22466 22467 22470 22472 22475 22476 22478 22479 22482
     22486 22488 22490 22492 22497 22499 22500 22502 22503 22505 22505
     22509 22509 22511 22512 22516 22522 22524 22528 22530 22541 22545
     22545 22549 22549 22553 22553 22555 22555 22557 22561 22564 22564
     22566 22567 22570 22570 22573 22573 22575 22578 22580 22581 22585
     22586 22589 22589 22591 22593 22596 22596 22601 22605 22607 22610
     22612 22613 22615 22618 22622 22623 22625 22626 22628 22629 22631
     22633 22635 22636 22640 22640 22642 22642 22645 22645 22648 22649
     22652 22652 22654 22657 22659 22659 22661 22661 22663 22666 22668
     22669 22671 22672 22674 22676 22678 22679 22681 22682 22684 22690
     22694 22694 22696 22697 22699 22699 22702 22702 22705 22707 22712
     22716 22718 22718 22721 22722 22724 22725 22727 22728 22730 22730
     22732 22734 22736 22746 22748 22751 22753 22754 22756 22757 22761
     22761 22763 22764 22766 22771 22774 22775 22777 22781 22786 22786
     22788 22791 22793 22797 22799 22800 22802 22806 22808 22813 22815
     22815 22817 22821 22823 22842 22844 22844 22846 22847 22849 22852
     22854 22857 22859 22859 22862 22875 22877 22883 22885 22885 22887
     22895 22898 22902 22904 22905 22907 22909 22913 22916 22918 22920
     22922 22926 22930 22931 22933 22935 22937 22937 22939 22939 22941
     22943 22947 22949 22951 22960 22962 22963 22967 22967 22969 22972
     22974 22974 22977 22977 22979 22980 22982 22982 22984 22987 22989
     22989 22992 22996 22999 23002 23004 23007 23011 23016 23018 23020
     23022 23023 23025 23026 23028 23028 23030 23033 23035 23035 23039
     23041 23043 23049 23052 23054 23057 23059 23064 23064 23066 23068
     23070 23072 23075 23077 23079 23082 23085 23085 23087 23090 23092
     23094 23100 23100 23104 23105 23108 23114 23116 23116 23120 23120
     23125 23125 23130 23130 23134 23134 23138 23139 23141 23143 23146
     23146 23148 23149 23156 23159 23162 23163 23166 23167 23179 23179
     23184 23184 23186 23187 23190 23190 23193 23196 23198 23200 23202
     23202 23204 23204 23207 23207 23210 23210 23212 23212 23217 23219
     23221 23221 23224 23224 23226 23231 23233 23234 23236 23236 23238
     23238 23240 23241 23243 23244 23247 23248 23250 23250 23252 23252
     23254 23256 23258 23258 23260 23260 23264 23265 23267 23267 23269
     23270 23273 23275 23278 23278 23281 23281 23285 23286 23290 23291
     23293 23293 23296 23297 23301 23301 23304 23305 23307 23308 23318
     23319 23321 23321 23323 23323 23325 23325 23329 23330 23333 23333
     23338 23338 23340 23341 23344 23344 23346 23346 23348 23348 23350
     23352 23358 23358 23360 23361 23363 23363 23365 23365 23371 23372
     23376 23391 23394 23398 23400 23404 23406 23409 23411 23411 23413
     23413 23416 23416 23418 23418 23420 23425 23427 23441 23443 23462
     23464 23482 23484 23495 23497 23497 23500 23501 23503 23504 23506
     23508 23510 23515 23517 23522 23524 23529 23531 23532 23534 23537
     23539 23542 23544 23551 23553 23554 23556 23567 23569 23569 23571
     23578 23580 23584 23586 23593 23595 23598 23600 23602 23604 23606
     23608 23618 23621 23622 23624 23627 23629 23633 23635 23635 23637
     23637 23641 23644 23646 23646 23648 23657 23660 23665 23668 23670
     23673 23677 23679 23679 23681 23682 23687 23688 23690 23690 23692
     23693 23695 23698 23700 23700 23702 23709 23711 23715 23718 23718
     23720 23725 23729 23736 23738 23745 23748 23749 23751 23751 23753
     23753 23755 23755 23762 23762 23767 23767 23769 23769 23773 23773
     23776 23777 23780 23782 23784 23786 23789 23794 23796 23796 23798
     23798 23802 23803 23805 23805 23809 23811 23814 23815 23819 23819
     23821 23822 23825 23826 23828 23835 23838 23839 23842 23844 23846
     23847 23849 23849 23851 23851 23853 23854 23857 23857 23860 23860
     23865 23865 23869 23871 23874 23875 23878 23880 23882 23884 23886
     23886 23888 23890 23893 23893 23896 23897 23899 23901 23903 23906
     23908 23908 23913 23917 23919 23920 23923 23924 23926 23926 23929
     23930 23934 23935 23937 23940 23943 23944 23946 23948 23952 23952
     23954 23957 23961 23961 23963 23963 23965 23965 23967 23968 23970
     23970 23975 23975 23978 23980 23982 23982 23984 23984 23986 23986
     23988 23988 23991 23994 23996 23997 24003 24003 24005 24005 24007
     24007 24009 24009 24011 24014 24016 24016 24018 24019 24022 24022
     24024 24025 24027 24027 24029 24030 24032 24041 24043 24043 24046
     24047 24049 24053 24055 24057 24059 24059 24061 24062 24064 24067
     24069 24072 24075 24077 24079 24082 24084 24086 24088 24093 24095
     24096 24101 24104 24107 24107 24109 24115 24117 24120 24123 24128
     24130 24133 24135 24135 24137 24137 24139 24140 24142 24142 24144
     24145 24148 24152 24155 24156 24158 24159 24161 24164 24168 24168
     24170 24174 24176 24176 24178 24193 24195 24196 24198 24199 24202
     24203 24206 24209 24211 24215 24217 24218 24220 24220 24222 24224
     24226 24226 24228 24232 24234 24237 24241 24241 24243 24243 24245
     24249 24253 24255 24257 24259 24262 24262 24264 24268 24270 24278
     24282 24291 24293 24293 24296 24300 24304 24305 24307 24308 24310
     24312 24314 24316 24318 24324 24326 24337 24339 24345 24347 24349
     24351 24369 24372 24385 24388 24389 24391 24392 24394 24394 24396
     24398 24400 24409 24411 24413 24416 24420 24422 24423 24425 24429
     24431 24437 24439 24453 24455 24461 24463 24467 24469 24473 24476
     24478 24480 24482 24484 24484 24487 24497 24499 24501 24503 24505
     24508 24509 24515 24521 24523 24525 24527 24532 24534 24537 24540
     24542 24544 24546 24548 24548 24551 24563 24565 24566 24568 24568
     24570 24583 24586 24586 24589 24592 24594 24605 24607 24609 24612
     24619 24621 24621 24623 24623 24625 24625 24627 24627 24629 24629
     24634 24636 24639 24643 24646 24653 24656 24658 24660 24663 24665
     24666 24669 24669 24671 24677 24679 24685 24687 24689 24691 24691
     24693 24703 24705 24710 24712 24718 24721 24728 24730 24731 24733
     24736 24738 24749 24751 24760 24763 24766 24770 24770 24772 24779
     24782 24783 24785 24785 24787 24789 24792 24803 24805 24809 24811
     24829 24832 24835 24838 24842 24844 24855 24857 24860 24862 24868
     24870 24872 24874 24876 24880 24881 24884 24887 24889 24889 24892
     24895 24897 24898 24900 24910 24913 24913 24915 24915 24917 24917
     24920 24922 24925 24928 24930 24933 24935 24936 24939 24940 24942
     24952 24955 24964 24967 24967 24970 24971 24973 24974 24976 24980
     24982 24986 24988 24989 24991 24992 24996 24997 24999 25006 25010
     25010 25014 25018 25020 25020 25022 25022 25024 25027 25030 25042
     25044 25045 25052 25055 25057 25059 25061 25063 25065 25065 25068
     25069 25071 25071 25074 25074 25076 25080 25082 25082 25084 25089
     25091 25092 25094 25112 25114 25127 25129 25132 25134 25140 25142
     25145 25147 25147 25149 25156 25158 25161 25163 25166 25168 25174
     25176 25176 25178 25180 25182 25182 25184 25184 25187 25188 25190
     25201 25203 25203 25206 25206 25209 25210 25212 25216 25218 25220
     25225 25226 25229 25240 25242 25244 25246 25250 25252 25254 25256
     25256 25259 25260 25265 25265 25267 25267 25269 25271 25273 25279
     25282 25282 25284 25290 25292 25309 25311 25315 25317 25322 25324
     25327 25329 25335 25340 25343 25345 25348 25351 25358 25360 25361
     25363 25363 25366 25366 25368 25371 25373 25381 25383 25387 25389
     25389 25391 25391 25394 25394 25397 25398 25401 25407 25409 25414
     25417 25424 25426 25429 25431 25432 25435 25436 25438 25439 25441
     25443 25445 25449 25451 25454 25457 25458 25460 25464 25466 25469
     25471 25472 25474 25476 25479 25482 25484 25484 25486 25488 25490
     25490 25492 25494 25496 25499 25502 25520 25522 25525 25527 25528
     25530 25534 25536 25537 25539 25542 25544 25545 25549 25558 25562
     25564 25566 25566 25568 25569 25571 25571 25573 25573 25577 25578
     25580 25582 25586 25594 25597 25597 25599 25602 25605 25606 25609
     25613 25615 25616 25618 25620 25622 25624 25627 25628 25630 25630
     25632 25634 25636 25638 25640 25642 25644 25645 25647 25648 25652
     25654 25658 25658 25661 25663 25665 25666 25668 25672 25674 25675
     25678 25679 25681 25684 25688 25688 25690 25697 25699 25699 25703
     25703 25705 25705 25709 25709 25711 25711 25715 25716 25718 25718
     25720 25723 25725 25725 25730 25733 25735 25736 25743 25747 25749
     25750 25752 25755 25757 25759 25761 25761 25763 25766 25768 25769
     25771 25774 25776 25776 25778 25779 25781 25781 25783 25794 25796
     25797 25799 25799 25801 25806 25808 25810 25812 25813 25815 25816
     25818 25818 25822 25822 25824 25831 25833 25834 25836 25837 25839
     25842 25844 25847 25850 25851 25853 25857 25860 25861 25864 25866
     25871 25871 25874 25876 25878 25878 25880 25881 25883 25887 25890
     25894 25897 25900 25902 25903 25905 25905 25908 25919 25923 25923
     25925 25925 25927 25929 25932 25938 25940 25945 25947 25947 25949
     25952 25954 25955 25958 25959 25963 25965 25968 25968 25970 25970
     25972 25973 25975 25976 25978 25978 25981 25982 25985 25987 25989
     25989 25991 25996 25998 25998 26000 26003 26005 26005 26007 26009
     26011 26013 26015 26017 26019 26023 26025 26025 26027 26032 26034
     26036 26039 26039 26041 26041 26044 26045 26047 26047 26049 26054
     26056 26057 26059 26060 26062 26064 26066 26066 26068 26068 26070
     26073 26075 26075 26079 26083 26085 26089 26092 26098 26100 26103
     26105 26112 26114 26116 26118 26122 26124 26134 26137 26137 26140
     26161 26163 26167 26169 26172 26174 26182 26185 26188 26190 26191
     26193 26210 26212 26220 26222 26224 26227 26236 26238 26244 26247
     26249 26251 26254 26256 26258 26262 26269 26271 26272 26274 26274
     26276 26276 26278 26280 26283 26283 26285 26286 26289 26293 26296
     26297 26299 26300 26302 26308 26310 26313 26316 26316 26318 26319
     26324 26324 26326 26326 26329 26333 26335 26336 26342 26342 26344
     26345 26347 26348 26350 26350 26352 26352 26354 26357 26359 26368
     26371 26371 26373 26373 26375 26379 26381 26384 26387 26391 26393
     26393 26395 26400 26402 26402 26406 26408 26410 26415 26417 26417
     26419 26424 26426 26426 26429 26435 26437 26441 26444 26444 26446
     26449 26451 26454 26457 26457 26460 26470 26472 26474 26476 26487
     26491 26492 26494 26495 26497 26497 26500 26501 26503 26503 26505
     26505 26507 26508 26510 26513 26515 26515 26517 26526 26528 26531
     26533 26539 26541 26541 26543 26553 26555 26558 26560 26566 26568
     26570 26574 26580 26583 26586 26588 26590 26592 26594 26596 26599
     26601 26601 26604 26604 26606 26615 26617 26617 26619 26619 26621
     26624 26626 26629 26631 26636 26638 26639 26641 26644 26646 26647
     26649 26649 26653 26655 26657 26658 26663 26669 26671 26676 26679
     26681 26683 26694 26696 26698 26700 26702 26704 26709 26711 26713
     26715 26717 26719 26729 26731 26731 26733 26738 26740 26743 26745
     26748 26750 26751 26753 26758 26760 26760 26765 26765 26767 26767
     26771 26772 26774 26776 26778 26781 26783 26787 26789 26794 26797
     26803 26805 26806 26809 26812 26816 26816 26818 26818 26820 26822
     26824 26829 26831 26842 26844 26845 26847 26849 26851 26851 26853
     26853 26855 26856 26858 26866 26869 26870 26873 26877 26880 26881
     26884 26886 26888 26899 26902 26903 26905 26908 26911 26920 26922
     26922 26925 26925 26928 26929 26931 26934 26936 26937 26939 26939
     26941 26941 26943 26943 26946 26946 26949 26949 26953 26954 26958
     26958 26963 26965 26967 26967 26969 26974 26976 26982 26984 26997
     26999 27010 27012 27012 27014 27018 27021 27022 27025 27026 27028
     27030 27032 27032 27035 27036 27040 27041 27045 27048 27051 27051
     27053 27055 27057 27058 27060 27060 27063 27064 27066 27068 27070
     27071 27073 27073 27075 27075 27077 27077 27079 27080 27082 27086
     27088 27089 27091 27092 27094 27097 27099 27099 27101 27104 27106
     27106 27109 27109 27111 27112 27114 27115 27117 27119 27121 27123
     27125 27125 27129 27129 27131 27131 27133 27139 27141 27141 27146
     27148 27151 27151 27153 27157 27159 27163 27165 27172 27176 27179
     27182 27186 27188 27195 27197 27199 27204 27211 27214 27214 27216
     27219 27221 27222 27224 27225 27227 27227 27231 27231 27233 27234
     27236 27239 27242 27243 27249 27251 27256 27257 27260 27260 27262
     27265 27267 27268 27270 27271 27273 27273 27275 27275 27277 27278
     27280 27281 27287 27287 27291 27296 27298 27299 27301 27301 27305
     27308 27310 27313 27315 27316 27320 27320 27323 27323 27325 27327
     27329 27331 27334 27334 27336 27337 27340 27340 27344 27345 27347
     27350 27354 27359 27364 27364 27367 27368 27370 27370 27372 27372
     27376 27378 27382 27382 27386 27389 27394 27399 27401 27402 27404
     27404 27407 27410 27414 27415 27419 27419 27421 27428 27431 27432
     27435 27436 27439 27439 27442 27442 27445 27451 27453 27455 27459
     27459 27462 27463 27465 27466 27468 27470 27472 27472 27474 27476
     27478 27478 27480 27481 27483 27483 27485 27485 27487 27495 27497
     27499 27502 27504 27506 27509 27511 27513 27515 27527 27529 27531
     27533 27533 27538 27539 27541 27544 27546 27547 27550 27556 27560
     27573 27575 27590 27593 27593 27595 27599 27602 27611 27615 27615
     27617 27617 27619 27619 27622 27623 27626 27628 27630 27631 27633
     27633 27635 27635 27637 27637 27639 27639 27641 27641 27645 27645
     27647 27647 27650 27650 27652 27657 27661 27675 27679 27679 27681
     27684 27686 27690 27692 27692 27694 27696 27698 27704 27706 27707
     27709 27714 27718 27719 27721 27723 27725 27728 27730 27730 27732
     27733 27735 27735 27737 27746 27748 27748 27751 27755 27757 27757
     27759 27760 27762 27764 27766 27766 27768 27771 27773 27774 27777
     27779 27781 27785 27788 27789 27791 27792 27794 27804 27807 27807
     27809 27815 27817 27819 27821 27822 27824 27828 27832 27839 27841
     27842 27844 27846 27849 27850 27852 27853 27855 27863 27865 27870
     27872 27875 27877 27877 27879 27884 27886 27896 27898 27902 27905
     27905 27908 27908 27911 27911 27914 27916 27918 27919 27921 27923
     27927 27927 27929 27931 27934 27935 27941 27947 27950 27951 27953
     27955 27957 27958 27960 27961 27963 27967 27969 27969 27971 27976
     27978 27979 27981 27983 27985 27988 27991 27991 27993 27994 27996
     27996 27998 28001 28003 28007 28009 28010 28012 28012 28014 28016
     28020 28020 28023 28025 28028 28028 28031 28031 28034 28034 28037
     28037 28039 28041 28044 28046 28049 28057 28059 28065 28067 28068
     28070 28076 28078 28079 28082 28082 28084 28085 28087 28089 28092
     28093 28095 28096 28099 28104 28106 28108 28110 28111 28113 28114
     28117 28118 28120 28123 28125 28130 28132 28134 28136 28140 28142
     28145 28147 28156 28160 28160 28164 28165 28167 28174 28176 28177
     28179 28183 28185 28187 28189 28199 28201 28201 28203 28207 28210
     28210 28212 28212 28214 28214 28216 28222 28227 28229 28232 28235
     28237 28239 28241 28244 28246 28248 28251 28255 28258 28259 28263
     28264 28267 28267 28270 28271 28274 28275 28278 28278 28282 28283
     28285 28288 28290 28291 28293 28294 28297 28297 28300 28301 28303
     28304 28307 28307 28310 28310 28312 28313 28316 28317 28319 28320
     28322 28322 28325 28325 28327 28327 28330 28331 28333 28335 28337
     28340 28342 28343 28346 28347 28349 28349 28351 28357 28359 28367
     28369 28369 28371 28373 28375 28375 28378 28378 28381 28386 28388
     28390 28392 28393 28395 28399 28402 28402 28404 28404 28407 28409
     28411 28411 28413 28415 28417 28418 28420 28420 28422 28422 28424
     28426 28428 28429 28431 28431 28433 28433 28435 28438 28440 28440
     28442 28443 28448 28448 28450 28452 28454 28454 28457 28461 28463
     28467 28470 28470 28472 28472 28475 28476 28478 28479 28481 28481
     28485 28487 28491 28491 28493 28493 28495 28495 28497 28500 28503
     28511 28513 28514 28516 28516 28518 28518 28520 28520 28524 28528
     28530 28530 28532 28532 28536 28536 28538 28538 28540 28542 28544
     28548 28550 28553 28555 28558 28560 28564 28566 28567 28570 28570
     28572 28572 28575 28577 28579 28584 28586 28586 28590 28593 28595
     28595 28597 28598 28601 28601 28604 28604 28606 28606 28608 28611
     28613 28618 28625 28626 28628 28629 28632 28632 28634 28635 28638
     28641 28644 28644 28648 28649 28651 28652 28654 28657 28659 28659
     28661 28662 28665 28666 28668 28670 28672 28673 28677 28679 28681
     28681 28683 28683 28685 28685 28687 28687 28689 28689 28693 28693
     28695 28696 28698 28704 28707 28707 28710 28712 28716 28716 28719
     28720 28722 28722 28724 28725 28727 28727 28729 28729 28732 28732
     28734 28734 28739 28740 28744 28748 28750 28753 28756 28757 28760
     28760 28765 28766 28771 28773 28779 28784 28789 28790 28792 28792
     28796 28801 28805 28806 28809 28810 28814 28814 28818 28818 28820
     28829 28833 28833 28836 28836 28843 28849 28851 28852 28855 28861
     28864 28867 28872 28872 28874 28875 28879 28879 28881 28881 28883
     28886 28888 28889 28891 28893 28895 28895 28900 28900 28902 28905
     28907 28909 28911 28911 28913 28913 28919 28919 28921 28922 28925
     28925 28931 28935 28937 28940 28943 28944 28947 28950 28952 28954
     28956 28956 28958 28958 28960 28961 28966 28966 28971 28971 28973
     28973 28975 28977 28982 28982 28984 28984 28988 28988 28993 28993
     28997 28999 29001 29004 29006 29006 29008 29008 29010 29010 29013
     29015 29017 29018 29020 29020 29022 29022 29024 29024 29026 29026
     29028 29033 29036 29036 29038 29038 29042 29043 29048 29050 29053
     29053 29056 29056 29060 29061 29063 29066 29068 29068 29071 29071
     29074 29074 29076 29077 29080 29083 29087 29088 29090 29090 29096
     29096 29100 29100 29103 29107 29109 29109 29113 29114 29118 29121
     29123 29124 29128 29129 29131 29132 29134 29134 29136 29136 29138
     29143 29145 29146 29148 29148 29151 29152 29157 29159 29164 29166
     29173 29173 29176 29177 29179 29180 29182 29184 29190 29193 29197
     29197 29200 29200 29203 29203 29207 29207 29210 29211 29213 29213
     29215 29215 29220 29220 29224 29224 29226 29229 29231 29234 29236
     29251 29253 29256 29259 29264 29266 29267 29269 29270 29272 29283
     29286 29289 29291 29291 29294 29295 29297 29298 29300 29301 29303
     29314 29316 29316 29319 29319 29321 29323 29325 29327 29330 29331
     29334 29334 29339 29339 29343 29344 29346 29346 29351 29352 29356
     29362 29364 29364 29366 29369 29374 29374 29376 29380 29382 29385
     29388 29390 29392 29392 29394 29394 29397 29401 29403 29403 29406
     29410 29413 29413 29416 29417 29420 29428 29431 29438 29441 29445
     29447 29447 29450 29451 29453 29454 29458 29459 29461 29465 29467
     29471 29473 29474 29476 29477 29479 29484 29486 29487 29489 29490
     29492 29499 29501 29503 29507 29509 29517 29520 29522 29522 29526
     29528 29533 29536 29539 29539 29542 29554 29557 29557 29559 29564
     29566 29566 29568 29569 29571 29575 29577 29577 29579 29579 29582
     29582 29584 29585 29587 29592 29595 29596 29598 29600 29602 29602
     29605 29607 29609 29611 29613 29616 29618 29619 29621 29621 29623
     29623 29625 29629 29631 29632 29634 29634 29637 29638 29640 29651
     29654 29654 29657 29657 29661 29662 29664 29665 29667 29671 29673
     29674 29677 29678 29681 29682 29684 29685 29687 29691 29693 29697
     29699 29703 29705 29706 29711 29713 29715 29715 29722 29723 29728
     29730 29732 29734 29736 29750 29752 29754 29756 29756 29759 29761
     29763 29764 29766 29767 29771 29771 29773 29773 29777 29778 29781
     29781 29783 29783 29785 29792 29794 29803 29805 29811 29814 29815
     29822 29822 29824 29827 29829 29831 29833 29833 29835 29835 29838
     29842 29848 29850 29852 29852 29854 29859 29861 29867 29870 29874
     29877 29877 29881 29883 29885 29885 29887 29887 29894 29894 29896
     29898 29900 29900 29903 29904 29906 29908 29912 29912 29914 29916
     29918 29920 29922 29924 29926 29931 29934 29938 29940 29940 29942
     29944 29946 29948 29951 29951 29953 29953 29955 29958 29964 29967
     29969 29971 29973 29980 29982 29985 29987 29997 29999 30003 30005
     30016 30019 30036 30039 30039 30041 30050 30052 30055 30057 30059
     30061 30061 30063 30079 30081 30083 30085 30087 30089 30091 30094
     30103 30105 30106 30108 30109 30111 30117 30123 30124 30126 30133
     30136 30138 30140 30154 30156 30159 30162 30162 30164 30169 30171
     30172 30174 30180 30182 30188 30190 30196 30201 30202 30204 30204
     30206 30213 30215 30221 30223 30224 30226 30227 30229 30233 30235
     30247 30249 30251 30253 30253 30256 30256 30258 30261 30264 30268
     30270 30286 30290 30290 30292 30294 30296 30297 30300 30300 30302
     30303 30305 30309 30311 30322 30324 30324 30326 30326 30328 30328
     30330 30334 30336 30344 30347 30350 30352 30353 30355 30355 30357
     30358 30361 30365 30367 30368 30370 30376 30378 30378 30381 30382
     30384 30386 30388 30388 30391 30394 30397 30397 30399 30399 30401
     30403 30405 30406 30408 30418 30420 30420 30422 30425 30427 30428
     30430 30433 30435 30440 30442 30442 30444 30444 30446 30450 30452
     30452 30454 30454 30456 30457 30459 30460 30462 30462 30464 30465
     30468 30468 30470 30478 30482 30482 30484 30485 30487 30487 30489
     30492 30494 30496 30498 30498 30500 30502 30504 30505 30509 30511
     30516 30522 30524 30526 30528 30531 30533 30535 30538 30538 30541
     30546 30550 30551 30554 30556 30558 30568 30570 30572 30576 30576
     30578 30580 30585 30586 30589 30592 30596 30597 30603 30606 30609
     30610 30612 30614 30618 30618 30622 30624 30626 30626 30629 30629
     30631 30631 30633 30634 30636 30641 30643 30643 30645 30646 30649
     30649 30651 30655 30659 30659 30663 30663 30665 30665 30669 30669
     30673 30674 30677 30677 30679 30679 30681 30684 30686 30688 30690
     30695 30697 30705 30707 30708 30710 30710 30712 30712 30715 30722
     30725 30726 30729 30729 30732 30734 30737 30738 30740 30744 30746
     30746 30748 30749 30751 30755 30757 30759 30761 30761 30764 30766
     30768 30768 30770 30770 30772 30773 30775 30780 30782 30784 30787
     30789 30791 30792 30796 30796 30798 30800 30802 30802 30805 30807
     30812 30814 30816 30817 30819 30820 30824 30824 30826 30831 30834
     30834 30836 30836 30839 30839 30842 30842 30844 30844 30846 30846
     30849 30849 30854 30855 30857 30858 30860 30863 30865 30865 30867
     30869 30871 30872 30874 30879 30881 30881 30883 30885 30887 30890
     30892 30893 30895 30901 30905 30911 30913 30913 30917 30924 30926
     30926 30928 30934 30937 30939 30943 30945 30948 30948 30950 30952
     30954 30954 30956 30956 30959 30959 30962 30967 30970 30971 30973
     30973 30975 30977 30981 30983 30988 30988 30990 30990 30992 30995
     30998 30998 31001 31002 31004 31004 31006 31008 31012 31015 31017
     31021 31025 31025 31028 31029 31034 31041 31044 31051 31055 31057
     31059 31064 31066 31072 31074 31074 31077 31077 31079 31081 31083
     31083 31085 31085 31087 31087 31090 31090 31095 31100 31102 31105
     31108 31109 31114 31119 31121 31121 31123 31126 31128 31128 31130
     31133 31137 31137 31142 31147 31150 31153 31155 31156 31160 31163
     31165 31172 31174 31179 31181 31181 31183 31183 31185 31186 31188
     31190 31192 31192 31194 31194 31197 31207 31209 31213 31215 31217
     31224 31224 31227 31229 31232 31232 31234 31235 31237 31246 31249
     31249 31252 31253 31255 31260 31262 31265 31267 31267 31271 31271
     31275 31275 31277 31296 31298 31305 31308 31313 31317 31319 31321
     31321 31324 31325 31327 31331 31333 31333 31335 31335 31337 31339
     31341 31341 31344 31344 31348 31354 31357 31366 31368 31368 31370
     31371 31373 31373 31376 31384 31388 31392 31395 31395 31397 31398
     31400 31402 31404 31408 31411 31411 31413 31414 31417 31420 31423
     31423 31427 31439 31441 31443 31445 31447 31449 31459 31461 31462
     31464 31469 31471 31473 31476 31476 31478 31478 31480 31483 31485
     31487 31490 31492 31494 31496 31498 31499 31503 31503 31505 31505
     31508 31509 31512 31513 31515 31515 31518 31520 31523 31537 31539
     31542 31544 31546 31548 31553 31557 31561 31563 31570 31572 31574
     31576 31576 31578 31579 31581 31581 31584 31584 31586 31586 31588
     31591 31593 31594 31596 31605 31607 31607 31609 31611 31613 31614
     31616 31616 31620 31623 31625 31625 31627 31627 31629 31634 31636
     31650 31653 31661 31663 31666 31668 31670 31672 31672 31674 31677
     31680 31682 31684 31692 31695 31695 31697 31700 31702 31703 31705
     31707 31709 31709 31712 31713 31716 31718 31720 31722 31725 31726
     31729 31738 31740 31740 31742 31742 31744 31748 31750 31751 31753
     31753 31755 31759 31761 31764 31766 31767 31769 31769 31771 31771
     31774 31777 31779 31779 31781 31784 31786 31788 31793 31793 31795
     31796 31798 31802 31805 31809 31811 31811 31814 31814 31818 31818
     31820 31821 31823 31830 31832 31841 31843 31845 31847 31847 31849
     31849 31852 31854 31856 31856 31858 31861 31865 31865 31867 31870
     31873 31875 31878 31879 31881 31881 31883 31883 31885 31885 31887
     31890 31892 31893 31895 31896 31899 31906 31908 31912 31914 31915
     31917 31918 31920 31923 31926 31927 31929 31938 31940 31941 31943
     31946 31948 31951 31954 31962 31964 31968 31970 31970 31974 31977
     31979 31979 31983 31983 31986 31986 31988 31990 31992 31992 31994
     31995 31998 31998 32000 32000 32002 32011 32013 32013 32015 32030
     32032 32035 32038 32039 32042 32051 32053 32053 32057 32058 32060
     32072 32075 32081 32083 32083 32086 32087 32089 32094 32097 32099
     32101 32104 32106 32106 32110 32110 32112 32115 32117 32123 32125
     32125 32127 32127 32129 32131 32133 32134 32136 32137 32139 32143
     32145 32145 32147 32147 32150 32151 32153 32160 32162 32163 32166
     32167 32170 32187 32189 32191 32194 32199 32202 32207 32209 32210
     32213 32218 32220 32222 32224 32230 32232 32237 32239 32239 32241
     32242 32244 32246 32249 32251 32256 32257 32260 32261 32264 32267
     32272 32274 32277 32277 32279 32279 32283 32291 32294 32296 32299
     32303 32305 32307 32309 32311 32313 32315 32317 32319 32321 32321
     32323 32327 32330 32331 32333 32334 32336 32336 32338 32338 32340
     32342 32344 32346 32349 32351 32353 32354 32357 32359 32361 32363
     32365 32368 32371 32371 32376 32377 32379 32383 32385 32387 32390
     32394 32396 32406 32408 32408 32410 32429 32431 32435 32437 32442
     32445 32469 32471 32483 32485 32491 32493 32504 32506 32521 32523
     32527 32529 32541 32543 32566 32568 32568 32570 32575 32578 32581
     32583 32583 32588 32597 32599 32600 32602 32605 32607 32619 32621
     32622 32624 32626 32628 32629 32631 32633 32637 32643 32645 32648
     32650 32657 32660 32660 32662 32663 32666 32666 32668 32671 32673
     32676 32678 32682 32685 32688 32690 32690 32692 32692 32694 32694
     32696 32697 32700 32701 32703 32705 32707 32707 32709 32710 32712
     32712 32714 32714 32716 32716 32718 32719 32722 32722 32724 32725
     32728 32728 32731 32731 32735 32737 32739 32739 32741 32742 32744
     32745 32747 32748 32750 32755 32761 32769 32771 32793 32796 32802
     32804 32810 32812 32814 32816 32817 32819 32832 32834 32836 32838
     32838 32842 32845 32850 32850 32852 32852 32854 32854 32856 32856
     32858 32858 32862 32866 32868 32868 32870 32870 32872 32874 32877
     32877 32879 32887 32889 32889 32893 32897 32899 32905 32907 32908
     32910 32910 32915 32915 32918 32918 32920 32920 32922 32930 32932
     32935 32937 32943 32945 32946 32948 32948 32951 32954 32956 32964
     32966 32966 32968 32968 32972 32975 32978 32978 32980 32990 32992
     32993 32996 32997 32999 33012 33014 33014 33016 33018 33020 33022
     33026 33027 33029 33035 33037 33044 33046 33048 33050 33052 33054
     33054 33056 33056 33059 33060 33063 33063 33065 33065 33067 33068
     33071 33075 33077 33078 33080 33082 33084 33084 33086 33086 33093
     33096 33098 33100 33102 33102 33104 33109 33111 33111 33113 33114
     33119 33121 33125 33129 33131 33131 33133 33137 33140 33140 33143
     33158 33160 33160 33162 33163 33166 33169 33171 33171 33173 33174
     33176 33176 33178 33182 33184 33184 33186 33188 33190 33190 33192
     33194 33198 33198 33200 33200 33202 33205 33208 33208 33210 33211
     33213 33219 33221 33222 33224 33231 33233 33233 33235 33235 33237
     33237 33239 33243 33245 33249 33251 33253 33255 33256 33258 33261
     33264 33270 33272 33290 33292 33296 33298 33300 33302 33311 33313
     33315 33320 33339 33342 33342 33344 33344 33347 33351 33353 33353
     33355 33355 33358 33359 33361 33361 33366 33366 33368 33370 33372
     33373 33375 33376 33378 33380 33382 33384 33386 33387 33389 33396
     33398 33403 33405 33412 33415 33419 33421 33423 33425 33426 33428
     33428 33430 33437 33439 33441 33443 33460 33463 33471 33473 33473
     33476 33480 33482 33493 33495 33500 33502 33512 33514 33515 33517
     33517 33519 33519 33521 33521 33523 33524 33526 33527 33529 33531
     33533 33534 33536 33548 33550 33551 33553 33553 33556 33560 33562
     33567 33569 33571 33575 33576 33579 33594 33596 33597 33600 33600
     33602 33607 33609 33610 33613 33624 33626 33628 33630 33633 33635
     33648 33651 33651 33653 33653 33655 33656 33659 33661 33663 33664
     33666 33666 33668 33671 33673 33674 33677 33678 33682 33686 33688
     33696 33698 33698 33702 33709 33712 33722 33724 33729 33733 33733
     33735 33735 33737 33738 33740 33740 33742 33745 33747 33748 33750
     33750 33752 33752 33756 33757 33759 33761 33765 33765 33768 33771
     33775 33778 33780 33780 33782 33785 33787 33789 33793 33793 33795
     33796 33798 33799 33802 33807 33809 33809 33811 33811 33813 33813
     33816 33817 33820 33821 33824 33824 33826 33826 33828 33834 33836
     33836 33839 33839 33841 33841 33845 33845 33848 33849 33852 33853
     33861 33866 33869 33871 33873 33874 33878 33884 33888 33895 33897
     33905 33907 33914 33916 33917 33921 33922 33924 33925 33927 33929
     33931 33932 33934 33934 33936 33936 33938 33941 33943 33943 33945
     33945 33948 33948 33950 33951 33953 33953 33958 33958 33960 33962
     33965 33965 33967 33967 33969 33970 33972 33972 33976 33986 33988
     33988 33990 33997 33999 34001 34003 34003 34006 34006 34009 34010
     34012 34013 34015 34016 34019 34019 34021 34023 34026 34026 34028
     34028 34030 34034 34036 34036 34039 34039 34042 34045 34047 34048
     34050 34051 34054 34055 34060 34060 34062 34062 34064 34065 34067
     34069 34071 34072 34074 34074 34076 34076 34078 34079 34081 34087
     34090 34093 34095 34095 34098 34109 34111 34113 34115 34115 34118
     34118 34120 34123 34126 34131 34133 34138 34140 34148 34152 34155
     34157 34157 34159 34159 34162 34162 34164 34164 34167 34167 34169
     34171 34173 34177 34180 34188 34191 34193 34195 34196 34199 34201
     34203 34205 34207 34208 34210 34210 34212 34224 34228 34228 34230
     34234 34236 34239 34241 34242 34247 34247 34249 34251 34253 34256
     34259 34259 34261 34261 34264 34264 34266 34266 34268 34269 34271
     34272 34276 34278 34280 34282 34285 34285 34291 34291 34294 34295
     34297 34300 34302 34304 34306 34306 34308 34311 34314 34315 34317
     34318 34320 34323 34326 34331 34334 34334 34337 34338 34343 34343
     34345 34345 34349 34349 34351 34352 34358 34358 34360 34360 34362
     34362 34364 34365 34367 34370 34374 34374 34381 34404 34407 34407
     34409 34409 34411 34412 34414 34415 34417 34417 34421 34423 34425
     34434 34440 34440 34442 34445 34449 34449 34451 34451 34453 34454
     34456 34456 34458 34458 34460 34461 34465 34465 34467 34468 34470
     34477 34479 34481 34483 34490 34495 34497 34499 34503 34505 34507
     34509 34514 34516 34517 34519 34524 34526 34528 34531 34533 34535
     34535 34537 34537 34540 34548 34552 34558 34560 34560 34562 34571
     34573 34581 34583 34586 34588 34588 34590 34591 34593 34595 34597
     34597 34600 34601 34606 34607 34609 34610 34612 34612 34615 34615
     34617 34624 34627 34627 34629 34629 34631 34633 34635 34638 34643
     34643 34645 34645 34647 34649 34653 34653 34655 34657 34659 34662
     34664 34664 34666 34666 34670 34674 34676 34676 34678 34678 34680
     34680 34683 34687 34690 34697 34699 34701 34704 34704 34707 34707
     34709 34709 34711 34713 34718 34720 34722 34723 34727 34728 34731
     34735 34737 34737 34739 34739 34741 34741 34746 34747 34749 34753
     34756 34756 34758 34763 34766 34766 34768 34771 34773 34774 34777
     34780 34783 34784 34786 34789 34794 34799 34801 34803 34806 34811
     34814 34815 34817 34817 34819 34819 34821 34822 34825 34827 34829
     34838 34840 34844 34846 34847 34849 34851 34855 34856 34861 34862
     34864 34866 34869 34870 34873 34876 34880 34886 34888 34894 34897
     34917 34920 34921 34923 34924 34926 34926 34928 34930 34933 34933
     34935 34935 34937 34937 34939 34939 34941 34946 34948 34949 34952
     34952 34955 34955 34957 34957 34962 34962 34966 34972 34974 34976
     34978 34978 34980 34980 34984 34984 34986 34987 34989 34990 34992
     34993 34996 34997 34999 34999 35002 35002 35004 35014 35017 35023
     35025 35029 35032 35033 35035 35039 35041 35045 35047 35048 35055
     35061 35063 35065 35068 35070 35073 35074 35076 35076 35078 35079
     35082 35082 35084 35088 35090 35091 35093 35094 35096 35102 35104
     35105 35109 35112 35114 35115 35120 35122 35124 35126 35128 35131
     35134 35134 35136 35142 35145 35145 35148 35149 35151 35151 35154
     35154 35158 35159 35162 35164 35166 35172 35174 35174 35178 35179
     35181 35184 35186 35189 35191 35191 35194 35199 35201 35201 35203
     35203 35206 35211 35213 35213 35215 35216 35219 35224 35226 35228
     35231 35233 35237 35239 35241 35242 35244 35244 35247 35248 35250
     35255 35258 35258 35260 35261 35263 35266 35268 35276 35278 35282
     35284 35288 35290 35290 35292 35294 35299 35299 35301 35303 35305
     35305 35307 35307 35309 35309 35311 35311 35313 35313 35315 35316
     35318 35318 35320 35321 35325 35325 35327 35328 35330 35333 35335
     35336 35338 35338 35340 35340 35342 35352 35355 35355 35357 35360
     35362 35366 35370 35373 35375 35375 35377 35377 35379 35383 35386
     35390 35392 35393 35395 35395 35397 35401 35405 35406 35408 35416
     35419 35422 35424 35427 35429 35431 35433 35433 35435 35438 35440
     35443 35445 35447 35449 35452 35454 35456 35458 35463 35465 35469
     35471 35475 35477 35482 35486 35489 35491 35498 35500 35504 35506
     35507 35510 35511 35513 35513 35515 35516 35518 35519 35522 35524
     35526 35533 35535 35535 35537 35543 35546 35554 35556 35556 35558
     35559 35562 35566 35568 35569 35571 35576 35578 35578 35580 35580
     35582 35586 35588 35591 35594 35596 35598 35598 35600 35601 35604
     35604 35606 35607 35609 35617 35622 35622 35624 35624 35627 35629
     35632 35632 35635 35635 35639 35639 35641 35641 35644 35644 35646
     35646 35649 35654 35656 35657 35660 35663 35666 35668 35670 35670
     35672 35676 35678 35679 35683 35683 35686 35686 35691 35693 35695
     35698 35700 35700 35702 35705 35708 35710 35712 35713 35715 35717
     35722 35728 35730 35734 35737 35738 35740 35740 35742 35755 35757
     35760 35762 35770 35772 35782 35784 35791 35793 35817 35819 35848
     35850 35869 35871 35898 35901 35903 35905 35905 35909 35916 35918
     35921 35923 35925 35927 35931 35933 35933 35937 35940 35942 35942
     35944 35949 35955 35955 35957 35958 35960 35964 35966 35966 35970
     35970 35973 35975 35977 35982 35984 35984 35986 35988 35992 35993
     35995 35998 36000 36002 36004 36004 36007 36016 36018 36020 36022
     36029 36031 36043 36045 36047 36049 36049 36051 36051 36053 36054
     36057 36062 36064 36068 36070 36070 36072 36072 36074 36074 36076
     36077 36079 36080 36082 36082 36084 36085 36087 36088 36090 36095
     36097 36097 36099 36101 36103 36107 36109 36109 36111 36112 36114
     36116 36118 36119 36123 36123 36125 36127 36129 36176 36179 36182
     36184 36190 36192 36199 36201 36201 36203 36206 36208 36209 36211
     36215 36223 36223 36225 36226 36228 36229 36232 36232 36234 36235
     36237 36237 36240 36241 36244 36245 36249 36249 36254 36256 36259
     36259 36262 36262 36264 36264 36267 36268 36271 36271 36273 36277
     36279 36284 36286 36288 36290 36296 36298 36300 36302 36303 36305
     36305 36308 36311 36313 36315 36317 36319 36321 36321 36323 36325
     36327 36328 36330 36332 36335 36341 36343 36349 36351 36351 36353
     36353 36356 36358 36360 36364 36367 36369 36372 36372 36374 36374
     36381 36387 36390 36391 36393 36394 36396 36396 36398 36401 36403
     36410 36413 36413 36416 36418 36420 36420 36423 36434 36436 36437
     36441 36441 36443 36452 36454 36454 36457 36457 36460 36461 36463
     36466 36468 36468 36470 36470 36473 36476 36479 36479 36481 36485
     36487 36487 36489 36491 36493 36493 36495 36501 36505 36510 36513
     36514 36519 36519 36521 36531 36533 36533 36538 36539 36542 36542
     36544 36545 36547 36552 36554 36559 36561 36562 36564 36564 36571
     36572 36575 36575 36578 36579 36584 36584 36587 36587 36589 36590
     36592 36593 36599 36606 36608 36608 36610 36611 36613 36613 36615
     36618 36620 36620 36623 36624 36626 36633 36635 36641 36643 36643
     36645 36650 36652 36655 36659 36667 36670 36679 36681 36681 36684
     36687 36689 36693 36695 36696 36700 36703 36705 36713 36715 36735
     36737 36747 36749 36753 36755 36769 36771 36777 36779 36779 36781
     36786 36789 36802 36804 36808 36810 36811 36813 36814 36816 36821
     36824 36832 36834 36838 36840 36843 36845 36859 36861 36862 36864
     36870 36872 36891 36893 36899 36902 36906 36908 36911 36913 36921
     36923 36924 36926 36927 36929 36933 36935 36935 36937 36953 36955
     36958 36960 36963 36965 36969 36972 36976 36978 36978 36980 36986
     36988 36989 36991 36997 36999 37004 37006 37009 37011 37011 37013
     37013 37015 37017 37019 37019 37021 37021 37024 37027 37029 37030
     37032 37032 37034 37034 37036 37036 37038 37046 37048 37051 37053
     37054 37057 37057 37059 37061 37063 37064 37066 37066 37068 37068
     37070 37075 37077 37077 37079 37081 37083 37087 37089 37090 37092
     37096 37099 37099 37101 37101 37103 37104 37108 37112 37117 37120
     37122 37122 37124 37126 37128 37128 37133 37133 37136 37136 37138
     37138 37140 37146 37148 37148 37150 37150 37152 37152 37154 37155
     37157 37157 37159 37159 37161 37161 37165 37170 37172 37172 37174
     37175 37177 37178 37180 37181 37187 37187 37190 37200 37202 37204
     37206 37211 37213 37214 37217 37221 37223 37223 37225 37226 37228
     37243 37245 37247 37249 37251 37253 37255 37257 37262 37264 37269
     37271 37272 37274 37276 37278 37278 37281 37284 37286 37286 37288
     37288 37290 37302 37304 37304 37306 37309 37311 37315 37317 37329
     37331 37332 37334 37343 37345 37345 37347 37351 37353 37354 37356
     37361 37365 37367 37369 37369 37371 37373 37375 37377 37380 37383
     37385 37386 37388 37390 37392 37400 37404 37406 37411 37414 37416
     37417 37420 37420 37422 37424 37427 37434 37436 37436 37438 37440
     37442 37451 37453 37457 37463 37470 37472 37474 37476 37481 37486
     37489 37492 37497 37499 37504 37507 37507 37509 37509 37512 37514
     37517 37518 37521 37523 37525 37532 37535 37536 37540 37541 37543
     37544 37547 37547 37549 37551 37554 37555 37558 37565 37567 37571
     37573 37576 37579 37584 37586 37587 37589 37589 37591 37593 37596
     37597 37599 37601 37603 37605 37607 37610 37612 37614 37616 37616
     37618 37619 37624 37628 37631 37632 37634 37634 37636 37636 37638
     37638 37640 37640 37645 37645 37647 37649 37652 37653 37656 37658
     37660 37676 37678 37679 37682 37687 37690 37691 37694 37694 37700
     37700 37703 37707 37709 37709 37712 37714 37716 37720 37722 37724
     37726 37726 37728 37728 37732 37733 37735 37735 37737 37738 37740
     37745 37747 37750 37754 37754 37756 37762 37768 37768 37770 37773
     37775 37775 37778 37778 37780 37784 37786 37787 37790 37790 37793
     37793 37795 37796 37798 37801 37803 37806 37808 37808 37812 37814
     37817 37818 37825 37825 37827 37837 37840 37841 37843 37843 37846
     37849 37852 37855 37857 37858 37860 37864 37878 37883 37885 37885
     37889 37892 37895 37897 37901 37904 37907 37914 37919 37919 37921
     37921 37925 37925 37931 37931 37934 37935 37937 37942 37944 37944
     37946 37947 37949 37951 37953 37953 37955 37957 37960 37960 37962
     37962 37964 37964 37969 37971 37973 37973 37977 37980 37982 37987
     37992 37992 37994 37995 37997 38002 38005 38005 38007 38007 38012
     38015 38017 38017 38019 38032 38034 38037 38039 38039 38041 38086
     38088 38094 38096 38098 38101 38105 38107 38117 38119 38138 38140
     38171 38173 38175 38177 38182 38184 38194 38196 38204 38206 38210
     38212 38215 38217 38218 38220 38228 38230 38233 38235 38239 38241
     38253 38255 38259 38262 38265 38270 38272 38274 38276 38279 38287
     38289 38292 38294 38294 38296 38297 38301 38313 38315 38317 38321
     38322 38324 38324 38326 38326 38329 38335 38339 38339 38342 38349
     38352 38358 38360 38362 38364 38370 38372 38374 38376 38379 38381
     38398 38400 38406 38408 38418 38420 38423 38425 38426 38428 38431
     38433 38434 38436 38438 38440 38440 38442 38442 38444 38444 38446
     38447 38449 38461 38463 38466 38468 38473 38475 38477 38479 38480
     38482 38482 38484 38488 38491 38495 38497 38506 38508 38508 38510
     38510 38512 38512 38514 38520 38522 38527 38529 38534 38536 38539
     38541 38545 38548 38557 38559 38560 38563 38570 38574 38580 38582
     38590 38592 38593 38596 38599 38601 38607 38609 38610 38613 38614
     38616 38624 38626 38627 38632 38635 38639 38643 38646 38647 38649
     38651 38654 38654 38656 38666 38669 38671 38673 38673 38675 38675
     38678 38678 38681 38686 38689 38692 38695 38696 38698 38698 38701
     38701 38704 38707 38712 38713 38717 38718 38721 38724 38726 38726
     38728 38730 38734 38735 38737 38739 38741 38748 38750 38750 38752
     38763 38765 38766 38769 38769 38771 38772 38774 38781 38783 38785
     38788 38790 38793 38793 38795 38795 38797 38797 38799 38802 38804
     38810 38812 38812 38814 38816 38818 38819 38822 38822 38824 38824
     38827 38831 38833 38838 38840 38842 38844 38844 38846 38847 38849
     38849 38851 38862 38864 38865 38867 38868 38871 38873 38875 38878
     38880 38881 38884 38884 38886 38887 38889 38895 38897 38904 38906
     38907 38911 38915 38917 38920 38922 38922 38924 38932 38934 38938
     38940 38940 38942 38942 38944 38945 38947 38950 38955 38960 38962
     38965 38967 38969 38971 38974 38980 38980 38982 38983 38986 38991
     38993 39003 39006 39006 39010 39011 39013 39015 39018 39020 39023
     39025 39027 39050 39052 39053 39055 39057 39059 39060 39062 39064
     39066 39074 39076 39080 39082 39083 39085 39089 39092 39092 39094
     39096 39098 39099 39103 39103 39106 39112 39116 39116 39118 39118
     39121 39123 39125 39125 39128 39132 39134 39135 39137 39139 39141
     39147 39149 39151 39154 39156 39158 39158 39164 39166 39170 39171
     39173 39173 39175 39178 39180 39181 39184 39192 39194 39202 39204
     39204 39206 39208 39211 39212 39214 39214 39217 39221 39225 39230
     39232 39234 39237 39241 39243 39246 39248 39250 39252 39253 39255
     39257 39259 39260 39262 39264 39267 39267 39269 39269 39271 39282
     39284 39287 39290 39290 39292 39293 39295 39297 39300 39304 39306
     39307 39309 39309 39311 39321 39323 39323 39325 39325 39327 39327
     39333 39334 39336 39336 39340 39342 39344 39349 39353 39354 39356
     39357 39359 39359 39361 39361 39363 39366 39368 39369 39376 39381
     39384 39391 39394 39394 39399 39399 39402 39406 39408 39410 39412
     39413 39416 39417 39419 39419 39421 39423 39425 39429 39432 39432
     39435 39436 39438 39443 39446 39446 39449 39449 39454 39454 39456
     39456 39458 39460 39463 39464 39467 39467 39469 39470 39472 39472
     39475 39475 39477 39480 39486 39486 39488 39493 39495 39495 39498
     39502 39505 39505 39508 39511 39514 39515 39517 39517 39519 39519
     39522 39522 39524 39525 39529 39537 39539 39554 39556 39560 39562
     39564 39567 39571 39574 39576 39578 39589 39591 39592 39594 39594
     39596 39602 39604 39612 39614 39622 39624 39624 39627 39628 39630
     39640 39643 39644 39646 39655 39657 39663 39665 39669 39671 39671
     39673 39675 39677 39677 39679 39686 39688 39689 39691 39696 39698
     39699 39702 39702 39704 39708 39711 39712 39714 39715 39717 39723
     39725 39727 39729 39733 39735 39735 39737 39741 39745 39749 39751
     39753 39755 39759 39761 39761 39764 39768 39770 39771 39774 39774
     39777 39777 39779 39779 39781 39782 39784 39784 39786 39791 39795
     39797 39799 39801 39807 39808 39811 39815 39817 39819 39821 39828
     39830 39831 39834 39834 39837 39840 39846 39854 39856 39858 39860
     39860 39863 39865 39867 39868 39870 39873 39878 39882 39886 39890
     39892 39892 39894 39896 39899 39899 39901 39901 39903 39903 39905
     39909 39911 39912 39914 39915 39919 39923 39925 39925 39927 39930
     39933 39933 39935 39936 39938 39938 39940 39940 39942 39942 39944
     39949 39951 39958 39960 39964 39966 39966 39969 39978 39981 39986
     39989 39991 39993 39995 39997 39998 40001 40001 40003 40010 40014
     40016 40018 40020 40022 40024 40026 40032 40035 40035 40039 40043
     40046 40046 40048 40048 40050 40050 40053 40056 40059 40060 40063
     40063 40065 40066 40069 40072 40075 40075 40077 40078 40080 40082
     40084 40085 40090 40092 40094 40105 40107 40107 40109 40110 40112
     40120 40122 40125 40131 40135 40138 40144 40147 40153 40156 40159
     40162 40162 40165 40167 40169 40169 40171 40172 40176 40176 40178
     40180 40182 40183 40185 40185 40194 40195 40198 40201 40203 40203
     40206 40206 40209 40210 40213 40213 40215 40216 40219 40223 40227
     40227 40230 40230 40232 40232 40234 40236 40239 40240 40242 40244
     40250 40255 40257 40264 40266 40266 40272 40273 40275 40276 40281
     40281 40284 40293 40297 40300 40303 40304 40306 40306 40310 40311
     40314 40316 40318 40318 40323 40324 40326 40327 40329 40330 40333
     40335 40338 40339 40341 40344 40346 40346 40353 40353 40356 40356
     40361 40364 40366 40367 40369 40370 40372 40373 40376 40380 40383
     40383 40385 40388 40390 40391 40393 40394 40399 40399 40403 40407
     40409 40410 40414 40416 40421 40423 40425 40425 40427 40427 40429
     40432 40434 40436 40440 40442 40445 40446 40450 40450 40455 40455
     40458 40458 40462 40462 40464 40466 40469 40470 40473 40483 40485
     40486 40488 40493 40495 40495 40497 40499 40501 40506 40509 40511
     40513 40524 40526 40527 40529 40529 40533 40533 40535 40536 40538
     40540 40542 40542 40547 40548 40550 40557 40560 40561 40563 40563
     40565 40565 40568 40581 40583 40584 40587 40588 40590 40591 40593
     40595 40597 40600 40603 40603 40605 40607 40612 40614 40616 40618
     40620 40624 40627 40629 40632 40639 40643 40644 40646 40646 40648
     40649 40651 40658 40660 40661 40664 40665 40667 40672 40674 40674
     40676 40677 40679 40682 40684 40690 40692 40697 40699 40703 40706
     40707 40711 40713 40715 40715 40717 40727 40729 40731 40735 40738
     40742 40742 40746 40748 40751 40751 40753 40754 40756 40756 40759
     40759 40761 40767 40769 40769 40771 40775 40778 40779 40782 40792
     40794 40794 40797 40803 40806 40810 40812 40819 40821 40823 40826
     40826 40829 40829 40831 40832 40835 40845 40847 40850 40852 40855
     40857 40867 40869 40869 44032 44033 44036 44036 44039 44042 44048
     44055 44057 44061 44064 44064 44068 44068 44076 44077 44079 44081
     44088 44089 44092 44092 44096 44096 44107 44107 44109 44109 44116
     44116 44120 44120 44124 44124 44144 44145 44148 44148 44151 44152
     44154 44154 44160 44161 44163 44166 44169 44172 44176 44176 44180
     44180 44188 44189 44191 44193 44200 44202 44204 44204 44207 44208
     44216 44217 44219 44221 44225 44225 44228 44228 44232 44232 44236
     44236 44245 44245 44247 44247 44256 44257 44260 44260 44263 44264
     44266 44266 44268 44268 44271 44273 44275 44275 44277 44278 44284
     44285 44288 44288 44292 44292 44294 44294 44300 44301 44303 44303
     44305 44305 44312 44312 44316 44316 44320 44320 44329 44329 44332
     44333 44340 44341 44344 44344 44348 44348 44356 44357 44359 44359
     44361 44361 44368 44368 44372 44372 44376 44376 44385 44385 44387
     44387 44396 44397 44400 44400 44403 44406 44411 44413 44415 44415
     44417 44418 44424 44425 44428 44428 44432 44432 44444 44445 44452
     44452 44471 44471 44480 44481 44484 44484 44488 44488 44496 44497
     44499 44499 44508 44508 44512 44512 44516 44516 44536 44537 44540
     44540 44543 44545 44552 44553 44555 44555 44557 44557 44564 44564
     44592 44593 44596 44596 44599 44600 44602 44602 44608 44609 44611
     44611 44613 44614 44618 44618 44620 44622 44624 44624 44628 44628
     44630 44630 44636 44637 44639 44641 44645 44645 44648 44649 44652
     44652 44656 44656 44664 44665 44667 44669 44676 44677 44684 44684
     44732 44734 44736 44736 44740 44740 44748 44749 44751 44753 44760
     44761 44764 44764 44776 44776 44779 44779 44781 44781 44788 44788
     44792 44792 44796 44796 44807 44808 44813 44813 44816 44816 44844
     44845 44848 44848 44850 44850 44852 44852 44860 44861 44863 44863
     44865 44867 44872 44873 44880 44880 44892 44893 44900 44901 44921
     44921 44928 44928 44932 44932 44936 44936 44944 44945 44949 44949
     44956 44956 44984 44985 44988 44988 44992 44992 44999 45001 45003
     45003 45005 45006 45012 45012 45020 45020 45032 45033 45040 45041
     45044 45044 45048 45048 45056 45057 45060 45060 45068 45068 45072
     45072 45076 45076 45084 45085 45096 45096 45124 45125 45128 45128
     45130 45130 45132 45132 45134 45134 45139 45141 45143 45143 45145
     45145 45149 45149 45180 45181 45184 45184 45188 45188 45196 45197
     45199 45199 45201 45201 45208 45210 45212 45212 45215 45218 45224
     45225 45227 45231 45233 45233 45235 45237 45240 45240 45244 45244
     45252 45253 45255 45257 45264 45265 45268 45268 45272 45272 45280
     45280 45285 45285 45320 45321 45323 45324 45328 45328 45330 45331
     45336 45337 45339 45341 45347 45349 45352 45352 45356 45356 45364
     45365 45367 45369 45376 45377 45380 45380 45384 45384 45392 45393
     45396 45397 45400 45400 45404 45404 45408 45408 45432 45433 45436
     45436 45440 45440 45442 45442 45448 45449 45451 45451 45453 45453
     45458 45460 45464 45464 45468 45468 45480 45480 45516 45516 45520
     45520 45524 45524 45532 45533 45535 45535 45544 45545 45548 45548
     45552 45552 45561 45561 45563 45563 45565 45565 45572 45573 45576
     45576 45579 45580 45588 45589 45591 45591 45593 45593 45600 45600
     45620 45620 45628 45628 45656 45656 45660 45660 45664 45664 45672
     45673 45684 45685 45692 45692 45700 45701 45705 45705 45712 45713
     45716 45716 45720 45722 45728 45729 45731 45731 45733 45734 45738
     45738 45740 45740 45744 45744 45748 45748 45768 45769 45772 45772
     45776 45776 45778 45778 45784 45785 45787 45787 45789 45789 45794
     45794 45796 45798 45800 45800 45803 45807 45811 45813 45815 45819
     45823 45825 45828 45828 45832 45832 45840 45841 45843 45845 45852
     45852 45908 45910 45912 45912 45915 45916 45918 45919 45924 45925
     45927 45927 45929 45929 45931 45931 45934 45934 45936 45937 45940
     45940 45944 45944 45952 45953 45955 45957 45964 45964 45968 45968
     45972 45972 45984 45985 45992 45992 45996 45996 46020 46021 46024
     46024 46027 46028 46030 46030 46032 46032 46036 46037 46039 46039
     46041 46041 46043 46043 46045 46045 46048 46048 46052 46052 46056
     46056 46076 46076 46096 46096 46104 46104 46108 46108 46112 46112
     46120 46121 46123 46123 46132 46132 46160 46161 46164 46164 46168
     46168 46176 46177 46179 46179 46181 46181 46188 46188 46208 46208
     46216 46216 46237 46237 46244 46244 46248 46248 46252 46252 46261
     46261 46263 46263 46265 46265 46272 46272 46276 46276 46280 46280
     46288 46288 46293 46293 46300 46301 46304 46304 46307 46308 46310
     46310 46316 46317 46319 46319 46321 46321 46328 46328 46356 46357
     46360 46360 46363 46364 46372 46373 46375 46378 46384 46385 46388
     46388 46392 46392 46400 46401 46403 46405 46411 46413 46416 46416
     46420 46420 46428 46429 46431 46433 46496 46497 46500 46500 46504
     46504 46506 46507 46512 46513 46515 46517 46523 46525 46528 46528
     46532 46532 46540 46541 46543 46545 46552 46552 46572 46572 46608
     46609 46612 46612 46616 46616 46629 46629 46636 46636 46644 46644
     46664 46664 46692 46692 46696 46696 46748 46749 46752 46752 46756
     46756 46763 46764 46769 46769 46804 46804 46832 46832 46836 46836
     46840 46840 46848 46849 46853 46853 46888 46889 46892 46892 46895
     46896 46904 46905 46907 46907 46916 46916 46920 46920 46924 46924
     46932 46933 46944 46944 46948 46948 46952 46952 46960 46961 46963
     46963 46965 46965 46972 46973 46976 46976 46980 46980 46988 46989
     46991 46994 46998 47001 47004 47004 47008 47008 47016 47017 47019
     47021 47028 47029 47032 47032 47047 47047 47049 47049 47084 47085
     47088 47088 47092 47092 47100 47101 47103 47105 47111 47113 47116
     47116 47120 47120 47128 47129 47131 47131 47133 47133 47140 47141
     47144 47144 47148 47148 47156 47157 47159 47161 47168 47168 47172
     47172 47185 47185 47187 47187 47196 47197 47200 47200 47204 47204
     47212 47213 47215 47215 47217 47217 47224 47224 47228 47228 47245
     47245 47272 47272 47280 47280 47284 47284 47288 47288 47296 47297
     47299 47299 47301 47301 47308 47308 47312 47312 47316 47316 47325
     47325 47327 47327 47329 47329 47336 47337 47340 47340 47344 47344
     47352 47353 47355 47355 47357 47357 47364 47364 47384 47384 47392
     47392 47420 47421 47424 47424 47428 47428 47436 47436 47439 47439
     47441 47441 47448 47449 47452 47452 47456 47456 47464 47465 47467
     47467 47469 47469 47476 47477 47480 47480 47484 47484 47492 47493
     47495 47495 47497 47498 47501 47502 47532 47533 47536 47536 47540
     47540 47548 47549 47551 47551 47553 47553 47560 47561 47564 47564
     47566 47570 47576 47577 47579 47579 47581 47582 47585 47585 47587
     47589 47592 47592 47596 47596 47604 47605 47607 47610 47616 47617
     47624 47624 47637 47637 47672 47673 47676 47676 47680 47680 47682
     47682 47688 47689 47691 47691 47693 47694 47699 47701 47704 47704
     47708 47708 47716 47717 47719 47721 47728 47729 47732 47732 47736
     47736 47747 47749 47751 47751 47756 47756 47784 47785 47787 47788
     47792 47792 47794 47794 47800 47801 47803 47803 47805 47805 47812
     47812 47816 47816 47832 47833 47868 47868 47872 47872 47876 47876
     47885 47885 47887 47887 47889 47889 47896 47896 47900 47900 47904
     47904 47913 47913 47915 47915 47924 47926 47928 47928 47931 47934
     47940 47941 47943 47943 47945 47945 47949 47949 47951 47952 47956
     47956 47960 47960 47969 47969 47971 47971 47980 47980 48008 48008
     48012 48012 48016 48016 48036 48036 48040 48040 48044 48044 48052
     48052 48055 48055 48064 48064 48068 48068 48072 48072 48080 48080
     48083 48083 48120 48121 48124 48124 48127 48128 48130 48130 48136
     48137 48139 48141 48143 48143 48145 48145 48148 48152 48155 48159
     48164 48165 48167 48167 48169 48169 48173 48173 48176 48177 48180
     48180 48184 48184 48192 48193 48195 48197 48201 48201 48204 48205
     48208 48208 48221 48221 48260 48261 48264 48264 48267 48268 48270
     48270 48276 48277 48279 48279 48281 48282 48288 48289 48292 48292
     48295 48296 48304 48305 48307 48309 48316 48317 48320 48320 48324
     48324 48333 48333 48335 48337 48341 48341 48344 48344 48348 48348
     48372 48374 48376 48376 48380 48380 48388 48389 48391 48391 48393
     48393 48400 48400 48404 48404 48420 48420 48428 48428 48448 48448
     48456 48457 48460 48460 48464 48464 48472 48473 48484 48484 48488
     48488 48512 48513 48516 48516 48519 48522 48528 48529 48531 48531
     48533 48533 48537 48538 48540 48540 48548 48548 48560 48560 48568
     48568 48596 48597 48600 48600 48604 48604 48617 48617 48624 48624
     48628 48628 48632 48632 48640 48640 48643 48643 48645 48645 48652
     48653 48656 48656 48660 48660 48668 48669 48671 48671 48708 48709
     48712 48712 48716 48716 48718 48718 48724 48725 48727 48727 48729
     48731 48736 48737 48740 48740 48744 48744 48746 48746 48752 48753
     48755 48757 48763 48765 48768 48768 48772 48772 48780 48781 48783
     48785 48792 48793 48808 48808 48848 48849 48852 48852 48855 48856
     48864 48864 48867 48869 48876 48876 48897 48897 48904 48905 48920
     48921 48923 48925 48960 48961 48964 48964 48968 48968 48976 48977
     48981 48981 49044 49044 49072 49072 49093 49093 49100 49101 49104
     49104 49108 49108 49116 49116 49119 49119 49121 49121 49212 49212
     49233 49233 49240 49240 49244 49244 49248 49248 49256 49257 49296
     49297 49300 49300 49304 49304 49312 49313 49315 49315 49317 49317
     49324 49325 49327 49328 49331 49334 49340 49341 49343 49345 49349
     49349 49352 49353 49356 49356 49360 49360 49368 49369 49371 49373
     49380 49381 49384 49384 49388 49388 49396 49397 49399 49399 49401
     49401 49408 49408 49412 49412 49416 49416 49424 49424 49429 49429
     49436 49440 49443 49444 49446 49447 49452 49453 49455 49457 49462
     49462 49464 49465 49468 49468 49472 49472 49480 49481 49483 49485
     49492 49493 49496 49496 49500 49500 49508 49509 49511 49513 49520
     49520 49524 49524 49528 49528 49541 49541 49548 49550 49552 49552
     49556 49556 49558 49558 49564 49565 49567 49567 49569 49569 49573
     49573 49576 49577 49580 49580 49584 49584 49597 49597 49604 49604
     49608 49608 49612 49612 49620 49620 49623 49624 49632 49632 49636
     49636 49640 49640 49648 49649 49651 49651 49660 49661 49664 49664
     49668 49668 49676 49677 49679 49679 49681 49681 49688 49689 49692
     49692 49695 49696 49704 49705 49707 49707 49709 49709 49711 49711
     49713 49714 49716 49716 49736 49736 49744 49745 49748 49748 49752
     49752 49760 49760 49765 49765 49772 49773 49776 49776 49780 49780
     49788 49789 49791 49791 49793 49793 49800 49801 49808 49808 49816
     49816 49819 49819 49821 49821 49828 49829 49832 49832 49836 49837
     49844 49845 49847 49847 49849 49849 49884 49885 49888 49888 49891
     49892 49899 49901 49903 49903 49905 49905 49910 49910 49912 49913
     49915 49916 49920 49920 49928 49929 49932 49933 49939 49941 49944
     49944 49948 49948 49956 49957 49960 49961 49989 49989 50024 50025
     50028 50028 50032 50032 50034 50034 50040 50041 50044 50045 50052
     50052 50056 50056 50060 50060 50112 50112 50136 50137 50140 50140
     50143 50144 50146 50146 50152 50153 50157 50157 50164 50165 50168
     50168 50184 50184 50192 50192 50212 50212 50220 50220 50224 50224
     50228 50228 50236 50237 50248 50248 50276 50277 50280 50280 50284
     50284 50292 50293 50297 50297 50304 50304 50324 50324 50332 50332
     50360 50360 50364 50364 50409 50409 50416 50417 50420 50420 50424
     50424 50426 50426 50431 50433 50444 50444 50448 50448 50452 50452
     50460 50460 50472 50473 50476 50476 50480 50480 50488 50489 50491
     50491 50493 50493 50500 50501 50504 50506 50508 50510 50515 50517
     50519 50521 50525 50526 50528 50529 50532 50532 50536 50536 50544
     50545 50547 50549 50556 50557 50560 50560 50564 50564 50567 50567
     50572 50573 50575 50575 50577 50577 50581 50581 50583 50584 50588
     50588 50592 50592 50601 50601 50612 50613 50616 50617 50619 50622
     50628 50634 50636 50636 50638 50638 50640 50641 50644 50644 50648
     50648 50656 50657 50659 50659 50661 50661 50668 50670 50672 50672
     50676 50676 50678 50679 50684 50689 50693 50696 50700 50700 50704
     50704 50712 50713 50715 50716 50724 50725 50728 50728 50732 50734
     50736 50736 50739 50741 50743 50743 50745 50745 50747 50747 50752
     50753 50756 50756 50760 50760 50768 50769 50771 50773 50780 50781
     50784 50784 50796 50796 50799 50799 50801 50801 50808 50809 50812
     50812 50816 50816 50824 50825 50827 50827 50829 50829 50836 50837
     50840 50840 50844 50844 50852 50853 50855 50855 50857 50857 50864
     50865 50868 50868 50872 50874 50880 50881 50883 50883 50885 50885
     50892 50893 50896 50896 50900 50900 50908 50909 50912 50913 50920
     50921 50924 50924 50928 50928 50936 50937 50941 50941 50948 50949
     50952 50952 50956 50956 50964 50965 50967 50967 50969 50969 50976
     50977 50980 50980 50984 50984 50992 50993 50995 50995 50997 50997
     50999 50999 51004 51005 51008 51008 51012 51012 51018 51018 51020
     51021 51023 51023 51025 51032 51036 51036 51040 51040 51048 51048
     51051 51051 51060 51061 51064 51064 51068 51070 51075 51077 51079
     51082 51086 51086 51088 51089 51092 51092 51094 51096 51098 51098
     51104 51105 51107 51110 51116 51117 51120 51120 51124 51124 51132
     51133 51135 51137 51144 51145 51148 51148 51150 51150 51152 51152
     51160 51160 51165 51165 51172 51172 51176 51176 51180 51180 51200
     51201 51204 51204 51208 51208 51210 51210 51216 51217 51219 51219
     51221 51222 51228 51229 51232 51232 51236 51236 51244 51245 51247
     51247 51249 51249 51256 51256 51260 51260 51264 51264 51272 51273
     51276 51277 51284 51284 51312 51313 51316 51316 51320 51320 51322
     51322 51328 51329 51331 51331 51333 51335 51339 51341 51348 51348
     51357 51357 51359 51359 51361 51361 51368 51368 51388 51389 51396
     51396 51400 51400 51404 51404 51412 51413 51415 51415 51417 51417
     51424 51425 51428 51428 51445 51445 51452 51453 51456 51456 51460
     51462 51468 51469 51471 51471 51473 51473 51480 51480 51500 51500
     51508 51508 51536 51537 51540 51540 51544 51544 51552 51553 51555
     51555 51564 51564 51568 51568 51572 51572 51580 51580 51592 51593
     51596 51596 51600 51600 51608 51609 51611 51611 51613 51613 51648
     51649 51652 51652 51655 51656 51658 51658 51664 51665 51667 51667
     51669 51670 51673 51674 51676 51677 51680 51680 51682 51682 51684
     51684 51687 51687 51692 51693 51695 51697 51704 51705 51708 51708
     51712 51712 51720 51721 51723 51725 51732 51732 51736 51736 51753
     51753 51788 51789 51792 51792 51796 51796 51804 51805 51807 51809
     51816 51816 51837 51837 51844 51844 51864 51864 51900 51901 51904
     51904 51908 51908 51916 51917 51919 51919 51921 51921 51923 51923
     51928 51929 51936 51936 51948 51948 51956 51956 51976 51976 51984
     51984 51988 51988 51992 51992 52000 52001 52033 52033 52040 52041
     52044 52044 52048 52048 52056 52057 52061 52061 52068 52068 52088
     52089 52124 52124 52152 52152 52180 52180 52196 52196 52199 52199
     52201 52201 52236 52237 52240 52240 52244 52244 52252 52253 52257
     52258 52263 52265 52268 52268 52270 52270 52272 52272 52280 52281
     52283 52286 52292 52293 52296 52296 52300 52300 52308 52309 52311
     52313 52320 52320 52324 52324 52326 52326 52328 52328 52336 52336
     52341 52341 52376 52377 52380 52380 52384 52384 52392 52393 52395
     52397 52404 52405 52408 52408 52412 52412 52420 52421 52423 52423
     52425 52425 52432 52432 52436 52436 52452 52452 52460 52460 52464
     52464 52481 52481 52488 52489 52492 52492 52496 52496 52504 52505
     52507 52507 52509 52509 52516 52516 52520 52520 52524 52524 52537
     52537 52572 52572 52576 52576 52580 52580 52588 52589 52591 52591
     52593 52593 52600 52600 52616 52616 52628 52629 52632 52632 52636
     52636 52644 52645 52647 52647 52649 52649 52656 52656 52676 52676
     52684 52684 52688 52688 52712 52712 52716 52716 52720 52720 52728
     52729 52731 52731 52733 52733 52740 52740 52744 52744 52748 52748
     52756 52756 52761 52761 52768 52769 52772 52772 52776 52776 52784
     52785 52787 52787 52789 52789 52824 52825 52828 52828 52831 52833
     52840 52841 52843 52843 52845 52845 52852 52853 52856 52856 52860
     52860 52868 52869 52871 52871 52873 52873 52880 52881 52884 52884
     52888 52888 52896 52897 52899 52901 52908 52909 52929 52929 52964
     52965 52968 52968 52971 52972 52980 52981 52983 52985 52992 52993
     52996 52996 53000 53000 53008 53009 53011 53011 53013 53013 53020
     53020 53024 53024 53028 53028 53036 53037 53039 53041 53048 53048
     53076 53077 53080 53080 53084 53084 53092 53093 53095 53095 53097
     53097 53104 53105 53108 53108 53112 53112 53120 53120 53125 53125
     53132 53132 53153 53153 53160 53160 53168 53168 53188 53188 53216
     53217 53220 53220 53224 53224 53232 53233 53235 53235 53237 53237
     53244 53244 53248 53248 53252 53252 53265 53265 53272 53272 53293
     53293 53300 53301 53304 53304 53308 53308 53316 53317 53319 53319
     53321 53321 53328 53328 53332 53332 53336 53336 53344 53344 53356
     53357 53360 53360 53364 53364 53372 53373 53377 53377 53412 53413
     53416 53416 53420 53420 53428 53429 53431 53431 53433 53433 53440
     53441 53444 53444 53448 53449 53456 53457 53459 53461 53468 53469
     53472 53472 53476 53476 53484 53485 53487 53489 53496 53496 53517
     53517 53552 53553 53556 53556 53560 53560 53562 53562 53568 53569
     53571 53573 53580 53581 53584 53584 53588 53588 53596 53597 53599
     53599 53601 53601 53608 53608 53612 53612 53628 53628 53636 53636
     53640 53640 53664 53665 53668 53668 53672 53672 53680 53681 53683
     53683 53685 53685 53690 53690 53692 53692 53696 53696 53720 53720
     53748 53748 53752 53752 53767 53767 53769 53769 53776 53776 53804
     53805 53808 53808 53812 53812 53820 53821 53823 53823 53825 53825
     53832 53832 53852 53852 53860 53860 53888 53889 53892 53892 53896
     53896 53904 53905 53909 53909 53916 53916 53920 53920 53924 53924
     53932 53932 53937 53937 53944 53945 53948 53948 53951 53952 53954
     53954 53960 53961 53963 53963 53972 53972 53976 53976 53980 53980
     53988 53989 54000 54001 54004 54004 54008 54008 54016 54017 54019
     54019 54021 54021 54028 54030 54032 54032 54036 54036 54038 54038
     54044 54045 54047 54049 54053 54053 54056 54057 54060 54060 54064
     54064 54072 54073 54075 54077 54084 54085 54140 54141 54144 54144
     54148 54148 54156 54157 54159 54161 54168 54169 54172 54172 54176
     54176 54184 54185 54187 54187 54189 54189 54196 54196 54200 54200
     54204 54204 54212 54213 54216 54217 54224 54224 54232 54232 54241
     54241 54243 54243 54252 54253 54256 54256 54260 54260 54268 54269
     54271 54271 54273 54273 54280 54280 54301 54301 54336 54336 54340
     54340 54364 54364 54368 54368 54372 54372 54381 54381 54383 54383
     54392 54393 54396 54396 54399 54400 54402 54402 54408 54409 54411
     54411 54413 54413 54420 54420 54441 54441 54476 54476 54480 54480
     54484 54484 54492 54492 54495 54495 54504 54504 54508 54508 54512
     54512 54520 54520 54523 54523 54525 54525 54532 54532 54536 54536
     54540 54540 54548 54549 54551 54551 54588 54589 54592 54592 54596
     54596 54604 54605 54607 54607 54609 54609 54616 54617 54620 54620
     54624 54624 54629 54629 54632 54633 54635 54635 54637 54637 54644
     54645 54648 54648 54652 54652 54660 54661 54663 54665 54672 54672
     54693 54693 54728 54729 54732 54732 54736 54736 54738 54738 54744
     54745 54747 54747 54749 54749 54756 54757 54760 54760 54764 54764
     54772 54773 54775 54775 54777 54777 54784 54785 54788 54788 54792
     54792 54800 54801 54803 54805 54812 54812 54816 54816 54820 54820
     54829 54829 54840 54841 54844 54844 54848 54848 54853 54853 54856
     54857 54859 54859 54861 54861 54865 54865 54868 54869 54872 54872
     54876 54876 54887 54887 54889 54889 54896 54897 54900 54900 54915
     54915 54917 54917 54924 54925 54928 54928 54932 54932 54941 54941
     54943 54943 54945 54945 54952 54952 54956 54956 54960 54960 54969
     54969 54971 54971 54980 54981 54984 54984 54988 54988 54993 54993
     54996 54996 54999 54999 55001 55001 55008 55008 55012 55012 55016
     55016 55024 55024 55029 55029 55036 55037 55040 55040 55044 55044
     55057 55057 55064 55065 55068 55068 55072 55072 55080 55081 55083
     55083 55085 55085 55092 55093 55096 55096 55100 55100 55108 55108
     55111 55111 55113 55113 55120 55121 55124 55124 55126 55129 55136
     55137 55139 55139 55141 55141 55145 55145 55148 55148 55152 55152
     55156 55156 55164 55165 55169 55169 55176 55177 55180 55180 55184
     55184 55192 55193 55195 55195 55197 55197 63744 64011 65281 65374
     65377 65439 65504 65507 65509 65510 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 2259 :NAME "TIS-620" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Thai Industrial Standards Institute (TISI)                             [Tantsetthi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 3585 3642 3647 3675 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 8 :NAME "ISO_8859-5:1988" :ALIASES
   '("csISOLatinCyrillic" "cyrillic" #8="ISO-8859-5" "ISO_8859-5"
     "iso-ir-144")
   :MIME-ENCODING '#8# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: ISO-8859-5 (preferred MIME name)") :REFERENCES
   '("[RFC1345,KXS2]") :RANGES
   #(0 160 167 167 173 173 1025 1036 1038 1103 1105 1116 1118 1119 8470
     8470))
  (MAKE-CHARACTER-SET :MIB-ENUM 9 :NAME "ISO_8859-6:1987" :ALIASES
   '("csISOLatinArabic" "arabic" "ASMO-708" "ECMA-114" #9="ISO-8859-6"
     "ISO_8859-6" "iso-ir-127")
   :MIME-ENCODING '#9# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: ISO-8859-6 (preferred MIME name)") :REFERENCES
   '("[RFC1345,KXS2]") :RANGES
   #(0 160 164 164 173 173 1548 1548 1563 1563 1567 1567 1569 1594 1600
     1618))
  (MAKE-CHARACTER-SET :MIB-ENUM 2046 :NAME "IBM855" :ALIASES
   '("csIBM855" "855" "cp855") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 160 164 164 167 167 171 171 173 173 187 187 1025 1036
     1038 1103 1105 1116 1118 1119 8470 8470 9472 9472 9474 9474 9484
     9484 9488 9488 9492 9492 9496 9496 9500 9500 9508 9508 9516 9516
     9524 9524 9532 9532 9552 9553 9556 9556 9559 9559 9562 9562 9565
     9565 9568 9568 9571 9571 9574 9574 9577 9577 9580 9580 9600 9600
     9604 9604 9608 9608 9617 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 10 :NAME "ISO_8859-7:1987" :ALIASES
   '("csISOLatinGreek" "greek8" "greek" "ECMA-118" "ELOT_928"
     #10="ISO-8859-7" "ISO_8859-7" "iso-ir-126")
   :MIME-ENCODING '#10# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: ISO-8859-7 (preferred MIME name)") :REFERENCES
   '("[RFC1947,RFC1345,KXS2]") :RANGES
   #(0 160 163 163 166 169 171 173 176 179 183 183 187 187 189 189 890
     890 900 902 904 906 908 908 910 929 931 974 8213 8213 8216 8217
     8364 8364 8367 8367))
  (MAKE-CHARACTER-SET :MIB-ENUM 13 :NAME #11="ISO-8859-10" :ALIASES
   '("latin6" "csISOLatin6" "ISO_8859-10:1992" "l6" "iso-ir-157")
   :MIME-ENCODING '#11# :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 160 167 167 173 173 176 176 183 183 193 198 201 201 203 203 205
     208 211 214 216 216 218 223 225 230 233 233 235 235 237 240 243 246
     248 248 250 254 256 257 260 261 268 269 272 275 278 281 290 291 296
     299 302 303 310 312 315 316 325 326 330 333 352 353 358 363 370 371
     381 382 8213 8213))
  (MAKE-CHARACTER-SET :MIB-ENUM 4 :NAME "ISO_8859-1:1987" :ALIASES
   '("csISOLatin1" "CP819" "IBM819" "l1" "latin1" #12="ISO-8859-1"
     "ISO_8859-1" "iso-ir-100")
   :MIME-ENCODING '#12# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: ISO-8859-1 (preferred MIME name)") :REFERENCES
   '("[RFC1345,KXS2]") :RANGES #(0 255))
  (MAKE-CHARACTER-SET :MIB-ENUM 106 :NAME "UTF-8" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE '"RFC 3629" :COMMENTS 'NIL :REFERENCES
   '("[RFC3629]") :RANGES #(0 1114111))
  (MAKE-CHARACTER-SET :MIB-ENUM 105 :NAME "ISO-2022-CN-EXT" :ALIASES
   'NIL :MIME-ENCODING 'NIL :SOURCE '"RFC-1922" :COMMENTS 'NIL
   :REFERENCES '("[RFC1922]") :RANGES
   #(0 127 162 165 167 168 176 177 183 183 215 215 224 225 232 234 236
     237 242 243 247 247 249 250 252 252 257 257 275 275 283 283 299 299
     324 324 328 328 333 333 363 363 462 462 464 464 466 466 468 468 470
     470 472 472 474 474 476 476 593 593 609 609 711 711 713 715 729 729
     913 929 931 937 945 961 963 969 1025 1025 1040 1103 1105 1105 7743
     7743 8211 8214 8216 8217 8220 8221 8229 8230 8240 8240 8242 8243
     8245 8245 8251 8251 8254 8254 8451 8451 8453 8453 8457 8457 8470
     8470 8544 8555 8560 8569 8592 8595 8598 8601 8712 8712 8719 8719
     8721 8721 8725 8725 8730 8730 8733 8736 8741 8741 8743 8747 8750
     8750 8756 8759 8764 8765 8776 8776 8780 8780 8786 8786 8800 8801
     8804 8807 8814 8815 8857 8857 8869 8869 8895 8895 8978 8978 9216
     9247 9249 9249 9312 9321 9332 9371 9472 9547 9552 9552 9566 9566
     9569 9569 9578 9578 9581 9587 9601 9615 9620 9621 9632 9633 9650
     9651 9660 9661 9670 9671 9675 9675 9678 9679 9698 9701 9733 9734
     9737 9737 9792 9794 12288 12291 12293 12293 12296 12311 12317 12318
     12321 12329 12343 12343 12353 12435 12449 12534 12539 12539 12549
     12585 12832 12841 12963 12963 12992 13003 13144 13168 13198 13199
     13212 13214 13217 13217 13252 13252 13262 13262 13265 13266 13269
     13269 13280 13310 13312 13313 13316 13317 13324 13324 13334 13334
     13340 13340 13345 13345 13348 13348 13352 13353 13355 13358 13360
     13364 13368 13372 13377 13381 13385 13393 13399 13407 13411 13415
     13422 13425 13427 13431 13433 13454 13457 13463 13465 13473 13476
     13483 13485 13485 13487 13488 13490 13503 13506 13509 13511 13516
     13518 13521 13523 13528 13530 13540 13543 13545 13548 13551 13553
     13566 13568 13575 13578 13587 13589 13589 13591 13594 13596 13598
     13600 13610 13612 13650 13652 13660 13662 13671 13673 13683 13686
     13692 13696 13703 13711 13720 13727 13738 13748 13773 13776 13776
     13779 13788 13794 13805 13808 13814 13819 13826 13829 13837 13840
     13841 13843 13846 13849 13869 13871 13876 13878 13883 13887 13893
     13895 13898 13901 13907 13909 13909 13913 13918 13920 13925 13927
     13932 13935 13948 13950 13950 13952 13957 13959 13959 13961 13968
     13970 13976 13978 13978 13980 13998 14000 14015 14017 14021 14026
     14026 14029 14046 14053 14078 14082 14099 14101 14110 14112 14123
     14126 14149 14151 14152 14154 14154 14156 14169 14171 14176 14178
     14183 14185 14194 14196 14220 14223 14236 14239 14239 14241 14253
     14255 14263 14265 14273 14275 14277 14279 14292 14294 14304 14309
     14317 14319 14326 14328 14338 14340 14365 14368 14370 14373 14378
     14381 14383 14385 14386 14388 14395 14397 14412 14414 14432 14434
     14435 14437 14443 14445 14459 14461 14470 14472 14497 14499 14499
     14501 14506 14508 14508 14510 14512 14514 14518 14520 14520 14522
     14526 14528 14537 14539 14548 14552 14560 14562 14562 14564 14566
     14571 14573 14575 14578 14581 14583 14586 14591 14593 14614 14617
     14634 14636 14636 14638 14651 14654 14678 14682 14697 14699 14714
     14716 14727 14729 14744 14746 14751 14753 14768 14770 14770 14772
     14798 14802 14810 14817 14831 14833 14871 14873 14890 14893 14912
     14915 14926 14928 14928 14930 14942 14944 14954 14956 14957 14959
     14967 14969 14978 14980 14981 14983 14985 14987 14991 14993 14995
     14997 14998 15002 15002 15004 15014 15016 15017 15019 15025 15028
     15036 15038 15045 15050 15051 15053 15061 15063 15073 15076 15079
     15081 15083 15086 15101 15105 15120 15122 15125 15127 15134 15136
     15139 15141 15143 15145 15158 15160 15161 15163 15164 15167 15167
     15169 15172 15175 15176 15178 15180 15185 15189 15192 15202 15208
     15218 15224 15240 15243 15263 15265 15265 15267 15267 15269 15290
     15295 15312 15315 15334 15338 15347 15349 15355 15358 15366 15368
     15377 15380 15387 15389 15415 15417 15439 15442 15442 15444 15452
     15454 15464 15466 15469 15471 15478 15480 15503 15506 15528 15530
     15533 15535 15550 15552 15560 15562 15570 15574 15582 15588 15598
     15603 15626 15631 15646 15653 15672 15675 15686 15690 15705 15709
     15739 15741 15745 15748 15752 15756 15759 15761 15768 15770 15772
     15774 15777 15779 15780 15782 15784 15786 15792 15794 15797 15801
     15804 15806 15808 15810 15819 15821 15833 15839 15846 15851 15856
     15859 15865 15867 15868 15870 15877 15880 15923 15925 15934 15936
     15943 15945 15975 15979 15983 15985 16005 16007 16012 16014 16024
     16026 16033 16035 16046 16048 16053 16055 16058 16063 16068 16072
     16078 16081 16087 16093 16097 16103 16104 16107 16114 16117 16122
     16125 16127 16130 16130 16132 16132 16135 16135 16137 16196 16198
     16206 16208 16210 16213 16242 16244 16245 16247 16251 16253 16304
     16310 16319 16321 16335 16337 16339 16341 16351 16353 16395 16397
     16412 16414 16420 16423 16447 16449 16469 16471 16480 16482 16489
     16491 16522 16524 16536 16538 16551 16553 16564 16566 16578 16583
     16591 16593 16606 16608 16615 16617 16622 16624 16635 16637 16649
     16651 16661 16664 16669 16671 16674 16676 16690 16694 16696 16698
     16711 16714 16734 16736 16745 16748 16773 16776 16779 16781 16813
     16815 16819 16821 16835 16837 16841 16843 16882 16885 16894 16896
     16935 16938 16966 16968 16995 16997 17035 17037 17057 17059 17092
     17096 17116 17118 17162 17164 17205 17218 17230 17232 17247 17249
     17306 17308 17309 17311 17316 17318 17323 17325 17364 17366 17372
     17374 17388 17391 17413 17415 17449 17451 17468 17470 17493 17495
     17512 17514 17517 17519 17526 17529 17533 17535 17542 17544 17552
     17554 17560 17562 17581 17584 17597 17601 17619 17623 17639 17645
     17658 17660 17701 17705 17729 17731 17743 17745 17755 17757 17762
     17764 17781 17784 17795 17797 17835 17837 17853 17855 17877 17879
     17900 17902 17906 17908 17912 17914 17914 17916 17946 17948 17949
     17951 17958 17960 17969 17971 17993 17998 18002 18004 18026 18028
     18037 18039 18042 18044 18068 18070 18083 18085 18091 18093 18130
     18132 18210 18218 18226 18228 18248 18250 18264 18266 18266 18268
     18299 18301 18310 18312 18315 18321 18353 18355 18417 18420 18443
     18445 18453 18455 18457 18459 18489 18491 18499 18501 18544 18546
     18551 18553 18554 18556 18559 18563 18565 18567 18571 18573 18574
     18576 18582 18585 18594 18596 18617 18619 18632 18634 18641 18643
     18661 18663 18674 18676 18687 18689 18722 18724 18728 18730 18737
     18739 18747 18749 18758 18760 18770 18772 18779 18781 18808 18823
     18824 18826 18842 18844 18856 18858 18863 18865 18869 18873 18877
     18881 18887 18889 18894 18896 18920 18922 18922 18924 18924 18926
     18969 18971 19011 19013 19021 19023 19030 19032 19102 19104 19113
     19115 19121 19123 19278 19280 19291 19293 19305 19307 19394 19398
     19432 19434 19450 19452 19462 19464 19501 19503 19506 19508 19509
     19511 19560 19563 19571 19573 19590 19593 19607 19609 19612 19621
     19637 19639 19684 19686 19704 19706 19730 19738 19751 19753 19884
     19887 19889 19891 19893 19968 19973 19975 20022 20024 20035 20037
     20041 20043 20052 20054 20058 20060 20067 20070 20070 20072 20074
     20080 20081 20083 20085 20089 20089 20094 20105 20107 20111 20113
     20137 20139 20156 20158 20177 20179 20204 20206 20264 20266 20387
     20389 20394 20396 20396 20398 20451 20453 20538 20540 20540 20542
     20721 20723 20846 20848 20857 20859 20889 20891 20902 20904 20904
     20906 20965 20968 20968 20971 21093 21095 21104 21106 21134 21136
     21248 21251 21376 21378 21392 21394 21428 21430 21435 21437 21497
     21499 21501 21503 21523 21525 21654 21656 21659 21661 21719 21721
     21722 21724 21787 21789 21789 21792 21792 21794 21796 21798 21864
     21866 21880 21882 21935 21937 21943 21945 21947 21949 21996 21998
     22000 22002 22041 22043 22048 22050 22052 22054 22106 22108 22108
     22110 22137 22139 22176 22179 22179 22181 22221 22223 22228 22231
     22247 22249 22309 22311 22326 22329 22407 22410 22441 22443 22446
     22449 22450 22452 22485 22488 22549 22551 22591 22594 22637 22639
     22639 22641 22709 22711 22711 22714 22730 22733 22742 22744 22756
     22758 22761 22763 22764 22766 22813 22815 22842 22844 23153 23155
     23281 23283 23349 23351 23389 23391 23404 23406 23440 23442 23464
     23466 23496 23498 23553 23555 23589 23591 23596 23599 23601 23603
     23618 23620 23669 23671 23682 23684 23739 23741 23741 23743 23745
     23747 23748 23750 23775 23777 23899 23901 23925 23927 24041 24043
     24043 24046 24059 24061 24136 24138 24163 24165 24291 24293 24328
     24330 24335 24337 24341 24343 24384 24386 24388 24390 24411 24413
     24633 24635 24637 24639 24694 24696 25075 25077 25155 25157 25172
     25175 25191 25193 25367 25369 25382 25384 25524 25526 25528 25530
     25530 25532 25594 25597 25603 25605 25665 25667 25820 25822 25980
     25983 25989 25991 26047 26049 26068 26070 26072 26074 26174 26176
     26242 26244 26361 26364 26364 26366 26408 26410 26415 26417 26432
     26434 26465 26469 26469 26471 26527 26530 26533 26535 26536 26538
     26625 26628 26636 26638 26710 26712 26714 26718 26718 26720 26764
     26766 26809 26812 26812 26815 26901 26903 26904 26908 26912 26916
     26917 26921 27005 27008 27008 27010 27017 27021 27100 27103 27178
     27180 27181 27183 27183 27185 27249 27254 27255 27257 27258 27260
     27336 27338 27363 27365 27382 27384 27396 27398 27403 27405 27412
     27414 27422 27424 27481 27483 27568 27570 27614 27616 27629 27631
     27661 27663 27670 27672 27705 27707 27719 27721 27745 27747 27979
     27981 28059 28061 28178 28180 28180 28182 28281 28283 28289 28291
     28299 28301 28569 28571 28664 28666 28749 28751 28751 28753 28947
     28949 28968 28970 28987 28989 28989 28991 28991 28993 29012 29014
     29076 29078 29172 29176 29234 29236 29275 29277 29343 29345 29470
     29473 29538 29541 29680 29682 29712 29714 29714 29716 29926 29928
     29935 29939 29943 29945 29947 29949 29956 29958 30003 30005 30011
     30013 30033 30036 30047 30049 30056 30058 30066 30068 30081 30083
     30092 30094 30124 30126 30184 30186 30213 30215 30300 30302 30309
     30311 30329 30331 30475 30477 30610 30612 30663 30665 30666 30668
     30782 30784 30810 30812 30833 30837 30900 30902 30934 30936 30964
     30966 30997 30999 31029 31031 31219 31221 31227 31229 31267 31269
     31298 31300 31311 31313 31313 31315 31324 31326 31330 31332 31426
     31428 31446 31448 31489 31491 31494 31496 31544 31546 31548 31550
     31609 31611 31611 31614 31641 31643 31694 31696 31697 31699 31762
     31765 31766 31768 31792 31794 31822 31824 31840 31842 31869 31871
     31873 31875 31886 31889 31914 31916 31916 31918 31935 31937 31959
     31961 31992 31994 32037 32039 32074 32076 32153 32156 32211 32214
     32260 32263 32295 32297 32329 32332 32358 32360 32386 32388 32399
     32401 32571 32573 32584 32586 32594 32596 32597 32599 32681 32683
     32865 32867 33034 33037 33074 33076 33076 33078 33078 33080 33109
     33112 33187 33190 33205 33207 33234 33236 33243 33245 33302 33304
     33325 33327 33356 33358 33363 33365 33372 33374 33375 33377 33476
     33479 33624 33626 33716 33718 33722 33724 33782 33784 33816 33818
     33823 33825 33825 33827 33924 33926 33929 33931 34008 34010 34011
     34013 34101 34103 34220 34222 34321 34323 34323 34325 34351 34353
     34371 34373 34474 34476 34508 34510 34542 34544 34627 34629 34634
     34636 34686 34688 34725 34728 34767 34769 34989 34991 34991 34993
     35010 35013 35014 35016 35075 35077 35099 35102 35140 35142 35142
     35144 35175 35177 35190 35192 35255 35257 35328 35330 35378 35380
     35500 35503 35545 35547 35616 35618 35674 35676 35697 35699 35726
     35728 35791 35793 36045 36047 36324 36326 36341 36343 36365 36367
     36396 36398 36476 36479 36479 36481 36506 36508 36525 36527 36532
     36534 36538 36540 36541 36543 36548 36550 36551 36553 36577 36580
     36619 36621 36683 36685 36713 36715 36747 36749 36790 36792 36793
     36797 36825 36827 36849 36851 36902 36904 36920 36922 36949 36951
     36963 36965 37073 37075 37210 37212 37221 37223 37270 37272 37283
     37285 37303 37305 37319 37321 37329 37331 37344 37346 37369 37373
     37415 37419 37419 37421 37488 37490 37494 37496 37550 37552 37617
     37619 37689 37692 37695 37698 37699 37701 37764 37766 37766 37768
     37816 37819 37822 37824 37871 37873 37920 37922 37924 37927 37952
     37954 37970 37972 38215 38217 38218 38220 38228 38230 38281 38283
     38293 38295 38303 38305 38313 38315 38350 38352 38373 38375 38500
     38503 38521 38523 38634 38636 38731 38733 38733 38736 38768 38770
     38789 38791 38799 38801 38955 38957 39081 39083 39091 39093 39108
     39110 39135 39137 39149 39151 39182 39184 39265 39267 39282 39284
     39304 39306 39364 39366 39409 39411 39441 39444 39463 39465 39773
     39775 39799 39802 39829 39832 39856 39859 39859 39861 39885 39890
     39920 39924 39924 39926 39959 39963 39982 39985 40006 40008 40025
     40029 40040 40044 40047 40049 40060 40063 40066 40068 40072 40074
     40075 40077 40078 40080 40105 40107 40107 40109 40110 40112 40120
     40122 40125 40127 40129 40131 40135 40138 40144 40146 40153 40155
     40160 40162 40163 40165 40175 40177 40205 40207 40234 40237 40261
     40265 40291 40294 40332 40336 40362 40364 40389 40391 40506 40509
     40542 40544 40557 40559 40561 40563 40567 40569 40632 40635 40638
     40640 40736 40738 40801 40803 40869 65072 65074 65077 65092 65097
     65102 65104 65106 65108 65111 65113 65126 65128 65131 65281 65374
     65504 65505 65507 65507 65509 65509 131072 131072 131075 131077
     131079 131081 131083 131083 131090 131090 131092 131092 131096
     131098 131104 131106 131109 131109 131113 131113 131116 131118
     131131 131131 131137 131137 131139 131139 131144 131144 131157
     131157 131159 131159 131169 131170 131172 131175 131177 131177
     131179 131179 131181 131182 131184 131186 131188 131188 131191
     131191 131197 131199 131202 131202 131204 131204 131206 131208
     131213 131213 131215 131215 131220 131221 131224 131225 131228
     131228 131230 131230 131232 131232 131234 131237 131240 131241
     131244 131244 131246 131251 131253 131255 131257 131258 131260
     131263 131266 131268 131270 131270 131273 131273 131275 131275
     131279 131282 131285 131286 131294 131297 131300 131301 131303
     131303 131309 131309 131312 131315 131318 131318 131320 131320
     131324 131326 131332 131333 131335 131343 131345 131351 131353
     131355 131357 131364 131368 131368 131372 131374 131376 131377
     131380 131381 131383 131383 131385 131385 131395 131395 131400
     131400 131404 131404 131407 131407 131411 131412 131414 131417
     131422 131427 131433 131436 131440 131440 131456 131458 131462
     131463 131465 131466 131468 131468 131470 131471 131474 131474
     131484 131485 131491 131492 131494 131494 131496 131497 131501
     131501 131503 131504 131506 131506 131508 131516 131522 131523
     131531 131531 131535 131536 131544 131550 131569 131570 131572
     131572 131582 131585 131587 131601 131605 131606 131618 131618
     131623 131626 131628 131647 131666 131666 131668 131670 131672
     131672 131681 131694 131699 131699 131702 131702 131722 131724
     131729 131730 131732 131732 131734 131734 131746 131758 131760
     131771 131774 131774 131777 131778 131793 131798 131803 131803
     131805 131805 131809 131811 131815 131840 131848 131849 131851
     131852 131863 131864 131868 131868 131872 131873 131881 131895
     131897 131900 131902 131904 131908 131908 131912 131912 131928
     131928 131934 131935 131938 131938 131943 131948 131950 131963
     131970 131971 131992 131995 131998 131998 132001 132002 132014
     132019 132022 132036 132039 132040 132042 132042 132052 132052
     132054 132055 132065 132068 132071 132080 132084 132084 132096
     132096 132102 132102 132104 132110 132122 132123 132129 132136
     132145 132147 132149 132150 132154 132159 132162 132162 132164
     132165 132172 132174 132177 132179 132183 132187 132191 132191
     132196 132196 132201 132203 132207 132207 132210 132210 132214
     132215 132217 132219 132221 132225 132227 132230 132234 132234
     132236 132237 132240 132240 132246 132246 132248 132249 132254
     132254 132258 132258 132264 132264 132269 132269 132272 132274
     132277 132278 132280 132281 132287 132287 132294 132294 132296
     132296 132299 132300 132302 132303 132311 132311 132315 132317
     132319 132321 132324 132325 132327 132327 132330 132331 132333
     132334 132337 132337 132339 132340 132342 132344 132349 132349
     132352 132356 132358 132364 132367 132368 132370 132371 132373
     132381 132383 132384 132389 132389 132391 132393 132399 132399
     132401 132401 132406 132406 132408 132409 132412 132414 132417
     132420 132424 132424 132426 132427 132429 132430 132432 132436
     132438 132438 132440 132440 132442 132443 132445 132448 132450
     132457 132460 132462 132464 132464 132466 132467 132469 132469
     132473 132475 132484 132484 132490 132490 132492 132492 132499
     132500 132503 132503 132506 132506 132508 132508 132511 132512
     132515 132515 132518 132519 132521 132521 132523 132523 132529
     132529 132534 132537 132546 132550 132552 132558 132567 132573
     132578 132578 132581 132584 132593 132598 132602 132608 132612
     132615 132618 132621 132626 132626 132630 132632 132637 132639
     132641 132645 132647 132647 132649 132649 132655 132655 132657
     132659 132661 132664 132668 132670 132678 132683 132687 132690
     132695 132698 132703 132704 132706 132706 132717 132717 132725
     132731 132734 132737 132742 132743 132747 132747 132749 132751
     132753 132754 132758 132761 132763 132763 132765 132767 132769
     132769 132771 132771 132773 132778 132780 132788 132791 132804
     132809 132809 132813 132827 132832 132832 132837 132850 132852
     132857 132859 132859 132866 132866 132870 132871 132873 132873
     132876 132876 132880 132893 132895 132896 132901 132902 132904
     132904 132912 132933 132937 132937 132946 132946 132952 132955
     132957 132965 132967 132970 132977 132977 132989 133008 133016
     133016 133020 133021 133028 133042 133047 133047 133051 133055
     133058 133080 133084 133084 133087 133087 133094 133105 133107
     133107 133109 133109 133114 133127 133131 133131 133133 133136
     133138 133138 133143 133143 133147 133150 133152 133155 133158
     133159 133162 133163 133166 133172 133174 133177 133179 133179
     133185 133188 133196 133197 133201 133205 133214 133218 133222
     133222 133227 133234 133240 133241 133243 133248 133252 133257
     133262 133263 133266 133271 133273 133273 133280 133287 133293
     133293 133295 133300 133302 133303 133305 133308 133311 133313
     133315 133327 133329 133330 133332 133332 133334 133338 133342
     133345 133347 133355 133358 133361 133363 133365 133368 133370
     133373 133376 133380 133381 133383 133387 133390 133397 133399
     133404 133406 133407 133410 133411 133413 133413 133418 133420
     133422 133425 133427 133429 133432 133434 133436 133436 133438
     133444 133449 133452 133454 133454 133456 133459 133462 133462
     133465 133470 133474 133475 133478 133478 133481 133484 133486
     133489 133492 133494 133496 133496 133498 133499 133501 133502
     133504 133504 133506 133507 133509 133512 133516 133517 133521
     133525 133528 133532 133538 133540 133546 133548 133556 133559
     133568 133568 133572 133572 133576 133576 133582 133585 133587
     133588 133591 133596 133598 133605 133608 133611 133618 133629
     133632 133635 133637 133640 133643 133643 133645 133646 133648
     133648 133650 133652 133654 133654 133656 133664 133666 133667
     133669 133669 133671 133671 133676 133682 133684 133691 133696
     133700 133706 133711 133718 133728 133732 133732 133734 133746
     133752 133757 133759 133759 133764 133764 133766 133773 133775
     133775 133777 133781 133784 133791 133796 133798 133802 133803
     133805 133808 133811 133811 133817 133820 133823 133823 133832
     133833 133836 133836 133838 133838 133840 133840 133842 133845
     133847 133847 133849 133850 133854 133856 133860 133862 133864
     133868 133872 133873 133875 133875 133881 133884 133887 133890
     133893 133896 133902 133902 133904 133905 133907 133907 133912
     133912 133914 133919 133921 133923 133925 133935 133937 133939
     133944 133948 133951 133955 133959 133959 133961 133967 133972
     133985 133987 133987 133989 133992 133998 134006 134008 134008
     134011 134012 134014 134015 134017 134017 134019 134020 134022
     134023 134025 134027 134029 134030 134032 134035 134037 134044
     134046 134046 134048 134055 134058 134069 134071 134071 134081
     134081 134088 134110 134113 134113 134124 134124 134126 134126
     134131 134131 134133 134134 134148 134149 134152 134174 134177
     134177 134194 134194 134197 134198 134200 134200 134207 134207
     134222 134222 134224 134249 134251 134259 134283 134284 134286
     134286 134288 134288 134291 134291 134295 134295 134313 134315
     134317 134350 134379 134379 134382 134385 134388 134388 134391
     134395 134403 134403 134433 134435 134438 134467 134474 134474
     134481 134483 134496 134498 134501 134501 134506 134506 134512
     134512 134514 134514 134517 134517 134519 134519 134521 134521
     134567 134574 134576 134581 134583 134584 134586 134607 134609
     134623 134631 134631 134648 134648 134651 134654 134656 134656
     134701 134702 134704 134707 134709 134715 134719 134748 134750
     134763 134783 134784 134798 134799 134803 134803 134820 134820
     134860 134864 134868 134897 134902 134902 134933 134933 134936
     134936 134938 134938 134940 134940 134942 134942 134947 134947
     135004 135005 135010 135030 135032 135045 135048 135052 135062
     135063 135077 135079 135082 135082 135087 135088 135109 135143
     135162 135165 135184 135184 135186 135187 135225 135226 135228
     135229 135231 135231 135233 135241 135243 135251 135253 135255
     135272 135273 135275 135277 135306 135307 135311 135337 135350
     135350 135389 135389 135392 135407 135415 135415 135432 135434
     135441 135451 135456 135456 135459 135459 135473 135483 135489
     135489 135508 135508 135511 135514 135516 135519 135521 135521
     135533 135534 135536 135537 135542 135542 135544 135551 135557
     135558 135561 135561 135566 135571 135584 135585 135587 135587
     135591 135593 135600 135600 135602 135608 135611 135612 135614
     135615 135617 135617 135619 135620 135622 135630 135633 135640
     135643 135643 135648 135652 135658 135658 135664 135671 135673
     135673 135676 135680 135687 135687 135689 135695 135697 135697
     135701 135706 135711 135711 135714 135716 135722 135722 135725
     135726 135729 135729 135731 135731 135735 135737 135740 135740
     135742 135743 135748 135748 135754 135754 135762 135762 135768
     135771 135773 135773 135775 135778 135788 135788 135793 135795
     135797 135799 135808 135823 135827 135827 135838 135838 135843
     135846 135852 135852 135860 135870 135896 135896 135899 135902
     135904 135904 135911 135928 135937 135937 135942 135942 135954
     135955 135957 135961 135967 135967 135972 135975 135977 135988
     135992 135993 136010 136010 136015 136015 136017 136017 136019
     136021 136032 136033 136036 136055 136058 136059 136078 136078
     136080 136080 136085 136087 136103 136108 136110 136113 136115
     136131 136135 136136 136151 136151 136169 136171 136173 136184
     136201 136201 136206 136206 136208 136209 136212 136213 136219
     136219 136221 136230 136232 136235 136240 136243 136254 136254
     136258 136258 136263 136266 136271 136271 136280 136284 136286
     136292 136294 136294 136310 136314 136322 136322 136326 136326
     136328 136334 136336 136336 136338 136339 136342 136342 136353
     136353 136355 136356 136360 136360 136367 136372 136380 136381
     136393 136394 136397 136397 136400 136404 136409 136411 136419
     136420 136422 136422 136428 136428 136432 136438 136440 136440
     136444 136444 136447 136447 136449 136450 136455 136459 136463
     136463 136469 136469 136471 136475 136477 136479 136482 136482
     136484 136486 136488 136488 136492 136492 136495 136496 136501
     136503 136505 136505 136507 136510 136517 136518 136523 136526
     136529 136531 136533 136534 136536 136536 136538 136547 136551
     136556 136558 136559 136562 136562 136564 136564 136566 136566
     136569 136577 136580 136580 136582 136582 136584 136584 136586
     136587 136589 136589 136593 136597 136599 136599 136602 136602
     136604 136607 136610 136612 136617 136621 136623 136624 136626
     136628 136630 136633 136637 136639 136641 136641 136643 136643
     136646 136646 136648 136648 136652 136652 136658 136659 136661
     136661 136668 136674 136677 136678 136680 136680 136688 136688
     136692 136692 136695 136703 136706 136706 136708 136710 136713
     136713 136715 136720 136733 136733 136735 136736 136738 136739
     136745 136745 136747 136747 136752 136752 136754 136754 136756
     136764 136769 136769 136772 136772 136775 136778 136780 136780
     136782 136784 136789 136792 136794 136794 136796 136797 136800
     136800 136808 136814 136818 136818 136822 136829 136832 136833
     136837 136842 136844 136844 136847 136849 136854 136855 136859
     136860 136864 136864 136868 136868 136872 136872 136879 136881
     136888 136894 136909 136910 136912 136912 136914 136914 136918
     136930 136951 136951 136953 136953 136957 136958 136960 136975
     136977 136977 136987 136989 136991 136993 136995 136995 136997
     137014 137016 137016 137021 137021 137034 137034 137036 137037
     137039 137039 137041 137041 137049 137067 137074 137074 137104
     137105 137107 137108 137112 137124 137126 137135 137169 137169
     137172 137174 137181 137190 137192 137204 137207 137207 137213
     137213 137224 137224 137227 137227 137232 137236 137239 137239
     137241 137248 137250 137250 137253 137255 137271 137272 137277
     137277 137282 137283 137288 137293 137295 137303 137305 137308
     137311 137311 137317 137317 137320 137320 137337 137337 137340
     137343 137346 137346 137350 137355 137361 137373 137379 137379
     137393 137394 137396 137397 137399 137400 137408 137411 137413
     137417 137419 137426 137428 137429 137441 137441 137444 137444
     137449 137451 137453 137457 137463 137464 137468 137473 137480
     137480 137483 137484 137487 137487 137490 137490 137494 137499
     137505 137509 137512 137513 137515 137516 137518 137519 137521
     137522 137526 137526 137529 137530 137532 137532 137534 137536
     137539 137539 137541 137545 137549 137553 137559 137559 137563
     137566 137574 137574 137576 137578 137581 137581 137590 137591
     137593 137594 137597 137597 137603 137606 137611 137612 137614
     137615 137617 137617 137621 137621 137623 137624 137629 137635
     137641 137641 137645 137645 137653 137653 137657 137657 137660
     137663 137666 137666 137672 137672 137675 137679 137686 137690
     137692 137692 137698 137698 137705 137713 137716 137716 137722
     137723 137728 137738 137741 137743 137747 137748 137756 137756
     137758 137760 137762 137769 137780 137780 137782 137782 137787
     137801 137817 137817 137819 137819 137823 137834 137836 137837
     137850 137862 137870 137872 137875 137876 137878 137880 137883
     137883 137887 137893 137908 137908 137910 137910 137912 137912
     137914 137914 137916 137926 137938 137940 137946 137951 137961
     137961 137964 137969 137971 137971 137975 137975 137979 137982
     137987 137991 137994 137994 137996 137997 138001 138003 138006
     138006 138008 138011 138013 138013 138015 138016 138023 138024
     138026 138026 138028 138028 138030 138033 138035 138036 138041
     138043 138045 138046 138049 138050 138054 138054 138056 138056
     138058 138062 138064 138067 138069 138069 138073 138074 138077
     138080 138085 138085 138089 138091 138095 138095 138099 138100
     138105 138105 138109 138114 138126 138126 138128 138128 138133
     138133 138137 138137 138142 138143 138145 138145 138150 138151
     138160 138161 138171 138171 138175 138175 138178 138178 138180
     138180 138185 138186 138189 138189 138191 138192 138194 138194
     138196 138196 138198 138198 138200 138201 138203 138206 138208
     138211 138213 138218 138220 138222 138226 138232 138234 138235
     138242 138244 138247 138247 138249 138254 138256 138258 138260
     138264 138268 138269 138272 138272 138274 138278 138281 138287
     138294 138294 138296 138296 138298 138298 138300 138307 138315
     138325 138336 138344 138352 138354 138356 138356 138359 138359
     138361 138371 138380 138380 138382 138382 138385 138386 138388
     138401 138403 138403 138409 138410 138412 138422 138427 138427
     138430 138430 138434 138434 138436 138441 138445 138445 138447
     138449 138456 138456 138462 138465 138471 138472 138475 138477
     138484 138484 138488 138488 138494 138498 138500 138502 138504
     138504 138507 138511 138513 138515 138517 138521 138523 138527
     138529 138529 138532 138537 138539 138541 138543 138547 138552
     138552 138555 138556 138559 138561 138566 138566 138570 138571
     138574 138575 138577 138577 138579 138590 138595 138595 138606
     138606 138608 138609 138617 138618 138620 138620 138622 138622
     138626 138643 138658 138658 138660 138660 138665 138673 138675
     138675 138680 138681 138693 138695 138697 138697 138700 138700
     138703 138703 138706 138730 138733 138734 138744 138744 138760
     138762 138764 138764 138766 138766 138768 138780 138782 138794
     138796 138796 138805 138805 138819 138819 138822 138822 138826
     138827 138829 138829 138831 138831 138834 138834 138836 138836
     138850 138853 138855 138864 138866 138867 138869 138878 138886
     138886 138888 138888 138890 138890 138898 138898 138900 138900
     138905 138906 138909 138909 138913 138913 138923 138935 138937
     138943 138945 138945 138951 138951 138955 138955 138957 138957
     138961 138961 138967 138986 138988 138994 138996 138997 139006
     139006 139018 139022 139024 139037 139045 139045 139055 139056
     139062 139062 139066 139066 139068 139082 139088 139088 139091
     139093 139096 139096 139098 139105 139107 139111 139116 139117
     139125 139125 139128 139128 139130 139130 139133 139133 139136
     139136 139138 139138 139141 139146 139148 139149 139152 139153
     139156 139157 139160 139167 139174 139175 139177 139177 139182
     139186 139191 139191 139195 139198 139201 139201 139206 139212
     139215 139215 139217 139217 139220 139222 139224 139224 139227
     139229 139232 139235 139237 139237 139239 139249 139255 139257
     139259 139259 139261 139268 139271 139274 139278 139278 139280
     139283 139288 139289 139292 139293 139296 139297 139299 139299
     139305 139305 139311 139311 139316 139316 139318 139319 139321
     139321 139323 139323 139327 139333 139340 139340 139344 139344
     139346 139355 139362 139370 139372 139377 139379 139381 139385
     139395 139398 139399 139402 139403 139405 139405 139407 139416
     139418 139418 139421 139424 139429 139430 139433 139436 139438
     139439 139441 139442 139444 139458 139462 139462 139467 139485
     139491 139492 139495 139496 139498 139515 139521 139521 139524
     139527 139530 139536 139539 139544 139546 139548 139550 139559
     139568 139578 139580 139583 139586 139586 139593 139595 139597
     139607 139609 139611 139614 139618 139622 139622 139624 139627
     139629 139634 139637 139637 139640 139641 139643 139647 139651
     139653 139655 139659 139661 139662 139665 139666 139673 139676
     139679 139679 139682 139682 139692 139692 139695 139695 139697
     139700 139702 139704 139707 139713 139718 139719 139722 139725
     139727 139727 139729 139730 139732 139732 139734 139742 139744
     139744 139747 139755 139757 139757 139759 139760 139762 139772
     139778 139778 139780 139781 139783 139788 139799 139799 139801
     139813 139820 139821 139826 139846 139848 139848 139861 139862
     139864 139885 139892 139892 139894 139894 139903 139911 139913
     139913 139918 139918 139920 139920 139922 139929 139931 139933
     139944 139944 139946 139946 139949 139958 139962 139964 139969
     139975 139980 139980 139985 139988 139994 139994 139996 139997
     140001 140003 140006 140007 140010 140010 140013 140013 140015
     140018 140024 140024 140027 140032 140034 140036 140038 140041
     140043 140043 140045 140045 140049 140053 140059 140059 140061
     140062 140070 140070 140072 140072 140075 140077 140079 140079
     140081 140081 140083 140085 140088 140088 140090 140095 140097
     140102 140106 140106 140109 140112 140114 140114 140116 140126
     140128 140128 140130 140131 140134 140136 140139 140140 140142
     140147 140150 140155 140159 140161 140163 140164 140166 140167
     140170 140170 140172 140174 140176 140179 140184 140186 140188
     140189 140191 140192 140194 140203 140205 140205 140208 140210
     140214 140219 140225 140225 140229 140233 140242 140250 140260
     140260 140262 140266 140268 140268 140270 140280 140284 140286
     140288 140296 140298 140300 140303 140303 140306 140310 140313
     140313 140317 140321 140324 140329 140332 140333 140335 140335
     140338 140340 140343 140348 140350 140356 140358 140365 140368
     140380 140382 140382 140385 140388 140390 140390 140392 140395
     140397 140397 140399 140403 140405 140409 140413 140417 140419
     140420 140424 140426 140428 140428 140431 140433 140436 140439
     140443 140449 140452 140455 140457 140464 140466 140469 140471
     140473 140477 140480 140482 140491 140495 140496 140498 140504
     140508 140512 140514 140515 140521 140536 140540 140542 140544
     140548 140550 140561 140568 140568 140570 140570 140572 140573
     140575 140586 140589 140589 140593 140593 140595 140600 140611
     140611 140615 140616 140621 140639 140647 140647 140650 140657
     140662 140663 140665 140668 140670 140670 140675 140676 140680
     140682 140686 140690 140695 140703 140706 140711 140713 140717
     140722 140731 140733 140733 140737 140737 140739 140739 140743
     140750 140752 140757 140759 140760 140763 140763 140780 140780
     140782 140782 140785 140786 140793 140795 140799 140822 140832
     140832 140835 140835 140838 140840 140842 140842 140852 140856
     140858 140878 140886 140886 140898 140898 140902 140902 140910
     140910 140912 140945 140951 140951 140966 140966 140975 140976
     140986 140987 140993 141038 141078 141079 141082 141082 141091
     141093 141096 141141 141143 141143 141149 141152 141165 141166
     141170 141170 141181 141181 141191 141191 141193 141194 141198
     141210 141212 141235 141238 141238 141251 141252 141258 141258
     141276 141277 141280 141284 141286 141292 141294 141312 141322
     141322 141340 141340 141342 141345 141356 141356 141358 141358
     141361 141387 141390 141401 141410 141410 141412 141413 141427
     141428 141432 141432 141437 141439 141452 141453 141458 141479
     141488 141489 141513 141514 141517 141526 141528 141538 141554
     141554 141558 141558 141563 141580 141582 141587 141589 141590
     141592 141593 141604 141604 141606 141622 141633 141633 141635
     141636 141643 141651 141655 141656 141658 141663 141665 141665
     141669 141673 141679 141682 141691 141694 141701 141701 141704
     141705 141710 141712 141715 141718 141721 141730 141734 141740
     141750 141755 141759 141761 141766 141773 141777 141778 141780
     141782 141784 141784 141788 141788 141791 141792 141794 141794
     141796 141801 141804 141804 141811 141818 141822 141823 141831
     141838 141840 141844 141849 141851 141854 141855 141857 141857
     141859 141862 141865 141866 141871 141871 141874 141874 141876
     141876 141879 141882 141886 141886 141889 141889 141892 141896
     141899 141899 141901 141901 141905 141907 141909 141910 141912
     141913 141917 141917 141920 141920 141922 141922 141925 141926
     141928 141929 141931 141934 141936 141940 141954 141975 142004
     142008 142010 142016 142018 142018 142050 142050 142052 142080
     142083 142083 142086 142086 142110 142110 142122 142124 142127
     142158 142162 142163 142180 142180 142184 142184 142209 142212
     142214 142234 142236 142244 142247 142248 142254 142254 142280
     142280 142284 142284 142313 142364 142376 142376 142400 142400
     142402 142403 142405 142405 142407 142407 142431 142444 142446
     142452 142454 142460 142462 142464 142485 142486 142488 142488
     142512 142512 142515 142526 142528 142537 142539 142539 142541
     142550 142552 142554 142561 142562 142592 142592 142624 142638
     142640 142657 142659 142660 142664 142664 142669 142671 142719
     142749 142758 142758 142773 142773 142791 142792 142795 142813
     142820 142821 142829 142829 142838 142857 142883 142900 142914
     142914 142916 142916 142925 142925 142927 142929 142931 142938
     142943 142943 142956 142965 142978 142988 142993 142993 142997
     142998 143006 143010 143013 143014 143020 143020 143024 143025
     143028 143031 143038 143038 143041 143044 143048 143048 143050
     143050 143053 143055 143060 143061 143063 143063 143066 143067
     143069 143070 143072 143072 143077 143079 143081 143082 143084
     143087 143089 143092 143094 143095 143097 143099 143101 143102
     143109 143121 143124 143125 143128 143128 143132 143139 143141
     143141 143143 143144 143146 143146 143148 143151 143158 143158
     143162 143163 143165 143165 143167 143179 143183 143184 143191
     143192 143195 143195 143197 143201 143204 143210 143212 143216
     143218 143223 143226 143226 143231 143232 143234 143241 143243
     143245 143247 143248 143250 143252 143257 143262 143264 143264
     143267 143269 143271 143279 143281 143281 143283 143290 143292
     143294 143302 143307 143309 143310 143312 143318 143320 143321
     143323 143323 143326 143329 143332 143333 143336 143342 143346
     143349 143351 143353 143357 143362 143366 143366 143369 143370
     143374 143378 143380 143382 143384 143384 143386 143386 143389
     143390 143392 143393 143395 143400 143402 143407 143409 143409
     143411 143411 143413 143415 143418 143418 143420 143420 143427
     143427 143434 143435 143438 143439 143442 143442 143447 143447
     143449 143449 143452 143455 143458 143459 143467 143468 143473
     143473 143475 143479 143482 143484 143487 143490 143492 143492
     143494 143494 143497 143498 143500 143500 143503 143503 143505
     143505 143508 143509 143513 143513 143518 143528 143530 143531
     143533 143535 143537 143538 143541 143542 143544 143549 143551
     143555 143558 143558 143560 143560 143562 143565 143567 143569
     143572 143572 143574 143574 143576 143577 143581 143583 143586
     143586 143590 143594 143599 143599 143603 143603 143605 143605
     143607 143607 143611 143617 143624 143626 143630 143633 143636
     143636 143640 143642 143645 143650 143652 143652 143654 143656
     143658 143659 143662 143662 143664 143666 143668 143672 143675
     143676 143678 143681 143686 143692 143694 143694 143696 143696
     143702 143702 143706 143719 143727 143727 143729 143732 143735
     143742 143748 143748 143753 143755 143762 143774 143776 143776
     143783 143783 143785 143785 143787 143787 143794 143808 143834
     143834 143842 143852 143854 143859 143862 143862 143866 143867
     143885 143885 143887 143887 143903 143905 143908 143914 143926
     143926 143940 143940 143944 143944 143946 143946 143953 143964
     143977 143981 143984 143984 143994 143999 144001 144004 144006
     144006 144020 144021 144025 144027 144029 144033 144035 144040
     144046 144046 144052 144052 144054 144054 144056 144056 144061
     144061 144068 144069 144073 144077 144079 144080 144083 144083
     144087 144087 144094 144095 144099 144099 144101 144101 144111
     144111 144114 144120 144127 144127 144133 144133 144135 144136
     144141 144141 144143 144143 144147 144147 144150 144152 144156
     144156 144159 144159 144163 144163 144166 144169 144172 144173
     144188 144190 144195 144196 144198 144200 144207 144207 144209
     144212 144214 144214 144216 144217 144219 144231 144236 144236
     144238 144238 144244 144250 144260 144261 144268 144269 144272
     144272 144274 144276 144279 144279 144282 144284 144287 144287
     144291 144292 144297 144297 144301 144302 144305 144309 144312
     144316 144319 144323 144325 144325 144331 144331 144336 144338
     144340 144353 144370 144371 144374 144374 144376 144376 144379
     144379 144381 144381 144385 144385 144387 144387 144391 144403
     144432 144432 144437 144452 144485 144485 144488 144488 144490
     144491 144493 144493 144497 144497 144504 144505 144510 144521
     144527 144527 144552 144557 144559 144559 144566 144572 144576
     144609 144615 144618 144654 144655 144657 144662 144683 144687
     144689 144718 144720 144723 144767 144771 144775 144775 144778
     144778 144803 144805 144808 144808 144810 144818 144820 144831
     144833 144835 144848 144848 144883 144885 144887 144888 144890
     144891 144901 144901 144903 144903 144912 144914 144917 144947
     144955 144955 144959 144959 144961 144963 144987 144987 144996
     144996 145008 145014 145019 145042 145044 145062 145064 145066
     145071 145071 145110 145110 145112 145112 145114 145114 145131
     145131 145136 145149 145151 145160 145165 145166 145187 145192
     145209 145210 145217 145246 145253 145253 145255 145256 145271
     145271 145273 145273 145275 145276 145278 145278 145286 145287
     145289 145289 145291 145291 145296 145311 145315 145316 145328
     145328 145330 145330 145333 145333 145335 145335 145348 145362
     145368 145368 145384 145384 145388 145388 145391 145392 145394
     145398 145400 145406 145409 145409 145420 145420 145422 145424
     145428 145428 145433 145441 145453 145454 145463 145472 145475
     145475 145478 145478 145481 145482 145484 145487 145496 145496
     145499 145499 145502 145502 145504 145504 145507 145507 145509
     145509 145511 145511 145513 145515 145517 145517 145523 145524
     145526 145528 145530 145530 145532 145532 145536 145543 145545
     145555 145559 145571 145575 145578 145580 145588 145594 145595
     145597 145597 145608 145619 145621 145622 145625 145625 145629
     145641 145644 145644 145647 145648 145650 145667 145671 145682
     145684 145684 145687 145690 145692 145692 145696 145699 145701
     145701 145705 145705 145708 145709 145713 145714 145716 145719
     145721 145724 145726 145733 145735 145753 145760 145760 145762
     145763 145765 145771 145773 145773 145778 145780 145784 145791
     145793 145797 145799 145801 145803 145803 145812 145817 145820
     145823 145825 145826 145830 145832 145839 145839 145841 145842
     145845 145853 145855 145863 145865 145878 145885 145902 145904
     145913 145915 145921 145923 145923 145925 145937 145942 145958
     145960 145960 145967 145967 145969 145977 145979 145979 145988
     145993 145995 146007 146009 146009 146013 146019 146024 146029
     146031 146036 146038 146039 146042 146044 146046 146048 146050
     146051 146053 146054 146056 146056 146059 146064 146069 146071
     146073 146073 146075 146075 146080 146081 146083 146085 146089
     146090 146092 146099 146102 146105 146107 146113 146124 146125
     146128 146131 146134 146134 146136 146138 146142 146143 146145
     146153 146155 146157 146159 146160 146164 146164 146169 146170
     146178 146182 146184 146199 146202 146204 146209 146211 146217
     146222 146229 146229 146232 146242 146244 146245 146247 146247
     146268 146275 146285 146286 146289 146301 146312 146325 146339
     146339 146343 146356 146361 146361 146370 146370 146373 146383
     146385 146387 146389 146389 146395 146397 146399 146399 146402
     146402 146404 146418 146423 146430 146438 146438 146441 146446
     146448 146448 146452 146452 146454 146454 146458 146460 146462
     146467 146472 146472 146475 146478 146482 146482 146486 146488
     146491 146492 146495 146496 146500 146501 146503 146507 146510
     146515 146517 146518 146521 146524 146526 146526 146528 146529
     146532 146532 146534 146534 146538 146539 146542 146543 146548
     146551 146560 146564 146566 146566 146571 146571 146574 146581
     146589 146593 146610 146626 146628 146629 146634 146635 146648
     146651 146653 146653 146659 146679 146689 146689 146705 146706
     146708 146710 146718 146748 146750 146750 146754 146754 146782
     146782 146786 146786 146788 146788 146790 146790 146792 146793
     146796 146796 146801 146801 146807 146838 146840 146847 146849
     146854 146869 146869 146878 146879 146907 146907 146909 146909
     146911 146911 146913 146914 146916 146916 146918 146919 146921
     146921 146933 146934 146939 146977 147014 147015 147017 147018
     147020 147022 147034 147034 147037 147055 147057 147073 147084
     147088 147116 147116 147119 147122 147124 147124 147148 147149
     147151 147186 147199 147199 147225 147227 147229 147229 147231
     147235 147259 147259 147267 147276 147278 147312 147315 147318
     147325 147325 147332 147333 147363 147363 147368 147374 147397
     147428 147441 147444 147460 147464 147467 147467 147470 147470
     147473 147473 147490 147511 147513 147513 147518 147518 147529
     147530 147532 147533 147551 147551 147553 147591 147594 147594
     147617 147619 147621 147622 147634 147643 147645 147650 147652
     147652 147658 147658 147669 147669 147671 147674 147683 147683
     147689 147693 147695 147698 147700 147705 147707 147710 147721
     147721 147726 147729 147731 147741 147750 147750 147752 147755
     147764 147771 147773 147773 147777 147777 147781 147783 147786
     147790 147793 147794 147803 147803 147805 147805 147808 147812
     147814 147814 147816 147819 147821 147821 147824 147828 147831
     147833 147835 147837 147839 147840 147842 147846 147851 147851
     147853 147858 147866 147866 147869 147871 147887 147887 147889
     147890 147892 147892 147900 147905 147911 147911 147929 147931
     147933 147933 147935 147937 147947 147947 147951 147954 147956
     147959 147965 147965 147985 147986 147989 147990 148002 148003
     148005 148018 148025 148025 148042 148042 148049 148049 148061
     148061 148063 148063 148065 148071 148073 148079 148089 148091
     148119 148121 148123 148124 148143 148143 148146 148146 148148
     148164 148166 148167 148200 148202 148204 148204 148207 148209
     148211 148211 148225 148225 148227 148227 148232 148246 148251
     148253 148280 148285 148301 148301 148304 148304 148307 148317
     148326 148327 148350 148350 148354 148354 148375 148376 148378
     148378 148381 148405 148407 148408 148414 148416 148440 148443
     148445 148448 148461 148464 148468 148480 148498 148498 148501
     148501 148503 148503 148510 148511 148518 148525 148527 148531
     148535 148536 148546 148547 148549 148550 148554 148555 148562
     148564 148566 148567 148572 148572 148578 148578 148583 148584
     148593 148593 148595 148602 148604 148610 148614 148614 148617
     148617 148627 148627 148631 148631 148636 148636 148638 148638
     148641 148644 148646 148647 148651 148652 148654 148654 148666
     148670 148672 148672 148676 148677 148680 148682 148687 148688
     148690 148690 148692 148693 148695 148695 148698 148698 148700
     148705 148709 148710 148714 148719 148721 148724 148726 148728
     148733 148734 148736 148739 148743 148743 148746 148746 148748
     148748 148750 148750 148752 148754 148756 148757 148759 148760
     148764 148764 148768 148768 148770 148778 148780 148780 148782
     148782 148784 148786 148793 148793 148800 148800 148805 148805
     148808 148809 148811 148811 148813 148813 148816 148817 148819
     148819 148821 148821 148824 148824 148826 148827 148829 148832
     148834 148843 148845 148855 148858 148862 148865 148865 148869
     148877 148879 148880 148883 148883 148885 148894 148896 148896
     148898 148903 148905 148916 148919 148919 148922 148926 148928
     148930 148932 148933 148935 148943 148945 148945 148951 148956
     148960 148960 148962 148963 148965 148966 148968 148975 148977
     148977 148979 148984 148986 148987 148991 148992 148995 148997
     149000 149001 149004 149005 149008 149009 149012 149012 149014
     149014 149016 149016 149018 149031 149035 149036 149042 149044
     149048 149056 149058 149058 149065 149066 149068 149069 149071
     149071 149073 149077 149079 149080 149083 149090 149092 149092
     149096 149096 149098 149099 149101 149101 149104 149109 149111
     149113 149122 149124 149128 149130 149132 149142 149150 149150
     149152 149152 149160 149174 149180 149183 149186 149186 149193
     149201 149211 149211 149215 149215 149217 149223 149225 149225
     149227 149227 149231 149236 149238 149238 149241 149243 149245
     149248 149250 149253 149255 149257 149259 149261 149263 149266
     149268 149268 149270 149270 149273 149273 149275 149277 149281
     149284 149286 149286 149288 149288 149291 149291 149295 149312
     149314 149315 149318 149318 149324 149325 149327 149340 149342
     149345 149347 149348 149350 149350 149352 149352 149356 149356
     149358 149378 149384 149384 149399 149399 149401 149411 149413
     149413 149423 149424 149426 149426 149432 149433 149438 149440
     149443 149458 149460 149461 149463 149463 149465 149466 149468
     149485 149492 149494 149496 149496 149498 149499 149501 149501
     149511 149511 149517 149530 149532 149537 149540 149541 149547
     149548 149557 149557 149561 149575 149582 149582 149585 149585
     149591 149593 149596 149614 149616 149622 149625 149628 149631
     149631 149635 149635 149637 149637 149642 149658 149660 149662
     149664 149664 149666 149667 149670 149674 149679 149679 149684
     149691 149699 149699 149701 149701 149704 149707 149709 149710
     149712 149715 149719 149721 149723 149725 149727 149728 149731
     149732 149735 149739 149742 149744 149746 149746 149748 149749
     149752 149756 149768 149768 149773 149773 149775 149779 149784
     149784 149800 149800 149802 149802 149809 149810 149813 149820
     149838 149838 149844 149852 149873 149873 149879 149880 149884
     149889 149891 149891 149904 149906 149920 149922 149925 149926
     149928 149928 149934 149940 149948 149948 149974 149975 149977
     149977 149984 149984 149986 149986 149991 149991 149994 150000
     150002 150002 150010 150010 150029 150029 150031 150032 150036
     150045 150072 150075 150080 150089 150091 150091 150098 150098
     150111 150111 150115 150116 150120 150121 150123 150130 150132
     150133 150147 150151 150155 150160 150178 150179 150185 150190
     150196 150196 150201 150201 150206 150208 150213 150214 150217
     150218 150220 150221 150228 150229 150232 150232 150243 150245
     150248 150253 150255 150263 150266 150268 150270 150270 150273
     150275 150279 150281 150284 150288 150290 150290 150294 150296
     150298 150301 150304 150308 150310 150310 150312 150314 150316
     150320 150324 150327 150330 150332 150334 150340 150348 150357
     150359 150360 150363 150371 150374 150378 150380 150380 150384
     150384 150387 150388 150393 150400 150405 150411 150413 150416
     150419 150420 150422 150422 150424 150429 150433 150434 150438
     150443 150445 150451 150453 150453 150456 150461 150465 150465
     150471 150473 150475 150478 150480 150480 150483 150487 150490
     150491 150493 150499 150502 150503 150508 150508 150511 150511
     150514 150515 150519 150519 150525 150525 150528 150528 150531
     150531 150533 150536 150542 150542 150545 150545 150549 150549
     150555 150555 150557 150563 150568 150582 150603 150608 150610
     150610 150613 150614 150622 150622 150627 150638 150641 150641
     150643 150643 150646 150646 150652 150654 150659 150660 150667
     150667 150669 150670 150673 150685 150691 150691 150695 150695
     150699 150704 150706 150706 150709 150715 150717 150719 150725
     150725 150728 150732 150735 150735 150738 150744 150747 150747
     150751 150752 150756 150756 150761 150767 150770 150771 150774
     150774 150777 150777 150779 150780 150783 150786 150788 150789
     150791 150792 150795 150796 150798 150798 150800 150803 150805
     150805 150807 150809 150812 150817 150820 150827 150830 150833
     150835 150836 150839 150851 150855 150855 150859 150859 150861
     150861 150865 150865 150868 150868 150871 150889 150891 150891
     150893 150893 150903 150913 150917 150918 150921 150922 150924
     150924 150931 150949 150952 150955 150966 150967 150976 150993
     150995 151004 151008 151008 151013 151013 151031 151033 151035
     151055 151057 151065 151070 151072 151074 151074 151076 151076
     151081 151103 151113 151113 151117 151117 151119 151119 151131
     151148 151153 151153 151155 151156 151158 151158 151161 151161
     151163 151163 151168 151169 151172 151197 151206 151209 151223
     151241 151245 151245 151253 151262 151266 151267 151269 151269
     151272 151272 151278 151279 151281 151290 151296 151302 151307
     151309 151312 151314 151318 151322 151324 151325 151327 151333
     151335 151335 151337 151338 151340 151340 151343 151346 151351
     151351 151353 151354 151356 151358 151361 151363 151366 151370
     151372 151374 151376 151376 151380 151380 151382 151382 151384
     151387 151397 151399 151404 151404 151407 151408 151417 151421
     151423 151423 151432 151434 151445 151445 151448 151449 151451
     151454 151456 151464 151466 151469 151474 151476 151478 151478
     151480 151480 151483 151484 151486 151486 151488 151488 151491
     151491 151493 151499 151502 151506 151509 151517 151519 151521
     151525 151525 151527 151531 151534 151534 151539 151543 151545
     151545 151551 151551 151553 151556 151559 151563 151565 151569
     151572 151577 151579 151580 151582 151583 151585 151590 151592
     151592 151595 151597 151599 151602 151604 151606 151609 151610
     151612 151613 151615 151615 151617 151625 151627 151627 151630
     151638 151642 151644 151646 151649 151657 151665 151670 151676
     151683 151685 151687 151688 151690 151690 151699 151705 151712
     151713 151716 151721 151724 151724 151729 151731 151733 151738
     151740 151740 151742 151742 151744 151747 151751 151754 151757
     151758 151763 151766 151768 151769 151772 151772 151777 151777
     151780 151780 151782 151787 151794 151805 151808 151808 151810
     151833 151835 151838 151845 151845 151847 151847 151849 151849
     151857 151873 151876 151876 151881 151881 151891 151891 151895
     151907 151909 151918 151920 151921 151923 151923 151926 151926
     151929 151929 151932 151932 151941 151945 151947 151974 151984
     151984 151987 151987 151989 151990 152003 152007 152009 152022
     152024 152033 152036 152036 152043 152043 152046 152046 152052
     152053 152062 152064 152066 152067 152069 152092 152105 152106
     152119 152120 152123 152159 152162 152162 152166 152166 152176
     152176 152180 152194 152196 152202 152208 152208 152213 152213
     152231 152232 152234 152260 152269 152269 152272 152272 152281
     152293 152296 152296 152299 152301 152303 152304 152315 152315
     152319 152336 152341 152343 152346 152351 152353 152353 152355
     152355 152360 152362 152365 152370 152373 152374 152378 152388
     152390 152390 152393 152396 152399 152399 152403 152405 152407
     152407 152410 152410 152414 152421 152423 152424 152426 152435
     152437 152447 152450 152455 152457 152464 152466 152474 152476
     152476 152481 152481 152484 152492 152496 152498 152501 152502
     152504 152514 152521 152521 152526 152527 152532 152532 152536
     152538 152540 152544 152546 152546 152554 152556 152559 152560
     152562 152565 152567 152567 152571 152572 152574 152574 152576
     152577 152579 152580 152593 152593 152595 152595 152597 152602
     152605 152606 152612 152612 152617 152620 152634 152634 152636
     152636 152641 152641 152644 152660 152664 152666 152674 152675
     152681 152681 152687 152688 152690 152694 152696 152706 152719
     152719 152722 152722 152724 152724 152732 152732 152740 152740
     152743 152758 152765 152765 152768 152769 152774 152775 152778
     152791 152794 152794 152813 152813 152817 152817 152826 152842
     152848 152848 152866 152866 152872 152875 152877 152885 152899
     152899 152901 152905 152908 152913 152915 152920 152922 152922
     152929 152929 152934 152934 152944 152959 152971 152972 152983
     152993 153002 153004 153011 153012 153015 153015 153019 153026
     153033 153033 153042 153043 153048 153049 153053 153055 153059
     153059 153066 153070 153073 153074 153076 153076 153079 153080
     153082 153083 153086 153087 153089 153089 153091 153094 153096
     153096 153101 153101 153109 153110 153126 153126 153131 153134
     153136 153137 153154 153154 153156 153156 153161 153163 153165
     153167 153188 153188 153192 153192 153195 153202 153207 153208
     153224 153229 153244 153244 153254 153259 153280 153283 153285
     153288 153300 153300 153304 153313 153326 153332 153339 153341
     153343 153343 153347 153349 153359 153359 153361 153364 153370
     153371 153375 153376 153379 153379 153382 153383 153385 153386
     153388 153388 153391 153394 153396 153396 153399 153401 153404
     153407 153409 153409 153411 153413 153415 153416 153418 153421
     153424 153424 153427 153430 153432 153432 153442 153455 153466
     153468 153470 153478 153490 153490 153495 153502 153513 153513
     153519 153519 153521 153532 153541 153541 153549 153564 153566
     153566 153591 153592 153594 153595 153597 153614 153633 153634
     153649 153655 153657 153658 153660 153665 153667 153667 153669
     153670 153684 153684 153690 153693 153695 153712 153715 153716
     153721 153721 153728 153729 153735 153736 153738 153743 153745
     153750 153758 153758 153773 153774 153776 153776 153780 153789
     153811 153811 153814 153818 153820 153820 153822 153824 153829
     153830 153832 153842 153847 153847 153854 153854 153856 153859
     153861 153863 153866 153868 153870 153870 153873 153876 153879
     153880 153882 153882 153884 153884 153886 153887 153890 153892
     153895 153899 153901 153902 153904 153904 153908 153919 153927
     153940 153943 153943 153947 153947 153950 153950 153952 153961
     153974 153992 153994 154001 154011 154018 154020 154026 154034
     154034 154037 154038 154040 154047 154053 154053 154055 154055
     154058 154058 154064 154064 154066 154080 154082 154082 154085
     154085 154092 154109 154122 154122 154125 154136 154145 154146
     154154 154158 154160 154167 154171 154173 154175 154178 154186
     154188 154190 154190 154192 154192 154194 154201 154208 154214
     154221 154222 154225 154230 154235 154235 154238 154244 154250
     154257 154261 154261 154264 154267 154277 154279 154284 154284
     154287 154289 154291 154291 154293 154294 154299 154301 154303
     154303 154307 154307 154312 154313 154317 154318 154320 154320
     154326 154326 154329 154338 154351 154353 154355 154365 154375
     154375 154380 154391 154393 154395 154397 154399 154410 154425
     154427 154432 154443 154443 154448 154472 154474 154481 154487
     154488 154501 154501 154509 154526 154528 154528 154530 154539
     154541 154543 154577 154618 154620 154623 154626 154626 154629
     154629 154633 154633 154648 154690 154692 154695 154701 154704
     154729 154729 154741 154753 154756 154770 154773 154796 154798
     154799 154811 154813 154822 154874 154879 154882 154899 154900
     154913 154930 154932 154939 154941 154943 154948 154949 154960
     154960 154963 154963 154972 154998 155000 155001 155005 155005
     155014 155016 155018 155039 155050 155051 155060 155081 155088
     155089 155092 155104 155106 155108 155112 155122 155124 155124
     155128 155128 155132 155136 155139 155139 155141 155141 155143
     155147 155152 155153 155158 155160 155164 155164 155169 155171
     155173 155173 155175 155176 155179 155181 155183 155183 155187
     155192 155197 155198 155201 155203 155205 155216 155221 155221
     155226 155226 155228 155234 155236 155236 155245 155245 155250
     155263 155272 155272 155275 155275 155288 155300 155315 155315
     155318 155318 155320 155320 155329 155350 155364 155376 155379
     155379 155391 155401 155411 155411 155416 155426 155428 155429
     155446 155459 155461 155461 155463 155463 155465 155466 155471
     155473 155479 155483 155491 155491 155493 155495 155500 155502
     155504 155508 155510 155522 155524 155527 155530 155530 155532
     155539 155542 155542 155545 155550 155552 155568 155578 155579
     155587 155603 155605 155605 155612 155612 155615 155615 155622
     155642 155644 155644 155655 155655 155657 155657 155666 155687
     155715 155716 155718 155718 155720 155742 155745 155745 155748
     155749 155760 155761 155764 155764 155776 155785 155787 155797
     155799 155810 155828 155829 155831 155831 155841 155841 155843
     155865 155867 155872 155874 155880 155888 155889 155913 155932
     155953 155954 155964 155989 156007 156008 156010 156010 156012
     156012 156019 156019 156021 156038 156040 156047 156053 156055
     156059 156071 156073 156076 156079 156080 156086 156086 156091
     156101 156104 156104 156110 156111 156113 156113 156115 156124
     156130 156131 156133 156133 156135 156140 156144 156147 156150
     156158 156160 156161 156163 156163 156165 156165 156167 156167
     156194 156195 156197 156201 156203 156210 156212 156214 156216
     156220 156225 156226 156231 156233 156238 156240 156245 156246
     156249 156250 156253 156256 156258 156259 156261 156263 156265
     156271 156275 156275 156279 156281 156283 156284 156286 156291
     156296 156296 156299 156299 156301 156312 156316 156321 156323
     156324 156327 156331 156333 156335 156339 156340 156342 156342
     156348 156351 156356 156365 156369 156369 156374 156388 156391
     156391 156395 156405 156409 156409 156411 156419 156421 156422
     156425 156426 156429 156439 156444 156452 156456 156462 156465
     156472 156474 156476 156478 156483 156486 156488 156490 156492
     156494 156496 156498 156498 156500 156501 156503 156504 156506
     156511 156517 156522 156525 156526 156530 156530 156537 156544
     156547 156547 156549 156555 156557 156557 156564 156564 156567
     156570 156573 156573 156578 156583 156585 156585 156587 156587
     156591 156593 156596 156596 156599 156604 156609 156609 156611
     156611 156613 156621 156625 156625 156629 156634 156636 156643
     156647 156651 156653 156653 156655 156656 156658 156658 156660
     156668 156672 156672 156674 156686 156691 156691 156693 156700
     156702 156702 156706 156714 156716 156723 156727 156734 156736
     156736 156742 156753 156760 156768 156770 156778 156784 156790
     156792 156798 156803 156808 156810 156810 156813 156822 156827
     156829 156831 156832 156834 156835 156837 156838 156840 156842
     156844 156847 156849 156851 156854 156856 156858 156859 156862
     156862 156864 156865 156867 156868 156878 156880 156882 156884
     156886 156890 156893 156894 156896 156896 156900 156901 156904
     156904 156906 156906 156908 156908 156910 156913 156916 156917
     156919 156926 156929 156934 156937 156937 156939 156945 156947
     156949 156951 156952 156955 156962 156965 156966 156968 156973
     156975 156977 156981 156981 156983 156985 156988 156993 156999
     156999 157002 157005 157007 157016 157018 157018 157024 157026
     157029 157030 157041 157041 157045 157056 157058 157058 157061
     157061 157064 157070 157072 157072 157084 157084 157090 157100
     157113 157113 157116 157123 157130 157130 157133 157133 157136
     157143 157145 157146 157151 157161 157167 157167 157171 157175
     157182 157182 157187 157189 157192 157195 157197 157197 157202
     157205 157208 157208 157211 157211 157213 157216 157219 157219
     157224 157229 157234 157240 157242 157243 157245 157259 157261
     157261 157264 157265 157270 157272 157275 157276 157279 157282
     157287 157308 157311 157315 157320 157321 157324 157326 157329
     157331 157336 157339 157341 157358 157362 157362 157368 157371
     157373 157373 157377 157381 157390 157392 157394 157409 157411
     157415 157417 157420 157427 157428 157430 157431 157433 157433
     157442 157444 157446 157448 157450 157468 157471 157473 157482
     157482 157484 157484 157493 157494 157497 157499 157501 157503
     157519 157560 157569 157569 157572 157573 157577 157577 157579
     157579 157589 157591 157593 157613 157615 157618 157621 157622
     157632 157633 157652 157653 157656 157657 157659 157661 157663
     157671 157673 157673 157679 157680 157683 157684 157696 157697
     157699 157719 157726 157728 157732 157733 157736 157737 157739
     157740 157751 157756 157758 157763 157765 157768 157772 157773
     157783 157784 157787 157788 157800 157805 157808 157810 157812
     157821 157827 157827 157829 157829 157832 157832 157834 157834
     157836 157841 157845 157846 157849 157853 157861 157867 157869
     157869 157871 157871 157880 157885 157887 157889 157894 157901
     157903 157907 157910 157910 157914 157918 157920 157920 157924
     157924 157929 157929 157934 157934 157938 157938 157940 157940
     157943 157951 157954 157956 157958 157958 157962 157962 157964
     157965 157968 157968 157970 157970 157972 157973 157976 157978
     157981 157983 157985 157987 157990 157992 157994 157996 158002
     158006 158009 158012 158015 158015 158017 158018 158021 158022
     158024 158027 158030 158037 158040 158041 158043 158043 158045
     158050 158053 158057 158059 158059 158061 158062 158066 158090
     158092 158092 158095 158097 158099 158099 158101 158110 158113
     158117 158119 158119 158121 158125 158127 158130 158134 158134
     158137 158143 158145 158146 158148 158149 158151 158154 158156
     158156 158158 158161 158163 158167 158173 158173 158175 158178
     158180 158183 158186 158187 158189 158190 158192 158193 158196
     158198 158200 158201 158203 158204 158206 158207 158209 158209
     158211 158224 158226 158234 158236 158237 158239 158244 158246
     158248 158252 158252 158255 158258 158260 158263 158266 158266
     158268 158271 158276 158276 158279 158287 158297 158297 158300
     158316 158321 158322 158324 158333 158340 158347 158349 158350
     158353 158365 158372 158373 158375 158375 158377 158382 158384
     158386 158390 158392 158397 158408 158410 158411 158414 158414
     158418 158418 158420 158421 158423 158423 158425 158425 158427
     158430 158434 158438 158442 158445 158447 158449 158451 158452
     158454 158460 158464 158473 158483 158503 158517 158517 158519
     158537 158539 158539 158557 158557 158562 158562 158569 158569
     158571 158571 158573 158611 158616 158617 158638 158638 158647
     158671 158673 158678 158680 158694 158696 158708 158712 158712
     158742 158745 158749 158750 158759 158759 158762 158789 158791
     158812 158814 158832 158834 158844 158851 158856 158892 158892
     158895 158896 158913 158958 158960 158978 158981 158981 158983
     159008 159019 159027 159056 159058 159069 159069 159071 159071
     159073 159106 159108 159131 159144 159152 159187 159189 159191
     159195 159197 159201 159203 159203 159217 159227 159229 159229
     159231 159236 159238 159268 159270 159290 159302 159309 159329
     159332 159336 159336 159338 159338 159357 159362 159366 159437
     159452 159466 159494 159494 159498 159506 159513 159513 159521
     159522 159524 159524 159529 159564 159566 159596 159598 159598
     159601 159601 159605 159611 159631 159632 159634 159634 159636
     159636 159639 159639 159660 159686 159688 159705 159712 159722
     159748 159751 159757 159757 159764 159771 159773 159773 159776
     159797 159799 159821 159823 159823 159825 159825 159828 159832
     159856 159856 159867 159878 159880 159903 159905 159905 159907
     159909 159911 159913 159920 159925 159944 159945 159950 159980
     159982 159984 159993 159996 160011 160011 160013 160035 160040
     160043 160054 160079 160083 160083 160086 160098 160105 160106
     160110 160116 160120 160120 160122 160126 160129 160129 160131
     160131 160134 160139 160143 160154 160156 160158 160163 160172
     160177 160179 160186 160191 160193 160201 160205 160205 160208
     160213 160216 160216 160220 160232 160236 160237 160241 160242
     160251 160257 160262 160265 160267 160267 160269 160270 160272
     160275 160277 160280 160282 160282 160284 160288 160296 160304
     160314 160337 160341 160341 160343 160344 160347 160348 160350
     160357 160359 160368 160371 160375 160389 160392 160397 160400
     160402 160425 160427 160433 160438 160440 160447 160447 160456
     160473 160475 160481 160495 160496 160499 160499 160513 160513
     160515 160541 160543 160548 160550 160557 160564 160565 160571
     160571 160573 160573 160575 160575 160593 160644 160648 160648
     160660 160660 160663 160664 160670 160670 160673 160678 160680
     160725 160732 160733 160740 160740 160742 160742 160744 160746
     160748 160748 160760 160770 160772 160799 160803 160803 160813
     160815 160817 160817 160819 160822 160832 160832 160836 160858
     160860 160884 160888 160888 160897 160899 160904 160904 160910
     160920 160922 160927 160929 160939 160956 160959 160968 160972
     160974 160991 160993 160994 161003 161004 161009 161028 161030
     161032 161038 161040 161048 161048 161053 161064 161066 161068
     161070 161079 161082 161082 161085 161085 161091 161106 161108
     161109 161115 161128 161134 161146 161148 161152 161154 161154
     161156 161160 161162 161167 161169 161182 161188 161188 161191
     161197 161199 161199 161203 161214 161216 161220 161222 161228
     161230 161234 161236 161247 161249 161251 161254 161261 161265
     161270 161272 161275 161279 161280 161282 161286 161288 161291
     161293 161299 161305 161306 161308 161328 161331 161331 161333
     161335 161341 161342 161344 161362 161368 161368 161371 161381
     161383 161391 161397 161397 161399 161399 161401 161401 161406
     161406 161408 161426 161429 161429 161435 161438 161441 161441
     161444 161466 161468 161469 161475 161475 161487 161516 161522
     161523 161528 161544 161546 161547 161549 161549 161553 161553
     161557 161557 161565 161575 161577 161577 161579 161586 161591
     161592 161597 161597 161602 161613 161615 161627 161634 161635
     161642 161647 161649 161666 161676 161680 161682 161687 161689
     161689 161691 161695 161698 161698 161702 161715 161718 161725
     161730 161731 161736 161738 161740 161746 161748 161755 161758
     161759 161761 161761 161763 161763 161766 161770 161775 161775
     161777 161780 161785 161785 161788 161789 161791 161791 161794
     161796 161798 161799 161801 161801 161803 161803 161806 161811
     161816 161827 161832 161835 161837 161838 161840 161840 161845
     161845 161851 161856 161861 161862 161864 161864 161867 161875
     161877 161878 161880 161880 161883 161883 161885 161885 161888
     161903 161911 161915 161917 161919 161922 161926 161933 161935
     161938 161940 161942 161942 161946 161946 161948 161950 161954
     161955 161957 161960 161963 161963 161965 161967 161969 161973
     161975 161988 161992 161993 161997 162012 162017 162019 162021
     162021 162024 162033 162036 162046 162051 162057 162059 162072
     162075 162083 162086 162087 162089 162092 162094 162095 162097
     162098 162102 162103 162106 162114 162116 162116 162120 162120
     162122 162122 162124 162125 162129 162129 162131 162131 162133
     162141 162145 162150 162152 162155 162157 162161 162163 162165
     162167 162178 162182 162183 162191 162191 162194 162194 162199
     162199 162201 162223 162234 162235 162238 162238 162241 162242
     162244 162246 162248 162273 162279 162280 162282 162283 162288
     162315 162329 162334 162336 162336 162339 162340 162342 162368
     162370 162375 162384 162385 162390 162390 162392 162392 162394
     162415 162417 162439 162452 162452 162454 162487 162489 162490
     162495 162495 162507 162507 162511 162512 162514 162544 162546
     162546 162552 162552 162554 162554 162557 162560 162562 162579
     162581 162583 162588 162589 162591 162596 162599 162614 162623
     162630 162632 162646 162648 162661 162663 162671 162676 162676
     162679 162688 162691 162698 162702 162708 162712 162715 162717
     162719 162722 162723 162725 162725 162728 162729 162731 162739
     162741 162744 162746 162749 162752 162754 162758 162758 162761
     162768 162771 162773 162776 162780 162784 162786 162789 162791
     162793 162795 162797 162801 162803 162803 162807 162817 162819
     162823 162825 162829 162832 162835 162837 162837 162840 162841
     162848 162849 162851 162851 162853 162853 162855 162864 162866
     162866 162868 162877 162879 162890 162893 162916 162920 162936
     162939 162940 162942 162965 162967 162968 162970 162975 162977
     162978 162980 162986 162988 162999 163006 163015 163017 163026
     163028 163028 163030 163030 163033 163042 163045 163055 163058
     163059 163061 163068 163073 163079 163084 163088 163092 163098
     163101 163105 163107 163110 163114 163117 163122 163123 163125
     163130 163140 163141 163143 163153 163164 163165 163168 163168
     163171 163186 163197 163197 163199 163199 163202 163202 163205
     163214 163225 163237 163242 163242 163249 163259 163268 163268
     163271 163271 163273 163287 163294 163305 163314 163314 163317
     163317 163323 163339 163345 163345 163348 163354 163358 163358
     163365 163368 163371 163371 163375 163375 163377 163379 163383
     163385 163389 163392 163398 163401 163406 163406 163416 163419
     163421 163422 163424 163424 163427 163427 163431 163431 163436
     163437 163439 163439 163442 163444 163446 163449 163453 163453
     163460 163460 163462 163464 163470 163472 163474 163476 163478
     163478 163485 163496 163506 163507 163510 163523 163534 163534
     163537 163549 163559 163559 163563 163564 163567 163574 163576
     163581 163583 163584 163593 163593 163598 163617 163621 163622
     163624 163638 163649 163650 163652 163654 163657 163670 163677
     163691 163693 163693 163695 163695 163699 163700 163702 163714
     163720 163720 163723 163723 163725 163733 163737 163749 163752
     163753 163755 163761 163764 163766 163768 163771 163776 163782
     163785 163785 163789 163789 163794 163808 163811 163812 163814
     163814 163822 163823 163825 163840 163846 163846 163859 163886
     163894 163895 163904 163925 163929 163929 163932 163932 163934
     163934 163936 163936 163938 163939 163955 163955 163957 163976
     163983 163983 163985 163985 163987 163987 163995 164024 164031
     164031 164050 164072 164078 164078 164095 164096 164099 164099
     164101 164123 164135 164141 164165 164166 164168 164168 164170
     164172 164174 164202 164209 164210 164212 164214 164224 164241
     164250 164252 164262 164264 164266 164277 164289 164296 164298
     164299 164303 164303 164308 164308 164310 164314 164317 164318
     164324 164331 164333 164340 164347 164349 164355 164357 164360
     164360 164363 164363 164367 164369 164371 164373 164376 164386
     164393 164398 164404 164405 164408 164412 164416 164416 164418
     164419 164424 164429 164436 164437 164439 164439 164443 164443
     164450 164454 164462 164465 164473 164474 164479 164481 164486
     164486 164490 164490 164493 164494 164498 164504 164507 164507
     164509 164509 164512 164514 164519 164519 164521 164522 164527
     164527 164529 164529 164531 164544 164550 164551 164556 164556
     164558 164572 164582 164594 164601 164601 164604 164604 164609
     164619 164621 164623 164630 164630 164637 164637 164639 164664
     164672 164675 164677 164692 164694 164695 164703 164704 164707
     164707 164711 164724 164726 164729 164731 164731 164735 164737
     164740 164741 164743 164749 164752 164752 164757 164758 164760
     164761 164764 164764 164769 164770 164772 164783 164785 164793
     164798 164798 164801 164801 164805 164811 164814 164815 164817
     164822 164824 164825 164829 164836 164838 164838 164840 164844
     164847 164854 164856 164858 164861 164862 164865 164866 164875
     164877 164879 164881 164884 164885 164888 164888 164891 164898
     164904 164905 164911 164914 164917 164918 164921 164921 164924
     164924 164926 164926 164928 164935 164937 164939 164941 164941
     164943 164948 164952 164952 164955 164955 164958 164958 164960
     164973 164981 164981 164983 164983 164988 164993 164995 165007
     165018 165019 165021 165021 165024 165024 165026 165040 165042
     165043 165052 165061 165063 165067 165069 165070 165074 165074
     165078 165081 165083 165083 165085 165085 165092 165097 165099
     165115 165117 165118 165120 165120 165123 165123 165125 165125
     165131 165131 165142 165143 165145 165151 165153 165156 165158
     165170 165197 165197 165199 165201 165203 165204 165206 165206
     165210 165226 165240 165240 165244 165245 165249 165249 165251
     165251 165254 165264 165267 165267 165273 165273 165277 165281
     165283 165285 165290 165297 165299 165308 165310 165316 165323
     165324 165328 165328 165331 165333 165335 165337 165342 165343
     165345 165347 165349 165361 165370 165373 165377 165381 165383
     165386 165388 165389 165393 165393 165395 165395 165401 165411
     165418 165422 165424 165424 165426 165426 165429 165429 165431
     165433 165436 165436 165439 165446 165454 165461 165463 165466
     165468 165468 165470 165472 165475 165476 165478 165481 165483
     165483 165485 165490 165493 165502 165506 165506 165513 165515
     165517 165517 165519 165519 165521 165531 165536 165536 165538
     165538 165541 165541 165544 165544 165548 165558 165563 165565
     165567 165567 165569 165569 165571 165572 165574 165590 165593
     165596 165598 165601 165603 165607 165611 165617 165619 165624
     165627 165627 165631 165635 165637 165638 165640 165640 165643
     165660 165667 165672 165674 165674 165678 165680 165682 165697
     165699 165699 165703 165703 165705 165713 165715 165717 165719
     165720 165723 165723 165725 165731 165733 165736 165738 165745
     165747 165752 165760 165765 165767 165767 165769 165769 165772
     165775 165777 165788 165790 165790 165792 165793 165798 165803
     165806 165809 165811 165811 165813 165829 165832 165832 165837
     165841 165844 165852 165854 165856 165858 165860 165862 165862
     165864 165864 165867 165868 165870 165872 165874 165874 165876
     165890 165894 165896 165898 165912 165922 165922 165924 165927
     165934 165934 165942 165945 165950 165950 165953 165953 165957
     165957 165963 165969 165971 165976 165981 165981 165984 165984
     165986 165987 165993 166008 166016 166017 166019 166034 166041
     166043 166048 166061 166063 166066 166069 166070 166073 166074
     166077 166087 166091 166091 166094 166094 166096 166097 166099
     166109 166112 166113 166115 166115 166118 166122 166125 166131
     166134 166140 166142 166146 166149 166149 166151 166154 166156
     166163 166165 166170 166173 166173 166177 166179 166181 166182
     166184 166185 166188 166189 166196 166196 166199 166199 166201
     166202 166206 166206 166208 166208 166210 166212 166218 166226
     166242 166242 166245 166250 166258 166259 166261 166261 166265
     166265 166267 166267 166274 166274 166276 166278 166294 166294
     166297 166297 166300 166301 166303 166303 166307 166307 166316
     166324 166327 166328 166334 166335 166350 166351 166355 166355
     166357 166357 166361 166361 166363 166363 166373 166390 166398
     166398 166444 166444 166446 166462 166505 166505 166508 166508
     166517 166517 166521 166522 166532 166533 166535 166548 166550
     166551 166559 166560 166582 166583 166588 166589 166607 166614
     166616 166617 166630 166630 166649 166649 166651 166651 166653
     166653 166659 166661 166669 166676 166678 166683 166685 166685
     166687 166687 166724 166724 166727 166727 166739 166739 166741
     166751 166780 166780 166782 166782 166784 166784 166787 166788
     166802 166805 166807 166807 166810 166810 166827 166827 166831
     166831 166839 166841 166843 166843 166845 166847 166852 166852
     166866 166866 166868 166868 166878 166883 166906 166908 166917
     166917 166921 166930 166941 166944 166948 166948 166953 166953
     166958 166959 166962 166963 166968 166968 166999 167006 167010
     167010 167012 167017 167027 167031 167034 167034 167039 167043
     167046 167052 167059 167061 167063 167064 167066 167067 167069
     167074 167077 167081 167085 167087 167090 167092 167094 167094
     167096 167096 167098 167100 167102 167102 167104 167109 167113
     167116 167119 167121 167126 167139 167141 167141 167152 167152
     167154 167159 167161 167170 167181 167193 167197 167197 167199
     167200 167205 167205 167208 167217 167219 167219 167224 167224
     167231 167233 167236 167238 167240 167245 167247 167251 167255
     167255 167260 167260 167266 167266 167268 167280 167282 167282
     167285 167285 167287 167287 167289 167289 167296 167307 167318
     167318 167322 167322 167324 167330 167340 167348 167350 167351
     167358 167358 167361 167367 167378 167381 167385 167385 167396
     167396 167398 167398 167402 167403 167408 167411 167415 167416
     167419 167419 167439 167448 167450 167454 167467 167475 167489
     167500 167502 167502 167507 167507 167509 167509 167512 167523
     167527 167527 167541 167558 167583 167599 167605 167605 167615
     167638 167640 167640 167654 167654 167656 167656 167658 167658
     167663 167679 167686 167686 167688 167688 167690 167705 167718
     167718 167722 167729 167741 167748 167757 167761 167766 167769
     167773 167773 167775 167777 167781 167784 167786 167788 167790
     167790 167793 167798 167800 167801 167804 167805 167807 167811
     167813 167819 167821 167821 167823 167823 167825 167837 167840
     167851 167854 167855 167858 167875 167878 167878 167880 167881
     167883 167898 167902 167902 167904 167926 167929 167929 167933
     167933 167935 167953 167956 167957 167959 167969 167972 167985
     167988 168000 168002 168002 168005 168016 168018 168024 168027
     168036 168039 168041 168043 168044 168048 168048 168050 168050
     168053 168053 168055 168055 168057 168060 168066 168073 168082
     168088 168097 168106 168108 168109 168115 168115 168117 168117
     168119 168131 168140 168140 168144 168158 168160 168161 168180
     168186 168188 168193 168196 168196 168201 168201 168214 168231
     168235 168235 168237 168238 168247 168248 168250 168263 168265
     168266 168270 168272 168274 168275 168285 168300 168305 168306
     168310 168311 168316 168329 168337 168344 168346 168347 168351
     168351 168356 168358 168363 168363 168365 168370 168373 168376
     168378 168378 168381 168381 168383 168386 168388 168388 168391
     168396 168398 168398 168401 168405 168407 168407 168411 168416
     168419 168419 168422 168427 168429 168433 168437 168438 168440
     168442 168444 168444 168446 168447 168451 168458 168460 168463
     168465 168466 168470 168470 168473 168477 168479 168479 168481
     168486 168488 168488 168491 168495 168497 168502 168504 168505
     168507 168510 168512 168516 168519 168525 168529 168529 168532
     168532 168534 168534 168537 168538 168541 168543 168545 168556
     168568 168581 168584 168585 168587 168587 168592 168607 168613
     168614 168617 168624 168626 168627 168631 168633 168636 168636
     168639 168660 168667 168667 168669 168690 168693 168694 168696
     168698 168700 168704 168707 168724 168730 168731 168733 168733
     168736 168738 168742 168755 168758 168758 168761 168763 168766
     168775 168777 168779 168782 168783 168785 168785 168787 168799
     168804 168811 168814 168815 168817 168821 168823 168825 168830
     168834 168836 168837 168839 168847 168849 168853 168855 168861
     168865 168865 168868 168883 168885 168895 168897 168897 168899
     168899 168901 168907 168910 168922 168926 168934 168936 168946
     168948 168955 168961 168965 168967 168967 168969 168973 168975
     168975 168979 168982 168984 168988 168990 169000 169003 169006
     169008 169010 169012 169015 169017 169020 169022 169027 169029
     169029 169031 169037 169040 169066 169068 169068 169070 169070
     169072 169086 169088 169092 169094 169100 169103 169119 169122
     169122 169124 169124 169126 169127 169130 169131 169133 169158
     169169 169169 169171 169174 169176 169176 169181 169201 169208
     169208 169210 169214 169216 169228 169235 169236 169241 169257
     169263 169264 169267 169278 169280 169280 169282 169282 169285
     169286 169288 169304 169309 169316 169318 169318 169321 169321
     169323 169327 169329 169334 169336 169336 169338 169338 169340
     169340 169342 169345 169348 169348 169350 169351 169354 169360
     169363 169363 169369 169373 169377 169385 169388 169388 169390
     169390 169392 169392 169394 169406 169409 169409 169412 169419
     169427 169435 169437 169440 169442 169443 169448 169464 169473
     169473 169475 169475 169477 169491 169493 169495 169497 169498
     169500 169505 169508 169511 169517 169523 169525 169535 169537
     169542 169544 169544 169546 169546 169550 169550 169553 169556
     169558 169559 169561 169566 169569 169569 169572 169572 169585
     169586 169588 169588 169590 169591 169594 169594 169596 169597
     169600 169615 169618 169619 169621 169630 169635 169636 169640
     169659 169663 169665 169669 169670 169672 169683 169691 169691
     169693 169711 169715 169715 169721 169744 169746 169751 169754
     169754 169759 169759 169762 169762 169772 169773 169775 169794
     169801 169801 169815 169819 169821 169833 169835 169836 169838
     169838 169840 169840 169844 169845 169848 169849 169852 169873
     169881 169882 169886 169902 169906 169906 169909 169909 169911
     169911 169914 169914 169916 169933 169939 169945 169947 169947
     169955 169960 169962 169962 169965 169969 169972 169972 169974
     169976 170000 170001 170005 170007 170009 170012 170018 170022
     170026 170028 170032 170032 170034 170034 170039 170039 170041
     170041 170043 170044 170046 170048 170052 170053 170056 170056
     170058 170058 170060 170062 170064 170069 170072 170072 170074
     170074 170077 170077 170081 170081 170084 170085 170087 170090
     170094 170094 170096 170099 170101 170101 170103 170111 170118
     170118 170120 170121 170124 170145 170151 170151 170157 170181
     170183 170184 170186 170186 170188 170190 170192 170192 170194
     170194 170197 170212 170221 170241 170249 170249 170256 170256
     170258 170259 170262 170285 170291 170291 170295 170295 170299
     170299 170301 170309 170311 170324 170326 170328 170332 170332
     170334 170334 170338 170338 170340 170340 170347 170356 170362
     170362 170368 170385 170388 170388 170393 170393 170395 170396
     170398 170408 170419 170419 170422 170432 170434 170443 170446
     170448 170452 170454 170456 170459 170464 170469 170513 170514
     170517 170519 170523 170532 170534 170535 170540 170552 170554
     170554 170557 170563 170566 170566 170569 170575 170579 170579
     170581 170581 170583 170595 170598 170598 170605 170614 170616
     170617 170621 170622 170624 170630 170633 170642 170644 170645
     170647 170652 170656 170667 170669 170670 170674 170675 170679
     170681 170683 170684 170686 170691 170697 170698 170702 170703
     170709 170709 170711 170711 170714 170714 170718 170718 170720
     170720 170725 170731 170733 170737 170739 170740 170743 170743
     170745 170746 170751 170753 170755 170756 170761 170763 170767
     170786 170800 170813 170819 170819 170823 170835 170840 170840
     170842 170842 170845 170859 170871 170872 170874 170889 170902
     170915 170918 170918 170926 170938 170941 170942 170947 170961
     170971 170976 170980 170980 170982 170985 170989 170989 170992
     170995 171000 171000 171002 171003 171007 171007 171009 171011
     171013 171015 171017 171031 171033 171033 171036 171048 171050
     171055 171057 171063 171065 171065 171068 171068 171070 171073
     171075 171077 171079 171092 171094 171101 171103 171113 171115
     171117 171119 171125 171127 171130 171133 171141 171144 171144
     171146 171153 171156 171156 171160 171166 171169 171171 171173
     171180 171187 171187 171190 171190 171193 171193 171195 171202
     171205 171209 171211 171222 171224 171225 171229 171229 171231
     171242 171245 171250 171253 171258 171264 171264 171267 171267
     171270 171275 171277 171278 171280 171285 171288 171289 171291
     171300 171304 171306 171309 171311 171313 171316 171318 171320
     171322 171323 171327 171330 171332 171333 171335 171336 171338
     171345 171348 171351 171355 171355 171360 171377 171382 171385
     171392 171406 171409 171409 171414 171414 171418 171418 171422
     171422 171424 171424 171426 171441 171446 171446 171453 171453
     171455 171455 171458 171459 171467 171482 171484 171484 171500
     171500 171502 171502 171508 171509 171511 171536 171551 171552
     171554 171554 171557 171557 171559 171560 171577 171577 171580
     171580 171582 171602 171618 171618 171633 171654 171673 171676
     171691 171691 171693 171710 171721 171723 171725 171725 171734
     171734 171736 171752 171762 171762 171764 171766 171778 171790
     171792 171794 171800 171800 171804 171809 171813 171822 171825
     171825 171828 171829 171835 171841 171852 171856 171859 171859
     171861 171861 171864 171864 171866 171873 171875 171875 171879
     171884 171888 171889 171891 171891 171893 171895 171919 171919
     171921 171929 171931 171943 171948 171948 171952 171980 171984
     171991 171995 172014 172017 172018 172023 172024 172028 172028
     172032 172033 172039 172045 172047 172047 172050 172059 172061
     172077 172088 172088 172090 172090 172092 172093 172095 172096
     172104 172138 172141 172142 172144 172144 172146 172146 172151
     172153 172155 172177 172179 172181 172186 172187 172189 172191
     172193 172193 172195 172195 172197 172197 172202 172202 172205
     172205 172212 172248 172250 172253 172256 172256 172260 172260
     172262 172263 172265 172266 172271 172271 172278 172278 172281
     172295 172297 172307 172311 172312 172315 172316 172318 172318
     172320 172320 172329 172331 172333 172345 172347 172366 172374
     172374 172378 172378 172380 172380 172383 172383 172385 172385
     172398 172429 172433 172433 172436 172436 172438 172439 172442
     172442 172445 172445 172449 172449 172456 172459 172461 172479
     172481 172482 172484 172484 172486 172486 172489 172490 172497
     172504 172506 172506 172508 172509 172512 172513 172524 172538
     172540 172540 172543 172543 172545 172545 172547 172551 172553
     172557 172560 172560 172562 172563 172568 172575 172578 172578
     172580 172587 172590 172593 172596 172597 172599 172604 172607
     172608 172630 172639 172641 172641 172643 172644 172646 172649
     172653 172659 172661 172664 172666 172666 172668 172668 172671
     172676 172678 172681 172683 172683 172685 172685 172687 172689
     172691 172697 172699 172700 172709 172712 172714 172717 172719
     172719 172721 172721 172723 172732 172739 172745 172747 172747
     172749 172753 172756 172762 172764 172765 172769 172771 172773
     172773 172775 172775 172778 172780 172782 172784 172786 172787
     172791 172792 172794 172798 172801 172802 172805 172814 172816
     172818 172820 172824 172826 172827 172829 172829 172832 172832
     172834 172837 172840 172840 172844 172851 172856 172856 172859
     172871 172873 172873 172875 172885 172888 172888 172890 172912
     172914 172918 172920 172932 172934 172935 172946 172951 172954
     172958 172960 172961 172963 172968 172970 172976 172978 172978
     172980 172982 172985 172987 172989 172990 172993 172995 172998
     173005 173009 173009 173011 173014 173016 173022 173025 173025
     173028 173032 173036 173044 173046 173056 173058 173060 173063
     173065 173067 173071 173073 173081 173083 173089 173092 173096
     173098 173099 173106 173108 173113 173113 173119 173124 173128
     173134 173138 173150 173155 173155 173157 173172 173175 173175
     173182 173189 173192 173192 173195 173196 173199 173206 173208
     173219 173223 173223 173225 173232 173236 173237 173240 173243
     173246 173249 173251 173252 173254 173254 173257 173260 173262
     173272 173274 173279 173281 173282 173284 173285 173287 173289
     173292 173305 173307 173314 173316 173318 173320 173320 173322
     173323 173325 173329 173332 173336 173339 173341 173346 173356
     173358 173361 173363 173363 173365 173368 173370 173380 173382
     173382 173384 173391 173393 173396 173399 173407 173409 173409
     173412 173412 173415 173423 173425 173425 173429 173435 173437
     173443 173445 173448 173451 173464 173466 173467 173473 173482
     173486 173488 173491 173491 173493 173495 173499 173502 173505
     173506 173509 173511 173513 173517 173519 173522 173524 173525
     173528 173549 173551 173551 173554 173554 173557 173569 173572
     173573 173578 173579 173581 173592 173595 173596 173599 173602
     173604 173610 173612 173614 173616 173616 173618 173618 173620
     173620 173622 173635 173638 173651 173654 173663 173667 173671
     173673 173674 173676 173676 173678 173678 173680 173680 173682
     173689 173691 173693 173695 173700 173702 173702 173704 173705
     173707 173710 173713 173715 173717 173721 173724 173728 173733
     173736 173739 173745 173747 173747 173749 173764 173768 173768
     173770 173771 173773 173782 194560 194566 194568 194571 194573
     194575 194577 194590 194592 194595 194597 194611 194613 194623
     194625 194649 194651 194656 194658 194659 194661 194664 194666
     194669 194671 194680 194682 194690 194692 194707 194710 194717
     194719 194721 194723 194743 194745 194750 194752 194755 194757
     194758 194760 194761 194763 194764 194767 194768 194770 194780
     194782 194794 194796 194802 194804 194805 194807 194808 194810
     194810 194812 194821 194823 194828 194830 194832 194834 194834
     194836 194843 194845 194845 194848 194857 194859 194859 194862
     194862 194864 194865 194867 194874 194876 194878 194880 194884
     194886 194889 194891 194897 194899 194899 194901 194911 194913
     194913 194915 194924 194926 194931 194933 194936 194938 194939
     194941 194942 194944 194949 194953 194961 194963 194963 194965
     194969 194971 194982 194984 194987 194989 194989 194991 194993
     194995 195003 195005 195005 195007 195015 195017 195017 195019
     195021 195023 195027 195029 195033 195035 195046 195048 195050
     195052 195052 195054 195056 195058 195063 195065 195080 195082
     195090 195092 195101 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 5 :NAME "ISO_8859-2:1987" :ALIASES
   '("csISOLatin2" "l2" "latin2" #13="ISO-8859-2" "ISO_8859-2"
     "iso-ir-101")
   :MIME-ENCODING '#13# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: ISO-8859-2 (preferred MIME name)") :REFERENCES
   '("[RFC1345,KXS2]") :RANGES
   #(0 160 164 164 167 168 173 173 176 176 180 180 184 184 193 194 196
     196 199 199 201 201 203 203 205 206 211 212 214 215 218 218 220 221
     223 223 225 226 228 228 231 231 233 233 235 235 237 238 243 244 246
     247 250 250 252 253 258 263 268 273 280 283 313 314 317 318 321 324
     327 328 336 337 340 341 344 347 350 357 366 369 377 382 711 711 728
     729 731 731 733 733))
  (MAKE-CHARACTER-SET :MIB-ENUM 6 :NAME "ISO_8859-3:1988" :ALIASES
   '("csISOLatin3" "l3" "latin3" #14="ISO-8859-3" "ISO_8859-3"
     "iso-ir-109")
   :MIME-ENCODING '#14# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: ISO-8859-3 (preferred MIME name)") :REFERENCES
   '("[RFC1345,KXS2]") :RANGES
   #(0 160 163 164 167 168 173 173 176 176 178 181 183 184 189 189 192
     194 196 196 199 207 209 212 214 215 217 220 223 226 228 228 231 239
     241 244 246 247 249 252 264 267 284 289 292 295 304 305 308 309 348
     351 364 365 379 380 728 729))
  (MAKE-CHARACTER-SET :MIB-ENUM 2013 :NAME "IBM862" :ALIASES
   '("csPC862LatinHebrew" "862" "cp862") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 163 165 165 170 172 176 178 183 183 186 189 191 191 209
     209 223 223 225 225 237 237 241 241 243 243 247 247 250 250 402 402
     915 915 920 920 931 931 934 934 937 937 945 945 948 949 956 956 960
     960 963 964 966 966 1488 1514 8319 8319 8359 8359 8729 8730 8734
     8734 8745 8745 8776 8776 8801 8801 8804 8805 8976 8976 8992 8993
     9472 9472 9474 9474 9484 9484 9488 9488 9492 9492 9496 9496 9500
     9500 9508 9508 9516 9516 9524 9524 9532 9532 9552 9580 9600 9600
     9604 9604 9608 9608 9612 9612 9616 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 109 :NAME "ISO-8859-13" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"ISO See (http://www.iana.org/assignments/charset-reg/ISO-8859-13)[Tumasonis]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 160 162 164 166 167 169 169 171 174 176 179 181 183 185 185 187
     190 196 198 201 201 211 211 213 216 220 220 223 223 228 230 233 233
     243 243 245 248 252 252 256 257 260 263 268 269 274 275 278 281 290
     291 298 299 302 303 310 311 315 316 321 326 332 333 342 343 346 347
     352 353 362 363 370 371 377 382 8217 8217 8220 8222))
  (MAKE-CHARACTER-SET :MIB-ENUM 110 :NAME "ISO-8859-14" :ALIASES
   '("l8" "iso-celtic" "latin8" "ISO_8859-14" "ISO_8859-14:1998"
     "iso-ir-199")
   :MIME-ENCODING 'NIL :SOURCE
   '"ISO See (http://www.iana.org/assignments/charset-reg/ISO-8859-14) [Simonsen]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 160 163 163 167 167 169 169 173 174 182 182 192 207 209 214 216
     221 223 239 241 246 248 253 255 255 266 267 288 289 372 376 7682
     7683 7690 7691 7710 7711 7744 7745 7766 7767 7776 7777 7786 7787
     7808 7813 7922 7923))
  (MAKE-CHARACTER-SET :MIB-ENUM 2258 :NAME "windows-1258" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1258) [Lazhintseva]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 160 207 209 221 223 239 241 253 255 255 258 259 262 263 272
     273 296 297 313 314 323 324 338 341 346 347 360 361 376 378 402 402
     416 417 431 432 471 472 475 476 500 501 504 511 710 710 732 732 768
     769 771 771 777 777 803 803 832 833 901 901 7684 7685 7688 7689
     7692 7693 7716 7717 7726 7731 7734 7735 7742 7743 7746 7747 7750
     7751 7756 7759 7764 7765 7770 7771 7778 7779 7788 7789 7800 7801
     7804 7811 7816 7817 7826 7827 7840 7929 8173 8174 8211 8212 8216
     8218 8220 8222 8224 8226 8230 8230 8240 8240 8249 8250 8363 8364
     8482 8482 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 111 :NAME "ISO-8859-15" :ALIASES
   '("Latin-9" "ISO_8859-15") :MIME-ENCODING 'NIL :SOURCE
   '"ISO  Please see: <http://www.iana.org/assignments/charset-reg/ISO-8859-15>"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 163 165 165 167 167 169 179 181 183 185 187 191 255 338 339 352
     353 376 376 381 382 8364 8364))
  (MAKE-CHARACTER-SET :MIB-ENUM 112 :NAME "ISO-8859-16" :ALIASES
   '("l10" "latin10" "ISO_8859-16" "ISO_8859-16:2001" "iso-ir-226")
   :MIME-ENCODING 'NIL :SOURCE '"ISO" :COMMENTS 'NIL :REFERENCES 'NIL
   :RANGES
   #(0 160 167 167 169 169 171 171 173 173 176 177 182 183 187 187 192
     194 196 196 198 207 210 212 214 214 217 220 223 226 228 228 230 239
     242 244 246 246 249 252 255 255 258 263 268 269 272 273 280 281 321
     324 336 339 346 347 352 353 368 369 376 382 536 539 8221 8222 8364
     8364))
  (MAKE-CHARACTER-SET :MIB-ENUM 2025 :NAME #15="GB2312" :ALIASES
   '("csGB2312") :MIME-ENCODING '#15# :SOURCE
   '"Chinese for People's Republic of China (PRC) mixed one byte,  two byte set: 20-7E = one byte ASCII A1-FE = two byte PRC Kanji See GB 2312-80 PCL Symbol Set Id: 18C"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 164 164 167 168 176 177 215 215 224 225 232 234 236 237 242
     243 247 247 249 250 252 252 257 257 275 275 283 283 299 299 333 333
     363 363 462 462 464 464 466 466 468 468 470 470 472 472 474 474 476
     476 711 711 713 713 913 929 931 937 945 961 963 969 1025 1025 1040
     1103 1105 1105 8213 8214 8216 8217 8220 8221 8230 8230 8240 8240
     8242 8243 8251 8251 8451 8451 8470 8470 8544 8555 8592 8595 8712
     8712 8719 8719 8721 8721 8730 8730 8733 8734 8736 8736 8741 8741
     8743 8747 8750 8750 8756 8759 8765 8765 8776 8776 8780 8780 8800
     8801 8804 8805 8814 8815 8857 8857 8869 8869 8978 8978 9312 9321
     9332 9371 9472 9547 9632 9633 9650 9651 9670 9671 9675 9675 9678
     9679 9733 9734 9792 9792 9794 9794 12288 12291 12293 12293 12296
     12305 12307 12311 12353 12435 12449 12534 12539 12539 12549 12585
     12832 12841 19968 19969 19971 19971 19975 19982 19984 19985 19987
     19990 19992 19998 20002 20002 20004 20005 20007 20008 20010 20013
     20016 20016 20018 20018 20020 20020 20022 20022 20024 20027 20029
     20031 20035 20035 20037 20037 20039 20041 20043 20048 20050 20052
     20054 20054 20056 20057 20060 20065 20070 20070 20073 20073 20080
     20081 20083 20083 20094 20094 20102 20102 20104 20105 20107 20111
     20113 20117 20120 20120 20122 20123 20127 20130 20132 20137 20139
     20142 20146 20147 20149 20149 20154 20155 20159 20167 20169 20171
     20173 20174 20177 20177 20179 20185 20189 20191 20193 20193 20195
     20197 20200 20200 20202 20204 20208 20208 20210 20211 20213 20215
     20219 20219 20221 20221 20223 20223 20225 20225 20233 20234 20237
     20241 20247 20251 20254 20256 20258 20258 20260 20263 20266 20267
     20271 20272 20274 20274 20276 20276 20278 20278 20280 20280 20282
     20282 20284 20285 20291 20291 20294 20294 20301 20305 20307 20307
     20309 20309 20311 20320 20323 20325 20327 20327 20329 20329 20332
     20332 20335 20336 20339 20340 20342 20342 20347 20348 20350 20351
     20355 20356 20360 20361 20363 20363 20365 20365 20367 20367 20369
     20369 20372 20372 20375 20375 20379 20379 20381 20381 20384 20384
     20387 20387 20389 20394 20396 20396 20398 20399 20405 20405 20415
     20415 20419 20421 20426 20426 20430 20433 20439 20440 20442 20442
     20444 20447 20449 20449 20451 20451 20454 20454 20456 20458 20461
     20463 20465 20465 20467 20467 20472 20472 20474 20474 20478 20478
     20492 20493 20495 20495 20498 20498 20500 20500 20504 20506 20508
     20508 20511 20511 20513 20513 20517 20518 20520 20522 20524 20526
     20538 20538 20540 20540 20542 20542 20547 20547 20551 20552 20556
     20556 20558 20559 20565 20565 20570 20570 20572 20572 20581 20581
     20588 20588 20598 20599 20603 20603 20606 20608 20613 20613 20616
     20616 20621 20621 20643 20643 20645 20645 20647 20649 20652 20652
     20658 20658 20666 20667 20687 20687 20694 20694 20698 20698 20710
     20711 20716 20718 20723 20723 20725 20725 20731 20731 20742 20743
     20747 20747 20754 20754 20769 20769 20799 20801 20803 20806 20808
     20809 20811 20811 20813 20813 20817 20817 20820 20822 20826 20826
     20828 20828 20834 20834 20837 20837 20840 20840 20843 20846 20848
     20849 20851 20857 20859 20861 20864 20866 20869 20869 20872 20873
     20876 20877 20882 20882 20885 20887 20889 20889 20891 20892 20896
     20896 20898 20898 20900 20901 20907 20908 20911 20915 20917 20919
     20923 20925 20928 20928 20932 20932 20934 20935 20937 20937 20939
     20940 20943 20943 20945 20945 20955 20955 20957 20957 20960 20961
     20964 20964 20971 20971 20973 20973 20975 20976 20979 20979 20981
     20982 20984 20989 20991 20995 20998 21000 21002 21002 21005 21006
     21009 21010 21014 21019 21021 21021 21024 21024 21028 21028 21032
     21033 21035 21035 21037 21038 21040 21040 21043 21043 21046 21051
     21053 21053 21055 21059 21066 21066 21068 21069 21072 21073 21076
     21076 21078 21078 21084 21084 21086 21086 21089 21089 21093 21093
     21095 21095 21097 21098 21103 21103 21106 21106 21117 21117 21119
     21119 21121 21122 21128 21128 21136 21136 21139 21139 21147 21147
     21149 21155 21160 21165 21169 21171 21182 21183 21187 21187 21191
     21191 21193 21193 21195 21195 21200 21200 21202 21202 21206 21206
     21208 21208 21215 21215 21220 21220 21232 21232 21241 21242 21246
     21248 21253 21254 21256 21256 21261 21261 21263 21264 21269 21271
     21273 21274 21277 21277 21280 21281 21283 21283 21286 21286 21290
     21290 21294 21294 21305 21307 21310 21311 21313 21313 21315 21315
     21317 21317 21319 21322 21326 21327 21329 21331 21333 21335 21338
     21338 21340 21340 21342 21348 21350 21351 21353 21353 21355 21355
     21358 21361 21363 21365 21367 21368 21370 21370 21375 21375 21378
     21378 21380 21382 21385 21385 21387 21389 21397 21397 21400 21400
     21402 21402 21405 21405 21407 21407 21410 21411 21413 21414 21416
     21417 21422 21422 21430 21430 21435 21435 21439 21439 21441 21442
     21448 21453 21457 21457 21460 21460 21462 21465 21467 21467 21471
     21472 21475 21478 21480 21491 21493 21497 21499 21501 21505 21505
     21507 21508 21510 21510 21512 21514 21516 21523 21525 21527 21531
     21531 21533 21537 21539 21539 21542 21545 21547 21551 21553 21554
     21556 21557 21560 21561 21563 21564 21566 21566 21568 21568 21571
     21571 21574 21574 21576 21576 21578 21579 21584 21584 21586 21593
     21595 21596 21602 21602 21604 21604 21606 21606 21608 21608 21617
     21619 21621 21624 21627 21629 21632 21632 21634 21634 21636 21636
     21638 21638 21643 21644 21646 21648 21650 21650 21652 21654 21657
     21659 21661 21661 21667 21668 21670 21677 21679 21679 21681 21681
     21683 21684 21688 21688 21691 21691 21693 21693 21695 21698 21700
     21700 21702 21705 21708 21717 21719 21719 21721 21722 21724 21727
     21733 21738 21741 21742 21746 21747 21754 21754 21756 21757 21759
     21759 21761 21761 21766 21767 21769 21769 21775 21777 21780 21780
     21787 21787 21792 21792 21794 21796 21799 21799 21802 21802 21804
     21804 21806 21809 21811 21811 21815 21815 21820 21820 21822 21823
     21825 21825 21827 21828 21830 21830 21833 21834 21840 21840 21845
     21846 21852 21852 21857 21857 21860 21863 21866 21866 21868 21870
     21877 21880 21883 21884 21886 21886 21888 21892 21895 21899 21903
     21903 21905 21905 21908 21908 21912 21913 21916 21917 21919 21919
     21927 21927 21937 21937 21939 21939 21941 21941 21943 21943 21945
     21945 21947 21947 21949 21950 21956 21957 21961 21961 21964 21965
     21969 21972 21974 21974 21980 21981 21983 21983 21985 21985 21987
     21990 21992 21992 21994 21996 21999 21999 22002 22003 22005 22005
     22007 22007 22013 22014 22016 22017 22024 22025 22028 22028 22030
     22031 22040 22040 22043 22043 22046 22047 22051 22052 22055 22055
     22060 22061 22065 22066 22068 22068 22070 22070 22073 22073 22075
     22075 22079 22079 22092 22094 22100 22100 22103 22105 22108 22108
     22114 22114 22116 22116 22120 22124 22129 22129 22134 22134 22139
     22140 22149 22150 22158 22159 22163 22163 22179 22179 22191 22191
     22199 22199 22204 22204 22218 22218 22228 22228 22231 22231 22234
     22235 22237 22242 22244 22244 22251 22251 22253 22253 22256 22257
     22260 22261 22265 22266 22269 22271 22275 22276 22278 22278 22280
     22282 22300 22300 22303 22303 22307 22307 22312 22314 22316 22320
     22323 22323 22329 22331 22334 22334 22336 22336 22338 22338 22343
     22343 22346 22346 22348 22353 22359 22359 22362 22369 22372 22372
     22374 22374 22376 22379 22381 22381 22383 22383 22387 22387 22390
     22391 22395 22396 22402 22406 22411 22412 22418 22419 22427 22427
     22432 22436 22438 22439 22441 22441 22443 22443 22445 22446 22450
     22450 22452 22452 22456 22456 22466 22467 22475 22475 22478 22479
     22482 22482 22484 22485 22488 22490 22493 22493 22495 22496 22500
     22500 22509 22509 22511 22511 22516 22516 22520 22522 22525 22525
     22528 22528 22530 22530 22534 22535 22539 22539 22541 22541 22545
     22545 22549 22549 22553 22553 22558 22558 22560 22561 22564 22564
     22570 22570 22576 22576 22581 22581 22596 22596 22604 22605 22609
     22609 22612 22612 22616 22616 22622 22622 22629 22629 22635 22636
     22654 22654 22656 22657 22659 22659 22661 22661 22665 22665 22674
     22675 22681 22682 22686 22687 22696 22697 22716 22716 22721 22721
     22725 22725 22737 22737 22741 22741 22756 22756 22763 22764 22766
     22766 22768 22768 22771 22771 22774 22774 22777 22777 22786 22786
     22788 22788 22791 22791 22797 22797 22799 22799 22804 22806 22809
     22810 22812 22812 22815 22815 22820 22821 22823 22823 22825 22827
     22829 22831 22833 22833 22836 22836 22839 22842 22844 22844 22849
     22850 22852 22852 22855 22857 22859 22859 22862 22863 22865 22865
     22868 22872 22874 22874 22880 22880 22882 22882 22885 22885 22899
     22900 22902 22902 22904 22905 22909 22909 22913 22916 22918 22920
     22922 22922 22925 22925 22930 22931 22934 22935 22937 22937 22942
     22942 22947 22949 22952 22955 22958 22959 22962 22962 22969 22969
     22971 22971 22974 22974 22982 22982 22986 22987 22992 22996 22999
     23000 23002 23002 23004 23005 23011 23011 23013 23013 23016 23016
     23020 23020 23033 23033 23035 23035 23039 23039 23041 23041 23043
     23049 23052 23052 23057 23057 23059 23059 23064 23064 23068 23068
     23071 23072 23075 23075 23077 23077 23081 23081 23089 23090 23092
     23092 23094 23094 23100 23100 23104 23104 23110 23110 23113 23114
     23125 23125 23130 23130 23138 23138 23143 23143 23146 23146 23156
     23159 23162 23162 23167 23167 23186 23186 23194 23195 23210 23210
     23218 23219 23221 23221 23224 23224 23230 23230 23233 23234 23241
     23241 23244 23244 23250 23250 23252 23252 23254 23254 23256 23256
     23260 23260 23264 23265 23267 23267 23270 23270 23273 23273 23275
     23275 23281 23281 23305 23305 23318 23319 23346 23346 23348 23348
     23351 23351 23360 23360 23376 23377 23379 23381 23383 23389 23391
     23391 23394 23398 23401 23402 23404 23404 23408 23409 23411 23411
     23413 23413 23418 23418 23421 23421 23424 23425 23427 23429 23431
     23433 23435 23436 23439 23439 23443 23443 23445 23445 23447 23454
     23456 23462 23466 23467 23472 23472 23475 23478 23480 23481 23485
     23487 23490 23490 23492 23495 23500 23500 23504 23504 23506 23507
     23517 23519 23521 23521 23524 23525 23528 23528 23534 23534 23536
     23536 23544 23548 23551 23551 23553 23553 23556 23556 23558 23558
     23561 23562 23567 23567 23569 23569 23572 23574 23576 23576 23578
     23578 23580 23581 23586 23586 23588 23589 23591 23591 23596 23596
     23601 23601 23604 23604 23608 23618 23621 23621 23624 23627 23630
     23633 23637 23637 23641 23641 23646 23646 23648 23649 23651 23651
     23653 23654 23662 23663 23665 23665 23673 23674 23679 23679 23681
     23682 23688 23688 23692 23693 23696 23697 23700 23700 23702 23708
     23714 23715 23721 23721 23723 23725 23729 23729 23731 23731 23733
     23733 23735 23736 23741 23741 23743 23743 23745 23745 23748 23748
     23755 23755 23762 23762 23769 23769 23777 23777 23780 23782 23784
     23784 23786 23786 23789 23789 23792 23792 23803 23803 23810 23811
     23814 23815 23822 23822 23828 23828 23830 23830 23835 23835 23838
     23838 23844 23844 23846 23847 23849 23849 23853 23854 23860 23860
     23869 23870 23879 23879 23882 23884 23896 23896 23899 23899 23901
     23901 23913 23913 23915 23916 23919 23919 23924 23924 23938 23938
     23961 23961 23965 23965 23991 23991 24005 24005 24013 24013 24027
     24027 24029 24030 24033 24034 24037 24041 24043 24043 24046 24047
     24049 24052 24055 24055 24061 24062 24065 24067 24069 24070 24072
     24072 24076 24076 24079 24081 24084 24086 24088 24093 24102 24103
     24109 24110 24113 24113 24119 24120 24123 24125 24130 24130 24132
     24133 24140 24140 24148 24149 24155 24155 24158 24158 24161 24162
     24178 24180 24182 24182 24184 24184 24186 24189 24191 24192 24196
     24196 24198 24199 24202 24203 24207 24209 24211 24215 24217 24218
     24220 24220 24222 24224 24229 24231 24237 24237 24243 24243 24245
     24249 24254 24254 24265 24266 24273 24275 24278 24278 24283 24283
     24296 24296 24298 24298 24308 24308 24310 24311 24314 24314 24318
     24324 24328 24328 24330 24331 24335 24335 24337 24337 24339 24339
     24341 24341 24343 24344 24347 24347 24351 24352 24357 24359 24361
     24362 24365 24365 24367 24367 24369 24369 24377 24378 24380 24380
     24384 24384 24400 24400 24402 24403 24405 24408 24413 24413 24417
     24418 24420 24420 24422 24422 24425 24426 24428 24429 24432 24433
     24435 24435 24439 24439 24441 24441 24443 24444 24448 24450 24452
     24453 24455 24460 24464 24464 24466 24466 24469 24469 24471 24473
     24476 24476 24481 24481 24488 24488 24490 24490 24493 24494 24501
     24501 24503 24503 24508 24509 24515 24518 24521 24521 24524 24525
     24527 24530 24534 24537 24541 24541 24544 24545 24548 24548 24551
     24551 24554 24555 24557 24558 24561 24561 24565 24565 24568 24568
     24571 24571 24573 24582 24586 24586 24589 24591 24594 24594 24596
     24598 24601 24601 24603 24605 24608 24609 24613 24619 24623 24623
     24629 24629 24635 24636 24639 24639 24641 24643 24651 24651 24653
     24653 24656 24656 24658 24658 24661 24661 24665 24666 24669 24669
     24674 24676 24679 24685 24687 24688 24691 24691 24694 24694 24696
     24701 24703 24703 24707 24708 24713 24713 24716 24717 24722 24722
     24724 24724 24726 24726 24730 24731 24733 24733 24735 24736 24739
     24739 24742 24742 24744 24744 24747 24749 24751 24751 24753 24754
     24756 24756 24760 24760 24763 24764 24773 24774 24778 24779 24785
     24785 24789 24789 24792 24792 24794 24794 24796 24797 24799 24800
     24806 24809 24811 24816 24819 24820 24822 24822 24825 24826 24832
     24833 24838 24838 24840 24841 24845 24847 24853 24853 24858 24858
     24863 24864 24867 24868 24870 24871 24875 24875 24895 24895 24904
     24904 24906 24906 24908 24908 24910 24910 24913 24913 24917 24917
     24925 24925 24930 24930 24935 24936 24944 24944 24949 24949 24951
     24951 24971 24971 24974 24974 24980 24980 24989 24989 24999 25001
     25004 25004 25015 25015 25022 25022 25026 25026 25032 25032 25034
     25035 25041 25042 25044 25044 25062 25062 25077 25077 25087 25087
     25094 25094 25096 25096 25098 25106 25109 25112 25114 25115 25119
     25119 25121 25122 25124 25125 25130 25130 25132 25132 25134 25134
     25139 25140 25143 25143 25149 25153 25155 25155 25159 25161 25163
     25166 25169 25172 25176 25176 25179 25179 25187 25187 25190 25191
     25193 25200 25203 25203 25206 25206 25209 25209 25212 25212 25214
     25216 25220 25220 25225 25226 25233 25235 25237 25240 25242 25243
     25247 25250 25252 25253 25256 25256 25259 25260 25265 25265 25269
     25269 25273 25273 25275 25277 25279 25279 25282 25282 25284 25290
     25292 25294 25296 25296 25298 25300 25302 25308 25311 25311 25314
     25315 25317 25321 25324 25327 25329 25329 25331 25332 25334 25335
     25340 25343 25345 25346 25351 25353 25358 25358 25361 25361 25366
     25366 25370 25371 25373 25381 25384 25384 25386 25387 25391 25391
     25394 25394 25401 25402 25405 25405 25410 25411 25413 25414 25417
     25417 25419 25424 25429 25429 25438 25439 25441 25443 25447 25447
     25449 25449 25453 25454 25457 25457 25462 25463 25466 25467 25472
     25472 25474 25474 25479 25482 25484 25484 25486 25488 25490 25490
     25494 25494 25496 25496 25504 25504 25506 25507 25509 25509 25511
     25514 25516 25518 25520 25520 25523 25524 25527 25528 25530 25530
     25532 25532 25534 25534 25540 25540 25542 25542 25545 25545 25549
     25552 25554 25554 25558 25558 25566 25566 25568 25569 25571 25571
     25577 25578 25581 25581 25586 25586 25588 25588 25590 25590 25592
     25592 25597 25597 25599 25602 25605 25605 25611 25612 25615 25616
     25619 25620 25627 25628 25630 25630 25632 25633 25638 25638 25642
     25642 25644 25645 25652 25652 25658 25658 25661 25661 25663 25663
     25665 25665 25668 25672 25674 25674 25682 25682 25684 25684 25688
     25688 25694 25694 25703 25703 25705 25705 25709 25709 25720 25722
     25730 25730 25732 25733 25735 25735 25745 25746 25749 25750 25753
     25753 25758 25758 25764 25764 25769 25769 25772 25774 25776 25776
     25781 25781 25783 25784 25786 25786 25788 25788 25792 25792 25794
     25794 25797 25797 25805 25806 25808 25808 25810 25810 25815 25816
     25822 25822 25826 25826 25828 25828 25830 25830 25856 25856 25865
     25865 25874 25874 25880 25880 25893 25893 25899 25899 25902 25903
     25908 25910 25912 25913 25915 25915 25918 25919 25925 25925 25928
     25929 25932 25932 25935 25935 25937 25937 25941 25942 25945 25945
     25947 25947 25949 25950 25954 25955 25958 25958 25963 25964 25968
     25968 25970 25970 25972 25972 25975 25975 25991 25991 25995 25996
     26000 26001 26003 26003 26007 26007 26009 26009 26011 26012 26015
     26015 26017 26017 26020 26021 26023 26023 26025 26025 26027 26027
     26029 26029 26031 26032 26041 26041 26044 26045 26049 26049 26051
     26054 26059 26060 26062 26063 26066 26066 26070 26071 26080 26080
     26082 26082 26085 26089 26092 26097 26102 26103 26106 26106 26112
     26112 26114 26115 26118 26118 26122 26122 26124 26124 26126 26127
     26131 26133 26137 26137 26141 26141 26143 26144 26149 26149 26151
     26152 26157 26157 26159 26159 26161 26161 26164 26166 26172 26172
     26174 26174 26177 26177 26179 26179 26187 26188 26191 26191 26194
     26199 26202 26202 26207 26207 26209 26209 26212 26212 26214 26214
     26216 26216 26222 26224 26228 26228 26230 26231 26234 26234 26238
     26238 26242 26242 26244 26244 26247 26247 26252 26252 26257 26257
     26262 26263 26269 26269 26279 26280 26286 26286 26292 26292 26297
     26297 26302 26302 26329 26329 26331 26333 26342 26342 26345 26345
     26352 26352 26354 26356 26359 26359 26361 26361 26364 26364 26366
     26368 26376 26379 26381 26381 26384 26384 26388 26389 26391 26391
     26395 26395 26397 26397 26399 26399 26406 26406 26408 26408 26410
     26413 26415 26415 26417 26417 26420 26421 26426 26426 26429 26429
     26432 26432 26434 26435 26438 26438 26440 26441 26444 26444 26446
     26449 26451 26451 26454 26454 26460 26460 26462 26465 26469 26469
     26472 26474 26477 26477 26479 26480 26482 26483 26485 26485 26487
     26487 26492 26492 26494 26495 26497 26497 26500 26500 26503 26503
     26505 26505 26507 26507 26512 26512 26517 26517 26519 26520 26522
     26522 26524 26526 26530 26531 26533 26533 26535 26536 26538 26539
     26541 26541 26543 26544 26547 26547 26549 26552 26561 26561 26563
     26564 26575 26580 26584 26586 26588 26590 26592 26592 26594 26594
     26597 26597 26601 26601 26604 26604 26607 26609 26611 26612 26621
     26621 26623 26624 26629 26629 26631 26636 26638 26639 26641 26641
     26643 26643 26646 26647 26653 26653 26657 26657 26665 26666 26674
     26675 26679 26681 26684 26686 26688 26694 26696 26698 26700 26700
     26702 26702 26704 26705 26707 26709 26720 26729 26731 26731 26740
     26740 26742 26743 26753 26753 26755 26755 26757 26758 26767 26767
     26771 26771 26775 26775 26786 26786 26790 26792 26797 26797 26799
     26800 26803 26803 26805 26805 26816 26816 26818 26818 26825 26825
     26827 26827 26829 26829 26834 26834 26837 26837 26840 26840 26842
     26842 26848 26848 26851 26851 26862 26862 26864 26865 26869 26869
     26873 26874 26876 26876 26881 26881 26885 26885 26891 26891 26893
     26894 26896 26896 26898 26898 26911 26912 26916 26916 26925 26925
     26928 26928 26932 26932 26937 26937 26941 26941 26943 26943 26946
     26946 26964 26964 26967 26967 26970 26970 26973 26974 26976 26976
     26979 26979 26982 26982 26987 26987 26990 26990 26993 26993 26999
     27001 27004 27004 27008 27008 27010 27010 27012 27012 27014 27017
     27021 27021 27028 27029 27032 27032 27035 27036 27047 27048 27051
     27051 27053 27053 27057 27057 27060 27060 27063 27063 27067 27067
     27073 27073 27082 27082 27084 27084 27086 27086 27088 27088 27092
     27092 27099 27099 27103 27104 27117 27117 27122 27122 27133 27133
     27135 27135 27146 27146 27159 27160 27167 27167 27169 27169 27176
     27176 27178 27178 27183 27183 27185 27185 27189 27189 27197 27198
     27204 27204 27207 27207 27216 27216 27224 27225 27227 27227 27233
     27233 27237 27237 27249 27249 27257 27257 27260 27260 27264 27264
     27268 27268 27278 27278 27280 27281 27287 27287 27296 27296 27305
     27305 27307 27308 27424 27428 27431 27431 27442 27442 27447 27447
     27449 27450 27454 27454 27459 27459 27462 27463 27465 27465 27468
     27468 27481 27481 27490 27495 27498 27498 27513 27513 27515 27516
     27521 27524 27526 27527 27529 27531 27533 27533 27538 27539 27542
     27542 27546 27547 27553 27553 27562 27562 27571 27573 27575 27575
     27583 27583 27585 27586 27589 27589 27595 27595 27597 27597 27599
     27599 27602 27607 27609 27609 27611 27611 27617 27617 27626 27627
     27631 27631 27635 27635 27637 27637 27641 27641 27645 27645 27653
     27655 27661 27661 27663 27665 27667 27670 27672 27675 27679 27679
     27681 27682 27684 27684 27686 27690 27694 27696 27698 27698 27700
     27701 27704 27704 27709 27709 27712 27714 27718 27719 27721 27722
     27728 27728 27732 27733 27735 27735 27739 27745 27748 27748 27752
     27754 27760 27760 27762 27762 27764 27764 27766 27766 27769 27769
     27773 27774 27777 27779 27781 27782 27784 27785 27788 27788 27791
     27792 27795 27796 27801 27801 27803 27803 27807 27807 27809 27809
     27811 27815 27817 27819 27821 27822 27825 27827 27832 27833 27835
     27839 27844 27845 27849 27850 27852 27852 27856 27856 27859 27863
     27867 27867 27870 27870 27872 27875 27877 27877 27880 27880 27882
     27883 27886 27889 27891 27891 27893 27896 27898 27902 27905 27905
     27908 27908 27911 27911 27915 27916 27918 27918 27922 27922 27927
     27927 27929 27931 27934 27934 27941 27941 27943 27943 27946 27947
     27950 27950 27953 27955 27957 27957 27961 27961 27963 27966 27969
     27969 27971 27971 27973 27976 27978 27979 27981 27983 27985 27988
     27993 27994 27996 27996 27998 27998 28000 28000 28003 28003 28006
     28006 28009 28010 28014 28015 28020 28020 28023 28024 28028 28028
     28034 28034 28037 28037 28040 28041 28044 28044 28046 28046 28049
     28049 28051 28053 28059 28059 28061 28065 28067 28068 28070 28075
     28078 28079 28082 28082 28085 28085 28088 28088 28095 28096 28100
     28103 28107 28108 28113 28113 28118 28118 28120 28121 28125 28126
     28128 28129 28132 28132 28134 28134 28139 28140 28142 28142 28145
     28145 28147 28147 28151 28151 28153 28153 28155 28156 28165 28165
     28170 28170 28172 28174 28176 28177 28180 28180 28182 28183 28186
     28186 28189 28189 28192 28193 28195 28197 28201 28201 28203 28203
     28205 28205 28207 28207 28210 28210 28212 28212 28216 28216 28218
     28218 28227 28228 28237 28238 28243 28244 28246 28246 28248 28248
     28251 28251 28255 28255 28267 28267 28270 28270 28286 28287 28291
     28291 28293 28294 28297 28297 28303 28304 28312 28312 28316 28316
     28319 28319 28322 28322 28325 28325 28327 28327 28330 28330 28335
     28335 28337 28338 28340 28340 28342 28343 28346 28347 28349 28349
     28353 28354 28359 28359 28363 28363 28367 28367 28369 28369 28371
     28373 28375 28375 28378 28378 28382 28386 28388 28390 28392 28393
     28404 28404 28409 28409 28418 28418 28422 28422 28425 28425 28431
     28431 28435 28437 28448 28448 28452 28452 28457 28459 28461 28461
     28463 28463 28465 28465 28467 28467 28470 28470 28478 28478 28486
     28487 28491 28491 28493 28493 28504 28504 28508 28508 28510 28510
     28514 28514 28518 28518 28525 28526 28530 28530 28532 28532 28536
     28536 28538 28538 28540 28540 28548 28548 28552 28553 28556 28558
     28572 28572 28577 28577 28583 28583 28595 28595 28598 28598 28601
     28601 28608 28608 28610 28610 28617 28617 28625 28626 28638 28638
     28640 28641 28654 28655 28689 28689 28698 28699 28707 28707 28725
     28725 28729 28729 28748 28748 28751 28751 28766 28766 28779 28781
     28783 28784 28789 28790 28792 28792 28796 28796 28798 28800 28805
     28805 28809 28810 28814 28814 28818 28818 28820 28822 28825 28825
     28828 28829 28843 28847 28849 28849 28851 28851 28855 28857 28859
     28861 28864 28867 28872 28872 28874 28874 28888 28889 28891 28891
     28895 28895 28900 28900 28902 28905 28907 28909 28911 28911 28919
     28919 28921 28921 28925 28925 28937 28938 28944 28944 28947 28947
     28949 28950 28952 28954 28966 28966 28975 28977 28982 28982 28997
     28997 29002 29002 29004 29004 29006 29006 29020 29020 29022 29022
     29028 29028 29030 29032 29038 29038 29042 29043 29048 29048 29050
     29050 29053 29053 29060 29060 29066 29066 29071 29071 29076 29076
     29080 29081 29087 29088 29096 29096 29100 29100 29107 29107 29109
     29109 29113 29113 29123 29123 29134 29134 29140 29141 29152 29152
     29157 29157 29159 29159 29166 29166 29177 29177 29190 29190 29213
     29213 29224 29224 29226 29226 29228 29228 29232 29233 29237 29241
     29243 29243 29245 29245 29247 29247 29255 29256 29260 29261 29266
     29266 29270 29270 29273 29273 29275 29275 29277 29277 29279 29279
     29281 29282 29286 29287 29289 29289 29294 29295 29298 29298 29301
     29301 29305 29306 29310 29313 29316 29316 29322 29323 29325 29325
     29327 29327 29330 29330 29343 29343 29356 29357 29359 29360 29364
     29364 29366 29369 29377 29380 29384 29384 29389 29390 29392 29392
     29394 29394 29399 29399 29401 29401 29406 29406 29408 29409 29416
     29417 29420 29428 29431 29432 29434 29436 29441 29441 29443 29443
     29450 29450 29454 29454 29459 29459 29461 29463 29467 29470 29473
     29474 29477 29477 29481 29484 29486 29486 29489 29489 29492 29492
     29495 29497 29502 29503 29517 29517 29520 29520 29522 29522 29527
     29527 29536 29536 29548 29549 29551 29551 29566 29566 29572 29572
     29575 29575 29577 29577 29579 29579 29582 29582 29585 29585 29590
     29590 29595 29595 29599 29599 29602 29602 29609 29609 29611 29611
     29614 29616 29618 29619 29623 29623 29626 29627 29632 29632 29634
     29634 29640 29642 29645 29645 29647 29649 29657 29657 29662 29662
     29664 29664 29669 29669 29671 29671 29673 29673 29677 29677 29682
     29682 29699 29699 29701 29702 29705 29706 29711 29712 29722 29723
     29730 29730 29733 29734 29736 29736 29738 29738 29740 29740 29742
     29742 29744 29744 29747 29750 29756 29756 29761 29761 29781 29781
     29783 29783 29785 29788 29790 29791 29805 29805 29808 29808 29814
     29815 29822 29822 29824 29825 29827 29827 29831 29831 29835 29835
     29838 29838 29840 29840 29852 29852 29854 29854 29863 29865 29882
     29882 29906 29906 29916 29916 29918 29918 29920 29920 29922 29924
     29926 29926 29934 29935 29940 29940 29942 29943 29951 29951 29956
     29956 29965 29965 29967 29967 29969 29969 29971 29971 29976 29978
     29980 29980 29983 29983 29989 29989 29992 29993 29995 29997 29999
     30003 30005 30005 30007 30008 30010 30011 30014 30014 30016 30016
     30021 30021 30024 30024 30027 30028 30030 30031 30036 30036 30041
     30044 30053 30054 30058 30058 30066 30066 30068 30068 30072 30073
     30079 30079 30083 30083 30086 30086 30091 30091 30095 30095 30097
     30098 30100 30100 30102 30103 30105 30106 30109 30109 30111 30113
     30115 30117 30123 30124 30126 30133 30136 30137 30140 30142 30146
     30149 30151 30154 30157 30157 30162 30162 30164 30166 30168 30168
     30171 30171 30174 30174 30178 30180 30182 30184 30186 30187 30192
     30193 30196 30196 30201 30201 30204 30204 30207 30209 30211 30211
     30213 30213 30218 30218 30220 30220 30224 30224 30229 30229 30231
     30233 30235 30235 30239 30240 30242 30242 30244 30246 30249 30251
     30253 30253 30256 30256 30259 30261 30264 30264 30268 30268 30270
     30272 30275 30275 30284 30285 30292 30292 30294 30294 30300 30300
     30302 30302 30307 30307 30315 30315 30319 30319 30328 30328 30331
     30331 30333 30334 30338 30338 30340 30340 30342 30344 30347 30347
     30350 30350 30353 30353 30355 30355 30358 30358 30361 30361 30372
     30372 30382 30382 30385 30386 30388 30388 30399 30399 30402 30402
     30405 30406 30408 30408 30410 30410 30413 30418 30420 30420 30422
     30424 30427 30427 30431 30431 30437 30437 30446 30447 30449 30450
     30452 30452 30456 30457 30460 30460 30462 30462 30465 30465 30468
     30468 30471 30473 30475 30475 30477 30477 30489 30490 30495 30496
     30498 30498 30502 30502 30504 30505 30509 30509 30511 30511 30517
     30520 30522 30522 30524 30524 30528 30529 30531 30531 30535 30535
     30544 30545 30554 30555 30561 30563 30565 30566 30568 30568 30571
     30572 30585 30585 30589 30592 30596 30597 30604 30606 30609 30610
     30623 30624 30626 30626 30629 30629 30631 30631 30633 30634 30636
     30636 30640 30640 30643 30643 30645 30645 30651 30651 30653 30653
     30655 30655 30669 30669 30679 30679 30683 30684 30690 30691 30693
     30693 30695 30695 30697 30697 30699 30702 30707 30707 30710 30710
     30712 30712 30717 30722 30729 30729 30732 30733 30737 30738 30740
     30740 30742 30744 30746 30746 30748 30749 30751 30751 30755 30755
     30757 30759 30761 30761 30764 30765 30768 30768 30772 30772 30775
     30780 30782 30782 30784 30784 30789 30789 30791 30791 30796 30796
     30798 30798 30800 30800 30802 30802 30805 30807 30813 30813 30826
     30830 30839 30839 30844 30844 30855 30855 30857 30857 30860 30862
     30865 30865 30867 30867 30871 30872 30874 30876 30879 30879 30881
     30881 30883 30883 30885 30885 30887 30887 30896 30900 30905 30905
     30910 30910 30913 30913 30917 30917 30921 30923 30928 30928 30932
     30933 30937 30937 30952 30952 30956 30956 30962 30962 30964 30964
     30967 30967 30970 30970 30977 30977 30981 30981 30995 30995 31006
     31006 31012 31012 31028 31028 31034 31036 31038 31038 31040 31041
     31046 31046 31048 31049 31059 31059 31062 31063 31066 31072 31074
     31074 31077 31077 31079 31080 31085 31085 31087 31087 31095 31096
     31098 31098 31104 31105 31108 31109 31114 31114 31119 31119 31130
     31130 31143 31143 31155 31155 31161 31163 31165 31166 31168 31169
     31171 31171 31174 31174 31177 31177 31179 31179 31181 31181 31185
     31186 31189 31189 31192 31192 31199 31199 31203 31204 31206 31207
     31209 31209 31211 31211 31213 31213 31215 31216 31224 31224 31227
     31227 31229 31229 31232 31232 31234 31235 31238 31238 31243 31243
     31245 31246 31252 31252 31255 31255 31258 31258 31262 31262 31264
     31264 31267 31267 31283 31283 31287 31287 31289 31289 31291 31293
     31295 31295 31302 31302 31313 31313 31319 31319 31344 31344 31348
     31348 31350 31354 31359 31361 31363 31364 31366 31366 31368 31368
     31373 31373 31377 31378 31381 31384 31388 31389 31391 31392 31397
     31398 31400 31400 31404 31405 31411 31411 31423 31423 31435 31435
     31446 31446 31449 31449 31454 31456 31459 31459 31461 31462 31469
     31469 31471 31471 31481 31482 31485 31485 31487 31487 31491 31492
     31494 31494 31496 31496 31498 31499 31503 31503 31505 31505 31508
     31509 31513 31513 31515 31515 31518 31518 31520 31520 31524 31526
     31528 31528 31530 31532 31534 31534 31537 31537 31539 31539 31544
     31544 31546 31546 31548 31548 31550 31550 31557 31557 31559 31559
     31561 31561 31563 31564 31567 31570 31572 31572 31574 31574 31576
     31576 31578 31579 31581 31581 31584 31584 31586 31586 31598 31598
     31601 31602 31605 31605 31607 31607 31609 31609 31611 31611 31614
     31614 31616 31616 31621 31621 31629 31629 31632 31632 31636 31637
     31639 31639 31644 31645 31649 31650 31654 31661 31665 31665 31668
     31668 31672 31672 31681 31681 31686 31687 31692 31692 31697 31697
     31699 31699 31705 31706 31709 31709 31713 31713 31717 31718 31722
     31722 31726 31726 31729 31729 31735 31735 31740 31740 31742 31742
     31751 31751 31755 31756 31759 31759 31766 31766 31775 31775 31782
     31783 31786 31786 31800 31800 31807 31809 31821 31821 31859 31860
     31867 31869 31881 31881 31889 31890 31893 31893 31895 31896 31900
     31903 31906 31906 31908 31909 31914 31914 31918 31918 31921 31923
     31929 31929 31932 31934 31937 31937 31941 31941 31943 31944 31946
     31946 31948 31949 31957 31959 31961 31961 31964 31964 31967 31968
     31976 31976 31983 31983 31992 31992 31995 31995 32010 32010 32032
     32032 32034 32034 32039 32039 32043 32043 32047 32047 32110 32110
     32119 32119 32166 32166 32174 32174 32315 32315 32321 32321 32327
     32327 32386 32386 32411 32411 32415 32429 32431 32435 32437 32442
     32445 32469 32471 32483 32485 32491 32493 32504 32506 32521 32523
     32527 32529 32541 32543 32566 32568 32568 32570 32570 32578 32578
     32580 32581 32592 32593 32596 32597 32599 32600 32602 32602 32607
     32607 32609 32610 32616 32618 32622 32622 32625 32626 32628 32628
     32633 32633 32638 32638 32641 32641 32650 32650 32652 32652 32654
     32654 32660 32660 32666 32666 32669 32671 32673 32673 32676 32676
     32679 32679 32687 32688 32690 32690 32696 32697 32700 32701 32703
     32703 32705 32705 32709 32709 32714 32714 32716 32716 32718 32718
     32724 32725 32728 32728 32735 32737 32741 32742 32745 32745 32750
     32750 32752 32753 32755 32755 32763 32764 32768 32769 32771 32774
     32779 32781 32784 32784 32786 32786 32788 32793 32796 32796 32800
     32800 32802 32802 32805 32810 32817 32817 32819 32819 32821 32824
     32827 32827 32829 32829 32831 32831 32834 32835 32838 32838 32842
     32845 32850 32850 32852 32852 32856 32856 32858 32858 32873 32874
     32881 32881 32895 32896 32899 32900 32902 32903 32905 32905 32907
     32908 32915 32915 32918 32918 32920 32920 32922 32925 32927 32930
     32932 32933 32937 32939 32941 32943 32945 32946 32948 32948 32951
     32951 32954 32954 32956 32964 32966 32966 32972 32974 32982 32983
     32985 32990 32993 32993 32996 32997 32999 33005 33007 33012 33014
     33014 33016 33016 33018 33018 33020 33021 33026 33026 33030 33030
     33033 33034 33037 33044 33046 33046 33048 33048 33050 33050 33054
     33054 33068 33068 33071 33071 33073 33074 33078 33078 33080 33080
     33086 33086 33094 33094 33096 33096 33098 33100 33104 33105 33107
     33109 33113 33114 33120 33120 33125 33125 33127 33127 33129 33129
     33133 33134 33136 33137 33140 33140 33145 33152 33154 33154 33160
     33160 33162 33162 33167 33167 33169 33169 33176 33176 33179 33181
     33187 33187 33190 33190 33192 33192 33194 33194 33203 33203 33210
     33211 33216 33219 33222 33222 33226 33226 33228 33228 33251 33251
     33255 33255 33258 33258 33260 33261 33267 33268 33275 33276 33278
     33278 33280 33282 33284 33286 33292 33293 33296 33296 33298 33298
     33300 33300 33307 33308 33310 33311 33313 33315 33320 33320 33322
     33325 33327 33329 33331 33337 33339 33339 33342 33342 33348 33348
     33351 33351 33353 33353 33355 33355 33359 33359 33368 33368 33370
     33370 33375 33375 33384 33384 33390 33392 33394 33396 33401 33402
     33405 33407 33410 33410 33412 33412 33416 33416 33418 33419 33421
     33423 33425 33426 33431 33433 33436 33437 33439 33439 33441 33441
     33444 33446 33448 33457 33459 33460 33463 33465 33469 33470 33473
     33473 33476 33476 33479 33480 33482 33487 33489 33493 33495 33496
     33499 33500 33502 33505 33507 33510 33515 33515 33519 33519 33521
     33521 33524 33524 33527 33527 33529 33529 33531 33531 33537 33545
     33548 33548 33550 33551 33553 33553 33556 33557 33559 33559 33562
     33564 33575 33576 33579 33581 33583 33583 33585 33585 33587 33590
     33592 33594 33596 33596 33600 33600 33603 33603 33606 33607 33609
     33609 33615 33618 33620 33620 33626 33628 33630 33633 33635 33647
     33655 33656 33659 33661 33669 33670 33673 33673 33678 33678 33682
     33683 33688 33688 33691 33692 33694 33694 33696 33696 33704 33707
     33712 33716 33718 33722 33724 33725 33728 33729 33733 33733 33735
     33735 33738 33738 33740 33740 33743 33743 33748 33748 33750 33750
     33752 33752 33756 33757 33759 33761 33765 33765 33769 33770 33776
     33778 33784 33785 33789 33789 33793 33793 33795 33796 33798 33798
     33803 33807 33809 33809 33816 33816 33820 33821 33828 33832 33841
     33841 33848 33848 33852 33853 33862 33862 33873 33873 33879 33879
     33881 33884 33889 33889 33891 33891 33897 33897 33899 33901 33905
     33905 33907 33907 33909 33910 33912 33912 33914 33914 33922 33922
     33927 33929 33931 33932 33934 33934 33943 33943 33945 33945 33948
     33948 33953 33953 33967 33967 33970 33970 33972 33972 33976 33978
     33981 33981 33983 33983 33985 33985 33988 33988 33993 33994 33997
     33997 34000 34001 34003 34003 34006 34006 34013 34013 34015 34016
     34019 34019 34021 34022 34028 34028 34032 34032 34044 34044 34047
     34047 34060 34060 34065 34065 34067 34067 34071 34071 34074 34074
     34079 34079 34081 34081 34091 34092 34103 34109 34115 34115 34120
     34122 34134 34134 34137 34137 34142 34142 34148 34148 34152 34152
     34162 34162 34164 34164 34169 34171 34174 34174 34180 34181 34183
     34183 34191 34191 34203 34204 34212 34212 34216 34216 34218 34218
     34222 34224 34231 34231 34233 34233 34241 34241 34249 34249 34255
     34256 34259 34259 34261 34261 34268 34268 34276 34276 34281 34281
     34299 34299 34303 34303 34309 34309 34321 34321 34326 34326 34343
     34343 34345 34345 34360 34360 34364 34364 34381 34385 34388 34388
     34394 34394 34398 34398 34402 34402 34411 34412 34414 34414 34417
     34417 34425 34434 34442 34445 34451 34451 34453 34453 34460 34461
     34467 34468 34471 34474 34476 34476 34479 34481 34484 34486 34490
     34490 34496 34496 34500 34500 34502 34503 34505 34507 34510 34513
     34516 34516 34520 34521 34523 34523 34526 34527 34532 34532 34537
     34537 34541 34542 34544 34548 34552 34553 34558 34558 34560 34560
     34562 34563 34567 34570 34573 34573 34578 34579 34581 34581 34583
     34584 34586 34586 34588 34588 34590 34590 34593 34595 34597 34597
     34601 34601 34606 34606 34609 34609 34612 34612 34615 34615 34619
     34619 34622 34623 34631 34633 34636 34636 34638 34638 34643 34643
     34647 34647 34649 34649 34656 34656 34659 34661 34670 34670 34672
     34672 34676 34676 34678 34678 34683 34686 34690 34691 34693 34693
     34696 34696 34699 34699 34701 34701 34707 34707 34711 34711 34719
     34719 34728 34728 34731 34733 34735 34735 34739 34739 34741 34741
     34746 34746 34749 34749 34752 34752 34758 34758 34762 34763 34769
     34771 34779 34779 34784 34784 34789 34789 34794 34794 34798 34798
     34809 34809 34814 34814 34819 34819 34826 34826 34835 34835 34837
     34838 34843 34843 34849 34850 34866 34866 34873 34873 34876 34876
     34880 34880 34884 34885 34892 34893 34900 34900 34903 34903 34905
     34905 34913 34917 34920 34921 34923 34924 34926 34926 34928 34928
     34930 34930 34935 34935 34941 34943 34945 34946 34948 34949 34952
     34952 34955 34955 34957 34957 34962 34962 34966 34966 34972 34972
     34978 34978 34980 34980 34987 34987 34989 34989 34993 34993 34999
     34999 35004 35004 35009 35010 35013 35014 35017 35017 35022 35022
     35026 35026 35028 35029 35032 35033 35039 35039 35042 35045 35048
     35048 35056 35057 35059 35060 35064 35065 35068 35068 35070 35070
     35074 35074 35082 35082 35088 35088 35090 35091 35097 35099 35105
     35105 35109 35109 35114 35115 35120 35120 35124 35124 35126 35126
     35137 35137 35140 35140 35166 35167 35174 35174 35195 35195 35199
     35199 35201 35201 35203 35203 35206 35206 35265 35266 35268 35276
     35278 35282 35286 35286 35290 35290 35292 35292 35294 35294 35299
     35299 35301 35302 35307 35307 35311 35311 35315 35315 35328 35328
     35335 35335 35390 35390 35400 35400 35449 35449 35465 35466 35475
     35475 35591 35591 35622 35622 35686 35686 35692 35692 35744 35755
     35757 35760 35762 35770 35772 35782 35784 35791 35793 35817 35819
     35848 35850 35869 35871 35895 35905 35905 35910 35911 35913 35913
     35916 35916 35925 35925 35930 35930 35937 35938 35946 35947 35955
     35955 35960 35962 35970 35970 35973 35973 35977 35978 35980 35980
     35988 35988 35992 35992 36125 36127 36129 36176 36179 36182 36184
     36190 36192 36196 36198 36199 36203 36203 36205 36205 36208 36208
     36211 36215 36225 36225 36228 36229 36234 36235 36241 36241 36244
     36244 36255 36255 36259 36259 36273 36273 36275 36277 36280 36280
     36282 36282 36284 36284 36286 36287 36291 36292 36294 36294 36299
     36300 36302 36303 36305 36305 36310 36311 36314 36315 36317 36319
     36323 36324 36328 36328 36330 36332 36335 36335 36339 36339 36341
     36341 36343 36347 36349 36349 36357 36357 36361 36362 36364 36364
     36367 36367 36372 36372 36381 36383 36386 36387 36393 36394 36396
     36396 36398 36399 36401 36401 36405 36405 36409 36410 36413 36413
     36416 36418 36420 36420 36423 36427 36433 36434 36441 36441 36454
     36454 36457 36457 36460 36461 36463 36464 36466 36466 36468 36468
     36470 36470 36476 36476 36479 36479 36481 36481 36485 36485 36487
     36487 36495 36496 36500 36500 36508 36508 36510 36510 36523 36524
     36527 36527 36530 36530 36538 36538 36558 36558 36710 36713 36715
     36735 36737 36747 36749 36753 36755 36764 36766 36767 36771 36771
     36776 36777 36779 36779 36784 36785 36790 36790 36793 36793 36797
     36798 36801 36802 36804 36805 36807 36808 36814 36814 36816 36817
     36819 36821 36824 36825 36827 36831 36834 36834 36836 36838 36840
     36843 36845 36846 36848 36848 36851 36851 36855 36857 36861 36861
     36864 36870 36873 36875 36877 36877 36879 36882 36884 36884 36886
     36887 36890 36891 36893 36898 36902 36902 36909 36911 36917 36918
     36920 36920 36923 36924 36926 36926 36929 36930 36932 36932 36935
     36935 36941 36941 36943 36947 36951 36952 36955 36955 36962 36963
     36965 36965 36968 36968 36973 36974 36980 36981 36989 36989 36991
     36992 36994 36995 37000 37000 37003 37003 37009 37009 37011 37011
     37013 37013 37015 37015 37017 37017 37019 37019 37021 37021 37025
     37027 37030 37030 37034 37034 37036 37036 37038 37041 37043 37046
     37048 37051 37054 37054 37057 37057 37060 37061 37063 37063 37066
     37066 37070 37073 37075 37075 37079 37079 37083 37085 37089 37090
     37094 37096 37099 37099 37101 37101 37103 37103 37108 37108 37112
     37112 37117 37118 37122 37122 37124 37124 37145 37145 37150 37150
     37154 37155 37167 37167 37169 37169 37177 37177 37187 37187 37190
     37190 37193 37200 37202 37202 37207 37207 37210 37210 37213 37214
     37217 37221 37225 37226 37228 37228 37230 37234 37236 37241 37245
     37247 37253 37253 37255 37255 37257 37257 37259 37261 37264 37266
     37274 37275 37282 37283 37290 37290 37293 37295 37300 37301 37306
     37306 37319 37319 37321 37322 37324 37327 37329 37329 37340 37340
     37492 37492 37518 37518 37550 37550 37576 37576 37694 37694 37738
     37738 37775 37775 37834 37834 37846 37846 37950 37950 37995 37995
     38021 38032 38034 38037 38039 38039 38041 38086 38088 38094 38096
     38098 38101 38105 38107 38117 38119 38138 38140 38171 38173 38175
     38177 38182 38184 38194 38196 38204 38206 38210 38212 38215 38217
     38218 38220 38228 38230 38233 38235 38239 38241 38253 38255 38259
     38262 38262 38271 38271 38376 38379 38381 38398 38400 38406 38408
     38418 38420 38423 38425 38426 38428 38429 38431 38431 38433 38434
     38442 38442 38446 38446 38449 38454 38459 38461 38463 38464 38466
     38466 38468 38473 38475 38477 38480 38480 38484 38485 38491 38491
     38495 38495 38497 38498 38500 38500 38503 38506 38508 38508 38514
     38514 38516 38519 38533 38534 38536 38536 38539 38539 38541 38541
     38543 38544 38548 38548 38551 38553 38556 38556 38567 38567 38576
     38576 38579 38579 38582 38582 38585 38585 38588 38590 38592 38593
     38596 38599 38601 38601 38604 38607 38610 38610 38613 38613 38624
     38624 38632 38634 38639 38639 38643 38643 38646 38647 38649 38649
     38654 38654 38656 38657 38660 38660 38662 38665 38669 38671 38675
     38675 38678 38678 38684 38684 38686 38686 38698 38698 38701 38701
     38704 38704 38706 38706 38712 38713 38718 38718 38738 38739 38742
     38742 38745 38745 38747 38747 38750 38750 38752 38754 38757 38757
     38761 38761 38771 38772 38774 38774 38780 38780 38789 38789 38795
     38795 38797 38797 38801 38802 38804 38804 38808 38808 38816 38816
     38819 38819 38827 38827 38829 38829 38831 38831 38834 38834 38836
     38836 38886 38887 38889 38893 38899 38899 38901 38902 39029 39050
     39052 39053 39055 39057 39059 39060 39062 39064 39066 39074 39076
     39079 39118 39118 39121 39123 39125 39125 39128 39130 39134 39135
     39143 39144 39181 39181 39184 39184 39214 39214 39252 39253 39267
     39267 39269 39269 39271 39282 39284 39287 39290 39290 39292 39293
     39295 39297 39300 39304 39306 39307 39309 39309 39311 39321 39333
     39333 39336 39336 39532 39537 39539 39554 39556 39560 39562 39564
     39567 39571 39574 39576 39578 39589 39591 39592 39600 39601 39606
     39608 39610 39610 39612 39612 39616 39618 39621 39621 39627 39628
     39633 39633 39635 39635 39640 39640 39647 39647 39649 39649 39654
     39654 39659 39659 39661 39661 39663 39663 39673 39673 39675 39675
     39683 39683 39688 39688 39695 39695 39699 39699 39711 39711 39715
     39715 39727 39727 39730 39730 39739 39740 39745 39749 39751 39753
     39757 39757 39759 39759 39761 39761 39764 39764 40060 40060 40063
     40063 40065 40066 40069 40072 40075 40075 40077 40078 40080 40082
     40084 40085 40090 40092 40094 40105 40107 40107 40109 40110 40112
     40120 40122 40125 40131 40135 40138 40144 40147 40153 40156 40159
     40162 40162 40479 40483 40485 40486 40488 40493 40495 40495 40497
     40499 40501 40506 40509 40511 40513 40524 40526 40527 40529 40529
     40533 40533 40535 40536 40538 40540 40542 40542 40547 40548 40550
     40557 40560 40561 40563 40563 40574 40575 40578 40578 40583 40584
     40587 40587 40594 40595 40605 40605 40607 40607 40614 40614 40628
     40628 40632 40632 40635 40635 40637 40638 40644 40644 40649 40649
     40653 40655 40657 40657 40660 40660 40664 40664 40667 40669 40671
     40672 40674 40674 40677 40677 40679 40679 40681 40682 40687 40687
     40697 40697 40699 40700 40702 40702 40715 40715 40717 40718 40720
     40720 40723 40723 40727 40727 40729 40729 40736 40736 40738 40738
     40748 40748 40751 40751 40759 40759 40761 40761 40763 40763 40765
     40766 40772 40772 40784 40785 40831 40832 40835 40844 40857 40859
     40863 40864 65281 65374 65504 65505 65507 65507 65509 65509 917504
     917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 17 :NAME #16="Shift_JIS" :ALIASES
   '("csShiftJIS" "MS_Kanji") :MIME-ENCODING '#16# :SOURCE
   '"This charset is an extension of csHalfWidthKatakana by adding graphic characters in JIS X 0208.  The CCS's are JIS X0201:1997 and JIS X0208:1997.  The complete definition is shown in Appendix 1 of JIS X0208:1997. This charset can be used for the top-level media type \"text\"."
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 162 163 165 165 167 168 172 172 176 177 180 180 182 182 215
     215 247 247 913 929 931 937 945 961 963 969 1025 1025 1040 1103
     1105 1105 8208 8208 8212 8214 8216 8217 8220 8221 8224 8225 8229
     8230 8240 8240 8242 8243 8251 8251 8254 8254 8451 8451 8470 8470
     8481 8481 8491 8491 8544 8553 8560 8569 8592 8595 8658 8658 8660
     8660 8704 8704 8706 8707 8711 8712 8715 8715 8721 8722 8730 8730
     8733 8736 8741 8741 8743 8748 8750 8750 8756 8757 8765 8765 8786
     8786 8800 8801 8806 8807 8810 8811 8834 8835 8838 8839 8869 8869
     8895 8895 8978 8978 9312 9331 9472 9475 9484 9484 9487 9488 9491
     9492 9495 9496 9499 9501 9504 9504 9507 9509 9512 9512 9515 9516
     9519 9520 9523 9524 9527 9528 9531 9532 9535 9535 9538 9538 9547
     9547 9632 9633 9650 9651 9660 9661 9670 9671 9675 9675 9678 9679
     9711 9711 9733 9734 9792 9792 9794 9794 9834 9834 9837 9837 9839
     9839 12288 12291 12293 12309 12316 12317 12319 12319 12353 12435
     12443 12446 12449 12534 12539 12542 12849 12850 12857 12857 12964
     12968 13059 13059 13069 13069 13076 13076 13080 13080 13090 13091
     13094 13095 13099 13099 13110 13110 13115 13115 13129 13130 13133
     13133 13137 13137 13143 13143 13179 13182 13198 13199 13212 13214
     13217 13217 13252 13252 13261 13261 19968 19969 19971 19971 19975
     19979 19981 19982 19984 19985 19988 19993 19998 19998 20001 20001
     20006 20006 20008 20008 20010 20010 20013 20013 20017 20018 20022
     20022 20024 20025 20027 20028 20031 20031 20034 20035 20037 20037
     20043 20043 20045 20047 20053 20057 20061 20063 20066 20066 20081
     20081 20083 20083 20094 20094 20096 20096 20098 20098 20101 20102
     20104 20108 20110 20110 20113 20114 20116 20117 20120 20121 20123
     20124 20126 20130 20132 20134 20136 20136 20139 20142 20144 20144
     20147 20147 20150 20150 20154 20154 20160 20162 20164 20164 20166
     20167 20170 20171 20173 20175 20180 20185 20189 20191 20193 20193
     20195 20197 20205 20206 20208 20208 20210 20210 20214 20215 20219
     20220 20224 20225 20227 20227 20233 20234 20237 20241 20250 20250
     20252 20253 20271 20272 20276 20276 20278 20278 20280 20282 20284
     20285 20291 20291 20294 20295 20301 20305 20307 20307 20309 20311
     20313 20318 20329 20329 20335 20336 20339 20339 20341 20342 20347
     20348 20351 20351 20355 20355 20358 20358 20360 20360 20362 20363
     20365 20365 20367 20367 20369 20370 20372 20372 20374 20374 20376
     20376 20378 20379 20381 20381 20384 20385 20395 20395 20397 20399
     20405 20406 20415 20415 20418 20420 20425 20426 20429 20430 20432
     20433 20436 20436 20439 20440 20442 20443 20445 20445 20447 20447
     20449 20449 20451 20453 20462 20463 20467 20467 20469 20470 20472
     20472 20474 20474 20478 20479 20485 20486 20489 20489 20491 20491
     20493 20493 20495 20495 20497 20498 20500 20500 20502 20502 20505
     20506 20510 20511 20513 20518 20520 20525 20534 20534 20537 20537
     20544 20544 20546 20547 20550 20553 20559 20560 20565 20566 20570
     20570 20572 20572 20581 20581 20588 20588 20592 20592 20594 20594
     20596 20598 20600 20600 20605 20605 20608 20608 20613 20613 20621
     20621 20625 20625 20628 20628 20632 20634 20652 20653 20658 20661
     20663 20663 20670 20670 20674 20674 20677 20677 20681 20682 20685
     20685 20687 20687 20689 20689 20693 20694 20696 20696 20698 20698
     20702 20702 20707 20707 20709 20709 20711 20711 20717 20718 20724
     20725 20729 20729 20731 20731 20736 20738 20740 20740 20745 20745
     20754 20754 20756 20758 20760 20760 20762 20762 20767 20767 20769
     20769 20778 20778 20786 20786 20791 20791 20794 20796 20799 20801
     20803 20814 20816 20816 20818 20818 20820 20820 20826 20826 20828
     20828 20834 20834 20836 20837 20840 20846 20849 20849 20853 20856
     20860 20860 20864 20864 20866 20866 20869 20870 20873 20874 20876
     20877 20879 20883 20885 20887 20889 20889 20893 20893 20896 20896
     20898 20898 20900 20902 20904 20908 20912 20919 20925 20926 20932
     20934 20937 20937 20939 20941 20950 20950 20955 20957 20960 20961
     20966 20967 20969 20970 20972 20973 20976 20977 20981 20982 20984
     20986 20989 20990 20992 20992 20995 20996 20998 21000 21002 21003
     21006 21006 21009 21009 21012 21013 21015 21015 21021 21021 21028
     21029 21031 21031 21033 21034 21038 21038 21040 21040 21043 21043
     21046 21051 21059 21060 21063 21063 21066 21069 21071 21071 21076
     21076 21078 21078 21083 21083 21086 21086 21091 21093 21097 21098
     21103 21109 21117 21117 21119 21119 21123 21123 21127 21129 21133
     21133 21137 21138 21140 21140 21147 21148 21151 21152 21155 21155
     21158 21158 21161 21165 21167 21167 21169 21169 21172 21173 21177
     21177 21180 21180 21182 21182 21184 21185 21187 21187 21189 21189
     21191 21191 21193 21193 21197 21197 21202 21202 21205 21205 21207
     21209 21211 21211 21213 21216 21218 21220 21222 21223 21234 21235
     21237 21237 21240 21242 21246 21250 21253 21256 21261 21261 21263
     21264 21269 21271 21273 21274 21277 21277 21280 21281 21283 21284
     21290 21290 21295 21295 21297 21297 21299 21299 21304 21307 21311
     21313 21315 21315 21317 21322 21325 21325 21329 21332 21335 21336
     21338 21338 21340 21340 21342 21342 21344 21344 21350 21350 21353
     21353 21358 21365 21367 21368 21371 21371 21375 21375 21378 21378
     21380 21380 21395 21395 21398 21398 21400 21400 21402 21402 21407
     21408 21413 21414 21416 21417 21421 21422 21424 21424 21426 21427
     21430 21430 21435 21435 21442 21443 21448 21454 21460 21460 21462
     21463 21465 21465 21467 21467 21469 21469 21471 21471 21473 21477
     21480 21491 21494 21496 21498 21498 21505 21505 21507 21508 21512
     21521 21531 21531 21533 21533 21535 21536 21542 21542 21545 21545
     21547 21550 21558 21558 21560 21561 21563 21566 21568 21568 21570
     21570 21574 21574 21576 21578 21582 21582 21585 21585 21599 21599
     21608 21608 21610 21610 21616 21617 21619 21619 21621 21623 21627
     21629 21632 21632 21636 21636 21638 21638 21642 21644 21646 21648
     21650 21650 21660 21660 21666 21666 21668 21669 21672 21673 21675
     21676 21679 21679 21682 21683 21688 21688 21692 21694 21696 21698
     21700 21700 21703 21705 21720 21720 21729 21730 21733 21734 21736
     21737 21741 21742 21746 21746 21754 21754 21757 21757 21759 21759
     21764 21764 21766 21767 21775 21776 21780 21780 21782 21782 21806
     21807 21809 21809 21811 21811 21816 21817 21822 21822 21824 21824
     21828 21830 21836 21836 21839 21839 21843 21843 21846 21847 21852
     21853 21859 21859 21883 21884 21886 21886 21888 21888 21891 21892
     21894 21895 21897 21899 21912 21914 21916 21919 21927 21932 21934
     21934 21936 21936 21942 21942 21956 21957 21959 21959 21972 21972
     21978 21978 21980 21980 21983 21983 21987 21988 22007 22007 22009
     22009 22013 22014 22022 22022 22025 22025 22036 22036 22038 22040
     22043 22043 22057 22057 22063 22063 22065 22066 22068 22068 22070
     22070 22072 22072 22082 22082 22092 22092 22094 22094 22096 22096
     22107 22107 22116 22116 22120 22120 22122 22124 22132 22132 22136
     22136 22138 22138 22144 22144 22150 22151 22154 22154 22159 22159
     22164 22164 22176 22176 22178 22178 22181 22181 22190 22190 22196
     22196 22198 22198 22204 22204 22208 22211 22216 22216 22222 22222
     22225 22225 22227 22227 22231 22232 22234 22235 22238 22238 22240
     22240 22243 22243 22254 22254 22256 22256 22258 22259 22265 22266
     22269 22269 22271 22272 22275 22276 22280 22281 22283 22283 22285
     22285 22287 22287 22290 22291 22294 22294 22296 22296 22300 22300
     22303 22303 22310 22312 22317 22317 22320 22320 22327 22328 22331
     22331 22336 22336 22338 22338 22343 22343 22346 22346 22350 22353
     22361 22361 22369 22369 22372 22374 22377 22378 22399 22399 22402
     22402 22408 22409 22411 22411 22419 22419 22432 22432 22434 22436
     22442 22442 22444 22444 22448 22448 22451 22451 22464 22464 22467
     22467 22470 22472 22475 22475 22478 22478 22482 22484 22486 22486
     22492 22492 22495 22496 22499 22499 22516 22516 22519 22519 22521
     22522 22524 22524 22528 22528 22530 22530 22533 22534 22538 22539
     22549 22549 22553 22553 22557 22557 22561 22561 22564 22564 22570
     22570 22575 22577 22580 22581 22586 22586 22589 22589 22592 22593
     22602 22603 22609 22610 22612 22612 22615 22618 22622 22622 22626
     22626 22633 22633 22635 22635 22640 22640 22642 22642 22645 22645
     22649 22649 22654 22654 22659 22659 22661 22661 22675 22675 22679
     22679 22684 22684 22686 22687 22696 22696 22699 22699 22702 22702
     22706 22707 22712 22715 22718 22718 22721 22721 22725 22725 22727
     22727 22730 22730 22732 22732 22737 22737 22739 22739 22741 22741
     22743 22745 22748 22748 22750 22751 22756 22757 22763 22764 22766
     22770 22775 22775 22777 22781 22786 22786 22793 22795 22799 22800
     22805 22806 22808 22812 22818 22818 22821 22821 22823 22823 22825
     22830 22833 22834 22839 22840 22846 22846 22852 22852 22855 22857
     22862 22865 22867 22869 22871 22872 22874 22875 22877 22877 22880
     22880 22882 22883 22885 22885 22887 22890 22892 22892 22894 22894
     22899 22900 22904 22904 22909 22909 22913 22916 22922 22922 22925
     22925 22931 22931 22934 22934 22937 22937 22939 22939 22941 22941
     22947 22949 22952 22952 22956 22956 22962 22962 22969 22971 22974
     22974 22982 22982 22985 22985 22987 22987 22992 22993 22995 22996
     23001 23002 23004 23004 23013 23014 23016 23016 23018 23019 23030
     23030 23035 23035 23039 23039 23041 23041 23043 23043 23049 23049
     23057 23057 23064 23064 23066 23066 23068 23068 23071 23072 23077
     23077 23081 23081 23087 23087 23093 23094 23100 23100 23104 23105
     23110 23110 23113 23113 23130 23130 23138 23138 23142 23142 23146
     23146 23148 23148 23167 23167 23186 23186 23194 23195 23228 23230
     23233 23234 23241 23241 23243 23244 23248 23248 23254 23255 23265
     23265 23267 23267 23270 23270 23273 23273 23290 23291 23305 23305
     23307 23308 23318 23318 23330 23330 23338 23338 23340 23340 23344
     23344 23346 23346 23350 23350 23358 23358 23360 23360 23363 23363
     23365 23365 23376 23377 23380 23384 23386 23389 23391 23391 23395
     23398 23401 23401 23403 23403 23408 23409 23411 23411 23413 23413
     23416 23416 23418 23418 23424 23424 23427 23427 23429 23429 23431
     23433 23435 23437 23439 23439 23445 23445 23447 23453 23455 23455
     23458 23462 23470 23470 23472 23472 23475 23478 23480 23481 23487
     23488 23490 23495 23497 23497 23500 23500 23504 23504 23506 23508
     23512 23512 23515 23515 23517 23519 23521 23522 23524 23529 23531
     23532 23534 23534 23536 23536 23539 23539 23541 23542 23544 23544
     23546 23546 23550 23551 23553 23554 23556 23563 23565 23567 23569
     23569 23571 23571 23574 23574 23578 23578 23582 23582 23584 23584
     23586 23586 23588 23588 23592 23592 23597 23597 23601 23601 23608
     23617 23621 23622 23624 23624 23626 23627 23629 23633 23635 23635
     23637 23637 23646 23646 23648 23649 23652 23653 23660 23660 23662
     23663 23665 23665 23670 23670 23673 23673 23692 23692 23696 23697
     23700 23700 23713 23713 23718 23718 23720 23721 23723 23724 23729
     23729 23731 23731 23734 23736 23738 23740 23742 23742 23749 23749
     23751 23751 23769 23769 23776 23777 23784 23786 23789 23789 23791
     23792 23797 23798 23802 23803 23805 23805 23815 23815 23819 23819
     23822 23822 23825 23825 23828 23835 23839 23839 23842 23842 23847
     23847 23849 23849 23874 23874 23883 23884 23886 23886 23888 23888
     23890 23891 23900 23900 23913 23913 23916 23917 23919 23919 23923
     23923 23926 23926 23938 23938 23940 23940 23943 23943 23947 23948
     23952 23952 23965 23965 23970 23970 23980 23980 23982 23982 23991
     23994 23996 23997 24009 24009 24012 24013 24016 24016 24018 24019
     24022 24022 24027 24027 24029 24030 24033 24033 24035 24035 24037
     24040 24043 24043 24046 24046 24049 24053 24055 24055 24059 24059
     24061 24062 24066 24067 24070 24070 24075 24076 24081 24081 24086
     24086 24089 24091 24093 24093 24101 24101 24107 24107 24109 24109
     24111 24112 24115 24115 24118 24120 24125 24125 24128 24128 24131
     24133 24135 24135 24140 24140 24142 24142 24148 24149 24151 24151
     24159 24159 24161 24164 24178 24182 24184 24191 24193 24193 24195
     24196 24199 24199 24202 24202 24207 24207 24213 24215 24218 24218
     24220 24220 24224 24224 24230 24231 24235 24235 24237 24237 24245
     24248 24257 24259 24264 24266 24271 24272 24275 24275 24278 24278
     24282 24283 24285 24285 24287 24291 24296 24297 24300 24300 24304
     24305 24307 24308 24310 24312 24314 24316 24318 24319 24321 24321
     24323 24324 24329 24333 24335 24337 24339 24344 24347 24347 24351
     24351 24353 24353 24357 24359 24361 24361 24365 24365 24367 24367
     24369 24369 24372 24373 24375 24376 24380 24380 24382 24382 24385
     24385 24389 24389 24392 24392 24394 24394 24396 24396 24398 24398
     24401 24401 24403 24403 24406 24407 24409 24409 24412 24413 24417
     24418 24422 24423 24425 24429 24432 24433 24435 24435 24439 24439
     24441 24441 24444 24444 24447 24453 24455 24456 24458 24460 24464
     24467 24471 24473 24478 24478 24480 24481 24488 24490 24493 24494
     24499 24500 24503 24503 24505 24505 24508 24509 24515 24515 24517
     24517 24524 24525 24534 24537 24540 24542 24544 24544 24548 24548
     24555 24555 24560 24561 24565 24565 24568 24568 24571 24571 24573
     24573 24575 24575 24590 24592 24594 24594 24597 24598 24601 24601
     24603 24605 24608 24609 24613 24619 24623 24623 24625 24625 24634
     24634 24641 24643 24646 24646 24650 24651 24653 24653 24656 24656
     24658 24658 24661 24661 24665 24666 24669 24669 24671 24672 24674
     24677 24680 24685 24687 24688 24693 24693 24695 24695 24705 24705
     24707 24709 24713 24717 24722 24722 24724 24724 24726 24727 24730
     24731 24735 24736 24739 24739 24742 24743 24745 24746 24754 24758
     24760 24760 24764 24765 24773 24775 24785 24785 24787 24787 24789
     24789 24792 24792 24794 24794 24796 24796 24798 24801 24803 24803
     24807 24808 24816 24820 24822 24823 24825 24827 24832 24833 24835
     24835 24838 24838 24840 24841 24845 24847 24849 24849 24853 24853
     24858 24859 24863 24865 24871 24872 24876 24876 24880 24880 24884
     24884 24887 24887 24892 24895 24898 24898 24900 24900 24903 24904
     24906 24910 24915 24915 24917 24917 24920 24922 24925 24925 24927
     24927 24930 24931 24933 24933 24935 24936 24939 24939 24942 24945
     24947 24951 24958 24958 24962 24962 24967 24967 24970 24970 24974
     24974 24976 24977 24980 24980 24982 24982 24984 24986 24996 24996
     24999 24999 25001 25001 25003 25004 25006 25006 25010 25010 25014
     25014 25018 25018 25022 25022 25027 25027 25030 25037 25040 25040
     25059 25059 25062 25062 25074 25074 25076 25076 25078 25080 25082
     25082 25084 25088 25096 25098 25100 25102 25104 25108 25110 25110
     25114 25115 25117 25119 25121 25121 25126 25126 25130 25130 25134
     25136 25138 25140 25144 25144 25147 25147 25151 25153 25159 25161
     25163 25163 25165 25166 25171 25171 25173 25173 25176 25176 25179
     25179 25182 25182 25184 25184 25187 25187 25192 25192 25198 25198
     25201 25201 25206 25206 25209 25209 25212 25212 25214 25216 25218
     25220 25225 25226 25233 25240 25243 25244 25246 25246 25254 25254
     25259 25260 25265 25265 25269 25269 25273 25273 25275 25277 25282
     25282 25285 25290 25292 25293 25295 25300 25303 25305 25307 25309
     25312 25313 25324 25327 25329 25329 25331 25331 25333 25335 25342
     25343 25345 25346 25351 25353 25356 25356 25361 25361 25369 25369
     25375 25375 25383 25384 25387 25387 25391 25391 25402 25402 25405
     25407 25417 25417 25420 25421 25423 25424 25429 25429 25431 25431
     25436 25436 25447 25449 25451 25451 25454 25454 25458 25458 25462
     25463 25466 25467 25472 25472 25475 25475 25480 25481 25484 25484
     25486 25487 25490 25490 25494 25494 25496 25496 25499 25499 25503
     25507 25509 25509 25511 25516 25522 25522 25524 25525 25531 25531
     25534 25534 25536 25536 25539 25540 25542 25542 25545 25545 25551
     25552 25554 25554 25558 25558 25562 25563 25569 25569 25571 25571
     25577 25577 25582 25582 25588 25590 25594 25594 25606 25606 25613
     25613 25615 25615 25619 25619 25622 25623 25628 25628 25638 25638
     25640 25640 25644 25645 25652 25652 25654 25654 25658 25658 25662
     25662 25666 25666 25678 25678 25688 25688 25696 25696 25703 25703
     25705 25705 25711 25711 25718 25718 25720 25720 25722 25722 25731
     25731 25736 25736 25746 25747 25749 25749 25754 25754 25757 25758
     25764 25765 25769 25769 25771 25771 25773 25774 25776 25776 25778
     25778 25785 25785 25787 25788 25793 25794 25797 25797 25799 25799
     25805 25806 25810 25810 25812 25812 25816 25816 25818 25818 25824
     25827 25830 25831 25836 25836 25839 25839 25841 25842 25844 25844
     25846 25846 25850 25850 25853 25854 25856 25856 25861 25861 25880
     25880 25884 25885 25891 25892 25898 25900 25903 25903 25908 25913
     25915 25915 25918 25919 25925 25925 25928 25928 25933 25935 25937
     25937 25941 25945 25949 25950 25954 25955 25958 25958 25964 25964
     25968 25968 25970 25970 25972 25973 25975 25976 25986 25987 25991
     25993 25996 25996 25998 25998 26000 26001 26007 26007 26009 26009
     26011 26012 26015 26015 26017 26017 26020 26021 26023 26023 26027
     26029 26031 26032 26039 26039 26041 26041 26044 26045 26049 26049
     26051 26054 26059 26060 26063 26063 26066 26066 26071 26071 26073
     26073 26075 26075 26080 26082 26085 26089 26092 26093 26097 26097
     26106 26107 26112 26112 26114 26115 26118 26119 26121 26122 26124
     26124 26126 26127 26131 26133 26140 26140 26142 26144 26148 26149
     26151 26152 26157 26159 26161 26161 26164 26166 26171 26172 26175
     26175 26177 26180 26185 26185 26187 26187 26191 26191 26194 26194
     26199 26199 26201 26201 26205 26207 26210 26210 26212 26217 26222
     26224 26227 26228 26230 26230 26234 26234 26241 26241 26243 26244
     26247 26249 26254 26254 26257 26257 26262 26265 26269 26269 26272
     26272 26274 26274 26278 26278 26283 26283 26286 26286 26290 26290
     26292 26292 26296 26297 26300 26300 26302 26303 26305 26305 26308
     26308 26311 26311 26313 26313 26326 26326 26329 26330 26332 26333
     26336 26336 26342 26342 26345 26345 26352 26352 26354 26357 26359
     26368 26371 26371 26376 26377 26379 26379 26381 26383 26388 26391
     26395 26395 26397 26399 26406 26408 26410 26414 26417 26417 26420
     26420 26422 26424 26426 26426 26429 26429 26431 26431 26433 26433
     26438 26438 26441 26441 26446 26449 26451 26451 26454 26454 26457
     26457 26460 26460 26462 26470 26474 26474 26477 26477 26479 26483
     26485 26485 26487 26487 26492 26492 26494 26495 26501 26501 26503
     26503 26505 26505 26507 26508 26512 26512 26517 26517 26519 26519
     26522 26522 26524 26525 26528 26530 26534 26534 26537 26537 26543
     26543 26547 26548 26550 26553 26555 26555 26560 26561 26564 26564
     26566 26566 26570 26570 26574 26577 26579 26580 26584 26584 26586
     26586 26589 26590 26594 26594 26596 26596 26599 26599 26601 26601
     26604 26604 26606 26607 26609 26609 26611 26613 26619 26619 26622
     26623 26625 26628 26643 26643 26646 26647 26654 26654 26657 26658
     26665 26667 26674 26674 26676 26676 26680 26681 26684 26685 26688
     26692 26694 26694 26696 26696 26701 26702 26704 26708 26713 26713
     26716 26717 26719 26719 26723 26723 26727 26727 26740 26740 26742
     26743 26750 26751 26753 26753 26755 26755 26757 26757 26765 26765
     26767 26767 26771 26772 26775 26775 26779 26779 26781 26781 26783
     26784 26786 26786 26790 26792 26797 26797 26799 26801 26803 26803
     26805 26806 26809 26810 26812 26812 26820 26820 26822 26822 26824
     26827 26829 26829 26831 26831 26834 26834 26836 26837 26839 26840
     26842 26842 26847 26849 26851 26851 26855 26855 26862 26863 26866
     26866 26873 26874 26880 26881 26884 26885 26888 26888 26891 26895
     26898 26898 26905 26908 26913 26915 26917 26918 26920 26920 26922
     26922 26928 26928 26932 26932 26934 26934 26937 26937 26941 26941
     26943 26943 26954 26954 26963 26965 26969 26970 26972 26974 26976
     26978 26984 26984 26986 26987 26989 26991 26995 26997 26999 27001
     27004 27006 27009 27010 27018 27018 27022 27022 27025 27025 27028
     27029 27032 27032 27035 27036 27040 27040 27047 27047 27054 27054
     27057 27058 27060 27060 27067 27067 27070 27071 27073 27073 27075
     27075 27079 27079 27082 27086 27088 27088 27091 27091 27096 27097
     27101 27102 27106 27106 27111 27112 27115 27115 27117 27117 27122
     27122 27129 27129 27131 27131 27133 27133 27135 27135 27138 27138
     27141 27141 27146 27148 27154 27156 27159 27159 27161 27161 27163
     27163 27166 27167 27169 27171 27177 27179 27182 27182 27184 27184
     27189 27190 27192 27194 27197 27197 27204 27204 27206 27208 27211
     27211 27224 27225 27231 27231 27233 27234 27238 27238 27243 27243
     27250 27251 27256 27256 27262 27264 27268 27268 27277 27278 27280
     27280 27287 27287 27292 27292 27296 27296 27298 27299 27306 27306
     27308 27308 27310 27310 27315 27315 27320 27320 27323 27323 27329
     27331 27345 27345 27347 27347 27354 27355 27358 27359 27362 27362
     27364 27364 27368 27368 27370 27370 27386 27387 27396 27397 27402
     27402 27410 27410 27414 27414 27421 27421 27423 27425 27427 27427
     27431 27431 27442 27442 27447 27450 27453 27454 27459 27459 27463
     27463 27465 27465 27468 27468 27470 27470 27472 27472 27475 27476
     27481 27481 27483 27483 27487 27487 27489 27492 27494 27494 27497
     27498 27503 27503 27507 27508 27512 27513 27515 27515 27519 27520
     27523 27524 27526 27526 27529 27531 27533 27533 27541 27542 27544
     27544 27550 27550 27556 27556 27562 27563 27567 27567 27569 27573
     27575 27575 27578 27580 27583 27584 27589 27590 27595 27595 27597
     27598 27602 27604 27606 27606 27608 27608 27611 27611 27615 27615
     27627 27628 27631 27631 27635 27635 27656 27656 27663 27663 27665
     27665 27667 27668 27671 27671 27675 27675 27683 27684 27700 27700
     27703 27704 27710 27714 27726 27726 27728 27728 27733 27733 27735
     27735 27738 27738 27740 27744 27746 27746 27752 27752 27754 27754
     27759 27760 27762 27763 27770 27770 27773 27774 27777 27779 27782
     27782 27784 27784 27788 27789 27792 27792 27794 27795 27798 27798
     27801 27803 27809 27810 27819 27819 27822 27822 27825 27825 27827
     27827 27832 27839 27841 27841 27844 27845 27849 27850 27852 27852
     27859 27859 27861 27861 27863 27863 27865 27867 27869 27869 27873
     27875 27877 27877 27880 27880 27882 27882 27887 27889 27891 27891
     27908 27908 27915 27916 27922 27922 27927 27927 27929 27929 27931
     27931 27934 27935 27941 27941 27945 27947 27954 27955 27957 27958
     27960 27960 27963 27963 27965 27966 27969 27969 27972 27973 27993
     27994 27996 27996 28003 28004 28006 28006 28009 28010 28012 28012
     28014 28015 28020 28020 28023 28025 28037 28037 28039 28040 28044
     28044 28046 28046 28051 28051 28053 28054 28057 28057 28059 28060
     28076 28076 28079 28079 28082 28082 28085 28085 28088 28088 28092
     28092 28096 28096 28101 28103 28107 28108 28111 28111 28113 28114
     28117 28117 28120 28121 28126 28126 28129 28129 28132 28132 28134
     28134 28136 28136 28138 28140 28142 28142 28145 28147 28149 28149
     28151 28156 28165 28165 28167 28171 28179 28179 28181 28181 28185
     28187 28189 28189 28191 28193 28195 28199 28201 28201 28203 28207
     28216 28218 28220 28220 28222 28222 28227 28227 28234 28234 28237
     28238 28246 28246 28248 28248 28251 28252 28255 28255 28263 28263
     28267 28267 28270 28271 28274 28274 28278 28278 28286 28288 28290
     28290 28300 28300 28303 28304 28310 28310 28312 28312 28316 28317
     28319 28319 28322 28322 28325 28325 28330 28330 28335 28335 28338
     28338 28342 28343 28346 28346 28349 28349 28351 28351 28354 28354
     28356 28357 28361 28361 28363 28364 28369 28369 28371 28373 28381
     28382 28396 28396 28399 28399 28402 28402 28404 28404 28407 28408
     28414 28415 28417 28418 28422 28422 28425 28425 28431 28431 28433
     28433 28435 28437 28448 28448 28450 28451 28459 28460 28465 28466
     28472 28472 28478 28479 28481 28481 28485 28485 28500 28500 28504
     28504 28507 28508 28511 28511 28516 28516 28518 28518 28525 28528
     28532 28532 28536 28536 28538 28538 28540 28540 28544 28546 28548
     28548 28550 28550 28552 28552 28558 28558 28561 28561 28567 28567
     28577 28577 28579 28580 28586 28586 28593 28593 28595 28595 28597
     28597 28601 28601 28608 28611 28614 28614 28628 28629 28632 28632
     28635 28635 28639 28641 28644 28644 28651 28652 28654 28655 28657
     28657 28659 28659 28661 28662 28666 28666 28670 28670 28673 28673
     28677 28677 28679 28679 28681 28681 28683 28683 28687 28687 28689
     28689 28693 28693 28696 28696 28698 28699 28701 28703 28710 28712
     28716 28716 28720 28720 28722 28722 28734 28734 28748 28748 28753
     28753 28760 28760 28771 28771 28779 28779 28783 28784 28792 28792
     28796 28797 28805 28805 28809 28810 28814 28814 28818 28818 28825
     28825 28843 28847 28851 28851 28856 28859 28872 28872 28875 28875
     28879 28879 28889 28889 28893 28893 28895 28895 28913 28913 28921
     28921 28925 28925 28932 28932 28937 28937 28943 28943 28948 28948
     28953 28954 28956 28956 28961 28961 28966 28966 28982 28982 28988
     28988 28998 28999 29001 29001 29004 29004 29006 29006 29013 29014
     29017 29017 29020 29020 29026 29026 29028 29031 29033 29033 29036
     29036 29038 29038 29053 29053 29060 29060 29064 29064 29066 29066
     29071 29071 29076 29077 29081 29081 29087 29087 29096 29096 29100
     29100 29105 29105 29113 29113 29118 29118 29121 29121 29123 29123
     29128 29129 29134 29134 29136 29136 29138 29138 29140 29141 29143
     29143 29151 29152 29157 29159 29164 29166 29173 29173 29177 29177
     29179 29180 29182 29183 29190 29190 29197 29197 29200 29200 29211
     29211 29224 29224 29226 29226 29228 29229 29232 29232 29234 29234
     29237 29238 29242 29248 29254 29256 29259 29260 29266 29266 29272
     29273 29275 29275 29277 29277 29279 29279 29281 29282 29287 29287
     29289 29289 29298 29298 29300 29300 29305 29305 29309 29310 29312
     29314 29319 29319 29330 29330 29334 29334 29344 29344 29346 29346
     29351 29351 29356 29356 29359 29359 29361 29362 29366 29366 29369
     29369 29374 29374 29378 29380 29382 29382 29390 29390 29392 29392
     29394 29394 29399 29399 29401 29401 29403 29403 29408 29410 29417
     29417 29420 29421 29431 29433 29436 29437 29450 29450 29462 29463
     29467 29469 29471 29471 29476 29477 29481 29483 29486 29487 29492
     29492 29494 29495 29502 29503 29508 29509 29518 29519 29527 29527
     29539 29539 29544 29544 29546 29546 29552 29552 29554 29554 29557
     29557 29559 29560 29562 29563 29572 29572 29575 29575 29577 29577
     29579 29579 29590 29590 29609 29609 29618 29619 29627 29627 29629
     29629 29632 29632 29634 29634 29640 29642 29645 29646 29650 29650
     29654 29654 29662 29662 29664 29664 29667 29667 29669 29669 29674
     29674 29677 29678 29681 29681 29685 29685 29688 29688 29694 29694
     29699 29699 29701 29703 29705 29705 29730 29730 29733 29734 29737
     29738 29742 29742 29746 29750 29754 29754 29759 29759 29761 29761
     29781 29781 29785 29788 29790 29792 29794 29796 29801 29802 29807
     29808 29811 29811 29814 29814 29822 29822 29827 29827 29833 29833
     29835 29835 29854 29855 29858 29858 29863 29863 29872 29872 29885
     29885 29898 29898 29903 29903 29908 29908 29916 29916 29920 29920
     29922 29923 29926 29927 29929 29929 29934 29934 29936 29938 29942
     29944 29953 29953 29955 29957 29964 29966 29969 29969 29971 29971
     29973 29973 29976 29976 29978 29978 29980 29980 29982 29983 29987
     29987 29989 29990 29992 29992 29995 29996 29999 30003 30007 30008
     30010 30012 30020 30020 30022 30022 30025 30029 30031 30031 30033
     30033 30036 30036 30041 30045 30048 30048 30050 30050 30052 30055
     30057 30059 30061 30061 30063 30064 30067 30068 30070 30072 30079
     30079 30082 30082 30086 30087 30089 30091 30094 30095 30097 30097
     30100 30100 30106 30106 30109 30109 30115 30115 30117 30117 30123
     30123 30129 30131 30133 30133 30136 30137 30140 30142 30146 30147
     30149 30149 30151 30151 30154 30154 30157 30157 30162 30162 30164
     30165 30168 30169 30171 30171 30174 30174 30178 30179 30185 30185
     30192 30192 30194 30196 30202 30202 30204 30204 30206 30207 30209
     30209 30217 30217 30219 30219 30221 30221 30239 30242 30244 30244
     30247 30247 30256 30256 30260 30260 30267 30267 30274 30274 30278
     30280 30284 30284 30290 30290 30294 30294 30296 30296 30300 30300
     30305 30306 30311 30314 30316 30316 30320 30320 30322 30322 30326
     30326 30328 30328 30330 30334 30336 30336 30338 30340 30342 30344
     30347 30347 30350 30350 30352 30352 30355 30355 30358 30358 30361
     30364 30366 30366 30374 30374 30382 30382 30384 30384 30388 30388
     30391 30394 30399 30399 30402 30403 30406 30406 30408 30408 30410
     30410 30413 30413 30418 30418 30422 30423 30427 30428 30430 30431
     30433 30433 30435 30437 30439 30439 30442 30442 30446 30446 30450
     30450 30452 30452 30456 30456 30459 30459 30462 30462 30465 30465
     30468 30468 30471 30473 30475 30476 30491 30491 30494 30496 30500
     30502 30505 30505 30519 30520 30522 30522 30524 30524 30528 30528
     30534 30535 30554 30555 30561 30561 30563 30563 30565 30566 30568
     30568 30571 30571 30585 30585 30590 30591 30603 30603 30606 30606
     30609 30609 30622 30622 30624 30624 30629 30629 30636 30637 30640
     30640 30643 30643 30646 30646 30649 30649 30651 30653 30655 30655
     30663 30663 30669 30669 30679 30679 30682 30684 30690 30691 30693
     30693 30695 30695 30697 30697 30701 30703 30707 30707 30716 30716
     30722 30722 30732 30732 30738 30738 30740 30741 30752 30753 30757
     30759 30770 30770 30772 30772 30778 30778 30783 30783 30789 30789
     30798 30798 30813 30813 30820 30820 30827 30828 30831 30831 30834
     30834 30836 30836 30842 30842 30844 30844 30849 30849 30854 30855
     30860 30862 30865 30865 30867 30867 30869 30869 30871 30871 30874
     30874 30883 30883 30887 30887 30889 30890 30895 30895 30901 30901
     30906 30906 30908 30908 30910 30910 30913 30913 30917 30918 30922
     30923 30928 30929 30932 30932 30938 30938 30951 30952 30956 30956
     30959 30959 30964 30964 30973 30973 30977 30977 30983 30983 30990
     30990 30993 30994 31001 31001 31014 31014 31018 31020 31024 31024
     31034 31034 31036 31036 31038 31038 31040 31041 31047 31049 31056
     31056 31059 31059 31061 31063 31066 31066 31069 31072 31074 31074
     31077 31077 31080 31080 31085 31085 31095 31095 31098 31098 31103
     31105 31108 31109 31114 31114 31117 31119 31124 31124 31131 31131
     31133 31133 31142 31143 31146 31146 31150 31150 31152 31152 31155
     31155 31161 31162 31165 31169 31177 31177 31179 31179 31185 31186
     31189 31189 31192 31192 31199 31199 31201 31201 31203 31204 31206
     31207 31209 31209 31212 31212 31216 31216 31227 31227 31232 31232
     31240 31240 31243 31243 31245 31246 31252 31252 31255 31258 31260
     31260 31263 31264 31278 31278 31281 31282 31287 31287 31291 31296
     31298 31299 31302 31302 31305 31305 31309 31312 31319 31319 31329
     31331 31337 31337 31339 31339 31344 31344 31348 31348 31350 31350
     31353 31354 31357 31357 31359 31359 31361 31361 31363 31364 31368
     31368 31378 31379 31381 31384 31391 31391 31401 31402 31406 31408
     31414 31414 31418 31418 31423 31423 31427 31429 31431 31432 31434
     31435 31437 31437 31439 31439 31441 31443 31445 31445 31449 31450
     31452 31453 31455 31459 31461 31463 31466 31467 31469 31469 31471
     31472 31478 31478 31480 31482 31487 31487 31490 31490 31492 31492
     31494 31494 31496 31496 31498 31499 31503 31503 31505 31505 31512
     31513 31515 31515 31518 31518 31520 31520 31525 31526 31528 31528
     31532 31532 31539 31539 31541 31542 31545 31545 31557 31558 31560
     31561 31563 31565 31567 31570 31572 31572 31574 31574 31581 31581
     31589 31589 31591 31591 31596 31596 31598 31598 31600 31601 31604
     31605 31610 31610 31622 31623 31627 31627 31629 31629 31631 31631
     31634 31634 31636 31637 31639 31642 31644 31647 31649 31649 31658
     31658 31661 31661 31665 31665 31668 31668 31672 31672 31680 31681
     31684 31684 31686 31687 31689 31689 31691 31692 31695 31695 31709
     31709 31712 31712 31716 31718 31721 31721 31725 31725 31731 31731
     31734 31735 31744 31744 31751 31751 31757 31757 31761 31764 31767
     31767 31775 31775 31777 31777 31779 31779 31783 31783 31786 31787
     31799 31800 31805 31808 31811 31811 31820 31821 31823 31824 31828
     31828 31830 31830 31832 31832 31839 31840 31844 31845 31852 31852
     31859 31859 31861 31861 31870 31870 31873 31875 31881 31881 31883
     31883 31885 31885 31888 31888 31890 31890 31893 31893 31895 31896
     31899 31899 31903 31903 31905 31906 31908 31909 31911 31912 31915
     31915 31917 31918 31921 31923 31929 31929 31933 31934 31936 31936
     31938 31938 31941 31941 31946 31946 31950 31950 31954 31954 31958
     31958 31960 31960 31964 31964 31966 31968 31970 31970 31975 31975
     31983 31983 31986 31986 31988 31988 31990 31990 31992 31992 31994
     31995 31998 31998 32000 32000 32002 32002 32004 32006 32010 32011
     32013 32013 32016 32016 32020 32021 32023 32028 32032 32034 32043
     32044 32046 32048 32050 32051 32053 32053 32057 32058 32063 32063
     32066 32070 32072 32072 32075 32076 32078 32080 32086 32086 32091
     32092 32094 32094 32097 32099 32102 32102 32104 32104 32110 32110
     32113 32115 32117 32118 32121 32121 32125 32125 32137 32137 32143
     32143 32147 32147 32153 32156 32159 32160 32162 32163 32171 32178
     32180 32181 32183 32184 32186 32187 32189 32191 32199 32199 32202
     32203 32207 32207 32209 32210 32213 32214 32216 32216 32218 32218
     32220 32222 32224 32225 32228 32228 32232 32233 32236 32236 32239
     32239 32242 32242 32244 32244 32251 32251 32257 32257 32260 32261
     32265 32267 32274 32274 32283 32283 32286 32287 32289 32291 32294
     32294 32299 32299 32302 32302 32305 32306 32309 32309 32311 32311
     32313 32315 32317 32318 32321 32321 32323 32323 32326 32326 32330
     32331 32333 32333 32338 32338 32340 32342 32345 32346 32349 32350
     32358 32359 32361 32362 32365 32365 32368 32368 32377 32377 32379
     32381 32383 32383 32386 32387 32392 32394 32396 32396 32398 32400
     32402 32404 32406 32406 32411 32412 32566 32566 32568 32568 32570
     32570 32581 32581 32583 32583 32588 32590 32592 32593 32596 32597
     32600 32600 32607 32608 32615 32619 32622 32622 32624 32624 32626
     32626 32629 32629 32631 32633 32642 32643 32645 32648 32650 32650
     32652 32652 32654 32654 32660 32660 32666 32666 32669 32670 32673
     32673 32675 32676 32680 32681 32686 32687 32690 32690 32694 32694
     32696 32697 32701 32701 32705 32705 32709 32710 32714 32714 32716
     32716 32722 32722 32724 32725 32736 32737 32742 32742 32745 32745
     32747 32747 32752 32752 32755 32755 32761 32761 32763 32764 32768
     32769 32771 32774 32779 32780 32784 32784 32786 32786 32789 32789
     32791 32793 32796 32796 32801 32801 32808 32808 32819 32819 32822
     32822 32827 32827 32829 32829 32831 32831 32838 32838 32842 32842
     32850 32850 32854 32854 32856 32856 32858 32858 32862 32863 32865
     32866 32872 32872 32879 32880 32882 32884 32886 32887 32889 32889
     32893 32895 32900 32903 32905 32905 32907 32908 32915 32915 32918
     32918 32920 32920 32922 32923 32925 32925 32929 32930 32933 32933
     32937 32938 32940 32941 32943 32943 32945 32946 32948 32948 32954
     32954 32963 32964 32966 32966 32972 32972 32974 32974 32982 32982
     32985 32987 32989 32990 32993 32993 32996 32997 33007 33007 33009
     33009 33012 33012 33016 33016 33020 33021 33026 33026 33029 33034
     33050 33051 33059 33059 33065 33065 33071 33071 33073 33073 33075
     33075 33081 33081 33086 33086 33094 33094 33099 33099 33102 33102
     33104 33105 33107 33109 33119 33119 33125 33126 33131 33131 33134
     33134 33136 33137 33140 33140 33144 33146 33151 33152 33154 33155
     33160 33160 33162 33162 33167 33167 33171 33171 33173 33173 33178
     33178 33180 33181 33184 33184 33187 33188 33192 33193 33200 33200
     33203 33203 33205 33205 33208 33208 33210 33210 33213 33216 33218
     33218 33222 33222 33224 33225 33229 33229 33233 33233 33235 33235
     33240 33242 33247 33248 33251 33251 33253 33253 33255 33256 33258
     33258 33261 33261 33267 33268 33274 33276 33278 33278 33281 33282
     33285 33285 33287 33290 33292 33294 33296 33296 33298 33298 33302
     33304 33307 33308 33310 33311 33321 33324 33326 33326 33331 33331
     33333 33337 33344 33344 33351 33351 33368 33370 33373 33373 33375
     33375 33378 33378 33380 33380 33382 33382 33384 33384 33386 33387
     33390 33391 33393 33394 33398 33400 33406 33406 33419 33419 33421
     33421 33426 33426 33433 33433 33437 33437 33439 33439 33445 33446
     33451 33453 33455 33455 33457 33457 33459 33459 33464 33465 33467
     33467 33469 33469 33477 33477 33489 33492 33495 33495 33497 33497
     33499 33500 33502 33503 33505 33505 33507 33507 33509 33511 33515
     33515 33521 33521 33523 33524 33529 33531 33537 33542 33545 33545
     33550 33550 33558 33560 33564 33564 33571 33571 33576 33576 33579
     33579 33583 33583 33585 33586 33588 33590 33592 33593 33600 33600
     33605 33605 33609 33610 33615 33616 33618 33618 33624 33624 33634
     33634 33651 33651 33653 33653 33655 33655 33659 33660 33663 33663
     33669 33669 33671 33671 33673 33674 33678 33678 33683 33683 33686
     33686 33690 33690 33694 33696 33698 33698 33704 33704 33706 33707
     33713 33713 33717 33717 33725 33725 33729 33729 33733 33733 33735
     33735 33738 33738 33740 33740 33742 33742 33747 33747 33750 33750
     33752 33752 33756 33756 33759 33760 33769 33769 33771 33771 33775
     33778 33780 33780 33782 33783 33787 33787 33789 33789 33795 33796
     33799 33799 33803 33806 33811 33811 33824 33824 33826 33826 33833
     33834 33836 33836 33841 33841 33845 33845 33848 33848 33852 33853
     33862 33862 33864 33865 33870 33870 33879 33879 33883 33883 33889
     33891 33894 33894 33897 33897 33899 33903 33905 33905 33909 33909
     33911 33911 33913 33914 33922 33922 33924 33924 33931 33931 33936
     33936 33940 33940 33945 33945 33948 33948 33951 33951 33953 33953
     33965 33965 33970 33970 33972 33972 33976 33977 33979 33980 33983
     33983 33985 33985 33988 33988 33990 33990 33993 33995 33997 33997
     34000 34001 34006 34006 34009 34010 34012 34012 34028 34028 34030
     34030 34036 34036 34044 34044 34047 34048 34054 34054 34065 34065
     34067 34069 34071 34072 34074 34074 34079 34079 34081 34081 34086
     34086 34092 34093 34101 34101 34109 34109 34112 34113 34115 34115
     34120 34123 34126 34126 34131 34131 34133 34133 34135 34138 34147
     34147 34152 34155 34157 34157 34167 34167 34174 34174 34176 34176
     34180 34180 34183 34184 34186 34186 34192 34193 34196 34196 34199
     34199 34201 34201 34203 34204 34212 34212 34214 34214 34216 34220
     34222 34224 34233 34234 34241 34241 34249 34249 34253 34253 34255
     34256 34261 34261 34268 34269 34276 34277 34281 34282 34295 34295
     34297 34299 34302 34302 34306 34306 34310 34311 34314 34315 34323
     34323 34326 34327 34330 34330 34338 34338 34349 34349 34351 34352
     34367 34367 34381 34382 34384 34384 34388 34389 34394 34394 34396
     34396 34398 34399 34407 34407 34411 34411 34417 34417 34425 34425
     34427 34427 34442 34444 34451 34451 34453 34453 34467 34468 34473
     34475 34479 34480 34486 34486 34500 34500 34502 34503 34505 34505
     34507 34507 34509 34510 34516 34516 34521 34521 34523 34523 34526
     34527 34532 34532 34537 34537 34540 34543 34552 34553 34555 34555
     34558 34558 34560 34560 34562 34563 34566 34566 34568 34570 34573
     34573 34577 34578 34584 34584 34586 34586 34588 34588 34597 34597
     34601 34601 34612 34612 34615 34615 34619 34619 34623 34623 34633
     34633 34635 34636 34638 34638 34643 34643 34645 34645 34647 34647
     34649 34649 34655 34656 34659 34659 34662 34662 34664 34664 34666
     34666 34670 34670 34676 34676 34678 34678 34680 34680 34687 34687
     34690 34690 34701 34701 34719 34719 34722 34722 34731 34731 34735
     34735 34739 34739 34746 34747 34749 34749 34752 34752 34756 34756
     34758 34759 34763 34763 34768 34768 34770 34770 34784 34784 34799
     34799 34802 34802 34806 34807 34809 34809 34811 34811 34814 34814
     34821 34821 34823 34823 34829 34831 34833 34833 34837 34838 34849
     34851 34855 34855 34865 34865 34870 34870 34873 34873 34875 34875
     34880 34880 34882 34882 34884 34884 34886 34886 34892 34893 34898
     34899 34903 34903 34905 34905 34907 34907 34909 34910 34913 34915
     34920 34920 34923 34923 34928 34928 34930 34930 34933 34933 34935
     34935 34941 34943 34945 34946 34952 34952 34955 34955 34957 34957
     34962 34962 34966 34967 34969 34969 34974 34974 34978 34978 34980
     34980 34987 34987 34990 34990 34992 34993 34996 34997 34999 34999
     35007 35007 35009 35013 35023 35023 35028 35029 35032 35033 35036
     35037 35039 35039 35041 35041 35048 35048 35058 35061 35064 35065
     35068 35070 35074 35074 35076 35076 35079 35079 35082 35082 35084
     35084 35088 35088 35090 35091 35100 35102 35109 35109 35114 35115
     35126 35126 35128 35128 35131 35131 35137 35137 35139 35140 35148
     35149 35158 35158 35166 35168 35172 35172 35174 35174 35178 35178
     35181 35181 35183 35183 35186 35186 35188 35188 35191 35191 35198
     35199 35201 35201 35203 35203 35206 35208 35210 35211 35215 35215
     35219 35219 35222 35224 35226 35226 35233 35233 35238 35239 35241
     35242 35244 35244 35247 35247 35250 35251 35258 35258 35261 35261
     35263 35264 35282 35282 35290 35290 35292 35293 35299 35299 35302
     35303 35316 35316 35320 35320 35328 35328 35330 35331 35336 35336
     35338 35338 35340 35340 35342 35342 35344 35344 35346 35347 35350
     35352 35355 35355 35357 35357 35359 35359 35363 35363 35365 35365
     35370 35370 35373 35373 35377 35377 35379 35380 35382 35383 35386
     35388 35393 35393 35398 35398 35400 35400 35408 35410 35412 35413
     35419 35419 35422 35422 35424 35424 35426 35427 35430 35430 35433
     35433 35435 35438 35440 35443 35449 35449 35452 35452 35458 35458
     35460 35461 35463 35463 35465 35465 35468 35469 35473 35473 35475
     35475 35477 35477 35480 35480 35482 35482 35486 35486 35488 35489
     35491 35496 35500 35501 35504 35504 35506 35506 35513 35513 35516
     35516 35518 35519 35522 35522 35524 35524 35527 35527 35531 35533
     35535 35535 35538 35538 35542 35542 35546 35548 35550 35554 35556
     35556 35558 35559 35563 35563 35565 35566 35569 35569 35571 35571
     35574 35576 35578 35578 35582 35582 35584 35586 35588 35588 35591
     35591 35596 35596 35598 35598 35600 35600 35604 35604 35606 35607
     35609 35611 35613 35613 35616 35617 35622 35622 35624 35624 35627
     35628 35635 35635 35641 35641 35646 35646 35649 35649 35657 35657
     35660 35660 35662 35663 35667 35667 35670 35670 35672 35672 35674
     35676 35679 35679 35686 35686 35691 35692 35695 35698 35700 35700
     35703 35703 35709 35709 35711 35712 35715 35715 35722 35722 35724
     35724 35726 35726 35728 35728 35730 35731 35734 35734 35737 35738
     35895 35895 35898 35898 35903 35903 35905 35905 35910 35910 35912
     35912 35914 35914 35916 35916 35918 35918 35920 35920 35925 35925
     35930 35930 35937 35938 35946 35948 35960 35962 35964 35964 35970
     35970 35973 35973 35977 35978 35980 35982 35988 35988 35992 35992
     35997 35998 36000 36002 36007 36016 36018 36020 36022 36024 36027
     36029 36031 36036 36039 36040 36042 36042 36045 36046 36049 36049
     36051 36051 36058 36060 36062 36062 36064 36064 36066 36068 36070
     36070 36074 36074 36077 36077 36080 36080 36084 36084 36090 36093
     36100 36101 36103 36104 36106 36107 36109 36109 36111 36112 36114
     36116 36118 36118 36196 36196 36198 36199 36203 36203 36205 36205
     36208 36209 36211 36212 36214 36215 36225 36225 36229 36229 36234
     36234 36249 36249 36259 36259 36264 36264 36275 36275 36282 36282
     36286 36286 36290 36290 36299 36300 36303 36303 36310 36310 36314
     36315 36317 36317 36319 36319 36321 36321 36323 36323 36328 36328
     36330 36331 36335 36335 36339 36339 36341 36341 36348 36348 36351
     36351 36360 36362 36367 36368 36381 36383 36394 36394 36400 36400
     36404 36405 36418 36418 36420 36420 36423 36426 36428 36428 36432
     36432 36437 36437 36441 36441 36447 36448 36451 36452 36466 36466
     36468 36468 36470 36470 36476 36476 36481 36481 36484 36485 36487
     36487 36490 36491 36493 36493 36497 36497 36499 36500 36505 36505
     36513 36513 36522 36524 36527 36529 36542 36542 36549 36550 36552
     36552 36554 36557 36559 36559 36562 36562 36571 36571 36575 36575
     36578 36579 36587 36587 36600 36600 36603 36606 36611 36611 36613
     36613 36617 36618 36620 36620 36626 36629 36633 36633 36635 36637
     36639 36639 36646 36646 36649 36650 36655 36655 36659 36659 36664
     36665 36667 36667 36670 36671 36674 36674 36676 36678 36681 36681
     36684 36686 36695 36695 36700 36700 36703 36703 36705 36708 36763
     36764 36766 36767 36771 36771 36775 36776 36781 36786 36791 36791
     36794 36796 36799 36799 36802 36802 36804 36805 36814 36814 36817
     36817 36820 36820 36826 36826 36834 36834 36837 36838 36841 36843
     36845 36845 36847 36848 36852 36852 36855 36858 36861 36861 36864
     36865 36867 36867 36869 36870 36875 36875 36877 36881 36883 36887
     36889 36890 36893 36899 36903 36903 36910 36910 36913 36914 36917
     36918 36920 36921 36924 36924 36926 36926 36929 36930 36933 36933
     36935 36935 36937 36939 36941 36950 36952 36953 36956 36956 36958
     36958 36960 36961 36963 36963 36965 36965 36967 36969 36973 36975
     36978 36978 36981 36984 36986 36986 36988 36989 36991 36996 36999
     36999 37001 37002 37007 37007 37009 37009 37027 37027 37030 37030
     37032 37032 37034 37034 37039 37039 37041 37041 37045 37045 37048
     37048 37057 37057 37066 37066 37070 37070 37083 37083 37086 37086
     37089 37090 37092 37092 37096 37096 37101 37101 37109 37109 37111
     37111 37117 37117 37122 37122 37138 37138 37141 37141 37145 37145
     37159 37159 37165 37165 37168 37168 37170 37170 37193 37198 37202
     37202 37204 37204 37206 37206 37208 37208 37218 37219 37221 37221
     37225 37226 37228 37228 37234 37235 37237 37237 37239 37240 37250
     37250 37255 37255 37257 37257 37259 37259 37261 37261 37264 37264
     37266 37266 37271 37271 37276 37276 37282 37282 37284 37284 37290
     37291 37295 37295 37300 37301 37304 37304 37306 37306 37312 37313
     37318 37321 37323 37329 37334 37336 37338 37343 37345 37345 37347
     37351 37357 37358 37365 37366 37372 37372 37375 37375 37382 37382
     37386 37386 37389 37390 37392 37393 37396 37397 37406 37406 37417
     37417 37420 37420 37428 37428 37431 37431 37433 37434 37436 37436
     37439 37440 37444 37445 37448 37449 37451 37451 37454 37454 37456
     37457 37463 37463 37465 37467 37470 37470 37474 37474 37476 37476
     37478 37479 37489 37489 37495 37496 37502 37502 37504 37504 37507
     37507 37509 37509 37512 37512 37521 37521 37523 37523 37525 37526
     37528 37528 37530 37532 37543 37543 37549 37549 37559 37559 37561
     37561 37583 37584 37586 37587 37589 37589 37591 37591 37593 37593
     37600 37600 37604 37604 37607 37607 37609 37610 37613 37613 37618
     37619 37624 37628 37631 37631 37634 37634 37638 37638 37647 37648
     37656 37658 37661 37662 37664 37667 37669 37670 37672 37672 37675
     37676 37678 37679 37682 37682 37685 37685 37690 37691 37700 37700
     37704 37704 37707 37707 37709 37709 37716 37716 37718 37719 37723
     37724 37728 37728 37740 37740 37742 37742 37744 37744 37749 37749
     37756 37756 37758 37758 37772 37772 37780 37780 37782 37783 37786
     37786 37796 37796 37799 37799 37804 37806 37808 37808 37817 37817
     37827 37827 37830 37830 37832 37832 37840 37841 37846 37848 37853
     37854 37857 37857 37860 37861 37864 37864 37880 37880 37891 37891
     37895 37895 37904 37904 37907 37908 37912 37914 37921 37921 37931
     37931 37937 37937 37941 37942 37944 37944 37946 37946 37953 37953
     37956 37957 37960 37960 37969 37971 37978 37979 37982 37982 37984
     37984 37986 37986 37994 37994 38000 38000 38005 38005 38007 38007
     38012 38015 38017 38017 38263 38263 38272 38272 38274 38275 38279
     38279 38281 38283 38287 38287 38289 38292 38294 38294 38296 38297
     38304 38304 38306 38309 38311 38312 38317 38317 38322 38322 38329
     38329 38331 38332 38334 38334 38339 38339 38343 38343 38346 38346
     38348 38349 38356 38358 38360 38360 38364 38364 38369 38370 38373
     38373 38428 38428 38433 38433 38440 38440 38442 38442 38446 38447
     38450 38450 38459 38459 38463 38464 38466 38466 38468 38468 38475
     38477 38479 38480 38491 38495 38498 38502 38506 38506 38508 38508
     38512 38512 38514 38515 38517 38520 38522 38522 38525 38525 38533
     38534 38536 38536 38538 38539 38541 38543 38548 38549 38551 38553
     38555 38557 38560 38560 38563 38563 38567 38568 38570 38570 38575
     38578 38580 38580 38582 38585 38587 38588 38592 38593 38596 38599
     38601 38601 38603 38606 38609 38609 38613 38614 38617 38617 38619
     38620 38626 38627 38632 38632 38634 38635 38640 38640 38642 38642
     38646 38647 38649 38649 38651 38651 38656 38656 38660 38660 38662
     38664 38666 38666 38669 38671 38673 38673 38675 38675 38678 38678
     38681 38681 38684 38684 38686 38686 38692 38692 38695 38695 38698
     38698 38704 38704 38706 38707 38712 38713 38715 38715 38717 38718
     38722 38724 38726 38726 38728 38729 38733 38733 38735 38735 38737
     38738 38741 38742 38745 38745 38748 38748 38750 38750 38752 38754
     38756 38756 38758 38758 38760 38761 38763 38763 38765 38765 38769
     38769 38772 38772 38777 38778 38780 38780 38785 38785 38788 38790
     38795 38795 38797 38797 38799 38800 38808 38808 38812 38812 38816
     38816 38819 38819 38822 38822 38824 38824 38827 38827 38829 38829
     38835 38836 38851 38851 38854 38854 38856 38856 38859 38859 38867
     38867 38876 38876 38893 38894 38898 38899 38901 38902 38907 38907
     38911 38911 38913 38915 38917 38918 38920 38920 38924 38924 38927
     38931 38935 38936 38938 38938 38945 38945 38948 38948 38956 38957
     38964 38964 38967 38968 38971 38973 38982 38982 38987 38991 38996
     38997 38999 39000 39003 39003 39006 39006 39013 39013 39015 39015
     39019 39019 39023 39025 39027 39028 39080 39080 39082 39082 39087
     39087 39089 39089 39094 39094 39107 39108 39110 39110 39131 39132
     39135 39135 39138 39138 39145 39145 39147 39147 39149 39151 39154
     39154 39156 39156 39164 39166 39171 39171 39173 39173 39177 39178
     39180 39180 39184 39184 39186 39188 39192 39192 39197 39198 39200
     39201 39204 39204 39207 39208 39212 39212 39214 39214 39229 39230
     39234 39234 39237 39237 39241 39241 39243 39244 39248 39250 39253
     39253 39255 39255 39318 39321 39326 39326 39333 39333 39336 39336
     39340 39342 39347 39348 39356 39356 39361 39361 39364 39366 39368
     39368 39376 39378 39381 39381 39384 39384 39387 39387 39389 39389
     39391 39391 39394 39394 39405 39406 39409 39410 39416 39416 39419
     39419 39423 39423 39425 39425 39429 39429 39438 39439 39442 39443
     39449 39449 39464 39464 39467 39467 39472 39472 39479 39479 39486
     39486 39488 39488 39490 39491 39493 39493 39501 39502 39509 39509
     39511 39511 39514 39515 39519 39519 39522 39522 39524 39525 39529
     39531 39592 39592 39597 39597 39600 39600 39608 39608 39612 39612
     39616 39616 39620 39620 39631 39631 39633 39633 39635 39636 39640
     39641 39644 39644 39646 39647 39650 39651 39654 39654 39658 39659
     39661 39663 39665 39665 39668 39668 39671 39671 39675 39675 39686
     39686 39704 39704 39706 39706 39711 39711 39714 39715 39717 39717
     39719 39722 39726 39727 39729 39730 39739 39740 39745 39749 39757
     39759 39761 39761 39764 39764 39768 39768 39770 39770 39791 39791
     39794 39794 39796 39797 39811 39811 39822 39823 39825 39827 39830
     39831 39839 39840 39848 39848 39850 39851 39853 39854 39857 39857
     39860 39860 39865 39865 39867 39867 39872 39872 39878 39878 39881
     39882 39887 39887 39889 39890 39892 39892 39894 39894 39899 39899
     39905 39908 39912 39912 39920 39922 39925 39925 39936 39936 39940
     39940 39942 39942 39944 39946 39948 39949 39952 39952 39954 39957
     39963 39963 39969 39969 39972 39973 39981 39984 39986 39986 39993
     39995 39998 39998 40006 40008 40018 40018 40023 40023 40026 40026
     40032 40032 40039 40039 40054 40054 40056 40056 40165 40165 40167
     40167 40169 40169 40171 40172 40176 40176 40179 40180 40182 40182
     40195 40195 40198 40201 40206 40206 40210 40210 40213 40213 40219
     40219 40223 40223 40227 40227 40230 40230 40232 40232 40234 40236
     40251 40251 40254 40255 40257 40257 40260 40260 40262 40262 40264
     40264 40272 40273 40281 40281 40284 40286 40288 40289 40292 40292
     40299 40300 40303 40304 40306 40306 40314 40314 40327 40327 40329
     40329 40335 40335 40346 40346 40356 40356 40361 40361 40363 40363
     40367 40367 40370 40370 40372 40372 40376 40376 40378 40379 40385
     40386 40388 40388 40390 40390 40399 40399 40403 40403 40409 40409
     40422 40422 40429 40429 40431 40431 40434 40434 40440 40442 40445
     40445 40473 40475 40478 40478 40565 40565 40568 40569 40573 40573
     40575 40575 40577 40577 40584 40584 40587 40588 40593 40595 40597
     40597 40599 40599 40605 40605 40607 40607 40613 40614 40617 40618
     40621 40621 40632 40636 40638 40639 40644 40644 40652 40658 40660
     40660 40664 40665 40667 40670 40672 40672 40677 40677 40680 40680
     40687 40687 40692 40692 40694 40695 40697 40697 40699 40701 40711
     40712 40718 40718 40723 40723 40725 40725 40736 40737 40748 40748
     40763 40763 40766 40766 40778 40779 40782 40783 40786 40786 40788
     40788 40799 40803 40806 40807 40810 40810 40812 40812 40818 40818
     40822 40823 40845 40845 40853 40853 40860 40861 40864 40864 57344
     59223 63785 63785 63964 63964 64014 64045 65281 65374 65377 65439
     65504 65509 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 2252 :NAME "windows-1252" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1252)       [Wendt]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 160 255 338 339 352 353 376 376 381 382 402 402 710 710 732
     732 8211 8212 8216 8218 8220 8222 8224 8226 8230 8230 8240 8240
     8249 8250 8364 8364 8482 8482))
  (MAKE-CHARACTER-SET :MIB-ENUM 2253 :NAME "windows-1253" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1253) [Lazhintseva]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 160 160 163 169 171 174 176 179 181 183 187 187 189 189 402
     402 900 902 904 906 908 908 910 929 931 974 8211 8213 8216 8218
     8220 8222 8224 8226 8230 8230 8240 8240 8249 8250 8364 8364 8482
     8482))
  (MAKE-CHARACTER-SET :MIB-ENUM 2250 :NAME "windows-1250" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1250) [Lazhintseva]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 160 160 164 164 166 169 171 174 176 177 180 184 187 187 193
     194 196 196 199 199 201 201 203 203 205 206 211 212 214 215 218 218
     220 221 223 223 225 226 228 228 231 231 233 233 235 235 237 238 243
     244 246 247 250 250 252 253 258 263 268 273 280 283 313 314 317 318
     321 324 327 328 336 337 340 341 344 347 350 357 366 369 377 382 711
     711 728 729 731 731 733 733 8211 8212 8216 8218 8220 8222 8224 8226
     8230 8230 8240 8240 8249 8250 8364 8364 8482 8482))
  (MAKE-CHARACTER-SET :MIB-ENUM 1012 :NAME "UTF-7" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE '"RFC 2152" :COMMENTS 'NIL :REFERENCES
   '("[RFC2152]") :RANGES #(0 1114111))
  (MAKE-CHARACTER-SET :MIB-ENUM 2251 :NAME "windows-1251" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1251) [Lazhintseva]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 160 160 164 164 166 167 169 169 171 174 176 177 181 183 187
     187 1025 1036 1038 1103 1105 1116 1118 1119 1168 1169 8211 8212
     8216 8218 8220 8222 8224 8226 8230 8230 8240 8240 8249 8250 8364
     8364 8470 8470 8482 8482))
  (MAKE-CHARACTER-SET :MIB-ENUM 15 :NAME "JIS_X0201" :ALIASES
   '("csHalfWidthKatakana" "X0201") :MIME-ENCODING 'NIL :SOURCE
   '"JIS X 0201-1976.   One byte only, this is equivalent to  JIS/Roman (similar to ASCII) plus eight-bit half-width Katakana"
   :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 91 93 125 127 127 165 165 8254 8254 65377 65439))
  (MAKE-CHARACTER-SET :MIB-ENUM 2256 :NAME "windows-1256" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1256) [Lazhintseva]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 160 160 162 169 171 185 187 190 215 215 224 224 226 226 231
     235 238 239 244 244 247 247 249 249 251 252 338 339 402 402 710 710
     1548 1548 1563 1563 1567 1567 1569 1594 1600 1618 1657 1657 1662
     1662 1670 1670 1672 1672 1681 1681 1688 1688 1705 1705 1711 1711
     1722 1722 1726 1726 1729 1729 1746 1746 8204 8207 8211 8212 8216
     8218 8220 8222 8224 8226 8230 8230 8240 8240 8249 8250 8364 8364
     8482 8482))
  (MAKE-CHARACTER-SET :MIB-ENUM 2049 :NAME "IBM861" :ALIASES
   '("csIBM861" "cp-is" "861" "cp861") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 161 163 163 171 172 176 178 183 183 187 189 191 191 193
     193 196 199 201 201 205 205 208 208 211 211 214 214 216 216 218 218
     220 226 228 235 237 237 240 240 243 244 246 248 250 254 402 402 915
     915 920 920 931 931 934 934 937 937 945 945 948 949 956 956 960 960
     963 964 966 966 8319 8319 8359 8359 8729 8730 8734 8734 8745 8745
     8776 8776 8801 8801 8804 8805 8976 8976 8992 8993 9472 9472 9474
     9474 9484 9484 9488 9488 9492 9492 9496 9496 9500 9500 9508 9508
     9516 9516 9524 9524 9532 9532 9552 9580 9600 9600 9604 9604 9608
     9608 9612 9612 9616 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 2026 :NAME #17="Big5" :ALIASES
   '("csBig5") :MIME-ENCODING '#17# :SOURCE
   '"Chinese for Taiwan Multi-byte set. PCL Symbol Set Id: 18T"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 128 167 167 175 177 183 183 215 215 247 247 711 711 713 715 717
     717 729 729 913 929 931 937 945 961 963 969 8211 8212 8216 8217
     8220 8221 8229 8231 8242 8242 8245 8245 8251 8251 8364 8364 8451
     8451 8453 8453 8457 8457 8544 8553 8592 8595 8598 8601 8725 8725
     8730 8730 8734 8736 8739 8739 8741 8741 8745 8747 8750 8750 8756
     8757 8786 8786 8800 8801 8806 8807 8853 8853 8857 8857 8869 8869
     8895 8895 9472 9472 9474 9474 9484 9484 9488 9488 9492 9492 9496
     9496 9500 9500 9508 9508 9516 9516 9524 9524 9532 9532 9552 9588
     9601 9615 9619 9621 9632 9633 9650 9651 9660 9661 9670 9671 9675
     9675 9678 9679 9698 9701 9733 9734 9792 9792 9794 9794 12288 12291
     12296 12306 12308 12309 12317 12318 12321 12329 12549 12585 12963
     12963 13198 13199 13212 13214 13217 13217 13252 13252 13262 13262
     13265 13266 13269 13269 19968 19969 19971 19971 19975 19985 19988
     19990 19992 19993 19998 19999 20006 20006 20011 20011 20013 20014
     20016 20019 20024 20025 20027 20028 20034 20035 20037 20037 20039
     20040 20043 20043 20045 20047 20050 20051 20054 20054 20056 20057
     20060 20063 20073 20073 20083 20083 20094 20095 20098 20100 20102
     20102 20104 20104 20107 20110 20113 20117 20121 20121 20123 20123
     20126 20127 20129 20130 20132 20134 20136 20136 20139 20142 20147
     20147 20150 20150 20153 20154 20160 20164 20166 20171 20173 20173
     20180 20186 20188 20191 20193 20193 20195 20197 20200 20201 20208
     20215 20219 20219 20221 20221 20223 20226 20228 20229 20232 20235
     20237 20245 20248 20249 20253 20253 20258 20258 20268 20269 20271
     20272 20275 20276 20278 20278 20280 20280 20282 20287 20289 20289
     20291 20291 20294 20297 20300 20324 20327 20327 20329 20332 20334
     20336 20339 20361 20363 20363 20365 20365 20367 20370 20372 20376
     20378 20382 20398 20399 20402 20403 20405 20407 20409 20411 20415
     20421 20423 20423 20425 20427 20429 20433 20435 20436 20438 20449
     20460 20460 20462 20463 20465 20465 20467 20472 20474 20474 20478
     20478 20480 20480 20485 20487 20489 20489 20491 20495 20497 20508
     20510 20515 20517 20525 20527 20529 20531 20531 20533 20533 20535
     20535 20540 20540 20544 20545 20547 20547 20549 20559 20561 20561
     20563 20563 20565 20565 20567 20567 20570 20581 20584 20587 20589
     20592 20594 20599 20602 20602 20605 20605 20608 20608 20610 20611
     20613 20613 20615 20615 20619 20622 20625 20626 20628 20630 20632
     20638 20642 20643 20652 20664 20666 20667 20669 20671 20673 20674
     20676 20683 20686 20687 20689 20689 20691 20695 20698 20699 20701
     20701 20704 20704 20707 20714 20716 20721 20723 20723 20725 20726
     20728 20729 20731 20731 20733 20736 20738 20748 20752 20757 20759
     20760 20762 20762 20764 20764 20767 20770 20772 20774 20777 20778
     20781 20782 20784 20789 20791 20797 20799 20801 20803 20809 20811
     20813 20818 20818 20820 20821 20823 20823 20825 20831 20833 20835
     20837 20837 20839 20841 20843 20846 20849 20849 20853 20856 20860
     20860 20864 20864 20871 20871 20873 20874 20877 20877 20879 20879
     20881 20885 20887 20888 20894 20894 20896 20896 20898 20898 20900
     20901 20906 20906 20908 20908 20912 20913 20918 20919 20921 20921
     20924 20926 20932 20934 20936 20936 20938 20942 20944 20944 20948
     20948 20951 20952 20956 20958 20960 20961 20976 20977 20979 20979
     20981 20982 20984 20986 20989 20989 20992 20993 20995 20995 20998
     21002 21004 21004 21006 21006 21008 21011 21014 21015 21020 21022
     21025 21025 21028 21029 21032 21034 21038 21038 21040 21043 21045
     21048 21050 21051 21057 21057 21059 21060 21062 21063 21065 21070
     21074 21074 21076 21078 21082 21087 21089 21090 21097 21103 21106
     21106 21108 21109 21111 21112 21114 21117 21119 21124 21127 21133
     21137 21137 21139 21139 21142 21145 21147 21147 21151 21152 21155
     21155 21158 21158 21161 21166 21179 21180 21182 21182 21184 21187
     21191 21191 21193 21193 21197 21197 21202 21203 21205 21209 21211
     21211 21213 21215 21218 21220 21222 21222 21225 21225 21227 21227
     21231 21233 21235 21237 21239 21240 21242 21244 21246 21247 21253
     21254 21256 21259 21261 21266 21269 21271 21273 21274 21276 21277
     21279 21283 21290 21290 21293 21293 21295 21297 21300 21300 21303
     21303 21305 21305 21308 21313 21315 21317 21319 21322 21324 21325
     21329 21332 21335 21335 21338 21338 21340 21340 21342 21342 21344
     21345 21347 21347 21350 21350 21356 21356 21358 21363 21365 21365
     21367 21369 21371 21372 21375 21375 21378 21378 21380 21380 21386
     21386 21390 21391 21394 21394 21396 21396 21398 21402 21404 21407
     21412 21413 21415 21415 21420 21421 21426 21426 21428 21428 21433
     21433 21435 21435 21443 21443 21448 21451 21453 21453 21460 21460
     21462 21463 21467 21467 21471 21471 21473 21478 21480 21491 21493
     21493 21496 21496 21499 21500 21505 21505 21507 21508 21510 21522
     21528 21529 21531 21536 21540 21550 21552 21553 21555 21555 21557
     21561 21563 21566 21568 21571 21573 21576 21578 21578 21582 21583
     21588 21588 21600 21608 21611 21612 21615 21624 21626 21634 21636
     21636 21638 21640 21643 21646 21648 21650 21653 21654 21656 21656
     21658 21658 21664 21666 21669 21681 21683 21683 21686 21688 21690
     21705 21710 21711 21718 21718 21726 21726 21728 21730 21732 21739
     21741 21742 21745 21747 21751 21752 21754 21757 21759 21759 21761
     21761 21763 21772 21774 21778 21780 21780 21783 21783 21786 21786
     21798 21799 21802 21802 21804 21817 21819 21820 21822 21822 21824
     21825 21827 21830 21832 21832 21834 21835 21837 21842 21845 21847
     21852 21852 21854 21855 21857 21862 21866 21866 21877 21879 21883
     21892 21895 21903 21905 21909 21912 21914 21916 21917 21919 21919
     21921 21928 21930 21934 21937 21939 21941 21941 21947 21947 21951
     21952 21954 21974 21977 21981 21983 21983 21985 21993 21999 21999
     22002 22002 22006 22007 22009 22010 22012 22018 22020 22020 22022
     22022 22024 22025 22028 22032 22034 22039 22043 22045 22047 22047
     22055 22055 22057 22058 22060 22060 22062 22064 22066 22070 22072
     22075 22077 22082 22085 22086 22088 22090 22092 22092 22094 22094
     22099 22099 22103 22106 22110 22110 22112 22112 22114 22118 22120
     22132 22134 22137 22142 22151 22156 22160 22163 22163 22165 22165
     22167 22170 22172 22173 22181 22184 22186 22190 22194 22199 22204
     22206 22208 22211 22213 22214 22216 22221 22225 22225 22227 22228
     22231 22231 22234 22235 22237 22241 22244 22245 22247 22247 22250
     22251 22254 22254 22256 22256 22263 22263 22265 22266 22271 22271
     22273 22276 22279 22285 22290 22292 22294 22294 22296 22296 22298
     22300 22302 22304 22306 22307 22312 22314 22316 22320 22323 22324
     22331 22331 22334 22334 22336 22337 22341 22341 22343 22343 22345
     22354 22369 22370 22372 22372 22374 22374 22376 22379 22381 22381
     22383 22391 22395 22397 22400 22400 22402 22403 22411 22412 22415
     22415 22419 22421 22423 22427 22429 22432 22434 22437 22446 22446
     22453 22454 22456 22458 22460 22461 22463 22463 22465 22467 22470
     22471 22475 22476 22478 22480 22482 22482 22484 22485 22492 22492
     22495 22501 22503 22503 22505 22505 22508 22510 22512 22525 22528
     22530 22532 22542 22544 22544 22548 22548 22553 22553 22555 22558
     22560 22561 22563 22565 22567 22570 22572 22585 22587 22587 22589
     22589 22591 22591 22600 22607 22609 22613 22615 22619 22621 22622
     22626 22629 22632 22632 22635 22635 22637 22637 22639 22639 22641
     22641 22644 22646 22649 22659 22661 22667 22670 22673 22675 22676
     22680 22680 22684 22689 22691 22691 22693 22694 22696 22697 22699
     22700 22702 22703 22705 22705 22707 22707 22714 22719 22721 22722
     22725 22729 22734 22735 22737 22742 22744 22747 22749 22751 22754
     22756 22759 22761 22763 22764 22767 22767 22772 22772 22777 22778
     22780 22783 22787 22787 22790 22790 22796 22799 22802 22802 22804
     22807 22809 22810 22812 22812 22816 22816 22818 22818 22820 22821
     22823 22823 22825 22831 22833 22833 22839 22840 22844 22844 22846
     22846 22848 22848 22852 22853 22855 22858 22862 22865 22867 22869
     22871 22872 22874 22874 22876 22876 22880 22882 22887 22887 22889
     22891 22893 22894 22896 22900 22902 22905 22907 22917 22922 22922
     22925 22928 22930 22931 22934 22937 22941 22942 22944 22952 22958
     22959 22961 22966 22969 22974 22976 22977 22979 22979 22981 22984
     22986 22996 22998 22998 23000 23000 23002 23006 23008 23009 23011
     23014 23016 23018 23020 23022 23025 23031 23034 23041 23043 23043
     23049 23050 23052 23052 23055 23055 23057 23057 23059 23059 23061
     23065 23067 23068 23070 23072 23075 23075 23077 23077 23081 23081
     23085 23086 23091 23091 23093 23097 23100 23100 23102 23102 23104
     23108 23110 23114 23116 23117 23120 23123 23125 23128 23130 23136
     23138 23138 23140 23143 23145 23146 23148 23149 23152 23152 23159
     23160 23162 23165 23167 23167 23171 23172 23178 23180 23182 23184
     23186 23189 23191 23191 23194 23199 23202 23202 23205 23207 23209
     23209 23212 23212 23214 23234 23236 23236 23238 23245 23253 23267
     23269 23270 23272 23278 23283 23291 23293 23293 23295 23295 23297
     23299 23301 23301 23303 23305 23307 23308 23311 23312 23315 23316
     23318 23319 23321 23323 23325 23326 23328 23329 23331 23336 23338
     23338 23340 23344 23346 23346 23348 23348 23352 23352 23356 23360
     23363 23363 23365 23365 23367 23368 23371 23374 23376 23377 23379
     23384 23386 23389 23391 23391 23394 23397 23401 23401 23403 23404
     23406 23406 23408 23411 23413 23413 23415 23416 23418 23419 23421
     23421 23423 23423 23425 23425 23427 23429 23431 23433 23435 23436
     23438 23439 23442 23443 23445 23445 23447 23452 23458 23464 23468
     23470 23472 23472 23475 23478 23480 23481 23487 23490 23492 23495
     23498 23502 23504 23508 23510 23510 23512 23513 23518 23532 23534
     23538 23541 23542 23544 23544 23546 23546 23553 23553 23555 23556
     23559 23570 23573 23574 23578 23578 23583 23583 23586 23586 23588
     23589 23592 23592 23594 23594 23596 23596 23600 23601 23603 23603
     23607 23612 23614 23617 23620 23624 23627 23633 23636 23638 23640
     23641 23644 23645 23648 23648 23650 23653 23655 23658 23660 23663
     23665 23665 23667 23668 23673 23676 23678 23678 23686 23686 23688
     23693 23695 23701 23709 23709 23711 23729 23731 23731 23733 23736
     23750 23756 23758 23760 23762 23764 23766 23771 23774 23775 23784
     23784 23786 23786 23788 23790 23792 23793 23796 23796 23798 23801
     23803 23803 23805 23805 23807 23809 23814 23815 23819 23823 23825
     23826 23828 23828 23830 23831 23833 23835 23837 23840 23842 23849
     23854 23854 23856 23866 23868 23869 23871 23875 23877 23877 23879
     23879 23881 23884 23886 23886 23888 23890 23893 23893 23897 23897
     23902 23902 23906 23907 23909 23909 23911 23913 23915 23916 23919
     23919 23921 23922 23927 23927 23929 23930 23932 23938 23940 23940
     23942 23946 23949 23949 23954 23957 23959 23959 23961 23962 23964
     23970 23975 23978 23980 23986 23988 23989 23991 23992 23994 23994
     23996 23997 24000 24000 24002 24003 24006 24007 24009 24009 24011
     24011 24013 24013 24015 24015 24017 24018 24020 24022 24024 24024
     24029 24034 24037 24040 24043 24043 24046 24046 24048 24052 24055
     24055 24057 24057 24061 24063 24066 24068 24070 24070 24074 24074
     24076 24076 24078 24078 24081 24081 24084 24091 24093 24093 24095
     24101 24104 24105 24107 24107 24109 24109 24115 24116 24118 24120
     24125 24126 24128 24129 24131 24133 24138 24143 24147 24149 24151
     24153 24155 24157 24159 24163 24166 24176 24178 24182 24184 24185
     24187 24190 24192 24192 24194 24194 24196 24196 24199 24205 24207
     24207 24213 24215 24218 24220 24224 24224 24226 24232 24234 24238
     24240 24249 24254 24254 24257 24258 24260 24268 24270 24270 24273
     24291 24293 24297 24300 24300 24302 24303 24305 24307 24310 24311
     24314 24314 24318 24319 24321 24322 24324 24325 24327 24328 24330
     24331 24335 24335 24338 24341 24343 24344 24346 24347 24349 24349
     24351 24351 24354 24356 24358 24361 24365 24366 24368 24369 24371
     24371 24373 24376 24380 24380 24384 24384 24387 24388 24390 24390
     24392 24396 24398 24399 24404 24404 24406 24409 24413 24413 24418
     24418 24420 24421 24423 24423 24425 24429 24431 24433 24435 24436
     24438 24441 24444 24450 24453 24460 24464 24466 24470 24473 24475
     24476 24478 24481 24485 24486 24488 24495 24498 24498 24501 24503
     24505 24505 24507 24513 24515 24515 24517 24517 24521 24521 24524
     24525 24527 24530 24532 24537 24541 24542 24544 24545 24547 24549
     24552 24552 24554 24555 24557 24559 24561 24561 24563 24565 24567
     24568 24570 24571 24573 24573 24575 24576 24585 24599 24601 24606
     24608 24610 24612 24623 24626 24629 24631 24631 24633 24633 24640
     24647 24649 24649 24652 24653 24656 24656 24658 24661 24664 24667
     24669 24671 24674 24688 24690 24690 24703 24705 24707 24714 24716
     24718 24720 24720 24722 24722 24724 24727 24730 24733 24735 24736
     24738 24739 24744 24744 24752 24754 24756 24769 24771 24783 24785
     24785 24787 24789 24792 24797 24799 24802 24804 24804 24806 24806
     24816 24828 24830 24833 24835 24838 24840 24843 24845 24848 24850
     24854 24856 24856 24858 24861 24863 24863 24867 24867 24871 24873
     24875 24876 24878 24879 24882 24882 24884 24884 24886 24887 24891
     24891 24894 24897 24900 24911 24914 24918 24920 24920 24922 24923
     24925 24927 24929 24931 24933 24936 24938 24940 24942 24942 24944
     24951 24953 24954 24956 24956 24958 24958 24960 24960 24962 24963
     24969 24974 24976 24980 24982 24982 24986 24987 24989 24989 24991
     24991 24993 24994 24996 24996 24999 25014 25016 25016 25018 25018
     25020 25020 25022 25023 25025 25027 25029 25037 25046 25046 25048
     25048 25054 25056 25059 25067 25069 25070 25072 25074 25077 25089
     25091 25092 25095 25098 25100 25102 25104 25106 25108 25110 25113
     25115 25119 25125 25127 25127 25129 25131 25133 25134 25136 25136
     25138 25140 25142 25142 25146 25146 25149 25155 25158 25163 25165
     25166 25168 25172 25176 25180 25182 25182 25184 25190 25197 25204
     25206 25207 25209 25217 25219 25220 25222 25226 25228 25228 25230
     25231 25233 25240 25256 25265 25267 25270 25272 25273 25275 25279
     25282 25282 25284 25284 25286 25300 25302 25308 25323 25347 25351
     25353 25355 25361 25363 25366 25384 25389 25391 25391 25394 25396
     25398 25398 25400 25406 25408 25425 25428 25434 25445 25445 25447
     25449 25451 25451 25453 25458 25461 25464 25466 25469 25472 25477
     25479 25482 25484 25490 25492 25492 25494 25497 25499 25509 25511
     25521 25533 25534 25536 25536 25538 25552 25554 25555 25557 25565
     25567 25569 25571 25573 25575 25579 25581 25590 25593 25593 25606
     25606 25609 25616 25618 25624 25626 25628 25630 25640 25642 25648
     25651 25655 25657 25657 25661 25665 25667 25667 25675 25675 25677
     25678 25680 25684 25688 25689 25691 25697 25701 25705 25707 25712
     25714 25723 25725 25725 25727 25727 25730 25730 25733 25733 25735
     25740 25743 25744 25746 25747 25749 25754 25756 25760 25762 25766
     25769 25769 25771 25774 25776 25779 25787 25791 25793 25797 25799
     25799 25801 25803 25805 25808 25810 25810 25812 25812 25814 25819
     25824 25824 25826 25828 25830 25830 25832 25833 25835 25837 25839
     25844 25847 25848 25850 25857 25859 25860 25862 25863 25865 25865
     25868 25872 25875 25881 25883 25885 25888 25894 25897 25903 25906
     25907 25910 25913 25915 25915 25917 25919 25921 25921 25923 25923
     25925 25926 25928 25930 25935 25935 25937 25937 25939 25945 25948
     25950 25954 25960 25962 25962 25964 25964 25967 25967 25970 25980
     25983 25988 25991 25991 25996 25996 26000 26002 26004 26007 26009
     26009 26011 26018 26020 26021 26023 26024 26026 26028 26030 26032
     26034 26035 26038 26041 26043 26045 26047 26047 26049 26054 26059
     26064 26066 26067 26070 26071 26074 26075 26077 26079 26081 26082
     26085 26086 26088 26089 26092 26101 26106 26109 26112 26112 26114
     26133 26140 26141 26143 26146 26148 26152 26155 26155 26157 26159
     26161 26166 26169 26170 26177 26179 26181 26181 26183 26183 26185
     26186 26188 26188 26191 26191 26193 26194 26201 26207 26209 26210
     26212 26214 26216 26216 26218 26218 26220 26220 26222 26226 26228
     26228 26230 26236 26238 26238 26240 26240 26244 26244 26246 26253
     26256 26257 26260 26265 26269 26269 26271 26274 26280 26283 26286
     26290 26292 26293 26295 26299 26301 26302 26304 26304 26308 26308
     26310 26316 26319 26319 26322 26322 26326 26326 26328 26334 26336
     26336 26339 26340 26342 26342 26344 26345 26347 26350 26352 26352
     26354 26356 26358 26361 26364 26364 26366 26369 26371 26373 26376
     26379 26381 26381 26383 26384 26386 26389 26391 26392 26395 26395
     26397 26397 26399 26403 26406 26408 26410 26414 26417 26417 26419
     26421 26424 26431 26437 26441 26443 26449 26451 26451 26453 26455
     26457 26458 26460 26464 26474 26474 26476 26477 26479 26495 26497
     26497 26499 26503 26505 26505 26507 26510 26512 26517 26519 26522
     26524 26525 26527 26527 26542 26544 26546 26555 26560 26566 26568
     26580 26584 26591 26594 26599 26601 26616 26618 26618 26620 26620
     26623 26623 26642 26644 26646 26648 26650 26650 26652 26653 26655
     26657 26661 26662 26664 26667 26669 26671 26673 26677 26680 26685
     26688 26694 26696 26697 26699 26705 26707 26708 26731 26731 26733
     26735 26737 26738 26740 26745 26747 26755 26757 26759 26761 26764
     26767 26772 26774 26775 26779 26781 26783 26788 26791 26805 26820
     26820 26822 26825 26827 26830 26832 26840 26842 26842 26844 26849
     26851 26852 26854 26860 26862 26877 26884 26888 26890 26901 26903
     26903 26917 26917 26922 26922 26927 26928 26930 26933 26935 26937
     26939 26941 26943 26946 26948 26949 26952 26956 26958 26959 26961
     26964 26966 26976 26978 26979 26981 26982 26984 26993 26996 27003
     27010 27011 27014 27014 27021 27022 27024 27025 27027 27031 27033
     27036 27038 27038 27040 27041 27043 27057 27059 27063 27065 27065
     27067 27071 27073 27076 27078 27078 27081 27088 27091 27092 27097
     27097 27106 27106 27108 27112 27115 27118 27121 27124 27126 27128
     27131 27138 27140 27146 27149 27149 27151 27151 27153 27153 27155
     27161 27163 27163 27165 27169 27171 27171 27173 27176 27186 27186
     27188 27189 27192 27201 27204 27204 27206 27209 27211 27211 27213
     27217 27220 27222 27224 27227 27229 27234 27236 27236 27238 27243
     27245 27245 27247 27247 27254 27254 27262 27265 27267 27269 27271
     27271 27273 27273 27276 27278 27280 27287 27290 27292 27294 27302
     27304 27304 27308 27311 27315 27316 27318 27323 27325 27325 27330
     27331 27333 27335 27339 27341 27343 27345 27347 27347 27353 27361
     27365 27365 27367 27368 27370 27372 27374 27377 27379 27379 27384
     27388 27392 27392 27394 27396 27400 27403 27407 27411 27414 27418
     27422 27422 27424 27425 27427 27427 27429 27429 27432 27432 27436
     27437 27439 27439 27441 27444 27446 27455 27457 27459 27461 27470
     27472 27473 27476 27478 27481 27481 27483 27484 27486 27495 27498
     27498 27501 27501 27506 27506 27510 27513 27515 27515 27518 27520
     27522 27524 27526 27526 27528 27530 27532 27535 27537 27537 27540
     27545 27547 27547 27550 27552 27554 27559 27562 27563 27565 27568
     27570 27571 27573 27575 27578 27578 27580 27581 27583 27584 27587
     27597 27599 27600 27602 27604 27606 27608 27610 27611 27614 27614
     27616 27616 27618 27620 27622 27624 27627 27628 27631 27632 27634
     27635 27639 27641 27643 27654 27656 27657 27659 27661 27663 27665
     27667 27670 27672 27675 27677 27677 27679 27681 27683 27688 27690
     27692 27694 27696 27699 27700 27702 27702 27704 27704 27707 27707
     27710 27715 27718 27718 27722 27728 27730 27730 27732 27733 27735
     27735 27737 27737 27739 27745 27749 27755 27757 27757 27759 27764
     27766 27766 27768 27768 27770 27771 27773 27774 27776 27792 27794
     27798 27800 27805 27819 27822 27824 27825 27827 27828 27830 27847
     27849 27850 27852 27853 27855 27863 27865 27870 27872 27875 27877
     27877 27879 27881 27883 27891 27893 27893 27897 27897 27904 27905
     27907 27908 27911 27922 27926 27931 27933 27936 27938 27938 27941
     27941 27943 27970 27992 27994 27998 28010 28012 28016 28020 28032
     28034 28046 28048 28053 28055 28056 28074 28076 28078 28079 28082
     28085 28087 28088 28090 28096 28098 28098 28100 28109 28111 28134
     28136 28151 28153 28157 28160 28160 28163 28163 28165 28165 28185
     28189 28191 28200 28203 28214 28216 28225 28227 28231 28233 28235
     28237 28238 28241 28246 28248 28248 28250 28265 28267 28267 28270
     28271 28273 28276 28279 28281 28296 28297 28301 28304 28306 28308
     28310 28313 28315 28327 28330 28331 28334 28340 28342 28343 28345
     28346 28348 28374 28376 28376 28380 28380 28395 28399 28401 28402
     28404 28409 28411 28419 28421 28426 28429 28431 28434 28437 28440
     28442 28444 28444 28446 28451 28453 28455 28457 28467 28469 28476
     28478 28481 28483 28483 28494 28501 28503 28504 28506 28507 28509
     28516 28518 28519 28521 28528 28530 28531 28534 28536 28538 28544
     28546 28546 28548 28553 28555 28558 28560 28560 28562 28567 28574
     28574 28576 28596 28598 28598 28600 28602 28604 28605 28607 28612
     28614 28623 28628 28629 28632 28632 28635 28644 28646 28649 28651
     28658 28660 28660 28663 28663 28666 28668 28670 28673 28676 28679
     28681 28687 28689 28689 28692 28701 28703 28708 28710 28715 28719
     28725 28727 28732 28734 28742 28744 28746 28748 28748 28753 28754
     28757 28760 28762 28763 28765 28774 28776 28779 28784 28785 28788
     28788 28790 28790 28792 28792 28794 28794 28796 28797 28802 28806
     28810 28810 28814 28814 28817 28822 28824 28826 28831 28831 28833
     28833 28836 28836 28841 28841 28843 28849 28851 28853 28855 28856
     28858 28858 28862 28862 28869 28872 28874 28875 28877 28879 28881
     28884 28887 28890 28892 28894 28896 28898 28900 28900 28911 28912
     28915 28916 28918 28925 28927 28928 28930 28930 28932 28932 28934
     28934 28937 28942 28944 28944 28947 28947 28951 28951 28953 28956
     28958 28963 28965 28966 28968 28968 28974 28978 28982 28982 28986
     28986 28993 28996 28998 28999 29001 29001 29003 29006 29008 29008
     29010 29012 29014 29014 29016 29018 29020 29034 29036 29036 29038
     29038 29040 29040 29042 29042 29048 29048 29051 29051 29053 29053
     29056 29058 29060 29063 29065 29066 29071 29072 29074 29074 29076
     29076 29079 29079 29081 29089 29092 29093 29095 29098 29100 29100
     29103 29107 29109 29109 29112 29113 29116 29131 29134 29136 29138
     29138 29140 29142 29144 29148 29151 29154 29156 29160 29164 29166
     29168 29170 29172 29172 29176 29177 29179 29183 29185 29187 29189
     29191 29194 29194 29196 29197 29200 29200 29203 29204 29209 29211
     29213 29215 29218 29219 29222 29226 29228 29229 29232 29232 29237
     29238 29240 29243 29245 29247 29249 29250 29252 29252 29254 29260
     29263 29263 29266 29267 29270 29270 29272 29275 29277 29283 29287
     29287 29289 29290 29292 29292 29294 29296 29298 29300 29302 29305
     29307 29313 29316 29318 29320 29321 29323 29326 29328 29331 29333
     29336 29338 29339 29341 29342 29345 29354 29356 29356 29358 29360
     29364 29365 29370 29370 29373 29373 29375 29382 29385 29388 29390
     29390 29392 29394 29396 29396 29398 29402 29404 29404 29407 29409
     29411 29412 29414 29414 29416 29419 29427 29428 29430 29441 29447
     29448 29450 29452 29455 29455 29457 29459 29462 29465 29467 29470
     29474 29475 29477 29479 29481 29481 29485 29485 29488 29495 29498
     29500 29502 29504 29506 29509 29513 29514 29516 29518 29520 29522
     29527 29531 29533 29538 29541 29548 29550 29552 29554 29555 29557
     29560 29562 29579 29582 29582 29586 29591 29597 29597 29599 29602
     29604 29606 29608 29609 29611 29613 29618 29625 29627 29628 29630
     29632 29634 29635 29637 29640 29642 29645 29650 29652 29654 29662
     29664 29664 29667 29667 29669 29669 29671 29675 29677 29678 29684
     29686 29688 29688 29690 29690 29692 29697 29699 29709 29718 29718
     29722 29723 29725 29725 29728 29734 29736 29750 29754 29754 29759
     29762 29764 29764 29766 29766 29770 29771 29773 29778 29780 29781
     29783 29783 29785 29788 29790 29791 29794 29796 29799 29799 29801
     29802 29805 29811 29813 29813 29817 29817 29820 29825 29827 29827
     29829 29835 29840 29840 29842 29842 29844 29845 29847 29848 29850
     29850 29852 29852 29854 29857 29859 29859 29861 29867 29869 29869
     29871 29874 29877 29880 29882 29883 29885 29891 29893 29893 29898
     29899 29903 29903 29908 29926 29928 29929 29932 29932 29934 29934
     29940 29943 29947 29947 29949 29952 29954 29956 29959 29960 29963
     29965 29967 29976 29978 29978 29980 29981 29983 29983 29985 29986
     29989 29990 29992 30003 30007 30010 30013 30016 30023 30024 30027
     30028 30030 30031 30036 30036 30041 30045 30047 30047 30050 30054
     30058 30060 30063 30064 30070 30073 30077 30080 30084 30084 30086
     30087 30090 30092 30095 30097 30100 30101 30104 30106 30109 30109
     30114 30117 30119 30119 30122 30123 30128 30128 30130 30131 30133
     30134 30136 30146 30148 30149 30151 30151 30154 30162 30164 30165
     30167 30171 30173 30180 30182 30183 30189 30189 30191 30209 30211
     30211 30216 30221 30223 30225 30227 30230 30233 30249 30253 30253
     30255 30261 30264 30264 30266 30266 30268 30269 30274 30275 30278
     30281 30284 30284 30288 30288 30290 30291 30294 30298 30300 30300
     30303 30306 30308 30309 30313 30314 30316 30318 30320 30322 30325
     30325 30328 30329 30331 30335 30337 30338 30340 30340 30342 30347
     30350 30351 30354 30355 30357 30358 30361 30366 30372 30372 30374
     30374 30378 30379 30381 30384 30388 30389 30392 30392 30394 30395
     30397 30399 30402 30406 30408 30410 30413 30414 30418 30420 30426
     30431 30433 30433 30435 30439 30441 30442 30444 30453 30455 30460
     30462 30462 30465 30465 30467 30469 30471 30475 30480 30483 30485
     30485 30489 30491 30493 30493 30495 30496 30498 30499 30501 30501
     30503 30505 30509 30509 30511 30511 30513 30526 30532 30535 30538
     30543 30546 30546 30548 30550 30553 30556 30558 30563 30565 30575
     30585 30585 30588 30597 30599 30601 30603 30607 30609 30609 30613
     30613 30615 30615 30617 30627 30629 30629 30631 30632 30634 30637
     30640 30647 30650 30653 30655 30655 30658 30658 30660 30660 30663
     30663 30665 30666 30668 30672 30675 30677 30679 30684 30686 30686
     30688 30688 30690 30691 30693 30693 30695 30697 30700 30707 30711
     30717 30722 30723 30725 30726 30729 30729 30732 30740 30749 30749
     30751 30755 30757 30773 30775 30776 30787 30787 30789 30789 30792
     30794 30796 30798 30800 30800 30802 30802 30812 30814 30816 30816
     30818 30818 30820 30821 30824 30833 30841 30841 30843 30844 30846
     30849 30851 30855 30857 30857 30860 30860 30862 30863 30865 30865
     30867 30874 30878 30885 30887 30893 30896 30896 30898 30900 30906
     30908 30910 30910 30913 30913 30915 30917 30920 30929 30932 30933
     30938 30939 30941 30947 30949 30949 30951 30954 30956 30957 30959
     30959 30962 30964 30967 30967 30969 30975 30977 30978 30980 30981
     30985 30985 30988 30988 30990 30990 30992 30996 30999 30999 31001
     31001 31003 31006 31009 31009 31011 31021 31023 31023 31025 31025
     31029 31029 31032 31034 31037 31042 31044 31052 31055 31063 31066
     31073 31075 31077 31079 31083 31085 31085 31088 31088 31090 31092
     31097 31098 31100 31101 31103 31103 31105 31106 31112 31112 31114
     31115 31117 31120 31122 31128 31130 31132 31136 31138 31140 31140
     31142 31144 31146 31150 31152 31156 31158 31163 31165 31169 31173
     31173 31176 31177 31179 31179 31181 31183 31185 31186 31189 31190
     31192 31192 31196 31200 31203 31204 31206 31207 31209 31214 31222
     31224 31226 31227 31232 31232 31234 31237 31240 31240 31242 31245
     31248 31253 31255 31260 31262 31264 31266 31266 31270 31270 31272
     31272 31275 31275 31278 31281 31287 31287 31289 31289 31291 31293
     31295 31296 31300 31300 31302 31304 31306 31310 31316 31316 31318
     31320 31322 31324 31327 31330 31335 31337 31339 31342 31344 31345
     31348 31350 31352 31355 31358 31361 31364 31372 31375 31376 31378
     31378 31380 31385 31390 31392 31394 31395 31400 31404 31406 31407
     31409 31416 31418 31418 31422 31425 31428 31429 31431 31431 31434
     31435 31441 31441 31448 31449 31455 31456 31459 31462 31467 31467
     31469 31471 31478 31479 31481 31483 31485 31485 31487 31489 31492
     31494 31496 31498 31502 31507 31512 31515 31517 31518 31520 31520
     31522 31526 31528 31528 31530 31541 31544 31544 31547 31547 31552
     31552 31556 31570 31572 31572 31574 31574 31576 31576 31584 31585
     31587 31591 31593 31593 31597 31598 31600 31608 31618 31618 31620
     31621 31623 31624 31626 31633 31636 31641 31643 31645 31648 31649
     31652 31652 31660 31661 31663 31663 31665 31665 31668 31669 31671
     31673 31678 31678 31680 31681 31684 31684 31686 31687 31689 31692
     31694 31694 31700 31701 31704 31723 31728 31732 31735 31737 31739
     31739 31741 31747 31749 31751 31753 31761 31769 31769 31772 31779
     31781 31789 31792 31792 31795 31795 31799 31801 31803 31808 31811
     31811 31813 31813 31815 31818 31820 31821 31824 31824 31827 31828
     31831 31831 31833 31836 31839 31840 31843 31847 31849 31852 31854
     31855 31858 31859 31861 31861 31864 31866 31869 31869 31871 31873
     31876 31877 31880 31882 31884 31885 31889 31890 31892 31896 31902
     31903 31905 31907 31909 31909 31911 31912 31919 31919 31921 31925
     31929 31935 31941 31941 31944 31944 31946 31948 31950 31950 31952
     31954 31956 31959 31961 31961 31964 31968 31970 31970 31975 31976
     31978 31978 31980 31980 31982 31986 31988 31988 31990 31992 31995
     31995 31997 31998 32000 32034 32040 32041 32043 32044 32046 32051
     32053 32054 32056 32071 32074 32074 32078 32086 32088 32088 32091
     32092 32094 32095 32097 32099 32102 32107 32109 32115 32121 32125
     32127 32129 32131 32134 32136 32136 32140 32143 32145 32148 32150
     32150 32156 32163 32166 32167 32169 32170 32172 32178 32180 32181
     32183 32194 32196 32199 32201 32204 32206 32206 32210 32210 32215
     32219 32221 32225 32227 32227 32230 32234 32236 32236 32238 32244
     32246 32247 32249 32251 32259 32259 32264 32279 32282 32293 32297
     32299 32301 32329 32332 32332 32336 32346 32348 32348 32350 32355
     32360 32363 32365 32365 32367 32368 32370 32382 32384 32386 32390
     32392 32394 32397 32399 32399 32401 32401 32403 32412 32566 32566
     32568 32570 32573 32575 32579 32581 32584 32584 32586 32589 32591
     32593 32596 32597 32600 32600 32603 32609 32611 32611 32613 32622
     32624 32624 32626 32627 32629 32631 32633 32639 32643 32643 32645
     32654 32657 32658 32660 32662 32666 32670 32672 32674 32676 32681
     32684 32685 32687 32691 32693 32707 32709 32709 32711 32711 32713
     32722 32724 32725 32727 32727 32731 32732 32734 32739 32741 32742
     32744 32757 32759 32761 32763 32769 32771 32775 32779 32786 32788
     32793 32795 32796 32798 32799 32801 32801 32804 32804 32806 32806
     32808 32810 32812 32812 32816 32816 32819 32823 32825 32825 32829
     32831 32835 32835 32838 32840 32842 32842 32847 32850 32854 32854
     32856 32856 32858 32858 32860 32862 32868 32868 32871 32871 32876
     32876 32879 32883 32885 32889 32893 32895 32898 32898 32900 32903
     32905 32908 32911 32912 32914 32915 32917 32918 32920 32925 32929
     32931 32933 32933 32937 32939 32941 32943 32945 32946 32948 32949
     32952 32952 32954 32954 32962 32965 32967 32970 32972 32977 32980
     32990 32992 32993 32995 32998 33005 33005 33007 33013 33016 33022
     33024 33026 33029 33030 33032 33032 33034 33034 33045 33046 33048
     33049 33051 33051 33053 33055 33057 33061 33063 33063 33065 33065
     33067 33069 33071 33072 33081 33082 33085 33086 33091 33092 33094
     33095 33098 33109 33115 33116 33118 33118 33120 33122 33124 33127
     33129 33129 33131 33131 33134 33140 33142 33146 33151 33152 33154
     33155 33158 33165 33167 33167 33173 33173 33175 33184 33186 33187
     33190 33193 33195 33196 33198 33198 33200 33205 33207 33207 33209
     33216 33218 33223 33225 33226 33228 33229 33231 33234 33237 33237
     33239 33243 33245 33251 33253 33258 33260 33262 33266 33268 33271
     33276 33278 33282 33284 33285 33287 33293 33296 33298 33300 33302
     33307 33314 33317 33317 33320 33320 33322 33324 33327 33327 33330
     33338 33340 33341 33343 33344 33346 33346 33348 33349 33351 33351
     33353 33353 33355 33355 33358 33363 33365 33372 33374 33375 33377
     33377 33379 33380 33382 33382 33384 33385 33387 33391 33393 33394
     33396 33397 33399 33400 33404 33408 33411 33413 33418 33419 33421
     33428 33432 33435 33437 33445 33447 33449 33451 33457 33459 33470
     33472 33472 33474 33475 33489 33495 33497 33497 33499 33500 33502
     33505 33507 33512 33514 33517 33519 33526 33529 33531 33534 33534
     33536 33545 33548 33549 33558 33559 33561 33561 33563 33564 33566
     33566 33568 33568 33570 33570 33572 33581 33583 33583 33585 33596
     33599 33605 33607 33620 33622 33622 33651 33656 33658 33663 33665
     33665 33667 33667 33670 33680 33682 33691 33693 33694 33696 33696
     33698 33707 33710 33712 33725 33725 33727 33740 33742 33743 33745
     33745 33748 33753 33755 33765 33767 33772 33774 33782 33784 33791
     33793 33793 33795 33796 33798 33799 33801 33811 33819 33819 33827
     33827 33833 33833 33835 33837 33839 33853 33855 33856 33858 33863
     33865 33865 33867 33870 33872 33874 33876 33876 33878 33879 33881
     33883 33885 33889 33891 33891 33893 33897 33899 33904 33907 33914
     33917 33918 33922 33922 33926 33926 33933 33937 33940 33940 33943
     33954 33956 33956 33959 33964 33966 33970 33972 33972 33974 33974
     33976 33980 33983 33986 33988 33991 33993 34004 34006 34007 34011
     34011 34023 34028 34030 34036 34038 34039 34041 34048 34050 34050
     34054 34063 34065 34074 34076 34081 34083 34097 34107 34107 34109
     34110 34112 34113 34115 34122 34125 34126 34129 34129 34131 34137
     34139 34139 34141 34142 34144 34158 34161 34161 34165 34172 34174
     34174 34176 34193 34196 34198 34200 34212 34214 34218 34223 34225
     34227 34234 34237 34240 34242 34249 34251 34251 34253 34258 34261
     34261 34263 34266 34268 34271 34273 34278 34280 34285 34287 34290
     34294 34299 34301 34305 34308 34311 34313 34316 34321 34321 34327
     34332 34334 34343 34345 34346 34348 34350 34353 34358 34360 34364
     34366 34368 34371 34371 34374 34376 34379 34382 34384 34384 34386
     34390 34393 34393 34395 34396 34398 34399 34401 34405 34407 34411
     34413 34417 34419 34420 34423 34423 34425 34428 34437 34439 34442
     34446 34448 34449 34451 34458 34460 34462 34465 34469 34471 34474
     34479 34481 34483 34505 34507 34508 34512 34513 34515 34516 34518
     34527 34530 34532 34534 34534 34536 34541 34549 34555 34558 34558
     34560 34574 34577 34579 34584 34588 34590 34590 34592 34602 34604
     34606 34608 34613 34615 34616 34618 34620 34622 34627 34630 34630
     34636 34671 34675 34683 34689 34693 34695 34697 34701 34701 34703
     34708 34710 34712 34714 34719 34722 34724 34730 34736 34738 34752
     34754 34758 34760 34764 34769 34772 34775 34777 34779 34792 34794
     34797 34799 34799 34802 34804 34806 34807 34809 34812 34814 34819
     34821 34822 34824 34829 34832 34833 34835 34839 34841 34841 34843
     34845 34847 34854 34856 34860 34862 34867 34869 34873 34875 34881
     34883 34884 34888 34888 34890 34894 34898 34899 34901 34903 34905
     34907 34909 34909 34913 34915 34919 34923 34925 34925 34927 34930
     34932 34935 34937 34937 34940 34947 34952 34953 34955 34958 34961
     34963 34965 34971 34974 34975 34977 34978 34980 34980 34983 34984
     34986 34988 34993 34994 34998 35002 35004 35006 35008 35010 35017
     35024 35026 35026 35028 35039 35041 35041 35047 35048 35051 35052
     35054 35060 35062 35070 35073 35074 35077 35079 35081 35084 35086
     35086 35088 35098 35102 35103 35105 35107 35109 35111 35113 35123
     35125 35128 35131 35134 35137 35138 35140 35140 35142 35142 35145
     35145 35147 35148 35151 35155 35158 35172 35174 35174 35177 35183
     35185 35188 35190 35190 35193 35196 35198 35199 35201 35203 35205
     35206 35208 35208 35211 35211 35215 35215 35219 35219 35221 35224
     35227 35231 35233 35236 35238 35238 35242 35242 35244 35247 35250
     35250 35254 35255 35257 35258 35261 35264 35282 35286 35289 35293
     35295 35302 35304 35305 35307 35309 35312 35316 35318 35320 35322
     35324 35326 35328 35330 35332 35335 35336 35338 35338 35340 35340
     35342 35347 35349 35352 35355 35355 35357 35359 35362 35363 35365
     35365 35367 35367 35370 35370 35372 35373 35376 35377 35380 35380
     35382 35382 35385 35388 35390 35393 35396 35398 35400 35400 35402
     35402 35404 35410 35412 35417 35419 35419 35422 35422 35424 35427
     35430 35430 35432 35433 35435 35438 35440 35447 35449 35452 35455
     35455 35457 35463 35467 35469 35471 35471 35473 35475 35477 35478
     35480 35482 35486 35486 35488 35489 35491 35496 35498 35499 35504
     35504 35506 35506 35510 35510 35512 35520 35522 35529 35531 35531
     35533 35533 35535 35535 35537 35545 35547 35554 35556 35556 35558
     35560 35563 35563 35565 35576 35578 35580 35582 35586 35588 35592
     35594 35595 35597 35614 35616 35616 35618 35624 35626 35628 35630
     35633 35635 35635 35637 35639 35641 35646 35648 35650 35653 35659
     35662 35674 35676 35677 35679 35680 35683 35683 35685 35688 35690
     35693 35695 35696 35700 35700 35703 35707 35709 35712 35714 35714
     35716 35718 35720 35720 35722 35724 35726 35726 35730 35734 35736
     35738 35740 35740 35742 35743 35895 35895 35897 35897 35899 35903
     35905 35907 35909 35920 35924 35927 35930 35930 35932 35933 35935
     35935 35937 35938 35940 35942 35944 35949 35951 35955 35957 35963
     35965 35965 35968 35970 35972 35974 35977 35978 35980 35981 35983
     35989 35991 35994 35996 35998 36000 36005 36007 36012 36015 36016
     36018 36037 36039 36040 36042 36042 36044 36044 36047 36047 36049
     36051 36053 36053 36055 36055 36057 36058 36060 36072 36074 36074
     36076 36078 36080 36081 36083 36085 36088 36094 36096 36096 36098
     36098 36100 36106 36109 36109 36111 36112 36115 36119 36121 36121
     36123 36123 36196 36196 36198 36201 36203 36208 36210 36212 36214
     36217 36219 36219 36221 36221 36224 36225 36228 36229 36233 36234
     36236 36246 36249 36249 36251 36252 36255 36257 36259 36259 36261
     36261 36263 36264 36266 36271 36274 36279 36281 36282 36284 36284
     36286 36287 36289 36290 36293 36296 36299 36305 36307 36307 36309
     36317 36319 36324 36326 36332 36334 36340 36346 36346 36348 36352
     36354 36359 36361 36362 36365 36365 36367 36391 36393 36393 36395
     36395 36398 36398 36400 36401 36403 36406 36408 36409 36412 36418
     36420 36421 36423 36430 36432 36432 36435 36439 36441 36455 36457
     36458 36460 36461 36463 36463 36466 36468 36470 36470 36472 36472
     36474 36476 36481 36482 36484 36494 36496 36504 36506 36506 36509
     36513 36515 36518 36520 36524 36530 36530 36538 36538 36541 36541
     36544 36544 36546 36546 36553 36557 36559 36559 36561 36564 36567
     36568 36571 36577 36581 36585 36587 36588 36590 36591 36593 36593
     36596 36604 36606 36611 36613 36619 36621 36622 36624 36632 36634
     36640 36643 36646 36649 36650 36652 36652 36654 36655 36658 36665
     36667 36667 36670 36672 36674 36681 36683 36683 36685 36699 36701
     36708 36763 36764 36767 36767 36771 36771 36774 36774 36776 36776
     36781 36786 36788 36788 36799 36799 36802 36802 36804 36806 36809
     36809 36811 36811 36813 36814 36817 36823 36832 36838 36840 36840
     36842 36843 36845 36846 36848 36848 36852 36856 36858 36870 36875
     36877 36879 36881 36884 36887 36889 36900 36909 36911 36913 36914
     36916 36918 36920 36920 36924 36927 36929 36930 36932 36932 36935
     36935 36937 36939 36941 36949 36952 36953 36955 36958 36960 36960
     36962 36963 36967 36969 36971 36971 36973 36976 36978 37000 37002
     37003 37005 37005 37007 37009 37012 37013 37015 37017 37019 37019
     37022 37027 37029 37031 37034 37034 37039 37046 37048 37048 37053
     37055 37057 37057 37059 37059 37061 37061 37063 37064 37066 37067
     37070 37070 37076 37085 37087 37093 37096 37101 37103 37109 37113
     37129 37131 37131 37133 37138 37140 37140 37142 37156 37158 37174
     37176 37179 37182 37185 37187 37200 37202 37203 37205 37208 37210
     37210 37215 37221 37224 37226 37228 37228 37230 37231 37234 37237
     37239 37242 37248 37255 37257 37259 37261 37261 37263 37267 37273
     37283 37285 37285 37287 37288 37290 37301 37303 37303 37305 37306
     37308 37310 37312 37315 37317 37319 37321 37321 37323 37329 37331
     37333 37335 37338 37340 37341 37346 37348 37350 37358 37361 37361
     37363 37365 37367 37369 37373 37373 37375 37383 37385 37386 37388
     37389 37391 37394 37396 37399 37401 37402 37404 37404 37406 37406
     37411 37415 37421 37422 37424 37428 37430 37434 37437 37440 37445
     37446 37448 37460 37462 37463 37466 37467 37470 37470 37472 37473
     37475 37479 37484 37485 37487 37488 37490 37490 37494 37494 37496
     37504 37506 37507 37509 37512 37514 37518 37521 37521 37523 37533
     37536 37548 37554 37559 37561 37561 37563 37564 37568 37587 37589
     37589 37591 37593 37597 37601 37604 37604 37606 37610 37614 37617
     37623 37628 37630 37634 37636 37636 37638 37638 37640 37641 37643
     37648 37650 37654 37656 37659 37661 37675 37677 37679 37683 37686
     37688 37689 37692 37692 37702 37703 37705 37714 37716 37724 37726
     37726 37728 37729 37731 37733 37735 37735 37738 37738 37740 37741
     37744 37745 37749 37751 37753 37756 37758 37758 37760 37760 37762
     37763 37768 37770 37772 37775 37777 37778 37780 37787 37789 37791
     37793 37802 37804 37804 37806 37813 37815 37815 37824 37824 37826
     37828 37831 37832 37834 37834 37836 37842 37844 37850 37852 37855
     37857 37860 37862 37864 37868 37868 37870 37870 37877 37888 37891
     37891 37894 37895 37897 37910 37912 37913 37920 37920 37928 37932
     37934 37934 37936 37939 37941 37949 37951 37952 37956 37964 37967
     37970 37973 37973 37975 37975 37981 37982 37984 37984 37986 37988
     37992 37995 37997 38008 38012 38019 38263 38263 38266 38269 38272
     38272 38274 38275 38278 38278 38280 38281 38283 38292 38296 38296
     38299 38300 38302 38303 38305 38305 38307 38309 38312 38313 38315
     38318 38320 38321 38325 38327 38329 38336 38339 38339 38341 38349
     38352 38358 38362 38364 38366 38373 38428 38428 38430 38430 38432
     38436 38440 38440 38442 38442 38444 38450 38457 38461 38463 38464
     38466 38468 38474 38481 38483 38484 38488 38488 38491 38495 38497
     38500 38506 38509 38511 38520 38524 38526 38528 38528 38531 38539
     38541 38542 38545 38549 38551 38553 38555 38556 38558 38558 38561
     38562 38564 38564 38567 38570 38572 38572 38574 38574 38576 38577
     38579 38580 38584 38585 38587 38588 38591 38606 38610 38623 38625
     38627 38629 38629 38632 38634 38639 38642 38645 38651 38653 38653
     38655 38656 38658 38658 38660 38665 38667 38667 38669 38675 38678
     38678 38680 38681 38684 38688 38690 38700 38702 38704 38706 38706
     38709 38709 38712 38714 38717 38719 38722 38724 38726 38729 38731
     38731 38738 38738 38742 38742 38744 38744 38746 38748 38750 38750
     38752 38754 38758 38758 38760 38762 38764 38764 38766 38766 38768
     38768 38770 38772 38774 38776 38778 38789 38792 38792 38794 38795
     38797 38799 38804 38804 38807 38810 38812 38814 38816 38822 38824
     38824 38826 38830 38835 38835 38838 38839 38841 38841 38843 38843
     38847 38847 38849 38849 38851 38855 38857 38857 38859 38864 38867
     38873 38876 38879 38881 38881 38883 38883 38885 38885 38893 38893
     38896 38897 38899 38899 38902 38902 38904 38907 38909 38920 38922
     38922 38924 38931 38934 38936 38939 38942 38944 38945 38948 38948
     38950 38953 38955 38955 38957 38957 38959 38960 38962 38962 38965
     38965 38967 38969 38971 38971 38977 38977 38979 38982 38984 38986
     38988 38995 38999 39001 39003 39008 39010 39013 39015 39015 39017
     39019 39023 39028 39080 39081 39084 39087 39089 39091 39094 39094
     39096 39096 39098 39106 39108 39108 39110 39110 39113 39113 39115
     39116 39131 39131 39135 39135 39138 39139 39141 39141 39143 39143
     39145 39147 39149 39149 39151 39151 39154 39154 39156 39156 39158
     39158 39161 39162 39164 39166 39168 39168 39170 39171 39173 39173
     39175 39178 39180 39180 39184 39192 39194 39195 39198 39199 39201
     39201 39204 39205 39207 39219 39221 39221 39226 39226 39228 39231
     39233 39233 39235 39235 39237 39237 39239 39241 39243 39244 39246
     39246 39248 39257 39259 39260 39262 39263 39265 39265 39318 39321
     39324 39326 39329 39329 39331 39331 39333 39336 39339 39349 39353
     39355 39357 39357 39361 39363 39367 39367 39369 39369 39371 39385
     39387 39389 39391 39391 39394 39397 39399 39399 39401 39402 39404
     39406 39408 39409 39412 39412 39414 39423 39425 39431 39433 39435
     39437 39439 39441 39441 39444 39446 39449 39454 39456 39456 39458
     39461 39463 39463 39465 39470 39472 39474 39476 39482 39485 39494
     39496 39498 39500 39504 39506 39511 39513 39515 39518 39520 39522
     39522 39524 39531 39592 39592 39595 39595 39597 39597 39599 39601
     39603 39604 39607 39609 39611 39612 39614 39618 39622 39623 39626
     39626 39629 39629 39631 39638 39640 39640 39644 39644 39647 39647
     39649 39649 39651 39651 39654 39655 39659 39663 39665 39667 39670
     39671 39673 39678 39681 39681 39683 39686 39688 39688 39690 39694
     39696 39698 39701 39706 39710 39712 39714 39717 39719 39721 39723
     39723 39726 39727 39729 39731 39733 39733 39735 39735 39738 39740
     39742 39743 39745 39750 39752 39752 39754 39759 39761 39762 39764
     39766 39768 39771 39775 39777 39780 39780 39782 39784 39788 39788
     39791 39793 39796 39799 39802 39806 39808 39808 39810 39810 39813
     39816 39824 39827 39829 39829 39834 39835 39838 39838 39840 39842
     39844 39846 39848 39848 39850 39851 39853 39855 39861 39862 39864
     39865 39869 39869 39871 39873 39875 39876 39878 39882 39891 39895
     39897 39900 39902 39902 39904 39906 39908 39912 39914 39916 39920
     39920 39927 39928 39933 39933 39941 39945 39947 39947 39949 39950
     39954 39956 39959 39959 39964 39965 39969 39969 39971 39973 39976
     39977 39979 39981 39985 39988 39990 39991 39993 39993 39995 40001
     40004 40004 40006 40006 40008 40014 40016 40016 40018 40018 40020
     40025 40030 40032 40034 40035 40038 40040 40045 40046 40049 40049
     40051 40053 40055 40058 40165 40167 40169 40170 40173 40173 40177
     40183 40185 40189 40191 40192 40195 40201 40208 40208 40210 40210
     40212 40213 40215 40217 40219 40219 40221 40224 40226 40227 40229
     40230 40232 40233 40237 40241 40243 40243 40246 40248 40251 40251
     40253 40259 40261 40261 40266 40268 40271 40271 40273 40276 40278
     40285 40287 40289 40295 40300 40303 40309 40311 40313 40315 40315
     40317 40317 40319 40322 40324 40332 40336 40336 40338 40338 40340
     40340 40342 40356 40358 40362 40364 40365 40367 40367 40369 40380
     40382 40383 40385 40387 40389 40389 40391 40392 40394 40403 40405
     40415 40417 40422 40424 40425 40427 40432 40434 40443 40445 40455
     40457 40457 40459 40459 40461 40461 40463 40469 40471 40471 40473
     40475 40477 40478 40565 40565 40569 40570 40572 40573 40575 40576
     40578 40579 40582 40590 40593 40596 40599 40599 40601 40605 40607
     40609 40612 40613 40615 40615 40617 40617 40621 40622 40624 40624
     40628 40631 40635 40636 40638 40638 40640 40640 40642 40643 40648
     40648 40652 40657 40659 40662 40664 40664 40666 40672 40676 40680
     40683 40683 40685 40688 40690 40695 40697 40701 40703 40705 40710
     40711 40713 40714 40718 40720 40722 40723 40725 40726 40728 40732
     40734 40734 40736 40736 40738 40741 40744 40760 40763 40763 40765
     40766 40768 40771 40774 40783 40786 40786 40788 40793 40795 40801
     40803 40807 40810 40812 40814 40818 40820 40827 40830 40830 40845
     40845 40848 40850 40852 40853 40856 40856 40860 40860 40864 40864
     40866 40866 40868 40868 63153 63560 64012 64013 65072 65073 65075
     65092 65097 65106 65108 65111 65113 65126 65128 65131 65281 65281
     65283 65286 65288 65338 65340 65340 65343 65343 65345 65374 65504
     65505 65507 65507 65509 65509 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 2257 :NAME "windows-1257" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1257) [Lazhintseva]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 160 160 162 164 166 169 171 185 187 190 196 198 201 201 211
     211 213 216 220 220 223 223 228 230 233 233 243 243 245 248 252 252
     256 257 260 263 268 269 274 275 278 281 290 291 298 299 302 303 310
     311 315 316 321 326 332 333 342 343 346 347 352 353 362 363 370 371
     377 382 711 711 729 729 731 731 8211 8212 8216 8218 8220 8222 8224
     8226 8230 8230 8240 8240 8249 8250 8364 8364 8482 8482))
  (MAKE-CHARACTER-SET :MIB-ENUM 104 :NAME "ISO-2022-CN" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE '"RFC-1922" :COMMENTS 'NIL :REFERENCES
   '("[RFC1922]") :RANGES
   #(0 127 164 164 167 168 176 177 183 183 215 215 224 225 232 234 236
     237 242 243 247 247 249 250 252 252 257 257 275 275 283 283 299 299
     333 333 363 363 462 462 464 464 466 466 468 468 470 470 472 472 474
     474 476 476 711 711 713 715 729 729 913 929 931 937 945 961 963 969
     1025 1025 1040 1103 1105 1105 8211 8214 8216 8217 8220 8221 8229
     8230 8240 8240 8242 8243 8245 8245 8251 8251 8254 8254 8451 8451
     8453 8453 8457 8457 8470 8470 8544 8555 8560 8569 8592 8595 8598
     8601 8712 8712 8719 8719 8721 8721 8725 8725 8730 8730 8733 8736
     8741 8741 8743 8747 8750 8750 8756 8759 8764 8765 8776 8776 8780
     8780 8786 8786 8800 8801 8804 8807 8814 8815 8857 8857 8869 8869
     8895 8895 8978 8978 9216 9247 9249 9249 9312 9321 9332 9371 9472
     9547 9552 9552 9566 9566 9569 9569 9578 9578 9581 9587 9601 9615
     9620 9621 9632 9633 9650 9651 9660 9661 9670 9671 9675 9675 9678
     9679 9698 9701 9733 9734 9737 9737 9792 9794 12288 12291 12293
     12293 12296 12311 12317 12318 12321 12329 12353 12435 12449 12534
     12539 12539 12549 12585 12832 12841 12963 12963 13198 13199 13212
     13214 13217 13217 13252 13252 13262 13262 13265 13266 13269 13269
     19968 19969 19971 19971 19975 19985 19987 19990 19992 19999 20002
     20002 20004 20008 20010 20014 20016 20020 20022 20022 20024 20031
     20034 20035 20037 20037 20039 20041 20043 20048 20050 20052 20054
     20054 20056 20057 20060 20065 20070 20070 20073 20073 20080 20081
     20083 20083 20094 20095 20098 20100 20102 20102 20104 20105 20107
     20111 20113 20117 20120 20123 20126 20130 20132 20137 20139 20142
     20146 20147 20149 20150 20153 20155 20159 20171 20173 20174 20177
     20177 20179 20186 20188 20191 20193 20193 20195 20197 20200 20204
     20208 20215 20219 20219 20221 20221 20223 20226 20228 20229 20232
     20235 20237 20245 20247 20251 20253 20256 20258 20258 20260 20263
     20266 20269 20271 20272 20274 20276 20278 20278 20280 20280 20282
     20287 20289 20289 20291 20291 20294 20297 20300 20325 20327 20327
     20329 20332 20334 20336 20339 20361 20363 20363 20365 20365 20367
     20370 20372 20376 20378 20382 20384 20384 20387 20387 20389 20394
     20396 20396 20398 20399 20402 20403 20405 20407 20409 20411 20415
     20421 20423 20423 20425 20427 20429 20433 20435 20436 20438 20449
     20451 20451 20454 20454 20456 20458 20460 20463 20465 20465 20467
     20472 20474 20474 20478 20478 20480 20480 20485 20487 20489 20489
     20491 20495 20497 20508 20510 20515 20517 20529 20531 20531 20533
     20533 20535 20535 20538 20538 20540 20540 20542 20542 20544 20545
     20547 20547 20549 20559 20561 20561 20563 20563 20565 20565 20567
     20567 20570 20581 20584 20592 20594 20599 20602 20603 20605 20608
     20610 20611 20613 20613 20615 20616 20619 20622 20625 20626 20628
     20630 20632 20638 20642 20643 20645 20645 20647 20649 20652 20664
     20666 20667 20669 20671 20673 20674 20676 20683 20686 20687 20689
     20689 20691 20695 20698 20699 20701 20701 20704 20704 20707 20714
     20716 20721 20723 20723 20725 20726 20728 20729 20731 20731 20733
     20736 20738 20748 20752 20757 20759 20760 20762 20762 20764 20764
     20767 20770 20772 20774 20777 20778 20781 20782 20784 20789 20791
     20797 20799 20801 20803 20809 20811 20813 20817 20818 20820 20823
     20825 20831 20833 20835 20837 20837 20839 20841 20843 20846 20848
     20849 20851 20857 20859 20861 20864 20866 20869 20869 20871 20874
     20876 20877 20879 20879 20881 20889 20891 20892 20894 20894 20896
     20896 20898 20898 20900 20901 20906 20908 20911 20915 20917 20919
     20921 20921 20923 20926 20928 20928 20932 20945 20948 20948 20951
     20952 20955 20958 20960 20961 20964 20964 20971 20971 20973 20973
     20975 20977 20979 20979 20981 20982 20984 20989 20991 20995 20998
     21002 21004 21006 21008 21011 21014 21022 21024 21025 21028 21029
     21032 21035 21037 21038 21040 21043 21045 21051 21053 21053 21055
     21060 21062 21063 21065 21070 21072 21074 21076 21078 21082 21087
     21089 21090 21093 21093 21095 21095 21097 21103 21106 21106 21108
     21109 21111 21112 21114 21117 21119 21124 21127 21133 21136 21137
     21139 21139 21142 21145 21147 21147 21149 21155 21158 21158 21160
     21166 21169 21171 21179 21180 21182 21187 21191 21191 21193 21193
     21195 21195 21197 21197 21200 21200 21202 21203 21205 21209 21211
     21211 21213 21215 21218 21220 21222 21222 21225 21225 21227 21227
     21231 21233 21235 21237 21239 21244 21246 21248 21253 21254 21256
     21259 21261 21266 21269 21271 21273 21274 21276 21277 21279 21283
     21286 21286 21290 21290 21293 21297 21300 21300 21303 21303 21305
     21313 21315 21317 21319 21322 21324 21327 21329 21335 21338 21338
     21340 21340 21342 21348 21350 21351 21353 21353 21355 21356 21358
     21365 21367 21372 21375 21375 21378 21378 21380 21382 21385 21391
     21394 21394 21396 21402 21404 21407 21410 21417 21420 21422 21426
     21426 21428 21428 21430 21430 21433 21433 21435 21435 21439 21439
     21441 21443 21448 21453 21457 21457 21460 21460 21462 21465 21467
     21467 21471 21478 21480 21491 21493 21497 21499 21501 21505 21505
     21507 21508 21510 21523 21525 21529 21531 21537 21539 21561 21563
     21566 21568 21571 21573 21576 21578 21579 21582 21584 21586 21593
     21595 21596 21600 21608 21611 21612 21615 21624 21626 21634 21636
     21636 21638 21640 21643 21650 21652 21654 21656 21659 21661 21661
     21664 21681 21683 21684 21686 21688 21690 21705 21708 21719 21721
     21722 21724 21730 21732 21739 21741 21742 21745 21747 21751 21752
     21754 21757 21759 21759 21761 21761 21763 21772 21774 21778 21780
     21780 21783 21783 21786 21787 21792 21792 21794 21796 21798 21799
     21802 21802 21804 21817 21819 21820 21822 21825 21827 21830 21832
     21835 21837 21842 21845 21847 21852 21852 21854 21855 21857 21863
     21866 21866 21868 21870 21877 21880 21883 21892 21895 21903 21905
     21909 21912 21914 21916 21917 21919 21919 21921 21928 21930 21934
     21937 21939 21941 21941 21943 21943 21945 21945 21947 21947 21949
     21952 21954 21974 21977 21981 21983 21983 21985 21996 21999 21999
     22002 22003 22005 22007 22009 22010 22012 22018 22020 22020 22022
     22022 22024 22025 22028 22032 22034 22040 22043 22047 22051 22052
     22055 22055 22057 22058 22060 22070 22072 22075 22077 22082 22085
     22086 22088 22090 22092 22094 22099 22100 22103 22106 22108 22108
     22110 22110 22112 22112 22114 22118 22120 22132 22134 22137 22139
     22140 22142 22151 22156 22160 22163 22163 22165 22165 22167 22170
     22172 22173 22179 22179 22181 22184 22186 22191 22194 22199 22204
     22206 22208 22211 22213 22214 22216 22221 22225 22225 22227 22228
     22231 22231 22234 22235 22237 22242 22244 22245 22247 22247 22250
     22251 22253 22254 22256 22257 22260 22261 22263 22263 22265 22266
     22269 22271 22273 22276 22278 22285 22290 22292 22294 22294 22296
     22296 22298 22300 22302 22304 22306 22307 22312 22314 22316 22320
     22323 22324 22329 22331 22334 22334 22336 22338 22341 22341 22343
     22343 22345 22354 22359 22359 22362 22370 22372 22372 22374 22374
     22376 22379 22381 22381 22383 22391 22395 22397 22400 22400 22402
     22406 22411 22412 22415 22415 22418 22421 22423 22427 22429 22439
     22441 22441 22443 22443 22445 22446 22450 22450 22452 22454 22456
     22458 22460 22461 22463 22463 22465 22467 22470 22471 22475 22476
     22478 22480 22482 22482 22484 22485 22488 22490 22492 22493 22495
     22501 22503 22503 22505 22505 22508 22525 22528 22530 22532 22542
     22544 22545 22548 22549 22553 22553 22555 22558 22560 22561 22563
     22565 22567 22570 22572 22585 22587 22587 22589 22589 22591 22591
     22596 22596 22600 22607 22609 22613 22615 22619 22621 22622 22626
     22629 22632 22632 22635 22637 22639 22639 22641 22641 22644 22646
     22649 22659 22661 22667 22670 22676 22680 22682 22684 22689 22691
     22691 22693 22694 22696 22697 22699 22700 22702 22703 22705 22705
     22707 22707 22714 22714 22716 22719 22721 22722 22725 22729 22734
     22735 22737 22742 22744 22747 22749 22751 22754 22756 22759 22761
     22763 22764 22766 22768 22771 22772 22774 22774 22777 22778 22780
     22783 22786 22788 22790 22791 22796 22799 22802 22802 22804 22807
     22809 22810 22812 22812 22815 22816 22818 22818 22820 22821 22823
     22823 22825 22831 22833 22833 22836 22836 22839 22842 22844 22844
     22846 22846 22848 22850 22852 22853 22855 22859 22862 22865 22867
     22872 22874 22874 22876 22876 22880 22882 22885 22885 22887 22887
     22889 22891 22893 22894 22896 22900 22902 22905 22907 22920 22922
     22922 22925 22928 22930 22931 22934 22937 22941 22942 22944 22955
     22958 22959 22961 22966 22969 22974 22976 22977 22979 22979 22981
     22984 22986 22996 22998 23000 23002 23006 23008 23009 23011 23014
     23016 23018 23020 23022 23025 23031 23033 23041 23043 23050 23052
     23052 23055 23055 23057 23057 23059 23059 23061 23065 23067 23068
     23070 23072 23075 23075 23077 23077 23081 23081 23085 23086 23089
     23097 23100 23100 23102 23102 23104 23108 23110 23114 23116 23117
     23120 23123 23125 23128 23130 23136 23138 23138 23140 23143 23145
     23146 23148 23149 23152 23152 23156 23160 23162 23165 23167 23167
     23171 23172 23178 23180 23182 23184 23186 23189 23191 23191 23194
     23199 23202 23202 23205 23207 23209 23210 23212 23212 23214 23234
     23236 23236 23238 23245 23250 23250 23252 23267 23269 23270 23272
     23278 23281 23281 23283 23289 23291 23291 23293 23293 23295 23295
     23297 23299 23301 23301 23303 23305 23307 23308 23311 23312 23315
     23316 23318 23319 23321 23323 23325 23326 23328 23329 23331 23336
     23338 23338 23340 23344 23346 23346 23348 23348 23351 23352 23356
     23360 23363 23363 23365 23365 23367 23368 23371 23374 23376 23377
     23379 23389 23391 23391 23394 23398 23401 23404 23406 23406 23408
     23411 23413 23413 23415 23416 23418 23419 23421 23421 23423 23425
     23427 23429 23431 23433 23435 23436 23438 23439 23442 23443 23445
     23445 23447 23454 23456 23464 23466 23470 23472 23472 23475 23478
     23480 23481 23485 23490 23492 23495 23498 23502 23504 23508 23510
     23510 23512 23513 23517 23532 23534 23538 23541 23542 23544 23548
     23551 23551 23553 23553 23555 23556 23558 23570 23572 23574 23576
     23576 23578 23578 23580 23581 23583 23583 23586 23586 23588 23589
     23591 23592 23594 23594 23596 23596 23600 23601 23603 23604 23607
     23618 23620 23633 23636 23638 23640 23641 23644 23646 23648 23658
     23660 23663 23665 23665 23667 23668 23673 23676 23678 23679 23681
     23682 23686 23686 23688 23693 23695 23709 23711 23729 23731 23731
     23733 23736 23741 23741 23743 23743 23745 23745 23748 23748 23750
     23756 23758 23760 23762 23764 23766 23771 23774 23775 23777 23777
     23780 23782 23784 23784 23786 23786 23788 23790 23792 23793 23796
     23796 23798 23801 23803 23803 23805 23805 23807 23811 23814 23815
     23819 23823 23825 23826 23828 23828 23830 23831 23833 23835 23837
     23840 23842 23849 23853 23854 23856 23866 23868 23875 23877 23877
     23879 23879 23881 23884 23886 23886 23888 23890 23893 23893 23896
     23897 23899 23899 23901 23902 23906 23907 23909 23909 23911 23913
     23915 23916 23919 23919 23921 23922 23924 23924 23927 23927 23929
     23930 23932 23938 23940 23940 23942 23946 23949 23949 23954 23957
     23959 23959 23961 23962 23964 23970 23975 23978 23980 23986 23988
     23989 23991 23992 23994 23994 23996 23997 24000 24000 24002 24003
     24005 24007 24009 24009 24011 24011 24013 24013 24015 24015 24017
     24018 24020 24022 24024 24024 24027 24027 24029 24034 24037 24041
     24043 24043 24046 24052 24055 24055 24057 24057 24061 24063 24065
     24070 24072 24072 24074 24074 24076 24076 24078 24081 24084 24093
     24095 24105 24107 24107 24109 24110 24113 24113 24115 24116 24118
     24120 24123 24126 24128 24133 24138 24143 24147 24149 24151 24153
     24155 24163 24166 24176 24178 24182 24184 24192 24194 24194 24196
     24196 24198 24205 24207 24209 24211 24215 24217 24220 24222 24224
     24226 24232 24234 24238 24240 24249 24254 24254 24257 24258 24260
     24268 24270 24270 24273 24291 24293 24298 24300 24300 24302 24303
     24305 24308 24310 24311 24314 24314 24318 24325 24327 24328 24330
     24331 24335 24335 24337 24341 24343 24344 24346 24347 24349 24349
     24351 24352 24354 24362 24365 24369 24371 24371 24373 24378 24380
     24380 24384 24384 24387 24388 24390 24390 24392 24396 24398 24400
     24402 24409 24413 24414 24417 24418 24420 24423 24425 24429 24431
     24433 24435 24436 24438 24441 24443 24450 24452 24460 24464 24466
     24469 24473 24475 24476 24478 24481 24485 24486 24488 24495 24498
     24498 24501 24503 24505 24505 24507 24513 24515 24518 24521 24521
     24524 24525 24527 24530 24532 24537 24541 24542 24544 24545 24547
     24549 24551 24552 24554 24555 24557 24559 24561 24561 24563 24565
     24567 24568 24570 24571 24573 24582 24585 24599 24601 24606 24608
     24610 24612 24623 24626 24629 24631 24631 24633 24633 24635 24636
     24639 24647 24649 24649 24651 24653 24656 24656 24658 24661 24664
     24667 24669 24671 24674 24688 24690 24691 24694 24694 24696 24701
     24703 24705 24707 24714 24716 24718 24720 24720 24722 24722 24724
     24727 24730 24733 24735 24736 24738 24739 24742 24742 24744 24744
     24747 24749 24751 24754 24756 24769 24771 24783 24785 24785 24787
     24789 24792 24797 24799 24802 24804 24804 24806 24809 24811 24828
     24830 24833 24835 24838 24840 24843 24845 24848 24850 24854 24856
     24856 24858 24861 24863 24864 24867 24868 24870 24873 24875 24876
     24878 24879 24882 24882 24884 24884 24886 24887 24891 24891 24894
     24897 24900 24911 24913 24918 24920 24920 24922 24923 24925 24927
     24929 24931 24933 24936 24938 24940 24942 24942 24944 24951 24953
     24954 24956 24956 24958 24958 24960 24960 24962 24963 24969 24974
     24976 24980 24982 24982 24986 24987 24989 24989 24991 24991 24993
     24994 24996 24996 24999 25016 25018 25018 25020 25020 25022 25023
     25025 25027 25029 25037 25041 25042 25044 25044 25046 25046 25048
     25048 25054 25056 25059 25067 25069 25070 25072 25074 25077 25089
     25091 25092 25094 25106 25108 25115 25119 25125 25127 25127 25129
     25134 25136 25136 25138 25140 25142 25143 25146 25146 25149 25155
     25158 25166 25168 25172 25176 25180 25182 25182 25184 25191 25193
     25204 25206 25207 25209 25217 25219 25220 25222 25226 25228 25228
     25230 25231 25233 25240 25242 25243 25247 25250 25252 25253 25256
     25265 25267 25270 25272 25273 25275 25279 25282 25282 25284 25300
     25302 25308 25311 25311 25314 25315 25317 25321 25323 25347 25351
     25353 25355 25361 25363 25366 25370 25371 25373 25381 25384 25389
     25391 25391 25394 25396 25398 25398 25400 25406 25408 25425 25428
     25434 25438 25439 25441 25443 25445 25445 25447 25449 25451 25451
     25453 25458 25461 25464 25466 25469 25472 25477 25479 25482 25484
     25490 25492 25492 25494 25497 25499 25509 25511 25521 25523 25524
     25527 25528 25530 25530 25532 25534 25536 25536 25538 25552 25554
     25555 25557 25569 25571 25573 25575 25579 25581 25590 25592 25593
     25597 25597 25599 25602 25605 25606 25609 25616 25618 25624 25626
     25628 25630 25640 25642 25648 25651 25655 25657 25658 25661 25665
     25667 25672 25674 25675 25677 25678 25680 25684 25688 25689 25691
     25697 25701 25705 25707 25712 25714 25723 25725 25725 25727 25727
     25730 25730 25732 25733 25735 25740 25743 25747 25749 25754 25756
     25760 25762 25766 25769 25769 25771 25774 25776 25779 25781 25781
     25783 25784 25786 25797 25799 25799 25801 25803 25805 25808 25810
     25810 25812 25812 25814 25819 25822 25822 25824 25824 25826 25828
     25830 25830 25832 25833 25835 25837 25839 25844 25847 25848 25850
     25857 25859 25860 25862 25863 25865 25865 25868 25872 25874 25881
     25883 25885 25888 25894 25897 25903 25906 25913 25915 25915 25917
     25919 25921 25921 25923 25923 25925 25926 25928 25930 25932 25932
     25935 25935 25937 25937 25939 25945 25947 25950 25954 25960 25962
     25964 25967 25968 25970 25980 25983 25988 25991 25991 25995 25996
     26000 26007 26009 26009 26011 26018 26020 26021 26023 26032 26034
     26035 26038 26041 26043 26045 26047 26047 26049 26054 26059 26064
     26066 26067 26070 26071 26074 26075 26077 26082 26085 26089 26092
     26103 26106 26109 26112 26112 26114 26133 26137 26137 26140 26141
     26143 26146 26148 26152 26155 26155 26157 26159 26161 26166 26169
     26170 26172 26172 26174 26174 26177 26179 26181 26181 26183 26183
     26185 26188 26191 26191 26193 26199 26201 26207 26209 26210 26212
     26214 26216 26216 26218 26218 26220 26220 26222 26226 26228 26228
     26230 26236 26238 26238 26240 26240 26242 26242 26244 26244 26246
     26253 26256 26257 26260 26265 26269 26269 26271 26274 26279 26283
     26286 26290 26292 26293 26295 26299 26301 26302 26304 26304 26308
     26308 26310 26316 26319 26319 26322 26322 26326 26326 26328 26334
     26336 26336 26339 26340 26342 26342 26344 26345 26347 26350 26352
     26352 26354 26356 26358 26361 26364 26364 26366 26369 26371 26373
     26376 26379 26381 26381 26383 26384 26386 26389 26391 26392 26395
     26395 26397 26397 26399 26403 26406 26408 26410 26415 26417 26417
     26419 26421 26424 26432 26434 26435 26437 26441 26443 26449 26451
     26451 26453 26455 26457 26458 26460 26465 26469 26469 26472 26474
     26476 26477 26479 26495 26497 26497 26499 26503 26505 26505 26507
     26510 26512 26517 26519 26522 26524 26527 26530 26531 26533 26533
     26535 26536 26538 26539 26541 26544 26546 26555 26560 26566 26568
     26580 26584 26592 26594 26599 26601 26616 26618 26618 26620 26621
     26623 26624 26629 26629 26631 26636 26638 26639 26641 26644 26646
     26648 26650 26650 26652 26653 26655 26657 26661 26662 26664 26667
     26669 26671 26673 26677 26679 26686 26688 26694 26696 26705 26707
     26709 26720 26729 26731 26731 26733 26735 26737 26738 26740 26745
     26747 26755 26757 26759 26761 26764 26767 26772 26774 26775 26779
     26781 26783 26788 26790 26805 26816 26816 26818 26818 26820 26820
     26822 26825 26827 26830 26832 26840 26842 26842 26844 26849 26851
     26852 26854 26860 26862 26877 26881 26881 26884 26888 26890 26901
     26903 26903 26911 26912 26916 26917 26922 26922 26925 26925 26927
     26928 26930 26933 26935 26937 26939 26941 26943 26946 26948 26949
     26952 26956 26958 26959 26961 26964 26966 26976 26978 26979 26981
     26982 26984 26993 26996 27004 27008 27008 27010 27012 27014 27017
     27021 27022 27024 27025 27027 27036 27038 27038 27040 27041 27043
     27057 27059 27063 27065 27065 27067 27071 27073 27076 27078 27078
     27081 27088 27091 27092 27097 27097 27099 27099 27103 27104 27106
     27106 27108 27112 27115 27118 27121 27124 27126 27128 27131 27138
     27140 27146 27149 27149 27151 27151 27153 27153 27155 27161 27163
     27163 27165 27169 27171 27171 27173 27176 27178 27178 27183 27183
     27185 27186 27188 27189 27192 27201 27204 27204 27206 27209 27211
     27211 27213 27217 27220 27222 27224 27227 27229 27234 27236 27243
     27245 27245 27247 27247 27249 27249 27254 27254 27257 27257 27260
     27260 27262 27265 27267 27269 27271 27271 27273 27273 27276 27278
     27280 27287 27290 27292 27294 27302 27304 27305 27307 27311 27315
     27316 27318 27323 27325 27325 27330 27331 27333 27335 27339 27341
     27343 27345 27347 27347 27353 27361 27365 27365 27367 27368 27370
     27372 27374 27377 27379 27379 27384 27388 27392 27392 27394 27396
     27400 27403 27407 27411 27414 27418 27422 27422 27424 27429 27431
     27432 27436 27437 27439 27439 27441 27444 27446 27455 27457 27459
     27461 27470 27472 27473 27476 27478 27481 27481 27483 27484 27486
     27495 27498 27498 27501 27501 27506 27506 27510 27513 27515 27516
     27518 27524 27526 27535 27537 27547 27550 27559 27562 27563 27565
     27568 27570 27575 27578 27578 27580 27581 27583 27597 27599 27600
     27602 27611 27614 27614 27616 27620 27622 27624 27626 27628 27631
     27632 27634 27635 27637 27637 27639 27641 27643 27657 27659 27661
     27663 27665 27667 27670 27672 27675 27677 27677 27679 27692 27694
     27696 27698 27702 27704 27704 27707 27707 27709 27715 27718 27719
     27721 27728 27730 27730 27732 27733 27735 27735 27737 27737 27739
     27745 27748 27755 27757 27757 27759 27764 27766 27766 27768 27771
     27773 27774 27776 27792 27794 27798 27800 27805 27807 27807 27809
     27809 27811 27815 27817 27822 27824 27828 27830 27847 27849 27850
     27852 27853 27855 27863 27865 27870 27872 27875 27877 27877 27879
     27891 27893 27902 27904 27905 27907 27908 27911 27922 27926 27931
     27933 27936 27938 27938 27941 27941 27943 27971 27973 27976 27978
     27979 27981 27983 27985 27988 27992 27994 27996 27996 27998 28010
     28012 28016 28020 28032 28034 28046 28048 28053 28055 28056 28059
     28059 28061 28065 28067 28068 28070 28076 28078 28079 28082 28085
     28087 28088 28090 28096 28098 28098 28100 28109 28111 28134 28136
     28151 28153 28157 28160 28160 28163 28163 28165 28165 28170 28170
     28172 28174 28176 28177 28180 28180 28182 28183 28185 28189 28191
     28201 28203 28214 28216 28225 28227 28231 28233 28235 28237 28238
     28241 28246 28248 28248 28250 28265 28267 28267 28270 28271 28273
     28276 28279 28281 28286 28287 28291 28291 28293 28294 28296 28297
     28301 28304 28306 28308 28310 28313 28315 28327 28330 28331 28334
     28340 28342 28343 28345 28376 28378 28378 28380 28380 28382 28386
     28388 28390 28392 28393 28395 28399 28401 28402 28404 28409 28411
     28419 28421 28426 28429 28431 28434 28437 28440 28442 28444 28444
     28446 28455 28457 28467 28469 28476 28478 28481 28483 28483 28486
     28487 28491 28491 28493 28501 28503 28504 28506 28516 28518 28519
     28521 28528 28530 28532 28534 28536 28538 28544 28546 28546 28548
     28553 28555 28558 28560 28560 28562 28567 28572 28572 28574 28574
     28576 28596 28598 28598 28600 28602 28604 28605 28607 28612 28614
     28623 28625 28626 28628 28629 28632 28632 28635 28644 28646 28649
     28651 28658 28660 28660 28663 28663 28666 28668 28670 28673 28676
     28679 28681 28687 28689 28689 28692 28701 28703 28708 28710 28715
     28719 28725 28727 28732 28734 28742 28744 28746 28748 28748 28751
     28751 28753 28754 28757 28760 28762 28763 28765 28774 28776 28781
     28783 28785 28788 28790 28792 28792 28794 28794 28796 28800 28802
     28806 28809 28810 28814 28814 28817 28822 28824 28826 28828 28829
     28831 28831 28833 28833 28836 28836 28841 28841 28843 28849 28851
     28853 28855 28862 28864 28867 28869 28872 28874 28875 28877 28879
     28881 28884 28887 28898 28900 28900 28902 28905 28907 28909 28911
     28912 28915 28916 28918 28925 28927 28928 28930 28930 28932 28932
     28934 28934 28937 28942 28944 28944 28947 28947 28949 28956 28958
     28963 28965 28966 28968 28968 28974 28978 28982 28982 28986 28986
     28993 28999 29001 29006 29008 29008 29010 29012 29014 29014 29016
     29018 29020 29034 29036 29036 29038 29038 29040 29040 29042 29043
     29048 29048 29050 29051 29053 29053 29056 29058 29060 29063 29065
     29066 29071 29072 29074 29074 29076 29076 29079 29089 29092 29093
     29095 29098 29100 29100 29103 29107 29109 29109 29112 29113 29116
     29131 29134 29136 29138 29138 29140 29142 29144 29148 29151 29154
     29156 29160 29164 29166 29168 29170 29172 29172 29176 29177 29179
     29183 29185 29187 29189 29191 29194 29194 29196 29197 29200 29200
     29203 29204 29209 29211 29213 29215 29218 29219 29222 29226 29228
     29229 29232 29233 29237 29243 29245 29247 29249 29250 29252 29252
     29254 29261 29263 29263 29266 29267 29270 29270 29272 29275 29277
     29283 29286 29287 29289 29290 29292 29292 29294 29296 29298 29313
     29316 29318 29320 29331 29333 29336 29338 29339 29341 29343 29345
     29354 29356 29360 29364 29370 29373 29373 29375 29382 29384 29390
     29392 29394 29396 29396 29398 29402 29404 29404 29406 29409 29411
     29412 29414 29414 29416 29428 29430 29441 29443 29443 29447 29448
     29450 29452 29454 29455 29457 29459 29461 29465 29467 29470 29473
     29475 29477 29479 29481 29486 29488 29500 29502 29504 29506 29509
     29513 29514 29516 29518 29520 29522 29527 29531 29533 29538 29541
     29552 29554 29555 29557 29560 29562 29579 29582 29582 29585 29591
     29595 29595 29597 29597 29599 29602 29604 29606 29608 29609 29611
     29616 29618 29628 29630 29632 29634 29635 29637 29645 29647 29652
     29654 29662 29664 29664 29667 29667 29669 29669 29671 29675 29677
     29678 29682 29682 29684 29686 29688 29688 29690 29690 29692 29697
     29699 29709 29711 29712 29718 29718 29722 29723 29725 29725 29728
     29734 29736 29750 29754 29754 29756 29756 29759 29762 29764 29764
     29766 29766 29770 29771 29773 29778 29780 29781 29783 29783 29785
     29788 29790 29791 29794 29796 29799 29799 29801 29802 29805 29811
     29813 29815 29817 29817 29820 29825 29827 29827 29829 29835 29838
     29838 29840 29840 29842 29842 29844 29845 29847 29848 29850 29850
     29852 29852 29854 29857 29859 29859 29861 29867 29869 29869 29871
     29874 29877 29880 29882 29883 29885 29891 29893 29893 29898 29899
     29903 29903 29906 29906 29908 29926 29928 29929 29932 29932 29934
     29935 29940 29943 29947 29947 29949 29952 29954 29956 29959 29960
     29963 29965 29967 29978 29980 29981 29983 29983 29985 29986 29989
     29990 29992 30003 30005 30005 30007 30011 30013 30016 30021 30021
     30023 30024 30027 30028 30030 30031 30036 30036 30041 30045 30047
     30047 30050 30054 30058 30060 30063 30064 30066 30066 30068 30068
     30070 30073 30077 30080 30083 30084 30086 30087 30090 30092 30095
     30098 30100 30106 30109 30109 30111 30117 30119 30119 30122 30124
     30126 30134 30136 30149 30151 30162 30164 30171 30173 30180 30182
     30184 30186 30187 30189 30189 30191 30209 30211 30211 30213 30213
     30216 30221 30223 30225 30227 30251 30253 30253 30255 30261 30264
     30264 30266 30266 30268 30272 30274 30275 30278 30281 30284 30285
     30288 30288 30290 30292 30294 30298 30300 30300 30302 30309 30313
     30322 30325 30325 30328 30329 30331 30335 30337 30338 30340 30340
     30342 30347 30350 30351 30353 30355 30357 30358 30361 30366 30372
     30372 30374 30374 30378 30379 30381 30386 30388 30389 30392 30392
     30394 30395 30397 30399 30402 30406 30408 30410 30413 30420 30422
     30424 30426 30431 30433 30433 30435 30439 30441 30442 30444 30453
     30455 30460 30462 30462 30465 30465 30467 30469 30471 30475 30477
     30477 30480 30483 30485 30485 30489 30491 30493 30493 30495 30496
     30498 30499 30501 30505 30509 30509 30511 30511 30513 30526 30528
     30529 30531 30535 30538 30546 30548 30550 30553 30556 30558 30563
     30565 30575 30585 30585 30588 30597 30599 30601 30603 30607 30609
     30610 30613 30613 30615 30615 30617 30627 30629 30629 30631 30637
     30640 30647 30650 30653 30655 30655 30658 30658 30660 30660 30663
     30663 30665 30666 30668 30672 30675 30677 30679 30684 30686 30686
     30688 30688 30690 30691 30693 30693 30695 30697 30699 30707 30710
     30723 30725 30726 30729 30729 30732 30740 30742 30744 30746 30746
     30748 30749 30751 30755 30757 30773 30775 30780 30782 30782 30784
     30784 30787 30787 30789 30789 30791 30794 30796 30798 30800 30800
     30802 30802 30805 30807 30812 30814 30816 30816 30818 30818 30820
     30821 30824 30833 30839 30839 30841 30841 30843 30844 30846 30848
     30851 30855 30857 30857 30860 30863 30865 30865 30867 30876 30878
     30885 30887 30893 30896 30900 30905 30908 30910 30910 30913 30913
     30915 30917 30920 30929 30932 30933 30937 30939 30941 30947 30949
     30949 30951 30954 30956 30957 30959 30959 30962 30964 30967 30967
     30969 30975 30977 30978 30980 30981 30985 30985 30988 30988 30990
     30990 30992 30996 30999 30999 31001 31001 31003 31006 31009 31009
     31011 31021 31023 31023 31025 31025 31028 31029 31032 31042 31044
     31052 31055 31063 31066 31077 31079 31083 31085 31085 31087 31088
     31090 31092 31095 31098 31100 31101 31103 31106 31108 31109 31112
     31112 31114 31115 31117 31120 31122 31128 31130 31132 31136 31138
     31140 31140 31142 31144 31146 31150 31152 31156 31158 31163 31165
     31169 31171 31171 31173 31174 31176 31177 31179 31179 31181 31183
     31185 31186 31189 31190 31192 31192 31196 31200 31203 31204 31206
     31207 31209 31216 31222 31224 31226 31227 31229 31229 31232 31232
     31234 31238 31240 31240 31242 31246 31248 31253 31255 31260 31262
     31264 31266 31267 31270 31270 31272 31272 31275 31275 31278 31281
     31283 31283 31287 31287 31289 31289 31291 31293 31295 31296 31300
     31300 31302 31304 31306 31310 31313 31313 31316 31316 31318 31320
     31322 31324 31327 31330 31335 31337 31339 31342 31344 31345 31348
     31355 31358 31361 31363 31373 31375 31378 31380 31385 31388 31392
     31394 31395 31397 31398 31400 31407 31409 31416 31418 31418 31422
     31425 31428 31429 31431 31431 31434 31435 31441 31441 31446 31446
     31448 31449 31454 31456 31459 31462 31467 31467 31469 31471 31478
     31479 31481 31483 31485 31485 31487 31489 31491 31494 31496 31499
     31502 31509 31512 31515 31517 31518 31520 31520 31522 31526 31528
     31528 31530 31541 31544 31544 31546 31548 31550 31550 31552 31552
     31556 31570 31572 31572 31574 31574 31576 31576 31578 31579 31581
     31581 31584 31591 31593 31593 31597 31598 31600 31609 31611 31611
     31614 31614 31616 31616 31618 31618 31620 31621 31623 31624 31626
     31633 31636 31641 31643 31645 31648 31650 31652 31652 31654 31661
     31663 31663 31665 31665 31668 31669 31671 31673 31678 31678 31680
     31681 31684 31684 31686 31687 31689 31692 31694 31694 31697 31697
     31699 31701 31704 31723 31726 31726 31728 31732 31735 31737 31739
     31747 31749 31751 31753 31761 31766 31766 31769 31769 31772 31779
     31781 31789 31792 31792 31795 31795 31799 31801 31803 31809 31811
     31811 31813 31813 31815 31818 31820 31821 31824 31824 31827 31828
     31831 31831 31833 31836 31839 31840 31843 31847 31849 31852 31854
     31855 31858 31861 31864 31869 31871 31873 31876 31877 31880 31882
     31884 31885 31889 31890 31892 31896 31900 31903 31905 31909 31912
     31912 31914 31914 31918 31919 31921 31925 31929 31935 31937 31937
     31941 31941 31943 31944 31946 31950 31952 31954 31956 31959 31961
     31961 31964 31968 31970 31970 31975 31976 31978 31978 31980 31980
     31982 31986 31988 31988 31990 31992 31995 31995 31997 31998 32000
     32034 32039 32041 32043 32044 32046 32051 32053 32054 32056 32071
     32074 32074 32078 32086 32088 32088 32091 32092 32094 32095 32097
     32099 32102 32107 32109 32115 32119 32119 32121 32125 32127 32129
     32131 32134 32136 32136 32140 32143 32145 32148 32150 32150 32156
     32163 32166 32167 32169 32170 32172 32178 32180 32181 32183 32194
     32196 32199 32201 32204 32206 32206 32210 32210 32215 32219 32221
     32225 32227 32227 32230 32234 32236 32236 32238 32244 32246 32247
     32249 32251 32259 32259 32264 32279 32282 32293 32297 32299 32301
     32329 32332 32332 32336 32346 32348 32348 32350 32355 32360 32363
     32365 32365 32367 32368 32370 32382 32384 32386 32390 32392 32394
     32397 32399 32399 32401 32401 32403 32412 32415 32429 32431 32435
     32437 32442 32445 32469 32471 32483 32485 32491 32493 32504 32506
     32521 32523 32527 32529 32541 32543 32566 32568 32570 32573 32575
     32578 32581 32584 32584 32586 32589 32591 32593 32596 32597 32599
     32600 32602 32611 32613 32622 32624 32631 32633 32639 32641 32641
     32643 32643 32645 32654 32657 32658 32660 32662 32666 32674 32676
     32681 32684 32685 32687 32691 32693 32707 32709 32709 32711 32711
     32713 32722 32724 32725 32727 32728 32731 32732 32734 32739 32741
     32742 32744 32757 32759 32761 32763 32769 32771 32775 32779 32786
     32788 32793 32795 32796 32798 32802 32804 32810 32812 32812 32816
     32817 32819 32825 32827 32827 32829 32831 32834 32835 32838 32840
     32842 32845 32847 32850 32852 32852 32854 32854 32856 32856 32858
     32858 32860 32862 32868 32868 32871 32871 32873 32874 32876 32876
     32879 32883 32885 32889 32893 32896 32898 32903 32905 32908 32911
     32912 32914 32915 32917 32918 32920 32925 32927 32933 32937 32939
     32941 32943 32945 32946 32948 32949 32951 32952 32954 32954 32956
     32970 32972 32977 32980 32990 32992 32993 32995 33005 33007 33014
     33016 33022 33024 33026 33029 33030 33032 33034 33037 33046 33048
     33051 33053 33055 33057 33061 33063 33063 33065 33065 33067 33069
     33071 33074 33078 33078 33080 33082 33085 33086 33091 33092 33094
     33096 33098 33109 33113 33116 33118 33118 33120 33122 33124 33127
     33129 33129 33131 33131 33133 33140 33142 33152 33154 33155 33158
     33165 33167 33167 33169 33169 33173 33173 33175 33184 33186 33187
     33190 33196 33198 33198 33200 33205 33207 33207 33209 33223 33225
     33226 33228 33229 33231 33234 33237 33237 33239 33243 33245 33251
     33253 33258 33260 33262 33266 33268 33271 33276 33278 33282 33284
     33293 33296 33298 33300 33302 33307 33315 33317 33317 33320 33320
     33322 33325 33327 33344 33346 33346 33348 33349 33351 33351 33353
     33353 33355 33355 33358 33363 33365 33372 33374 33375 33377 33377
     33379 33380 33382 33382 33384 33385 33387 33397 33399 33402 33404
     33408 33410 33413 33416 33416 33418 33419 33421 33428 33431 33457
     33459 33470 33472 33476 33479 33480 33482 33487 33489 33497 33499
     33500 33502 33505 33507 33512 33514 33517 33519 33527 33529 33531
     33534 33534 33536 33545 33548 33551 33553 33553 33556 33559 33561
     33564 33566 33566 33568 33568 33570 33570 33572 33581 33583 33583
     33585 33596 33599 33620 33622 33622 33626 33628 33630 33633 33635
     33647 33651 33656 33658 33663 33665 33665 33667 33667 33669 33680
     33682 33694 33696 33696 33698 33707 33710 33716 33718 33722 33724
     33725 33727 33740 33742 33743 33745 33745 33748 33753 33755 33765
     33767 33772 33774 33782 33784 33791 33793 33793 33795 33796 33798
     33799 33801 33811 33816 33816 33819 33821 33827 33833 33835 33837
     33839 33853 33855 33856 33858 33863 33865 33865 33867 33870 33872
     33874 33876 33876 33878 33879 33881 33889 33891 33891 33893 33897
     33899 33905 33907 33914 33917 33918 33922 33922 33926 33929 33931
     33937 33940 33940 33943 33954 33956 33956 33959 33964 33966 33970
     33972 33972 33974 33974 33976 33981 33983 33986 33988 33991 33993
     34004 34006 34007 34011 34011 34013 34013 34015 34016 34019 34019
     34021 34028 34030 34036 34038 34039 34041 34048 34050 34050 34054
     34063 34065 34074 34076 34081 34083 34097 34103 34110 34112 34113
     34115 34122 34125 34126 34129 34129 34131 34137 34139 34139 34141
     34142 34144 34158 34161 34162 34164 34172 34174 34174 34176 34193
     34196 34198 34200 34212 34214 34218 34222 34225 34227 34234 34237
     34249 34251 34251 34253 34259 34261 34261 34263 34266 34268 34271
     34273 34278 34280 34285 34287 34290 34294 34299 34301 34305 34308
     34311 34313 34316 34321 34321 34326 34332 34334 34343 34345 34346
     34348 34350 34353 34358 34360 34364 34366 34368 34371 34371 34374
     34376 34379 34390 34393 34396 34398 34399 34401 34405 34407 34417
     34419 34420 34423 34423 34425 34434 34437 34439 34442 34446 34448
     34449 34451 34458 34460 34462 34465 34469 34471 34474 34476 34476
     34479 34481 34483 34508 34510 34513 34515 34516 34518 34527 34530
     34532 34534 34534 34536 34542 34544 34555 34558 34558 34560 34574
     34577 34579 34581 34581 34583 34588 34590 34590 34592 34602 34604
     34606 34608 34613 34615 34616 34618 34620 34622 34627 34630 34633
     34636 34672 34675 34686 34689 34693 34695 34697 34699 34699 34701
     34701 34703 34708 34710 34712 34714 34719 34722 34724 34728 34728
     34730 34736 34738 34752 34754 34758 34760 34764 34769 34772 34775
     34777 34779 34792 34794 34799 34802 34804 34806 34807 34809 34812
     34814 34819 34821 34822 34824 34829 34832 34833 34835 34839 34841
     34841 34843 34845 34847 34854 34856 34860 34862 34867 34869 34873
     34875 34881 34883 34885 34888 34888 34890 34894 34898 34903 34905
     34907 34909 34909 34913 34917 34919 34930 34932 34935 34937 34937
     34940 34949 34952 34953 34955 34958 34961 34963 34965 34972 34974
     34975 34977 34978 34980 34980 34983 34984 34986 34989 34993 34994
     34998 35002 35004 35006 35008 35010 35013 35014 35017 35022 35024
     35024 35026 35026 35028 35039 35041 35045 35047 35048 35051 35052
     35054 35060 35062 35070 35073 35074 35077 35079 35081 35084 35086
     35086 35088 35099 35102 35103 35105 35107 35109 35111 35113 35128
     35131 35134 35137 35138 35140 35140 35142 35142 35145 35145 35147
     35148 35151 35155 35158 35172 35174 35174 35177 35183 35185 35188
     35190 35190 35193 35196 35198 35199 35201 35203 35205 35206 35208
     35208 35211 35211 35215 35215 35219 35219 35221 35224 35227 35231
     35233 35236 35238 35238 35242 35242 35244 35247 35250 35250 35254
     35255 35257 35258 35261 35266 35268 35276 35278 35286 35289 35302
     35304 35305 35307 35309 35311 35316 35318 35320 35322 35324 35326
     35328 35330 35332 35335 35336 35338 35338 35340 35340 35342 35347
     35349 35352 35355 35355 35357 35359 35362 35363 35365 35365 35367
     35367 35370 35370 35372 35373 35376 35377 35380 35380 35382 35382
     35385 35388 35390 35393 35396 35398 35400 35400 35402 35402 35404
     35410 35412 35417 35419 35419 35422 35422 35424 35427 35430 35430
     35432 35433 35435 35438 35440 35447 35449 35452 35455 35455 35457
     35463 35465 35469 35471 35471 35473 35475 35477 35478 35480 35482
     35486 35486 35488 35489 35491 35496 35498 35499 35504 35504 35506
     35506 35510 35510 35512 35520 35522 35529 35531 35531 35533 35533
     35535 35535 35537 35545 35547 35554 35556 35556 35558 35560 35563
     35563 35565 35576 35578 35580 35582 35586 35588 35592 35594 35595
     35597 35614 35616 35616 35618 35624 35626 35628 35630 35633 35635
     35635 35637 35639 35641 35646 35648 35650 35653 35659 35662 35674
     35676 35677 35679 35680 35683 35683 35685 35688 35690 35693 35695
     35696 35700 35700 35703 35707 35709 35712 35714 35714 35716 35718
     35720 35720 35722 35724 35726 35726 35730 35734 35736 35738 35740
     35740 35742 35755 35757 35760 35762 35770 35772 35782 35784 35791
     35793 35817 35819 35848 35850 35869 35871 35895 35897 35897 35899
     35903 35905 35907 35909 35920 35924 35927 35930 35930 35932 35933
     35935 35935 35937 35938 35940 35942 35944 35949 35951 35955 35957
     35963 35965 35965 35968 35970 35972 35974 35977 35978 35980 35981
     35983 35989 35991 35994 35996 35998 36000 36005 36007 36012 36015
     36016 36018 36037 36039 36040 36042 36042 36044 36044 36047 36047
     36049 36051 36053 36053 36055 36055 36057 36058 36060 36072 36074
     36074 36076 36078 36080 36081 36083 36085 36088 36094 36096 36096
     36098 36098 36100 36106 36109 36109 36111 36112 36115 36119 36121
     36121 36123 36123 36125 36127 36129 36176 36179 36182 36184 36190
     36192 36196 36198 36201 36203 36208 36210 36217 36219 36219 36221
     36221 36224 36225 36228 36229 36233 36246 36249 36249 36251 36252
     36255 36257 36259 36259 36261 36261 36263 36264 36266 36271 36273
     36282 36284 36284 36286 36287 36289 36296 36299 36305 36307 36307
     36309 36324 36326 36332 36334 36341 36343 36352 36354 36359 36361
     36362 36364 36365 36367 36391 36393 36396 36398 36401 36403 36406
     36408 36410 36412 36418 36420 36421 36423 36430 36432 36439 36441
     36455 36457 36458 36460 36461 36463 36464 36466 36468 36470 36470
     36472 36472 36474 36476 36479 36479 36481 36482 36484 36504 36506
     36506 36508 36513 36515 36518 36520 36524 36527 36527 36530 36530
     36538 36538 36541 36541 36544 36544 36546 36546 36553 36559 36561
     36564 36567 36568 36571 36577 36581 36585 36587 36588 36590 36591
     36593 36593 36596 36604 36606 36611 36613 36619 36621 36622 36624
     36632 36634 36640 36643 36646 36649 36650 36652 36652 36654 36655
     36658 36665 36667 36667 36670 36672 36674 36681 36683 36683 36685
     36699 36701 36708 36710 36713 36715 36735 36737 36747 36749 36753
     36755 36764 36766 36767 36771 36771 36774 36774 36776 36777 36779
     36779 36781 36786 36788 36788 36790 36790 36793 36793 36797 36799
     36801 36802 36804 36809 36811 36811 36813 36814 36816 36825 36827
     36838 36840 36843 36845 36846 36848 36848 36851 36870 36873 36877
     36879 36882 36884 36887 36889 36900 36902 36902 36909 36911 36913
     36914 36916 36918 36920 36920 36923 36927 36929 36930 36932 36932
     36935 36935 36937 36939 36941 36949 36951 36953 36955 36958 36960
     36960 36962 36963 36965 36965 36967 36969 36971 36971 36973 36976
     36978 37000 37002 37003 37005 37005 37007 37009 37011 37013 37015
     37017 37019 37019 37021 37027 37029 37031 37034 37034 37036 37036
     37038 37046 37048 37051 37053 37055 37057 37057 37059 37061 37063
     37064 37066 37067 37070 37073 37075 37085 37087 37101 37103 37109
     37112 37129 37131 37131 37133 37138 37140 37140 37142 37156 37158
     37174 37176 37179 37182 37185 37187 37200 37202 37203 37205 37208
     37210 37210 37213 37221 37224 37226 37228 37228 37230 37242 37245
     37255 37257 37261 37263 37267 37273 37283 37285 37285 37287 37288
     37290 37301 37303 37303 37305 37306 37308 37310 37312 37315 37317
     37319 37321 37329 37331 37333 37335 37338 37340 37341 37346 37348
     37350 37358 37361 37361 37363 37365 37367 37369 37373 37373 37375
     37383 37385 37386 37388 37389 37391 37394 37396 37399 37401 37402
     37404 37404 37406 37406 37411 37415 37421 37422 37424 37428 37430
     37434 37437 37440 37445 37446 37448 37460 37462 37463 37466 37467
     37470 37470 37472 37473 37475 37479 37484 37485 37487 37488 37490
     37490 37492 37492 37494 37494 37496 37504 37506 37507 37509 37512
     37514 37518 37521 37521 37523 37533 37536 37548 37550 37550 37554
     37559 37563 37564 37568 37587 37589 37589 37591 37593 37597 37601
     37604 37604 37606 37610 37614 37617 37623 37628 37630 37634 37636
     37636 37638 37638 37640 37641 37643 37648 37650 37654 37656 37659
     37661 37675 37677 37679 37683 37686 37688 37689 37692 37692 37694
     37694 37702 37703 37705 37714 37716 37724 37726 37726 37728 37729
     37731 37733 37735 37735 37738 37738 37740 37741 37744 37745 37749
     37751 37753 37756 37758 37758 37760 37760 37762 37763 37768 37770
     37772 37775 37777 37778 37780 37787 37789 37791 37793 37802 37804
     37804 37806 37813 37815 37815 37824 37824 37826 37828 37831 37832
     37834 37834 37836 37842 37844 37850 37852 37855 37857 37860 37862
     37864 37868 37868 37870 37870 37877 37888 37891 37891 37894 37895
     37897 37910 37912 37913 37920 37920 37928 37932 37934 37934 37936
     37939 37941 37952 37956 37964 37967 37970 37973 37973 37975 37975
     37981 37982 37984 37984 37986 37988 37992 37995 37997 38008 38012
     38019 38021 38032 38034 38037 38039 38039 38041 38086 38088 38094
     38096 38098 38101 38105 38107 38117 38119 38138 38140 38171 38173
     38175 38177 38182 38184 38194 38196 38204 38206 38210 38212 38215
     38217 38218 38220 38228 38230 38233 38235 38239 38241 38253 38255
     38259 38262 38263 38266 38269 38271 38272 38274 38275 38278 38278
     38280 38281 38283 38292 38296 38296 38299 38300 38302 38303 38305
     38305 38307 38309 38312 38313 38315 38318 38320 38321 38325 38327
     38329 38336 38339 38339 38341 38349 38352 38358 38362 38364 38366
     38373 38376 38379 38381 38398 38400 38406 38408 38418 38420 38423
     38425 38426 38428 38436 38440 38440 38442 38442 38444 38454 38457
     38461 38463 38464 38466 38481 38483 38485 38488 38488 38491 38495
     38497 38500 38503 38509 38511 38520 38524 38526 38528 38528 38531
     38539 38541 38549 38551 38553 38555 38556 38558 38558 38561 38562
     38564 38564 38567 38570 38572 38572 38574 38574 38576 38577 38579
     38580 38582 38582 38584 38585 38587 38607 38610 38627 38629 38629
     38632 38634 38639 38643 38645 38651 38653 38658 38660 38665 38667
     38667 38669 38675 38678 38678 38680 38681 38684 38688 38690 38704
     38706 38706 38709 38709 38712 38714 38717 38719 38722 38724 38726
     38729 38731 38731 38738 38739 38742 38742 38744 38748 38750 38750
     38752 38754 38757 38758 38760 38762 38764 38764 38766 38766 38768
     38768 38770 38772 38774 38776 38778 38789 38792 38792 38794 38795
     38797 38799 38801 38802 38804 38804 38807 38810 38812 38814 38816
     38822 38824 38824 38826 38831 38834 38836 38838 38839 38841 38841
     38843 38843 38847 38847 38849 38849 38851 38855 38857 38857 38859
     38864 38867 38873 38876 38879 38881 38881 38883 38883 38885 38887
     38889 38893 38896 38897 38899 38899 38901 38902 38904 38907 38909
     38920 38922 38922 38924 38931 38934 38936 38939 38942 38944 38945
     38948 38948 38950 38953 38955 38955 38957 38957 38959 38960 38962
     38962 38965 38965 38967 38969 38971 38971 38977 38977 38979 38982
     38984 38986 38988 38995 38999 39001 39003 39008 39010 39013 39015
     39015 39017 39019 39023 39050 39052 39053 39055 39057 39059 39060
     39062 39064 39066 39074 39076 39081 39084 39087 39089 39091 39094
     39094 39096 39096 39098 39106 39108 39108 39110 39110 39113 39113
     39115 39116 39118 39118 39121 39123 39125 39125 39128 39131 39134
     39135 39138 39139 39141 39141 39143 39147 39149 39149 39151 39151
     39154 39154 39156 39156 39158 39158 39161 39162 39164 39166 39168
     39168 39170 39171 39173 39173 39175 39178 39180 39181 39184 39192
     39194 39195 39198 39199 39201 39201 39204 39205 39207 39219 39221
     39221 39226 39226 39228 39231 39233 39233 39235 39235 39237 39237
     39239 39241 39243 39244 39246 39246 39248 39257 39259 39260 39262
     39263 39265 39265 39267 39267 39269 39269 39271 39282 39284 39287
     39290 39290 39292 39293 39295 39297 39300 39304 39306 39307 39309
     39309 39311 39321 39324 39326 39329 39329 39331 39331 39333 39336
     39339 39349 39353 39355 39357 39357 39361 39363 39367 39367 39369
     39369 39371 39385 39387 39389 39391 39391 39394 39397 39399 39399
     39401 39402 39404 39406 39408 39409 39412 39412 39414 39423 39425
     39431 39433 39435 39437 39439 39441 39441 39444 39446 39449 39454
     39456 39456 39458 39461 39463 39463 39465 39470 39472 39474 39476
     39482 39485 39494 39496 39498 39500 39504 39506 39511 39513 39515
     39518 39520 39522 39522 39524 39537 39539 39554 39556 39560 39562
     39564 39567 39571 39574 39576 39578 39589 39591 39592 39595 39595
     39597 39597 39599 39601 39603 39604 39606 39612 39614 39618 39621
     39623 39626 39629 39631 39638 39640 39640 39644 39644 39647 39647
     39649 39649 39651 39651 39654 39655 39659 39663 39665 39667 39670
     39671 39673 39678 39681 39681 39683 39686 39688 39688 39690 39699
     39701 39706 39710 39712 39714 39717 39719 39721 39723 39723 39726
     39727 39729 39731 39733 39733 39735 39735 39738 39740 39742 39743
     39745 39759 39761 39762 39764 39766 39768 39771 39775 39777 39780
     39780 39782 39784 39788 39788 39791 39793 39796 39799 39802 39806
     39808 39808 39810 39810 39813 39816 39824 39827 39829 39829 39834
     39835 39838 39838 39840 39842 39844 39846 39848 39848 39850 39851
     39853 39855 39861 39862 39864 39865 39869 39869 39871 39873 39875
     39876 39878 39882 39891 39895 39897 39900 39902 39902 39904 39906
     39908 39912 39914 39916 39920 39920 39927 39928 39933 39933 39941
     39945 39947 39947 39949 39950 39954 39956 39959 39959 39964 39965
     39969 39969 39971 39973 39976 39977 39979 39981 39985 39988 39990
     39991 39993 39993 39995 40001 40004 40004 40006 40006 40008 40014
     40016 40016 40018 40018 40020 40025 40030 40032 40034 40035 40038
     40040 40045 40046 40049 40049 40051 40053 40055 40058 40060 40060
     40063 40063 40065 40066 40069 40072 40075 40075 40077 40078 40080
     40082 40084 40085 40090 40092 40094 40105 40107 40107 40109 40110
     40112 40120 40122 40125 40131 40135 40138 40144 40147 40153 40156
     40159 40162 40162 40165 40167 40169 40170 40173 40173 40177 40183
     40185 40189 40191 40192 40195 40201 40208 40208 40210 40210 40212
     40213 40215 40217 40219 40219 40221 40224 40226 40227 40229 40230
     40232 40233 40237 40241 40243 40243 40246 40248 40251 40251 40253
     40259 40261 40261 40266 40268 40271 40271 40273 40276 40278 40285
     40287 40289 40295 40300 40303 40309 40311 40313 40315 40315 40317
     40317 40319 40322 40324 40332 40336 40336 40338 40338 40340 40340
     40342 40356 40358 40362 40364 40365 40367 40367 40369 40380 40382
     40383 40385 40387 40389 40389 40391 40392 40394 40403 40405 40415
     40417 40422 40424 40425 40427 40432 40434 40443 40445 40455 40457
     40457 40459 40459 40461 40461 40463 40469 40471 40471 40473 40475
     40477 40483 40485 40486 40488 40493 40495 40495 40497 40499 40501
     40506 40509 40511 40513 40524 40526 40527 40529 40529 40533 40533
     40535 40536 40538 40540 40542 40542 40547 40548 40550 40557 40560
     40561 40563 40563 40565 40565 40569 40570 40572 40576 40578 40579
     40582 40590 40593 40596 40599 40599 40601 40605 40607 40609 40612
     40615 40617 40617 40621 40622 40624 40624 40628 40632 40635 40638
     40640 40640 40642 40644 40648 40649 40652 40657 40659 40662 40664
     40664 40666 40672 40674 40674 40676 40683 40685 40688 40690 40695
     40697 40705 40710 40711 40713 40715 40717 40720 40722 40723 40725
     40732 40734 40734 40736 40736 40738 40741 40744 40761 40763 40763
     40765 40766 40768 40772 40774 40786 40788 40793 40795 40801 40803
     40807 40810 40812 40814 40818 40820 40827 40830 40832 40835 40845
     40848 40850 40852 40853 40856 40860 40863 40864 40866 40866 40868
     40868 65072 65074 65077 65092 65097 65102 65104 65106 65108 65111
     65113 65126 65128 65131 65281 65374 65504 65505 65507 65507 65509
     65509 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 2254 :NAME "windows-1254" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1254) [Lazhintseva]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 160 207 209 220 223 239 241 252 255 255 286 287 304 305 338
     339 350 353 376 376 402 402 710 710 732 732 8211 8212 8216 8218
     8220 8222 8224 8226 8230 8230 8240 8240 8249 8250 8364 8364 8482
     8482))
  (MAKE-CHARACTER-SET :MIB-ENUM 2255 :NAME "windows-1255" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1255) [Lazhintseva]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES
   #(0 127 160 163 165 169 171 185 187 191 215 215 247 247 402 402 710
     710 732 732 1456 1465 1467 1475 1488 1514 1520 1524 8206 8207 8211
     8212 8216 8218 8220 8222 8224 8226 8230 8230 8240 8240 8249 8250
     8362 8362 8364 8364 8482 8482 64285 64285 64287 64287 64298 64310
     64312 64316 64318 64318 64320 64321 64323 64324 64326 64334 917504
     917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 2027 :NAME "macintosh" :ALIASES
   '("csMacintosh" "mac") :MIME-ENCODING 'NIL :SOURCE
   '"The Unicode Standard ver1.0, ISBN 0-201-56788-1, Oct 1991"
   :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 165 167 172 174 177 180 184 186 187 191 207 209 214 216
     220 223 239 241 252 255 255 305 305 338 339 376 376 402 402 710 711
     728 733 960 960 8211 8212 8216 8218 8220 8222 8224 8226 8230 8230
     8240 8240 8249 8250 8260 8260 8482 8482 8486 8486 8706 8706 8710
     8710 8719 8719 8721 8721 8730 8730 8734 8734 8747 8747 8776 8776
     8800 8800 8804 8805 9674 9674 64257 64258))
  (MAKE-CHARACTER-SET :MIB-ENUM 18 :NAME
   "Extended_UNIX_Code_Packed_Format_for_Japanese" :ALIASES
   '(#18="EUC-JP" "csEUCPkdFmtJapanese") :MIME-ENCODING '#18# :SOURCE
   '"Standardized by OSF, UNIX International, and UNIX Systems Laboratories Pacific.  Uses ISO 2022 rules to select code set 0: US-ASCII (a single 7-bit byte set) code set 1: JIS X0208-1990 (a double 8-bit byte set) restricted to A0-FF in both bytes code set 2: Half Width Katakana (a single 7-bit byte set) requiring SS2 as the character prefix code set 3: JIS X0212-1990 (a double 7-bit byte set) restricted to A0-FF in both bytes requiring SS3 as the character prefix"
   :COMMENTS '("Alias: EUC-JP  (preferred MIME name)") :REFERENCES 'NIL
   :RANGES
   #(0 141 144 159 161 170 172 172 174 177 180 180 182 182 184 184 186
     186 191 207 209 275 278 290 292 299 302 333 336 382 461 476 501 501
     711 711 728 731 733 733 900 902 904 906 908 908 910 929 931 974
     1025 1036 1038 1103 1105 1116 1118 1119 8208 8208 8213 8214 8216
     8217 8220 8221 8224 8225 8229 8230 8240 8240 8242 8243 8251 8251
     8254 8254 8451 8451 8470 8470 8482 8482 8491 8491 8592 8595 8658
     8658 8660 8660 8704 8704 8706 8707 8711 8712 8715 8715 8722 8722
     8730 8730 8733 8734 8736 8736 8743 8748 8756 8757 8765 8765 8786
     8786 8800 8801 8806 8807 8810 8811 8834 8835 8838 8839 8869 8869
     8978 8978 9472 9475 9484 9484 9487 9488 9491 9492 9495 9496 9499
     9501 9504 9504 9507 9509 9512 9512 9515 9516 9519 9520 9523 9524
     9527 9528 9531 9532 9535 9535 9538 9538 9547 9547 9632 9633 9650
     9651 9660 9661 9670 9671 9675 9675 9678 9679 9711 9711 9733 9734
     9792 9792 9794 9794 9834 9834 9837 9837 9839 9839 12288 12291 12293
     12309 12316 12316 12353 12435 12443 12446 12449 12534 12539 12542
     19968 19973 19975 19982 19984 19986 19988 19993 19998 19999 20001
     20001 20003 20004 20006 20006 20008 20008 20010 20011 20013 20018
     20021 20022 20024 20025 20027 20028 20031 20037 20039 20039 20043
     20043 20045 20047 20049 20049 20053 20058 20060 20063 20066 20067
     20072 20073 20081 20081 20083 20085 20089 20089 20094 20096 20098
     20098 20101 20102 20104 20110 20113 20114 20116 20121 20123 20130
     20132 20134 20136 20136 20139 20144 20147 20147 20150 20150 20153
     20154 20160 20164 20166 20167 20170 20171 20173 20176 20180 20187
     20189 20197 20200 20200 20205 20211 20213 20215 20219 20219 20221
     20227 20232 20242 20245 20247 20249 20250 20252 20253 20270 20273
     20275 20286 20288 20288 20290 20291 20294 20297 20299 20320 20323
     20323 20329 20330 20332 20332 20334 20337 20339 20339 20341 20351
     20353 20358 20360 20372 20374 20379 20381 20385 20395 20395 20397
     20399 20402 20402 20405 20407 20409 20409 20411 20422 20424 20434
     20436 20436 20439 20440 20442 20445 20447 20453 20462 20464 20466
     20467 20469 20470 20472 20472 20474 20474 20476 20481 20484 20487
     20489 20500 20502 20511 20513 20526 20528 20528 20530 20531 20533
     20534 20537 20537 20539 20539 20544 20547 20549 20554 20556 20556
     20558 20563 20565 20567 20569 20570 20572 20572 20575 20576 20578
     20579 20581 20583 20586 20586 20588 20589 20592 20594 20596 20598
     20600 20600 20605 20605 20608 20609 20611 20614 20618 20618 20621
     20628 20630 20630 20632 20636 20638 20642 20650 20650 20652 20653
     20655 20656 20658 20661 20663 20663 20665 20666 20669 20670 20672
     20672 20674 20677 20679 20679 20681 20682 20684 20689 20691 20694
     20696 20696 20698 20698 20700 20703 20706 20713 20717 20719 20721
     20722 20725 20726 20729 20731 20734 20734 20736 20740 20742 20745
     20747 20750 20752 20752 20754 20754 20756 20767 20769 20769 20771
     20771 20775 20776 20778 20778 20780 20781 20783 20783 20785 20789
     20791 20796 20799 20816 20818 20821 20823 20824 20826 20826 20828
     20828 20831 20831 20834 20834 20836 20838 20840 20846 20849 20849
     20853 20856 20860 20860 20862 20862 20864 20864 20866 20870 20873
     20883 20885 20889 20893 20893 20896 20902 20904 20909 20912 20920
     20922 20922 20924 20927 20930 20930 20932 20934 20936 20937 20939
     20941 20943 20943 20945 20947 20949 20950 20952 20952 20955 20958
     20960 20962 20965 20967 20969 20970 20973 20974 20976 20986 20989
     20990 20992 21000 21002 21003 21006 21006 21009 21016 21021 21021
     21026 21026 21028 21029 21031 21034 21038 21038 21040 21043 21045
     21052 21059 21061 21063 21063 21065 21069 21071 21071 21076 21080
     21082 21084 21086 21089 21091 21094 21097 21098 21102 21109 21111
     21113 21117 21117 21119 21120 21122 21123 21125 21125 21127 21130
     21132 21133 21137 21144 21146 21148 21151 21152 21155 21159 21161
     21165 21167 21169 21172 21182 21184 21185 21187 21193 21196 21197
     21199 21199 21201 21202 21204 21209 21211 21226 21228 21228 21232
     21242 21246 21251 21253 21254 21256 21256 21258 21261 21263 21265
     21267 21267 21269 21281 21283 21283 21285 21285 21287 21293 21295
     21299 21301 21301 21304 21315 21317 21325 21329 21332 21335 21340
     21342 21342 21344 21345 21347 21347 21349 21350 21353 21353 21356
     21365 21367 21369 21371 21371 21374 21375 21378 21380 21383 21384
     21390 21390 21395 21396 21398 21398 21400 21402 21405 21405 21407
     21409 21412 21414 21416 21419 21421 21424 21426 21432 21434 21435
     21437 21437 21440 21440 21442 21443 21445 21445 21448 21455 21458
     21463 21465 21467 21469 21491 21493 21496 21498 21498 21505 21508
     21512 21521 21523 21523 21530 21531 21533 21533 21535 21537 21542
     21551 21553 21553 21556 21558 21560 21561 21563 21566 21568 21568
     21570 21572 21574 21578 21581 21583 21585 21585 21598 21599 21602
     21602 21604 21604 21606 21611 21613 21614 21616 21617 21619 21623
     21627 21629 21631 21633 21635 21638 21640 21641 21643 21650 21653
     21654 21660 21660 21663 21663 21665 21666 21668 21679 21681 21683
     21687 21698 21700 21700 21702 21706 21709 21710 21720 21720 21728
     21730 21733 21734 21736 21738 21740 21743 21746 21746 21750 21750
     21754 21754 21756 21761 21764 21769 21772 21776 21780 21782 21802
     21803 21806 21807 21809 21811 21813 21814 21816 21817 21819 21822
     21824 21825 21828 21831 21833 21834 21836 21837 21839 21841 21843
     21843 21846 21848 21850 21854 21856 21857 21859 21860 21862 21862
     21883 21884 21886 21892 21894 21899 21902 21903 21905 21908 21911
     21914 21916 21919 21923 21924 21927 21934 21936 21936 21938 21938
     21942 21942 21951 21951 21953 21953 21955 21959 21961 21961 21963
     21964 21966 21966 21969 21972 21975 21976 21978 21980 21982 21983
     21986 21988 21993 21993 22006 22007 22009 22009 22013 22015 22021
     22022 22024 22026 22029 22034 22036 22036 22038 22041 22043 22043
     22057 22057 22060 22060 22063 22073 22075 22077 22079 22084 22086
     22086 22089 22089 22091 22096 22100 22100 22107 22107 22110 22110
     22112 22116 22118 22118 22120 22125 22127 22127 22129 22130 22132
     22133 22136 22136 22138 22138 22144 22144 22148 22152 22154 22156
     22159 22159 22164 22165 22169 22170 22173 22176 22178 22178 22181
     22185 22187 22190 22193 22193 22195 22196 22198 22199 22204 22204
     22206 22206 22208 22211 22213 22213 22216 22225 22227 22227 22231
     22241 22243 22248 22251 22251 22253 22254 22256 22259 22262 22263
     22265 22266 22269 22269 22271 22276 22279 22285 22287 22287 22289
     22291 22293 22294 22296 22296 22298 22301 22303 22304 22306 22314
     22316 22320 22323 22324 22327 22328 22331 22331 22333 22336 22338
     22338 22341 22343 22346 22346 22348 22354 22369 22370 22372 22379
     22381 22385 22387 22389 22391 22391 22393 22396 22398 22399 22401
     22403 22408 22409 22411 22412 22419 22421 22423 22423 22425 22426
     22428 22436 22439 22442 22444 22444 22448 22448 22451 22451 22456
     22456 22461 22461 22464 22464 22467 22467 22470 22472 22475 22476
     22478 22479 22482 22486 22492 22497 22499 22500 22502 22503 22505
     22505 22509 22509 22512 22512 22516 22522 22524 22528 22530 22534
     22536 22541 22549 22549 22553 22553 22555 22555 22557 22561 22564
     22564 22566 22567 22570 22570 22573 22573 22575 22578 22580 22581
     22585 22586 22589 22589 22591 22593 22601 22605 22607 22610 22612
     22613 22615 22618 22622 22623 22625 22626 22628 22628 22631 22633
     22635 22635 22640 22640 22642 22642 22645 22645 22648 22649 22652
     22652 22654 22657 22659 22659 22661 22661 22663 22666 22668 22669
     22671 22672 22675 22676 22678 22679 22684 22685 22687 22690 22694
     22694 22696 22697 22699 22699 22702 22702 22705 22707 22712 22716
     22718 22718 22721 22722 22724 22725 22727 22728 22730 22730 22732
     22734 22736 22746 22748 22751 22753 22754 22756 22757 22761 22761
     22763 22764 22766 22771 22775 22775 22777 22781 22786 22786 22789
     22790 22793 22796 22799 22800 22802 22806 22808 22813 22817 22821
     22823 22835 22837 22840 22846 22847 22851 22852 22854 22857 22862
     22869 22871 22875 22877 22883 22885 22885 22887 22895 22898 22902
     22904 22905 22907 22909 22913 22916 22922 22926 22930 22931 22933
     22935 22937 22937 22939 22939 22941 22941 22943 22943 22947 22949
     22951 22952 22956 22960 22962 22963 22967 22967 22969 22972 22974
     22974 22977 22977 22979 22980 22982 22982 22984 22987 22989 22989
     22992 22996 23001 23002 23004 23007 23011 23016 23018 23019 23022
     23023 23025 23026 23028 23028 23030 23031 23035 23035 23039 23041
     23043 23044 23049 23049 23052 23054 23057 23059 23064 23064 23066
     23066 23068 23068 23070 23072 23075 23077 23079 23082 23085 23085
     23087 23088 23093 23094 23100 23100 23104 23105 23108 23113 23116
     23116 23120 23120 23125 23125 23130 23130 23134 23134 23138 23139
     23141 23143 23146 23146 23148 23149 23159 23159 23162 23163 23166
     23167 23179 23179 23184 23184 23186 23187 23190 23190 23193 23196
     23198 23200 23202 23202 23207 23207 23212 23212 23217 23219 23221
     23221 23224 23224 23226 23231 23233 23234 23236 23236 23238 23238
     23240 23241 23243 23244 23247 23248 23254 23255 23258 23258 23260
     23260 23264 23265 23267 23267 23269 23270 23273 23274 23278 23278
     23285 23286 23290 23291 23293 23293 23296 23297 23304 23305 23307
     23308 23318 23319 23321 23321 23323 23323 23325 23325 23329 23330
     23333 23333 23338 23338 23340 23341 23344 23344 23346 23346 23348
     23348 23350 23350 23352 23352 23358 23358 23360 23361 23363 23363
     23365 23365 23371 23372 23376 23378 23380 23384 23386 23391 23395
     23398 23400 23401 23403 23403 23406 23409 23411 23411 23413 23413
     23416 23416 23418 23418 23420 23425 23427 23441 23443 23453 23455
     23455 23458 23462 23464 23465 23468 23482 23484 23484 23487 23495
     23497 23497 23500 23501 23503 23504 23506 23508 23510 23515 23517
     23522 23524 23529 23531 23531 23534 23537 23539 23542 23544 23544
     23546 23546 23549 23551 23553 23554 23556 23567 23569 23569 23571
     23571 23574 23575 23578 23578 23582 23584 23586 23588 23590 23590
     23592 23593 23595 23598 23600 23602 23605 23606 23608 23617 23621
     23622 23624 23624 23626 23627 23629 23633 23635 23635 23637 23637
     23641 23642 23644 23644 23646 23646 23648 23653 23655 23657 23660
     23665 23668 23670 23673 23677 23687 23688 23690 23690 23692 23692
     23695 23698 23700 23700 23709 23709 23711 23715 23718 23718 23720
     23724 23729 23736 23738 23740 23742 23742 23749 23749 23751 23751
     23753 23753 23755 23755 23762 23762 23767 23767 23769 23769 23773
     23773 23776 23777 23784 23786 23789 23794 23796 23796 23798 23798
     23802 23803 23805 23805 23809 23809 23814 23815 23819 23819 23821
     23822 23825 23826 23828 23835 23839 23839 23842 23844 23846 23847
     23849 23849 23851 23851 23857 23857 23860 23860 23865 23865 23869
     23869 23871 23871 23874 23875 23878 23878 23880 23880 23882 23884
     23886 23886 23888 23890 23893 23893 23897 23897 23900 23900 23903
     23906 23908 23908 23913 23914 23916 23917 23919 23920 23923 23923
     23926 23926 23929 23930 23934 23935 23937 23940 23943 23944 23946
     23948 23952 23952 23954 23957 23961 23961 23963 23963 23965 23965
     23967 23968 23970 23970 23975 23975 23979 23980 23982 23982 23984
     23984 23986 23986 23988 23988 23991 23994 23996 23997 24003 24003
     24007 24007 24009 24009 24011 24014 24016 24016 24018 24019 24022
     24022 24024 24025 24027 24027 24029 24030 24032 24033 24035 24041
     24043 24043 24046 24046 24049 24053 24055 24057 24059 24059 24061
     24062 24064 24064 24066 24067 24070 24071 24075 24077 24081 24082
     24084 24086 24088 24091 24093 24093 24095 24096 24101 24101 24104
     24104 24107 24107 24109 24112 24114 24115 24117 24120 24125 24126
     24128 24128 24131 24133 24135 24135 24137 24137 24139 24140 24142
     24142 24144 24145 24148 24152 24155 24156 24158 24159 24161 24164
     24168 24168 24170 24174 24176 24176 24178 24182 24184 24193 24195
     24196 24199 24199 24202 24203 24206 24207 24213 24215 24218 24218
     24220 24220 24224 24224 24226 24226 24228 24232 24234 24237 24241
     24241 24243 24243 24245 24248 24253 24255 24257 24259 24262 24262
     24264 24268 24270 24278 24282 24291 24293 24293 24296 24297 24299
     24300 24304 24305 24307 24308 24310 24312 24314 24316 24318 24319
     24321 24324 24326 24337 24339 24345 24347 24349 24351 24351 24353
     24361 24363 24369 24372 24376 24379 24385 24388 24389 24391 24392
     24394 24394 24396 24398 24400 24401 24403 24404 24406 24409 24411
     24413 24416 24420 24422 24423 24425 24429 24431 24437 24439 24442
     24444 24453 24455 24461 24463 24467 24470 24473 24476 24478 24480
     24482 24484 24484 24487 24497 24499 24500 24504 24505 24508 24509
     24515 24517 24519 24521 24523 24525 24528 24532 24534 24537 24540
     24542 24544 24546 24548 24548 24552 24563 24565 24566 24568 24568
     24570 24573 24575 24575 24583 24583 24586 24586 24589 24592 24594
     24605 24607 24609 24612 24619 24621 24621 24623 24623 24625 24625
     24627 24627 24629 24629 24634 24634 24640 24643 24646 24653 24656
     24658 24660 24663 24665 24666 24669 24669 24671 24677 24679 24685
     24687 24689 24693 24693 24695 24695 24702 24703 24705 24708 24710
     24710 24712 24718 24721 24728 24730 24731 24733 24736 24738 24746
     24752 24760 24763 24766 24770 24770 24772 24779 24782 24783 24785
     24785 24787 24789 24792 24803 24805 24805 24807 24808 24816 24829
     24832 24835 24838 24842 24844 24855 24857 24860 24862 24863 24865
     24866 24871 24872 24874 24876 24880 24881 24884 24887 24889 24889
     24892 24895 24897 24898 24900 24910 24915 24915 24917 24917 24920
     24922 24925 24928 24930 24931 24933 24933 24935 24936 24939 24940
     24942 24952 24955 24956 24958 24964 24967 24967 24970 24971 24973
     24974 24976 24980 24982 24986 24988 24989 24991 24992 24996 24997
     24999 25006 25010 25010 25014 25014 25016 25018 25020 25020 25022
     25022 25024 25027 25030 25040 25045 25045 25052 25055 25057 25059
     25061 25063 25065 25065 25068 25069 25071 25071 25074 25074 25076
     25076 25078 25080 25082 25082 25084 25089 25091 25092 25095 25098
     25100 25102 25104 25110 25114 25123 25126 25127 25129 25131 25134
     25136 25138 25140 25144 25145 25147 25147 25149 25149 25151 25156
     25158 25161 25163 25166 25168 25174 25176 25176 25178 25180 25182
     25182 25184 25184 25187 25188 25192 25192 25197 25199 25201 25201
     25203 25203 25206 25206 25209 25210 25212 25216 25218 25220 25225
     25226 25229 25240 25243 25244 25246 25246 25254 25254 25256 25256
     25259 25260 25265 25265 25267 25267 25269 25271 25273 25279 25282
     25282 25284 25290 25292 25309 25312 25313 25322 25322 25324 25327
     25329 25335 25340 25343 25345 25348 25351 25357 25360 25361 25363
     25363 25366 25366 25368 25369 25375 25375 25383 25387 25389 25389
     25391 25391 25397 25398 25401 25402 25404 25407 25409 25412 25414
     25414 25417 25424 25426 25429 25431 25432 25435 25436 25445 25449
     25451 25454 25457 25458 25460 25464 25466 25469 25471 25472 25474
     25476 25479 25482 25484 25484 25486 25488 25490 25490 25492 25494
     25496 25499 25502 25519 25522 25522 25524 25525 25531 25531 25533
     25534 25536 25537 25539 25542 25544 25545 25550 25558 25562 25564
     25568 25569 25571 25571 25573 25573 25577 25578 25580 25580 25582
     25582 25586 25590 25592 25594 25606 25606 25609 25610 25613 25613
     25615 25616 25618 25620 25622 25624 25628 25628 25630 25630 25632
     25632 25634 25634 25636 25638 25640 25642 25644 25645 25647 25648
     25652 25654 25658 25658 25661 25663 25666 25666 25675 25675 25678
     25679 25681 25684 25688 25688 25690 25693 25695 25697 25699 25699
     25703 25703 25705 25705 25709 25709 25711 25711 25715 25716 25718
     25718 25720 25720 25722 25723 25725 25725 25731 25731 25733 25733
     25735 25736 25743 25747 25749 25749 25752 25755 25757 25759 25761
     25761 25763 25766 25768 25769 25771 25774 25776 25776 25778 25779
     25785 25785 25787 25791 25793 25794 25796 25797 25799 25799 25801
     25806 25808 25810 25812 25813 25815 25816 25818 25818 25824 25831
     25833 25834 25836 25837 25839 25842 25844 25847 25850 25851 25853
     25857 25860 25861 25864 25866 25871 25871 25875 25876 25878 25878
     25880 25881 25883 25887 25890 25892 25894 25894 25897 25900 25902
     25903 25905 25905 25908 25919 25923 25923 25925 25925 25927 25929
     25933 25933 25935 25938 25940 25945 25949 25952 25954 25955 25958
     25959 25963 25964 25968 25968 25970 25970 25972 25973 25975 25976
     25978 25978 25981 25981 25985 25987 25989 25989 25991 25994 25996
     25996 25998 25998 26000 26002 26005 26005 26007 26009 26011 26013
     26015 26017 26019 26023 26027 26032 26034 26036 26039 26039 26041
     26041 26044 26045 26047 26047 26049 26054 26056 26057 26059 26060
     26062 26064 26066 26066 26068 26068 26070 26073 26075 26075 26079
     26082 26085 26089 26092 26093 26096 26098 26100 26101 26105 26107
     26110 26112 26114 26116 26118 26122 26124 26127 26129 26134 26140
     26161 26163 26167 26169 26169 26172 26172 26175 26182 26185 26188
     26190 26191 26193 26194 26199 26201 26203 26210 26212 26212 26214
     26220 26222 26224 26227 26236 26238 26241 26243 26244 26247 26249
     26251 26254 26256 26258 26262 26269 26271 26272 26274 26274 26276
     26276 26278 26278 26283 26283 26285 26286 26289 26290 26292 26293
     26296 26297 26299 26300 26302 26308 26311 26313 26316 26316 26318
     26319 26324 26324 26326 26326 26329 26333 26335 26336 26342 26342
     26344 26345 26347 26348 26350 26350 26352 26352 26354 26357 26359
     26368 26371 26371 26373 26373 26375 26377 26379 26379 26381 26383
     26387 26391 26393 26393 26395 26400 26402 26402 26406 26408 26410
     26414 26417 26417 26419 26420 26422 26424 26426 26426 26429 26431
     26433 26433 26437 26441 26444 26444 26446 26449 26451 26454 26457
     26457 26460 26470 26474 26474 26476 26487 26491 26492 26494 26495
     26497 26497 26500 26501 26503 26503 26505 26505 26507 26508 26510
     26513 26515 26515 26517 26525 26528 26530 26534 26534 26537 26537
     26543 26553 26555 26557 26560 26566 26568 26570 26574 26580 26583
     26586 26588 26590 26593 26594 26596 26596 26598 26599 26601 26601
     26604 26604 26606 26615 26617 26617 26619 26619 26622 26623 26626
     26628 26643 26644 26646 26647 26649 26649 26653 26655 26657 26658
     26663 26669 26671 26676 26680 26681 26683 26685 26687 26694 26696
     26696 26698 26698 26700 26702 26704 26709 26711 26713 26715 26717
     26719 26719 26723 26723 26727 26727 26731 26731 26734 26738 26740
     26743 26745 26748 26750 26751 26753 26758 26760 26760 26765 26765
     26767 26767 26771 26772 26774 26776 26778 26781 26783 26787 26789
     26794 26797 26803 26805 26806 26809 26812 26820 26822 26824 26829
     26831 26842 26844 26845 26847 26849 26851 26851 26853 26853 26855
     26856 26858 26866 26869 26870 26873 26877 26880 26881 26884 26886
     26888 26899 26902 26903 26905 26908 26913 26915 26917 26918 26920
     26920 26922 26922 26928 26929 26931 26934 26936 26937 26939 26939
     26941 26941 26943 26943 26946 26946 26949 26949 26953 26954 26958
     26958 26963 26965 26967 26967 26969 26974 26976 26982 26984 26997
     26999 27010 27018 27018 27021 27022 27025 27026 27028 27030 27032
     27032 27035 27036 27040 27041 27045 27048 27051 27051 27053 27055
     27057 27058 27060 27060 27063 27064 27066 27068 27070 27071 27073
     27073 27075 27075 27077 27077 27079 27080 27082 27086 27088 27089
     27091 27091 27094 27097 27101 27102 27106 27106 27109 27109 27111
     27112 27115 27115 27117 27119 27121 27123 27125 27125 27129 27129
     27131 27131 27133 27139 27141 27141 27146 27148 27151 27151 27153
     27157 27159 27159 27161 27163 27165 27172 27176 27179 27182 27182
     27184 27184 27186 27186 27188 27195 27197 27199 27204 27211 27214
     27214 27216 27218 27221 27222 27224 27225 27227 27227 27231 27231
     27233 27234 27236 27236 27238 27239 27242 27242 27249 27251 27256
     27256 27262 27265 27267 27268 27270 27271 27273 27273 27275 27275
     27277 27278 27280 27281 27287 27287 27291 27296 27298 27299 27301
     27301 27306 27308 27310 27313 27315 27316 27320 27320 27323 27323
     27325 27327 27329 27331 27334 27334 27336 27337 27340 27340 27344
     27345 27347 27350 27354 27359 27364 27364 27367 27368 27370 27370
     27372 27372 27376 27378 27386 27389 27394 27399 27401 27402 27407
     27410 27414 27415 27419 27419 27421 27425 27427 27428 27431 27432
     27435 27436 27439 27439 27442 27442 27445 27451 27453 27455 27459
     27459 27462 27463 27465 27466 27468 27470 27472 27472 27474 27476
     27478 27478 27480 27481 27483 27483 27485 27485 27487 27492 27494
     27495 27497 27499 27502 27504 27507 27509 27512 27513 27515 27515
     27517 27520 27522 27526 27529 27531 27533 27533 27541 27544 27547
     27547 27550 27552 27554 27556 27560 27573 27575 27584 27587 27590
     27593 27593 27595 27598 27602 27604 27606 27606 27608 27608 27610
     27611 27615 27615 27617 27617 27619 27619 27622 27623 27627 27628
     27630 27631 27633 27633 27635 27635 27639 27639 27641 27641 27647
     27647 27650 27650 27652 27653 27656 27657 27661 27668 27671 27671
     27673 27673 27675 27675 27679 27679 27683 27684 27686 27688 27692
     27692 27694 27694 27699 27704 27706 27707 27710 27714 27722 27723
     27725 27728 27730 27730 27732 27733 27735 27735 27737 27744 27746
     27746 27751 27752 27754 27755 27757 27757 27759 27760 27762 27764
     27766 27766 27768 27771 27773 27774 27777 27779 27781 27785 27788
     27789 27792 27792 27794 27804 27807 27807 27809 27810 27819 27819
     27822 27822 27824 27828 27832 27839 27841 27842 27844 27846 27849
     27850 27852 27853 27855 27863 27865 27869 27872 27875 27877 27877
     27879 27884 27886 27892 27908 27908 27911 27911 27914 27916 27918
     27919 27921 27923 27927 27927 27929 27931 27934 27935 27941 27947
     27950 27951 27953 27955 27957 27958 27960 27961 27963 27967 27969
     27969 27972 27973 27991 27991 27993 27994 27996 27996 27998 27999
     28001 28001 28003 28007 28009 28010 28012 28012 28014 28016 28020
     28020 28023 28025 28028 28028 28034 28034 28037 28037 28039 28040
     28044 28044 28046 28046 28049 28057 28059 28060 28074 28074 28076
     28076 28079 28079 28082 28082 28084 28085 28087 28089 28092 28093
     28095 28096 28100 28104 28106 28108 28110 28111 28113 28114 28117
     28118 28120 28121 28123 28123 28125 28130 28132 28134 28136 28140
     28142 28145 28147 28151 28153 28156 28160 28160 28164 28165 28167
     28171 28179 28179 28181 28181 28185 28187 28189 28199 28201 28201
     28203 28207 28210 28210 28214 28214 28216 28220 28222 28222 28227
     28229 28232 28235 28237 28239 28241 28244 28246 28248 28251 28255
     28258 28259 28263 28264 28267 28267 28270 28271 28274 28275 28278
     28278 28283 28283 28285 28288 28290 28290 28300 28301 28303 28304
     28307 28307 28310 28310 28312 28313 28316 28317 28319 28320 28322
     28322 28325 28325 28327 28327 28330 28330 28333 28335 28337 28339
     28342 28343 28346 28347 28349 28349 28351 28357 28359 28367 28369
     28369 28371 28373 28381 28382 28395 28399 28402 28402 28404 28404
     28407 28409 28411 28411 28413 28415 28417 28418 28420 28420 28422
     28422 28424 28426 28428 28429 28431 28431 28433 28433 28435 28438
     28440 28440 28442 28443 28448 28448 28450 28451 28454 28454 28457
     28461 28463 28467 28470 28470 28472 28472 28475 28476 28478 28479
     28481 28481 28485 28485 28495 28495 28497 28500 28503 28511 28513
     28514 28516 28516 28518 28518 28520 28520 28524 28528 28532 28532
     28536 28536 28538 28538 28540 28542 28544 28548 28550 28552 28555
     28558 28560 28564 28566 28567 28570 28570 28575 28577 28579 28584
     28586 28586 28590 28593 28595 28595 28597 28598 28601 28601 28604
     28604 28608 28611 28613 28616 28618 28618 28628 28629 28632 28632
     28634 28635 28638 28641 28644 28644 28648 28649 28651 28652 28654
     28657 28659 28659 28661 28662 28665 28666 28668 28670 28672 28673
     28677 28679 28681 28681 28683 28683 28685 28685 28687 28687 28689
     28689 28693 28693 28695 28696 28698 28699 28701 28704 28707 28707
     28710 28711 28716 28716 28719 28720 28722 28722 28724 28724 28727
     28727 28729 28729 28732 28732 28734 28734 28739 28740 28744 28748
     28750 28750 28753 28753 28756 28757 28760 28760 28765 28766 28771
     28773 28779 28780 28782 28784 28789 28790 28792 28792 28796 28798
     28801 28801 28805 28806 28809 28810 28814 28814 28818 28818 28820
     28825 28827 28827 28836 28836 28843 28849 28851 28852 28855 28858
     28872 28872 28874 28875 28879 28879 28881 28881 28883 28886 28888
     28889 28892 28893 28895 28895 28900 28900 28913 28913 28921 28922
     28925 28925 28931 28935 28937 28937 28939 28940 28943 28943 28948
     28948 28953 28954 28956 28956 28958 28958 28960 28961 28966 28966
     28971 28971 28973 28973 28975 28977 28982 28982 28984 28984 28988
     28988 28993 28993 28997 28999 29001 29004 29006 29006 29008 29008
     29010 29010 29013 29015 29017 29018 29020 29020 29022 29022 29024
     29024 29026 29026 29028 29033 29036 29036 29038 29038 29049 29049
     29053 29053 29056 29056 29060 29061 29063 29064 29066 29066 29068
     29068 29071 29071 29074 29074 29076 29077 29081 29083 29087 29088
     29090 29090 29096 29096 29100 29100 29103 29107 29113 29114 29118
     29121 29123 29124 29128 29129 29131 29132 29134 29134 29136 29136
     29138 29143 29145 29146 29148 29148 29151 29152 29157 29159 29164
     29166 29173 29173 29176 29177 29179 29180 29182 29184 29190 29193
     29197 29197 29200 29200 29203 29203 29207 29207 29210 29211 29213
     29213 29215 29215 29220 29220 29224 29224 29226 29229 29231 29232
     29234 29234 29236 29238 29240 29251 29253 29256 29259 29260 29262
     29264 29266 29267 29269 29270 29272 29283 29287 29289 29291 29291
     29294 29295 29297 29298 29300 29300 29303 29305 29307 29314 29316
     29316 29319 29319 29321 29321 29325 29326 29330 29331 29334 29334
     29339 29339 29344 29344 29346 29346 29351 29352 29356 29359 29361
     29362 29364 29364 29366 29366 29369 29369 29374 29374 29377 29380
     29382 29383 29385 29385 29388 29388 29390 29390 29392 29392 29394
     29394 29397 29401 29403 29403 29407 29410 29413 29413 29417 29417
     29420 29421 29427 29428 29431 29438 29442 29442 29444 29445 29447
     29447 29450 29451 29453 29453 29458 29459 29462 29465 29467 29471
     29474 29474 29476 29477 29479 29484 29486 29487 29489 29490 29492
     29495 29498 29499 29501 29503 29507 29509 29517 29520 29522 29522
     29526 29528 29533 29536 29539 29539 29542 29548 29550 29554 29557
     29557 29559 29564 29568 29569 29571 29575 29577 29577 29579 29579
     29582 29582 29584 29584 29587 29587 29589 29592 29596 29596 29598
     29600 29602 29602 29605 29606 29609 29611 29613 29613 29618 29619
     29621 29621 29623 29623 29625 29625 29627 29629 29631 29632 29634
     29634 29637 29638 29640 29647 29650 29651 29654 29654 29657 29657
     29661 29662 29664 29665 29667 29667 29669 29671 29673 29674 29677
     29678 29681 29681 29684 29685 29687 29691 29693 29697 29699 29703
     29705 29706 29713 29713 29722 29723 29730 29730 29732 29734 29736
     29750 29753 29754 29759 29761 29763 29764 29766 29767 29771 29771
     29773 29773 29777 29778 29781 29781 29783 29783 29785 29792 29794
     29796 29798 29803 29805 29811 29814 29814 29822 29822 29824 29825
     29827 29827 29829 29831 29833 29833 29835 29835 29839 29842 29848
     29850 29852 29852 29854 29859 29862 29867 29870 29874 29877 29877
     29881 29881 29883 29883 29885 29885 29887 29887 29896 29898 29900
     29900 29903 29904 29907 29908 29912 29912 29914 29916 29918 29920
     29922 29924 29926 29931 29934 29938 29940 29940 29942 29944 29946
     29948 29951 29951 29955 29958 29964 29966 29969 29971 29973 29976
     29978 29978 29980 29980 29982 29985 29987 29996 29999 30003 30006
     30016 30019 30020 30022 30034 30036 30036 30039 30039 30041 30050
     30052 30055 30057 30059 30061 30061 30063 30065 30067 30068 30070
     30079 30081 30082 30085 30087 30089 30091 30094 30101 30105 30106
     30108 30109 30114 30117 30123 30123 30129 30133 30136 30138 30140
     30151 30154 30154 30156 30159 30162 30162 30164 30165 30167 30169
     30171 30172 30174 30180 30183 30183 30185 30185 30188 30188 30190
     30196 30201 30202 30204 30204 30206 30212 30215 30221 30223 30223
     30226 30227 30229 30230 30233 30233 30235 30247 30249 30249 30253
     30253 30256 30256 30258 30261 30264 30268 30272 30284 30290 30290
     30293 30294 30296 30297 30300 30300 30303 30303 30305 30306 30308
     30309 30311 30314 30316 30322 30324 30324 30326 30326 30328 30328
     30330 30334 30336 30337 30339 30344 30347 30350 30352 30352 30355
     30355 30357 30358 30361 30365 30367 30368 30370 30376 30378 30378
     30381 30382 30384 30384 30388 30388 30391 30394 30397 30397 30399
     30399 30401 30403 30405 30406 30408 30414 30418 30418 30420 30420
     30422 30423 30425 30425 30427 30428 30430 30433 30435 30440 30442
     30442 30444 30444 30446 30446 30448 30450 30452 30452 30454 30454
     30456 30457 30459 30460 30462 30462 30464 30465 30468 30468 30470
     30476 30478 30478 30482 30482 30484 30485 30487 30487 30489 30492
     30494 30496 30498 30498 30500 30502 30504 30505 30509 30511 30516
     30522 30524 30526 30528 30528 30530 30530 30533 30535 30538 30538
     30541 30543 30546 30546 30550 30551 30554 30556 30558 30568 30570
     30572 30576 30576 30578 30580 30585 30586 30589 30592 30596 30596
     30603 30606 30609 30609 30612 30614 30618 30618 30622 30624 30626
     30626 30629 30629 30631 30631 30634 30634 30636 30641 30643 30643
     30645 30646 30649 30649 30651 30655 30659 30659 30663 30663 30665
     30665 30669 30669 30673 30674 30677 30677 30679 30679 30681 30684
     30686 30688 30690 30695 30697 30698 30700 30705 30707 30708 30712
     30712 30715 30716 30722 30722 30725 30726 30729 30729 30732 30734
     30737 30738 30740 30741 30749 30749 30752 30755 30757 30759 30765
     30766 30768 30768 30770 30770 30772 30773 30775 30775 30778 30778
     30783 30783 30787 30789 30791 30792 30796 30796 30798 30798 30802
     30802 30812 30814 30816 30817 30819 30820 30824 30824 30826 30828
     30830 30831 30834 30834 30836 30836 30842 30842 30844 30844 30846
     30846 30849 30849 30854 30855 30858 30858 30860 30863 30865 30865
     30867 30869 30871 30872 30874 30874 30877 30879 30881 30881 30883
     30884 30887 30890 30892 30893 30895 30899 30901 30901 30906 30911
     30913 30913 30917 30924 30926 30926 30928 30934 30938 30939 30943
     30945 30948 30948 30950 30952 30954 30954 30956 30956 30959 30959
     30962 30964 30966 30967 30970 30971 30973 30973 30975 30977 30982
     30983 30988 30988 30990 30990 30992 30994 31001 31002 31004 31004
     31006 31008 31013 31015 31017 31021 31025 31025 31028 31029 31034
     31041 31044 31051 31055 31057 31059 31064 31066 31072 31074 31074
     31077 31077 31079 31081 31083 31083 31085 31085 31090 31090 31095
     31095 31097 31100 31102 31105 31108 31109 31114 31119 31121 31121
     31123 31126 31128 31128 31131 31133 31137 31137 31142 31147 31150
     31153 31155 31156 31160 31163 31165 31170 31172 31172 31175 31179
     31183 31183 31185 31186 31188 31190 31192 31192 31194 31194 31197
     31207 31209 31213 31216 31217 31224 31224 31227 31228 31232 31232
     31234 31235 31239 31246 31249 31249 31252 31253 31255 31260 31262
     31265 31271 31271 31275 31275 31277 31282 31284 31285 31287 31296
     31298 31305 31308 31312 31317 31319 31321 31321 31324 31325 31327
     31331 31333 31333 31335 31335 31337 31339 31341 31341 31344 31344
     31348 31350 31352 31354 31357 31366 31368 31368 31370 31371 31376
     31384 31390 31392 31395 31395 31401 31402 31404 31404 31406 31408
     31411 31411 31413 31414 31417 31420 31423 31423 31427 31439 31441
     31443 31445 31445 31449 31453 31455 31459 31461 31462 31464 31469
     31471 31473 31476 31476 31478 31478 31480 31483 31485 31487 31490
     31490 31492 31492 31494 31496 31498 31499 31503 31503 31505 31505
     31508 31508 31512 31513 31515 31515 31518 31520 31523 31523 31525
     31537 31539 31542 31545 31545 31549 31549 31551 31553 31557 31561
     31563 31570 31572 31574 31581 31581 31584 31584 31588 31591 31593
     31594 31596 31605 31607 31607 31610 31610 31620 31620 31622 31623
     31625 31625 31627 31627 31629 31634 31636 31649 31653 31653 31658
     31658 31660 31661 31663 31666 31668 31670 31672 31672 31674 31677
     31680 31682 31684 31692 31695 31695 31700 31700 31702 31703 31705
     31707 31709 31709 31712 31712 31716 31718 31720 31722 31725 31725
     31730 31738 31740 31740 31742 31742 31744 31748 31750 31751 31753
     31753 31755 31759 31761 31764 31767 31767 31769 31769 31771 31771
     31775 31777 31779 31779 31781 31784 31786 31788 31793 31793 31795
     31796 31798 31802 31805 31808 31811 31811 31814 31814 31818 31818
     31820 31821 31823 31830 31832 31841 31843 31845 31847 31847 31849
     31849 31852 31854 31856 31856 31858 31859 31861 31861 31865 31865
     31868 31870 31873 31875 31878 31879 31881 31881 31883 31883 31885
     31885 31887 31888 31890 31890 31892 31893 31895 31896 31899 31899
     31902 31906 31908 31912 31915 31915 31917 31918 31920 31923 31926
     31927 31929 31936 31938 31938 31940 31941 31943 31946 31949 31951
     31954 31962 31964 31968 31970 31970 31974 31975 31977 31977 31979
     31979 31983 31983 31986 31986 31988 31990 31992 31992 31994 31995
     31998 31998 32000 32000 32002 32011 32013 32013 32015 32030 32032
     32035 32038 32038 32042 32051 32053 32053 32057 32058 32060 32072
     32075 32081 32083 32083 32086 32087 32089 32094 32097 32099 32101
     32104 32106 32106 32110 32110 32112 32115 32117 32118 32120 32123
     32125 32125 32127 32127 32129 32131 32133 32134 32136 32137 32139
     32141 32143 32143 32145 32145 32147 32147 32150 32151 32153 32159
     32162 32163 32166 32167 32170 32187 32189 32191 32194 32199 32202
     32207 32209 32210 32213 32213 32215 32218 32220 32222 32224 32226
     32228 32230 32232 32237 32239 32239 32241 32242 32244 32246 32249
     32251 32256 32257 32260 32261 32264 32267 32272 32274 32277 32277
     32279 32279 32283 32291 32294 32296 32299 32303 32305 32307 32309
     32311 32313 32315 32317 32319 32321 32321 32323 32327 32330 32331
     32333 32334 32336 32336 32338 32338 32340 32342 32344 32346 32349
     32351 32353 32354 32357 32359 32361 32363 32365 32368 32371 32371
     32376 32377 32379 32383 32385 32387 32390 32394 32396 32406 32408
     32408 32410 32414 32566 32566 32568 32568 32570 32575 32579 32581
     32583 32583 32588 32597 32600 32600 32603 32605 32607 32609 32611
     32619 32621 32622 32624 32626 32629 32629 32631 32633 32637 32640
     32642 32643 32645 32648 32650 32657 32660 32660 32662 32663 32666
     32666 32668 32670 32673 32676 32678 32678 32680 32682 32685 32687
     32690 32690 32692 32692 32694 32694 32696 32697 32700 32701 32703
     32705 32707 32707 32709 32710 32712 32712 32714 32714 32716 32716
     32718 32719 32722 32722 32724 32725 32731 32731 32735 32737 32739
     32739 32741 32742 32744 32745 32747 32748 32750 32752 32754 32755
     32761 32769 32771 32776 32778 32793 32796 32801 32804 32804 32806
     32806 32808 32808 32812 32812 32814 32814 32816 32816 32819 32823
     32825 32832 32836 32836 32838 32838 32842 32842 32850 32850 32854
     32854 32856 32856 32858 32858 32862 32866 32868 32868 32870 32870
     32872 32872 32877 32877 32879 32887 32889 32889 32893 32895 32897
     32897 32900 32905 32907 32908 32910 32910 32915 32915 32918 32918
     32920 32920 32922 32926 32929 32930 32933 32935 32937 32941 32943
     32943 32945 32946 32948 32948 32952 32954 32963 32964 32966 32966
     32968 32968 32972 32975 32978 32978 32980 32987 32989 32990 32992
     32993 32996 32997 33005 33012 33014 33014 33016 33018 33020 33022
     33026 33027 33029 33035 33046 33048 33050 33052 33054 33054 33056
     33056 33059 33060 33063 33063 33065 33065 33068 33068 33071 33073
     33075 33075 33077 33077 33081 33082 33084 33084 33086 33086 33093
     33095 33098 33100 33102 33102 33104 33109 33111 33111 33119 33121
     33125 33129 33131 33131 33133 33137 33140 33140 33143 33146 33151
     33158 33160 33160 33162 33163 33166 33168 33171 33171 33173 33174
     33176 33176 33178 33182 33184 33184 33186 33188 33192 33193 33198
     33198 33200 33200 33202 33205 33208 33208 33210 33211 33213 33216
     33218 33219 33221 33222 33224 33227 33229 33231 33233 33233 33235
     33235 33237 33237 33239 33243 33245 33249 33251 33253 33255 33256
     33258 33261 33264 33270 33272 33283 33285 33285 33287 33290 33292
     33296 33298 33300 33302 33311 33313 33314 33320 33324 33326 33326
     33330 33338 33344 33344 33347 33351 33355 33355 33358 33359 33361
     33361 33366 33366 33368 33370 33372 33373 33375 33376 33378 33380
     33382 33384 33386 33387 33389 33391 33393 33394 33396 33396 33398
     33400 33403 33403 33405 33409 33411 33412 33415 33415 33417 33419
     33421 33422 33425 33426 33428 33428 33430 33430 33432 33435 33437
     33437 33439 33441 33443 33460 33463 33471 33477 33478 33488 33493
     33495 33495 33497 33500 33502 33512 33514 33515 33517 33517 33519
     33519 33521 33521 33523 33524 33526 33527 33529 33531 33533 33534
     33536 33547 33550 33550 33558 33560 33563 33567 33569 33571 33576
     33576 33579 33594 33596 33597 33600 33600 33602 33605 33607 33607
     33609 33610 33613 33624 33648 33648 33651 33651 33653 33653 33655
     33656 33659 33661 33663 33664 33666 33666 33668 33671 33673 33674
     33677 33678 33682 33686 33688 33696 33698 33698 33702 33709 33713
     33713 33717 33717 33725 33729 33733 33733 33735 33735 33737 33738
     33740 33740 33742 33745 33747 33748 33750 33750 33752 33752 33756
     33757 33759 33760 33768 33771 33775 33778 33780 33780 33782 33785
     33787 33789 33793 33793 33795 33796 33798 33799 33802 33807 33809
     33809 33811 33811 33813 33813 33817 33817 33824 33824 33826 33826
     33833 33834 33836 33836 33839 33839 33841 33841 33845 33845 33848
     33849 33852 33853 33861 33866 33869 33871 33873 33874 33878 33884
     33888 33895 33897 33905 33907 33914 33916 33917 33921 33922 33924
     33925 33931 33931 33936 33936 33938 33941 33945 33945 33948 33948
     33950 33951 33953 33953 33958 33958 33960 33962 33965 33965 33967
     33967 33969 33970 33972 33972 33976 33986 33988 33988 33990 33997
     33999 34001 34003 34003 34006 34006 34009 34010 34012 34012 34023
     34023 34026 34026 34028 34028 34030 34034 34036 34036 34039 34039
     34042 34045 34047 34048 34050 34051 34054 34055 34060 34060 34062
     34062 34064 34065 34067 34069 34071 34072 34074 34074 34076 34076
     34078 34079 34081 34087 34090 34093 34095 34095 34098 34102 34109
     34109 34111 34113 34115 34115 34118 34118 34120 34123 34126 34131
     34133 34138 34140 34148 34152 34155 34157 34157 34159 34159 34167
     34167 34169 34171 34173 34177 34180 34188 34191 34193 34195 34196
     34199 34201 34203 34205 34207 34208 34210 34210 34212 34223 34228
     34228 34230 34234 34236 34239 34241 34242 34247 34247 34249 34251
     34253 34256 34261 34261 34264 34264 34266 34266 34268 34269 34271
     34272 34276 34278 34280 34282 34285 34285 34291 34291 34294 34295
     34297 34300 34302 34304 34306 34306 34308 34311 34314 34315 34317
     34318 34320 34323 34326 34331 34334 34334 34337 34338 34343 34343
     34345 34345 34349 34349 34351 34352 34358 34358 34360 34360 34362
     34362 34364 34365 34367 34370 34374 34374 34381 34382 34384 34384
     34386 34394 34396 34404 34407 34407 34409 34409 34411 34412 34415
     34415 34417 34417 34421 34423 34425 34427 34440 34440 34442 34445
     34449 34449 34451 34451 34453 34454 34456 34456 34458 34458 34460
     34460 34465 34465 34467 34468 34470 34475 34477 34477 34479 34481
     34483 34489 34495 34497 34499 34503 34505 34505 34507 34507 34509
     34510 34513 34514 34516 34517 34519 34519 34521 34524 34526 34528
     34531 34533 34535 34535 34537 34537 34540 34543 34552 34558 34560
     34560 34562 34571 34573 34580 34584 34586 34588 34588 34590 34591
     34593 34593 34595 34595 34597 34597 34600 34601 34606 34607 34609
     34610 34612 34612 34615 34615 34617 34624 34627 34627 34629 34629
     34633 34633 34635 34638 34643 34643 34645 34645 34647 34649 34653
     34653 34655 34657 34659 34662 34664 34664 34666 34666 34670 34671
     34673 34674 34676 34676 34678 34678 34680 34680 34683 34683 34687
     34687 34690 34697 34699 34701 34704 34704 34707 34707 34709 34709
     34711 34713 34718 34720 34722 34723 34727 34727 34731 34735 34737
     34737 34739 34739 34741 34741 34746 34747 34749 34753 34756 34756
     34758 34763 34766 34766 34768 34768 34770 34770 34773 34774 34777
     34778 34780 34780 34783 34784 34786 34788 34794 34795 34797 34797
     34799 34799 34801 34803 34806 34811 34814 34815 34817 34817 34819
     34819 34821 34822 34825 34827 34829 34838 34840 34844 34846 34847
     34849 34851 34855 34856 34861 34862 34864 34866 34869 34870 34873
     34876 34880 34886 34888 34894 34897 34899 34901 34916 34920 34921
     34923 34923 34928 34930 34933 34933 34935 34935 34937 34937 34939
     34939 34941 34946 34952 34952 34955 34955 34957 34957 34962 34962
     34966 34972 34974 34976 34978 34978 34980 34980 34984 34984 34986
     34987 34990 34990 34992 34993 34996 34997 34999 34999 35002 35002
     35005 35013 35018 35023 35025 35029 35032 35033 35035 35039 35041
     35041 35047 35048 35055 35061 35063 35065 35068 35070 35073 35074
     35076 35076 35078 35079 35082 35082 35084 35088 35090 35091 35093
     35094 35096 35098 35100 35102 35104 35104 35109 35112 35114 35115
     35120 35122 35125 35126 35128 35131 35134 35134 35136 35142 35145
     35145 35148 35149 35151 35151 35154 35154 35158 35159 35162 35164
     35166 35172 35174 35174 35178 35179 35181 35184 35186 35189 35191
     35191 35194 35199 35201 35201 35203 35203 35206 35211 35213 35213
     35215 35216 35219 35224 35226 35228 35231 35233 35237 35239 35241
     35242 35244 35244 35247 35248 35250 35255 35258 35258 35260 35261
     35263 35264 35282 35282 35284 35288 35290 35290 35292 35293 35299
     35299 35301 35303 35305 35305 35307 35307 35309 35309 35313 35313
     35315 35316 35318 35318 35320 35321 35325 35325 35327 35328 35330
     35333 35335 35336 35338 35338 35340 35340 35342 35352 35355 35355
     35357 35360 35362 35366 35370 35373 35375 35375 35377 35377 35379
     35383 35386 35390 35392 35393 35395 35395 35397 35401 35405 35406
     35408 35416 35419 35422 35424 35427 35429 35431 35433 35433 35435
     35438 35440 35443 35445 35447 35449 35452 35454 35456 35458 35463
     35465 35465 35467 35469 35471 35475 35477 35482 35486 35489 35491
     35497 35500 35504 35506 35507 35510 35511 35513 35513 35515 35516
     35518 35519 35522 35524 35526 35533 35535 35535 35537 35543 35546
     35554 35556 35556 35558 35559 35563 35566 35568 35569 35571 35576
     35578 35578 35580 35580 35582 35586 35588 35591 35594 35596 35598
     35598 35600 35601 35604 35604 35606 35607 35609 35617 35622 35622
     35624 35624 35627 35629 35632 35632 35635 35635 35639 35639 35641
     35641 35644 35644 35646 35646 35649 35654 35656 35657 35660 35663
     35666 35668 35670 35670 35672 35676 35678 35679 35683 35683 35686
     35686 35691 35693 35695 35698 35700 35700 35702 35705 35708 35710
     35712 35713 35715 35717 35722 35728 35730 35734 35737 35738 35740
     35740 35742 35743 35895 35898 35901 35903 35905 35905 35909 35916
     35918 35921 35923 35925 35927 35931 35933 35933 35937 35940 35942
     35942 35944 35949 35955 35955 35957 35958 35960 35964 35966 35966
     35970 35970 35973 35975 35977 35982 35984 35984 35986 35988 35992
     35993 35995 35998 36000 36002 36004 36004 36007 36016 36018 36020
     36022 36029 36031 36043 36045 36047 36049 36049 36051 36051 36053
     36054 36057 36062 36064 36068 36070 36070 36072 36072 36074 36074
     36076 36077 36079 36080 36082 36082 36085 36085 36087 36088 36090
     36095 36097 36097 36099 36101 36103 36107 36109 36109 36111 36112
     36114 36116 36118 36119 36123 36123 36196 36199 36201 36201 36203
     36206 36208 36209 36211 36212 36215 36215 36223 36223 36225 36226
     36228 36229 36232 36232 36234 36234 36237 36237 36240 36241 36245
     36245 36249 36249 36254 36256 36259 36259 36262 36262 36264 36264
     36267 36268 36271 36271 36274 36275 36277 36277 36279 36279 36281
     36284 36286 36286 36288 36288 36290 36290 36293 36296 36298 36300
     36302 36303 36305 36305 36308 36311 36313 36315 36317 36317 36319
     36319 36321 36321 36323 36325 36327 36328 36330 36332 36335 36341
     36348 36349 36351 36351 36353 36353 36356 36358 36360 36363 36367
     36369 36372 36372 36374 36374 36381 36387 36390 36391 36394 36394
     36400 36401 36403 36409 36413 36413 36416 36418 36420 36420 36423
     36432 36436 36437 36441 36441 36443 36452 36457 36457 36460 36461
     36463 36466 36468 36468 36470 36470 36473 36476 36481 36485 36487
     36487 36489 36491 36493 36493 36496 36501 36505 36507 36509 36510
     36513 36514 36519 36519 36521 36529 36531 36531 36533 36533 36538
     36539 36542 36542 36544 36545 36547 36552 36554 36557 36559 36559
     36561 36562 36564 36564 36571 36572 36575 36575 36578 36579 36584
     36584 36587 36587 36589 36590 36592 36593 36599 36606 36608 36608
     36610 36611 36613 36613 36615 36618 36620 36620 36623 36624 36626
     36633 36635 36641 36643 36643 36645 36650 36652 36655 36659 36667
     36670 36679 36681 36681 36684 36687 36689 36693 36695 36696 36700
     36703 36705 36709 36763 36769 36771 36776 36781 36786 36789 36792
     36794 36796 36798 36802 36804 36806 36810 36811 36813 36814 36816
     36821 36826 36826 36832 36832 36834 36838 36840 36843 36845 36849
     36852 36859 36861 36862 36864 36870 36872 36872 36875 36881 36883
     36891 36893 36899 36903 36906 36908 36911 36913 36921 36924 36924
     36926 36927 36929 36933 36935 36935 36937 36950 36952 36953 36955
     36958 36960 36963 36965 36969 36972 36976 36978 36978 36980 36986
     36988 36989 36991 36997 36999 37004 37006 37009 37013 37013 37015
     37017 37019 37019 37024 37027 37029 37030 37032 37032 37034 37034
     37039 37046 37048 37048 37053 37054 37057 37057 37059 37061 37063
     37064 37066 37066 37068 37068 37070 37070 37074 37074 37077 37077
     37079 37081 37083 37085 37087 37087 37089 37090 37092 37093 37096
     37096 37099 37099 37101 37101 37103 37104 37108 37111 37117 37120
     37122 37122 37124 37126 37128 37128 37133 37133 37136 37136 37138
     37138 37140 37140 37142 37146 37148 37148 37150 37150 37152 37152
     37154 37155 37157 37157 37159 37159 37161 37161 37165 37170 37172
     37172 37174 37175 37177 37178 37180 37181 37187 37187 37191 37199
     37202 37204 37206 37211 37217 37221 37223 37223 37225 37226 37228
     37229 37234 37237 37239 37243 37249 37251 37253 37255 37257 37259
     37261 37262 37264 37269 37271 37272 37276 37276 37278 37278 37281
     37282 37284 37284 37286 37286 37288 37288 37290 37302 37304 37304
     37306 37309 37311 37315 37317 37321 37323 37329 37331 37332 37334
     37343 37345 37345 37347 37351 37353 37354 37356 37361 37365 37367
     37369 37369 37371 37373 37375 37377 37380 37383 37385 37386 37388
     37390 37392 37398 37400 37400 37404 37406 37411 37414 37416 37417
     37420 37420 37422 37424 37427 37434 37436 37436 37438 37440 37442
     37451 37453 37457 37463 37470 37472 37474 37476 37481 37486 37489
     37493 37497 37499 37504 37507 37507 37509 37509 37512 37514 37517
     37518 37521 37523 37525 37532 37535 37536 37540 37541 37543 37544
     37547 37547 37549 37549 37551 37551 37554 37554 37558 37565 37567
     37571 37573 37576 37579 37584 37586 37587 37589 37589 37591 37593
     37596 37597 37599 37601 37603 37605 37607 37610 37612 37614 37616
     37616 37618 37619 37624 37628 37631 37632 37634 37634 37638 37638
     37640 37640 37645 37645 37647 37649 37652 37653 37656 37658 37660
     37676 37678 37679 37682 37687 37690 37691 37700 37700 37703 37705
     37707 37707 37709 37709 37712 37714 37716 37720 37722 37724 37726
     37726 37728 37728 37732 37733 37735 37735 37737 37738 37740 37745
     37747 37750 37754 37754 37756 37762 37768 37768 37770 37773 37775
     37775 37778 37778 37780 37784 37786 37787 37790 37790 37793 37793
     37795 37796 37798 37801 37803 37806 37808 37808 37812 37814 37817
     37818 37825 37825 37827 37837 37840 37841 37843 37843 37846 37849
     37852 37855 37857 37858 37860 37864 37879 37883 37885 37885 37889
     37892 37895 37897 37901 37904 37907 37914 37919 37919 37921 37921
     37931 37931 37934 37935 37937 37942 37944 37944 37946 37947 37949
     37949 37951 37951 37953 37953 37955 37957 37960 37960 37962 37962
     37964 37964 37969 37971 37973 37973 37977 37980 37982 37987 37992
     37992 37994 37995 37997 38002 38005 38005 38007 38007 38012 38015
     38017 38017 38019 38020 38263 38265 38270 38270 38272 38272 38274
     38276 38279 38287 38289 38289 38291 38292 38294 38294 38296 38297
     38301 38313 38315 38317 38322 38322 38324 38324 38326 38326 38329
     38335 38339 38339 38342 38349 38352 38358 38360 38362 38364 38370
     38372 38374 38428 38430 38433 38434 38436 38438 38440 38440 38442
     38442 38444 38444 38446 38447 38449 38451 38455 38461 38463 38466
     38468 38468 38475 38477 38479 38480 38482 38482 38484 38484 38486
     38488 38491 38495 38497 38502 38506 38506 38508 38508 38510 38510
     38512 38512 38514 38520 38522 38527 38529 38534 38536 38539 38541
     38543 38545 38545 38548 38557 38559 38560 38563 38570 38574 38580
     38582 38588 38592 38593 38596 38599 38601 38606 38609 38610 38613
     38614 38616 38623 38626 38627 38632 38635 38639 38642 38646 38647
     38649 38651 38656 38656 38658 38666 38669 38671 38673 38673 38675
     38675 38678 38678 38681 38686 38689 38692 38695 38696 38698 38698
     38704 38707 38712 38713 38717 38718 38721 38724 38726 38726 38728
     38730 38734 38735 38738 38738 38741 38748 38750 38750 38752 38756
     38758 38763 38765 38766 38769 38769 38771 38772 38774 38781 38783
     38785 38788 38790 38793 38793 38795 38795 38797 38797 38799 38800
     38805 38810 38812 38812 38814 38816 38818 38819 38822 38822 38824
     38824 38827 38830 38833 38838 38840 38842 38844 38844 38846 38847
     38849 38849 38851 38862 38864 38865 38867 38868 38871 38873 38875
     38878 38880 38881 38884 38884 38893 38895 38897 38904 38906 38907
     38911 38911 38913 38915 38917 38920 38922 38922 38924 38932 38934
     38938 38940 38940 38942 38942 38944 38945 38947 38950 38955 38960
     38962 38965 38967 38968 38971 38974 38980 38980 38982 38983 38986
     38991 38993 39003 39006 39006 39010 39011 39013 39015 39018 39020
     39023 39025 39027 39028 39080 39080 39082 39083 39085 39089 39092
     39092 39094 39096 39098 39099 39103 39103 39106 39110 39112 39112
     39116 39116 39131 39132 39135 39135 39137 39139 39141 39143 39145
     39147 39149 39151 39154 39156 39158 39158 39164 39166 39170 39171
     39173 39173 39175 39178 39180 39180 39184 39192 39194 39202 39204
     39204 39206 39208 39211 39212 39214 39214 39217 39221 39225 39230
     39232 39234 39237 39241 39243 39246 39248 39250 39252 39253 39255
     39257 39259 39260 39262 39264 39318 39321 39323 39323 39325 39325
     39327 39327 39333 39334 39336 39336 39340 39342 39344 39349 39353
     39354 39356 39357 39359 39359 39361 39361 39363 39366 39368 39369
     39376 39381 39384 39391 39394 39394 39399 39399 39402 39406 39408
     39410 39412 39413 39416 39417 39419 39419 39421 39423 39425 39429
     39435 39436 39438 39443 39446 39446 39449 39449 39454 39454 39456
     39456 39458 39460 39463 39464 39467 39467 39469 39470 39472 39472
     39475 39475 39477 39480 39486 39486 39488 39493 39495 39495 39498
     39502 39505 39505 39508 39511 39514 39515 39517 39517 39519 39519
     39522 39522 39524 39525 39529 39531 39592 39592 39594 39594 39596
     39600 39602 39602 39604 39606 39608 39609 39611 39612 39614 39617
     39619 39620 39622 39622 39624 39624 39630 39640 39643 39644 39646
     39648 39650 39655 39657 39663 39665 39669 39671 39671 39673 39675
     39677 39677 39679 39686 39688 39689 39691 39694 39696 39696 39698
     39698 39702 39702 39704 39708 39711 39712 39714 39715 39717 39723
     39725 39727 39729 39733 39735 39735 39737 39741 39745 39749 39752
     39752 39755 39759 39761 39761 39764 39768 39770 39771 39774 39774
     39777 39777 39779 39779 39781 39782 39784 39784 39786 39791 39795
     39797 39799 39801 39807 39808 39811 39815 39817 39819 39821 39828
     39830 39831 39834 39834 39837 39840 39846 39854 39856 39858 39860
     39860 39863 39865 39867 39868 39870 39873 39878 39882 39886 39890
     39892 39892 39894 39896 39899 39899 39901 39901 39903 39903 39905
     39909 39911 39912 39914 39915 39919 39923 39925 39925 39927 39930
     39933 39933 39935 39936 39938 39938 39940 39940 39942 39942 39944
     39949 39951 39958 39960 39964 39966 39966 39969 39978 39981 39986
     39989 39991 39993 39995 39997 39998 40001 40001 40003 40010 40014
     40016 40018 40020 40022 40024 40026 40032 40035 40035 40039 40043
     40046 40046 40048 40048 40050 40050 40053 40056 40059 40059 40165
     40167 40169 40169 40171 40172 40176 40176 40178 40180 40182 40183
     40185 40185 40194 40195 40198 40201 40203 40203 40206 40206 40209
     40210 40213 40213 40215 40216 40219 40223 40227 40227 40230 40230
     40232 40232 40234 40236 40239 40240 40242 40244 40250 40255 40257
     40264 40266 40266 40272 40273 40275 40276 40281 40281 40284 40293
     40297 40300 40303 40304 40306 40306 40310 40311 40314 40316 40318
     40318 40323 40324 40326 40327 40329 40330 40333 40335 40338 40339
     40341 40344 40346 40346 40353 40353 40356 40356 40361 40364 40366
     40367 40369 40370 40372 40373 40376 40380 40383 40383 40385 40388
     40390 40391 40393 40394 40399 40399 40403 40407 40409 40410 40414
     40416 40421 40423 40425 40425 40427 40427 40429 40432 40434 40436
     40440 40442 40445 40446 40450 40450 40455 40455 40458 40458 40462
     40462 40464 40466 40469 40470 40473 40478 40565 40565 40568 40573
     40575 40581 40583 40584 40587 40588 40590 40591 40593 40595 40597
     40600 40603 40603 40605 40607 40612 40614 40616 40618 40620 40624
     40627 40629 40632 40636 40638 40639 40644 40644 40646 40646 40648
     40648 40651 40656 40658 40658 40660 40661 40664 40665 40667 40672
     40676 40677 40679 40680 40684 40690 40692 40697 40699 40701 40703
     40703 40706 40707 40711 40713 40718 40727 40729 40731 40735 40738
     40742 40742 40746 40748 40751 40751 40753 40754 40756 40756 40759
     40759 40761 40767 40769 40769 40771 40775 40778 40779 40782 40783
     40786 40792 40794 40794 40797 40803 40806 40810 40812 40819 40821
     40823 40826 40826 40829 40829 40845 40845 40847 40850 40852 40855
     40860 40862 40864 40867 40869 40869 65281 65281 65283 65286 65288
     65292 65294 65374 65377 65439 65507 65507 65509 65509 917504
     917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 2101 :NAME "Big5-HKSCS" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"See (http://www.iana.org/assignments/charset-reg/Big5-HKSCS)"
   :COMMENTS 'NIL :REFERENCES '("[Yick]") :RANGES
   #(0 128 162 163 165 165 167 168 176 177 183 183 192 193 200 202 210
     211 215 215 224 225 232 234 236 237 242 243 247 250 252 252 256 257
     274 275 282 283 299 299 331 333 339 339 363 363 461 462 464 466 468
     468 470 470 472 472 474 474 476 476 592 593 596 596 603 603 609 609
     618 618 629 629 643 643 650 650 710 711 713 715 729 729 913 929 931
     937 945 961 963 969 1025 1025 1040 1103 1105 1105 7870 7873 8211
     8212 8216 8217 8220 8221 8226 8226 8229 8230 8242 8242 8245 8245
     8251 8251 8254 8254 8451 8451 8453 8453 8457 8457 8470 8470 8481
     8481 8544 8553 8560 8569 8592 8595 8598 8601 8632 8633 8679 8679
     8730 8730 8734 8736 8739 8739 8741 8741 8745 8747 8750 8750 8756
     8757 8764 8764 8786 8786 8800 8801 8806 8807 8869 8869 8895 8895
     9312 9321 9332 9341 9472 9472 9474 9474 9484 9484 9488 9488 9492
     9492 9496 9496 9500 9500 9508 9508 9516 9516 9524 9524 9532 9532
     9552 9587 9601 9615 9620 9621 9632 9633 9650 9651 9660 9661 9670
     9671 9675 9675 9678 9679 9698 9701 9733 9734 9737 9737 9792 9794
     10045 10045 11904 11904 11908 11908 11910 11912 11914 11914 11916
     11917 11925 11925 11932 11933 11941 11941 11943 11943 11946 11946
     11948 11948 11950 11950 11958 11958 11964 11964 11966 11966 11974
     11974 11978 11978 11980 11981 11983 11983 11990 11991 11998 11998
     12003 12003 12083 12083 12288 12291 12293 12306 12308 12309 12317
     12318 12321 12329 12353 12435 12443 12446 12449 12534 12540 12542
     12549 12585 12849 12849 12963 12963 13198 13199 13212 13214 13217
     13217 13252 13252 13262 13262 13265 13266 13269 13269 13365 13365
     13376 13376 13388 13388 13412 13412 13427 13427 13434 13434 13437
     13438 13459 13459 13462 13462 13477 13477 13487 13487 13500 13500
     13505 13505 13512 13512 13535 13535 13540 13540 13563 13563 13574
     13574 13630 13630 13649 13649 13651 13651 13665 13665 13677 13677
     13680 13680 13682 13682 13687 13688 13700 13700 13719 13720 13729
     13729 13733 13733 13741 13741 13759 13759 13761 13761 13765 13765
     13767 13767 13770 13770 13774 13774 13778 13778 13782 13782 13787
     13787 13789 13789 13809 13811 13819 13819 13822 13822 13833 13833
     13848 13848 13850 13850 13859 13859 13869 13869 13877 13877 13881
     13881 13886 13886 13895 13897 13902 13902 13919 13919 13946 13946
     13953 13953 13978 13978 13989 13989 13994 13994 13996 13996 14000
     14001 14005 14005 14009 14009 14012 14012 14017 14017 14019 14021
     14023 14024 14035 14036 14038 14038 14045 14045 14049 14050 14053
     14054 14069 14069 14081 14081 14083 14083 14088 14088 14090 14090
     14093 14093 14108 14108 14114 14115 14117 14117 14124 14125 14128
     14128 14130 14131 14138 14138 14144 14144 14147 14147 14178 14178
     14191 14191 14231 14231 14240 14240 14265 14265 14270 14270 14322
     14322 14328 14328 14331 14331 14351 14351 14361 14361 14368 14368
     14381 14381 14390 14390 14392 14392 14435 14435 14496 14496 14531
     14531 14540 14540 14545 14545 14586 14586 14600 14600 14612 14612
     14631 14631 14642 14642 14655 14655 14669 14669 14691 14691 14720
     14720 14729 14730 14738 14738 14745 14745 14747 14747 14753 14753
     14756 14756 14776 14776 14812 14812 14818 14818 14821 14821 14828
     14828 14840 14840 14843 14843 14846 14846 14849 14849 14851 14851
     14854 14854 14871 14872 14889 14890 14900 14900 14923 14923 14930
     14930 14935 14935 14940 14940 14942 14942 14950 14951 14999 14999
     15019 15019 15037 15037 15070 15070 15072 15072 15088 15088 15090
     15090 15099 15099 15118 15118 15129 15129 15138 15138 15147 15147
     15161 15161 15170 15170 15192 15192 15200 15200 15217 15218 15227
     15228 15232 15232 15254 15254 15257 15257 15265 15265 15292 15292
     15294 15294 15298 15298 15300 15300 15319 15319 15325 15325 15340
     15340 15346 15348 15373 15373 15377 15377 15381 15381 15384 15384
     15444 15444 15563 15563 15565 15565 15569 15569 15574 15574 15580
     15580 15595 15595 15599 15599 15635 15635 15645 15645 15666 15666
     15675 15675 15686 15686 15692 15692 15694 15694 15697 15697 15711
     15711 15714 15714 15721 15722 15727 15727 15733 15733 15741 15741
     15749 15749 15754 15754 15759 15759 15761 15761 15781 15781 15789
     15789 15796 15796 15807 15807 15814 15815 15820 15821 15827 15827
     15835 15835 15847 15848 15851 15851 15859 15859 15863 15863 15868
     15869 15878 15878 15936 15936 15939 15939 15944 15944 15957 15957
     15988 15988 16040 16042 16045 16045 16049 16049 16056 16056 16063
     16063 16066 16066 16071 16071 16074 16074 16076 16076 16080 16081
     16086 16087 16090 16090 16094 16094 16097 16098 16103 16103 16105
     16105 16107 16107 16112 16112 16115 16116 16122 16122 16124 16124
     16127 16128 16132 16132 16134 16134 16142 16142 16211 16211 16216
     16217 16227 16227 16252 16252 16275 16275 16320 16320 16343 16343
     16348 16348 16357 16357 16365 16365 16377 16378 16388 16388 16413
     16413 16441 16441 16453 16453 16467 16467 16471 16471 16482 16482
     16485 16485 16490 16490 16495 16495 16552 16552 16571 16571 16575
     16575 16584 16584 16600 16600 16607 16607 16634 16634 16643 16644
     16649 16649 16654 16654 16690 16690 16743 16743 16748 16748 16750
     16750 16767 16767 16784 16784 16818 16818 16836 16836 16842 16842
     16847 16847 16859 16859 16879 16879 16889 16889 16913 16913 16960
     16960 16992 16992 17002 17002 17018 17018 17036 17036 17044 17044
     17077 17077 17081 17081 17084 17084 17140 17140 17147 17148 17195
     17195 17262 17262 17303 17303 17338 17338 17345 17345 17369 17369
     17375 17375 17389 17389 17394 17394 17409 17410 17427 17427 17445
     17445 17453 17453 17530 17530 17551 17551 17567 17568 17570 17570
     17584 17584 17591 17591 17600 17600 17605 17605 17614 17614 17629
     17629 17631 17631 17636 17636 17641 17644 17652 17652 17667 17668
     17673 17673 17675 17675 17686 17686 17693 17693 17703 17703 17710
     17710 17715 17715 17723 17723 17725 17725 17727 17727 17731 17731
     17745 17746 17749 17749 17756 17756 17762 17762 17770 17770 17783
     17783 17797 17797 17897 17897 17926 17926 17935 17935 17941 17941
     17943 17943 18011 18011 18042 18042 18048 18048 18127 18128 18165
     18165 18195 18195 18200 18200 18254 18254 18300 18300 18328 18328
     18342 18342 18358 18358 18389 18389 18413 18413 18420 18420 18432
     18432 18443 18443 18487 18487 18525 18525 18545 18545 18587 18587
     18605 18606 18640 18640 18653 18653 18669 18669 18675 18675 18682
     18682 18694 18694 18705 18705 18718 18718 18725 18725 18730 18730
     18733 18733 18741 18741 18748 18748 18750 18750 18757 18757 18769
     18769 18771 18771 18789 18789 18794 18794 18802 18802 18825 18825
     18849 18849 18855 18855 18911 18911 18917 18917 18919 18919 18959
     18959 18973 18973 18980 18980 18997 18997 19094 19094 19124 19124
     19128 19128 19153 19153 19172 19172 19199 19199 19225 19225 19244
     19244 19255 19255 19311 19312 19314 19314 19323 19323 19326 19326
     19342 19342 19344 19344 19347 19347 19350 19351 19357 19357 19389
     19390 19392 19392 19460 19460 19463 19463 19470 19470 19515 19515
     19518 19518 19547 19547 19565 19565 19581 19581 19620 19620 19630
     19630 19632 19632 19639 19639 19661 19661 19681 19681 19693 19693
     19721 19721 19728 19728 19764 19764 19857 19857 19868 19868 19968
     19969 19971 19972 19975 19985 19988 19990 19992 19994 19996 19996
     19998 19999 20001 20001 20004 20004 20006 20006 20008 20008 20010
     20014 20016 20019 20022 20025 20027 20029 20031 20031 20034 20035
     20037 20037 20039 20041 20043 20043 20045 20047 20050 20051 20054
     20054 20056 20063 20073 20074 20083 20083 20088 20088 20094 20105
     20107 20110 20113 20117 20120 20123 20126 20130 20132 20134 20136
     20136 20139 20142 20147 20147 20150 20151 20153 20156 20159 20164
     20166 20171 20173 20174 20180 20186 20188 20191 20193 20193 20195
     20197 20200 20203 20206 20206 20208 20216 20219 20219 20221 20221
     20223 20229 20232 20235 20237 20245 20248 20250 20253 20253 20258
     20258 20264 20265 20268 20269 20271 20272 20274 20276 20278 20287
     20289 20291 20293 20297 20299 20324 20327 20327 20329 20332 20334
     20336 20338 20363 20365 20365 20367 20370 20372 20376 20378 20382
     20386 20386 20392 20392 20395 20395 20398 20400 20402 20403 20405
     20407 20409 20411 20413 20413 20415 20421 20423 20433 20435 20436
     20438 20449 20452 20453 20460 20460 20462 20463 20465 20474 20477
     20478 20480 20480 20483 20483 20485 20489 20491 20495 20497 20508
     20510 20515 20517 20529 20531 20533 20535 20535 20540 20540 20544
     20545 20547 20547 20549 20559 20561 20561 20563 20563 20565 20568
     20570 20582 20584 20592 20594 20599 20602 20602 20605 20605 20608
     20611 20613 20613 20615 20616 20619 20622 20624 20626 20628 20630
     20632 20638 20642 20643 20646 20646 20652 20664 20666 20667 20669
     20671 20673 20674 20676 20683 20685 20689 20691 20695 20697 20699
     20701 20701 20703 20705 20707 20714 20716 20721 20723 20726 20728
     20729 20731 20750 20752 20757 20759 20760 20762 20762 20764 20764
     20767 20770 20772 20774 20777 20779 20781 20782 20784 20789 20791
     20797 20799 20801 20803 20809 20811 20813 20818 20818 20820 20823
     20825 20835 20837 20837 20839 20846 20849 20849 20852 20857 20860
     20860 20864 20864 20866 20866 20870 20874 20877 20877 20879 20879
     20881 20888 20890 20890 20892 20892 20894 20894 20896 20896 20898
     20898 20900 20901 20903 20904 20906 20908 20912 20921 20924 20926
     20931 20948 20951 20952 20955 20962 20964 20964 20973 20973 20976
     20977 20979 20982 20984 20986 20988 20990 20992 20995 20997 21004
     21006 21006 21008 21011 21014 21015 21020 21025 21028 21034 21038
     21038 21040 21048 21050 21052 21057 21057 21059 21060 21062 21063
     21065 21070 21074 21074 21076 21079 21081 21090 21096 21103 21106
     21109 21111 21117 21119 21124 21127 21133 21135 21137 21139 21140
     21142 21147 21151 21153 21155 21156 21158 21158 21160 21166 21173
     21173 21177 21177 21179 21180 21182 21182 21184 21187 21189 21189
     21191 21191 21193 21193 21196 21197 21200 21203 21205 21209 21211
     21211 21213 21220 21222 21222 21225 21225 21227 21227 21231 21233
     21235 21237 21239 21244 21246 21247 21249 21249 21253 21254 21256
     21259 21261 21266 21269 21271 21273 21274 21276 21277 21279 21283
     21287 21287 21290 21290 21292 21293 21295 21300 21303 21305 21307
     21317 21319 21322 21324 21326 21329 21332 21335 21335 21338 21338
     21340 21345 21347 21348 21350 21351 21353 21353 21356 21365 21367
     21369 21371 21375 21378 21378 21380 21380 21386 21386 21390 21391
     21394 21396 21398 21402 21404 21408 21410 21410 21412 21422 21424
     21424 21426 21426 21428 21428 21430 21430 21433 21433 21435 21435
     21441 21443 21445 21445 21448 21453 21456 21458 21460 21460 21462
     21467 21471 21478 21480 21491 21493 21496 21499 21500 21502 21502
     21505 21505 21507 21508 21510 21524 21526 21526 21528 21537 21539
     21555 21557 21561 21563 21566 21568 21571 21573 21576 21578 21579
     21581 21583 21588 21588 21600 21613 21615 21624 21626 21634 21636
     21640 21643 21656 21658 21658 21660 21660 21662 21662 21664 21684
     21686 21705 21707 21712 21718 21718 21722 21722 21726 21726 21728
     21739 21741 21743 21745 21747 21751 21752 21754 21757 21759 21759
     21761 21780 21783 21784 21786 21786 21790 21790 21795 21795 21797
     21800 21802 21817 21819 21820 21822 21825 21827 21835 21837 21843
     21845 21847 21852 21855 21857 21862 21865 21867 21873 21875 21877
     21879 21881 21881 21883 21892 21894 21909 21912 21914 21916 21917
     21919 21919 21921 21934 21936 21941 21945 21948 21951 21983 21985
     21994 21996 21996 21999 22002 22005 22007 22009 22018 22020 22022
     22024 22025 22028 22039 22043 22051 22053 22053 22055 22055 22057
     22058 22060 22064 22066 22075 22077 22083 22085 22086 22088 22090
     22092 22096 22098 22100 22103 22106 22109 22110 22112 22118 22120
     22132 22134 22140 22142 22151 22153 22160 22162 22163 22165 22165
     22167 22170 22172 22175 22177 22177 22180 22184 22186 22191 22193
     22199 22201 22201 22204 22211 22213 22214 22216 22221 22225 22225
     22227 22228 22230 22231 22234 22235 22237 22242 22244 22245 22247
     22247 22250 22251 22253 22257 22263 22263 22265 22266 22269 22269
     22271 22276 22279 22285 22290 22294 22296 22296 22298 22304 22306
     22307 22312 22314 22316 22320 22322 22324 22331 22331 22334 22339
     22341 22343 22345 22354 22356 22356 22359 22359 22363 22363 22367
     22367 22369 22370 22372 22372 22374 22379 22381 22381 22383 22391
     22394 22400 22402 22403 22408 22408 22410 22413 22415 22416 22419
     22421 22423 22437 22439 22439 22442 22442 22446 22446 22452 22454
     22456 22463 22465 22468 22470 22472 22475 22476 22478 22480 22482
     22482 22484 22485 22487 22487 22492 22503 22505 22505 22508 22526
     22528 22542 22544 22544 22546 22546 22548 22548 22553 22553 22555
     22558 22560 22570 22572 22587 22589 22589 22591 22592 22596 22596
     22599 22607 22609 22613 22615 22623 22626 22629 22632 22633 22635
     22637 22639 22639 22641 22646 22649 22659 22661 22667 22670 22673
     22675 22676 22680 22682 22684 22689 22691 22691 22693 22700 22702
     22705 22707 22707 22709 22710 22714 22719 22721 22722 22725 22729
     22731 22731 22734 22735 22737 22742 22744 22752 22754 22756 22759
     22761 22763 22764 22767 22768 22770 22772 22777 22783 22786 22791
     22794 22794 22796 22799 22801 22802 22804 22807 22809 22810 22812
     22812 22815 22816 22818 22818 22820 22821 22823 22823 22825 22831
     22833 22834 22836 22836 22839 22840 22844 22844 22846 22846 22848
     22848 22852 22853 22855 22858 22862 22865 22867 22869 22871 22872
     22874 22874 22876 22876 22880 22882 22885 22885 22887 22887 22889
     22891 22893 22894 22896 22905 22907 22917 22921 22922 22925 22928
     22930 22932 22934 22938 22941 22952 22956 22956 22958 22974 22976
     22977 22979 22996 22998 22998 23000 23006 23008 23009 23011 23014
     23016 23043 23049 23053 23055 23055 23057 23059 23061 23065 23067
     23068 23070 23073 23075 23077 23079 23079 23081 23086 23091 23091
     23093 23097 23100 23102 23104 23114 23116 23117 23120 23150 23152
     23153 23159 23167 23169 23172 23174 23174 23176 23176 23178 23180
     23182 23191 23193 23202 23205 23207 23209 23209 23211 23212 23214
     23236 23238 23247 23251 23251 23253 23270 23272 23278 23280 23280
     23282 23291 23293 23295 23297 23299 23301 23301 23303 23305 23307
     23309 23311 23313 23315 23316 23318 23319 23321 23323 23325 23329
     23331 23336 23338 23344 23346 23346 23348 23348 23352 23352 23356
     23361 23363 23368 23370 23377 23379 23384 23386 23389 23391 23391
     23394 23398 23400 23401 23403 23406 23408 23416 23418 23421 23423
     23429 23431 23433 23435 23436 23438 23440 23442 23443 23445 23445
     23447 23455 23458 23464 23466 23466 23468 23470 23472 23472 23475
     23478 23480 23481 23487 23495 23498 23502 23504 23513 23518 23532
     23534 23539 23541 23542 23544 23544 23546 23546 23551 23551 23553
     23553 23555 23557 23559 23574 23578 23578 23580 23580 23582 23584
     23586 23589 23592 23592 23594 23594 23596 23596 23600 23601 23603
     23603 23607 23612 23614 23617 23620 23633 23635 23638 23640 23641
     23644 23646 23648 23648 23650 23653 23655 23658 23660 23663 23665
     23665 23667 23668 23673 23676 23678 23678 23685 23686 23688 23693
     23695 23701 23705 23706 23708 23729 23731 23731 23733 23736 23738
     23738 23745 23746 23750 23756 23758 23764 23766 23771 23774 23775
     23781 23781 23784 23786 23788 23793 23796 23796 23798 23801 23803
     23803 23805 23805 23807 23809 23814 23815 23819 23826 23828 23835
     23837 23840 23842 23849 23852 23852 23854 23866 23868 23875 23877
     23884 23886 23886 23888 23890 23893 23895 23897 23897 23899 23899
     23902 23902 23906 23907 23909 23909 23911 23913 23915 23916 23919
     23922 23924 23924 23927 23927 23929 23930 23932 23938 23940 23947
     23949 23950 23954 23957 23959 23959 23961 23962 23964 23970 23972
     23972 23975 23986 23988 23994 23996 23997 24000 24003 24006 24007
     24009 24009 24011 24011 24013 24013 24015 24015 24017 24018 24020
     24024 24027 24027 24029 24034 24037 24040 24043 24043 24046 24046
     24048 24053 24055 24055 24057 24057 24061 24063 24066 24068 24070
     24070 24073 24076 24078 24078 24081 24082 24084 24091 24093 24093
     24095 24101 24104 24105 24107 24107 24109 24110 24115 24116 24118
     24120 24125 24126 24128 24133 24136 24136 24138 24143 24147 24149
     24151 24153 24155 24163 24166 24176 24178 24182 24184 24192 24194
     24196 24198 24205 24207 24207 24213 24215 24218 24220 24224 24224
     24226 24232 24234 24238 24240 24249 24253 24254 24257 24258 24260
     24270 24272 24291 24293 24297 24300 24300 24302 24303 24305 24308
     24310 24316 24318 24319 24321 24322 24324 24325 24327 24328 24330
     24335 24338 24341 24343 24344 24346 24347 24349 24349 24351 24351
     24354 24361 24365 24366 24368 24369 24371 24371 24373 24376 24378
     24378 24380 24380 24384 24384 24387 24388 24390 24390 24392 24401
     24404 24404 24406 24409 24412 24413 24417 24421 24423 24423 24425
     24429 24431 24436 24438 24441 24443 24451 24453 24460 24464 24466
     24470 24473 24475 24476 24478 24481 24484 24495 24497 24498 24501
     24503 24505 24513 24515 24517 24521 24521 24524 24525 24527 24530
     24532 24537 24539 24539 24541 24545 24547 24549 24552 24552 24554
     24555 24557 24559 24561 24561 24563 24565 24567 24568 24570 24571
     24573 24573 24575 24576 24585 24599 24601 24606 24608 24623 24625
     24629 24631 24631 24633 24633 24635 24635 24640 24647 24649 24650
     24652 24653 24656 24656 24658 24661 24664 24667 24669 24671 24674
     24688 24690 24690 24693 24693 24695 24695 24702 24705 24707 24714
     24716 24718 24720 24720 24722 24722 24724 24727 24730 24736 24738
     24740 24743 24744 24752 24769 24771 24783 24785 24785 24787 24789
     24791 24804 24806 24809 24816 24833 24835 24848 24850 24854 24856
     24861 24863 24863 24867 24867 24871 24873 24875 24876 24878 24880
     24882 24882 24884 24884 24886 24887 24891 24891 24893 24898 24900
     24912 24914 24918 24920 24936 24938 24940 24942 24951 24953 24954
     24956 24958 24960 24963 24967 24967 24969 24974 24976 24982 24984
     24989 24991 24991 24993 24994 24996 24996 24999 25018 25020 25020
     25022 25027 25029 25037 25039 25040 25043 25043 25046 25046 25048
     25048 25050 25050 25054 25056 25058 25067 25069 25070 25072 25074
     25077 25089 25091 25092 25095 25098 25100 25102 25104 25106 25108
     25110 25113 25115 25119 25125 25127 25127 25129 25134 25136 25136
     25138 25140 25142 25143 25145 25146 25149 25155 25158 25166 25168
     25172 25176 25180 25182 25182 25184 25190 25192 25192 25197 25204
     25206 25207 25209 25226 25228 25228 25230 25240 25245 25245 25252
     25252 25254 25254 25256 25265 25267 25270 25272 25273 25275 25279
     25282 25308 25311 25311 25317 25317 25323 25347 25351 25353 25355
     25361 25363 25366 25368 25368 25384 25391 25393 25406 25408 25425
     25428 25434 25444 25445 25447 25449 25451 25458 25461 25469 25471
     25477 25479 25490 25492 25492 25494 25497 25499 25509 25511 25521
     25529 25529 25533 25534 25536 25555 25557 25579 25581 25590 25592
     25593 25595 25596 25598 25598 25606 25607 25609 25616 25618 25624
     25626 25628 25630 25640 25642 25648 25650 25659 25661 25665 25667
     25667 25675 25675 25677 25678 25680 25684 25688 25697 25701 25705
     25707 25725 25727 25727 25730 25730 25733 25733 25735 25741 25743
     25747 25749 25754 25756 25760 25762 25766 25769 25769 25771 25780
     25782 25782 25787 25797 25799 25799 25801 25803 25805 25808 25810
     25812 25814 25819 25821 25821 25824 25837 25839 25844 25847 25848
     25850 25857 25859 25860 25862 25863 25865 25866 25868 25873 25875
     25881 25883 25894 25897 25904 25906 25913 25915 25915 25917 25919
     25921 25921 25923 25923 25925 25926 25928 25930 25933 25933 25935
     25935 25937 25937 25939 25945 25948 25951 25954 25960 25962 25965
     25967 25967 25970 25980 25983 25993 25996 25996 26000 26002 26004
     26007 26009 26009 26011 26018 26020 26021 26023 26024 26026 26028
     26030 26032 26034 26035 26037 26041 26043 26047 26049 26054 26059
     26068 26070 26071 26074 26075 26077 26083 26085 26086 26088 26089
     26092 26101 26106 26109 26111 26112 26114 26133 26136 26136 26140
     26152 26155 26155 26157 26166 26169 26170 26177 26181 26183 26188
     26191 26191 26193 26195 26199 26199 26201 26207 26209 26216 26218
     26220 26222 26228 26230 26238 26240 26240 26244 26254 26256 26258
     26260 26266 26269 26269 26271 26274 26276 26276 26280 26283 26285
     26299 26301 26304 26308 26308 26310 26319 26322 26322 26326 26326
     26328 26334 26336 26336 26339 26340 26342 26342 26344 26345 26347
     26350 26352 26356 26358 26361 26364 26364 26366 26373 26376 26384
     26386 26392 26395 26395 26397 26403 26405 26408 26410 26414 26417
     26417 26419 26422 26424 26431 26436 26441 26443 26449 26451 26451
     26453 26455 26457 26458 26460 26466 26471 26471 26474 26474 26476
     26477 26479 26495 26497 26497 26499 26503 26505 26505 26507 26517
     26519 26522 26524 26525 26527 26528 26532 26532 26540 26540 26542
     26555 26559 26566 26568 26580 26582 26591 26594 26599 26601 26618
     26620 26620 26622 26628 26637 26637 26640 26640 26642 26644 26646
     26648 26650 26658 26661 26662 26664 26667 26669 26671 26673 26686
     26688 26705 26707 26710 26717 26717 26725 26725 26731 26731 26733
     26735 26737 26738 26740 26745 26747 26764 26767 26772 26774 26776
     26779 26781 26783 26788 26790 26806 26809 26809 26813 26813 26819
     26830 26832 26840 26842 26842 26844 26849 26851 26852 26854 26860
     26862 26877 26880 26901 26903 26904 26906 26907 26917 26917 26922
     26922 26924 26924 26927 26928 26930 26937 26939 26950 26952 26956
     26958 26959 26961 27003 27008 27008 27010 27011 27013 27014 27018
     27018 27021 27022 27024 27025 27027 27036 27038 27063 27065 27065
     27067 27076 27078 27078 27081 27089 27091 27094 27097 27097 27105
     27106 27108 27113 27115 27118 27121 27124 27126 27149 27151 27151
     27153 27153 27155 27169 27171 27171 27173 27176 27179 27181 27186
     27189 27192 27201 27204 27209 27211 27227 27229 27234 27236 27243
     27245 27245 27247 27247 27249 27249 27252 27252 27254 27254 27258
     27258 27262 27269 27271 27271 27273 27274 27276 27287 27289 27304
     27307 27311 27313 27323 27325 27326 27330 27331 27333 27341 27343
     27345 27347 27348 27352 27361 27365 27365 27367 27368 27370 27372
     27374 27377 27379 27379 27382 27382 27384 27388 27392 27392 27394
     27397 27400 27403 27407 27411 27414 27418 27421 27422 27424 27425
     27427 27427 27429 27429 27432 27432 27436 27437 27439 27439 27441
     27455 27457 27459 27461 27470 27472 27474 27476 27479 27481 27481
     27483 27484 27486 27495 27498 27498 27501 27501 27503 27503 27506
     27506 27508 27508 27510 27515 27518 27524 27526 27526 27528 27530
     27532 27535 27537 27537 27540 27545 27547 27547 27550 27552 27554
     27559 27562 27563 27565 27568 27570 27571 27573 27575 27578 27578
     27580 27581 27583 27585 27587 27597 27599 27600 27602 27604 27606
     27608 27610 27612 27614 27614 27616 27620 27622 27624 27626 27628
     27631 27632 27634 27635 27639 27654 27656 27657 27659 27661 27663
     27665 27667 27670 27672 27677 27679 27681 27683 27688 27690 27692
     27694 27697 27699 27707 27709 27715 27718 27718 27721 27728 27730
     27730 27732 27733 27735 27745 27749 27755 27757 27766 27768 27771
     27773 27792 27794 27798 27800 27805 27807 27807 27810 27810 27818
     27828 27830 27847 27849 27863 27865 27875 27877 27877 27879 27891
     27893 27893 27897 27897 27904 27922 27926 27931 27933 27936 27938
     27938 27940 27970 27982 27982 27991 27996 27998 28010 28012 28018
     28020 28056 28068 28069 28074 28076 28078 28079 28081 28085 28087
     28096 28098 28098 28100 28109 28111 28134 28136 28151 28153 28157
     28160 28160 28162 28165 28170 28170 28175 28175 28181 28181 28184
     28189 28191 28214 28216 28225 28227 28231 28233 28235 28237 28246
     28248 28265 28267 28267 28270 28271 28273 28276 28278 28281 28284
     28284 28294 28294 28296 28297 28299 28299 28301 28304 28306 28308
     28310 28327 28330 28331 28334 28374 28376 28381 28386 28386 28392
     28393 28395 28399 28401 28402 28404 28431 28434 28442 28444 28444
     28446 28455 28457 28481 28483 28484 28494 28504 28506 28516 28518
     28519 28521 28528 28530 28532 28534 28546 28548 28558 28560 28560
     28562 28567 28573 28598 28600 28612 28614 28623 28627 28629 28632
     28644 28646 28649 28651 28658 28660 28660 28662 28664 28666 28668
     28670 28673 28675 28679 28681 28687 28689 28689 28692 28708 28710
     28716 28719 28725 28727 28732 28734 28742 28744 28748 28752 28754
     28756 28760 28762 28780 28783 28785 28788 28790 28792 28794 28796
     28799 28801 28806 28809 28811 28814 28815 28817 28822 28824 28826
     28831 28833 28835 28839 28841 28841 28843 28849 28851 28853 28855
     28862 28864 28864 28868 28872 28874 28890 28892 28898 28900 28900
     28911 28913 28915 28928 28930 28930 28932 28934 28937 28942 28944
     28944 28947 28947 28951 28951 28953 28963 28965 28966 28968 28969
     28971 28972 28974 28982 28986 28987 28990 28990 28992 28999 29001
     29012 29014 29014 29016 29018 29020 29036 29038 29038 29040 29043
     29045 29048 29050 29054 29056 29058 29060 29066 29068 29068 29070
     29072 29074 29074 29076 29076 29078 29093 29095 29098 29100 29101
     29103 29109 29111 29114 29116 29131 29134 29138 29140 29142 29144
     29149 29151 29154 29156 29160 29163 29166 29168 29170 29172 29174
     29176 29177 29179 29183 29185 29187 29189 29191 29193 29194 29196
     29200 29203 29207 29209 29211 29213 29215 29218 29220 29222 29230
     29232 29232 29237 29238 29240 29243 29245 29250 29252 29252 29254
     29260 29263 29264 29266 29267 29269 29283 29286 29287 29289 29290
     29292 29292 29294 29296 29298 29300 29302 29305 29307 29314 29316
     29321 29323 29336 29338 29339 29341 29343 29345 29354 29356 29360
     29362 29362 29364 29365 29370 29370 29373 29373 29375 29382 29385
     29390 29392 29394 29396 29396 29398 29402 29404 29404 29407 29412
     29414 29414 29416 29419 29427 29428 29430 29442 29444 29444 29447
     29448 29450 29452 29455 29459 29462 29465 29467 29470 29474 29475
     29477 29486 29488 29500 29502 29509 29512 29514 29516 29522 29527
     29531 29533 29538 29541 29548 29550 29560 29562 29579 29582 29583
     29586 29592 29596 29602 29604 29613 29618 29625 29627 29628 29630
     29632 29634 29635 29637 29648 29650 29662 29664 29675 29677 29679
     29683 29697 29699 29709 29713 29714 29716 29719 29721 29734 29736
     29754 29756 29756 29759 29771 29773 29783 29785 29792 29794 29797
     29799 29814 29817 29818 29820 29827 29829 29837 29840 29840 29842
     29842 29844 29845 29847 29852 29854 29857 29859 29867 29869 29869
     29871 29874 29876 29880 29882 29883 29885 29891 29893 29893 29896
     29896 29898 29900 29903 29904 29907 29929 29932 29932 29934 29934
     29936 29938 29940 29944 29947 29947 29949 29952 29954 29957 29959
     29960 29963 29978 29980 29983 29985 29986 29989 29990 29992 30005
     30007 30011 30013 30016 30018 30018 30022 30024 30026 30031 30033
     30033 30035 30037 30041 30045 30047 30048 30050 30055 30058 30064
     30066 30066 30070 30074 30077 30080 30083 30084 30086 30087 30090
     30098 30100 30101 30104 30106 30109 30110 30114 30117 30119 30119
     30122 30123 30128 30134 30136 30149 30151 30152 30154 30162 30164
     30165 30167 30180 30182 30183 30189 30189 30191 30211 30215 30221
     30223 30225 30227 30230 30233 30249 30252 30253 30255 30261 30264
     30264 30266 30269 30272 30272 30274 30275 30278 30281 30284 30292
     30294 30298 30300 30300 30303 30306 30308 30311 30313 30314 30316
     30326 30328 30335 30337 30338 30340 30340 30342 30347 30350 30352
     30354 30355 30357 30358 30361 30366 30369 30369 30372 30374 30378
     30379 30381 30384 30388 30389 30391 30392 30394 30395 30397 30399
     30402 30406 30408 30410 30412 30414 30418 30420 30422 30422 30426
     30431 30433 30433 30435 30439 30441 30442 30444 30453 30455 30460
     30462 30462 30465 30465 30467 30469 30471 30476 30478 30483 30485
     30485 30489 30491 30493 30496 30498 30505 30507 30507 30509 30509
     30511 30511 30513 30526 30528 30528 30531 30535 30538 30543 30546
     30546 30548 30550 30552 30556 30558 30563 30565 30575 30578 30578
     30583 30597 30599 30601 30603 30607 30609 30609 30611 30611 30613
     30613 30615 30627 30629 30629 30631 30632 30634 30637 30639 30647
     30649 30655 30658 30661 30663 30663 30665 30672 30675 30677 30679
     30684 30686 30686 30688 30688 30690 30691 30693 30697 30700 30708
     30711 30718 30722 30723 30725 30726 30728 30729 30732 30740 30744
     30744 30748 30755 30757 30773 30775 30777 30780 30781 30786 30789
     30791 30798 30800 30804 30812 30814 30816 30816 30818 30818 30820
     30822 30824 30833 30841 30844 30846 30849 30851 30857 30860 30863
     30865 30865 30867 30874 30878 30885 30887 30893 30895 30900 30902
     30902 30904 30908 30910 30910 30913 30913 30915 30917 30919 30933
     30935 30936 30938 30939 30941 30947 30949 30949 30951 30954 30956
     30965 30967 30967 30969 30975 30977 30978 30980 30982 30985 30985
     30988 30988 30990 30990 30992 30996 30999 30999 31001 31001 31003
     31006 31009 31009 31011 31023 31025 31030 31032 31042 31044 31052
     31055 31077 31079 31083 31085 31085 31088 31092 31097 31098 31100
     31107 31110 31112 31114 31115 31117 31133 31135 31138 31140 31150
     31152 31156 31158 31163 31165 31169 31172 31174 31176 31177 31179
     31183 31185 31186 31188 31190 31192 31192 31196 31200 31202 31204
     31206 31207 31209 31214 31217 31217 31220 31220 31222 31224 31226
     31227 31232 31232 31234 31238 31240 31240 31242 31245 31248 31253
     31255 31260 31262 31264 31266 31266 31270 31270 31272 31272 31275
     31275 31277 31281 31287 31287 31289 31296 31299 31310 31316 31316
     31318 31320 31322 31324 31327 31330 31333 31333 31335 31337 31339
     31342 31344 31345 31348 31350 31352 31355 31357 31361 31363 31372
     31375 31378 31380 31385 31390 31392 31394 31395 31400 31404 31406
     31416 31418 31420 31422 31429 31431 31435 31439 31439 31441 31441
     31443 31443 31448 31453 31455 31456 31458 31463 31465 31467 31469
     31471 31478 31479 31481 31483 31485 31489 31492 31494 31496 31500
     31502 31508 31512 31515 31517 31520 31522 31541 31544 31545 31547
     31547 31552 31552 31554 31570 31572 31574 31576 31576 31584 31591
     31593 31593 31596 31608 31611 31611 31618 31618 31620 31621 31623
     31624 31626 31634 31636 31641 31643 31645 31648 31652 31660 31661
     31663 31663 31665 31666 31668 31669 31671 31673 31678 31678 31680
     31681 31684 31684 31686 31687 31689 31692 31694 31696 31700 31701
     31704 31723 31728 31732 31735 31747 31749 31751 31753 31762 31765
     31765 31769 31769 31771 31779 31781 31789 31792 31792 31795 31795
     31797 31797 31799 31801 31803 31808 31810 31813 31815 31818 31820
     31821 31824 31825 31827 31828 31830 31831 31833 31837 31839 31840
     31843 31847 31849 31856 31858 31861 31864 31873 31875 31878 31880
     31882 31884 31886 31889 31890 31892 31896 31900 31900 31902 31903
     31905 31907 31909 31912 31916 31916 31918 31919 31921 31925 31928
     31935 31938 31938 31941 31941 31943 31950 31952 31959 31961 31962
     31964 31968 31970 31970 31974 31976 31978 31978 31980 31993 31995
     31998 32000 32034 32037 32037 32040 32041 32043 32044 32046 32051
     32053 32054 32056 32071 32074 32074 32077 32086 32088 32088 32090
     32095 32097 32099 32102 32107 32109 32115 32121 32125 32127 32129
     32131 32134 32136 32137 32140 32143 32145 32148 32150 32151 32156
     32164 32166 32181 32183 32194 32196 32199 32201 32208 32210 32212
     32215 32225 32227 32234 32236 32236 32238 32247 32249 32251 32253
     32254 32259 32259 32263 32279 32282 32293 32295 32295 32297 32299
     32301 32329 32332 32332 32336 32346 32348 32348 32350 32355 32357
     32357 32359 32363 32365 32368 32370 32386 32390 32392 32394 32399
     32401 32412 32415 32415 32420 32420 32428 32428 32442 32442 32455
     32455 32463 32463 32479 32479 32518 32518 32566 32570 32573 32577
     32579 32581 32583 32597 32600 32600 32603 32609 32611 32611 32613
     32622 32624 32627 32629 32639 32643 32643 32645 32655 32657 32663
     32666 32670 32672 32681 32684 32707 32709 32709 32711 32711 32713
     32722 32724 32725 32727 32727 32731 32739 32741 32757 32759 32776
     32779 32786 32788 32793 32795 32801 32804 32806 32808 32810 32812
     32812 32814 32817 32819 32823 32825 32825 32827 32831 32835 32835
     32838 32840 32842 32842 32847 32850 32852 32852 32854 32854 32856
     32856 32858 32862 32865 32868 32870 32871 32876 32876 32879 32883
     32885 32889 32893 32896 32898 32898 32900 32903 32905 32908 32911
     32912 32914 32915 32917 32918 32920 32925 32927 32927 32929 32931
     32933 32933 32935 32935 32937 32939 32941 32943 32945 32946 32948
     32952 32954 32954 32956 32957 32962 32970 32972 32977 32980 32990
     32992 32993 32995 32998 33001 33001 33004 33005 33007 33014 33016
     33022 33024 33027 33029 33034 33036 33036 33038 33038 33042 33042
     33044 33051 33053 33055 33057 33061 33063 33063 33065 33069 33071
     33072 33074 33074 33076 33076 33079 33079 33081 33082 33085 33086
     33090 33092 33094 33096 33098 33110 33113 33116 33118 33118 33120
     33122 33124 33127 33129 33129 33131 33131 33133 33140 33142 33146
     33148 33148 33151 33152 33154 33156 33158 33165 33167 33167 33171
     33171 33173 33173 33175 33184 33186 33187 33189 33196 33198 33198
     33200 33207 33209 33226 33228 33229 33231 33234 33237 33237 33239
     33243 33245 33258 33260 33263 33266 33268 33270 33276 33278 33282
     33284 33285 33287 33293 33296 33298 33300 33302 33304 33304 33306
     33314 33317 33318 33320 33325 33327 33327 33330 33338 33340 33344
     33346 33346 33348 33349 33351 33351 33353 33353 33355 33355 33358
     33372 33374 33375 33377 33382 33384 33385 33387 33391 33393 33394
     33396 33408 33411 33413 33415 33415 33418 33419 33421 33428 33432
     33435 33437 33457 33459 33470 33472 33472 33474 33476 33482 33482
     33487 33497 33499 33500 33502 33512 33514 33527 33529 33545 33547
     33549 33558 33566 33568 33568 33570 33570 33572 33581 33583 33583
     33585 33597 33599 33605 33607 33620 33622 33623 33634 33635 33638
     33638 33647 33647 33651 33656 33658 33663 33665 33665 33667 33667
     33669 33694 33696 33696 33698 33708 33710 33712 33721 33721 33725
     33743 33745 33745 33747 33753 33755 33765 33767 33782 33784 33791
     33793 33793 33795 33799 33801 33812 33814 33814 33816 33816 33819
     33820 33824 33825 33827 33828 33830 33830 33833 33833 33835 33856
     33858 33870 33872 33897 33899 33914 33917 33920 33922 33922 33926
     33926 33928 33928 33933 33940 33942 33956 33959 33970 33972 33972
     33974 33974 33976 33986 33988 33991 33993 34004 34006 34007 34010
     34011 34014 34014 34017 34018 34020 34021 34023 34028 34030 34036
     34038 34048 34050 34051 34053 34074 34076 34081 34083 34097 34099
     34100 34104 34104 34107 34107 34109 34110 34112 34126 34129 34139
     34141 34142 34144 34161 34163 34163 34165 34172 34174 34174 34176
     34193 34196 34198 34200 34212 34214 34218 34223 34225 34227 34234
     34237 34249 34251 34251 34253 34258 34261 34261 34263 34266 34268
     34278 34280 34290 34294 34306 34308 34311 34313 34317 34320 34321
     34324 34324 34326 34332 34334 34346 34348 34351 34353 34358 34360
     34364 34366 34368 34370 34371 34373 34376 34379 34382 34384 34384
     34386 34390 34393 34393 34395 34396 34398 34399 34401 34405 34407
     34420 34423 34423 34425 34428 34430 34430 34437 34439 34442 34446
     34448 34458 34460 34462 34464 34469 34471 34474 34477 34477 34479
     34505 34507 34508 34512 34513 34515 34516 34518 34527 34530 34532
     34534 34534 34536 34541 34543 34543 34549 34555 34558 34558 34560
     34574 34577 34579 34584 34588 34590 34590 34592 34602 34604 34606
     34608 34613 34615 34616 34618 34620 34622 34627 34630 34630 34636
     34673 34675 34683 34685 34685 34689 34697 34699 34701 34703 34708
     34710 34712 34714 34719 34722 34725 34729 34758 34760 34764 34766
     34766 34769 34772 34774 34792 34794 34799 34802 34807 34809 34812
     34814 34822 34824 34829 34831 34833 34835 34841 34843 34845 34847
     34867 34869 34873 34875 34886 34888 34888 34890 34895 34898 34899
     34901 34903 34905 34907 34909 34910 34912 34917 34919 34923 34925
     34930 34932 34935 34937 34937 34940 34948 34951 34953 34955 34958
     34961 34963 34965 34972 34974 34978 34980 34980 34983 34984 34986
     34988 34990 34990 34993 34994 34996 35002 35004 35010 35013 35013
     35015 35015 35017 35024 35026 35026 35028 35039 35041 35041 35046
     35048 35051 35052 35054 35074 35077 35079 35081 35084 35086 35086
     35088 35098 35102 35103 35105 35111 35113 35123 35125 35128 35131
     35134 35137 35140 35142 35143 35145 35145 35147 35149 35151 35156
     35158 35174 35177 35183 35185 35188 35190 35191 35193 35196 35198
     35203 35205 35211 35215 35215 35217 35217 35219 35224 35227 35231
     35233 35239 35241 35242 35244 35247 35250 35250 35254 35255 35257
     35258 35260 35265 35270 35270 35282 35286 35289 35293 35295 35305
     35307 35309 35312 35316 35318 35320 35322 35324 35326 35328 35330
     35332 35335 35336 35338 35338 35340 35340 35342 35347 35349 35352
     35355 35359 35362 35363 35365 35365 35367 35367 35369 35373 35376
     35377 35380 35380 35382 35382 35384 35393 35396 35398 35400 35402
     35404 35410 35412 35417 35419 35419 35422 35422 35424 35427 35430
     35433 35435 35438 35440 35447 35449 35452 35454 35455 35457 35463
     35467 35469 35471 35478 35480 35482 35484 35484 35486 35486 35488
     35489 35491 35499 35503 35504 35506 35506 35508 35508 35510 35510
     35512 35520 35522 35529 35531 35531 35533 35533 35535 35535 35537
     35554 35556 35556 35558 35560 35562 35563 35565 35576 35578 35580
     35582 35586 35588 35592 35594 35616 35618 35624 35626 35633 35635
     35635 35637 35639 35641 35651 35653 35674 35676 35680 35682 35683
     35685 35693 35695 35696 35700 35700 35703 35707 35709 35714 35716
     35718 35720 35720 35722 35724 35726 35726 35728 35728 35730 35734
     35736 35740 35742 35744 35774 35774 35810 35810 35895 35895 35897
     35897 35899 35903 35905 35907 35909 35921 35924 35927 35930 35930
     35932 35933 35935 35935 35937 35938 35940 35942 35944 35949 35951
     35955 35957 35963 35965 35965 35968 35970 35972 35974 35977 35978
     35980 35981 35983 35989 35991 36005 36007 36013 36015 36016 36018
     36037 36039 36040 36042 36042 36044 36045 36047 36047 36049 36055
     36057 36078 36080 36085 36087 36094 36096 36096 36098 36109 36111
     36121 36123 36125 36196 36196 36198 36201 36203 36208 36210 36212
     36214 36219 36221 36221 36224 36226 36228 36229 36233 36234 36236
     36246 36249 36249 36251 36252 36255 36257 36259 36259 36261 36271
     36274 36279 36281 36282 36284 36284 36286 36291 36293 36296 36299
     36305 36307 36317 36319 36324 36326 36332 36334 36340 36346 36346
     36348 36359 36361 36362 36365 36395 36397 36398 36400 36401 36403
     36406 36408 36410 36412 36418 36420 36432 36435 36439 36441 36458
     36460 36461 36463 36463 36465 36472 36474 36476 36478 36478 36480
     36482 36484 36494 36496 36504 36506 36506 36509 36513 36515 36525
     36528 36528 36530 36530 36534 36534 36538 36538 36540 36541 36544
     36544 36546 36547 36553 36559 36561 36564 36567 36568 36570 36578
     36580 36585 36587 36591 36593 36594 36596 36604 36606 36611 36613
     36619 36621 36622 36624 36640 36643 36646 36649 36650 36652 36656
     36658 36665 36667 36667 36670 36683 36685 36708 36710 36711 36718
     36718 36755 36755 36763 36764 36767 36768 36771 36771 36773 36774
     36776 36776 36781 36796 36798 36799 36801 36802 36804 36806 36809
     36814 36816 36823 36826 36826 36832 36838 36840 36840 36842 36843
     36845 36846 36848 36848 36852 36870 36872 36872 36875 36877 36879
     36882 36884 36887 36889 36900 36909 36911 36913 36920 36924 36927
     36929 36930 36932 36932 36935 36935 36937 36950 36952 36953 36955
     36958 36960 36964 36967 36969 36971 36976 36978 37000 37002 37003
     37005 37005 37007 37009 37012 37013 37015 37017 37019 37019 37022
     37027 37029 37032 37034 37034 37038 37046 37048 37048 37051 37051
     37053 37055 37057 37057 37059 37061 37063 37064 37066 37067 37070
     37070 37076 37085 37087 37093 37096 37101 37103 37109 37113 37129
     37131 37131 37133 37138 37140 37140 37142 37156 37158 37174 37176
     37179 37182 37185 37187 37203 37205 37210 37212 37212 37214 37221
     37223 37226 37228 37228 37230 37232 37234 37242 37244 37244 37248
     37255 37257 37267 37273 37283 37285 37285 37287 37303 37305 37310
     37312 37319 37321 37321 37323 37329 37331 37338 37340 37341 37343
     37343 37346 37358 37361 37361 37363 37365 37367 37370 37373 37386
     37388 37399 37401 37402 37404 37404 37406 37406 37411 37416 37418
     37419 37421 37422 37424 37434 37436 37441 37444 37446 37448 37464
     37466 37467 37469 37479 37483 37488 37490 37490 37494 37519 37521
     37521 37523 37533 37536 37548 37550 37550 37553 37559 37561 37564
     37566 37589 37591 37593 37595 37595 37597 37601 37603 37612 37614
     37620 37622 37636 37638 37641 37643 37648 37650 37654 37656 37659
     37661 37681 37683 37686 37688 37689 37692 37692 37696 37714 37716
     37724 37726 37742 37744 37745 37747 37758 37760 37770 37772 37778
     37780 37802 37804 37813 37815 37816 37819 37819 37821 37821 37823
     37824 37826 37828 37830 37832 37834 37860 37862 37864 37868 37868
     37870 37870 37872 37873 37875 37875 37877 37889 37891 37892 37894
     37913 37915 37915 37917 37917 37920 37920 37924 37934 37936 37939
     37941 37952 37954 37965 37967 37970 37972 37973 37975 37976 37979
     37979 37981 37982 37984 37984 37986 37989 37991 38009 38011 38019
     38021 38021 38047 38047 38050 38050 38081 38081 38083 38083 38108
     38108 38134 38134 38189 38189 38215 38215 38263 38264 38266 38269
     38271 38272 38274 38275 38277 38278 38280 38281 38283 38292 38294
     38297 38299 38300 38302 38318 38320 38321 38325 38327 38329 38336
     38339 38339 38341 38349 38352 38358 38362 38364 38366 38373 38376
     38376 38388 38388 38428 38430 38432 38436 38440 38440 38442 38442
     38444 38451 38456 38461 38463 38469 38474 38481 38483 38484 38486
     38486 38488 38488 38491 38495 38497 38500 38505 38509 38511 38520
     38523 38526 38528 38529 38531 38539 38541 38543 38545 38553 38555
     38556 38558 38558 38561 38565 38567 38570 38572 38572 38574 38574
     38576 38577 38579 38580 38582 38582 38584 38585 38587 38589 38591
     38606 38610 38623 38625 38627 38629 38629 38632 38634 38639 38642
     38644 38651 38653 38653 38655 38656 38658 38665 38667 38667 38669
     38675 38678 38678 38680 38681 38683 38700 38702 38706 38709 38710
     38712 38714 38717 38724 38726 38729 38731 38731 38737 38738 38742
     38744 38746 38754 38758 38758 38760 38762 38764 38766 38768 38772
     38774 38776 38778 38789 38791 38795 38797 38799 38804 38804 38807
     38822 38824 38824 38826 38830 38833 38836 38838 38843 38845 38857
     38859 38864 38866 38873 38876 38881 38883 38883 38885 38886 38893
     38894 38896 38899 38901 38902 38904 38907 38909 38920 38922 38922
     38924 38936 38939 38945 38947 38948 38950 38953 38955 38955 38957
     38960 38962 38965 38967 38969 38971 38971 38977 38977 38979 38995
     38999 39001 39003 39008 39010 39015 39017 39020 39023 39029 39080
     39081 39084 39087 39089 39092 39094 39108 39110 39116 39118 39118
     39131 39132 39134 39139 39141 39143 39145 39149 39151 39151 39153
     39154 39156 39158 39161 39162 39164 39166 39168 39168 39170 39171
     39173 39173 39175 39178 39180 39180 39182 39182 39184 39196 39198
     39199 39201 39201 39204 39205 39207 39219 39221 39221 39223 39235
     39237 39237 39239 39246 39248 39257 39259 39263 39265 39267 39318
     39321 39323 39326 39329 39329 39331 39336 39338 39349 39352 39357
     39361 39365 39367 39367 39369 39369 39371 39389 39391 39391 39393
     39399 39401 39402 39404 39406 39408 39409 39412 39423 39425 39431
     39433 39441 39444 39446 39449 39463 39465 39474 39476 39494 39496
     39498 39500 39504 39506 39516 39518 39520 39522 39532 39567 39567
     39592 39592 39595 39595 39597 39597 39599 39604 39606 39618 39622
     39623 39626 39626 39629 39629 39631 39640 39644 39644 39647 39651
     39654 39655 39659 39663 39665 39668 39670 39671 39673 39679 39681
     39686 39688 39698 39700 39706 39710 39712 39714 39717 39719 39723
     39725 39727 39729 39733 39735 39735 39737 39740 39742 39750 39752
     39752 39754 39762 39764 39766 39768 39771 39775 39777 39780 39780
     39782 39785 39788 39788 39791 39793 39796 39799 39802 39811 39813
     39816 39819 39819 39821 39827 39829 39829 39831 39831 39834 39835
     39837 39842 39844 39846 39848 39848 39850 39851 39853 39856 39861
     39862 39864 39865 39869 39869 39871 39873 39875 39876 39878 39882
     39887 39887 39891 39895 39897 39902 39904 39906 39908 39917 39920
     39921 39924 39924 39927 39928 39933 39933 39935 39935 39938 39938
     39941 39950 39952 39952 39954 39957 39959 39959 39963 39965 39967
     39969 39971 39974 39976 39977 39979 39981 39983 39983 39985 39991
     39993 40001 40004 40006 40008 40016 40018 40025 40029 40032 40034
     40035 40038 40040 40045 40046 40049 40053 40055 40060 40165 40167
     40169 40170 40173 40173 40177 40183 40185 40189 40191 40192 40194
     40201 40204 40204 40208 40208 40210 40210 40212 40217 40219 40219
     40221 40227 40229 40230 40232 40233 40237 40241 40243 40244 40246
     40249 40251 40251 40253 40261 40265 40268 40270 40276 40278 40289
     40295 40309 40311 40313 40315 40332 40336 40336 40338 40340 40342
     40365 40367 40367 40369 40389 40391 40415 40417 40422 40424 40425
     40427 40432 40434 40455 40457 40469 40471 40479 40565 40565 40569
     40573 40575 40590 40592 40610 40612 40625 40628 40631 40635 40644
     40646 40648 40652 40657 40659 40662 40664 40664 40666 40672 40674
     40674 40676 40680 40683 40683 40685 40706 40710 40714 40718 40720
     40722 40723 40725 40732 40734 40734 40736 40736 40738 40761 40763
     40763 40765 40766 40768 40784 40786 40807 40809 40812 40814 40818
     40820 40827 40830 40831 40845 40846 40848 40850 40852 40853 40856
     40857 40860 40860 40863 40864 40866 40866 40868 40869 57347 57347
     57368 57368 57371 57371 57389 57389 57400 57400 57420 57420 57434
     57434 57492 57492 57497 57498 57541 57541 57552 57552 57586 57586
     57593 57593 57597 57597 57606 57606 57609 57610 57648 57648 57680
     57680 57684 57684 57693 57693 57704 57704 57706 57706 57713 57713
     57717 57717 57743 57743 57787 57787 57804 57805 57815 57815 57839
     57839 57842 57842 57850 57850 57854 57854 57858 57858 57867 57867
     57881 57881 57884 57884 57895 57895 57925 57925 57930 57930 57934
     57934 57963 57963 57978 57978 57986 57986 58000 58000 58011 58011
     58017 58017 58034 58034 58039 58039 58049 58049 58057 58057 58068
     58068 58082 58082 58091 58092 58096 58097 58112 58112 58129 58129
     58149 58149 58160 58160 58181 58181 58210 58210 58241 58242 58246
     58246 58249 58249 58260 58260 58262 58262 58270 58270 58272 58272
     58292 58292 58299 58299 58317 58317 58319 58319 58325 58325 58337
     58337 58343 58343 58352 58352 58354 58354 58356 58356 58406 58406
     58422 58422 58440 58440 58445 58445 58448 58448 58471 58473 58484
     58484 58506 58506 58529 58529 58561 58561 58566 58566 58589 58589
     58654 58654 58660 58660 58664 58664 58670 58671 58733 58733 58736
     58736 58746 58746 58749 58749 58756 58756 58763 58763 58778 58778
     58782 58782 58784 58784 58790 58790 58794 58794 58818 58818 58824
     58824 58840 58840 58865 58865 58905 58905 58908 58908 58950 58950
     58964 58964 58976 58976 58986 58986 59005 59005 59008 59008 59022
     59022 59031 59031 59038 59038 59072 59072 59077 59077 59102 59103
     59137 59137 59155 59155 59173 59173 59197 59197 59201 59201 59218
     59218 59229 59229 59233 59233 59240 59240 59253 59253 59258 59258
     59261 59261 59280 59280 59287 59287 59302 59302 59316 59316 59354
     59354 59357 59357 59368 59369 59373 59373 59377 59377 59387 59388
     59392 59392 59404 59404 59411 59411 59425 59425 59432 59432 59437
     59437 59439 59439 59469 59469 59477 59478 59483 59483 59485 59485
     59493 59493 59527 59527 59535 59535 59537 59537 59553 59553 59556
     59556 59586 59587 59597 59597 59605 59606 59609 59610 59617 59617
     59621 59621 59646 59646 59671 59672 59679 59679 59685 59685 59691
     59691 59700 59700 59728 59728 59751 59751 59758 59759 59763 59763
     59767 59767 59770 59771 59773 59773 59778 59778 59793 59793 59851
     59851 60073 60073 60095 60097 60101 60101 60105 60108 60111 60113
     60115 60116 60118 60119 60121 60122 60124 60124 60126 60131 60135
     60135 60137 60138 60140 60140 60142 60142 60144 60145 60149 60150
     60152 60155 60159 60160 60162 60165 60168 60175 60178 60178 60181
     60181 60185 60189 60191 60192 60195 60195 60197 60197 60199 60199
     60202 60202 60204 60206 60208 60217 60221 60223 60227 60228 60234
     60234 60237 60238 60243 60243 60245 60246 60248 60248 60250 60251
     60258 60259 60261 60261 60263 60265 60271 60273 60275 60275 60278
     60279 60282 60282 60286 60288 60291 60291 60296 60296 60298 60298
     60300 60300 60304 60304 60307 60307 60309 60309 60313 60313 60315
     60315 60319 60319 60322 60322 60325 60326 60330 60331 60334 60334
     60338 60339 60349 60349 60351 60351 60357 60357 60362 60362 60364
     60364 60366 60366 60368 60368 60373 60373 60375 60376 60378 60379
     60381 60381 60383 60383 60385 60385 60388 60388 60392 60393 60395
     60396 60399 60399 60402 60402 60405 60405 60409 60410 60414 60414
     60424 60425 60431 60431 60435 60435 60441 60444 60452 60452 60454
     60454 60459 60459 60463 60463 60466 60466 60479 60479 60483 60489
     60491 60491 60495 60495 60498 60498 60500 60500 60502 60504 60506
     60506 60508 60509 60512 60512 60514 60515 60519 60519 60521 60525
     60527 60528 60530 60531 60533 60533 60537 60537 60539 60539 60542
     60542 60548 60548 60558 60558 60562 60562 60565 60565 60591 60591
     60598 60598 60623 60623 60636 60636 60640 60641 60656 60656 60658
     60658 60668 60668 60689 60689 60701 60701 60708 60708 60713 60713
     60716 60716 60731 60731 60737 60737 60751 60751 60754 60754 60767
     60767 60779 60779 60792 60792 60804 60805 60808 60808 60811 60811
     60814 60814 60879 60879 60890 60890 60909 60909 60936 60936 61021
     61021 61034 61034 61093 61093 61104 61104 61108 61108 61111 61111
     62211 62232 62245 62245 62247 62247 62276 62276 62278 62278 62282
     62283 62369 62369 62448 62448 62457 62457 62460 62460 62472 62472
     62475 62475 62525 62525 62529 62530 62537 62537 62541 62541 62544
     62544 62548 62548 62554 62557 62562 62562 62565 62566 62569 62569
     62573 62575 62583 62584 62588 62590 62592 62592 62597 62597 62600
     62600 62603 62603 62605 62605 62607 62609 62615 62616 62619 62619
     62628 62628 62631 62631 62634 62634 62637 62637 62639 62639 62643
     62644 62647 62647 62650 62653 62656 62657 62659 62659 62664 62664
     62670 62671 62674 62674 62690 62690 62693 62693 62695 62697 62707
     62707 62754 62755 62757 62758 62762 62762 62778 62779 62781 62781
     62798 62798 62803 62803 62806 62806 62809 62810 62814 62814 62821
     62821 62827 62827 62834 62834 63152 63152 63461 63462 63464 63464
     63466 63467 63469 63470 64012 64013 65072 65073 65075 65092 65097
     65104 65106 65106 65108 65111 65113 65126 65129 65131 65281 65341
     65343 65343 65345 65373 65380 65380 65506 65506 65508 65508 65517
     65517 131134 131134 131142 131142 131150 131150 131176 131176
     131310 131310 131352 131352 131499 131499 131521 131521 131540
     131540 131604 131604 131675 131675 131700 131701 131737 131737
     131742 131742 131744 131744 131775 131776 131813 131813 131850
     131850 131877 131877 131905 131905 131909 131911 131966 131968
     132007 132007 132021 132021 132041 132041 132043 132043 132085
     132085 132092 132092 132115 132116 132127 132127 132197 132197
     132231 132231 132241 132242 132259 132259 132348 132348 132350
     132350 132423 132423 132494 132494 132517 132517 132531 132531
     132560 132560 132575 132576 132587 132587 132625 132625 132633
     132634 132656 132656 132878 132878 132985 132985 133164 133164
     133235 133235 133398 133398 133460 133460 133497 133497 133649
     133649 133712 133712 133812 133812 134031 134031 134056 134056
     134086 134086 134114 134114 134143 134143 134176 134176 134209
     134211 134227 134227 134263 134264 134300 134300 134351 134351
     134355 134358 134470 134473 134476 134478 134526 134527 134567
     134567 134665 134666 134669 134673 134678 134678 134685 134685
     134765 134765 134773 134779 134826 134828 134838 134838 134904
     134907 134957 134958 134960 134961 134971 134971 135053 135053
     135085 135085 135092 135094 135146 135149 135197 135198 135279
     135279 135285 135288 135291 135291 135348 135348 135360 135361
     135367 135369 135412 135414 135485 135485 135496 135496 135552
     135552 135559 135559 135641 135641 135759 135759 135804 135804
     135849 135849 135856 135856 135907 135907 135934 135934 135938
     135941 135994 135994 136092 136092 136133 136134 136173 136173
     136190 136190 136274 136274 136276 136277 136343 136343 136374
     136374 136424 136424 136445 136445 136598 136598 136714 136714
     136723 136723 136766 136766 136801 136801 136850 136850 136896
     136898 136915 136915 136917 136917 136934 136936 136954 136956
     136958 136958 136976 136976 136998 136998 137018 137020 137047
     137047 137068 137073 137075 137076 137137 137141 137177 137180
     137205 137206 137208 137212 137256 137258 137274 137275 137285
     137285 137310 137310 137313 137316 137339 137339 137347 137349
     137374 137378 137406 137407 137430 137433 137466 137466 137475
     137477 137488 137490 137493 137493 137511 137511 137531 137531
     137540 137540 137596 137596 137608 137608 137622 137622 137691
     137691 137715 137715 137803 137803 138052 138052 138402 138402
     138405 138405 138566 138566 138682 138682 138698 138698 138705
     138705 138745 138745 138807 138807 138889 138889 138916 138916
     138920 138920 138965 138965 139114 139114 139169 139169 139337
     139337 139418 139418 139463 139463 139516 139516 139562 139562
     139635 139635 139642 139642 139681 139681 139715 139715 139900
     139900 140065 140065 140069 140069 140221 140221 140240 140240
     140389 140389 140401 140401 140427 140427 140563 140563 140571
     140571 140592 140592 140685 140685 140719 140719 140734 140734
     140827 140828 140843 140843 140904 140904 140950 140950 140952
     140952 141044 141046 141083 141083 141087 141087 141173 141173
     141185 141185 141236 141237 141315 141315 141407 141408 141485
     141485 141505 141505 141559 141559 141625 141625 141675 141675
     141696 141696 142031 142031 142037 142037 142114 142114 142186
     142186 142286 142286 142374 142375 142417 142417 142421 142421
     142491 142491 142497 142497 142534 142534 142599 142600 142695
     142695 142752 142752 142755 142756 142861 142861 142902 142902
     142968 142968 143331 143331 143435 143435 143485 143486 143502
     143502 143543 143543 143548 143548 143578 143578 143619 143619
     143677 143677 143746 143746 143780 143781 143816 143817 143863
     143865 143909 143909 143919 143919 143921 143924 143970 143970
     144009 144010 144043 144045 144082 144082 144096 144097 144128
     144128 144138 144138 144308 144308 144358 144358 144372 144373
     144382 144382 144384 144384 144464 144464 144498 144498 144613
     144613 144688 144688 144730 144730 144789 144789 144793 144793
     144796 144796 144845 144847 144896 144896 144956 144956 144960
     144960 145015 145015 145062 145062 145069 145069 145082 145082
     145119 145119 145134 145134 145184 145184 145199 145199 145215
     145215 145254 145254 145281 145281 145314 145314 145340 145340
     145346 145346 145365 145367 146139 146139 146158 146158 146266
     146266 146585 146587 146615 146615 146631 146633 146684 146687
     146779 146779 146870 146877 146936 146936 146988 146993 147080
     147083 147135 147135 147191 147196 147253 147253 147265 147265
     147327 147330 147383 147383 147392 147392 147435 147440 147473
     147473 147513 147517 147543 147543 147595 147597 147657 147657
     147716 147716 147775 147776 147780 147780 147797 147799 147804
     147804 147807 147807 147834 147834 147875 147877 147938 147938
     147964 147964 147995 147995 148043 148043 148054 148054 148057
     148057 148086 148088 148133 148133 148169 148170 148218 148218
     148250 148250 148296 148296 148322 148325 148364 148364 148374
     148374 148380 148380 148413 148413 148417 148417 148457 148457
     148466 148466 148472 148472 148533 148534 148570 148571 148615
     148616 148665 148665 148686 148686 148691 148691 148769 148769
     148856 148856 149034 149034 149093 149093 149143 149143 149204
     149204 149254 149254 149285 149285 149391 149391 149539 149539
     149747 149747 149759 149761 149772 149772 149782 149783 149785
     149785 149807 149807 149811 149811 149822 149826 149858 149859
     149876 149878 149887 149887 149896 149903 149927 149927 149931
     149933 149943 149947 149982 149983 149987 149987 150006 150009
     150049 150058 150078 150078 150094 150097 150117 150119 150135
     150138 150163 150166 150180 150183 150193 150195 150202 150204
     150215 150215 150225 150225 150239 150239 150242 150242 150382
     150382 150517 150517 150537 150537 150686 150687 150745 150745
     150790 150790 150968 150968 151018 151019 151120 151120 151310
     151310 151388 151388 151426 151426 151447 151447 151450 151450
     151465 151465 151490 151490 151709 151709 151880 151880 151933
     151934 152035 152035 152038 152039 152096 152097 152263 152263
     152280 152280 152337 152337 152339 152339 152613 152613 152623
     152624 152684 152684 152686 152686 152923 152925 152933 152934
     152961 152961 152964 152964 152975 152975 153017 153017 153045
     153045 153051 153051 153056 153056 153141 153141 153169 153169
     153219 153219 153334 153334 153350 153350 153373 153373 153381
     153381 153458 153458 153543 153543 153568 153569 153687 153687
     153714 153714 153825 153825 153942 153942 154028 154028 154268
     154268 154286 154286 154345 154345 154484 154484 154547 154548
     154566 154566 154625 154625 154630 154630 154698 154698 154725
     154725 154816 154817 154878 154878 154912 154912 154947 154947
     155150 155150 155265 155267 155302 155302 155324 155324 155351
     155352 155467 155467 155617 155618 155689 155689 155748 155748
     155812 155813 155937 155937 155993 155996 156077 156078 156125
     156125 156248 156248 156257 156257 156368 156368 156497 156497
     156606 156606 156688 156690 156809 156809 156824 156824 156946
     156946 157042 157042 157101 157101 157222 157222 157359 157359
     157361 157361 157416 157416 157505 157505 157619 157620 157644
     157644 157790 157790 157806 157806 157832 157832 157843 157843
     157966 157966 157969 157969 158120 158120 158133 158133 158194
     158194 158202 158202 158254 158254 158274 158274 158289 158290
     158474 158474 158504 158504 158544 158547 158614 158615 158643
     158643 158711 158711 158753 158753 158846 158850 158903 158904
     158909 158909 158912 158912 159010 159018 159136 159143 159196
     159196 159210 159211 159216 159216 159237 159237 159298 159301
     159342 159342 159346 159346 159351 159351 159364 159364 159440
     159447 159526 159526 159603 159604 159647 159647 159649 159649
     159710 159711 159758 159758 159826 159827 159917 159919 159949
     159949 159992 159992 160009 160009 160012 160012 160038 160039
     160100 160101 160117 160117 160283 160283 160486 160486 160666
     160666 160802 160802 160900 160900 160902 160902 161248 161248
     161252 161252 161277 161278 161292 161292 161330 161330 161337
     161337 161365 161367 161428 161428 161551 161551 161589 161590
     161601 161601 161630 161630 161668 161669 161904 161904 162084
     162084 162151 162151 162318 162318 162393 162393 162493 162494
     162548 162548 162584 162584 162616 162618 162804 162804 162834
     162834 163119 163119 163155 163156 163187 163187 163215 163215
     163224 163224 163261 163261 163292 163292 163405 163405 163407
     163407 163842 163842 163849 163849 164029 164030 164073 164073
     164084 164084 164142 164142 164207 164207 164359 164359 164438
     164438 164557 164557 164578 164578 164666 164666 164709 164709
     164733 164733 164882 164882 164994 164994 165180 165181 165228
     165228 165364 165364 165376 165376 165387 165387 165413 165413
     165435 165435 165546 165547 165592 165592 165606 165606 165892
     165892 165931 165931 166195 166195 166216 166217 166252 166252
     166270 166270 166281 166281 166312 166312 166314 166315 166332
     166332 166336 166336 166364 166364 166366 166366 166369 166369
     166372 166372 166393 166396 166415 166415 166422 166422 166437
     166437 166441 166441 166468 166475 166489 166490 166529 166531
     166554 166556 166598 166598 166603 166604 166622 166627 166629
     166629 166668 166668 166689 166690 166699 166701 166703 166703
     166732 166732 166734 166734 166736 166736 166755 166758 166764
     166764 166799 166799 166809 166809 166812 166813 166850 166850
     166853 166853 166871 166871 166873 166874 166887 166892 166911
     166911 166941 166941 166947 166947 166955 166955 166960 166960
     166969 166969 167117 167117 167220 167220 167478 167478 167525
     167526 167602 167603 167641 167641 168075 168075 168111 168113
     168164 168165 168205 168205 168208 168208 168348 168348 168360
     168360 168427 168427 168989 168989 169011 169011 169032 169032
     169177 169178 169374 169374 169449 169449 170071 170071 170148
     170148 170218 170218 170243 170243 170245 170245 170287 170287
     170333 170333 170346 170346 170397 170397 170757 170757 170766
     170766 170965 170965 171181 171181 171326 171326 171388 171388
     171565 171565 171624 171624 171692 171692 171715 171715 171811
     171811 171824 171824 171959 171959 172257 172257 172275 172275
     172280 172280 172286 172286 172323 172323 172368 172368 172434
     172435 172511 172511 172576 172576 172595 172595 172703 172703
     172722 172722 172767 172767 172969 172969 173510 173510 173737
     173737 194624 194624 194708 194708 194765 194765 194964 194964
     195028 195028 917504 917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 3 :NAME "ANSI_X3.4-1968" :ALIASES
   '("csASCII" "cp367" "IBM367" "us" #19="US-ASCII" "ISO646-US" "ASCII"
     "ISO_646.irv:1991" "ANSI_X3.4-1986" "iso-ir-6")
   :MIME-ENCODING '#19# :SOURCE '"ECMA registry" :COMMENTS
   '("Alias: US-ASCII (preferred MIME name)") :REFERENCES
   '("[RFC1345,KXS2]") :RANGES #(0 127))
  (MAKE-CHARACTER-SET :MIB-ENUM 2050 :NAME "IBM863" :ALIASES
   '("csIBM863" "863" "cp863") :MIME-ENCODING 'NIL :SOURCE
   '"IBM Keyboard layouts and code pages, PN 07G4586 June 1991"
   :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES
   #(0 127 160 160 162 164 166 168 171 172 175 180 182 184 187 190 192
     192 194 194 199 203 206 207 212 212 217 217 219 220 223 224 226 226
     231 235 238 239 243 244 247 247 249 252 402 402 915 915 920 920 931
     931 934 934 937 937 945 945 948 949 956 956 960 960 963 964 966 966
     8215 8215 8319 8319 8729 8730 8734 8734 8745 8745 8776 8776 8801
     8801 8804 8805 8976 8976 8992 8993 9472 9472 9474 9474 9484 9484
     9488 9488 9492 9492 9496 9496 9500 9500 9508 9508 9516 9516 9524
     9524 9532 9532 9552 9580 9600 9600 9604 9604 9608 9608 9612 9612
     9616 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 1015 :NAME "UTF-16" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE '"RFC 2781" :COMMENTS 'NIL :REFERENCES
   '("[RFC2781]") :RANGES #(0 55295 57344 1114111))
  (MAKE-CHARACTER-SET :MIB-ENUM 37 :NAME #20="ISO-2022-KR" :ALIASES
   '("csISO2022KR") :MIME-ENCODING '#20# :SOURCE
   '"RFC-1557 (see also KS_C_5601-1987)" :COMMENTS 'NIL :REFERENCES
   '("[RFC1557,Choi]") :RANGES
   #(0 127 161 161 164 164 167 168 170 170 173 174 176 180 182 186 188
     191 198 198 208 208 215 216 222 223 230 230 240 240 247 248 254 254
     273 273 294 295 305 307 312 312 319 322 329 331 338 339 358 359 711
     711 720 720 728 731 733 733 913 929 931 937 945 961 963 969 1025
     1025 1040 1103 1105 1105 8213 8213 8216 8217 8220 8221 8224 8225
     8229 8230 8240 8240 8242 8243 8251 8251 8308 8308 8319 8319 8321
     8324 8364 8364 8451 8451 8457 8457 8467 8467 8470 8470 8481 8482
     8486 8486 8491 8491 8531 8532 8539 8542 8544 8553 8560 8569 8592
     8601 8658 8658 8660 8660 8704 8704 8706 8707 8711 8712 8715 8715
     8719 8719 8721 8721 8730 8730 8733 8734 8736 8736 8741 8741 8743
     8748 8750 8750 8756 8757 8764 8765 8786 8786 8800 8801 8804 8805
     8810 8811 8834 8835 8838 8839 8857 8857 8869 8869 8978 8978 9312
     9326 9332 9346 9372 9397 9424 9449 9472 9475 9484 9547 9618 9618
     9632 9633 9635 9641 9650 9651 9654 9655 9660 9661 9664 9665 9670
     9672 9675 9675 9678 9681 9733 9734 9742 9743 9756 9756 9758 9758
     9792 9792 9794 9794 9824 9825 9827 9829 9831 9834 9836 9837 12288
     12291 12296 12305 12307 12309 12353 12435 12449 12534 12593 12686
     12800 12828 12896 12923 12927 12927 13184 13188 13192 13258 13263
     13264 13267 13267 13270 13270 13272 13272 13275 13277 19968 19969
     19971 19971 19975 19979 19981 19981 19985 19985 19988 19990 19992
     19993 19998 19998 20013 20013 20018 20018 20024 20025 20027 20027
     20034 20035 20037 20037 20043 20043 20045 20047 20054 20054 20056
     20057 20061 20063 20075 20075 20077 20077 20083 20083 20086 20087
     20094 20094 20098 20098 20102 20102 20104 20104 20107 20108 20110
     20110 20112 20114 20116 20117 20120 20120 20123 20123 20126 20126
     20129 20130 20132 20134 20136 20136 20139 20142 20150 20150 20154
     20154 20160 20161 20164 20164 20167 20167 20170 20171 20173 20173
     20180 20185 20189 20189 20191 20191 20195 20197 20208 20208 20210
     20210 20214 20215 20219 20219 20225 20225 20233 20235 20237 20241
     20271 20271 20276 20276 20278 20278 20280 20280 20282 20282 20284
     20285 20291 20291 20294 20296 20301 20305 20309 20309 20313 20316
     20329 20329 20335 20336 20339 20339 20342 20342 20346 20346 20350
     20351 20353 20353 20355 20356 20358 20358 20360 20360 20362 20363
     20365 20365 20367 20367 20369 20369 20374 20374 20376 20376 20379
     20379 20381 20381 20398 20399 20405 20406 20415 20415 20418 20420
     20425 20426 20430 20430 20433 20433 20435 20436 20439 20439 20442
     20442 20445 20445 20447 20449 20462 20463 20465 20465 20467 20467
     20469 20469 20472 20472 20474 20474 20482 20482 20486 20486 20489
     20489 20491 20491 20493 20493 20497 20498 20502 20502 20505 20506
     20508 20508 20510 20511 20513 20513 20515 20516 20518 20520 20522
     20525 20539 20539 20547 20547 20551 20553 20559 20559 20565 20565
     20570 20570 20572 20572 20581 20581 20596 20598 20600 20600 20608
     20608 20613 20613 20621 20621 20625 20625 20632 20633 20652 20653
     20658 20659 20661 20661 20663 20663 20670 20670 20677 20677 20681
     20682 20687 20687 20689 20689 20693 20694 20698 20698 20702 20702
     20709 20709 20711 20711 20717 20717 20729 20729 20731 20731 20735
     20737 20740 20740 20742 20742 20745 20745 20754 20754 20767 20767
     20769 20769 20778 20778 20786 20786 20791 20791 20794 20794 20796
     20796 20800 20801 20803 20809 20811 20814 20818 20818 20828 20828
     20834 20834 20837 20837 20839 20846 20849 20849 20853 20856 20860
     20860 20864 20864 20870 20870 20874 20874 20877 20877 20882 20882
     20885 20885 20887 20887 20896 20896 20901 20901 20906 20906 20908
     20908 20918 20919 20925 20925 20932 20932 20934 20934 20937 20937
     20939 20941 20956 20958 20961 20961 20976 20977 20982 20982 20984
     20986 20989 20989 20992 20992 20995 20995 20998 21000 21002 21002
     21006 21006 21009 21009 21015 21015 21021 21021 21028 21029 21033
     21034 21038 21038 21040 21040 21046 21051 21059 21059 21063 21063
     21066 21069 21076 21076 21078 21078 21083 21083 21085 21085 21089
     21089 21097 21098 21103 21103 21106 21106 21109 21109 21117 21117
     21119 21119 21123 21123 21127 21129 21133 21133 21137 21138 21147
     21147 21151 21152 21155 21156 21161 21163 21182 21182 21185 21185
     21187 21187 21189 21189 21191 21191 21193 21193 21197 21197 21202
     21202 21205 21206 21208 21209 21211 21211 21213 21215 21218 21220
     21235 21235 21237 21237 21240 21240 21242 21243 21246 21247 21253
     21253 21256 21256 21261 21261 21263 21264 21269 21271 21273 21273
     21280 21281 21283 21283 21290 21290 21295 21295 21305 21305 21311
     21313 21315 21316 21319 21322 21325 21325 21329 21332 21335 21335
     21338 21338 21340 21340 21342 21342 21344 21344 21350 21350 21352
     21352 21359 21361 21364 21365 21367 21367 21373 21373 21375 21375
     21380 21380 21395 21395 21400 21400 21402 21402 21407 21408 21413
     21414 21421 21421 21435 21435 21443 21443 21448 21451 21453 21453
     21460 21460 21462 21463 21467 21467 21473 21477 21481 21485 21487
     21491 21496 21496 21507 21508 21512 21514 21516 21521 21531 21531
     21533 21533 21535 21536 21542 21542 21545 21545 21547 21547 21555
     21555 21560 21561 21563 21564 21566 21566 21570 21570 21576 21576
     21578 21578 21585 21585 21608 21608 21610 21610 21617 21617 21619
     21619 21621 21621 21627 21629 21632 21632 21638 21638 21644 21644
     21646 21646 21648 21648 21668 21668 21672 21672 21675 21676 21683
     21683 21688 21688 21693 21693 21696 21697 21700 21700 21704 21705
     21729 21729 21733 21733 21736 21736 21741 21742 21746 21746 21754
     21754 21764 21764 21766 21767 21774 21774 21776 21776 21788 21788
     21807 21807 21809 21809 21813 21813 21822 21822 21828 21828 21830
     21830 21839 21839 21843 21843 21846 21846 21854 21854 21859 21859
     21884 21884 21888 21888 21892 21892 21894 21895 21897 21898 21912
     21914 21916 21917 21927 21927 21929 21932 21934 21934 21957 21957
     21959 21959 21972 21972 21978 21978 21980 21980 21983 21983 21987
     21988 22013 22014 22022 22022 22025 22025 22036 22036 22039 22039
     22063 22063 22066 22066 22068 22068 22070 22070 22099 22099 22120
     22120 22123 22123 22132 22132 22150 22150 22181 22181 22188 22188
     22190 22190 22196 22196 22204 22204 22218 22218 22221 22221 22225
     22225 22234 22235 22238 22238 22240 22240 22256 22256 22265 22266
     22275 22276 22280 22280 22283 22283 22285 22285 22290 22291 22294
     22294 22296 22296 22303 22303 22312 22312 22317 22317 22320 22320
     22331 22331 22336 22336 22338 22338 22343 22343 22346 22346 22349
     22350 22352 22353 22369 22369 22372 22372 22374 22374 22378 22378
     22382 22382 22384 22384 22389 22389 22396 22396 22402 22402 22408
     22408 22411 22411 22419 22419 22432 22432 22434 22435 22467 22467
     22471 22472 22475 22475 22478 22478 22495 22496 22512 22512 22516
     22516 22519 22519 22521 22522 22524 22524 22528 22528 22530 22530
     22533 22534 22536 22538 22558 22558 22561 22561 22564 22564 22567
     22567 22570 22570 22575 22577 22580 22581 22586 22586 22602 22603
     22607 22607 22609 22609 22612 22612 22615 22616 22618 22618 22622
     22622 22625 22626 22628 22628 22645 22645 22649 22649 22652 22652
     22654 22654 22659 22659 22661 22661 22665 22665 22675 22675 22684
     22684 22686 22687 22696 22697 22702 22702 22707 22707 22714 22715
     22718 22718 22721 22721 22725 22725 22727 22727 22734 22734 22737
     22737 22739 22739 22741 22741 22744 22745 22750 22751 22756 22756
     22763 22764 22767 22767 22777 22779 22781 22781 22799 22799 22804
     22806 22809 22810 22812 22812 22818 22818 22823 22823 22825 22827
     22829 22830 22833 22833 22839 22839 22846 22846 22852 22852 22855
     22857 22862 22865 22868 22869 22871 22871 22874 22874 22880 22880
     22882 22882 22887 22887 22890 22894 22899 22900 22904 22904 22909
     22909 22914 22916 22922 22922 22931 22931 22934 22935 22937 22937
     22949 22949 22952 22952 22956 22956 22969 22969 22971 22971 22974
     22974 22979 22979 22982 22982 22985 22985 22987 22987 22992 22993
     22995 22996 23001 23002 23004 23005 23014 23014 23016 23016 23018
     23018 23020 23020 23022 23022 23032 23032 23035 23035 23039 23039
     23041 23041 23043 23043 23057 23057 23064 23064 23067 23068 23071
     23072 23077 23077 23081 23081 23094 23094 23100 23100 23105 23105
     23110 23110 23113 23113 23130 23130 23138 23138 23142 23142 23186
     23186 23194 23195 23204 23204 23233 23234 23236 23236 23241 23241
     23244 23244 23265 23265 23270 23270 23273 23273 23301 23301 23305
     23305 23307 23308 23318 23318 23338 23338 23360 23360 23363 23363
     23376 23377 23380 23381 23383 23384 23386 23386 23388 23389 23391
     23391 23395 23396 23401 23401 23403 23403 23408 23409 23413 23413
     23416 23416 23418 23418 23420 23420 23429 23429 23431 23433 23435
     23436 23439 23439 23443 23443 23445 23452 23458 23462 23468 23468
     23470 23470 23472 23472 23475 23478 23480 23481 23487 23488 23490
     23495 23500 23500 23504 23504 23506 23508 23511 23511 23518 23519
     23521 23522 23524 23529 23531 23532 23534 23535 23541 23542 23544
     23544 23546 23546 23553 23553 23556 23556 23559 23563 23565 23567
     23569 23569 23574 23574 23577 23577 23588 23588 23592 23592 23601
     23601 23608 23612 23614 23616 23621 23622 23624 23624 23627 23627
     23629 23630 23633 23633 23637 23637 23643 23643 23648 23648 23650
     23650 23652 23653 23660 23660 23663 23663 23665 23665 23673 23673
     23696 23697 23713 23713 23721 23721 23723 23724 23729 23729 23731
     23731 23733 23733 23735 23736 23738 23738 23742 23742 23744 23744
     23769 23769 23776 23776 23784 23784 23791 23792 23796 23796 23798
     23798 23803 23803 23805 23805 23815 23815 23821 23822 23825 23825
     23828 23828 23830 23831 23833 23833 23847 23847 23849 23849 23883
     23884 23888 23888 23913 23913 23916 23916 23919 23919 23943 23943
     23947 23947 23965 23965 23968 23968 23970 23970 23978 23978 23992
     23992 23994 23994 23996 23997 24013 24013 24018 24018 24022 24022
     24029 24030 24033 24034 24037 24040 24043 24043 24046 24046 24049
     24052 24055 24055 24061 24062 24066 24067 24070 24070 24076 24076
     24081 24081 24086 24086 24089 24089 24091 24091 24093 24093 24101
     24101 24107 24107 24109 24109 24115 24115 24118 24118 24120 24120
     24125 24125 24127 24128 24132 24133 24135 24135 24140 24140 24149
     24149 24159 24159 24161 24163 24178 24180 24183 24185 24187 24190
     24196 24196 24199 24199 24202 24202 24207 24207 24213 24213 24215
     24215 24218 24218 24220 24220 24224 24224 24230 24231 24235 24235
     24237 24237 24245 24248 24254 24254 24258 24258 24264 24266 24272
     24272 24275 24275 24278 24278 24282 24283 24287 24288 24290 24291
     24300 24300 24307 24307 24310 24311 24314 24315 24321 24321 24324
     24324 24330 24330 24335 24335 24337 24337 24339 24341 24343 24344
     24347 24347 24351 24351 24358 24359 24361 24361 24369 24369 24373
     24373 24378 24378 24380 24380 24392 24392 24394 24394 24396 24396
     24398 24398 24406 24407 24409 24409 24411 24411 24418 24418 24422
     24423 24425 24429 24432 24433 24439 24439 24441 24441 24444 24444
     24447 24449 24453 24453 24455 24455 24458 24460 24464 24466 24471
     24473 24478 24478 24480 24481 24488 24490 24494 24494 24501 24501
     24503 24503 24505 24505 24509 24509 24515 24515 24517 24517 24524
     24525 24534 24537 24544 24544 24555 24555 24565 24565 24573 24573
     24575 24575 24591 24591 24594 24594 24598 24598 24604 24605 24608
     24609 24613 24613 24615 24616 24618 24618 24623 24623 24641 24643
     24653 24653 24656 24656 24658 24658 24661 24661 24665 24665 24669
     24669 24674 24677 24680 24682 24684 24685 24687 24688 24709 24709
     24713 24713 24716 24717 24724 24724 24726 24726 24730 24731 24735
     24736 24739 24740 24743 24743 24752 24752 24754 24756 24758 24758
     24760 24760 24764 24765 24773 24773 24775 24775 24785 24785 24794
     24794 24796 24796 24799 24801 24816 24817 24819 24819 24822 24822
     24825 24827 24833 24833 24838 24838 24840 24841 24845 24847 24853
     24853 24858 24859 24863 24863 24871 24871 24880 24880 24884 24884
     24887 24887 24892 24892 24894 24895 24898 24898 24900 24900 24903
     24904 24906 24908 24915 24915 24917 24917 24920 24921 24925 24925
     24927 24927 24930 24932 24935 24936 24939 24939 24942 24942 24944
     24944 24950 24951 24957 24958 24961 24962 24970 24970 24974 24974
     24976 24977 24980 24980 24984 24986 24996 24996 24999 24999 25001
     25001 25003 25004 25006 25006 25010 25010 25014 25014 25018 25018
     25022 25022 25027 25027 25031 25035 25062 25062 25074 25074 25078
     25080 25082 25082 25084 25084 25087 25088 25095 25096 25098 25098
     25100 25102 25104 25106 25110 25110 25114 25114 25119 25119 25121
     25121 25130 25130 25134 25134 25136 25137 25140 25140 25142 25142
     25150 25153 25159 25161 25163 25163 25165 25165 25171 25171 25176
     25176 25198 25198 25201 25201 25206 25206 25209 25209 25212 25212
     25215 25216 25220 25220 25225 25226 25233 25234 25237 25237 25239
     25240 25243 25243 25259 25259 25265 25265 25269 25269 25273 25273
     25276 25277 25282 25282 25287 25289 25292 25293 25295 25296 25298
     25300 25302 25305 25307 25308 25324 25327 25329 25329 25331 25331
     25335 25335 25342 25343 25345 25345 25351 25351 25353 25353 25361
     25361 25387 25387 25391 25391 25402 25403 25405 25406 25417 25417
     25420 25420 25423 25424 25429 25429 25447 25448 25454 25454 25458
     25458 25463 25463 25466 25467 25471 25471 25475 25475 25480 25481
     25484 25484 25490 25490 25494 25494 25496 25496 25499 25499 25504
     25506 25509 25509 25511 25514 25536 25536 25540 25540 25542 25542
     25551 25552 25558 25558 25562 25563 25569 25569 25581 25582 25588
     25588 25590 25591 25613 25613 25615 25615 25620 25620 25622 25623
     25628 25628 25634 25634 25644 25645 25658 25658 25662 25662 25688
     25688 25696 25696 25705 25705 25711 25711 25720 25722 25736 25736
     25745 25747 25754 25754 25758 25758 25764 25765 25771 25771 25773
     25774 25776 25776 25778 25778 25787 25787 25793 25793 25796 25797
     25799 25799 25802 25802 25805 25806 25810 25810 25812 25812 25816
     25816 25818 25818 25825 25826 25829 25831 25836 25836 25842 25842
     25844 25844 25850 25850 25854 25854 25856 25856 25860 25860 25880
     25880 25885 25885 25891 25891 25898 25900 25903 25903 25910 25913
     25915 25915 25918 25919 25925 25925 25928 25928 25933 25935 25937
     25937 25942 25943 25950 25950 25954 25955 25958 25958 25964 25965
     25970 25970 25972 25973 25975 25976 25982 25982 25986 25987 25989
     25989 25991 25991 25996 25996 26000 26001 26007 26007 26009 26009
     26011 26012 26015 26015 26017 26017 26020 26021 26023 26023 26027
     26028 26031 26032 26039 26039 26041 26041 26044 26045 26049 26049
     26053 26053 26059 26060 26063 26063 26066 26066 26071 26071 26080
     26080 26083 26083 26085 26086 26088 26089 26092 26093 26097 26097
     26100 26100 26106 26109 26111 26111 26118 26119 26121 26122 26124
     26124 26126 26129 26131 26133 26142 26144 26149 26149 26151 26152
     26157 26157 26159 26161 26164 26164 26166 26166 26170 26171 26177
     26180 26185 26185 26187 26187 26191 26191 26201 26201 26203 26203
     26205 26207 26212 26217 26219 26219 26222 26223 26227 26228 26230
     26232 26234 26234 26244 26244 26247 26249 26254 26254 26256 26257
     26262 26264 26269 26269 26272 26272 26274 26274 26283 26283 26286
     26286 26290 26292 26297 26297 26299 26299 26302 26302 26308 26308
     26310 26311 26313 26313 26326 26326 26329 26329 26332 26333 26336
     26336 26342 26342 26352 26352 26354 26356 26359 26362 26364 26364
     26366 26368 26371 26371 26376 26377 26379 26379 26381 26381 26388
     26389 26391 26391 26395 26395 26397 26399 26406 26408 26410 26414
     26417 26417 26420 26420 26422 26422 26426 26426 26429 26429 26438
     26438 26441 26441 26446 26449 26451 26451 26454 26454 26460 26460
     26462 26463 26477 26477 26479 26481 26483 26483 26485 26485 26487
     26487 26491 26491 26494 26495 26503 26503 26505 26505 26507 26507
     26511 26512 26515 26515 26517 26517 26519 26519 26522 26522 26524
     26525 26543 26544 26547 26547 26550 26552 26558 26558 26564 26564
     26575 26580 26586 26586 26589 26589 26601 26601 26604 26604 26607
     26609 26611 26614 26619 26619 26622 26622 26642 26643 26646 26647
     26657 26658 26666 26666 26671 26671 26680 26681 26684 26685 26688
     26691 26696 26696 26702 26702 26704 26705 26707 26708 26733 26733
     26742 26742 26751 26751 26753 26753 26757 26757 26767 26767 26771
     26772 26775 26775 26781 26781 26783 26783 26785 26786 26791 26792
     26797 26797 26799 26801 26803 26803 26805 26806 26820 26821 26825
     26825 26827 26827 26829 26829 26834 26834 26837 26837 26839 26840
     26842 26842 26847 26848 26855 26856 26862 26862 26866 26866 26873
     26874 26880 26880 26885 26885 26893 26894 26898 26898 26919 26919
     26928 26928 26941 26941 26943 26943 26954 26954 26963 26965 26967
     26967 26969 26970 26974 26974 26976 26979 26984 26984 26987 26987
     26989 26991 26997 26997 26999 27001 27029 27029 27035 27036 27045
     27045 27047 27047 27054 27054 27060 27060 27067 27067 27073 27073
     27075 27075 27083 27085 27088 27088 27112 27112 27114 27114 27131
     27131 27133 27133 27135 27135 27138 27138 27146 27146 27153 27153
     27155 27155 27159 27159 27161 27161 27166 27167 27169 27169 27171
     27171 27189 27189 27192 27194 27197 27197 27204 27204 27208 27208
     27211 27211 27218 27219 27224 27225 27231 27231 27233 27233 27243
     27243 27264 27264 27268 27268 27273 27273 27277 27278 27287 27287
     27292 27292 27298 27299 27315 27315 27323 27323 27330 27331 27347
     27347 27354 27355 27382 27382 27387 27387 27396 27396 27402 27402
     27404 27404 27410 27410 27414 27414 27424 27425 27427 27427 27442
     27442 27450 27450 27453 27454 27462 27463 27468 27468 27470 27470
     27472 27472 27487 27487 27489 27494 27498 27498 27506 27506 27511
     27512 27515 27515 27519 27519 27523 27524 27526 27526 27529 27530
     27542 27542 27544 27544 27550 27550 27566 27567 27570 27570 27573
     27573 27575 27575 27578 27578 27580 27580 27583 27583 27585 27585
     27589 27590 27595 27595 27597 27597 27599 27599 27602 27604 27606
     27608 27611 27611 27627 27628 27656 27656 27663 27663 27665 27665
     27667 27667 27683 27683 27700 27700 27703 27704 27710 27710 27712
     27714 27726 27726 27728 27728 27733 27733 27735 27735 27738 27738
     27741 27744 27752 27752 27754 27754 27757 27757 27760 27760 27762
     27762 27766 27766 27770 27770 27773 27774 27777 27779 27781 27784
     27788 27788 27792 27792 27794 27798 27801 27803 27819 27819 27822
     27822 27827 27827 27832 27833 27835 27839 27841 27842 27844 27844
     27849 27850 27852 27852 27859 27859 27861 27861 27863 27863 27867
     27867 27873 27875 27877 27877 27880 27880 27883 27883 27886 27888
     27891 27891 27915 27916 27921 27921 27927 27927 27929 27929 27931
     27931 27934 27934 27941 27941 27943 27943 27945 27946 27954 27954
     27957 27958 27960 27961 27963 27963 27965 27966 27969 27969 27993
     27994 27996 27996 28003 28003 28006 28006 28009 28010 28012 28012
     28014 28014 28020 28020 28023 28025 28031 28031 28037 28037 28039
     28041 28044 28046 28049 28049 28051 28051 28053 28053 28079 28079
     28082 28082 28085 28085 28096 28096 28099 28103 28107 28107 28111
     28111 28113 28113 28120 28122 28126 28126 28129 28129 28136 28136
     28138 28139 28142 28142 28145 28145 28147 28147 28149 28149 28151
     28155 28183 28183 28185 28187 28191 28193 28195 28198 28203 28205
     28207 28207 28210 28210 28212 28212 28214 28214 28216 28216 28218
     28218 28220 28222 28227 28228 28234 28234 28237 28237 28246 28246
     28248 28248 28251 28252 28254 28255 28263 28263 28267 28267 28270
     28271 28274 28275 28282 28282 28304 28304 28310 28310 28316 28317
     28319 28319 28322 28322 28325 28325 28330 28331 28335 28335 28337
     28337 28342 28342 28346 28346 28354 28354 28356 28357 28361 28361
     28363 28364 28366 28366 28369 28369 28371 28372 28399 28399 28404
     28404 28408 28408 28414 28415 28417 28418 28422 28422 28431 28431
     28433 28433 28436 28437 28448 28448 28450 28451 28459 28460 28465
     28466 28472 28472 28479 28479 28481 28481 28497 28497 28500 28500
     28503 28504 28506 28507 28510 28511 28514 28514 28516 28516 28525
     28526 28528 28528 28538 28538 28540 28542 28545 28545 28548 28548
     28552 28552 28557 28558 28560 28560 28564 28564 28567 28567 28579
     28580 28583 28583 28590 28591 28593 28593 28595 28595 28601 28601
     28606 28606 28608 28611 28618 28618 28629 28629 28634 28634 28639
     28641 28644 28644 28649 28649 28651 28652 28655 28655 28657 28657
     28670 28670 28673 28673 28677 28678 28681 28681 28683 28683 28687
     28687 28689 28689 28693 28693 28696 28696 28698 28703 28707 28707
     28711 28712 28719 28719 28727 28727 28734 28734 28748 28748 28752
     28753 28760 28760 28765 28765 28771 28771 28779 28779 28784 28784
     28792 28792 28796 28797 28805 28805 28810 28810 28814 28814 28818
     28818 28824 28826 28833 28833 28836 28836 28843 28845 28847 28847
     28851 28851 28855 28857 28872 28872 28875 28875 28879 28879 28888
     28889 28893 28893 28895 28895 28913 28913 28921 28921 28925 28925
     28932 28932 28937 28937 28940 28940 28953 28954 28958 28958 28961
     28961 28966 28966 28976 28976 28982 28982 28999 28999 29001 29002
     29004 29004 29006 29006 29008 29008 29014 29014 29017 29017 29020
     29020 29022 29022 29028 29031 29033 29033 29036 29036 29038 29038
     29053 29053 29060 29060 29065 29066 29071 29071 29074 29074 29076
     29076 29081 29081 29087 29087 29090 29090 29100 29100 29105 29105
     29113 29114 29118 29118 29121 29121 29123 29123 29128 29129 29134
     29134 29136 29136 29138 29138 29140 29141 29151 29151 29157 29159
     29165 29166 29179 29180 29182 29184 29190 29190 29200 29200 29211
     29211 29226 29226 29228 29229 29232 29232 29234 29234 29237 29238
     29242 29243 29245 29246 29248 29248 29254 29256 29260 29260 29266
     29266 29272 29273 29275 29275 29277 29277 29279 29279 29281 29282
     29287 29287 29289 29289 29298 29298 29305 29305 29309 29309 29312
     29313 29346 29346 29351 29351 29356 29356 29359 29359 29376 29376
     29378 29378 29380 29380 29390 29390 29392 29392 29399 29399 29401
     29401 29409 29409 29417 29417 29432 29433 29436 29437 29450 29450
     29462 29462 29467 29469 29477 29477 29481 29483 29494 29495 29502
     29503 29508 29509 29520 29520 29522 29522 29527 29527 29544 29544
     29546 29546 29552 29552 29554 29554 29557 29557 29560 29560 29562
     29563 29572 29572 29574 29575 29577 29577 29579 29579 29582 29582
     29588 29588 29590 29592 29599 29599 29607 29607 29609 29609 29613
     29613 29618 29619 29625 29625 29632 29632 29634 29634 29641 29642
     29644 29645 29647 29647 29654 29654 29657 29657 29661 29662 29664
     29664 29667 29670 29673 29674 29677 29677 29687 29687 29689 29689
     29693 29694 29697 29697 29699 29699 29701 29703 29705 29705 29715
     29715 29723 29723 29728 29730 29733 29734 29736 29736 29738 29740
     29742 29744 29747 29750 29752 29752 29754 29754 29759 29761 29763
     29764 29771 29771 29781 29781 29783 29783 29785 29788 29790 29792
     29794 29794 29796 29797 29800 29802 29807 29807 29822 29822 29826
     29827 29831 29831 29833 29833 29835 29835 29848 29848 29852 29852
     29854 29855 29857 29857 29859 29859 29861 29861 29863 29864 29866
     29866 29872 29872 29874 29874 29877 29877 29881 29881 29885 29885
     29887 29887 29894 29894 29898 29898 29903 29903 29908 29908 29912
     29912 29914 29914 29916 29916 29920 29920 29922 29923 29926 29926
     29934 29934 29943 29943 29953 29953 29956 29956 29969 29969 29973
     29973 29976 29976 29978 29979 29983 29983 29987 29987 29989 29990
     29992 29992 29995 29996 30000 30003 30007 30008 30010 30010 30023
     30023 30028 30028 30031 30031 30033 30033 30035 30036 30041 30041
     30043 30045 30050 30050 30053 30054 30058 30058 30063 30064 30069
     30070 30072 30072 30074 30074 30079 30079 30086 30087 30090 30091
     30094 30095 30097 30097 30109 30109 30117 30117 30123 30123 30129
     30131 30133 30133 30136 30137 30140 30142 30146 30146 30149 30149
     30151 30151 30157 30157 30162 30162 30164 30165 30168 30169 30171
     30171 30178 30178 30192 30192 30194 30194 30196 30196 30202 30202
     30204 30204 30208 30208 30221 30221 30233 30233 30239 30242 30244
     30244 30246 30246 30267 30267 30274 30274 30284 30284 30286 30286
     30290 30290 30294 30294 30305 30305 30308 30308 30313 30313 30316
     30316 30320 30320 30322 30322 30328 30328 30331 30334 30340 30340
     30342 30343 30350 30350 30352 30352 30355 30355 30382 30382 30394
     30394 30399 30399 30402 30403 30406 30406 30408 30408 30410 30410
     30418 30418 30422 30422 30427 30428 30430 30431 30433 30433 30435
     30436 30439 30439 30446 30446 30450 30450 30452 30452 30456 30456
     30460 30460 30462 30462 30465 30465 30468 30468 30472 30473 30475
     30475 30494 30494 30496 30496 30505 30505 30519 30520 30522 30522
     30524 30524 30528 30528 30541 30541 30555 30555 30561 30561 30563
     30563 30566 30566 30571 30571 30585 30585 30590 30591 30603 30603
     30609 30609 30622 30622 30629 30629 30636 30637 30640 30640 30643
     30643 30651 30652 30655 30655 30679 30679 30683 30684 30690 30691
     30693 30693 30697 30697 30701 30703 30707 30707 30722 30722 30738
     30738 30757 30759 30764 30764 30770 30770 30772 30772 30789 30789
     30799 30799 30813 30813 30827 30828 30831 30831 30844 30844 30849
     30849 30855 30855 30860 30862 30865 30865 30871 30871 30883 30883
     30887 30887 30889 30889 30906 30908 30913 30913 30917 30917 30922
     30923 30926 30926 30928 30928 30952 30952 30956 30956 30959 30959
     30965 30965 30971 30971 30977 30977 30990 30990 30998 30998 31018
     31020 31034 31034 31038 31038 31040 31041 31047 31049 31056 31056
     31062 31063 31066 31070 31072 31072 31077 31077 31080 31080 31085
     31085 31098 31098 31103 31103 31105 31105 31117 31119 31121 31121
     31142 31143 31146 31146 31150 31150 31153 31153 31155 31155 31161
     31161 31165 31169 31177 31179 31185 31186 31189 31189 31192 31192
     31199 31199 31204 31204 31206 31207 31209 31209 31227 31227 31232
     31232 31237 31237 31240 31240 31243 31243 31245 31245 31252 31252
     31255 31255 31257 31258 31260 31260 31263 31264 31278 31278 31281
     31281 31286 31287 31291 31293 31295 31296 31302 31302 31305 31305
     31309 31310 31319 31319 31329 31330 31337 31337 31339 31339 31344
     31344 31348 31348 31350 31350 31353 31354 31357 31357 31359 31359
     31361 31361 31364 31364 31368 31368 31378 31379 31381 31381 31384
     31384 31391 31391 31401 31402 31406 31407 31418 31418 31428 31429
     31431 31431 31434 31435 31447 31447 31449 31449 31453 31453 31455
     31456 31459 31459 31461 31461 31466 31466 31469 31469 31471 31471
     31478 31478 31481 31482 31487 31487 31503 31503 31505 31505 31513
     31513 31515 31515 31518 31518 31520 31520 31526 31526 31532 31533
     31545 31545 31558 31558 31561 31561 31563 31565 31567 31570 31572
     31572 31574 31574 31584 31584 31596 31596 31598 31598 31605 31605
     31613 31613 31623 31623 31627 31627 31631 31631 31636 31637 31639
     31639 31642 31642 31645 31645 31649 31649 31661 31661 31665 31665
     31668 31668 31672 31672 31680 31681 31684 31684 31686 31687 31689
     31689 31698 31698 31712 31712 31716 31716 31721 31721 31751 31751
     31762 31762 31774 31774 31777 31777 31783 31783 31786 31787 31805
     31807 31811 31811 31820 31821 31840 31840 31844 31844 31852 31852
     31859 31859 31875 31875 31881 31881 31890 31890 31893 31893 31895
     31896 31903 31903 31909 31909 31911 31911 31918 31918 31921 31923
     31929 31929 31934 31934 31946 31946 31958 31958 31966 31968 31975
     31975 31995 31995 31998 31998 32000 32000 32002 32002 32004 32008
     32010 32011 32013 32013 32016 32016 32020 32020 32023 32027 32032
     32034 32043 32044 32046 32048 32051 32051 32053 32053 32057 32058
     32066 32070 32080 32080 32094 32094 32097 32098 32102 32102 32104
     32104 32106 32106 32110 32110 32113 32115 32118 32118 32121 32121
     32127 32127 32142 32143 32147 32147 32156 32156 32160 32160 32162
     32162 32172 32173 32177 32178 32180 32181 32184 32184 32186 32187
     32189 32191 32199 32199 32202 32203 32214 32214 32216 32216 32218
     32218 32221 32222 32224 32225 32227 32227 32232 32233 32236 32236
     32239 32239 32244 32244 32251 32251 32265 32266 32277 32277 32283
     32283 32285 32287 32289 32289 32291 32291 32299 32299 32302 32303
     32305 32305 32311 32311 32317 32318 32321 32321 32323 32323 32326
     32327 32338 32338 32340 32341 32350 32350 32353 32353 32361 32363
     32365 32365 32368 32368 32377 32377 32380 32380 32386 32386 32396
     32396 32399 32399 32403 32403 32406 32406 32408 32408 32411 32412
     32566 32566 32568 32568 32570 32570 32588 32588 32592 32592 32596
     32597 32618 32619 32622 32622 32624 32624 32626 32626 32629 32629
     32631 32631 32633 32633 32645 32645 32648 32648 32650 32650 32652
     32652 32654 32654 32660 32660 32666 32666 32670 32670 32676 32676
     32680 32681 32690 32690 32696 32697 32701 32701 32705 32705 32709
     32709 32714 32714 32716 32716 32718 32718 32722 32722 32724 32725
     32735 32737 32745 32745 32747 32747 32752 32752 32761 32761 32764
     32764 32768 32769 32771 32771 32773 32774 32777 32777 32780 32780
     32784 32784 32789 32789 32791 32792 32813 32813 32819 32819 32822
     32822 32829 32829 32831 32831 32835 32835 32838 32838 32842 32842
     32854 32854 32856 32856 32858 32858 32862 32862 32879 32880 32882
     32883 32887 32887 32893 32895 32900 32903 32905 32905 32907 32908
     32918 32918 32923 32923 32925 32925 32929 32930 32933 32933 32937
     32938 32943 32943 32945 32946 32948 32948 32954 32954 32963 32964
     32972 32972 32974 32974 32986 32987 32990 32990 32993 32993 32996
     32997 33009 33009 33012 33012 33016 33016 33021 33021 33026 33026
     33029 33032 33034 33034 33048 33048 33050 33051 33059 33059 33065
     33065 33067 33067 33071 33071 33081 33081 33086 33086 33099 33099
     33102 33102 33104 33105 33108 33109 33125 33126 33131 33131 33136
     33137 33144 33146 33151 33152 33160 33160 33162 33162 33167 33167
     33178 33178 33180 33181 33184 33184 33187 33187 33192 33192 33203
     33203 33205 33205 33210 33210 33213 33216 33218 33218 33222 33222
     33229 33229 33240 33240 33247 33247 33251 33251 33253 33253 33255
     33256 33258 33258 33261 33261 33267 33268 33274 33276 33278 33278
     33285 33285 33287 33288 33290 33290 33292 33293 33298 33298 33307
     33308 33310 33311 33313 33313 33322 33324 33333 33335 33337 33337
     33344 33344 33349 33349 33351 33351 33369 33369 33380 33380 33382
     33382 33390 33391 33393 33394 33398 33398 33400 33400 33406 33406
     33419 33419 33421 33422 33426 33426 33433 33434 33437 33437 33439
     33439 33445 33446 33449 33449 33452 33455 33457 33457 33459 33459
     33463 33465 33467 33469 33471 33471 33489 33490 33492 33493 33495
     33495 33499 33499 33502 33503 33505 33505 33509 33511 33521 33521
     33533 33534 33537 33541 33545 33545 33559 33559 33576 33576 33579
     33579 33583 33583 33585 33585 33588 33590 33592 33593 33600 33600
     33607 33607 33609 33610 33615 33615 33617 33618 33651 33651 33655
     33655 33659 33659 33673 33674 33678 33678 33686 33686 33688 33688
     33694 33694 33698 33698 33705 33707 33725 33725 33729 33729 33733
     33733 33737 33738 33740 33740 33747 33747 33750 33750 33756 33756
     33769 33769 33771 33771 33775 33778 33780 33780 33785 33785 33789
     33789 33795 33796 33802 33802 33804 33806 33833 33833 33836 33836
     33841 33841 33848 33848 33853 33853 33865 33865 33879 33879 33883
     33883 33889 33889 33891 33891 33894 33894 33899 33900 33903 33903
     33909 33909 33914 33914 33936 33936 33940 33940 33945 33945 33948
     33948 33953 33953 33970 33970 33976 33976 33979 33980 33983 33984
     33986 33986 33988 33988 33990 33990 33993 33993 33995 33995 33997
     33997 34001 34001 34010 34010 34028 34028 34030 34030 34036 34036
     34044 34044 34065 34065 34067 34068 34071 34072 34074 34074 34078
     34078 34081 34081 34083 34083 34085 34085 34092 34093 34095 34095
     34109 34109 34111 34111 34113 34113 34115 34115 34121 34121 34126
     34126 34131 34131 34137 34137 34147 34147 34152 34154 34157 34157
     34180 34180 34183 34183 34191 34191 34193 34193 34196 34196 34203
     34203 34214 34214 34216 34218 34223 34224 34234 34234 34241 34241
     34249 34249 34253 34255 34261 34261 34268 34269 34276 34277 34281
     34282 34295 34295 34298 34299 34303 34303 34306 34306 34310 34311
     34314 34314 34326 34327 34330 34330 34349 34349 34367 34367 34382
     34382 34384 34384 34388 34389 34395 34396 34398 34399 34407 34407
     34425 34425 34442 34442 34444 34444 34451 34451 34467 34468 34473
     34473 34503 34503 34507 34507 34516 34516 34521 34521 34523 34523
     34527 34527 34532 34532 34541 34541 34558 34558 34560 34560 34562
     34563 34568 34568 34584 34584 34586 34586 34588 34588 34638 34638
     34645 34645 34647 34647 34655 34655 34662 34662 34664 34664 34676
     34676 34678 34678 34680 34680 34690 34690 34701 34701 34719 34719
     34722 34722 34739 34739 34746 34746 34756 34756 34784 34784 34796
     34796 34799 34799 34802 34802 34809 34809 34811 34811 34814 34814
     34821 34821 34847 34847 34850 34851 34865 34865 34870 34870 34875
     34875 34880 34880 34886 34886 34892 34893 34898 34899 34903 34903
     34905 34905 34907 34907 34909 34909 34913 34915 34920 34920 34923
     34923 34928 34928 34930 34930 34935 34935 34942 34943 34945 34946
     34952 34952 34955 34955 34957 34957 34962 34962 34966 34967 34974
     34974 34987 34987 34996 34996 35009 35010 35023 35023 35028 35029
     35033 35033 35036 35037 35039 35039 35041 35041 35048 35048 35059
     35061 35064 35064 35069 35069 35079 35079 35088 35088 35090 35091
     35096 35097 35109 35109 35114 35114 35126 35126 35128 35128 35131
     35131 35137 35137 35140 35140 35167 35167 35172 35172 35178 35178
     35186 35186 35199 35199 35201 35201 35203 35203 35206 35207 35211
     35211 35215 35215 35219 35219 35222 35222 35233 35233 35241 35242
     35250 35250 35258 35258 35261 35261 35264 35264 35282 35282 35299
     35299 35316 35316 35320 35320 35328 35328 35330 35331 35336 35336
     35338 35338 35340 35340 35342 35342 35347 35347 35350 35352 35355
     35355 35357 35357 35359 35359 35363 35363 35365 35365 35370 35370
     35373 35373 35377 35377 35380 35380 35382 35382 35386 35387 35408
     35408 35412 35413 35419 35419 35422 35422 35424 35424 35426 35427
     35430 35430 35433 35433 35437 35438 35440 35443 35445 35445 35449
     35449 35461 35461 35463 35463 35468 35469 35475 35475 35477 35477
     35480 35480 35486 35486 35488 35489 35491 35494 35496 35496 35498
     35498 35504 35504 35506 35506 35513 35513 35516 35516 35518 35519
     35522 35522 35524 35524 35527 35527 35531 35531 35533 35533 35535
     35535 35538 35538 35542 35542 35547 35548 35553 35553 35558 35559
     35562 35563 35565 35566 35569 35569 35574 35576 35578 35578 35582
     35582 35584 35586 35588 35588 35598 35598 35600 35600 35604 35604
     35606 35607 35609 35611 35613 35613 35616 35616 35624 35624 35627
     35628 35635 35635 35641 35641 35649 35649 35657 35657 35662 35663
     35672 35672 35674 35674 35676 35676 35686 35686 35692 35692 35695
     35696 35700 35700 35703 35703 35709 35709 35712 35712 35722 35722
     35728 35728 35730 35731 35734 35734 35738 35738 35895 35895 35903
     35903 35905 35905 35910 35910 35912 35912 35914 35914 35916 35916
     35925 35925 35930 35930 35937 35937 35946 35947 35961 35962 35970
     35970 35978 35978 35980 35980 35997 35998 36000 36002 36007 36012
     36015 36016 36019 36020 36022 36024 36027 36029 36031 36036 36039
     36040 36042 36042 36049 36049 36051 36051 36058 36058 36060 36060
     36062 36062 36064 36064 36066 36068 36070 36070 36074 36074 36077
     36077 36084 36084 36091 36093 36100 36101 36103 36104 36106 36106
     36109 36109 36115 36115 36118 36118 36196 36196 36198 36198 36203
     36203 36208 36208 36211 36212 36215 36215 36229 36229 36234 36234
     36249 36249 36259 36259 36264 36264 36275 36275 36282 36282 36286
     36286 36294 36294 36299 36300 36303 36303 36315 36315 36317 36317
     36321 36321 36323 36323 36328 36328 36335 36335 36339 36339 36362
     36362 36367 36368 36382 36382 36394 36394 36400 36400 36405 36405
     36418 36418 36420 36420 36423 36426 36441 36441 36447 36448 36468
     36468 36470 36470 36481 36481 36487 36487 36490 36490 36493 36493
     36522 36524 36544 36544 36554 36557 36562 36562 36575 36575 36587
     36587 36600 36600 36603 36603 36606 36606 36611 36611 36613 36613
     36617 36617 36626 36629 36635 36639 36646 36647 36649 36650 36655
     36655 36659 36659 36664 36665 36667 36667 36670 36671 36676 36677
     36681 36681 36685 36686 36701 36701 36703 36703 36706 36706 36763
     36764 36771 36771 36774 36774 36776 36776 36781 36781 36783 36786
     36802 36802 36805 36805 36814 36814 36817 36817 36820 36820 36838
     36838 36842 36843 36845 36845 36848 36848 36850 36850 36855 36855
     36857 36857 36861 36861 36864 36867 36869 36870 36872 36872 36875
     36875 36877 36877 36879 36881 36884 36885 36887 36887 36889 36890
     36893 36899 36910 36910 36913 36914 36917 36917 36920 36920 36924
     36924 36926 36926 36929 36930 36935 36935 36938 36939 36941 36942
     36944 36945 36947 36949 36953 36953 36956 36958 36960 36961 36963
     36963 36969 36969 36973 36975 36978 36978 36981 36981 36983 36984
     36986 36986 36988 36989 36991 36996 36999 37000 37002 37002 37007
     37007 37009 37009 37013 37013 37017 37017 37026 37027 37030 37030
     37032 37032 37034 37034 37039 37041 37045 37045 37048 37048 37057
     37057 37066 37066 37086 37086 37089 37089 37096 37096 37101 37101
     37109 37109 37117 37117 37122 37122 37138 37138 37141 37141 37145
     37145 37159 37159 37165 37165 37170 37170 37193 37198 37202 37202
     37218 37218 37225 37226 37228 37228 37237 37237 37239 37240 37255
     37255 37257 37257 37259 37259 37261 37261 37266 37266 37276 37276
     37291 37292 37294 37295 37297 37297 37300 37301 37312 37312 37319
     37319 37321 37321 37323 37329 37335 37336 37340 37341 37347 37347
     37351 37351 37354 37354 37365 37365 37389 37389 37392 37394 37399
     37399 37406 37406 37428 37428 37434 37434 37439 37440 37445 37445
     37449 37449 37463 37463 37467 37467 37470 37470 37474 37474 37476
     37478 37504 37504 37507 37507 37509 37509 37521 37521 37523 37523
     37526 37526 37528 37528 37532 37532 37555 37555 37558 37559 37561
     37561 37580 37580 37583 37583 37586 37586 37604 37604 37610 37610
     37624 37624 37628 37628 37636 37636 37648 37648 37656 37656 37658
     37658 37662 37666 37668 37668 37670 37670 37672 37672 37675 37675
     37678 37679 37704 37704 37706 37707 37709 37709 37716 37716 37723
     37723 37742 37742 37749 37749 37756 37756 37758 37758 37772 37772
     37780 37780 37782 37782 37786 37786 37795 37795 37799 37799 37804
     37805 37808 37808 37827 37827 37841 37841 37854 37854 37857 37857
     37860 37860 37878 37878 37892 37892 37912 37912 37925 37925 37931
     37931 37941 37941 37944 37944 37956 37956 37969 37970 37979 37979
     38013 38013 38015 38015 38263 38263 38272 38272 38275 38275 38281
     38281 38283 38283 38287 38287 38289 38292 38296 38296 38307 38309
     38312 38312 38317 38317 38321 38321 38331 38332 38343 38343 38346
     38346 38356 38358 38364 38364 38369 38370 38428 38428 38433 38433
     38442 38442 38446 38446 38450 38450 38459 38459 38463 38464 38466
     38466 38468 38468 38475 38477 38480 38480 38491 38495 38498 38500
     38506 38506 38512 38512 38515 38515 38517 38520 38525 38525 38533
     38534 38538 38539 38541 38542 38548 38549 38552 38553 38555 38556
     38563 38563 38567 38568 38570 38570 38577 38577 38583 38583 38587
     38587 38592 38593 38596 38599 38601 38601 38603 38606 38613 38614
     38617 38617 38619 38620 38626 38627 38632 38634 38639 38640 38642
     38642 38646 38647 38649 38649 38651 38651 38656 38656 38662 38663
     38673 38673 38675 38675 38678 38678 38681 38681 38684 38684 38686
     38686 38695 38695 38704 38704 38706 38706 38713 38713 38717 38717
     38722 38722 38724 38724 38728 38728 38737 38737 38742 38742 38748
     38748 38750 38750 38753 38754 38761 38761 38765 38765 38772 38772
     38775 38775 38778 38778 38795 38795 38797 38797 38799 38799 38816
     38816 38824 38824 38827 38827 38829 38829 38854 38854 38859 38859
     38867 38867 38876 38876 38899 38899 38902 38902 38907 38907 38911
     38915 38917 38918 38920 38920 38922 38922 38924 38924 38928 38931
     38935 38936 38957 38957 38960 38960 38968 38969 38971 38971 38982
     38982 38988 38990 38996 38996 39000 39000 39002 39002 39006 39006
     39013 39013 39015 39015 39019 39019 39023 39023 39080 39080 39087
     39087 39089 39089 39108 39108 39111 39111 39131 39132 39135 39135
     39137 39138 39149 39151 39156 39156 39164 39166 39171 39171 39177
     39178 39180 39180 39184 39184 39187 39187 39192 39192 39198 39198
     39200 39200 39208 39208 39237 39237 39241 39241 39243 39245 39249
     39250 39252 39252 39255 39255 39318 39318 39321 39321 39325 39325
     39333 39333 39336 39336 39340 39342 39345 39345 39347 39348 39353
     39353 39361 39361 39376 39378 39381 39381 39385 39385 39389 39389
     39391 39391 39405 39405 39409 39409 39423 39423 39425 39425 39432
     39432 39438 39439 39449 39449 39467 39467 39472 39472 39478 39479
     39488 39488 39491 39491 39493 39493 39501 39501 39509 39509 39511
     39511 39514 39515 39519 39519 39522 39522 39525 39525 39529 39530
     39592 39592 39608 39608 39635 39636 39640 39640 39653 39653 39662
     39662 39706 39706 39719 39719 39722 39722 39729 39729 39740 39740
     39745 39749 39759 39759 39764 39764 39770 39770 39791 39791 39822
     39822 39825 39825 39839 39839 39851 39851 39854 39854 39881 39881
     39894 39894 39908 39908 39912 39912 39949 39949 39952 39952 39954
     39954 39957 39957 39973 39973 39986 39986 39995 39995 40007 40007
     40009 40009 40023 40023 40165 40165 40167 40167 40169 40169 40179
     40180 40182 40182 40201 40201 40219 40219 40230 40230 40232 40232
     40251 40251 40273 40273 40285 40285 40288 40289 40300 40300 40306
     40306 40361 40361 40367 40367 40372 40372 40388 40388 40407 40407
     40434 40434 40440 40442 40474 40474 40478 40478 40565 40565 40569
     40569 40573 40573 40575 40575 40594 40595 40599 40599 40605 40605
     40607 40607 40613 40613 40628 40629 40635 40635 40638 40638 40643
     40643 40653 40654 40657 40657 40660 40660 40664 40664 40667 40668
     40670 40670 40680 40680 40692 40692 40711 40712 40718 40718 40723
     40723 40736 40736 40763 40763 40778 40779 40782 40782 40786 40786
     40799 40799 40801 40801 40807 40807 40810 40810 40812 40812 40823
     40823 40845 40845 40848 40848 40853 40853 40860 40860 44032 44033
     44036 44036 44039 44042 44048 44055 44057 44061 44064 44064 44068
     44068 44076 44077 44079 44081 44088 44089 44092 44092 44096 44096
     44107 44107 44109 44109 44116 44116 44120 44120 44124 44124 44144
     44145 44148 44148 44151 44152 44154 44154 44160 44161 44163 44166
     44169 44172 44176 44176 44180 44180 44188 44189 44191 44193 44200
     44202 44204 44204 44207 44208 44216 44217 44219 44221 44225 44225
     44228 44228 44232 44232 44236 44236 44245 44245 44247 44247 44256
     44257 44260 44260 44263 44264 44266 44266 44268 44268 44271 44273
     44275 44275 44277 44278 44284 44285 44288 44288 44292 44292 44294
     44294 44300 44301 44303 44303 44305 44305 44312 44312 44316 44316
     44320 44320 44329 44329 44332 44333 44340 44341 44344 44344 44348
     44348 44356 44357 44359 44359 44361 44361 44368 44368 44372 44372
     44376 44376 44385 44385 44387 44387 44396 44397 44400 44400 44403
     44406 44411 44413 44415 44415 44417 44418 44424 44425 44428 44428
     44432 44432 44444 44445 44452 44452 44471 44471 44480 44481 44484
     44484 44488 44488 44496 44497 44499 44499 44508 44508 44512 44512
     44516 44516 44536 44537 44540 44540 44543 44545 44552 44553 44555
     44555 44557 44557 44564 44564 44592 44593 44596 44596 44599 44600
     44602 44602 44608 44609 44611 44611 44613 44614 44618 44618 44620
     44622 44624 44624 44628 44628 44630 44630 44636 44637 44639 44641
     44645 44645 44648 44649 44652 44652 44656 44656 44664 44665 44667
     44669 44676 44677 44684 44684 44732 44734 44736 44736 44740 44740
     44748 44749 44751 44753 44760 44761 44764 44764 44776 44776 44779
     44779 44781 44781 44788 44788 44792 44792 44796 44796 44807 44808
     44813 44813 44816 44816 44844 44845 44848 44848 44850 44850 44852
     44852 44860 44861 44863 44863 44865 44867 44872 44873 44880 44880
     44892 44893 44900 44901 44921 44921 44928 44928 44932 44932 44936
     44936 44944 44945 44949 44949 44956 44956 44984 44985 44988 44988
     44992 44992 44999 45001 45003 45003 45005 45006 45012 45012 45020
     45020 45032 45033 45040 45041 45044 45044 45048 45048 45056 45057
     45060 45060 45068 45068 45072 45072 45076 45076 45084 45085 45096
     45096 45124 45125 45128 45128 45130 45130 45132 45132 45134 45134
     45139 45141 45143 45143 45145 45145 45149 45149 45180 45181 45184
     45184 45188 45188 45196 45197 45199 45199 45201 45201 45208 45210
     45212 45212 45215 45218 45224 45225 45227 45231 45233 45233 45235
     45237 45240 45240 45244 45244 45252 45253 45255 45257 45264 45265
     45268 45268 45272 45272 45280 45280 45285 45285 45320 45321 45323
     45324 45328 45328 45330 45331 45336 45337 45339 45341 45347 45349
     45352 45352 45356 45356 45364 45365 45367 45369 45376 45377 45380
     45380 45384 45384 45392 45393 45396 45397 45400 45400 45404 45404
     45408 45408 45432 45433 45436 45436 45440 45440 45442 45442 45448
     45449 45451 45451 45453 45453 45458 45460 45464 45464 45468 45468
     45480 45480 45516 45516 45520 45520 45524 45524 45532 45533 45535
     45535 45544 45545 45548 45548 45552 45552 45561 45561 45563 45563
     45565 45565 45572 45573 45576 45576 45579 45580 45588 45589 45591
     45591 45593 45593 45600 45600 45620 45620 45628 45628 45656 45656
     45660 45660 45664 45664 45672 45673 45684 45685 45692 45692 45700
     45701 45705 45705 45712 45713 45716 45716 45720 45722 45728 45729
     45731 45731 45733 45734 45738 45738 45740 45740 45744 45744 45748
     45748 45768 45769 45772 45772 45776 45776 45778 45778 45784 45785
     45787 45787 45789 45789 45794 45794 45796 45798 45800 45800 45803
     45807 45811 45813 45815 45819 45823 45825 45828 45828 45832 45832
     45840 45841 45843 45845 45852 45852 45908 45910 45912 45912 45915
     45916 45918 45919 45924 45925 45927 45927 45929 45929 45931 45931
     45934 45934 45936 45937 45940 45940 45944 45944 45952 45953 45955
     45957 45964 45964 45968 45968 45972 45972 45984 45985 45992 45992
     45996 45996 46020 46021 46024 46024 46027 46028 46030 46030 46032
     46032 46036 46037 46039 46039 46041 46041 46043 46043 46045 46045
     46048 46048 46052 46052 46056 46056 46076 46076 46096 46096 46104
     46104 46108 46108 46112 46112 46120 46121 46123 46123 46132 46132
     46160 46161 46164 46164 46168 46168 46176 46177 46179 46179 46181
     46181 46188 46188 46208 46208 46216 46216 46237 46237 46244 46244
     46248 46248 46252 46252 46261 46261 46263 46263 46265 46265 46272
     46272 46276 46276 46280 46280 46288 46288 46293 46293 46300 46301
     46304 46304 46307 46308 46310 46310 46316 46317 46319 46319 46321
     46321 46328 46328 46356 46357 46360 46360 46363 46364 46372 46373
     46375 46378 46384 46385 46388 46388 46392 46392 46400 46401 46403
     46405 46411 46413 46416 46416 46420 46420 46428 46429 46431 46433
     46496 46497 46500 46500 46504 46504 46506 46507 46512 46513 46515
     46517 46523 46525 46528 46528 46532 46532 46540 46541 46543 46545
     46552 46552 46572 46572 46608 46609 46612 46612 46616 46616 46629
     46629 46636 46636 46644 46644 46664 46664 46692 46692 46696 46696
     46748 46749 46752 46752 46756 46756 46763 46764 46769 46769 46804
     46804 46832 46832 46836 46836 46840 46840 46848 46849 46853 46853
     46888 46889 46892 46892 46895 46896 46904 46905 46907 46907 46916
     46916 46920 46920 46924 46924 46932 46933 46944 46944 46948 46948
     46952 46952 46960 46961 46963 46963 46965 46965 46972 46973 46976
     46976 46980 46980 46988 46989 46991 46994 46998 47001 47004 47004
     47008 47008 47016 47017 47019 47021 47028 47029 47032 47032 47047
     47047 47049 47049 47084 47085 47088 47088 47092 47092 47100 47101
     47103 47105 47111 47113 47116 47116 47120 47120 47128 47129 47131
     47131 47133 47133 47140 47141 47144 47144 47148 47148 47156 47157
     47159 47161 47168 47168 47172 47172 47185 47185 47187 47187 47196
     47197 47200 47200 47204 47204 47212 47213 47215 47215 47217 47217
     47224 47224 47228 47228 47245 47245 47272 47272 47280 47280 47284
     47284 47288 47288 47296 47297 47299 47299 47301 47301 47308 47308
     47312 47312 47316 47316 47325 47325 47327 47327 47329 47329 47336
     47337 47340 47340 47344 47344 47352 47353 47355 47355 47357 47357
     47364 47364 47384 47384 47392 47392 47420 47421 47424 47424 47428
     47428 47436 47436 47439 47439 47441 47441 47448 47449 47452 47452
     47456 47456 47464 47465 47467 47467 47469 47469 47476 47477 47480
     47480 47484 47484 47492 47493 47495 47495 47497 47498 47501 47502
     47532 47533 47536 47536 47540 47540 47548 47549 47551 47551 47553
     47553 47560 47561 47564 47564 47566 47570 47576 47577 47579 47579
     47581 47582 47585 47585 47587 47589 47592 47592 47596 47596 47604
     47605 47607 47610 47616 47617 47624 47624 47637 47637 47672 47673
     47676 47676 47680 47680 47682 47682 47688 47689 47691 47691 47693
     47694 47699 47701 47704 47704 47708 47708 47716 47717 47719 47721
     47728 47729 47732 47732 47736 47736 47747 47749 47751 47751 47756
     47756 47784 47785 47787 47788 47792 47792 47794 47794 47800 47801
     47803 47803 47805 47805 47812 47812 47816 47816 47832 47833 47868
     47868 47872 47872 47876 47876 47885 47885 47887 47887 47889 47889
     47896 47896 47900 47900 47904 47904 47913 47913 47915 47915 47924
     47926 47928 47928 47931 47934 47940 47941 47943 47943 47945 47945
     47949 47949 47951 47952 47956 47956 47960 47960 47969 47969 47971
     47971 47980 47980 48008 48008 48012 48012 48016 48016 48036 48036
     48040 48040 48044 48044 48052 48052 48055 48055 48064 48064 48068
     48068 48072 48072 48080 48080 48083 48083 48120 48121 48124 48124
     48127 48128 48130 48130 48136 48137 48139 48141 48143 48143 48145
     48145 48148 48152 48155 48159 48164 48165 48167 48167 48169 48169
     48173 48173 48176 48177 48180 48180 48184 48184 48192 48193 48195
     48197 48201 48201 48204 48205 48208 48208 48221 48221 48260 48261
     48264 48264 48267 48268 48270 48270 48276 48277 48279 48279 48281
     48282 48288 48289 48292 48292 48295 48296 48304 48305 48307 48309
     48316 48317 48320 48320 48324 48324 48333 48333 48335 48337 48341
     48341 48344 48344 48348 48348 48372 48374 48376 48376 48380 48380
     48388 48389 48391 48391 48393 48393 48400 48400 48404 48404 48420
     48420 48428 48428 48448 48448 48456 48457 48460 48460 48464 48464
     48472 48473 48484 48484 48488 48488 48512 48513 48516 48516 48519
     48522 48528 48529 48531 48531 48533 48533 48537 48538 48540 48540
     48548 48548 48560 48560 48568 48568 48596 48597 48600 48600 48604
     48604 48617 48617 48624 48624 48628 48628 48632 48632 48640 48640
     48643 48643 48645 48645 48652 48653 48656 48656 48660 48660 48668
     48669 48671 48671 48708 48709 48712 48712 48716 48716 48718 48718
     48724 48725 48727 48727 48729 48731 48736 48737 48740 48740 48744
     48744 48746 48746 48752 48753 48755 48757 48763 48765 48768 48768
     48772 48772 48780 48781 48783 48785 48792 48793 48808 48808 48848
     48849 48852 48852 48855 48856 48864 48864 48867 48869 48876 48876
     48897 48897 48904 48905 48920 48921 48923 48925 48960 48961 48964
     48964 48968 48968 48976 48977 48981 48981 49044 49044 49072 49072
     49093 49093 49100 49101 49104 49104 49108 49108 49116 49116 49119
     49119 49121 49121 49212 49212 49233 49233 49240 49240 49244 49244
     49248 49248 49256 49257 49296 49297 49300 49300 49304 49304 49312
     49313 49315 49315 49317 49317 49324 49325 49327 49328 49331 49334
     49340 49341 49343 49345 49349 49349 49352 49353 49356 49356 49360
     49360 49368 49369 49371 49373 49380 49381 49384 49384 49388 49388
     49396 49397 49399 49399 49401 49401 49408 49408 49412 49412 49416
     49416 49424 49424 49429 49429 49436 49440 49443 49444 49446 49447
     49452 49453 49455 49457 49462 49462 49464 49465 49468 49468 49472
     49472 49480 49481 49483 49485 49492 49493 49496 49496 49500 49500
     49508 49509 49511 49513 49520 49520 49524 49524 49528 49528 49541
     49541 49548 49550 49552 49552 49556 49556 49558 49558 49564 49565
     49567 49567 49569 49569 49573 49573 49576 49577 49580 49580 49584
     49584 49597 49597 49604 49604 49608 49608 49612 49612 49620 49620
     49623 49624 49632 49632 49636 49636 49640 49640 49648 49649 49651
     49651 49660 49661 49664 49664 49668 49668 49676 49677 49679 49679
     49681 49681 49688 49689 49692 49692 49695 49696 49704 49705 49707
     49707 49709 49709 49711 49711 49713 49714 49716 49716 49736 49736
     49744 49745 49748 49748 49752 49752 49760 49760 49765 49765 49772
     49773 49776 49776 49780 49780 49788 49789 49791 49791 49793 49793
     49800 49801 49808 49808 49816 49816 49819 49819 49821 49821 49828
     49829 49832 49832 49836 49837 49844 49845 49847 49847 49849 49849
     49884 49885 49888 49888 49891 49892 49899 49901 49903 49903 49905
     49905 49910 49910 49912 49913 49915 49916 49920 49920 49928 49929
     49932 49933 49939 49941 49944 49944 49948 49948 49956 49957 49960
     49961 49989 49989 50024 50025 50028 50028 50032 50032 50034 50034
     50040 50041 50044 50045 50052 50052 50056 50056 50060 50060 50112
     50112 50136 50137 50140 50140 50143 50144 50146 50146 50152 50153
     50157 50157 50164 50165 50168 50168 50184 50184 50192 50192 50212
     50212 50220 50220 50224 50224 50228 50228 50236 50237 50248 50248
     50276 50277 50280 50280 50284 50284 50292 50293 50297 50297 50304
     50304 50324 50324 50332 50332 50360 50360 50364 50364 50409 50409
     50416 50417 50420 50420 50424 50424 50426 50426 50431 50433 50444
     50444 50448 50448 50452 50452 50460 50460 50472 50473 50476 50476
     50480 50480 50488 50489 50491 50491 50493 50493 50500 50501 50504
     50506 50508 50510 50515 50517 50519 50521 50525 50526 50528 50529
     50532 50532 50536 50536 50544 50545 50547 50549 50556 50557 50560
     50560 50564 50564 50567 50567 50572 50573 50575 50575 50577 50577
     50581 50581 50583 50584 50588 50588 50592 50592 50601 50601 50612
     50613 50616 50617 50619 50622 50628 50634 50636 50636 50638 50638
     50640 50641 50644 50644 50648 50648 50656 50657 50659 50659 50661
     50661 50668 50670 50672 50672 50676 50676 50678 50679 50684 50689
     50693 50696 50700 50700 50704 50704 50712 50713 50715 50716 50724
     50725 50728 50728 50732 50734 50736 50736 50739 50741 50743 50743
     50745 50745 50747 50747 50752 50753 50756 50756 50760 50760 50768
     50769 50771 50773 50780 50781 50784 50784 50796 50796 50799 50799
     50801 50801 50808 50809 50812 50812 50816 50816 50824 50825 50827
     50827 50829 50829 50836 50837 50840 50840 50844 50844 50852 50853
     50855 50855 50857 50857 50864 50865 50868 50868 50872 50874 50880
     50881 50883 50883 50885 50885 50892 50893 50896 50896 50900 50900
     50908 50909 50912 50913 50920 50921 50924 50924 50928 50928 50936
     50937 50941 50941 50948 50949 50952 50952 50956 50956 50964 50965
     50967 50967 50969 50969 50976 50977 50980 50980 50984 50984 50992
     50993 50995 50995 50997 50997 50999 50999 51004 51005 51008 51008
     51012 51012 51018 51018 51020 51021 51023 51023 51025 51032 51036
     51036 51040 51040 51048 51048 51051 51051 51060 51061 51064 51064
     51068 51070 51075 51077 51079 51082 51086 51086 51088 51089 51092
     51092 51094 51096 51098 51098 51104 51105 51107 51110 51116 51117
     51120 51120 51124 51124 51132 51133 51135 51137 51144 51145 51148
     51148 51150 51150 51152 51152 51160 51160 51165 51165 51172 51172
     51176 51176 51180 51180 51200 51201 51204 51204 51208 51208 51210
     51210 51216 51217 51219 51219 51221 51222 51228 51229 51232 51232
     51236 51236 51244 51245 51247 51247 51249 51249 51256 51256 51260
     51260 51264 51264 51272 51273 51276 51277 51284 51284 51312 51313
     51316 51316 51320 51320 51322 51322 51328 51329 51331 51331 51333
     51335 51339 51341 51348 51348 51357 51357 51359 51359 51361 51361
     51368 51368 51388 51389 51396 51396 51400 51400 51404 51404 51412
     51413 51415 51415 51417 51417 51424 51425 51428 51428 51445 51445
     51452 51453 51456 51456 51460 51462 51468 51469 51471 51471 51473
     51473 51480 51480 51500 51500 51508 51508 51536 51537 51540 51540
     51544 51544 51552 51553 51555 51555 51564 51564 51568 51568 51572
     51572 51580 51580 51592 51593 51596 51596 51600 51600 51608 51609
     51611 51611 51613 51613 51648 51649 51652 51652 51655 51656 51658
     51658 51664 51665 51667 51667 51669 51670 51673 51674 51676 51677
     51680 51680 51682 51682 51684 51684 51687 51687 51692 51693 51695
     51697 51704 51705 51708 51708 51712 51712 51720 51721 51723 51725
     51732 51732 51736 51736 51753 51753 51788 51789 51792 51792 51796
     51796 51804 51805 51807 51809 51816 51816 51837 51837 51844 51844
     51864 51864 51900 51901 51904 51904 51908 51908 51916 51917 51919
     51919 51921 51921 51923 51923 51928 51929 51936 51936 51948 51948
     51956 51956 51976 51976 51984 51984 51988 51988 51992 51992 52000
     52001 52033 52033 52040 52041 52044 52044 52048 52048 52056 52057
     52061 52061 52068 52068 52088 52089 52124 52124 52152 52152 52180
     52180 52196 52196 52199 52199 52201 52201 52236 52237 52240 52240
     52244 52244 52252 52253 52257 52258 52263 52265 52268 52268 52270
     52270 52272 52272 52280 52281 52283 52286 52292 52293 52296 52296
     52300 52300 52308 52309 52311 52313 52320 52320 52324 52324 52326
     52326 52328 52328 52336 52336 52341 52341 52376 52377 52380 52380
     52384 52384 52392 52393 52395 52397 52404 52405 52408 52408 52412
     52412 52420 52421 52423 52423 52425 52425 52432 52432 52436 52436
     52452 52452 52460 52460 52464 52464 52481 52481 52488 52489 52492
     52492 52496 52496 52504 52505 52507 52507 52509 52509 52516 52516
     52520 52520 52524 52524 52537 52537 52572 52572 52576 52576 52580
     52580 52588 52589 52591 52591 52593 52593 52600 52600 52616 52616
     52628 52629 52632 52632 52636 52636 52644 52645 52647 52647 52649
     52649 52656 52656 52676 52676 52684 52684 52688 52688 52712 52712
     52716 52716 52720 52720 52728 52729 52731 52731 52733 52733 52740
     52740 52744 52744 52748 52748 52756 52756 52761 52761 52768 52769
     52772 52772 52776 52776 52784 52785 52787 52787 52789 52789 52824
     52825 52828 52828 52831 52833 52840 52841 52843 52843 52845 52845
     52852 52853 52856 52856 52860 52860 52868 52869 52871 52871 52873
     52873 52880 52881 52884 52884 52888 52888 52896 52897 52899 52901
     52908 52909 52929 52929 52964 52965 52968 52968 52971 52972 52980
     52981 52983 52985 52992 52993 52996 52996 53000 53000 53008 53009
     53011 53011 53013 53013 53020 53020 53024 53024 53028 53028 53036
     53037 53039 53041 53048 53048 53076 53077 53080 53080 53084 53084
     53092 53093 53095 53095 53097 53097 53104 53105 53108 53108 53112
     53112 53120 53120 53125 53125 53132 53132 53153 53153 53160 53160
     53168 53168 53188 53188 53216 53217 53220 53220 53224 53224 53232
     53233 53235 53235 53237 53237 53244 53244 53248 53248 53252 53252
     53265 53265 53272 53272 53293 53293 53300 53301 53304 53304 53308
     53308 53316 53317 53319 53319 53321 53321 53328 53328 53332 53332
     53336 53336 53344 53344 53356 53357 53360 53360 53364 53364 53372
     53373 53377 53377 53412 53413 53416 53416 53420 53420 53428 53429
     53431 53431 53433 53433 53440 53441 53444 53444 53448 53449 53456
     53457 53459 53461 53468 53469 53472 53472 53476 53476 53484 53485
     53487 53489 53496 53496 53517 53517 53552 53553 53556 53556 53560
     53560 53562 53562 53568 53569 53571 53573 53580 53581 53584 53584
     53588 53588 53596 53597 53599 53599 53601 53601 53608 53608 53612
     53612 53628 53628 53636 53636 53640 53640 53664 53665 53668 53668
     53672 53672 53680 53681 53683 53683 53685 53685 53690 53690 53692
     53692 53696 53696 53720 53720 53748 53748 53752 53752 53767 53767
     53769 53769 53776 53776 53804 53805 53808 53808 53812 53812 53820
     53821 53823 53823 53825 53825 53832 53832 53852 53852 53860 53860
     53888 53889 53892 53892 53896 53896 53904 53905 53909 53909 53916
     53916 53920 53920 53924 53924 53932 53932 53937 53937 53944 53945
     53948 53948 53951 53952 53954 53954 53960 53961 53963 53963 53972
     53972 53976 53976 53980 53980 53988 53989 54000 54001 54004 54004
     54008 54008 54016 54017 54019 54019 54021 54021 54028 54030 54032
     54032 54036 54036 54038 54038 54044 54045 54047 54049 54053 54053
     54056 54057 54060 54060 54064 54064 54072 54073 54075 54077 54084
     54085 54140 54141 54144 54144 54148 54148 54156 54157 54159 54161
     54168 54169 54172 54172 54176 54176 54184 54185 54187 54187 54189
     54189 54196 54196 54200 54200 54204 54204 54212 54213 54216 54217
     54224 54224 54232 54232 54241 54241 54243 54243 54252 54253 54256
     54256 54260 54260 54268 54269 54271 54271 54273 54273 54280 54280
     54301 54301 54336 54336 54340 54340 54364 54364 54368 54368 54372
     54372 54381 54381 54383 54383 54392 54393 54396 54396 54399 54400
     54402 54402 54408 54409 54411 54411 54413 54413 54420 54420 54441
     54441 54476 54476 54480 54480 54484 54484 54492 54492 54495 54495
     54504 54504 54508 54508 54512 54512 54520 54520 54523 54523 54525
     54525 54532 54532 54536 54536 54540 54540 54548 54549 54551 54551
     54588 54589 54592 54592 54596 54596 54604 54605 54607 54607 54609
     54609 54616 54617 54620 54620 54624 54624 54629 54629 54632 54633
     54635 54635 54637 54637 54644 54645 54648 54648 54652 54652 54660
     54661 54663 54665 54672 54672 54693 54693 54728 54729 54732 54732
     54736 54736 54738 54738 54744 54745 54747 54747 54749 54749 54756
     54757 54760 54760 54764 54764 54772 54773 54775 54775 54777 54777
     54784 54785 54788 54788 54792 54792 54800 54801 54803 54805 54812
     54812 54816 54816 54820 54820 54829 54829 54840 54841 54844 54844
     54848 54848 54853 54853 54856 54857 54859 54859 54861 54861 54865
     54865 54868 54869 54872 54872 54876 54876 54887 54887 54889 54889
     54896 54897 54900 54900 54915 54915 54917 54917 54924 54925 54928
     54928 54932 54932 54941 54941 54943 54943 54945 54945 54952 54952
     54956 54956 54960 54960 54969 54969 54971 54971 54980 54981 54984
     54984 54988 54988 54993 54993 54996 54996 54999 54999 55001 55001
     55008 55008 55012 55012 55016 55016 55024 55024 55029 55029 55036
     55037 55040 55040 55044 55044 55057 55057 55064 55065 55068 55068
     55072 55072 55080 55081 55083 55083 55085 55085 55092 55093 55096
     55096 55100 55100 55108 55108 55111 55111 55113 55113 55120 55121
     55124 55124 55126 55129 55136 55137 55139 55139 55141 55141 55145
     55145 55148 55148 55152 55152 55156 55156 55164 55165 55169 55169
     55176 55177 55180 55180 55184 55184 55192 55193 55195 55195 55197
     55197 63744 64011 65281 65374 65504 65507 65509 65510 917504
     917631))
  (MAKE-CHARACTER-SET :MIB-ENUM 2086 :NAME "IBM866" :ALIASES
   '("csIBM866" "866" "cp866") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLDG Volume 2 (SE09-8002-03) August 1994" :COMMENTS 'NIL
   :REFERENCES '("[Pond]") :RANGES
   #(0 127 160 160 164 164 176 176 183 183 1025 1025 1028 1028 1031 1031
     1038 1038 1040 1103 1105 1105 1108 1108 1111 1111 1118 1118 8470
     8470 8729 8730 9472 9472 9474 9474 9484 9484 9488 9488 9492 9492
     9496 9496 9500 9500 9508 9508 9516 9516 9524 9524 9532 9532 9552
     9580 9600 9600 9604 9604 9608 9608 9612 9612 9616 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 2088 :NAME "KOI8-U" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE '"RFC 2319" :COMMENTS 'NIL :REFERENCES
   '("[RFC2319]") :RANGES
   #(0 127 160 160 169 169 176 176 178 178 183 183 247 247 1025 1025
     1028 1028 1030 1031 1040 1103 1105 1105 1108 1108 1110 1111 1168
     1169 8729 8730 8776 8776 8804 8805 8992 8993 9472 9472 9474 9474
     9484 9484 9488 9488 9492 9492 9496 9496 9500 9500 9508 9508 9516
     9516 9524 9524 9532 9532 9552 9554 9556 9556 9559 9563 9565 9569
     9571 9571 9574 9578 9580 9580 9600 9600 9604 9604 9608 9608 9612
     9612 9616 9619 9632 9632))
  (MAKE-CHARACTER-SET :MIB-ENUM 2083 :NAME "VIQR" :ALIASES '("csVIQR")
   :MIME-ENCODING 'NIL :SOURCE '"RFC 1456" :COMMENTS 'NIL :REFERENCES
   '("[RFC1456]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2103 :NAME "PTCP154" :ALIASES
   '("Cyrillic-Asian" "CP154" "PT154" "csPTCP154") :MIME-ENCODING 'NIL
   :SOURCE '"See (http://www.iana.org/assignments/charset-reg/PTCP154)"
   :COMMENTS 'NIL :REFERENCES '("[Uskov]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2045 :NAME "IBM851" :ALIASES
   '("csIBM851" "851" "cp851") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2085 :NAME "HZ-GB-2312" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"RFC 1842, RFC 1843                                               [RFC1842, RFC1843]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2107 :NAME "TSCII" :ALIASES '("csTSCII")
   :MIME-ENCODING 'NIL :SOURCE
   '"See <http://www.iana.org/assignments/charset-reg/TSCII>           [Kalyanasundaram]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2106 :NAME "BRF" :ALIASES '("csBRF")
   :MIME-ENCODING 'NIL :SOURCE
   '"See <http://www.iana.org/assignments/charset-reg/BRF>                    [Thibault]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2105 :NAME "KOI7-switched" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"See <http://www.iana.org/assignments/charset-reg/KOI7-switched>"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2104 :NAME "Amiga-1251" :ALIASES
   '("Ami-1251" "Amiga1251" "Ami1251") :MIME-ENCODING 'NIL :SOURCE
   '"See (http://www.amiga.ultranet.ru/Amiga-1251.html)" :COMMENTS
   '("															"
     "(Aliases are provided for historical reasons and should not be used) [Malyshev]")
   :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2102 :NAME "IBM1047" :ALIASES
   '("IBM-1047") :MIME-ENCODING 'NIL :SOURCE
   '"IBM1047 (EBCDIC Latin 1/Open Systems)" :COMMENTS
   '("http://www-1.ibm.com/servers/eserver/iseries/software/globalization/pdf/cp01047z.pdf")
   :REFERENCES '("[Robrigado]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2100 :NAME "IBM01149" :ALIASES
   '("ebcdic-is-871+euro" "CP01149" "CCSID01149") :MIME-ENCODING 'NIL
   :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01149)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2099 :NAME "IBM01148" :ALIASES
   '("ebcdic-international-500+euro" "CP01148" "CCSID01148")
   :MIME-ENCODING 'NIL :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01148)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2098 :NAME "IBM01147" :ALIASES
   '("ebcdic-fr-297+euro" "CP01147" "CCSID01147") :MIME-ENCODING 'NIL
   :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01147)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2097 :NAME "IBM01146" :ALIASES
   '("ebcdic-gb-285+euro" "CP01146" "CCSID01146") :MIME-ENCODING 'NIL
   :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01146)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2096 :NAME "IBM01145" :ALIASES
   '("ebcdic-es-284+euro" "CP01145" "CCSID01145") :MIME-ENCODING 'NIL
   :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01145)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2095 :NAME "IBM01144" :ALIASES
   '("ebcdic-it-280+euro" "CP01144" "CCSID01144") :MIME-ENCODING 'NIL
   :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01144)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2094 :NAME "IBM01143" :ALIASES
   '("ebcdic-se-278+euro" "ebcdic-fi-278+euro" "CP01143" "CCSID01143")
   :MIME-ENCODING 'NIL :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01143)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2093 :NAME "IBM01142" :ALIASES
   '("ebcdic-no-277+euro" "ebcdic-dk-277+euro" "CP01142" "CCSID01142")
   :MIME-ENCODING 'NIL :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01142)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2092 :NAME "IBM01141" :ALIASES
   '("ebcdic-de-273+euro" "CP01141" "CCSID01141") :MIME-ENCODING 'NIL
   :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01141)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2091 :NAME "IBM01140" :ALIASES
   '("ebcdic-us-37+euro" "CP01140" "CCSID01140") :MIME-ENCODING 'NIL
   :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM01140)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2090 :NAME "IBM00924" :ALIASES
   '("ebcdic-Latin9--euro" "CP00924" "CCSID00924") :MIME-ENCODING 'NIL
   :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM00924)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2089 :NAME "IBM00858" :ALIASES
   '("PC-Multilingual-850+euro" "CP00858" "CCSID00858") :MIME-ENCODING
   'NIL :SOURCE
   '"IBM See (http://www.iana.org/assignments/charset-reg/IBM00858)    [Mahdi]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2081 :NAME "MNEM" :ALIASES '("csMnem")
   :MIME-ENCODING 'NIL :SOURCE
   '"RFC 1345, also known as \"mnemonic+ascii+8200\"" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2080 :NAME "MNEMONIC" :ALIASES
   '("csMnemonic") :MIME-ENCODING 'NIL :SOURCE
   '"RFC 1345, also known as \"mnemonic+ascii+38\"" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2079 :NAME "UNKNOWN-8BIT" :ALIASES
   '("csUnknown8BiT") :MIME-ENCODING 'NIL :SOURCE 'NIL :COMMENTS 'NIL
   :REFERENCES '("[RFC1428]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2078 :NAME "EBCDIC-US" :ALIASES
   '("csEBCDICUS") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2077 :NAME "EBCDIC-UK" :ALIASES
   '("csEBCDICUK") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2076 :NAME "EBCDIC-ES-S" :ALIASES
   '("csEBCDICESS") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2075 :NAME "EBCDIC-ES-A" :ALIASES
   '("csEBCDICESA") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2074 :NAME "EBCDIC-ES" :ALIASES
   '("csEBCDICES") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2073 :NAME "EBCDIC-PT" :ALIASES
   '("csEBCDICPT") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2072 :NAME "EBCDIC-IT" :ALIASES
   '("csEBCDICIT") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2071 :NAME "EBCDIC-FR" :ALIASES
   '("csEBCDICFR") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2070 :NAME "EBCDIC-FI-SE-A" :ALIASES
   '("csEBCDICFISEA") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2069 :NAME "EBCDIC-FI-SE" :ALIASES
   '("csEBCDICFISE") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2068 :NAME "EBCDIC-DK-NO-A" :ALIASES
   '("csEBCDICDKNOA") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2067 :NAME "EBCDIC-DK-NO" :ALIASES
   '("csEBCDICDKNO") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2066 :NAME "EBCDIC-CA-FR" :ALIASES
   '("csEBCDICCAFR") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2065 :NAME "EBCDIC-AT-DE-A" :ALIASES
   '("csEBCDICATDEA") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2064 :NAME "EBCDIC-AT-DE" :ALIASES
   '("csIBMEBCDICATDE") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2063 :NAME "IBM1026" :ALIASES
   '("csIBM1026" "CP1026") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2062 :NAME "IBM918" :ALIASES
   '("csIBM918" "ebcdic-cp-ar2" "CP918") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2061 :NAME "IBM905" :ALIASES
   '("csIBM905" "ebcdic-cp-tr" "CP905") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3174 Character Set Ref, GA27-3831-02, March 1990" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2060 :NAME "IBM904" :ALIASES
   '("csIBBM904" "904" "cp904") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2059 :NAME "IBM903" :ALIASES
   '("csIBM903" "cp903") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2058 :NAME "IBM891" :ALIASES
   '("csIBM891" "cp891") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2057 :NAME "IBM880" :ALIASES
   '("csIBM880" "EBCDIC-Cyrillic" "cp880") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2056 :NAME "IBM871" :ALIASES
   '("csIBM871" "ebcdic-cp-is" "CP871") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2055 :NAME "IBM870" :ALIASES
   '("csIBM870" "ebcdic-cp-yu" "ebcdic-cp-roece" "CP870") :MIME-ENCODING
   'NIL :SOURCE '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2053 :NAME "IBM868" :ALIASES
   '("csIBM868" "cp-ar" "CP868") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2044 :NAME "IBM500" :ALIASES
   '("csIBM500" "ebcdic-cp-ch" "ebcdic-cp-be" "CP500") :MIME-ENCODING
   'NIL :SOURCE '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2043 :NAME "IBM424" :ALIASES
   '("csIBM424" "ebcdic-cp-he" "cp424") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2042 :NAME "IBM423" :ALIASES
   '("csIBM423" "ebcdic-cp-gr" "cp423") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2041 :NAME "IBM420" :ALIASES
   '("csIBM420" "ebcdic-cp-ar1" "cp420") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990,  IBM NLS RM p 11-11"
   :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2040 :NAME "IBM297" :ALIASES
   '("csIBM297" "ebcdic-cp-fr" "cp297") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2039 :NAME "IBM290" :ALIASES
   '("csIBM290" "EBCDIC-JP-kana" "cp290") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3174 Character Set Ref, GA27-3831-02, March 1990" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2038 :NAME "IBM285" :ALIASES
   '("csIBM285" "ebcdic-cp-gb" "CP285") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2037 :NAME "IBM284" :ALIASES
   '("csIBM284" "ebcdic-cp-es" "CP284") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2036 :NAME "IBM281" :ALIASES
   '("csIBM281" "cp281" "EBCDIC-JP-E") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3174 Character Set Ref, GA27-3831-02, March 1990" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2035 :NAME "IBM280" :ALIASES
   '("csIBM280" "ebcdic-cp-it" "CP280") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2034 :NAME "IBM278" :ALIASES
   '("csIBM278" "ebcdic-cp-se" "ebcdic-cp-fi" "CP278") :MIME-ENCODING
   'NIL :SOURCE '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2033 :NAME "IBM277" :ALIASES
   '("csIBM277" "EBCDIC-CP-NO" "EBCDIC-CP-DK") :MIME-ENCODING 'NIL
   :SOURCE '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2032 :NAME "IBM275" :ALIASES
   '("csIBM275" "cp275" "EBCDIC-BR") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2031 :NAME "IBM274" :ALIASES
   '("csIBM274" "CP274" "EBCDIC-BE") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3174 Character Set Ref, GA27-3831-02, March 1990" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2030 :NAME "IBM273" :ALIASES
   '("csIBM273" "CP273") :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2029 :NAME "IBM038" :ALIASES
   '("csIBM038" "cp038" "EBCDIC-INT") :MIME-ENCODING 'NIL :SOURCE
   '"IBM 3174 Character Set Ref, GA27-3831-02, March 1990" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2028 :NAME "IBM037" :ALIASES
   '("csIBM037" "ebcdic-cp-nl" "ebcdic-cp-wt" "ebcdic-cp-ca"
     "ebcdic-cp-us" "cp037")
   :MIME-ENCODING 'NIL :SOURCE
   '"IBM NLS RM Vol2 SE09-8002-01, March 1990" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2024 :NAME "Windows-31J" :ALIASES
   '("csWindows31J") :MIME-ENCODING 'NIL :SOURCE
   '"Windows Japanese.  A further extension of Shift_JIS to include NEC special characters (Row 13), NEC selection of IBM extensions (Rows 89 to 92), and IBM extensions (Rows 115 to 119).  The CCS's are JIS X0201:1997, JIS X0208:1997, and these extensions. This charset can be used for the top-level media type \"text\", but it is of limited or specialized use (see RFC2278). PCL Symbol Set id: 19K"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2023 :NAME "Microsoft-Publishing"
   :ALIASES '("csMicrosoftPublishing") :MIME-ENCODING 'NIL :SOURCE
   '"PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 6J"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2022 :NAME "Ventura-Math" :ALIASES
   '("csVenturaMath") :MIME-ENCODING 'NIL :SOURCE
   '"PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 6M"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2021 :NAME "HP-DeskTop" :ALIASES
   '("csHPDesktop") :MIME-ENCODING 'NIL :SOURCE
   '"PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 7J"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2020 :NAME "Adobe-Symbol-Encoding"
   :ALIASES '("csHPPSMath") :MIME-ENCODING 'NIL :SOURCE
   '"PostScript Language Reference Manual PCL Symbol Set id: 5M"
   :COMMENTS 'NIL :REFERENCES '("[Adobe]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2019 :NAME "HP-Math8" :ALIASES
   '("csHPMath8") :MIME-ENCODING 'NIL :SOURCE
   '"PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 8M"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2018 :NAME "HP-Pi-font" :ALIASES
   '("csHPPiFont") :MIME-ENCODING 'NIL :SOURCE
   '"PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 15U"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2017 :NAME "HP-Legal" :ALIASES
   '("csHPLegal") :MIME-ENCODING 'NIL :SOURCE
   '"PCL 5 Comparison Guide, Hewlett-Packard, HP part number 5961-0510, October 1992 PCL Symbol Set id: 1U"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2016 :NAME "IBM-Thai" :ALIASES
   '("csIBMThai") :MIME-ENCODING 'NIL :SOURCE
   '"Presentation Set, CPGID: 838" :COMMENTS 'NIL :REFERENCES
   '("[IBM-CIDT]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2015 :NAME "IBM-Symbols" :ALIASES
   '("csIBMSymbols") :MIME-ENCODING 'NIL :SOURCE
   '"Presentation Set, CPGID: 259" :COMMENTS 'NIL :REFERENCES
   '("[IBM-CIDT]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2014 :NAME "PC8-Turkish" :ALIASES
   '("csPC8Turkish") :MIME-ENCODING 'NIL :SOURCE
   '"PC Latin Turkish.  PCL Symbol Set id: 9T" :COMMENTS 'NIL
   :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2012 :NAME "PC8-Danish-Norwegian"
   :ALIASES '("csPC8DanishNorwegian") :MIME-ENCODING 'NIL :SOURCE
   '"PC Danish Norwegian 8-bit PC set for Danish Norwegian PCL Symbol Set id: 11U"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2008 :NAME "DEC-MCS" :ALIASES
   '("csDECMCS" "dec") :MIME-ENCODING 'NIL :SOURCE
   '"VAX/VMS User's Manual,  Order Number: AI-Y517A-TE, April 1986."
   :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2007 :NAME "Ventura-International"
   :ALIASES '("csVenturaInternational") :MIME-ENCODING 'NIL :SOURCE
   '"Ventura International.  ASCII plus coded characters similar  to Roman8. PCL Symbol Set id: 13J"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2006 :NAME "Ventura-US" :ALIASES
   '("csVenturaUS") :MIME-ENCODING 'NIL :SOURCE
   '"Ventura US.  ASCII plus characters typically used in  publishing, like pilcrow, copyright, registered, trade mark, section, dagger, and double dagger in the range A0 (hex) to FF (hex). PCL Symbol Set id: 14J"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2005 :NAME "Adobe-Standard-Encoding"
   :ALIASES '("csAdobeStandardEncoding") :MIME-ENCODING 'NIL :SOURCE
   '"PostScript Language Reference Manual PCL Symbol Set id: 10J"
   :COMMENTS 'NIL :REFERENCES '("[Adobe]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2003 :NAME "ISO-8859-9-Windows-Latin-5"
   :ALIASES '("csWindows31Latin5") :MIME-ENCODING 'NIL :SOURCE
   '"Extended ISO 8859-9.  Latin-5 for Windows 3.1 PCL Symbol Set id: 5T"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2002 :NAME "ISO-8859-2-Windows-Latin-2"
   :ALIASES '("csWindows31Latin2") :MIME-ENCODING 'NIL :SOURCE
   '"Extended ISO 8859-2.  Latin-2 for Windows 3.1. PCL Symbol Set id: 9E"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2001 :NAME
   "ISO-8859-1-Windows-3.1-Latin-1" :ALIASES '("csWindows31Latin1")
   :MIME-ENCODING 'NIL :SOURCE
   '"Extended ISO 8859-1 Latin-1 for Windows 3.1.   PCL Symbol Set id: 19U"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 2000 :NAME
   "ISO-8859-1-Windows-3.0-Latin-1" :ALIASES '("csWindows30Latin1")
   :MIME-ENCODING 'NIL :SOURCE
   '"Extended ISO 8859-1 Latin-1 for Windows 3.0.   PCL Symbol Set id: 9U"
   :COMMENTS 'NIL :REFERENCES '("[HP-PCL5]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1020 :NAME "BOCU-1" :ALIASES
   '("csBOCU-1") :MIME-ENCODING 'NIL :SOURCE
   '"http://www.unicode.org/notes/tn6/" :COMMENTS 'NIL :REFERENCES
   '("[Scherer]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1018 :NAME "UTF-32BE" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"<http://www.unicode.org/unicode/reports/tr19/>" :COMMENTS 'NIL
   :REFERENCES '("[Davis]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1017 :NAME "UTF-32" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"<http://www.unicode.org/unicode/reports/tr19/>" :COMMENTS 'NIL
   :REFERENCES '("[Davis]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1016 :NAME "CESU-8" :ALIASES
   '("csCESU-8") :MIME-ENCODING 'NIL :SOURCE
   '"<http://www.unicode.org/unicode/reports/tr26>" :COMMENTS 'NIL
   :REFERENCES '("[Phipps]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1011 :NAME "SCSU" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE
   '"SCSU See (http://www.iana.org/assignments/charset-reg/SCSU)     [Scherer]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1010 :NAME "UNICODE-1-1" :ALIASES
   '("csUnicode11") :MIME-ENCODING 'NIL :SOURCE '"RFC 1641" :COMMENTS
   'NIL :REFERENCES '("[RFC1641]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1009 :NAME "ISO-Unicode-IBM-1265"
   :ALIASES '("csUnicodeIBM1265") :MIME-ENCODING 'NIL :SOURCE
   '"IBM Hebrew Presentation Set, GCSGID: 1265" :COMMENTS 'NIL
   :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1008 :NAME "ISO-Unicode-IBM-1264"
   :ALIASES '("csUnicodeIBM1264") :MIME-ENCODING 'NIL :SOURCE
   '"IBM Arabic Presentation Set, GCSGID: 1264" :COMMENTS 'NIL
   :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1007 :NAME "ISO-Unicode-IBM-1276"
   :ALIASES '("csUnicodeIBM1276") :MIME-ENCODING 'NIL :SOURCE
   '"IBM Cyrillic Greek Extended Presentation Set, GCSGID: 1276"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1006 :NAME "ISO-Unicode-IBM-1268"
   :ALIASES '("csUnicodeIBM1268") :MIME-ENCODING 'NIL :SOURCE
   '"IBM Latin-4 Extended Presentation Set, GCSGID: 1268" :COMMENTS 'NIL
   :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1005 :NAME "ISO-Unicode-IBM-1261"
   :ALIASES '("csUnicodeIBM1261") :MIME-ENCODING 'NIL :SOURCE
   '"IBM Latin-2, -3, -5, Extended Presentation Set, GCSGID: 1261"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM NIL :NAME "ISO-10646-J-1" :ALIASES 'NIL
   :MIME-ENCODING 'NIL :SOURCE '"ISO 10646 Japanese, see RFC 1815."
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1003 :NAME "ISO-10646-Unicode-Latin1"
   :ALIASES '("ISO-10646" "csUnicodeLatin1") :MIME-ENCODING 'NIL :SOURCE
   '"ISO Latin-1 subset of Unicode. Basic Latin and Latin-1  Supplement  = collections 1 and 2.  See ISO 10646, Appendix A.  See RFC 1815."
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1002 :NAME "ISO-10646-UCS-Basic"
   :ALIASES '("csUnicodeASCII") :MIME-ENCODING 'NIL :SOURCE
   '"ASCII subset of Unicode.  Basic Latin = collection 1 See ISO 10646, Appendix A"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 1000 :NAME "ISO-10646-UCS-2" :ALIASES
   '("csUnicode") :MIME-ENCODING 'NIL :SOURCE
   '"the 2-octet Basic Multilingual Plane, aka Unicode this needs to specify network byte order: the standard does not specify (it is a 16-bit integer space)"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 119 :NAME "KZ-1048" :ALIASES
   '("csKZ1048" "RK1048" "STRK1048-2002") :MIME-ENCODING 'NIL :SOURCE
   '"See <http://www.iana.org/assignments/charset-reg/KZ-1048>      [Veremeev, Kikkarin]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 118 :NAME "ISO-11548-1" :ALIASES
   '("csISO115481" "ISO_TR_11548-1" "ISO_11548-1") :MIME-ENCODING 'NIL
   :SOURCE
   '"See <http://www.iana.org/assignments/charset-reg/ISO-11548-1>            [Thibault]"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 117 :NAME "OSD_EBCDIC_DF04_1" :ALIASES
   'NIL :MIME-ENCODING 'NIL :SOURCE
   '"Fujitsu-Siemens standard mainframe EBCDIC encoding Please see: <http://www.iana.org/assignments/charset-reg/OSD-EBCDIC-DF04-1>"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 116 :NAME "OSD_EBCDIC_DF03_IRV" :ALIASES
   'NIL :MIME-ENCODING 'NIL :SOURCE
   '"Fujitsu-Siemens standard mainframe EBCDIC encoding Please see: <http://www.iana.org/assignments/charset-reg/OSD-EBCDIC-DF03-IRV>"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 115 :NAME "OSD_EBCDIC_DF04_15" :ALIASES
   'NIL :MIME-ENCODING 'NIL :SOURCE
   '"Fujitsu-Siemens standard mainframe EBCDIC encoding Please see: <http://www.iana.org/assignments/charset-reg/OSD-EBCDIC-DF04-15>"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 103 :NAME "UNICODE-1-1-UTF-7" :ALIASES
   '("csUnicode11UTF7") :MIME-ENCODING 'NIL :SOURCE '"RFC 1642"
   :COMMENTS 'NIL :REFERENCES '("[RFC1642]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 102 :NAME "KSC5636" :ALIASES
   '("csKSC5636" "ISO646-KR") :MIME-ENCODING 'NIL :SOURCE 'NIL :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 101 :NAME "dk-us" :ALIASES '("csDKUS")
   :MIME-ENCODING 'NIL :SOURCE 'NIL :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 100 :NAME "us-dk" :ALIASES '("csUSDK")
   :MIME-ENCODING 'NIL :SOURCE 'NIL :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 99 :NAME "DS_2089" :ALIASES
   '("csISO646Danish" "dk" "ISO646-DK" "DS2089") :MIME-ENCODING 'NIL
   :SOURCE '"Danish Standard, DS 2089, February 1974" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 98 :NAME "JIS_X0212-1990" :ALIASES
   '("csISO159JISX02121990" "iso-ir-159" "x0212") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 97 :NAME "latin-lap" :ALIASES
   '("csISO158Lap" "iso-ir-158" "lap") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 96 :NAME "ISO_10367-box" :ALIASES
   '("csISO10367Box" "iso-ir-155") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 95 :NAME "ISO_8859-supp" :ALIASES
   '("csISO8859Supp" "latin1-2-5" "iso-ir-154") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 94 :NAME "GOST_19768-74" :ALIASES
   '("csISO153GOST1976874" "iso-ir-153" "ST_SEV_358-88") :MIME-ENCODING
   'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 93 :NAME "ISO_6937-2-25" :ALIASES
   '("csISO6937Add" "iso-ir-152") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 92 :NAME "NC_NC00-10:81" :ALIASES
   '("csISO151Cuba" "ISO646-CU" "iso-ir-151" "cuba") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 91 :NAME "greek-ccitt" :ALIASES
   '("csISO150GreekCCITT" "csISO150" "iso-ir-150") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 90 :NAME "JUS_I.B1.003-mac" :ALIASES
   '("csISO147Macedonian" "iso-ir-147" "macedonian") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 89 :NAME "JUS_I.B1.003-serb" :ALIASES
   '("csISO146Serbian" "serbian" "iso-ir-146") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 88 :NAME "IEC_P27-1" :ALIASES
   '("csISO143IECP271" "iso-ir-143") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 87 :NAME "JUS_I.B1.002" :ALIASES
   '("csISO141JUSIB1002" "yu" "js" "ISO646-YU" "iso-ir-141")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 86 :NAME "CSN_369103" :ALIASES
   '("csISO139CSN369103" "iso-ir-139") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 85 :NAME "ISO_8859-8-I" :ALIASES
   '(#21="ISO-8859-8-I" "csISO88598I") :MIME-ENCODING '#21# :SOURCE
   '"RFC1556" :COMMENTS '("Alias: ISO-8859-8-I (preferred MIME name)")
   :REFERENCES '("[RFC1556,Nussbacher]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 84 :NAME "ISO_8859-8-E" :ALIASES
   '(#22="ISO-8859-8-E" "csISO88598E") :MIME-ENCODING '#22# :SOURCE
   '"RFC1556" :COMMENTS '("Alias: ISO-8859-8-E (preferred MIME name)")
   :REFERENCES '("[RFC1556,Nussbacher]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 83 :NAME "T.101-G2" :ALIASES
   '("csISO128T101G2" "iso-ir-128") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 82 :NAME "ISO_8859-6-I" :ALIASES
   '(#23="ISO-8859-6-I" "csISO88596I") :MIME-ENCODING '#23# :SOURCE
   '"RFC1556" :COMMENTS '("Alias: ISO-8859-6-I (preferred MIME name)")
   :REFERENCES '("[RFC1556,IANA]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 81 :NAME "ISO_8859-6-E" :ALIASES
   '(#24="ISO-8859-6-E" "csISO88596E") :MIME-ENCODING '#24# :SOURCE
   '"RFC1556" :COMMENTS '("Alias: ISO-8859-6-E (preferred MIME name)")
   :REFERENCES '("[RFC1556,IANA]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 80 :NAME "CSA_Z243.4-1985-gr" :ALIASES
   '("csISO123CSAZ24341985gr" "iso-ir-123") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 79 :NAME "CSA_Z243.4-1985-2" :ALIASES
   '("csISO122Canadian2" "csa7-2" "ISO646-CA2" "iso-ir-122")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 78 :NAME "CSA_Z243.4-1985-1" :ALIASES
   '("csISO121Canadian1" "ca" "csa7-1" "ISO646-CA" "iso-ir-121")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 77 :NAME "ECMA-cyrillic" :ALIASES
   '("csISO111ECMACyrillic" "KOI8-E" "iso-ir-111") :MIME-ENCODING 'NIL
   :SOURCE
   '"ISO registry (formerly ECMA registry) http://www.itscj.ipsj.jp/ISO-IR/111.pdf"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 76 :NAME "T.61-8bit" :ALIASES
   '("csISO103T618bit" "iso-ir-103" "T.61") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 75 :NAME "T.61-7bit" :ALIASES
   '("csISO102T617bit" "iso-ir-102") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 74 :NAME "ANSI_X3.110-1983" :ALIASES
   '("csISO99NAPLPS" "NAPLPS" "CSA_T500-1983" "iso-ir-99")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 73 :NAME "ISO_2033-1983" :ALIASES
   '("csISO2033" "e13b" "iso-ir-98") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 72 :NAME "JIS_C6229-1984-kana" :ALIASES
   '("csISO96JISC62291984kana" "iso-ir-96") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 71 :NAME "JIS_C6229-1984-hand-add"
   :ALIASES '("csISO95JIS62291984handadd" "jp-ocr-hand-add" "iso-ir-95")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 70 :NAME "JIS_C6229-1984-hand" :ALIASES
   '("csISO94JIS62291984hand" "jp-ocr-hand" "iso-ir-94") :MIME-ENCODING
   'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 69 :NAME "JIS_C6229-1984-b-add" :ALIASES
   '("csISO93JIS62291984badd" "jp-ocr-b-add" "iso-ir-93") :MIME-ENCODING
   'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 68 :NAME "JIS_C6229-1984-b" :ALIASES
   '("csISO92JISC62991984b" "jp-ocr-b" "ISO646-JP-OCR-B" "iso-ir-92")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 67 :NAME "JIS_C6229-1984-a" :ALIASES
   '("csISO91JISC62291984a" "jp-ocr-a" "iso-ir-91") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 66 :NAME "iso-ir-90" :ALIASES
   '("csISO90") :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS
   'NIL :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 65 :NAME "ASMO_449" :ALIASES
   '("csISO89ASMO449" "iso-ir-89" "arabic7" "ISO_9036") :MIME-ENCODING
   'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 64 :NAME "greek7" :ALIASES
   '("csISO88Greek7" "iso-ir-88") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 63 :NAME "JIS_C6226-1983" :ALIASES
   '("csISO87JISX0208" "JIS_X0208-1983" "x0208" "iso-ir-87")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 62 :NAME "MSZ_7795.3" :ALIASES
   '("csISO86Hungarian" "hu" "ISO646-HU" "iso-ir-86") :MIME-ENCODING
   'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 61 :NAME "ES2" :ALIASES
   '("csISO85Spanish2" "ISO646-ES2" "iso-ir-85") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 60 :NAME "PT2" :ALIASES
   '("csISO84Portuguese2" "ISO646-PT2" "iso-ir-84") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 59 :NAME "videotex-suppl" :ALIASES
   '("csISO70VideotexSupp1" "iso-ir-70") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 58 :NAME "NS_4551-2" :ALIASES
   '("csISO61Norwegian2" "no2" "iso-ir-61" "ISO646-NO2") :MIME-ENCODING
   'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 57 :NAME "GB_2312-80" :ALIASES
   '("csISO58GB231280" "chinese" "iso-ir-58") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 56 :NAME "GB_1988-80" :ALIASES
   '("csISO57GB1988" "ISO646-CN" "cn" "iso-ir-57") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 55 :NAME "ISO_5428:1980" :ALIASES
   '("csISO5428Greek" "iso-ir-55") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 54 :NAME "ISO_5427:1981" :ALIASES
   '("ISO5427Cyrillic1981" "iso-ir-54") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 53 :NAME "INIS-cyrillic" :ALIASES
   '("csISO51INISCyrillic" "iso-ir-51") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 52 :NAME "INIS-8" :ALIASES
   '("csISO50INIS8" "iso-ir-50") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 51 :NAME "INIS" :ALIASES
   '("csISO49INIS" "iso-ir-49") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 50 :NAME "BS_viewdata" :ALIASES
   '("csISO47BSViewdata" "iso-ir-47") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 49 :NAME "JIS_C6226-1978" :ALIASES
   '("csISO42JISC62261978" "iso-ir-42") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 48 :NAME "ISO_5427" :ALIASES
   '("csISO5427Cyrillic" "iso-ir-37") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 47 :NAME "Latin-greek-1" :ALIASES
   '("csISO27LatinGreek1" "iso-ir-27") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 46 :NAME "NF_Z_62-010_(1973)" :ALIASES
   '("csISO25French" "ISO646-FR1" "iso-ir-25") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 45 :NAME "latin-greek" :ALIASES
   '("csISO19LatinGreek" "iso-ir-19") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 44 :NAME "greek7-old" :ALIASES
   '("csISO18Greek7Old" "iso-ir-18") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 43 :NAME "PT" :ALIASES
   '("csISO16Portuguese" "ISO646-PT" "iso-ir-16") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 42 :NAME "JIS_C6220-1969-ro" :ALIASES
   '("csISO14JISC6220ro" "ISO646-JP" "jp" "iso-ir-14") :MIME-ENCODING
   'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 41 :NAME "JIS_C6220-1969-jp" :ALIASES
   '("csISO13JISC6220jp" "x0201-7" "katakana" "iso-ir-13"
     "JIS_C6220-1969")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 36 :NAME "KS_C_5601-1987" :ALIASES
   '("csKSC56011987" "korean" "KSC_5601" "KS_C_5601-1989" "iso-ir-149")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 35 :NAME "SEN_850200_B" :ALIASES
   '("csISO10Swedish" "se" "ISO646-SE" "ISO646-FI" "FI" "iso-ir-10")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 34 :NAME "NATS-DANO-ADD" :ALIASES
   '("csNATSDANOADD" "iso-ir-9-2") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 33 :NAME "NATS-DANO" :ALIASES
   '("csNATSDANO" "iso-ir-9-1") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 32 :NAME "NATS-SEFI-ADD" :ALIASES
   '("csNATSSEFIADD" "iso-ir-8-2") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 31 :NAME "NATS-SEFI" :ALIASES
   '("csNATSSEFI" "iso-ir-8-1") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 30 :NAME "ISO_646.irv:1983" :ALIASES
   '("csISO2IntlRefVersion" "irv" "iso-ir-2") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 29 :NAME "INVARIANT" :ALIASES
   '("csINVARIANT") :MIME-ENCODING 'NIL :SOURCE 'NIL :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 28 :NAME "ISO_646.basic:1983" :ALIASES
   '("csISO646basic1983" "ref") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry" :COMMENTS 'NIL :REFERENCES '("[RFC1345,KXS2]")
   :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 27 :NAME "ISO-10646-UTF-1" :ALIASES
   '("csISO10646UTF1") :MIME-ENCODING 'NIL :SOURCE
   '"Universal Transfer Format (1), this is the multibyte encoding, that subsets ASCII-7. It does not have byte ordering issues."
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 26 :NAME "NF_Z_62-010" :ALIASES
   '("csISO69French" "fr" "ISO646-FR" "iso-ir-69") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 25 :NAME "NS_4551-1" :ALIASES
   '("csISO60Norwegian1" "csISO60DanishNorwegian" "no" "ISO646-NO"
     "iso-ir-60")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 24 :NAME "DIN_66003" :ALIASES
   '("csISO21German" "ISO646-DE" "de" "iso-ir-21") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 23 :NAME "ES" :ALIASES
   '("csISO17Spanish" "ISO646-ES" "iso-ir-17") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 22 :NAME "IT" :ALIASES
   '("csISO15Italian" "ISO646-IT" "iso-ir-15") :MIME-ENCODING 'NIL
   :SOURCE '"ECMA registry" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 21 :NAME "SEN_850200_C" :ALIASES
   '("csISO11SwedishForNames" "se2" "ISO646-SE2" "iso-ir-11")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 20 :NAME "BS_4730" :ALIASES
   '("csISO4UnitedKingdom" "uk" "gb" "ISO646-GB" "iso-ir-4")
   :MIME-ENCODING 'NIL :SOURCE '"ECMA registry" :COMMENTS 'NIL
   :REFERENCES '("[RFC1345,KXS2]") :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 19 :NAME
   "Extended_UNIX_Code_Fixed_Width_for_Japanese" :ALIASES
   '("csEUCFixWidJapanese") :MIME-ENCODING 'NIL :SOURCE
   '"Used in Japan.  Each character is 2 octets. code set 0: US-ASCII (a single 7-bit byte set) 1st byte = 00 2nd byte = 20-7E code set 1: JIS X0208-1990 (a double 7-bit byte set) restricted  to A0-FF in both bytes code set 2: Half Width Katakana (a single 7-bit byte set) 1st byte = 00 2nd byte = A0-FF code set 3: JIS X0212-1990 (a double 7-bit byte set) restricted to A0-FF in the first byte and 21-7E in the second byte"
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 16 :NAME "JIS_Encoding" :ALIASES
   '("csJISEncoding") :MIME-ENCODING 'NIL :SOURCE
   '"JIS X 0202-1991.  Uses ISO 2022 escape sequences to shift code sets as documented in JIS X 0202-1991."
   :COMMENTS 'NIL :REFERENCES 'NIL :RANGES NIL)
  (MAKE-CHARACTER-SET :MIB-ENUM 14 :NAME "ISO_6937-2-add" :ALIASES
   '("csISOTextComm" "iso-ir-142") :MIME-ENCODING 'NIL :SOURCE
   '"ECMA registry and ISO 6937-2:1983" :COMMENTS 'NIL :REFERENCES
   '("[RFC1345,KXS2]") :RANGES NIL)))


(fill-character-set-emacs-encoding)
(fill-character-set-lisp-encoding)

;;;; THE END ;;;;

