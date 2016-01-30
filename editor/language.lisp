;;; -- not used --
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(in-package "COMMON-LISP-USER")

;; while debugging:
#-(and)
(when (find-package "COM.INFORMATIMAGO.EDITOR")
  (delete-package "COM.INFORMATIMAGO.EDITOR"))

;;;---------------------------------------------------------------------
;;;
;;; We put on *FEATURES* a keyword representing the language to use for
;;; documentation strings:
;;;

(defvar *languages* '((:DK . :DANSK)
                      (:DE . :DEUTSCH)
                      (:EN . :ENGLISH)
                      (:ES . :ESPAÑOL)
                      (:FR . :FRANÇAIS)
                      (:NL . :NEDERLANDS)
                      (:RU . :РУССКИЙ))
  "Maps the language code (in keyword) as used in the LANG environment variable,
to language names (as keyword).")

;; Remove the old languages, if any.
(setf *features* (set-difference *features* (mapcar (function cdr) *languages*)))

;; Push the new language.  By default we use :ENGLISH.
(pushnew (progn
           ;; In clisp, we use the custom:*current-language* variable:
           #+clisp (intern (string custom:*current-language*) "KEYWORD")
           ;; Otherwise if we have ASDF, we try to get the environment variable LANG:
           #+(and (not clisp) asdf)
           (let* ((lang (getenv "LANG"))
                  (entry (assoc lang *languages* :test (function string-equal))))
             (if entry
                 (cdr entry)
                 :english))
           ;; otherwise we use English:
           #-(or clisp asdf) :english)
         *features*)

;;; In any case, if we don't have the documentation in the selected
;;; language, we fall back to docstrings in English.
;;;
;;;---------------------------------------------------------------------


