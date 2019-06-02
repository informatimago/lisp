;;;; .sexph and .sexpc


(dolist (sym '(do))
  (cl-indent sym 0))

(dolist (sym '(declare-structure declare-union declare-type
               declare-enumeration declare-constant declare-variable declare-function
               define-constant define-variable define-macro
               .ifndef .ifdef .if
               \#ifndef \#ifdef \#if

               block))
  (cl-indent sym 1))




(cl-indent 'define-function 4)

(dolist (sym '(declare-structure declare-union declare-enumeration declare-type))
  (setf (get sym 'lisp-define-type) 'type))

(dolist (sym '(declare-constant declare-variable define-constant define-variable))
  (setf (get sym 'lisp-define-type) 'var))


;; (re-search-forward "(\\([ \t]*\\|\\(\\s_\\|\\w\\)+:\\)?\\(declare-\\(structure\\|union\\|enumeration\\|constant\\|variable\\|function\\)\\|define-\\(constant\\|variable\\|function\\|macro\\)\\)\\_>[ \t\n]+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\|\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?")

(font-lock-add-keywords
 'lisp-mode
 '(("(\\([ \t]*\\|\\(\\s_\\|\\w\\)+:\\)?\\(declare-\\(structure\\|union\\|enumeration\\|type\\|constant\\|variable\\|function\\)\\|define-\\(constant\\|variable\\|function\\|macro\\)\\)\\_>[ \t\n]+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\|\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?"
    (1 font-lock-keyword-face)
    (3 (let ((type (get (intern-soft (match-string 1)) (quote lisp-define-type))))
         (cond ((eq type (quote var))  font-lock-variable-name-face)
               ((eq type (quote type)) font-lock-type-face)
               ((or (not (match-string 2))
                    (and (match-string 2) (match-string 4)))
                font-lock-function-name-face)
               (t t)))))))


;; (setf font-lock-keywords
;;       '(t (("\\<[Rr][Kk]:\\sw\\sw+\\>" 0 font-lock-builtin-face)
;;            ("(\\(\\<[-A-Za-z0-9]+-define-[-A-Za-z0-9]+\\>\\)" 1 font-lock-keyword)
;;            ("(\\([ 	]*\\|\\(\\s_\\|\\w\\)+:\\)?\\(declare-\\(structure\\|union\\|enumeration\\|type\\|constant\\|variable\\|function\\)\\|define-\\(constant\\|variable\\|function\\|macro\\)\\)\\_>[
;; ]+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\|\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?"
;;             (1 font-lock-keyword-face)
;;             (3 (let ((type (get (intern-soft (match-string 1))
;;                                 (quote lisp-define-type))))
;;                  (match-string 1)
;;                  (intern-soft (match-string 1)))
;;                (cond ((eq type (quote var)) font-lock-variable-name-face)
;;                      ((eq type (quote type)) font-lock-type-face)
;;                      ((or (not (match-string 2))
;;                           (and (match-string 2)
;;                                (match-string 4))) font-lock-function-name-face)
;;                      (t t))))
;;            (slime-search-suppressed-forms 0 (quote slime-reader-conditional-face) t)
;;            ("(\\(\\(\\s_\\|\\w\\)*:\\(define-\\|do-\\|with-\\|without-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
;;            ("(\\(\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)
;;            ("(\\(check-\\(\\s_\\|\\w\\)*\\)" 1 font-lock-warning-face)
;;            ("(\\(assert-\\(\\s_\\|\\w\\)*\\)" 1 font-lock-warning-face)
;;            (mm/match-labels
;;             (1 font-lock-keyword-face nil)
;;             (2 font-lock-function-name-face nil t)
;;             (3 font-lock-function-name-face nil t)
;;             (4 font-lock-function-name-face nil t)
;;             (5 font-lock-function-name-face nil t)
;;             (6 font-lock-function-name-face nil t)
;;             (7 font-lock-function-name-face nil t)
;;             (8 font-lock-function-name-face nil t))
;;            ("(\\(def\\(?:c\\(?:lass\\|onstant\\)\\|generic\\|ine-\\(?:co\\(?:mpiler-macro\\|ndition\\)\\|m\\(?:ethod-combination\\|odify-macro\\)\\|s\\(?:etf-expander\\|ymbol-macro\\)\\)\\|m\\(?:acro\\|ethod\\)\\|pa\\(?:ckage\\|rameter\\)\\|s\\(?:etf\\|\\(?:truc\\|ubs\\)t\\)\\|type\\|un\\|var\\)\\)\\_>[ 	']*\\(([ 	']*\\)?\\(\\(setf\\)[ 	]+\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\|\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?"
;;             (1 font-lock-keyword-face)
;;             (3 (let ((type (get (intern-soft (match-string 1))
;;                                 (quote lisp-define-type))))
;;                  (cond ((eq type (quote var)) font-lock-variable-name-face)
;;                        ((eq type (quote type)) font-lock-type-face)
;;                        ((or (not (match-string 2))
;;                             (and (match-string 2)
;;                                  (match-string 4))) font-lock-function-name-face))) nil t))
;;            ("\\[\\(\\^\\)" 1 font-lock-negation-char-face prepend)
;;            ("(\\(b\\(?:\\(?:loc\\|rea\\)k\\)\\|c\\(?:ase\\|case\\|o\\(?:mpiler-let\\|nd\\(?:ition-case\\)?\\)\\|typecase\\)\\|d\\(?:e\\(?:cla\\(?:im\\|re\\)\\|structuring-bind\\)\\|o\\(?:\\*\\|list\\|times\\)?\\)\\|e\\(?:case\\|typecase\\|val-when\\)\\|flet\\*?\\|go\\|handler-\\(?:bind\\|case\\)\\|i\\(?:f\\|gnore-errors\\|n-package\\)\\|l\\(?:a\\(?:bels\\|mbda\\)\\|et[*f]?\\|o\\(?:cally\\|op\\)\\)\\|m\\(?:acrolet\\|ultiple-value-\\(?:bind\\|prog1\\)\\)\\|pro\\(?:claim\\|g[*12nv]?\\)\\|re\\(?:start-\\(?:bind\\|case\\)\\|turn\\(?:-from\\)?\\)\\|symbol-macrolet\\|t\\(?:agbody\\|\\(?:h\\|ypecas\\)e\\)\\|un\\(?:less\\|wind-protect\\)\\|w\\(?:h\\(?:en\\|ile\\)\\|ith-\\(?:accessors\\|co\\(?:mpilation-unit\\|ndition-restarts\\)\\|hash-table-iterator\\|input-from-string\\|o\\(?:pen-\\(?:file\\|stream\\)\\|utput-to-string\\)\\|package-iterator\\|s\\(?:imple-restart\\|lots\\|tandard-io-syntax\\)\\)\\)\\)\\_>" . 1)
;;            ("(\\(catch\\|throw\\|provide\\|require\\)\\_>[ 	']*\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?"
;;             (1 font-lock-keyword-face)
;;             (2 font-lock-constant-face nil t))
;;            ("(\\(a\\(?:\\(?:bo\\|sse\\)rt\\)\\|c\\(?:error\\|heck-type\\)\\|error\\|signal\\|warn\\)\\_>"
;;             (1 font-lock-warning-face))
;;            ("[`‘]\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)['’]"
;;             (1 font-lock-constant-face prepend))
;;            ("\\_<:\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\_>"
;;             (0 font-lock-builtin-face))
;;            ("\\_<\\&\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\_>" . font-lock-type-face)
;;            (lisp--match-hidden-arg
;;             (0 (quote (face font-lock-warning-face help-echo "Hidden behind deeper element; move to another line?"))))
;;            ("\\<[Rr][Kk]:\\sw\\sw+\\>"
;;             (0 font-lock-builtin-face))
;;            ("(\\(\\<[-A-Za-z0-9]+-define-[-A-Za-z0-9]+\\>\\)"
;;             (1 font-lock-keyword)))))

;;;; THE END ;;;;
