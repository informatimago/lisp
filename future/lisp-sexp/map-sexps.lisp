;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               map-sexps.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    MAP-SEXPS can be used to filter lisp sources.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-06 <PJB> Translated from emacs lisp.
;;;;    2003-01-20 <PJB> Added walk-sexps, map-sexps, replace-sexps;
;;;;                     reimplemented get-sexps with walk-sexps.
;;;;    199?-??-?? <PJB> pjb-sources.el creation.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.MAP"
  (:use "COMMON-LISP")
  (:export
   "MAP-SEXPS"
   "WALK-SEXPS")
  (:documentation "
    This package exports functions to apply a function to all the
    sexps in a lisp source file.


    Copyright Pascal J. Bourguignon 2003 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SEXP-SOURCE")



;; ------------------------------------------------------------------------
;; map-sexps
;; ------------------------------------------------------------------------
;; Applies a function on all s-exps from a lisp source file.
;;

(defun skip-comments ()
  "
DO:     Move the point over spaces and lisp comments ( ;...\n or #| ... |# ),
        in the current buffer.
RETURN: (not eof)
"
  (interactive)
  (let* ((data (header-comment-description-for-mode major-mode))
         (comment-regexp (hcd-comment-regexp data))
         (space-or-comment (format "\\(%s\\)\\|\\(%s\\)"
                             "[ \t\n\v\f\r]+"
                             comment-regexp)) )
    (unless data
      (error "Don't know how to handle this major mode %S." major-mode))
    (while (looking-at space-or-comment)
      (goto-char (match-end 0)))
    (< (point) (point-max))))


(defparameter *source-readtable*
  (when (fboundp 'copy-readtable)
    (let ((rt (copy-readtable nil)))
      (set-dispatch-macro-character (cl-char ?#) (cl-char ?+)
                                    (lambda (stream subchar arg)
                                      `('\#+ ,(read stream nil nil t)))
                                    rt)
      (set-dispatch-macro-character (cl-char ?#) (cl-char ?-)
                                    (lambda (stream subchar arg)
                                      `('\#- ,(read stream nil nil t)))
                                    rt)
      rt)))



(defvar *map-sexps-top-level* nil "Private")
(defvar *map-sexps-deeply*    nil "Private")
(defvar *map-sexps-atoms*     nil "Private")
(defvar *map-sexps-function*  nil "Private")


(defvar *walk-sexps-end-marker* nil)

(defun walk-sexps (fun)
  "
DO:     Recursively scan sexps from (point) in current buffer up to
        the end-of-file or until scan-sexps raises a scan-error.
        Call fun on each sexps and each of their children etc.
fun:    A function (sexp start end)
        sexp:    The sexp parsed from a source file.
        start:   The point starting the sexp.
        end:     The point ending the sexp.
NOTE:   All positions are kept in markers, so modifying the buffer between
        start and end should be OK.
        However  ' or ` are passed as (quote ...) or (backquote ...)
        to the function fun without reparsing the sexp inside them.
        Ie. if you modify such a source, (which can be detected looking at
        the character at start position),  you still get the original sexp.
"
  (let ((quote-stack '())
        (start-stack '())
        (*walk-sexps-end-marker* (make-marker))
        quote-depth
        start-m sexp)
    (skip-comments)
    (when (/= (point) (point-max))
      (when (member major-mode *lisp-modes*)
        ;; gather the quotes:
        (while (looking-at "['`] *")
          ;; quote or backquote
          ;; NOT NEEDED ANYMORE WITH GNU Emacs 21.
          ;; --- (push (set-marker (make-marker) (point)) start-stack)
          ;; --- (push (if (= (char-after) ?') 'quote 'backquote) quote-stack)
          (forward-char 1)
          (skip-comments)))
      ;; get the sexp:
      (setq start-m (set-marker (make-marker) (point)))
      (forward-sexp 1)
      (set-marker *walk-sexps-end-marker* (point))
      ;; (forward-sexp -1)
      ;; (assert (= (marker-position start-m) (point)) t)
      (goto-char (marker-position start-m))
      (setq sexp (cl-sexp-at-point))
      ;; push the quotes on the sexp:
      (setq quote-depth (length quote-stack))
      (while quote-stack
        (setq sexp (cons (pop quote-stack) (list sexp))))
      ;; process the quotes:
      (setq start-stack (nreverse start-stack))
      (dotimes (i quote-depth)
        (message "sexp = %S\nstart = %S\nend = %S\n" sexp (marker-position (car start-stack)) *walk-sexps-end-marker*)
        (funcall fun sexp
                 (marker-position (car start-stack)) *walk-sexps-end-marker*)
        (set-marker (pop start-stack) nil)
        (setq sexp (cadr sexp)))
      ;; process the sexp:
      (message "sexp = %S\nstart = %S\nend = %S\n" sexp  (marker-position start-m) *walk-sexps-end-marker*)
      (funcall fun sexp (marker-position start-m)  *walk-sexps-end-marker*)
      (when *map-sexps-deeply*
        (when (= (char-syntax (char-after (marker-position start-m))) 40) ;; "("
          ;; then the subsexps:
          (goto-char (marker-position start-m))
          (down-list 1)
          (loop
             (condition-case nil
                 (walk-sexps fun)
               (scan-error (return-from nil))))
          (up-list 1)))
      ;; then go to the next sexp:
      (goto-char (marker-position *walk-sexps-end-marker*))
      (set-marker start-m nil)
      (set-marker *walk-sexps-end-marker* nil)))
  nil)



(defun map-sexps-filter (sexp start end)
  (when (and (or *map-sexps-top-level* *map-sexps-deeply*)
             (or *map-sexps-atoms* (not (atom sexp))))
    (funcall *map-sexps-function* sexp start end))
  (setq *map-sexps-top-level* nil))

(defun new-map-sexps (source-file fun &rest cl-keys)
   "
DO:     Scan all sexps in the source file.
        (skipping spaces and comment between top-level sexps).
        If the deeply flag is set,
        then subsexps are also passed to the function fun, after the sexp,
        else only the top-level sexps are
        If the atoms flags is set
        then atoms are also considered (and passed to the selector).
fun:    A function (sexp start end)
        sexp:    The sexp parsed from a source file.
        start:   The point starting the sexp.
        end:     The point ending the sexp.
KEYS:   :deeply   (boolean,  default nil)
        :atoms    (boolean,  default nil)
NOTE:   Scanning stops as soon as an error is detected by forward-sexp.
RETURN: The list of results from fun.
"
  (cl-parsing-keywords ((:deeply   t)
                        (:atoms    nil)) nil


    ))

(defun new-map-sexps (source-file fun &rest cl-keys)
   "
DO:     Scan all sexps in the source file.
        (skipping spaces and comment between top-level sexps).
        If the deeply flag is set,
        then subsexps are also passed to the function fun, after the sexp,
        else only the top-level sexps are
        If the atoms flags is set
        then atoms are also considered (and passed to the selector).
fun:    A function (sexp start end)
        sexp:    The sexp parsed from a source file.
        start:   The point starting the sexp.
        end:     The point ending the sexp.
KEYS:   :deeply   (boolean,  default nil)
        :atoms    (boolean,  default nil)
NOTE:   Scanning stops as soon as an error is detected by forward-sexp.
RETURN: The list of results from fun.
"
  (cl-parsing-keywords ((:deeply   t)
                        (:atoms    nil)) nil

    `(source-text:map-source-file ,fun ,source-file
                                 :deeply ,cl-deeply
                                 :atoms ,cl-atoms)

    ))

(defun map-sexps (source-file fun &rest cl-keys)
  "
DO:     Scan all sexps in the source file.
        (skipping spaces and comment between top-level sexps).
        If the deeply flag is set,
        then subsexps are also passed to the function fun, after the sexp,
        else only the top-level sexps are
        If the atoms flags is set
        then atoms are also considered (and passed to the selector).
fun:    A function (sexp start end)
        sexp:    The sexp parsed from a source file.
        start:   The point starting the sexp.
        end:     The point ending the sexp.
KEYS:   :deeply   (boolean,  default nil)
        :atoms    (boolean,  default nil)
NOTE:   Scanning stops as soon as an error is detected by forward-sexp.
RETURN: The list of results from fun.
"
  (cl-parsing-keywords ((:deeply   nil)
                        (:atoms    nil)) ()
    (message "map-sexps deeply %S  atoms %S" cl-deeply cl-atoms)
    (save-excursion
      (save-restriction
        (let ((old-buffer            (current-buffer))
              (existing-buffer       (buffer-named source-file))
              (*map-sexps-deeply*    cl-deeply)
              (*map-sexps-atoms*     cl-atoms)
              (*map-sexps-top-level* t)
              (*map-sexps-function*  fun)
              last-bosexp)
          (if existing-buffer
              (switch-to-buffer existing-buffer)
              (find-file source-file))
          (widen)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (setq *map-sexps-top-level* t)
            (walk-sexps (function map-sexps-filter)))
          (if existing-buffer
              (switch-to-buffer old-buffer)
              (kill-buffer (current-buffer))))))))


(defun old-old-map-sexps (source-file fun)
  "
DO:     Scan all top-level sexps in the source file.
        (skipping spaces and comment between top-level sexps).
fun:    A function (sexp start end)
        sexp:    The sexp parsed from a source file.
        start:   The point starting the sexp.
        end:     The point ending the sexp.
:deeply
NOTE:   Scanning stops as soon as an error is detected by forward-sexp.
RETURN: The list of results from fun.
"
  (save-excursion
    (save-restriction
      (let ((old-buffer (current-buffer))
            (existing-buffer (buffer-named source-file))
            last-bosexp)
        (if existing-buffer
            (switch-to-buffer existing-buffer)
            (find-file source-file))
        (widen)
        (goto-char (point-max))
        (forward-sexp -1)
        (setq last-bosexp (point))
        (goto-char (point-min))
        (prog1
            (loop with eof  = (gensym)
               while (<= (point) last-bosexp)
               for end   = (progn (forward-sexp 1)  (point))
               for start = (progn (forward-sexp -1) (point))
               for sexp  = (condition-case nil (sexp-at-point) (error eof))
               until (eq eof sexp)
               collect (funcall fun sexp start end) into map-sexps-result
               do (condition-case nil
                      (forward-sexp 1)
                    (error               (goto-char (point-max)))
                    (wrong-type-argument (goto-char (point-max))))
               finally (unless existing-buffer (kill-buffer source-file))
               finally return (nreverse map-sexps-result))
          (switch-to-buffer old-buffer))))))


(defun count-sexps ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((place (point))
          (count 0))
      (forward-sexp)
      (while (< place (point))
        (incf count)
        (setq place (point))
        (forward-sexp))
      (message "There are %d top-level sexps." count)
      count))) ;;count-sexps

;; ------------------------------------------------------------------------
;; get-sexps
;; ------------------------------------------------------------------------
;; Read all s-exps from a lisp source file. Can filter s-exps by a given
;; selector function.
;;

(defun get-sexps (source-file &rest cl-keys)
  "
KEYS:    :selector (function: sexp --> boolean, default: (lambda (s) t))
         :deeply   (boolean,  default nil)
         :atoms    (boolean,  default nil)
DO:      Scan all sexp in the source-file.
         A selector function may indicate which sexp must be collected.
         If the deeply flag is set,
         then if a sexp is not selected then sub-sexp are scanned and tested.
         If the atoms flags is set
         then atoms are also considered (and passed to the selector).
NOTE:    Scanning stops as soon as an error is detected by forward-sexp.
RETURN:  A list of selected sexp.
"
  (save-excursion
    (cl-parsing-keywords ((:selector (function (lambda (s) t)))
                          (:deeply   nil)
                          (:atoms    nil)) nil
      (let ((get-sexps-result '()))
        (map-sexps
         source-file
         (lambda (sexp start end)
           (when (funcall cl-selector sexp)
             (push sexp get-sexps-result)))
         :deeply cl-deeply :atoms cl-atoms)
        (nreverse get-sexps-result)))))


;;; (show
;;;  (sort
;;;   (let ((histo (make-hash-table)) (max-lisp-eval-depth 1000))
;;;     (mapc (lambda (path)
;;;             (message path)
;;;             (mapcar (lambda (sexp) (incf (gethash (depth sexp) histo 0)))
;;;                     (get-sexps path)))
;;;           (directory "~/src/common/lisp/emacs/[a-z]*.el"))
;;;     (let ((result '()))
;;;       (maphash (lambda (deep value) (push (cons deep value) result)) histo)
;;;       result))
;;;   (lambda (a b) (< (car a) (car b))))
;;;  )
;;;
;;; ==> ((1 . 325) (2 . 329) (3 . 231) (4 . 163) (5 . 138) (6 . 158) (7 .
;;; 102) (8 . 94) (9 . 63) (10 . 40) (11 . 16) (12 . 20) (13 . 9) (14 . 4)
;;; (15 . 5) (16 . 4) (17 . 2) (19 . 2) (23 . 1))



;; (defun old-get-sexps (source-file &rest cl-keys)
;;   "
;; KEYS:    :selector (a function, default: true)
;;          :deeply   (a boolean,  default nil)
;;          :atoms    (a boolean,  default nil)
;; DO:      Scan all sexp in the source-file.
;;          A selector function (sexp->bool) may indicate which sexp must
;;          be collected.  If the deeply flag is set, then if a sexp is not
;;          selected then sub-sexp are scanned and tested.  If the atoms flags
;;          is set then atoms are also considered (and passed to the selector).
;; NOTE:    Scanning stops as soon as an error is detected by forward-sexp.
;; RETURN:  A list of selected sexp.
;; "
;;   (cl-parsing-keywords ((:selector (function identity))
;;                         (:deeply   nil)
;;                         (:atoms    nil)) nil
;;     (save-excursion
;;       (save-restriction
;;         (let ((existing-buffer (buffer-named source-file)))
;;           (if existing-buffer
;;               (set-buffer existing-buffer)
;;               (find-file source-file))
;;           (widen)
;;           (goto-char (point-min))
;;           (loop with result = nil
;;              while (/= (point) (point-max))
;;              for sexp = (condition-case nil (sexp-at-point) (error nil))
;;              do (flet ((deeply-select
;;                            (sexp)
;;                          (if (atom sexp)
;;                              (if (and cl-atoms (funcall cl-selector sexp))
;;                                  (push sexp result))
;;                              (let (subsexp)
;;                                (while sexp
;;                                  (if (consp sexp)
;;                                      (setq subsexp (car sexp)
;;                                            sexp    (cdr sexp))
;;                                      (setq subsexp sexp
;;                                            sexp    nil))
;;                                  (cond
;;                                    ((atom subsexp)
;;                                     (if (and cl-atoms
;;                                              (funcall cl-selector subsexp))
;;                                         (push subsexp result)))
;;                                    ((funcall cl-selector subsexp)
;;                                     (push subsexp result))
;;                                    (cl-deeply
;;                                     (deeply-select subsexp))))))))
;;                   (if (atom sexp)
;;                       (if (and cl-atoms (funcall cl-selector sexp))
;;                           (push sexp result))
;;                       (cond
;;                         ((funcall cl-selector sexp)
;;                          (push sexp result))
;;                         (cl-deeply
;;                          (deeply-select sexp)))))
;;              (condition-case nil
;;                  (forward-sexp 1)
;;                (error (goto-char (point-max)))
;;                (wrong-type-argument (goto-char (point-max))))
;;              finally (unless existing-buffer (kill-buffer source-file))
;;              finally return (nreverse result))
;;           ))))
;;   ) ;;old-get-sexps



;; ------------------------------------------------------------------------
;; replace-sexps
;; ------------------------------------------------------------------------
;; Applies a transformer function to all s-exps from a lisp source file,
;; replacing them by the result of this transformer function in the source file.
;;

;;; TODO: Use CLISP to pretty print, or find an elisp pretty printer.
;;; "(LET ((*PRINT-READABLY* T))
;;;    (SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
;;;    (WRITE (QUOTE ~S )))"


(defun replace-sexps (source-file transformer &rest cl-keys)
  "
DO:             Scan all sexp in the source-file.
                Each sexps is given to the transformer function whose result
                replaces the original sexps in the source-file.
                If the deeply flag is set, then the transformer is applied
                recursively to the sub-sexps.
                If the atoms flags is set then atoms are also considered
                (and passed to the transformer).
KEYS:           :deeply    (a boolean,  default nil)
                :atoms     (a boolean,  default nil)
transformer:    A function sexp --> sexp.
                If returing its argument (eq),
                then no replacement takes place (the comments and formating
                is then preserved.  Otherwise the source of the sexp is
                replaced by the returned sexp.
NOTE:           For now, no pretty-printing is done.
"
  (cl-parsing-keywords ((:deeply   nil)
                        (:atoms    nil)) nil
    (map-sexps
     source-file
     (lambda (sexp start end)
       (let ((replacement (funcall transformer sexp)))
         (unless (eq replacement sexp)
           (delete-region start end)
           (insert (let ((print-escape-newlines t)
                         (print-level nil)
                         (print-circle nil)
                         (print-length nil)) (format "%S" replacement)))
           (set-marker end (point)))))
     :deeply cl-deeply :atoms cl-atoms))
  nil)



;; ------------------------------------------------------------------------
;; clean-if*
;; ------------------------------------------------------------------------
;; Replace if* by if, when, unless or cond.
;;

(defun escape-sharp ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while
        (re-search-forward "\\(#\\([^A-Za-z0-9()\\\\ ]\\|\\\\.\\)*\\)" nil t)
      (let* ((match (match-string 1))
             (escap (base64-encode-string match t)))
        (replace-match (format "|ESCAPED-SHARP:%s|" escap) t t)))))


;;; (let ((s "toto #.\\( titi"))
;;; (string-match  "\\(#\\(\\\\.\\|[^A-Za-z0-9()\\\\ ]\\)*\\)" s)
;;; (match-string 1 s))



(defun unescape-sharp ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\(|ESCAPED-SHARP:\\([A-Za-z0-9+/=*]*\\)|\\)" nil t)
      (let* ((escap (match-string 2))
             (match (base64-decode-string escap)))
        (replace-match match t t nil 1)))))


(defun clean-if* ()
  (escape-sharp)
  (unwind-protect
       (replace-sexps
        (buffer-file-name)
        (lambda (sexp)
          (message "sexp=%S" sexp )
          (let ((backquoted (eql '\` (car sexp)))
                (original-sexp sexp))
            (when backquoted (setq sexp (second sexp)))
            (if (and (consp sexp) (symbolp (car sexp))
                     (string-equal 'if* (car sexp)))
                (do* ((subs (cons 'elseif (cdr sexp)))
                      (clauses '())
                      (condition)
                      (statements)
                      (token))
                     ((null subs)
                      (let ((result
                             (progn ;;generate the new sexp
                               (setq clauses (nreverse clauses))
                               (cond
                                 ((and (= 1 (length clauses))
                                       (every
                                        (lambda (clause) (not (null (cdr clause))))
                                        ;; clause = (cons condition statements)
                                        clauses)) ;; a when
                                  `(when ,(car (first clauses))
                                     ,@(cdr (first clauses))))
                                 ((or (= 1 (length clauses))
                                      (< 2 (length clauses))
                                      (not (eq t (car (second clauses))))) ;; a cond
                                  `(cond ,@clauses))
                                 (t ;; a if
                                  `(if ,(car (first clauses))
                                       ,(if (= 1 (length (cdr (first clauses))))
                                            (cadr (first clauses))
                                            `(progn ,@(cdr (first clauses))))
                                       ,(if (= 1 (length (cdr (second clauses))))
                                            (cadr (second clauses))
                                            `(progn ,@(cdr (second clauses)))))))) ))
                        (message "sexp=%S\nresult=%S" sexp result)
                        (if backquoted (list '\` result) result)))
                  ;; read the condition:
                  (setq token (pop subs))
                  (cond
                    ((not (symbolp token))
                     (error "unexpected token %S in %S" token sexp))
                    ((null subs)
                     (error "unexpected end of sexp in %S" sexp))
                    ((string-equal token 'elseif)
                     (setq condition (pop subs))
                     (unless (or (string-equal (car subs) 'then)
                                 (string-equal (car subs) 'thenret))
                       (error "missing THEN after condition in %S" sexp))
                     (pop subs))
                    ((string-equal token 'else)
                     (setq condition t))
                    (t
                     (error "unexpected token %S in %S" token sexp)))
                  ;; read the statements:
                  (do () ((or (null subs)
                              (and (consp subs) (symbolp (car subs))
                                   (member* (car subs) '(elseif else)
                                             :test (function string-equal)))))
                    (push (pop subs) statements))
                  (push (cons condition (nreverse statements)) clauses)
                  (setq condition nil statements nil))
                original-sexp)))
        :deeply t :atoms nil)
    (unescape-sharp)))



;;;; THE END ;;;;
