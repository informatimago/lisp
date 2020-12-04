;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-unicode.lisp
;;;;LANGUAGE:           Common Lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements functions to offset unicode strings to different planes.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2020-12-04 <PJB> Converted to Common Lisp from emeacs lisp.
;;;;    2017-01-25 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2017 - 2020
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.CLMISC.UNICODE"
  (:nicknames "FMT-UNICODE")
  (:use "COMMON-LISP")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SYMBOL"
                "SCAT")

  ;; formatter functions:
  (:export "PARENTHESIZED-LATIN" "CIRCLED-LATIN" "FULLWIDTH-LATIN"
           "MATHEMATICAL-BOLD" "MATHEMATICAL-BOLD-ITALIC"
           "MATHEMATICAL-SCRIPT" "MATHEMATICAL-SCRIPT-BOLD"
           "MATHEMATICAL-DOUBLE-STRUCK" "MATHEMATICAL-FRAKTUR"
           "MATHEMATICAL-FRAKTUR-BOLD" "MATHEMATICAL-SANS-SERIF"
           "MATHEMATICAL-SANS-SERIF-BOLD"
           "MATHEMATICAL-SANS-SERIF-ITALIC"
           "MATHEMATICAL-SANS-SERIF-BOLD-ITALIC"
           "MATHEMATICAL-MONOSPACE")

  (:export "UNICODE-OFFSET"
           "MAP-NAMES" "MAP-NAMED"
           "MAP-NAME" "MAP-OFFSET-FUNCTION" "MAP-RANGES"
           "DEFINE-UNICODE-OFFSET-MAP"))
(in-package "COM.INFORMATIMAGO.CLMISC.UNICODE")

(defparameter *maps* (make-hash-table))

(defun map-names ()
  (let ((names '()))
    (maphash (lambda (name map)
               (declare (ignore map))
               (push name names))
             *maps*)
    names))

(defun map-named (name)
  (gethash name *maps*))


(defstruct (map (:type list))
  offset-function
  name
  ranges)


(defun format-string (stream string colon at parameters)
  ;; justification:
  ;; left
  ;; right        :
  ;; centered     @
  ;; parameters:  width, padding-character
  ;; if no width or length>=width, then no justification.
  (let ((width  (pop parameters)))
    (if width
        (let ((length (length string)))
          (if (< length width)
              (let* ((padding-character (or (pop parameters) #\space))
                     (padding-string (make-string (- width length)
                                                  :initial-element padding-character)))
                (cond (colon ; right
                       (write-string padding-string stream)
                       (write-string string         stream))
                      (at ; centered
                       (let ((middle (truncate (length padding-string) 2)))
                         (write-string (subseq padding-string 0 middle) stream)
                         (write-string string stream)
                         (write-string (subseq padding-string middle) stream)))
                      (t ; left
                       (write-string string         stream)
                       (write-string padding-string stream))))
              (write-string string stream)))
        (write-string string stream))))

(defun generate-formatter-function (name)
  `(defun ,name (stream arg colon at &rest parameters)
     (format-string stream
                    (unicode-offset (function ,(scat 'offset- name))
                                    (typecase arg
                                      (string arg)
                                      (t (prin1-to-string arg))))
                    colon at parameters)))


(defun offset-clauses-to-case-clauses (offset-clauses)
  (let ((case-clauses '())
        (uppercase-clause nil))
    (labels ((add-clauses (min max start)
               (loop
                 :for ch-code :from (char-code min) :to (char-code max)
                 :for offset-code :from start
                 :do (add-clause (list (list (code-char ch-code))
                                       (code-char offset-code)))))
             (add-clause (clause)
               (let ((clause-pos (position (first (first clause)) case-clauses
                                           :key (lambda (x) (first (first x))))))
                 (setf case-clauses
                       (if clause-pos
                           (subst clause (nth clause-pos case-clauses) case-clauses)
                           (nconc case-clauses (list clause)))))))
      #+(and nil testing-add-clause) (progn
                                       (add-clauses #\a #\z 65)
                                       (print case-clauses)
                                       (add-clause (list (list #\b)  #\3))
                                       (add-clause (list (list #\3)  #\z))
                                       (print case-clauses))
      (dolist (clause offset-clauses case-clauses)
        (case (length clause)
          ((2)
           (if (eq :lower (first clause))
               (if uppercase-clause
                   (add-clauses #\a #\z (+ (second clause)
                                           (third uppercase-clause)))
                   (error ":LOWER clause without an uppercase #\\A #\\Z clause."))
               (add-clause (list (list (character (first clause)))
                                 (code-char (second clause))))))
          ((3)
           (add-clauses (first clause) (second clause) (third clause))
           (when (and (char= #\A (first clause))
                      (char= #\Z (second clause)))
             (setf uppercase-clause clause)))
          (otherwise
           (error "Invalid offset-clause: ~S" clause)))))))

(defun offset-clauses-to-ranges (offset-clauses)
  (let ((ranges '())
        (uppercase-clause nil))
    (labels ((add-clauses (min max start)
               (declare (ignore start))
               (push (list min max) ranges))
             (add-clause (clause)
               (let ((clause-pos (position (first (first clause)) ranges
                                           :test (lambda (char range)
                                                   (and (char<= char (first range))
                                                        (char<= (second range) char))))))
                 (unless clause-pos
                   (push (list (first (first clause))
                               (first (first clause)))
                         ranges)))))
      (dolist (clause offset-clauses (sort ranges (lambda (r1 r2)
                                                    (char< (first r1)
                                                           (first r2)))))
        (case (length clause)
          ((2)
           (if (eq :lower (first clause))
               (if uppercase-clause
                   (add-clauses #\a #\z 0))
               (add-clause (list (list (character (first clause))) #\a))))
          ((3) (add-clauses (character (first clause))
                            (character (second clause))
                            (third clause)))
          (otherwise
           (error "Invalid offset-clause: ~S" clause)))))))

(defun generate-offset-function (name offset-clauses)
  (let ((fname (scat 'offset- name)))
    `(progn

       (defun ,fname (char)
         (case char
           ,@(offset-clauses-to-case-clauses offset-clauses)
           (otherwise char)))
       
       (setf (gethash ',name *maps*)
             (make-map :offset-function (function ,fname)
                       :name ',name
                       :ranges ',(offset-clauses-to-ranges offset-clauses)))
       
       ',fname)))

(defun unicode-offset (map-name string)
  (map 'string (etypecase map-name
                 (function map-name)
                 (symbol (let ((map  (map-named map-name)))
                           (if map
                               (map-offset-function map)
                               (error "There's no map named ~S" map-name)))))
       string))

(defmacro define-unicode-offset-map (name (&rest offset-clauses))
  `(progn
     ,(generate-offset-function name offset-clauses)
     ,(generate-formatter-function name)))

(define-unicode-offset-map parenthesized-latin
             ((#\0 #\9 #x1d7f6)
              (#\a #\z 9372)))

(define-unicode-offset-map circled-latin
    ((#\0 9450)
     (#\1 #\9 #x2460)
     (#\A #\Z  9398)
     (:lower +26)))

(define-unicode-offset-map fullwidth-latin
    ((#\0 #\9  65296)
     (#\A #\Z  65313)
     (:lower +32)))

(define-unicode-offset-map mathematical-bold
    ((#\0 #\9  #x1d7f6)
     (#\A #\Z  119808)
     (:lower +26)))

(define-unicode-offset-map mathematical-bold-italic
    ((#\0 #\9  #x1d7f6)
     (#\A #\Z  119912)
     (:lower +26)))

;; (define-unicode-offset-map mathematical-script
;;     ((#\0 #\9  #x1d7f6)
;;      (#\A 119964)
;;      (#\C #\D 119965)
;;      (#\G #\D #x1D4A2)
;;      (:lower +26)
;;      (#\e #x212f)
;;      (#\g #x210A)
;;      (#\o #x2134)))

(define-unicode-offset-map mathematical-script-bold
    ((#\0 #\9  120782)
     (#\A #\Z  120016)
     (:lower +26)))

(define-unicode-offset-map mathematical-double-struck
    ((#\0 #\9  120792)
     (#\A #\Z  #x1d538)
     (:lower +26)
     (#\C 8450)
     (#\H 8461)
     (#\N 8469)
     (#\P 8473)
     (#\Q 8474)
     (#\R 8477)
     (#\Z 8484)))

(define-unicode-offset-map mathematical-fraktur
    ((#\1 #\9  #x02170)
     (#\A #\Z  #x1d504)
     (:lower +26)
     (#\C 8493)
     (#\H 8460)
     (#\I 8465)
     (#\R 8476)
     (#\Z 8488)))

(define-unicode-offset-map mathematical-fraktur-bold
    ((#\1 #\9  #x02160)
     (#\A #\Z  #x1d56c)
     (:lower +26)))

(define-unicode-offset-map mathematical-sans-serif
    ((#\0 #\9  120802)
     (#\A #\Z  #x1d5A0)
     (:lower +26)))

(define-unicode-offset-map mathematical-sans-serif-bold
    ((#\0 #\9  120812)
     (#\A #\Z  #x1d5D4)
     (:lower +26)))

(define-unicode-offset-map mathematical-sans-serif-italic
    ((#\0 #\9  #x1d7ce)
     (#\A #\Z  #x1d608)
     (:lower +26)))

(define-unicode-offset-map mathematical-sans-serif-bold-italic
    ((#\0 #\9  #x1d7ce)
     (#\A #\Z  120380)
     (:lower +26)))

(define-unicode-offset-map mathematical-monospace
    ((#\0 #\9 #x1D7F6)
     (#\A #\Z #x1D670)
     (:lower +26)))


;; braille has more complex rules.    
(defparameter *braille*
  '((capital "⠠")
    (number "⠼")
    (bracket "⠶")

    ("1" "⠼⠁")
    ("2" "⠼⠂")
    ("3" "⠼⠃")
    ("4" "⠼⠄")
    ("5" "⠼⠅")
    ("6" "⠼⠆")
    ("7" "⠼⠇")
    ("8" "⠼⠈")
    ("9" "⠼⠉")
    ("0" "⠼⠊")

    ("a" "⠁")
    ("b" "⠂")
    ("c" "⠃")
    ("d" "⠄")
    ("e" "⠅")
    ("f" "⠆")
    ("g" "⠇")
    ("h" "⠈")
    ("i" "⠉")
    ("j" "⠊")
    ("k" "⠋")
    ("l" "⠌")
    ("m" "⠍")
    ("n" "⠎")
    ("o" "⠏")
    ("p" "⠐")
    ("q" "⠑")
    ("r" "⠒")
    ("s" "⠓")
    ("t" "⠔")
    ("u" "⠥")
    ("v" "⠧")
    ("w" "⠺")
    ("x" "⠭")
    ("y" "⠽")
    ("z" "⠵")
    ("ç" "⠯")
    ("é" "⠿")
    ("à" "⠷")
    ("è" "⠮")
    ("ù" "⠾")
    ("â" "⠡")
    ("ê" "⠣")
    ("î" "⠩")
    ("ô" "⠹")
    ("û" "⠱")
    ("ë" "⠫")
    ("ï" "⠻")
    ("ü" "⠳")
    ("ö" "⠪")
    ("ì" "⠌")
    ("ä" "⠜")
    ("ò" "⠬")

    ("," "⠂")
    (";" "⠆")
    ("'" "⠄")
    (":" "⠒")
    ("-" "⠤")
    ("." "⠨")
    ("." "⠲")
    ("!" "⠖")
    ("?" "⠦")
    ("`" "⠦")
    ("‘" "⠦")
    ("’" "⠴")
    ("/" "⠌")
    ("(" "⠶")
    (")" "⠶")

    ("A" "⠠⠁")
    ("B" "⠠⠂")
    ("C" "⠠⠃")
    ("D" "⠠⠄")
    ("E" "⠠⠅")
    ("F" "⠠⠆")
    ("G" "⠠⠇")
    ("H" "⠠⠈")
    ("I" "⠠⠉")
    ("J" "⠠⠊")
    ("K" "⠠⠋")
    ("L" "⠠⠌")
    ("M" "⠠⠍")
    ("N" "⠠⠎")
    ("O" "⠠⠏")
    ("P" "⠠⠐")
    ("Q" "⠠⠑")
    ("R" "⠠⠒")
    ("S" "⠠⠓")
    ("T" "⠠⠔")
    ("U" "⠠⠥")
    ("V" "⠠⠧")
    ("W" "⠠⠺")
    ("X" "⠠⠭")
    ("Y" "⠠⠽")
    ("Z" "⠠⠵")
    ("Ç" "⠠⠯")
    ("É" "⠠⠿")
    ("À" "⠠⠷")
    ("È" "⠠⠮")
    ("Ù" "⠠⠾")
    ("Â" "⠠⠡")
    ("Ê" "⠠⠣")
    ("Î" "⠠⠩")
    ("Ô" "⠠⠹")
    ("Û" "⠠⠱")
    ("Ë" "⠠⠫")
    ("Ï" "⠠⠻")
    ("Ü" "⠠⠳")
    ("Ö" "⠠⠪")
    ("Ì" "⠠⠌")
    ("Ä" "⠠⠜")
    ("Ò" "⠠⠬")
    ))

(defun test ()
  (mapc (lambda (test-string)
          (format t (format nil "~{~40A~:*~~0@*~~/fmt-unicode:~A/~%~}" (map-names)) test-string))
        '("0123456789"
          "MONSIEUR JACK, VOUS DACTYLOGRAPHIEZ BIEN MIEUX QUE WOLF."
          "monsieur jack, vous dactylographiez bien mieux que wolf."))

  (format t "~30/fmt-unicode:mathematical-monospace/~%"  "Hello, World!")
  (format t "~30:/fmt-unicode:mathematical-monospace/~%" "Hello, World!")
  (format t "~30@/fmt-unicode:mathematical-monospace/~%" "Hello, World!"))

;;;; THE END ;;;;
