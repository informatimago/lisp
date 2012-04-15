;; -*- mode:lisp; coding:utf-8 -*-


(eval-when (:compile-toplevel)
  (print @"ASCII test string.")
  (print @"ISO-8859-1 test string, c'est bientôt l'été.")
  (print @"Unicodde test string, λαμβδα!")
  (print (class-of @"ASCII test string."))
  (print (class-of @"ISO-8859-1 test string, c'est bientôt l'été."))
  (print (class-of @"Unicodde test string, λαμβδα!"))
  (print (find-method (function make-load-form) nil (list (class-of @"ASCII test string.")) nil))
  (print (find-method (function make-load-form) nil (list (class-of @"ISO-8859-1 test string, c'est bientôt l'été.")) nil))
  (print (find-method (function make-load-form) nil (list (class-of @"Unicode test string, λαμβδα!")) nil))
  (terpri))

(defvar *test-1* @"ASCII test string.")

(defvar *test-2* @"ISO-8859-1 test string, c'est bientôt l'été.")

(defvar *test-3* @"Unicodde test string, λαμβδα!")
