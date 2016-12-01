;;;; -*- coding:utf-8 -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(dolist (file  '(split-sequence
                 defpkg
                 mixins
                 attributes
                 species
                 thing
                 universe
                 comestible
                 living
                 character
                 room
                 dice
                 commands
                 comdefs
                 driver))
  (load (merge-pathnames (make-pathname :name (string file)
                                        :version :newest)
                         "PACKAGES:NET;COMMON-LISP;LMUD;LMUD;")))



;;;; lmud.lisp                        --                     --          ;;;;
