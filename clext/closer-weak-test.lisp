;;;; -*- coding:utf-8 -*-

#+clisp (pushnew :weak-test *features*)
#+#.#+(and clisp (not debug-weak))
(cl:if (cl:y-or-n-p "Are we debugging it on clisp?") '(and) '(or))
#-(and clisp (not debug-weak))'(or)  (pushnew :debug-weak *features*)

(ignore-errors
  (delete-package  "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK-USER")) 
(ignore-errors
  (delete-package  "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK"))
(load "closer-weak")
(in-package "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK-USER")


#+clisp(import '(ext:gc))
#+sbcl (defun gc () (sb-ext:gc :full t))
#+cmu  (import '(extensions:gc))

(defvar tab)

(load "tests")
(run-all-tests)



;; In clisp:
;;
;; finished   4 files:               0 errors out of    431 tests
;;   1                      weak:    0 errors out of    120 tests
;;   2                  weakhash:    0 errors out of     25 tests
;;   3                 weakhash2:    0 errors out of     46 tests
;;   4                   weakptr:    0 errors out of    240 tests

#|| 


(MAPCAR
 (LAMBDA (X)
   (LIST (WEAK-POINTER-P X)
         (WEAK-LIST-P X)
         (WEAK-AND-RELATION-P X)
         (WEAK-OR-RELATION-P X)
         (WEAK-MAPPING-P X)
         (WEAK-AND-MAPPING-P X)
         (WEAK-OR-MAPPING-P X)
         (WEAK-ALIST-P X)))
 (LIST '(A B C)
       #(A B C)
       (MAKE-WEAK-POINTER (LIST 'X))
       (MAKE-WEAK-LIST (LIST 'X 'Y 'Z))
       (MAKE-WEAK-AND-RELATION (LIST (LIST 'X)))
       (MAKE-WEAK-OR-RELATION (LIST (LIST 'X)))
       (MAKE-WEAK-MAPPING '#:G15 '#:G16)
       (MAKE-WEAK-AND-MAPPING (LIST '#:G15 '#:G16) '#:G17)
       (MAKE-WEAK-OR-MAPPING (LIST '#:G15 '#:G16) '#:G17)
       (MAKE-WEAK-ALIST)))

(LET ((A (LIST 'X))
      (B (LIST 'Y)))
  (LET ((W (MAKE-WEAK-AND-MAPPING (LIST A) B)))
    (GC)
    (LIST (MULTIPLE-VALUE-LIST (WEAK-AND-MAPPING-PAIR W))
          (MULTIPLE-VALUE-LIST (WEAK-AND-MAPPING-VALUE W)))))



(LET ((A (LIST 'X))
      (B (LIST 'Y)))
  (LET ((W (MAKE-WEAK-OR-MAPPING (LIST A) B)))
    (GC)
    (LIST (MULTIPLE-VALUE-LIST (WEAK-OR-MAPPING-PAIR W))
          (MULTIPLE-VALUE-LIST (WEAK-OR-MAPPING-VALUE W)))))

(LET
    ((A1 (LIST 'X1)) (A2 (LIST 'X2)) (A3 (LIST 'X3)) (A4 (LIST 'X4))
     (A5 (LIST 'X5)))
  (LET
      ((W1 (MAKE-WEAK-ALIST :INITIAL-CONTENTS (LIST (CONS A3 A4))))
       (W2 (MAKE-WEAK-ALIST :INITIAL-CONTENTS (LIST (CONS A1 A2))))
       (W3 (MAKE-WEAK-ALIST :INITIAL-CONTENTS (LIST (CONS A4 A5))))
       (W4 (MAKE-WEAK-ALIST :INITIAL-CONTENTS (LIST (CONS A2 A3)))))
    (SETQ A1 NIL A2 NIL A4 NIL A5 NIL) (GC)
    (LIST (WEAK-ALIST-CONTENTS W2)
          (WEAK-ALIST-CONTENTS W4)
          (WEAK-ALIST-CONTENTS W1)
          (WEAK-ALIST-CONTENTS W3))))


(LET ((A1 (LIST 'X1)) (A2 (LIST 'X2)) (A3 (LIST 'X3)) (A4 (LIST 'X4)) (A5 (LIST 'X5))) (LET ((W1 (MAKE-WEAK-ALIST :INITIAL-CONTENTS (LIST (CONS A3 A4)))) (W2 (MAKE-WEAK-ALIST :INITIAL-CONTENTS (LIST (CONS A1 A2)))) (W3 (MAKE-WEAK-ALIST :INITIAL-CONTENTS (LIST (CONS A4 A5)))) (W4 (MAKE-WEAK-ALIST :INITIAL-CONTENTS (LIST (CONS A2 A3))))) (SETQ A1 NIL A2 NIL A4 NIL A5 NIL) (GC) (LIST (WEAK-ALIST-CONTENTS W2) (WEAK-ALIST-CONTENTS W4) (WEAK-ALIST-CONTENTS W1) (WEAK-ALIST-CONTENTS W3))))



||#
