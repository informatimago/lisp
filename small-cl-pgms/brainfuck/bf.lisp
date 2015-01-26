;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               bf.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Brainfuck emulator
;;;;    -1- A Brainfuck Virtual Machine                 COMPLETE
;;;;    -2- A Brainfuck to Lisp *optimizing* compiler   COMPLETE
;;;;    -3- Implementing a lisp in Brainfuck            sketches
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-09-11 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2015
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
;;;;****************************************************************************

(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.BRAINFUCK"
  (:use "COMMON-LISP")
  (:export "BFVM" "MAKE-BFVM" "BFVM-MEM" "BFVM-MC" "BFVM-PGM" "BFVM-PC")
  (:export "BFLOAD" "BFVM-RUN")
  (:export "BFCOMPILE-FILE" "BFCOMPILE" "*BFCOMPILE*"))
(in-package  "COM.INFORMATIMAGO.SMALL-CL-PGMS.BRAINFUCK")

;;;----------------------------------------------------------------------
;;; -1- A Brainfuck Virtual Machine
;;;----------------------------------------------------------------------

(defconstant +bfsize+ 30000)

(defstruct bfvm
  (mem (make-array +bfsize+ :element-type '(unsigned-byte 8) :initial-element 0))
  (mc 0)
  (pgm "" :type string)
  (pc 0))


(defun find-matching-close (pgm pc inc opn cls)
  (incf pc inc)
  (loop
     with level = 0
     until (and (zerop level)
                (or (< pc 0)
                    (<= (length pgm) pc)
                    (char= cls (char pgm pc))))
     do
       ;; (print `(level ,level pc ,pc @pc ,(char pgm pc)))
       (cond ((char= opn (char pgm pc)) (incf level))
             ((char= cls (char pgm pc)) (decf level)))
       (incf pc inc)
     finally (return pc)))

    
(defun bfload (file)
  "Return a string containing the file"
  (with-open-file (in file)
    (let ((pgm (make-string (file-length in))))
      (subseq pgm 0 (read-sequence pgm in)))))


(defun bfvm-run (vm &key verbose)
  (let* ((mem (bfvm-mem vm))
         (mc  (bfvm-mc  vm))
         (pgm (bfvm-pgm vm))
         (pc  (bfvm-pc  vm))
         (lpgm (length pgm))
         (lmem (length mem)))
    (macrolet ((in-range-p (counter limit) `(< -1 ,counter ,limit)))
      (unwind-protect
           (loop while (and (in-range-p pc lpgm) (in-range-p mc lmem)) do
                (when verbose
                  (format *trace-output*
                          "PC:~5,'0D IR:~C M[~5,'0D]:~2,'0X  ~:*~4@A  ~4@A  ~C~%"
                          pc
                          (if (char= (char pgm pc) #\newline)
                              #\space
                              (char pgm pc))
                          mc
                          (aref mem mc)
                          (if (<= 128 (aref mem mc))
                              (- (aref mem mc) 256)
                              (aref mem mc))
                          (if (graphic-char-p (code-char (aref mem mc)))
                              (code-char (aref mem mc))
                              #\space))
                  (force-output *trace-output*))
                (case (char pgm pc)
                  (#\>  (incf mc)
                        (incf pc))
                  (#\<  (decf mc)
                        (incf pc))
                  (#\+  (setf (aref mem mc) (mod (1+ (aref mem mc)) 256))
                        (incf pc))
                  (#\-  (setf (aref mem mc) (mod (1- (aref mem mc)) 256))
                        (incf pc))
                  (#\.  (princ (code-char (aref mem mc)))
                        (incf pc))
                  (#\,  (setf (aref mem mc) (mod (char-code (read-char)) 256))
                        (incf pc))
                  (#\[  (if (zerop (aref mem mc))
                            (setf pc (find-matching-close pgm pc +1 #\[ #\]))
                            (incf pc)))
                  (#\]  (if (zerop (aref mem mc))
                            (incf pc)
                            (setf pc (find-matching-close pgm pc -1 #\] #\[))))
                  (otherwise (incf pc))))
        (setf (bfvm-mc vm) mc
              (bfvm-pc vm) pc))))
  (values))


(defun test-vm ()
  (time (bfvm-run (make-bfvm :pgm (bfload "99botles.bf")) :verbose nil)))



;;;----------------------------------------------------------------------
;;; -2- A Brainfuck to Lisp *optimizing* compiler
;;;----------------------------------------------------------------------


(defun bfparse (pgm)
  (loop
     with result = '()
     with stack = '()
     for ch across pgm
     do (case ch
          (#\> (push '(%forward  1) result))
          (#\< (push '(%backward 1) result))
          (#\+ (push '(%inc 1 0) result))
          (#\- (push '(%dec 1 0) result))
          (#\, (push '(%readc 0) result))
          (#\. (push '(%princ 0) result))
          (#\[ (push result stack)
               (setf result (list '%while-nz)))
          (#\] (setf result (cons (nreverse result) (pop stack)))))
     finally (progn (when stack (error "Missing closing brackets"))
                    (return-from bfparse (nreverse result)))))


(defmacro %forward (offset)
  `(incf mc ,offset))

(defmacro %backward (offset)
  `(decf mc ,offset))

(defmacro %inc    (value offset)
  `(setf (aref mem (+ mc ,offset))
         (mod (+ (aref mem (+ mc ,offset)) ,value) 256)))

(defmacro %dec    (value offset)
  `(setf (aref mem (+ mc ,offset))
         (mod (- (aref mem (+ mc ,offset)) ,value) 256)))

(defmacro %readc  (offset)
  `(setf (aref mem (+ mc ,offset)) (mod (char-code (read-char)) 256)))

(defmacro %princ  (offset)
  `(princ (code-char (aref mem (+ mc ,offset)))))

(defmacro %while-nz (&body body)
  (let ((lbeg (gensym "LOOP"))
        (lend (gensym "ENDL")))
    `(tagbody (when (zerop (aref mem mc)) (go ,lend))
        ,lbeg
        ,@body
        (unless (zerop (aref mem mc)) (go ,lbeg))
        ,lend)))


(defun bfoptimize-1 (tgm)
  (cond
    ((null tgm) tgm)
    ((equalp (first tgm) '(%while-nz (%dec 1 0)))
     (cons '(%zero 0) (bfoptimize-1 (rest tgm))))
    ((and (consp  (first tgm))
          (member (first (first tgm)) '(%forward %backward %inc %dec))
          (consp  (second tgm))
          (eq     (first (first tgm)) (first (second tgm))))
     (loop
        for rtgm on (rest tgm)
        with sum = (second (first tgm))
        while (and (consp (first rtgm))
                   (eq (first (first tgm)) (first (first rtgm))))
        do (incf sum (second (first rtgm)))
        finally (return (cons (list (first (first tgm)) sum)
                              (bfoptimize-1 rtgm)))))
    ((and (consp (first tgm)) (eq (first (first tgm)) '%while-nz))
     (cons (cons '%while-nz (bfoptimize-1 (rest (first tgm))))
           (bfoptimize-1 (rest tgm))))
    (t (cons (first tgm) (bfoptimize-1 (rest tgm))))))


(defmacro %zero    (offset)
  `(setf (aref mem (+ mc ,offset)) 0))


(defun bfoptimize-2 (tgm)
  (cond
    ((null tgm) tgm)
    ((equal (first tgm) '(%zero 0))
     (cond
       ((and (consp (second tgm))
             (eq (first  (second tgm)) '%inc))
        (cons (list '%set (second (second tgm)))     (bfoptimize-2 (cddr tgm))))
       ((and (consp (second tgm))
             (eq (first  (second tgm)) '%dec))
        (cons (list '%set (- (second (second tgm)))) (bfoptimize-2 (cddr tgm))))
       (t (cons '(%set 0) (bfoptimize-2 (cdr tgm))))))
    ((and (consp (first tgm))
          (eq (first (first tgm)) '%dec))
     (cons (list '%inc (- (second (first tgm))))  (bfoptimize-2 (rest tgm))))
    ((and (consp (first tgm))
          (eq (first (first tgm)) '%backward))
     (cons (list '%offset (- (second (first tgm))))  (bfoptimize-2 (rest tgm))))
        ((and (consp (first tgm))
          (eq (first (first tgm)) '%forward))
     (cons (list '%offset (second (first tgm)))  (bfoptimize-2 (rest tgm))))
    ((and (consp (first tgm)) (eq (first (first tgm)) '%while-nz))
     (cons (cons '%while-nz (bfoptimize-2 (rest (first tgm))))
           (bfoptimize-2 (rest tgm))))
    (t (cons (first tgm) (bfoptimize-2 (rest tgm))))))


(defmacro %set    (value offset)
  `(setf (aref mem (+ mc ,offset)) (mod ,value 256)))


(defmacro %offset (offset)
  `(incf mc ,offset))



(defun bfoptimize-3-offset (tgm)
  (loop
     with result = '()
     with offset = 0
     for item in tgm
     do (case (first item)
          ((%offset)
           (incf offset (second item)))
          ((%inc %set)
           (push `(,(first item) ,(second item) ,offset) result))
          ((%readc %princ)
           (push `(,(first item) ,(+ (second item) offset)) result))
          (otherwise (error "unexpected item ~A" item)))
     finally
       (unless (zerop offset) (push `(%offset ,offset) result))
       (return (nreverse result))))

(defun bfoptimize-3 (tgm)
  (let ((end (position '%while-nz tgm  :key (function first))))
    (if end
        (nconc (bfoptimize-3-offset (subseq tgm 0 end))
               (cons (cons '%while-nz (bfoptimize-3 (rest (elt tgm end))))
                     (bfoptimize-3 (subseq tgm (1+ end)))))
        (bfoptimize-3-offset tgm))))


;;; Uncomment these macros to trace the compiled forms:
;;
;; (defmacro %forward (offset)
;;   `(progn (incf mc ,offset)
;;           (print `(forward ,,offset --> mc = ,mc))))
;; 
;; (defmacro %backward (offset)
;;   `(progn (decf mc ,offset)
;;           (print `(backward ,,offset --> mc = ,mc))))
;; 
;; (defmacro %inc    (value offset)
;;   `(progn (setf (aref mem (+ mc ,offset))
;;                 (mod (+ (aref mem (+ mc ,offset)) ,value) 256))
;;           (print `(inc ,,value ,,offset --> (aref mem ,(+ mc ,offset)) = ,(aref mem (+ mc ,offset))))))
;; 
;; (defmacro %dec    (value offset)
;;   `(progn (setf (aref mem (+ mc ,offset))
;;                 (mod (- (aref mem (+ mc ,offset)) ,value) 256))
;;           (print `(dec ,,value ,,offset --> (aref mem ,(+ mc ,offset)) = ,(aref mem (+ mc ,offset))))))
;; 
;; (defmacro %readc  (offset)
;;   `(progn (setf (aref mem (+ mc ,offset)) (mod (char-code (read-char)) 256))
;;           (print `(readc ,,offset --> (aref mem ,(+ mc ,offset)) = ,(aref mem (+ mc ,offset))))))
;; 
;; (defmacro %princ  (offset)
;;   `(progn (princ (code-char (aref mem (+ mc ,offset))))
;;           (print `(princ ,,offset --> (aref mem ,(+ mc ,offset)) = ,(aref mem (+ mc ,offset))))))
;; 
;; (defmacro %while-nz (&body body)
;;   (let ((lbeg (gensym "LOOP"))
;;         (lend (gensym "ENDL")))
;;     `(tagbody
;;         (print `(while-nz ,',lbeg begin  (aref mem ,mc) = ,(aref mem mc)))
;;         (when (zerop (aref mem mc)) (go ,lend))
;;         ,lbeg
;;         ,@body
;;         (print `(while-nz ,',lbeg loop  (aref mem ,mc) = ,(aref mem mc)))
;;         (unless (zerop (aref mem mc)) (go ,lbeg))
;;         ,lend)))
;; 
;; (defmacro %zero    (offset)
;;   `(progn (setf (aref mem (+ mc ,offset)) 0)
;;           (print `(zero ,offset --> (aref mem ,(+ mc offset)) = ,(aref mem (+ mc offset))))))
;; 
;; (defmacro %set    (value offset)
;;   `(progn (setf (aref mem (+ mc ,offset)) (mod ,value 256))
;;           (print `(set ,,value ,,offset -->  (aref mem ,(+ mc ,offset)) = ,(aref mem (+ mc ,offset))))))
;; 
;; (defmacro %offset (offset)
;;   `(progn (incf mc ,offset)
;;           (print `(offset ,,offset --> mc = ,mc))))



(defvar *bfcompile* nil
  "When true, bfcompile compiles also the generated Lisp code.")


(defun bfcompile (pgm &key name ((:compile *bfcompile*) *bfcompile*))
  "
PGM:    a string containing the source of a Brainfuck program.
RETURN: a lisp function taking a virtual machine
        (only the memory and MC register are used),
        and realizing the same program.
"
  (flet ((do-compile (lambda-form) (compile name lambda-form))
         (do-eval    (lambda-form) (eval (if name
                                        `(defun ,name (vm) (,lambda-form vm))
                                        lambda-form))))
    (funcall (if *bfcompile*
                 (function do-compile)
                 (function do-eval))
             `(lambda (vm)
                (let ((mem (bfvm-mem vm))
                      (mc  (bfvm-mc  vm)))
                  (unwind-protect
                       (progn
                         ,@(bfoptimize-3
                            (bfoptimize-2
                             (bfoptimize-1
                              (bfparse pgm)))))
                    (setf (bfvm-mc vm) mc)))))))


(defun bfcompile-file (file &key ((:compile *bfcompile*) *bfcompile*))
  "Combines bfcompile and bfload."
  (bfcompile (bfload file) :compile *bfcompile*))


(defun test-compiler ()
  (time (funcall (bfcompile-file "99botles.bf")
                 (make-bfvm)))
  (time (funcall (bfcompile-file "99botles.bf" :compile t)
                 (make-bfvm))))




;;;----------------------------------------------------------------------
;;; -3- Implementing a lisp in Brainfuck
;;;----------------------------------------------------------------------


;; lisp primitives:
;;     () eq cons car cdr atom quote cond  lambda
;;
;; lisp registers stored in brainfuck memory:
;;     | 0 , sph , spl , sph , spl | 1 , hph , hpl , hph , hpl |
;;     | 2 , ach , acl , ach , acl |


(defconstant +max-addr+ (1- (truncate +bfsize+ 5)))
(defconstant +sc+ 1)
(defconstant +sp+ 2 "stack pointer")
(defconstant +hp+ 3 "heap  pointer")
(defconstant +ix+ 4 "")
(defconstant +ac+ 5 "accumulator a")
(defconstant +bc+ 6 "accumulator b")
(defconstant +ts+ 7)
(defconstant +cn+ 8)
(defconstant +min-addr+ 9)


(defun store-imm-to-car (n)
  (format nil ">[-]~A>[-]~A<<"
          (make-string (truncate n 256) :initial-element #\+)
          (make-string (mod n 256)      :initial-element #\+)))
(defun store-imm-to-cdr (n)
  (format nil ">>>[-]~A>[-]~A<<<<"
          (make-string (truncate n 256) :initial-element #\+)
          (make-string (mod n 256)      :initial-element #\+)))

(defun move-from-to (from to)
  (if (< to from)
      (make-string (* 5 (- from to)) :initial-element #\<)
      (make-string (* 5 (- to from)) :initial-element #\>)))

;; sp = (car 0) ; initially = 5999 = +max-addr+ =  bfsize/5-1
;; hp = (cdr 0) ; initially = 2
;; ac = 1

(defvar *simplify* t)

(defun simplify (bf)
  ;; delete all occurences of "><" "<>" "+-" "-+"
  ;;(print `(simplify ,bf))
  (if *simplify* 
      (loop
         with changed = t
         while changed
         do (labels
                ((simplify-couple (couple start)
                   (let ((pos (search couple bf :start2 start)))
                     (cond
                       (pos
                        (loop
                           with l = (1- pos)
                           with r = (+ pos (length couple))
                           while (and (< start l)
                                      (< r (length bf))
                                      (char= (char couple 0) (char bf l))
                                      (char= (char couple (1- (length couple)))
                                             (char bf r)))
                           do (setf changed t) (decf l) (incf r)
                           finally
                             (incf l)
                             ;; (print `(delete ,(subseq bf l r)))
                             ;; (print `(left ,(subseq bf start l)))
                             (return (concatenate 'string
                                             (subseq bf start l)
                                             (simplify-couple couple r)))))
                       ((zerop start)
                        ;; (print `(result ,bf))
                        bf)
                       (t
                        ;; (print `(right ,(subseq bf start)))
                        (subseq bf start))))))
              (setf changed nil)
              (setf bf (simplify-couple "<>" 0))
              (setf bf (simplify-couple "><" 0))
              (setf bf (simplify-couple "+-" 0))
              (setf bf (simplify-couple "-+" 0))
              )
         finally (return bf))
      bf))

(defmacro defbf (name args &body body)
  "
Defines a lisp function  that will generate brainfuck code translated
from the body.  The body itself consists in other functions defined
with defbf, or strings containing brainfuck instructions.
"
  (let ((vout (gensym)))
    `(defun ,name ,args
       (simplify (with-output-to-string (,vout)
                   ,@(mapcar (lambda (item) `(princ ,item ,vout)) body))))))


(defmacro repeat (repcnt &body body)
  (let ((vout (gensym)))
    `(with-output-to-string (,vout)
       (loop :repeat ,repcnt
             :do ,@(mapcar (lambda (item) `(princ ,item ,vout)) body)))))

(defmacro while-nz (&body body)
  (let ((vout (gensym)))
    `(with-output-to-string (,vout)
       (princ "[" ,vout)
       ,@(mapcar (lambda (item) `(princ ,item ,vout)) body)
       (princ "]" ,vout))))

(defmacro progbf (&body body)
  (let ((vout (gensym)))
    `(with-output-to-string (,vout)
       ,@(mapcar (lambda (item) `(princ ,item ,vout)) body))))

(defmacro letbf (bindings &body body)
  (let ((vout (gensym)))
    `(with-output-to-string (,vout)
       (let ,bindings
         ,@(mapcar (lambda (item) `(princ ,item ,vout)) body)))))


(defbf previous       ()    "<<<<<")
(defbf next           ()    ">>>>>")
(defbf rewind         ()    (while-nz (previous)))
(defbf backward-0     ()    (while-nz (previous)))
(defbf forward-0      ()    (while-nz (next)))
(defbf backward-1     ()    "-" (while-nz "+"  (previous) "-") "+")
(defbf forward-1      ()    "-" (while-nz "+"  (next)     "-") "+")
(defbf forward-1-set  ()    "-" (while-nz "++" (next)     "-") "+")
(defbf goto           (dst) (rewind) (move-from-to 0 dst))
(defbf clear-byte     ()    (while-nz "-"))
(defbf set-byte       ()    (while-nz "-") "+")
(defbf clear-cons     ()
  ">" (clear-byte) ">" (clear-byte) ">" (clear-byte) ">" (clear-byte) "<<<<")

  
(defbf format-memory ()
  (rewind) (clear-byte)
  (repeat  +max-addr+ (next) "+")
  (rewind) (next)
  (store-imm-to-car 0)          (store-imm-to-car 0) (next)   ; sc
  (store-imm-to-car +max-addr+) (store-imm-to-cdr 0) (next)   ; sp
  (store-imm-to-car +min-addr+) (store-imm-to-cdr 0) (next)   ; hp
  (rewind))


(defun dump-memory (vm)
  (loop
     with mem = (bfvm-mem vm)
     for addr to +max-addr+
     do (when (zerop (mod addr 5)) (format t "~%~4,'0X: " addr))
     (format t "~2,'0X ~2,'0X~2,'0X ~2,'0X~2,'0X  "
             (aref mem (+ (* 5 addr) 0))
             (aref mem (+ (* 5 addr) 1))
             (aref mem (+ (* 5 addr) 2))
             (aref mem (+ (* 5 addr) 3))
             (aref mem (+ (* 5 addr) 4)))
     finally (format t "~%")))


(defbf move-cdr-to-car ()
  ">>>" (while-nz "-<<+>>") ">" (while-nz "-<<+>>") "<<<<")


(defbf copy-reg-byte (src dst)
  (while-nz "-"
            (move-from-to src +sc+) "+"  ; copy src to +sc+
            (move-from-to +sc+ dst) "+"  ; copy src to dst
            (move-from-to dst src))      ; and back to src
  (move-from-to src +sc+)
  (while-nz "-"
            (move-from-to +sc+ src) "+" ; copy +sc+ to src
            (move-from-to src +sc+))
  (move-from-to +sc+ src))

(defbf copy-reg (src dst)
  (goto +sc+)             (clear-cons)
  (move-from-to +sc+ dst) (clear-cons)
  (move-from-to dst src)
  ">" (copy-reg-byte src dst) 
  ">" (copy-reg-byte src dst)
  ">" (copy-reg-byte src dst)
  ">" (copy-reg-byte src dst)
  "<<<<")                               ; back to src position


(defbf null-car (reg)
  (copy-reg reg +ts+)
  (goto +ts+)
  ">"  (while-nz "<"  (clear-byte) ">"  (clear-byte)) ; set mark if nz
  ">"  (while-nz "<<" (clear-byte) ">>" (clear-byte)) ; set mark if nz
  "<<" (while-nz ">>+<<-") ; move flag to lsb of car
  "+") ; set mark


(defbf not-null-car (reg)
  (copy-reg reg +ts+)
  (goto +ts+)
  (clear-byte)
  ">"  (while-nz "<"  (set-byte) ">"  (clear-byte)) ; set mark if nz
  ">"  (while-nz "<<" (set-byte) ">>" (clear-byte)) ; set mark if nz
  "<<" (while-nz ">>+<<-") ; move flag to lsb of car
  "+") ; set mark




(defbf increment-car (reg)
  (goto reg)
  ">>+"                                 ; increment lsb of car
  (while-nz                             ; if lsb is nz
   "<<" (clear-byte)                    ; clear mark
   (previous) (goto 1) ">>" (clear-byte)) 
  "<<"                                     ; goto mark
  (previous)                            ; move away from cleared mark
  (goto reg)                            ; come back
  ;; mark = 0 <=>  lsb is nz ; mark = 1 <=> lsb is z
  (while-nz                             ; mark = 1 ==> lsb is z
   ">+"                                 ; increment msb of car
   "<" (clear-byte))                    ; clear mark
  "+")                                  ; set mark
  

(defbf decrement-car (reg)
  (goto reg)
  ">>"                                  ; goto lsb of car
  (while-nz                             ; if lsb is nz
   "-"                                  ; decrement it
   "<<" (clear-byte)                    ; clear mark
   (previous) (goto +sc+) ">>" (clear-byte)) 
  "<<"                                    ; goto mark
  (previous)                            ; move away from cleared mark
  (goto reg)                            ; come back
  ;; mark = 0 <=>  lsb was nz ; mark = 1 <=> lsb is z
  (while-nz                             ; mark = 1 ==> lsb is z
   ">->-<<"                              ; roll over lsb; decrement msb
   (clear-byte))                    ; clear mark
  "+")                                  ; set mark


(defbf goto-indirect (reg)
  ;; move to address pointed to by (car reg)
  (copy-reg reg +cn+)
  (repeat +min-addr+
          (decrement-car +cn+))
  (not-null-car +cn+) ; at +ts+
  ">>"
  (while-nz
   "<<" (move-from-to +ts+ +min-addr+)
   (forward-1)
   "-"
   (backward-1)
   (decrement-car +cn+)
   (not-null-car +cn+) ; at +ts+
   ">>")
  "<<" (move-from-to +ts+ +min-addr+)
  (forward-1-set))


(defbf goto-indirect (reg)
  ;; move to address pointed to by (car reg)
  (copy-reg reg +cn+)
  (repeat +min-addr+
          (decrement-car +cn+))
  (goto +cn+)
  ">>" ;; lsb of cn
  (while-nz
   "<<" (move-from-to +cn+ +min-addr+)
   (forward-1)
   "-"
   (backward-1)
   (goto +cn+) ">>-")
  "<" ; msb of cn
  (while-nz
   "<" (move-from-to +cn+ +min-addr+)
   (forward-1)
   (repeat 256 "-" (next))
   (previous) (backward-1)
   (goto +cn+) ">-")
  "<" (move-from-to +cn+ +min-addr+)
  (forward-1-set) "-")



(defbf copy-byte-forward (offset)
  (repeat offset ">")
  (while-nz
   (repeat offset "<")
   (next)
   (forward-0)
   (repeat offset ">")
   "+"
   (repeat offset "<")
   (previous)
   (backward-0)
   (repeat offset ">")
   "-")
  (repeat offset "<"))


(defbf copy-byte-backward (offset)
  (repeat offset ">")
  (while-nz
   (repeat offset "<")
   (previous)
   (backward-0)
   (repeat offset ">")
   "+"
   (repeat offset "<")
   (next)
   (forward-0)
   (repeat offset ">")
   "-")
  (repeat offset "<"))


(defbf store-ac (reg)
  ;; store ac to cons at (car reg)
  (goto-indirect reg) (clear-cons) "-" ; clear mark
  (previous)
  (copy-reg +ac+ +cn+)
  (goto +cn+) "-" ; clear mark
  (copy-byte-forward 1)
  (copy-byte-forward 2)
  (copy-byte-forward 3)
  (copy-byte-forward 4)
  "+" (forward-0) "+") ; set marks.


(defbf load-ac (reg)
  ;; load ac from cons at (car reg)
  (goto-indirect reg) "-" ; clear mark
  (previous)
  (goto +ac+) (clear-cons) "-" ; clear mark
  (next) (forward-0)
  (copy-byte-backward 1)
  (copy-byte-backward 2)
  (copy-byte-backward 3)
  (copy-byte-backward 4)
  (previous)
  (backward-0)
  "+" ; set mark
  (copy-reg +ac+ +cn+)
  (goto +cn+) "-" ; clear mark
  (copy-byte-forward 1)
  (copy-byte-forward 2)
  (copy-byte-forward 3)
  (copy-byte-forward 4)
  "+" (forward-0) "+") ; set marks.


(defbf push-ac ()
  (decrement-car +sp+)
  (store-ac +sp+))

(defbf pop-ac ()
  (load-ac +sp+)
  (increment-car +sp+))



;;;---------------------------------------------------------------------

(defbf test1 (n a d)
  (format-memory)
  (goto +ac+)
  (store-imm-to-car a)
  (store-imm-to-cdr d)
  (goto +cn+)
  (store-imm-to-car n)
  (decrement-car +sp+)
  (decrement-car +sp+)
  (store-ac +sp+)
  ;;(repeat n (push-ac))
  )


(defbf test1 (&rest args)
  (format-memory)
  (goto +ac+)
  (store-imm-to-car #x0030)
  (store-imm-to-cdr #xbeef)
  (goto-indirect +ac+))



;; (copy-reg +sp+ +cn+)
;; (repeat +min-addr+
;;         (decrement-car +cn+))
;; (not-null-car +cn+)
;; ">>"
;; ;;(while-nz
;; "<<" (move-from-to +ts+ +min-addr+)
;; (forward-1)
;; "-"
;; (backward-1)
;; (decrement-car +cn+)
;; (not-null-car +cn+)                     ; at +ts+
;; ">>"
;; 
;; "<<" (move-from-to +ts+ +min-addr+)
;; (forward-1)
;; "-"
;; (backward-1)
;; (decrement-car +cn+)
;; (not-null-car +cn+)                     ; at +ts+
;; ">>"
;; )
;; 
;; "<<" (move-from-to +ts+ +min-addr+)
;; (forward-1-set))
;; 
;;   
;; 
;; (goto-indirect reg) (clear-cons) "-"    ; clear mark
;; ))
;; (previous)
;; (copy-reg +ac+ +cn+)
;; (goto +cn+) "-"                         ; clear mark
;; ))


#||

(progn (bfcompile '%test1 (test1 3 #xdead #xbeef))
       (setf vm (make-bfvm)) (%test1 vm) (dump-memory vm))

(progn (bfcompile '%test1 (store-ac +sp+)
       (setf vm (make-bfvm)) (%test1 vm) (dump-memory vm))


||#


(defun test-lisp/bf-vm ()
  (progn (setf vm  (make-bfvm :pgm (format-memory)))
         (bfvm-run  vm :verbose nil)
         (dump-memory vm)
         (setf (bfvm-pc vm) 0
               (bfvm-pgm vm) (progbf
                              (goto +ac+)
                              (store-imm-to-car #xdead)
                              (store-imm-to-cdr #xbeef)
                              (push-ac)
                              (goto +ac+)
                              (store-imm-to-car #xcafe)
                              (store-imm-to-cdr #xbabe)
                              (push-ac)))
         (bfvm-run  vm :verbose nil)
         (dump-memory vm)))


;; (defun bfeval (sexp env)
;;   (cond
;;     ((atom sexp) sexp)
;;     ((eq (car sexp) 'apply)
;;      (apply (function bfapply) (mapcar (function bfeval) (cdr sexp))))
;;     ((eq (car sexp) 'define)
;;      (
;; (eval '(apply (lambda () 
;;                 (define sym definition)
;;                 (define sym definition)
;;                 (define sym definition)
;;                 (sym))) ())
;; 
;; 
;; (defmacro my-defun (name arg-list &body body)
;;   `(progn (setf (symbol-function ',name) (lambda  ,arg-list (progn  ,@body)))
;;           ',name))
;; 
;; 
;; (defun my-apply (fun &rest effective-arg-list)
;;   (let* ((lambda-exp      (function-lambda-expression fun))
;;          (formal-arg-list (cadr lambda-exp))
;;          (sexp            (caddr lambda-exp)))
;;     (if (eq 'lambda (car lambda-exp))
;;         ;; let's skip the argument binding
;;         (my-eval sexp)
;;         ;; a primive function
;;         (funcall (function apply) fun effective-arg-list))));;my-apply
;; 
;; 
;; (defun symbol-value (symbol env)
;;   (cond ((null env) :unbound)
;;         ((eq symbol (car (car env))) (cdr (car env)))
;;         (t (symbol-value symbol (cdr env)))))
;; 
;;   
;; (defun my-eval (sexp env)
;;   (cond
;;    ((symbolp sexp)          (symbol-value sexp env))
;;    ((atom    sexp)          sexp)
;;    ((eq (car sexp) 'quote)  (car (cdr sexp)))
;;    ((eq (car sexp) 'if)     (if (my-eval (car (cdr sexp)))
;;                                 (my-eval (car (cdr (cdr sexp))))
;;                                 (my-eval (car (cdr (cdr (cdr sexp)))))))
;;    ((setq)   (setf (symbol-value (cadr sexp)) (my-eval (caddr sexp))))
;;         ((rplaca) (rplaca (my-eval (cadr sexp)) (my-eval (caddr sexp))))
;;         ((progn)
;;          (my-eval (cadr sexp))
;;          (if (cddr sexp) (my-eval (cons 'progn (cddr sexp)))))
;;         (otherwise
;;          (my-apply (symbol-function (car sexp))
;;                 (mapcar (function my-eval) (cdr sexp))))))));;my-eval
;; 
;; 
;; (my-defun a-fun () (setq x '(1 2 3)) (print x) (rplaca x 0) (print x))

;;  (bfcompile(bfload "99botles.bf") :name '99b)  (|99B| (make-bfvm))

#||
(test-vm)
(test-compiler)
(test-lisp/bf-vm)
||#



;; Local Variables:
;; eval: (cl-indent 'defbf 2)
;; End:
