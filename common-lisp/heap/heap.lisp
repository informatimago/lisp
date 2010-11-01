;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               heap.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package implements a heap for "common" data
;;;;    in shared memory segments.
;;;;    There is a garbage collector, and lisp data types.
;;;;
;;;;SEE
;;;;    "COMMON USER-LEVEL API" bellow.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-10 <PJB> Added some EVAL-WHEN needed for macros.
;;;;    2004-12-18 <PJB> Reached usable state.
;;;;    2004-12-02 <PJB> Created.
;;;;BUGS
;;;;    Copying of some important types is not implemented yet:
;;;;    double-float, vectors, arrays, structures (unfortunately, 
;;;;    structures will need either a define-common-structure, or 
;;;;    MOP or implementation specific API to get the list of slots).
;;;;
;;;;    with-common-lock mutex is too wide.  With smarter treatment of 
;;;;    new-generation, and possibly ad hoc mutexes, it should be
;;;;    possible to mutex only gc-allocate.
;;;;
;;;;    The number of different cell types should be reduced, 
;;;;    and the set of cell types should be distinct from the
;;;;    set of types of array elements.
;;;;
;;;;    The size and encoding of cells should be parameterizable,
;;;;    and a 32-bit version should be available on "legacy" systems.
;;;;
;;;;
;;;;    ;; We need a WITH-COMMON macro such as:
;;;;    
;;;;    (defcommon head nil)
;;;;    (defcommon tail nil)
;;;;    
;;;;    (defun insert (item)
;;;;      (with-common
;;;;       (with-common-lock
;;;;        (if (null head)
;;;;          (setf head (list item)
;;;;                tail head)
;;;;          (setf (cdr tail) (list item)
;;;;                tail (cdr tail))))))
;;;;    
;;;;    ;; would be generated as:
;;;;    
;;;;    (defun insert (item)
;;;;      (with-common-lock
;;;;       (let ((phead  (cfi-copy-to-common 'head))
;;;;             (ptail  (cfi-copy-to-common 'tail)))
;;;;         (if (cvm-null head)
;;;;           (progn
;;;;             (cvm-symbol-set-value phead (cfi-copy-to-common (list item)))
;;;;             (cvm-symbol-set-value ptail (cfi-symbol-value phead)))
;;;;           (progn
;;;;             (cvm-setcdr ptail (cfi-copy-to-common (list item)))
;;;;             (cvm-symbol-set-value ptail (cvm-cdr ptail)))))))
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
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
;;;;****************************************************************************

(CL:IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HEAP.HEAP"
  (:USE "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.HEAP.MEMORY")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY" "WSIOSBP" "DEFENUM")
  (:EXPORT "SET-COMMON" "GET-COMMON" "WITH-COMMON-LOCK" "*COMMON-VARIABLES*"
           "DEFCOMMON" "COMMON-INITIALIZE")
  (:DOCUMENTATION
   "
      This package implements a heap for 'common' data
      in shared memory segments.
      There is a garbage collector, and lisp data types.

      Copyright Pascal J. Bourguignon 2004 - 2005
      
      This program is free software; you can redistribute it and/or
      modify it under the terms of the GNU General Public License
      as published by the Free Software Foundation; either version
      2 of the License, or (at your option) any later version.
     "))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HEAP.HEAP")






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEBUGGING FLAGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *check-args* '(:dump-bitmap t
                       :dump-header t
                       :dump-free t
                       :dump-allocated t)
  "See the keyword arguments of gc-check")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+debug+)
    (defconstant +DEBUG+ '(:*debug*)
      "Possible items:
     :*debug*  check *DEBUG* at run-time.
     :range    check gc-address range.
     :check    check heap invariants.
     :ng       trace the new-generation stack.
     :ld       trace circle detection (calls to ld-put and ld-get).
     :objects  trace object allocations.
     :gc       debugging garbage collector.
     :allocate debugging allocator.
     :gcct     debugging checking code.
     :bitmap   debugging debugging code.
     :emulated-shared-memory Don't use shm, use a lisp vector for common memory.
                             (Use it only in +DEBUG+, not in *DEBUG*).
     ")))
(defvar *debug* nil)

(defmacro when-debug (what &body body)
  (cond
    ((intersection what +debug+)
     `(let ((*standard-output* *trace-output*)) ,@body))
    ((member :*debug* +debug+)
     `(when (intersection ',what   *debug*)
        (let ((*standard-output* *trace-output*)) ,@body)))))

(defmacro unless-debug (what &body body)
  (unless (intersection what +debug+)
    `(let ((*standard-output* *trace-output*)) ,@body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMMON HEAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; heap:
;; ---- ----- --- -----------------
;; addr ofset siz description
;; ---- ----- --- -----------------          
;;    0    0:   8 magic-cookie               | signature header
;;    1    8:   8 key | semaphore-number     |
;; -------------- heap-header                \
;;    2   16:   8 ct-structure 8             | heap root: heap-header structure
;;    3   24:   8 ct-t         6             |           + free-blocks array
;;    4   32:   8 class                      |  [ct-address SYSTEM:HEAP-HEADER]
;;    5   40:   8 size                       |
;;    6   48:   8 free-blocks                |  [ct-address 10]
;;    7   56:   8 root                       |  [ct-nil] initially.
;;    8   64:   8 new-generation             |  (cons nil nil) initially.
;;    9   72:   8 reserved                   /  [ct-nil]                 
;; -------------- free-blocks                \
;;   10   80:   8 ct-vector   34             |
;;   11   88:   8 ct-t        32             |
;;   12   96:   8 free-blocks[0] (size=16)   |  [ct-address 42] initially.
;;   13  104:   8 free-blocks[1] (size=24)   |  [ct-nil] initially.
;;   14  112:   8 free-blocks[2] (size=32)   |  [ct-nil] initially.
;;      ....      ....                       |  ....
;;   42  320:   8 free-blocks[30] (size=256) |  [ct-nil] initially.
;;   43  328:   8 free-blocks[31] (size>256) /  [ct-address 128] initially.
;; -------------- first free cons initially ---- from here down: gc managed.
;;   44  336:  16 20 initially free cons cells. [ct-free-block 2|43] initally.
;;      ....      ....
;;   63  504:  16 last initially free cons cell [ct-free-block 2|0] intially.
;;   64  512: 512 first free big block initially [ct-free-block 64|128]
;; -------------- second free big block initially.
;;  128 1024:     1 big block to the end of     [ct-free-block nnn|0] initially.
;;                              the segment.
;;      ffff:     last byte
;; -------------- -----------------
;; variable is a cvm-list of cvm-symbols.
;;
;; Addresses in shared segment are offset from the beginning of the segment.
;; We don't keep the segment key with the offset because we won't be using
;; normally intersegment addresses.  However, only the ct-address 
;; and ct-readable cell types would have to be changed if we used 
;; segmented addresses to allow growing the common-space in 
;; multi-segment fashion.



;; common::gc global variables:
;; (These variables could go into an object as slots 
;;  if multiple common heaps were needed).
(defvar *gc-memory*                nil) ; MEMORY instance.
(defvar *gc-heap-base*             0)   ; (base *gc-memory*)
(defvar *gc-heap-size*             0)   ; (size *gc-memory*) 8)
(defvar *gc-magic-cookie*          (+ 1 (random (expt 2 63)))) ; /= 0
(defvar *gc-symbol*                0) ; cvm-address of COMMON-LISP:SYMBOL
(defvar *gc-package*               0) ; cvm-address of COMMON-LISP:PACKAGE


(defmacro with-common-lock (&body body) `(with-memory *gc-memory* ,@body))
(defmacro with-gc-lock     (&body body) `(progn ,@body))



;;--------------------------
;; cell access / raw-memory
;;--------------------------
;; CVM Memory is organized as an array of 64-byte cells;
;; CVM Memory addresses (gc-addresses) are indices in this array.
;; valid gc-addresses are from 1 to (1- size)

(declaim (inline gc-store gc-load))


(defun gc-signature ()     
  (peek-uint64 *gc-memory* *gc-heap-base*))
(defun gc-sign (signature)
  (poke-uint64 *gc-memory* *gc-heap-base* signature))
(defun gc-store (gc-address object)
  (assert (< 0 gc-address (size *gc-memory*)))
  (poke-uint64 *gc-memory* (+ *gc-heap-base* gc-address) object))
(defun gc-load (gc-address)
  (assert (< 0 gc-address (size *gc-memory*)))
  (peek-uint64 *gc-memory* (+ *gc-heap-base* gc-address)))
(defun gc-peek-function (bit-size)
  (case bit-size
    (( 8) (values (lambda (address) (peek-uint8  *gc-memory* address)) 1))
    ((16) (values (lambda (address) (peek-uint16 *gc-memory* address)) 2))
    ((32) (values (lambda (address) (peek-uint32 *gc-memory* address)) 4))
    ((64) (values (lambda (address) (peek-uint64 *gc-memory* address)) 8))
    (otherwise (error "No peek function defined for bit field of width ~D" 
                      bit-size))))
(defun gc-poke-function (bit-size)
  (case bit-size
    (( 8) (values (lambda (address object) 
                    (poke-uint8  *gc-memory* address object)) 1))
    ((16) (values (lambda (address object) 
                    (poke-uint16 *gc-memory* address object)) 2))
    ((32) (values (lambda (address object) 
                    (poke-uint32 *gc-memory* address object)) 4))
    ((64) (values (lambda (address object) 
                    (poke-uint64 *gc-memory* address object)) 8))
    (otherwise (error "No poke function defined for bit field of width ~D" 
                      bit-size))))
(defun gc-dump-block (gc-address size stream &key (margin ""))
  (dump *gc-memory* (+ *gc-heap-base* gc-address) size
        :stream stream :margin margin :byte-size 8))


                
;;----------------------------------------
;; cell-type: tags
;;----------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defenum cell-type 
    "Some types are used for memory cells (ie. boxed values),
but some types are used only for array cells (ie. unboxed values)."
    ct-nil
    ct-t
    ct-unbound
    ct-bit                              ; boolean
    ct-character-8           ; --> char-code / code-char, no encoding!
    ct-character-16
    ct-character-24
    ct-character-32                     ; character
    ct-signed-byte-8
    ct-signed-byte-16
    ct-signed-byte-24
    ct-signed-byte-32
    ct-signed-byte-40
    ct-signed-byte-48
    ct-signed-byte-56                   ; fixnum
    ct-signed-byte-64
    ct-unsigned-byte-8
    ct-unsigned-byte-16
    ct-unsigned-byte-24
    ct-unsigned-byte-32
    ct-unsigned-byte-40
    ct-unsigned-byte-48
    ct-unsigned-byte-56
    ct-unsigned-byte-64
    ct-float-8
    ct-float-16
    ct-float-24
    ct-float-32                         ; single-float
    ct-float-40
    ct-float-48
    ct-float-56
    ct-float-64                         ; double-float 
    ct-structure
    ct-vector
    ct-vector-fp
    ct-array
    ct-address
    ct-readable
    ct-free-block)
  (defconstant ct-cons  64)
  (defconstant ct-mark 128))


;;--------------------------
;; structures
;;--------------------------

(defmacro cvm-define-structure (name type &rest fields)
  ;; TODO: we could do without the defconstant for structure fields...
  (wsiosbp
   `(progn
      (defun ,(intern (format nil "CVM-~A-P" name)) (self)
        (and (cvm-structure-p self)
             (= ,(1+ (length fields)) (cvm-length self))
             (= ct-t   (cvm-element-type self))
             (= ,(cond
                  ((eq 'symbol  name) '*gc-symbol*)
                  ((eq 'package name) '*gc-package*)
                  (t `(cvm-find-symbol ,(car type) 
                                       (cvm-find-package ,(cdr type)))))
                (cvm-structure-ref self 0))))
      ,@(loop
           for field in fields
           for index from 1
           append (let ((cst (intern (format nil "+~A-~A+" name field)))
                        (get (intern (format nil "CVM-~A-~A" name field)))
                        (set (intern (format nil "CVM-~A-SET-~A" name field))))
                    (list
                     `(defconstant ,cst ,index)
                     `(defun ,get (self) (cvm-structure-ref self ,cst))
                     `(defun ,set (self value)
                        (cvm-structure-store self ,cst value))
                     `(defsetf ,get ,set)))))))



;; Forward definitions because defsetf must be seen before use.

(cvm-define-structure symbol
    ("SYMBOL" . "COMMON-LISP")
  package name value function plist)


(cvm-define-structure package
    ("PACKAGE" . "COMMON-LISP")
  name nicknames symbols externals uses)


;;--------------------------
;; heap header
;;--------------------------

(cvm-define-structure hh
    ;; the only instance of this structure is stored in the common heap. 
    ("HEAP-HEADER" . "SYSTEM")
  size                                  ; ct-fixnum
  free-blocks                           ; ct-vector of ct-free-block
  root                             ; ct-nil or ct-address to a ct-cons
  new-generation              ; ct-nil or ct-address to a ct-vector-fp
  reserved)


;; We need to keep a new-generation list for partially allocated objects
;; until the value is stored in a symbol in a package in the root list.
;;
;; All blocks allocated by GC-ALLOCATE are pushed onto the new-generation
;; vector. new-generation is reset with GC-RESET-GENERATION.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ng-size+           31
    "Size of the new generation vector (max index = 30)")


  ;; free-blocks vector maximum:
  (defconstant +fb-big+            31
    "Index of big-block free-list (this is the max index for free-blocks).")

  ;; (aref free-blocks +fb-big+) == blocks of size > 256
  ;; (aref free-blocks k)        == blocks of size = (* 8 (+ 2 k))


  (defconstant +gc-heap-header+           2) ; cvm address
  (defconstant +gc-expected-free-block+  10) ; cvm address
  (defconstant +gc-start+                44) ; cvm address
  ;; cvm addresses are (integer 1 (1- *gc-heap-size*))
  ;; indexing the 64-bit cells of the common heap.

  );;eval-when

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMMON VIRTUAL MACHINE MEMORY (CVM)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   - primitive functions to work with shared memory values.
;;   : used by the shared memory garbage collector, and to manage the
;;     shared memory data structures (header, list of common symbols).
;;
;;     <typed-values> 64-bit
;;     (free-block size)
;;     null atom
;;     cons                                  --> <typed-value:address>
;;     car cdr setcar setcdr
;;     (make-vector element-type dimension)  --> <typed-value:address>
;;     (svref vector index)
;;     (svset vector index value)
;;     (make-structure  class nbfields)      --> <typed-value:address>
;;     (structure-ref   structure field-index)
;;     (structure-store structure field-index value)


;; Simple values are cells of 8 bytes:
;; 
;;     [free-block    s6 s5 s4 | s3 s2 s1 s0]
;;     [NIL            0  0  0 |  0  0  0  0]
;;     [T              0  0  0 |  0  0  0  1]
;;     [bit            0  0  0 |  0  0  0 {0,1}]
;;     [character-8    0  0  0 |  0  0  0 c0]
;;     [character-16   0  0  0 |  0  0 c1 c0]
;;     [character-24   0  0  0 |  0 c2 c1 c0]
;;     [character-32   0  0  0 | c3 c2 c1 c0]
;;     [{signed-byte,unsigned-byte,float}x[0,6]  v6 v5 v4 | v3 v2 v1 v0]
;; 
;; Reference values are addresses of the real value:
;;     
;;   [address  o6 o5 o4 | o3 o2 o1 o0] --> vector|vector-fp|array|structure|cons
;;   [readable o6 o5 o4 | o3 o2 o1 o0] --> string=(vector|vector-fp character)
;; 
;; readable is a special kind of value used to transmit other random
;; lisp values that are printable readably. 
;;     
;;   
;; Cons cells are 16-bytes double-cells. The type of the cons cell is 
;; that of the cdr+128:
;; 
;;     [cons+ cdr-2 | car-2 ]
;; 
;; Bigger values are greater multiples of 8-byte cells:
;; 
;; [[vector-data|structure-data] size
;; | [ element-type (T, unboxed-type) dimensions-1 ]
;; | data ]
;; 
;; first slot of structure is structure class (type of structure)
;; for symbols, the element-type is T and the structure class is NIL.
;;
;; [vector-fp-data size
;; | [ element-type (T, unboxed-type) dimensions-1 ]
;; | fill-pointer
;; | data ]
;; 
;; [array-data  size
;; | [ element-type (T, unboxed-type) ndimensions ]
;; | dimension_1 | ... | dimension_n
;; | data ]
;; 
;; symbol:
;; [structure-data 15 | [T,6] |NIL| package | name | value |NIL| plist ]
;; 
;; CL:SYMBOL:
;; [structure-data 15 | [T,6] |NIL| @'COMMON-LISP' | @'SYMBOL' |NIL|NIL|NIL]
;; 
;; 
;; cons+free-block
;; cons+nil
;; cons+t
;; cons+bit
;; cons+character-8
;; ...
;; cons+address
;; cons+readable



;; (defenum cell-type
;;   ct-address
;;   ct-readable
;;   ct-array
;;   ct-free-block
;;   ct-character
;;   ct-fixnum
;;   ct-float)
;; nil = (ct-address 0)
;; (defconstant ct-cons  128)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +address-cell-types+
    '(#.ct-address #.ct-readable))
  (defparameter +complex-element-types+
    '(#.ct-t #.ct-nil #.ct-address #.ct-readable)
    "array element types that are 'boxed'.")
  (defparameter +referenced-cell-types+
    '(#.ct-cons #.ct-structure #.ct-vector #.ct-vector-fp #.ct-array)
    "Cell types that can be referenced by a ct-address or ct-readable.")
  (defparameter +character-types+
    '(#.ct-character-8 #.ct-character-16 #.ct-character-24 #.ct-character-32))
  (defparameter +simple-types+
    '(#.ct-nil #.ct-t #.ct-bit #.ct-character-8
      #.ct-character-16 #.ct-character-24 #.ct-character-32
      #.ct-signed-byte-8 #.ct-signed-byte-16 #.ct-signed-byte-24
      #.ct-signed-byte-32 #.ct-signed-byte-40 #.ct-signed-byte-48
      #.ct-signed-byte-56 #.ct-signed-byte-64 #.ct-unsigned-byte-8
      #.ct-unsigned-byte-16 #.ct-unsigned-byte-24 
      #.ct-unsigned-byte-32 #.ct-unsigned-byte-40 
      #.ct-unsigned-byte-48 #.ct-unsigned-byte-56
      #.ct-unsigned-byte-64 #.ct-float-8 #.ct-float-16
      #.ct-float-24 #.ct-float-32 #.ct-float-40 #.ct-float-48 
      #.ct-float-56 #.ct-float-64))
  )


;; Mark Cons Type
;;   |MCTTTTTT|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|xxxxxxxx|
;;
;; ct-nil
;;   |M0000000|00000000|00000000|00000000|00000000|00000000|00000000|00000000|
;;
;; ct-t
;;   |M0000001|00000000|00000000|00000000|00000000|00000000|00000000|00000001|
;;
;; ct-free-block (next size)
;;   |M0000010|nnnnnnnn|nnnnnnnn|nnnnnnnn|ssssssss|ssssssss|ssssssss|ssssssss|
;;
;; ct-bit (bit)
;;   |M0000011|00000000|00000000|00000000|00000000|00000000|00000000|0000000b|
;;
;; ct-simples (value)
;;   |M0TTTTTT|vvvvvvvv|vvvvvvvv|vvvvvvvv|vvvvvvvv|vvvvvvvv|vvvvvvvv|vvvvvvvv|
;;
;; ct-address, ct-readable (address)
;;   |M0TTTTTT|aaaaaaaa|aaaaaaaa|aaaaaaaa|aaaaaaaa|aaaaaaaa|aaaaaaaa|aaaaaaaa|
;;
;; ct-cons (car cdr)
;;   |M1TTTTTT|aaaaaaaa|aaaaaaaa|aaaaaaaa|aaaaaaaa|aaaaaaaa|aaaaaaaa|aaaaaaaa|
;;   |M1TTTTTT|dddddddd|dddddddd|dddddddd|dddddddd|dddddddd|dddddddd|dddddddd|
;;
;; ct-structure, ct-vector, ct-vector-fp, ct-array (size element-type dimension)
;;   |M0TTTTTT|00000000|00000000|00000000|ssssssss|ssssssss|ssssssss|ssssssss|
;;   |00EEEEEE|dddddddd|dddddddd|dddddddd|dddddddd|dddddddd|dddddddd|dddddddd|
;;   |eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|
;;   | ...
;;   |eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|eeeeeeee|

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +type-code+ (byte  6 56))
  (defparameter +byte-56+   (byte 56  0))
  (defparameter +byte-32+   (byte 32  0))
  (defparameter +in-cons+   (byte  1 62))
  (defparameter +ex-cons+   (byte 62  0))
  (defparameter +in-mark+   (byte  1 63))
  (defparameter +ex-mark+   (byte 63  0))
  (defparameter +fb-size+   (byte 32  0))
  (defparameter +fb-next+   (byte 24 32))
  )
;; the +type-code+ must be defined before reading the next s-expression.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cvm-nil+       #.(dpb ct-nil +type-code+ 0)))


(defun gc-marked     (object) (plusp (ldb +in-mark+ object)))
(defun gc-set-mark   (object) (dpb 1 +in-mark+ object))
(defun gc-clear-mark (object) (ldb +ex-mark+ object))


(defun gc-reset-generation ()
  (let ((ng (cvm-hh-new-generation +gc-heap-header+)))
    (dotimes (i +ng-size+)
      (when-debug (:ng)
        (format t "ng:reset [~2D] ~16,'0X~%" i (cvm-svref ng i)))
      (cvm-svset ng i +cvm-nil+))
    (cvm-set-fill-pointer ng 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-with-generation (bindings body)
    (let ((sp (gensym)))
      `(let* ((,sp (cvm-fill-pointer (cvm-hh-new-generation +gc-heap-header+)))
              ,@bindings)
         (unwind-protect (progn ,@body)
           (loop with generation = (cvm-hh-new-generation +gc-heap-header+)
              for fp from (1- (cvm-fill-pointer generation)) downto ,sp
              do
              (when-debug (:ng)
                (format t "ng:pop   [~2D] ~16,'0X~%" 
                        fp  (cvm-svref generation fp)))
              (cvm-svset generation fp +cvm-nil+)
              finally (cvm-set-fill-pointer generation ,sp)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-generation (bindings &body body)
    (gen-with-generation bindings body)))


(defun gc-push-root (sym) 
  (with-generation  ((sym sym))
    (cvm-hh-set-root +gc-heap-header+
                     (cvm-make-cons sym (cvm-hh-root +gc-heap-header+)))))


(defun gc-delete-from-root (item)
  (cvm-hh-set-root +gc-heap-header+
                   (cvm-list-delete-eq (cvm-hh-root +gc-heap-header+) item)))

;; cvm-make-* calls gc-allocate and return an address.
;; cvm-form-* formats and returns and immediate object.


(defun cvm-eq (obj1 obj2) (= (ldb +ex-mark+ obj1) (ldb +ex-mark+ obj2)))
(defun cvm-form-head (type size)  (dpb type +type-code+  size))
(defun cvm-type-code (object)     (ldb +type-code+ object))


(defun cvm-type-of (object)
  (if (zerop (ldb +in-cons+ object))
      (ldb +type-code+ object)
      ct-cons))


(defun cvm-size-of (object)
  (case (cvm-type-of object)
    ((#.ct-free-block #.ct-vector #.ct-vector-fp #.ct-structure #.ct-array)
     (ldb +fb-size+ object)) ;; Warning: only 32-bit for size.
    ((#.ct-cons)  2)
    (otherwise    1)))


;;--------------------
;; free blocks
;;--------------------

;; [free-block    n2 n1 n0 | s3 s2 s1 s0]

(defun cvm-free-block-p (object) (eql (ldb +type-code+ object) ct-free-block))
(defun cvm-free-block-size (object)     (ldb +fb-size+ object))
(defun cvm-free-block-next (object)     (ldb +fb-next+ object))

(defun cvm-form-free-block (size next)  
  (dpb ct-free-block +type-code+ (dpb next +fb-next+ (ldb +fb-size+ size))))

(defun cvm-free-block-set-next (address new-next)
  (setf address (cvm-deref address))
  (gc-store address (cvm-form-free-block 
                     (cvm-free-block-size (gc-load address)) new-next)))

(defun cvm-free-block-set-size (address new-size)
  (setf address (cvm-deref address))
  (gc-store address (cvm-form-free-block 
                     new-size (cvm-free-block-next (gc-load address)))))


;;--------------------
;; addresses
;;--------------------

;; [address  o6 o5 o4 | o3 o2 o1 o0 ] --> vector|vector-fp|array|structure|cons
;; [readable o6 o5 o4 | o3 o2 o1 o0 ] --> string=(vector|vector-fp character)

(defun cvm-address-p  (object)  (= ct-address  (ldb +type-code+ object)))
(defun cvm-readable-p (object)  (= ct-readable  (ldb +type-code+ object)))
(defun cvm-form-address (address)
  (dpb ct-address  +type-code+ (ldb +byte-56+ address)))
(defun cvm-form-readable (address)
  (dpb ct-readable +type-code+ (ldb +byte-56+ address)))
(defun cvm-deref (object)      (ldb +byte-56+ object))



;;--------------------
;; booleans
;;--------------------


;; [NIL            0  0  0 |  0  0  0  0]
(defun cvm-null (object)       (eql (ldb +type-code+ object) ct-nil))



;; [T              0  0  0 |  0  0  0  1]
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cvm-t+         #.(dpb ct-t +type-code+ 1))
  );;eval-when

;; [unbound        0  0  0 |  0  0  0  0]

(defun cvm-unbound-p (object)  (eql (ldb +type-code+ object) ct-unbound))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cvm-unbound+   #.(dpb ct-unbound +type-code+ 0))
  );;eval-when

;;--------------------
;; bits
;;--------------------

;; [bit            0  0  0 |  0  0  0 {0,1}]

(defun cvm-bit-p     (object) (eql (ldb +type-code+ object) ct-bit))
(defun cvm-form-bit  (bit)    (dpb ct-bit +type-code+ (ldb (byte 1 0) bit)))
(defun cvm-bit-value (object) (ldb (byte 1 0) object))


;;--------------------
;; characters
;;--------------------

;; [character-8    0  0  0 |  0  0  0 c0]
;; [character-16   0  0  0 |  0  0 c1 c0]
;; [character-24   0  0  0 |  0 c2 c1 c0]
;; [character-32   0  0  0 | c3 c2 c1 c0]

(defun cvm-character-p (object)
  (<=  ct-character-8 (ldb +type-code+ object) ct-character-32))
(defun cvm-form-character (code)
  (dpb ct-character-32 +type-code+ (ldb +byte-32+ code)))
(defun cvm-character-code (object) (ldb +byte-32+ object))


;;--------------------
;; fixnums
;;--------------------

;; [{signed-byte,unsigned-byte}x[0,6]  v6 v5 v4 | v3 v2 v1 v0]

(defun cvm-fixnum-p (object)
  (or (<= ct-signed-byte-8   (ldb +type-code+ object) ct-signed-byte-56)
      (<= ct-unsigned-byte-8 (ldb +type-code+ object) ct-unsigned-byte-48)))
(defun cvm-form-fixnum (value)
  (unless (<= (- #.(expt 2 55)) value (1- #.(expt 2 55)))
    (error "Signed value too big."))
  (dpb ct-signed-byte-56 +type-code+ (ldb +byte-56+ value)))
(defun cvm-fixnum-value (object)
  (let ((value (ldb +byte-56+ object)))
    (if (< value #.(expt 2 55))
        value
        (- value #.(expt 2 56)))))


;;--------------------
;; floats
;;--------------------

;; [floatx[0,6]  v6 v5 v4 | v3 v2 v1 v0]

(defmacro gen-ieee-encoding (name type exponent-bits mantissa-bits)
  ;; Thanks to ivan4th (~ivan_iv@nat-msk-01.ti.ru) for correcting an off-by-1
  (wsiosbp
   `(progn
      (defun ,(intern (format nil "~A-TO-IEEE-754" name))  (float)
        (multiple-value-bind (mantissa exponent sign) 
            (integer-decode-float float)
          (dpb (if (minusp sign) 1 0)
               (byte 1 ,(1- (+ exponent-bits mantissa-bits)))
               (dpb (+ ,(+ (- (expt 2 (1- exponent-bits)) 2) mantissa-bits)
                       exponent)
                    (byte ,exponent-bits ,(1- mantissa-bits))
                    (ldb (byte ,(1- mantissa-bits) 0) mantissa)))))
      (defun ,(intern (format nil "IEEE-754-TO-~A" name))  (ieee)
        (let ((aval (scale-float
                     (coerce
                      (dpb 1 (byte 1 ,(1- mantissa-bits))
                           (ldb (byte ,(1- mantissa-bits) 0) ieee))
                      ,type)
                     (- (ldb (byte ,exponent-bits ,(1- mantissa-bits))
                             ieee) 
                        ,(1- (expt 2 (1- exponent-bits)))
                        ,(1- mantissa-bits)))))
          (if (zerop (ldb (byte 1 ,(1- (+ exponent-bits mantissa-bits))) ieee))
              aval
              (- aval)))))))


(gen-ieee-encoding float-32 'single-float  8 24)
(gen-ieee-encoding float-64 'double-float 11 53)


(defun test-ieee-read-double ()
  (with-open-file (in "value.ieee-754-double" 
                      :direction :input :element-type '(unsigned-byte 8))
    (loop while (< (file-position in) (file-length in))
       do (loop repeat 8 for i = 1 then (* i 256)
             for v = (read-byte in) then (+ v (* i (read-byte in))) 
             finally (progn
                       (let ((*print-base* 16)) (princ v))
                       (princ " ")
                       (princ (IEEE-754-TO-FLOAT-64 v))
                       (terpri))))))

(defun test-ieee-read-single ()
  (with-open-file (in "value.ieee-754-single" 
                      :direction :input :element-type '(unsigned-byte 8))
    (loop while (< (file-position in) (file-length in))
       do (loop repeat 4 for i = 1 then (* i 256)
             for v = (read-byte in) then (+ v (* i (read-byte in))) 
             finally (progn
                       (let ((*print-base* 16)) (princ v))
                       (princ " ")
                       (princ (IEEE-754-TO-FLOAT-32 v))
                       (terpri))))))

(defun test-single-to-ieee (&rest args)
  (dolist (arg args)
    (format t "~16,8R ~A~%" 
            (float-32-to-ieee-754 (coerce arg 'single-float)) arg)))

(defun test-double-to-ieee (&rest args)
  (dolist (arg args)
    (format t "~16,16R ~A~%" 
            (float-64-to-ieee-754 (coerce arg 'double-float)) arg)))


#|
CL-USER> (test-double-to-ieee 1.2d0 12.0d0 120.0d0 1200.0d0 1234.567d0)
3FF3333333333333 1.2d0
4028000000000000 12.0d0
405E000000000000 120.0d0
4092C00000000000 1200.0d0
40934A449BA5E354 1234.567d0
NIL
CL-USER> (test-ieee-read-double)
3FF3333333333333 1.2d0
4028000000000000 12.0d0
405E000000000000 120.0d0
4092C00000000000 1200.0d0
40934A449BA5E354 1234.567d0
NIL
CL-USER> (test-ieee-read-single)
3F99999A 1.2
41400000 12.0
42F00000 120.0
44960000 1200.0
449A5225 1234.567
NIL
CL-USER> (test-single-to-ieee 1.2 12.0 120.0 1200.0 1234.567)
3F99999A 1.2
41400000 12.0
42F00000 120.0
44960000 1200.0
449A5225 1234.567
NIL
CL-USER> 
|#


(defun cvm-single-float-p (object)
  (= ct-float-32  (ldb +type-code+ object)))
(defun cvm-form-single-float (value)
  (dpb ct-float-32 +type-code+ (float-32-to-ieee-754
                                (coerce value 'single-float))))
(defun cvm-single-float-value (object)
  (ieee-754-to-float-32 (ldb +byte-32+ object)))

;; TODO: double-float ==> consing


;;--------------------
;; conses
;;--------------------

;;     cons                                  --> <typed-value:address>
;;     car cdr setcar setcdr

(defun cvm-cons-p (cons) 
  ;; a ct-address to a ct-cons
  (and (cvm-address-p cons)
       (plusp (ldb +in-cons+ (gc-load (cvm-deref cons))))))


(defun cvm-make-cons (car-object cdr-object)
  (let* ((address (gc-allocate 2))
         (gc-addr (cvm-deref address)))
    (gc-store gc-addr      (dpb 1 +in-cons+ cdr-object))
    (gc-store (1+ gc-addr) (dpb 1 +in-cons+ car-object))
    (when-debug (:objects) (format t "~&object: ~16,'0X CONS~%" address))
    address))


(defun cvm-car (cons)
  (cond
    ((cvm-null cons)  ct-nil)
    ((cvm-cons-p cons)
     (ldb +ex-cons+ (gc-load (1+ (cvm-deref cons)))))
    (t (error "CVM-CAR of non-list object: ~A"
              (with-output-to-string (out)
                (gc-dump-object cons :stream out))))))


(defun cvm-cdr (cons)
  (cond
    ((cvm-null cons)  ct-nil)
    ((cvm-cons-p cons)
     (ldb +ex-cons+ (gc-load (cvm-deref cons))))
    (t (error "CVM-CDR of non-list object: ~A"
              (with-output-to-string (out)
                (gc-dump-object cons :stream out))))))


(defun cvm-setcar (cons value)
  (cond
    ((cvm-null cons)
     (error "CVM-SETCAR of NIL"))
    ((cvm-cons-p cons)
     (gc-store (1+ (cvm-deref cons)) (dpb 1 +in-cons+ value)))
    (t (error "CVM-SETCAR of non-list object: ~A"
              (with-output-to-string (out)
                (gc-dump-object cons :stream out))))))


(defun cvm-setcdr (cons value)
  (cond
    ((cvm-null cons)
     (error "CVM-SETCDR of NIL"))
    ((cvm-cons-p cons)
     (gc-store (cvm-deref cons) (dpb 1 +in-cons+ value)))
    (t (error "CVM-SETCDR of non-list object: ~A"
              (with-output-to-string (out) 
                (gc-dump-object cons :stream out))))))


;;--------------------
;; lists
;;--------------------

(defun cvm-list-length (list)
  (loop for curr = list then (cvm-cdr curr) until (cvm-null curr) count 1))


(defun cvm-list-elt (list index)
  (loop for curr = list then (cvm-cdr curr)
        repeat index
        finally (return (cvm-car curr))))


(defun cvm-member-eq (item list)
  (loop for curr = list then (cvm-cdr curr)
        until (or (cvm-null curr) (eql (cvm-car curr) item))
        finally (return curr)))


(defun cvm-list-nreverse (list)
  (loop for prev = +cvm-nil+ then curr
        for curr = list then next
        for next = (cvm-cdr curr)
        until (cvm-null curr)
        do (cvm-setcdr curr prev)
        finally (return prev)))


(defun cvm-list-delete-eq (list item)
  ;; list is a ct-nil or ct-address object.
  ;; item is a simple ct-* or a ct-address.
  (loop for prev = nil  then curr
        for curr = list then (cvm-cdr curr)
        until (or (cvm-null curr) (eql (cvm-car curr) item))
        finally (return (cond
                         ((cvm-null curr) #| no item |# 
                          list #| unchanged |#)
                         ((null prev) #| deleting the first item |#
                          (cvm-cdr list))
                         (t
                          (cvm-setcdr prev (cvm-cdr curr))
                          list)))))


(defun cvm-make-list (&key length initial-element initial-contents)
  (setf length (or length (length initial-contents)))
  (cond
    ((zerop length) +cvm-nil+)
    (initial-contents
     (loop with head = (cvm-make-cons (car initial-contents) +cvm-nil+)
           with tail = head
           with new = nil
           for item in (cdr initial-contents)
           repeat length
           do (progn
                (setf new (cvm-make-cons item +cvm-nil+))
                (cvm-setcdr tail new)
                (setf tail new))
           finally (return head)))
    (t
     (loop with head = (cvm-make-cons initial-element +cvm-nil+)
           with tail = head
           with new = nil
           repeat length
           do (progn
                (setf new (cvm-make-cons initial-element +cvm-nil+))
                (cvm-setcdr tail new)
                (setf tail new))
           finally (return head)))))


;; TODO: implement cvm-push and cvm-pop properly!
(defmacro cvm-push (item list)
  (let ((vlist (gensym))
        (vitem (gensym)))
    `(with-generation
         ((,vlist ,list)
          (,vitem ,item))
       (setf ,list (cvm-make-cons ,vitem ,vlist)))))
 

(defmacro cvm-pop (list)
  `(prog1 (cvm-car ,list) (setf ,list  (cvm-cdr ,list))))


(defmacro cvm-dolist ((var list &optional (result nil)) &body body)
  (let ((vlist (gensym)))
    `(do* ((,vlist ,list (cvm-cdr ,vlist))
           (,var (cvm-car ,vlist)(cvm-car ,vlist)))
         ((cvm-null ,vlist) ,result) ,@body)))


;;--------------------
;; vector, vector-fp, structure, array:
;;--------------------

(defun bits-to-words (bits)  "64 bit/word"  (truncate (+ 63 bits) 64))

(defun cvm-bit-size-of-unboxed-type (typecode)  
  (let ((size (if (<= ct-nil typecode ct-readable)
                  (aref #(
                          0             ; ct-nil
                          64            ; ct-t
                          64            ; ct-free-block
                          1             ; ct-bit
                          8             ; ct-character-8
                          16            ; ct-character-16
                          32            ; ct-character-24
                          32            ; ct-character-32
                          8             ; ct-signed-byte-8
                          16            ; ct-signed-byte-16
                          32            ; ct-signed-byte-24
                          32            ; ct-signed-byte-32
                          64            ; ct-signed-byte-40
                          64            ; ct-signed-byte-48
                          64            ; ct-signed-byte-56
                          64            ; ct-signed-byte-64
                          8             ; ct-unsigned-byte-8
                          16            ; ct-unsigned-byte-16
                          32            ; ct-unsigned-byte-24
                          32            ; ct-unsigned-byte-32
                          64            ; ct-unsigned-byte-40
                          64            ; ct-unsigned-byte-48
                          64            ; ct-unsigned-byte-56
                          64            ; ct-unsigned-byte-64
                          8             ; ct-float-8
                          16            ; ct-float-16
                          32            ; ct-float-24
                          32            ; ct-float-32
                          64            ; ct-float-40
                          64            ; ct-float-48
                          64            ; ct-float-56
                          64            ; ct-float-64
                          -1            ; ct-structure
                          -1            ; ct-vector
                          -1            ; ct-vector-fp
                          -1            ; ct-array
                          64            ; ct-address
                          64            ; ct-readable
                          ) typecode) 
                  -1)))
    (when (minusp size)
      (error "CVM-BIT-SIZE-OF-UNBOXED-TYPE: invalid typecode ~D (~A)"
             typecode (cell-type-label typecode)))
    size))

  
;; [ct-vector size
;; | [ element-type (T, unboxed-type) dimensions-1 ]
;; | data ]
;;
;; [ct-structure size
;; | ct-t dimensions-1 = nfields+1
;; | class             = field 0
;; | data ]            = field 1 .. nfields
;; 
;; [ct-vector-fp size
;; | element-type (T, unboxed-type) dimensions-1
;; | fill-pointer
;; | data ]
;; 
;; [ct-array  size
;; | element-type (T, unboxed-type) ndimensions
;; | dimension_1 | ... | dimension_n
;; | data ]
;; 
;; symbol:
;; [structure-data 15 | [T,6] |NIL| package | name | value |NIL| plist ]
;; 
;; CL:SYMBOL:
;; [structure-data 15 | [T,6] |NIL| @'COMMON-LISP' | @'SYMBOL' |NIL|NIL|NIL]


(defun cvm-initialize-vector (address cell-type element-type dimension
                                      &key fill-pointer class)
  (let* ((address (cvm-deref address))
         (ptr address))
    (gc-store ptr (cvm-form-head cell-type 
                                 (cvm-free-block-size (gc-load address))))
    (gc-store (incf ptr) (cvm-form-head element-type dimension))
    (when fill-pointer
      (gc-store (incf ptr) fill-pointer))
    (when class
      (gc-store (incf ptr) class))
    (dotimes (i (- (cvm-size-of (gc-load address)) (- ptr address) 1))
      (gc-store (incf ptr) +cvm-nil+)))
  address)
      


(defun cvm-vector-p       (address)
  (and (not (cvm-null address))
       (= (ldb +type-code+ (gc-load (cvm-deref address))) ct-vector)))
(defun cvm-vector-fp-p    (address)
  (and (not (cvm-null address))
       (= (ldb +type-code+ (gc-load (cvm-deref address))) ct-vector-fp)))
(defun cvm-structure-p    (address)
  (and (not (cvm-null address))
       (= (ldb +type-code+ (gc-load (cvm-deref address))) ct-structure)))
(defun cvm-array-p        (address)
  (and (not (cvm-null address))
       (= (ldb +type-code+ (gc-load (cvm-deref address))) ct-array)))


(defun cvm-element-type (address)
  (cvm-type-of      (gc-load (1+ (cvm-deref address)))))

(defun cvm-length       (address)
  (cvm-fixnum-value (gc-load (1+ (cvm-deref address)))))


(defun cvm-rows (address)
  (let ((address (cvm-deref address)))
    (case (cvm-type-of (gc-load address))
      ((#.ct-vector #.ct-structure)  
       (values  (cvm-fixnum-value (gc-load (incf address))) 
                (incf address)))
      ((#.ct-vector-fp)
       (values  (cvm-fixnum-value (gc-load (incf address))) 
                (incf address 2)))
      ((#.ct-array)
       (values (loop repeat (cvm-fixnum-value (gc-load (incf address)))
                  for row-dimension = 1 
                  then (* row-dimension 
                          (cvm-fixnum-value (gc-load (incf address))))
                  finally (return row-dimension))
               (incf address)))
      (otherwise (error "CVM-ROWS: bad argument type ~A (~D)" 
                        (cell-type-label (cvm-type-code (gc-load address)))
                        (cvm-type-code (gc-load address)))))))


(defun cvm-set-fill-pointer (address fill-pointer)
  (setf address (cvm-deref address))
  (assert (= ct-vector-fp (cvm-type-of (gc-load address))))
  (gc-store (+ 2 address) (cvm-form-fixnum fill-pointer)))


(defun cvm-fill-pointer (address)
  (setf address (cvm-deref address))
  (assert (= ct-vector-fp (cvm-type-of (gc-load address))))
  (cvm-fixnum-value (gc-load (+ 2 address))))


(defsetf cvm-fill-pointer cvm-set-fill-pointer)


(defun cvm-make-vector (element-type dimension)
  (let ((address (gc-allocate
                  (+ 2 (bits-to-words
                        (* (cvm-bit-size-of-unboxed-type element-type) 
                           dimension))))))
    (cvm-initialize-vector address ct-vector element-type dimension)
    (when-debug (:objects) (format t "~&object: ~16,'0X VECTOR~%" address))
    address))


(defun cvm-make-vector-fp (element-type dimension &key (fill-pointer +cvm-nil+))
  (let ((address (gc-allocate
                  (+ 3 (bits-to-words
                        (* (cvm-bit-size-of-unboxed-type element-type)
                           dimension))))))
    (cvm-initialize-vector address ct-vector-fp element-type dimension
                           :fill-pointer fill-pointer)
    (when-debug (:objects) (format t "~&object: ~16,'0X VECTOR+FP~%" address))
    address))


(defun cvm-vector-push (new-element vector)
  (unless (cvm-vector-fp-p vector)
    (error 'type-error :datum vector :expected-type 'ct-vector-fp))
  (let ((fill-pointer (cvm-fill-pointer vector)))
    (when (< fill-pointer (cvm-length vector))
      (progn (cvm-svset vector fill-pointer new-element)
             (cvm-set-fill-pointer vector (1+ fill-pointer))
             new-element))))


(defun cvm-vector-pop (vector)
  (let ((fill-pointer (cvm-fill-pointer vector)))
    (when (zerop fill-pointer)
      (error "CVM-VECTOR-POP: the fill pointer of the vector ~X is 0." vector))
    (cvm-svref vector fill-pointer)))


(defun cvm-make-structure (class nbfields)
  (let ((address (gc-allocate (+ 3 nbfields))))
    (cvm-initialize-vector address ct-structure ct-t 
                           (1+ nbfields) :class class)
    (when-debug (:objects) (format t "~&object: ~16,'0X STRUCTURE~%" address))
    address))



;; symbol:
;; [structure-data 15 | [T,6] |NIL| package | name | value |NIL| plist ]
;; 
;; CL:SYMBOL:
;; [structure-data 15 | [T,6] |NIL| @'COMMON-LISP' | @'SYMBOL' |NIL|NIL|NIL]


(defun cvm-svoperate (address index operation &optional value)
  (let ((element-size (cvm-bit-size-of-unboxed-type 
                       (cvm-element-type address))))
    (multiple-value-bind (dimension address) (cvm-rows address)
      (unless (<= 0 index (1- dimension))
        (error "SVREF: index out of bounds ~D [0,~D[" index dimension))
      ;; element-size: 1 8 16 32 64
      (if (= 1 element-size)
          (let* ((opaddr (+ *gc-heap-base* (* 8 (+ address (/ index 64)))))
                 (opofst (mod index 64))
                 (word (funcall (gc-peek-function 64) opaddr)))
            (if (eq operation :peek) 
                (ldb (byte 1 opofst) word)
                (funcall (gc-poke-function 64) opaddr
                         (dpb value (byte 1 opofst) word))))
          (let ((opaddr (case element-size
                          ((8)  (+ *gc-heap-base* (* 8 address ) index))
                          ((16) (+ *gc-heap-base* (* 8 address ) (* 2 index)))
                          ((32) (+ *gc-heap-base* (* 8 address ) (* 4 index)))
                          ((64) (+ *gc-heap-base* (* 8 (+ address index))))
                          (otherwise "CVM-SVOPERATE: bad element-size ~D" 
                                     element-size))))
            (if (eq operation :peek)
                (funcall (gc-peek-function element-size) opaddr)
                (funcall (gc-poke-function element-size) opaddr value)))))))


;;     (svref vector index)
;;     (svset vector index value)

(defun cvm-svref (address index)
  (cvm-svoperate address index :peek))
(defun cvm-svset (address index value) 
  (cvm-svoperate address index :poke value))
(defun cvm-structure-ref   (address index)
  (cvm-svoperate address index :peek))
(defun cvm-structure-store (address index value)
  (cvm-svoperate address index :poke value))



;; --------------------
;; strings
;; --------------------

(defun cvm-make-string (&key contents type length fill-pointer)
  (setf length (or length (length contents)))
  (let* ((chartype (cond
                     ((null contents)            (or type ct-character-32))
                     ((zerop (length contents))  ct-character-8)
                     (t  (loop
                            :for ch :across contents
                            :maximize (char-code ch) :into max
                            :finally (return
                                      (cond 
                                        ((< max 256)      ct-character-8)
                                        ((< max 65536)    ct-character-16)
                                        ((< max 16777216) ct-character-24)
                                        (t                ct-character-32)))))))
         (string (if fill-pointer
                     (cvm-make-vector-fp chartype length)
                     (cvm-make-vector    chartype length))))
    (when fill-pointer
      (cvm-set-fill-pointer string fill-pointer))
    (when contents
      (loop for i from 0 below length 
         do (cvm-svset string i (char-code (aref contents i)))))
    (when-debug (:objects) (format t "~&object: ~16,'0X STRING ~S~%"
                                   string (cvm-string-value string)))
    string))


(defun cvm-string-p (address)
  (and (or (cvm-vector-p address) (cvm-vector-fp-p address))
       (member (cvm-element-type address) +character-types+)))


(defun cvm-string= (stra strb)
  (assert (cvm-string-p stra))
  (assert (cvm-string-p strb))
  (and (= (cvm-length stra) (cvm-length strb))
       (loop for i from 0 below (cvm-length stra)
          do (unless (= (cvm-svref stra i) (cvm-svref strb i))
               (return-from cvm-string= nil))
          finally (return t))))


(defun cvm-string-value (address)
  (assert (cvm-string-p address))
  (loop with string = (if (cvm-vector-fp-p address)
                          (make-array (list (cvm-length address))
                                      :element-type 'character
                                      :initial-element (character " ")
                                      :fill-pointer (cvm-fill-pointer address))
                          (make-string (cvm-length address)))
     for i from 0 below (cvm-length address)
     do (setf (aref string i) (code-char (cvm-svref address i)))
     finally (return string)))


;; --------------------
;; symbols
;; --------------------


(defun cvm-make-symbol (name package &key (value +cvm-unbound+)
                        (function +cvm-unbound+) (plist +cvm-nil+))
  (let (common-lisp symbol sym)
    (if (cvm-null *gc-symbol*)
        (setf common-lisp (cvm-find-package "COMMON-LISP")
              symbol      (cvm-find-symbol "SYMBOL" common-lisp))
        (setf common-lisp (cvm-symbol-package *gc-symbol*)
              symbol      *gc-symbol*))
    (setf sym (cvm-make-structure symbol 5))
    (cvm-symbol-set-package  sym package)
    (cvm-symbol-set-name     sym name)
    (cvm-symbol-set-value    sym value)
    (cvm-symbol-set-function sym function)
    (cvm-symbol-set-plist    sym plist)
    (when-debug (:objects) (format t "~&object: ~16,'0X SYMBOL ~S~%"
                                   sym (cvm-string-value name)))
    sym))


(defun cvm-symbol-bound-p  (sym)
  (not (cvm-unbound-p (cvm-symbol-value sym))))

(defun cvm-symbol-fbound-p (sym) 
  (not (cvm-unbound-p (cvm-symbol-function sym))))

(defun cvm-symbol-makunbound (sym)
  (cvm-symbol-set-value sym +cvm-unbound+))

(defun cvm-symbol-fmakunbound (sym)
  (cvm-symbol-set-function sym +cvm-unbound+))

(defgeneric cvm-find-symbol (name package)
  (:method ((name/symbol string) (name/package string))
    (cvm-find-symbol (cvm-make-string :contents name/symbol)
                     (cvm-find-package name/package)))
  (:method ((name/symbol string) (package integer))
    (cvm-find-symbol (cvm-make-string :contents name/symbol) package))
  (:method ((name integer) (name/package string))
    (cvm-find-symbol name (cvm-find-package name/package)))
  (:method ((name integer) (package integer))
    (cvm-dolist (sym  (cvm-package-symbols package) nil)
      (when (cvm-string= name (cvm-symbol-name sym))
        (return-from cvm-find-symbol (values sym :external))))))


(defgeneric cvm-intern (name package)
  (:method ((name integer) (package integer))
    (multiple-value-bind (sym status) (cvm-find-symbol name package)
      (if sym
          (values sym status)
          (let ((sym (cvm-make-symbol name package)))
            (cvm-push sym (cvm-package-symbols package))
            (values sym nil))))))


;;--------------------
;; packages
;;--------------------


(defun cvm-make-package (name &key (nicknames +cvm-nil+) (uses +cvm-nil+))
  (let* ((common-lisp (if (cvm-null *gc-symbol*)
                          (cvm-find-package "COMMON-LISP")
                          (cvm-symbol-package *gc-symbol*)))
         (package     (if (cvm-null *gc-package*)
                          (cvm-find-symbol "PACKAGE" common-lisp)
                          *gc-package*))
         (pack (cvm-make-structure package 5)))
    (cvm-package-set-name      pack name)
    (cvm-package-set-nicknames pack nicknames)
    (cvm-package-set-uses      pack uses)
    (gc-push-root pack)
    (when-debug (:objects) (format t "~&object: ~16,'0X PACKAGE ~S~%"
                                   pack (cvm-string-value name)))
    pack))


(defmacro %and (&rest conjonctions)
  `(and ,@(mapcar (lambda (conj) 
                    (let ((vconj (gensym)))
                      `(let ((,vconj ,conj))
                         (format t "~S ==> ~A~%" ',conj ,vconj)
                         ,vconj)))  conjonctions)))


(defgeneric cvm-find-package (name)
  (:method ((name/package string))
    (cvm-find-package (cvm-make-string :contents name/package)))
  (:method ((name integer))
    (flet ((%package-p 
               (self)
             (and (cvm-structure-p self)
                  (= 6 (cvm-length self))
                  (= ct-t (cvm-element-type self))
                  (let ((stype  (cvm-structure-ref self 0)))
                    (and (cvm-structure-p stype)
                         (= 6 (cvm-length stype))
                         (= ct-t (cvm-element-type stype))
                         (if (cvm-null *gc-package*)
                             (and
                              (string= "PACKAGE" 
                                       (cvm-string-value (cvm-symbol-name stype)))
                              (string= "COMMON-LISP" 
                                       (cvm-string-value 
                                        (cvm-package-name
                                         (cvm-symbol-package stype)))))
                             (= *gc-package* stype)))))))
      (cvm-dolist (item (cvm-hh-root +gc-heap-header+) +cvm-nil+)
        ;; (format t "~A P/~A~%" (cvm-string-value (cvm-package-name item))
        ;;        (%package-p item))
        (when (and (%package-p item)
                   (cvm-string= name (cvm-package-name item)))
          (return-from cvm-find-package item))))))
        

(defgeneric cvm-delete-package (package)
  (:method ((name/package string))
    (cvm-delete-package (cvm-make-string :contents name/package)))
  (:method ((package integer))
    (if (or (eql package (cvm-find-package "COMMON-LISP"))
            (eql package (cvm-find-package "SYSTEM"))
            (not (cvm-member-eq package (cvm-hh-root +gc-heap-header+))))
        nil
        (progn
          (loop for symbols = (cvm-package-symbols package) then (cvm-cdr symbols)
             do (cvm-symbol-set-package (cvm-car symbols) +cvm-nil+))
          (gc-delete-from-root package)
          t))))


(defun cvm-list-all-packages ()
  (loop for items = (cvm-hh-root +gc-heap-header+) then (cvm-cdr items)
     until (cvm-null items)
     when (cvm-package-p (cvm-car items)) collect (cvm-car items)))


;;--------------------
;; dump & check
;;--------------------

(defun gc-dump-object (object &key (stream *standard-output*) (comment "")
                              (address nil))
  (declare (ignore comment))
  (setf address (and address (cvm-deref address)))
  (format stream 
    "~A~16,'0X  M/~1D C/~1D T/~2D V/~17,'0D ~A~A~%"
    (if address (format nil "~8,'0X: " address) "") object
    (ldb +in-mark+ object)   (ldb +in-cons+ object)
    (ldb +type-code+ object) (ldb +byte-56+ object)
    (cell-type-label (ldb +type-code+ object)) 
    "" #| comment |#))


(defun gc-dump-cell (address &key (stream *standard-output*) (contents t)
                             (margin ""))
  (setf address (cvm-deref address))
  (flet ((dump
             (address object &optional (comment ""))
           (gc-dump-object object :stream stream :comment comment
                           :address address))
         (dump-free
             (address object &optional (comment ""))
           (declare (ignore comment))
           (format stream 
             "~8,'0X: ~16,'0X  M/~1D C/~1D T/~2D S/~7,'0D N/~7,'0D ~A~A~%"
             address object
             (ldb +in-mark+ object)   (ldb +in-cons+ object)
             (ldb +type-code+ object) 
             (ldb +fb-size+ object) (ldb +fb-next+ object)
             (cell-type-label (ldb +type-code+ object)) 
             "" #| comment |#)))
    (if (zerop address)
        (format stream "~8,'0X = NIL~%"  0)
        (let ((object (gc-load address)))
          (case  (cvm-type-of object)
            ((#.ct-cons)
             (dump      address  (gc-load     address ) "[CDR]")
             (format stream "~A" margin)           
             (dump  (1+ address) (gc-load (1+ address)) "[CAR]"))
            ((#.ct-free-block)
             (dump-free address  (gc-load     address ) "[tc|sz]"))
            ((#.ct-structure #.ct-vector #.ct-vector-fp #.ct-array)
             (dump      address  (gc-load     address ) "[tc|sz]")
             (format stream "~A" margin)           
             (dump  (1+ address) (gc-load (1+ address)) "[el|dm]")
             (if (= ct-t (cvm-type-of (gc-load (1+  address))))
                 (when contents
                   (loop for address from (+ 2 address)
                      repeat (- (cvm-size-of object) 2) do
                      (format stream "~A" margin)
                      (gc-dump-cell address :Stream stream :margin margin)))
                 (gc-dump-block (+ 2 address) (- (cvm-size-of object) 2)
                                stream :margin margin)))
            (otherwise
             (dump      address  (gc-load     address ))))))))


(defun gc-dump-root (&key (stream *standard-output*)) 
  (cvm-dolist (e (cvm-hh-root +gc-heap-header+)) 
    (gc-dump-cell e)
    (princ (cvm-string-value (cvm-package-name e)) stream)
    (terpri stream) (terpri stream)
    (unless (cvm-null (cvm-structure-ref e 0))
      (gc-dump-cell (cvm-structure-ref e 0))
      (princ (cvm-string-value
              (cvm-symbol-name (cvm-structure-ref e 0))) stream)
      (terpri stream)(terpri stream))
    (terpri stream)))


(defmacro cvm-do-symbols ((vsym package) &body body)
  (let ((vpack (gensym)) (epack (gensym)))
    `(let ((,vpack ,package)(,epack))
       (cond
         ((stringp ,vpack)      (setf ,epack (cvm-find-package ,vpack)))
         ((cvm-string-p ,vpack) (setf ,epack (cvm-find-package
                                              (cvm-string-value ,vpack))))
         (t (setf ,epack ,vpack)))
       (unless (cvm-null ,epack) (error "Inexistent package ~A" ,vpack))
       (cvm-dolist (,vsym (cvm-package-symbols ,vpack)) ,@body))))


;;--------------------
;; We use this bitmap only to check the heap, not for the garbage collection.
;;--------------------


(defun gc-make-bitmap () 
  (make-array (list *gc-heap-size*)
              :element-type '(unsigned-byte 2) :initial-element 0))


(defun print-bitmap (bitmap &key (stream *standard-output*)
                     (start 0) (end (length bitmap)))
  (loop for i from start below end
     for c = (aref bitmap i) 
     initially (unless (zerop (mod i 64)) (format stream "~&~8,'0X: " i))
     do
     (when (zerop (mod i 64))  (format stream "~&~8,'0X: " i))
     (princ (aref "GFAV" c) stream)
     finally (terpri stream)))


(defun gc-bitmap-set-free (bitmap address)
  (assert (zerop (aref bitmap address)))
  (setf (aref bitmap address) 1))

(defun gc-bitmap-set-allocated (bitmap address)
  (assert (zerop (aref bitmap address)))
  (setf (aref bitmap address) 2))

(defun gc-bitmap-set-allocated-range (bitmap address size)
  (loop repeat size
     for i from address do
     (assert (evenp (aref bitmap i)))   ; 0 or 2
     (setf (aref bitmap i) 2)))

(defun gc-bitmap-set-visited (bitmap address)
  (assert (= 2 (aref bitmap address)))
  (setf (aref bitmap address) 3))

(defun gc-bitmap-set-visited-range (bitmap address size)
  (loop repeat size
     for i from address do
     (assert (= 2 (aref bitmap i)))
     (setf (aref bitmap i) 3)))

(defun gc-bitmap-clear-p     (bitmap address) (=  0 (aref bitmap address)))
(defun gc-bitmap-free-p      (bitmap address) (=  1 (aref bitmap address)))
(defun gc-bitmap-allocated-p (bitmap address) (<= 2 (aref bitmap address)))
(defun gc-bitmap-visited-p   (bitmap address) (=  3 (aref bitmap address)))


;;--------------------
;; checking the heap
;;--------------------


(defun gc-check-free-block-list (fbln fbl &key (stream *standard-output*)
                                 (dump-free nil) (bitmap nil))
  "
RETURN: the number of blocs, the total size of blocks.
RAISE:  simple-error conditions when an invariant is invalidated.
"
  (assert (or (cvm-null fbl) (cvm-address-p fbl)))
  (when dump-free
    (format stream "  [~2,'0X] " fbln)
    (gc-dump-cell (cvm-deref fbl)))
  (if (cvm-address-p fbl)
      (loop 
         for count from 0
         for size = 0
         for fbp = (cvm-deref fbl) then (cvm-free-block-next fb)
         for fb = (when (plusp fbp) (gc-load fbp))
         while (plusp fbp)
         do (progn
              (when dump-free
                (gc-dump-cell fbp :stream stream))
              (unless (cvm-free-block-p fb)
                (gc-dump-cell fbl)
                (assert nil ()
                        "~D~[th~;st~;nd~:;th~] free block in ~
                           free block list ~D is not a free-block ~
                           record at #x~8,'0X~%  It's ~16,'0X (~A)~%"
                        count (mod count 10) fbln (cvm-deref fbl)
                        fb (cell-type-label (cvm-type-of fb))))
              (assert (<= 1 (cvm-free-block-size fb) (- *gc-heap-size* fbp)))
              ;; Most free blocs are (<= 2 size) but un-linked free-blocks
              ;; of size 1 can happen.
              (incf size (cvm-free-block-size fb))
              ;; check the free cells are not already taken, and mark them:
              (when bitmap
                (when-debug (:bitmap)
                  (format t "fill bitmap free from ~X to ~X~%"
                          fbp (+ fbp (cvm-free-block-size fb))))
                (loop for i from fbp below (+ fbp (cvm-free-block-size fb))
                   do (gc-bitmap-set-free bitmap i))))
         finally (values count size))
      (values 0 0)))


(defun gc-check-tree (root-address &key (stream *standard-output*)
                      (allow-free-blocks nil) (bitmap nil)
                      (dump-allocated nil) (dump-data nil))
  ;; root-address is a gc-address
  (loop 
     with  referenced = (if allow-free-blocks
                            (cons ct-free-block +referenced-cell-types+)
                            +referenced-cell-types+)
     with  bitmap     = (or bitmap (gc-make-bitmap))
     with  addresses  = (list root-address)
     for   address    = (pop addresses)
     for   object     = (when address (gc-load address))
     while address
     do
     (when-debug (:gcct)
       (let ((*print-base* 16))(format t "  gcct: addresses = ~S~%" addresses))
       (format t "  gcct: address   = ~X~%" address))
     (assert (< 0 address *gc-heap-size*))
     (assert (gc-bitmap-allocated-p bitmap address))
     (when-debug (:gc)
       (when (or (cvm-address-p address) (cvm-readable-p address))
         (gc-dump-cell (cvm-deref address) :stream *trace-output* :contents nil)))
     (unless (gc-bitmap-visited-p   bitmap address)
       (when-debug (:gcct) (format t "  gcct:   was not already visited~%"))
       (flet ((follow
                  (addrobj)
                (let ((address (cvm-deref addrobj)))
                  (when-debug (:gcct)
                    (format t "  gcct:   follow ~16,'0X M/~A V/~A~%" addrobj
                            (member (cvm-type-code addrobj) +address-cell-types+)
                            (when (member (cvm-type-code addrobj)
                                          +address-cell-types+)
                              (not (gc-bitmap-visited-p bitmap address)))))
                  (when (and (member (cvm-type-code addrobj) +address-cell-types+)
                             (not (gc-bitmap-visited-p bitmap address)))
                    (let ((object (gc-load address)))
                      (assert (member (cvm-type-of object) referenced))
                      (gc-bitmap-set-allocated-range bitmap
                                                     address (cvm-size-of object))
                      (push address addresses)
                      (when-debug (:gcct)
                        (format t "  gcct:   new addresses = ~S~%" addresses)))))))
         (when-debug (:gcct) (format t "  gcct:   object=~X~%" object))
         (case (cvm-type-of object)
           ;; -------------------- free block
           ((#.ct-free-block)           ; Bad!
            (unless allow-free-blocks
              (assert nil () 
                      "Found a free-block in allocated data at #x~8,'0X" 
                      address)))
           ;; -------------------- address & readable 
           ((#.ct-address #.ct-readable)
            (when (or dump-allocated dump-data)
              (gc-dump-cell address :stream stream))
            (push (cvm-deref object) addresses)
            (gc-bitmap-set-visited bitmap address)) ; size = 1
           ;; -------------------- simple cells
           (#.(cons ct-unbound +simple-types+)
              (when (or dump-allocated dump-data)
                (gc-dump-cell address :stream stream))
              (gc-bitmap-set-visited bitmap address)) ; size = 1
           ;; -------------------- cons cells
           ((#.ct-cons)
            (when (or dump-allocated dump-data)
              (gc-dump-cell address :stream stream)) ; dumps CAR and CDR
            (follow object)                          ; CDR
            (follow (gc-load (1+ address)))          ; CAR
            (gc-bitmap-set-visited-range bitmap address 2)) ; size = 2
           ;; -------------------- structures and vectors
           ((#.ct-structure #.ct-vector #.ct-vector-fp #.ct-array)
            (when-debug (:gcct) (format t "  gcct:   is a structure~%"))
            (when (or dump-allocated dump-data)
              (gc-dump-cell address :stream stream :contents nil))
            (when (member (cvm-element-type address)  +complex-element-types+)
              ;; TODO: This condition is messy. Arrays of arrays of strings?
              ;; TODO: We could push only a descriptor for the range,
              ;; TODO: and decrement the range on pop.
              (loop for address from (+ address (cvm-size-of object) -1)
                 downto (+ 2 address)
                 do (follow (gc-load (cvm-deref address)))))
            (when-debug (:gcct)
              (format t "  gcct:   new addresses = ~S~%" addresses))
            (gc-bitmap-set-visited-range bitmap address (cvm-size-of object)))
           ;; -------------------- errors
           (otherwise
            (assert nil () "Unknown type-code ~D at #x~8,'0X"
                    (cvm-type-of object) address)))))
     finally (return bitmap)))


(defun gc-check-internal (&key (stream *standard-output*)
                          (allow-free-blocks nil)
                          (dump-all nil) (dump-bitmap nil)
                          (dump-header    nil) (dump-free nil)
                          (dump-allocated nil) (dump-data nil))
  (when dump-all
    (setf dump-header    t
          dump-bitmap    t
          dump-free      t
          dump-allocated t
          dump-data      t))
  (assert (plusp *gc-heap-base*))
  (when dump-header 
    (format stream "~&Heap header:~%")
    (format stream "  base address    = #x~8,'0X~%" *gc-heap-base*)
    (format stream "  magic cookie    = #x~16,'0X~%" (gc-signature))
    (format stream "  cell 1          = #x~16,'0X~%" 
            (peek-uint64 *gc-memory* *gc-heap-base*)))
  ;; check the header structure:
  (assert (= (gc-load +gc-heap-header+) (cvm-form-head ct-structure 8)))
  (assert (= (gc-load (1+ +gc-heap-header+)) (cvm-form-head ct-t 6)))
  ;; check the header slots:
  (assert 
   (and (cvm-fixnum-p (cvm-hh-size +gc-heap-header+))
        (= *gc-heap-size* (cvm-fixnum-value (cvm-hh-size +gc-heap-header+)))))
  (assert (cvm-vector-p (cvm-hh-free-blocks +gc-heap-header+)))
  (assert (or (cvm-null (cvm-hh-root +gc-heap-header+))
              (cvm-cons-p (cvm-hh-root +gc-heap-header+))))
  (assert (or (cvm-null (cvm-hh-new-generation +gc-heap-header+)) 
              (cvm-vector-fp-p (cvm-hh-new-generation +gc-heap-header+))))
  (when dump-header 
    (format stream "  heap size       = #x~8,'0X (~D) 64-bit cells~%"
            *gc-heap-size*  *gc-heap-size*)
    (format stream "  free-blocks    at #x~8,'0X~%" 
            (cvm-deref (cvm-hh-free-blocks +gc-heap-header+)))
    (format stream "  variables      at #x~8,'0X~%" 
            (cvm-deref (cvm-hh-root +gc-heap-header+)))
    (format stream "  new-generation at #x~8,'0X~%"
            (cvm-deref (cvm-hh-new-generation +gc-heap-header+))))
  ;; check the free block vector:
  (let ((fb (cvm-deref (cvm-hh-free-blocks +gc-heap-header+))))
    (when (/= fb +gc-expected-free-block+)
      (format stream "Warning: free-blocks vector moved to #x~8,'0X~%" fb))
    (assert (= (gc-load fb)      (cvm-form-head ct-vector 34)))
    (assert (= (gc-load (1+ fb)) (cvm-form-head ct-t 32)))
    (let ((bitmap (gc-make-bitmap)))
      (gc-bitmap-set-allocated-range bitmap +gc-heap-header+ 
                                     (cvm-size-of (gc-load +gc-heap-header+)))
      (gc-bitmap-set-allocated-range bitmap fb (cvm-size-of (gc-load fb)))
      (gc-bitmap-set-visited-range   bitmap fb (cvm-size-of (gc-load fb)))
      ;; check the free block lists:
      (when dump-header (format stream "Free lists:~%"))
      (loop for i from 0 upto +fb-big+ 
         do ;; (format t "checking free block list ~D~%" i)
         (gc-check-free-block-list i (cvm-svref fb i)
                                   :stream stream  :bitmap bitmap
                                   :dump-free dump-free))
      ;; check the allocated data:
      (when dump-header (format stream "Variables & New Generation:~%"))
      (gc-check-tree +gc-heap-header+ :stream stream
                     :allow-free-blocks allow-free-blocks :bitmap bitmap
                     :dump-allocated dump-allocated :dump-data dump-data)
      ;; check the bitmap: no clear bit above +gc-start+
      (when dump-bitmap
        (print-bitmap bitmap :stream stream))
      (loop with i = +gc-start+ 
         while (< i *gc-heap-size*)
         do (if (gc-bitmap-clear-p bitmap i)
                (let ((end (loop for j from i 
                              while (and (< j *gc-heap-size*)
                                         (gc-bitmap-clear-p bitmap j))
                              finally (return j))))
                  (when dump-bitmap
                    (format stream
                      "gc-check: Found a garbage range #x~8,'0X - #x~8,'0X~%"
                      i (1- end)))
                  (setf i end))
                (incf i))))))


(defun gc-check (&key (stream *standard-output*)
                 (allow-free-blocks nil)
                 (dump-all nil) (dump-bitmap nil)
                 (dump-header    nil) (dump-free nil)
                 (dump-allocated nil) (dump-data nil))
  (when dump-all
    (setf dump-header    t
          dump-bitmap    t
          dump-free      t
          dump-allocated t
          dump-data      t))
  ;; assume *gc-semaphore-set* and *gc-semaphore-number* are valid.
  (with-gc-lock
    (gc-check-internal :stream stream   
                       :allow-free-blocks allow-free-blocks
                       :dump-header dump-header    
                       :dump-bitmap dump-bitmap
                       :dump-free dump-free
                       :dump-allocated dump-allocated
                       :dump-data dump-data)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GARBAGE COLLECTOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ---------------------------------------------------------------------
;; allocate:
;; case big block:
;;  1- search a block of same size
;;  2- search a block of double size
;;  3- search a best fit block (size+2+)
;;  4- search a bigger block (size+1+)
;;  5- if not already gc in this allocate, 
;;     then garbage collect and try once more from the start.
;;  6- fail
;; case small block:
;;  1- search a block in the same bucket.
;;  2- search a block in bucket 2*idx if < 31, or in big blocks.
;;  3- search a best fit block (size+2+), from idx+2, idx+3, ..., in big blocks.
;;  4- search a bigger block (size+1+), seach idx+1.
;;  5- if not already gc in this allocate, 
;;     then garbage collect and try once more from the start.
;;  6- fail
;; ---------------------------------------------------------------------
;; garbage collect:
;;  - no copying collector, to avoid having to update the references 
;;    in other spaces.
;;  - mark:  recursively work from root and mark all accessible.
;;  - sweep: start from first cell, and in sequence, check whether it's marked,
;;           and gather free ranges.
;; ---------------------------------------------------------------------



(defun fb-extract (free-blocks previous old-block size)
  (when-debug (:allocate)
    (format t "fb-extract F/#x~16,'0X P/~A ~@
             ~&           O/#x~16,'0X S/#x~8,'0X~%" 
            free-blocks (if previous (format nil "#x~16,'0X" previous) "")
            old-block size)
    (gc-dump-cell old-block))
  (if previous
      (cvm-free-block-set-next previous old-block)
      (cvm-svset free-blocks size 
                 (cvm-form-address
                  (cvm-free-block-next old-block)))))


(defun fb-insert (free-blocks new-block)
  (when-debug (:allocate)
    (format t "fb-insert F/#x~16,'0X N/#x~16,'0X~%" free-blocks new-block))
  (assert (cvm-free-block-p (gc-load new-block)))
  (let ((size (cvm-free-block-size (gc-load new-block))))
    (if (< size +fb-big+)
        (progn
          (cvm-free-block-set-next new-block 
                                   (cvm-deref (cvm-svref free-blocks size)))
          (cvm-svset free-blocks size (cvm-form-address new-block)))
        (loop for prev = nil then curr
           for curr = (cvm-deref (cvm-svref free-blocks +fb-big+)) 
           then (cvm-free-block-next curr)
           while (and (plusp curr)
                      (< size (cvm-free-block-size (gc-load curr))))
           finally
           (cvm-free-block-set-next new-block curr)
           (if prev
               (cvm-free-block-set-next prev new-block)
               (cvm-svset free-blocks +fb-big+
                          (cvm-form-address new-block))))))
  (when-debug (:allocate) (gc-dump-cell new-block)))


(defun fb-search-big-block (free-list size)              
  (loop 
     with previous = nil
     with fit      = nil                ; size
     with double   = nil                ; size*2
     with best     = nil                ; size+2+n
     with bigger   = nil                ; size+1
     for prev = nil then curr
     for curr = free-list then (cvm-free-block-next curr)
     for obje = (if (plusp curr) (gc-load curr) 0)
     while (and (plusp curr) (null best))
     do 
     ;; (gc-dump-cell curr)
     ;; (format t "~X ~D ~D~%" obje(cvm-free-block-size obje) size)
     (cond
       ((and (not fit)    (=  size      (cvm-free-block-size obje)))  
        (when-debug (:allocate)
          (format t "fb-search-big-block #x~16,'0X #x~8,'0X ~@
                      found fit #x~16,'0X ~:[P/NIL~;~:*P/#x~16,'0X~]~%" 
                  free-list size curr (or previous prev)))
        (setf fit      curr
              previous (or previous prev)))
       ((and (not double) (=  (* size 2) (cvm-free-block-size obje)))
        (when-debug (:allocate)
          (format t "fb-search-big-block #x~16,'0X #x~8,'0X ~@
                      found double #x~16,'0X ~:[P/NIL~;~:*P/#x~16,'0X~]~%" 
                  free-list size curr (or previous prev)))
        (setf double   curr
              previous (or previous prev)))
       ((and (not best)   (<= (+ size 2) (cvm-free-block-size obje)))
        (when-debug (:allocate)
          (format t "fb-search-big-block #x~16,'0X #x~8,'0X ~@
                      found best #x~16,'0X ~:[P/NIL~;~:*P/#x~16,'0X~]~%" 
                  free-list size curr (or previous prev)))
        (setf best     curr
              previous (or previous prev)))
       ((and (not bigger) (<  size       (cvm-free-block-size obje)))
        (when-debug (:allocate)
          (format t "fb-search-big-block #x~16,'0X #x~8,'0X ~@
                      found bigger #x~16,'0X ~:[P/NIL~;~:*P/#x~16,'0X~]~%" 
                  free-list size curr (or previous prev)))
        (setf bigger   curr
              previous (or previous prev))))
     finally (return
               (values previous fit double best bigger)))) ;;fb-search-big-block


(defun gc-allocate-internal (size)
  (when (< size 2) (setf size 2))
  (let* ((garbage-collected nil)
         (free-blocks (cvm-deref (cvm-hh-free-blocks +gc-heap-header+)))
         (idx         (min size +fb-big+))
         (free-list   (cvm-deref (cvm-svref free-blocks idx)))
         prev fit double best bigger (big-searched nil))
    (macrolet ((return-fit 
                   (found size)
                 ;; nothing more, return fit
                 `(progn 
                    (when-debug (:allocate)
                      (format t "gc-allocate #x~8,'0X ~@
                                  found a fit #x~16,'0X~%" ,size ,found))
                    (return-from gc-allocate-internal ,found)))
               (return-double
                   (found size)
                 ;; split in two, fb-insert one, return the other
                 `(let* ((half (/ (cvm-free-block-size (gc-load ,found)) 2))
                         (free (+ ,found half)))
                    (gc-store free (cvm-form-free-block half 0))
                    (fb-insert free-blocks free)
                    (cvm-free-block-set-size ,found half)
                    (when-debug (:allocate)
                      (format t "gc-allocate #x~8,'0X ~@
                                  found a double #x~16,'0X F/#x~16,'0X~%" 
                              ,size ,found free))
                    (return-from gc-allocate-internal ,found)))
               (return-best
                   (found size)
                 ;; split in two, fb-insert the rest, return the one
                 `(let ((free (+ ,found ,size)))
                    (gc-store free
                              (cvm-form-free-block 
                               (- (cvm-free-block-size (gc-load ,found)) 
                                  ,size) 0))
                    (fb-insert free-blocks free)
                    (cvm-free-block-set-size ,found ,size)
                    (when-debug (:allocate)
                      (format t "gc-allocate #x~8,'0X ~@
                                  found a best #x~16,'0X F/#x~16,'0X~%" 
                              ,size ,found free))
                    (return-from gc-allocate-internal ,found)))
               (return-bigger
                   (found size)
                 ;; split in two, forget the smaller, return the one
                 ;; lose the 8-byte trailer. 
                 ;; (split to put a free-block header in it though!)
                 `(let ((free (+ ,found ,size)))
                    (gc-store free
                              (cvm-form-free-block 
                               (- (cvm-free-block-size (gc-load ,found))
                                  ,size) 0))
                    (cvm-free-block-set-size ,found ,size)
                    (when-debug (:allocate)
                      (format t "gc-allocate #x~8,'0X ~@
                                  found a bigger #x~16,'0X L/#x~16,'0X~%" 
                              ,size ,found free))
                    (return-from gc-allocate-internal ,found))))
      (tagbody 
         (when-debug (:allocate) (go :first-time))
       :again
         (when-debug (:allocate)
           (format t "gc-allocate #x~16,'0X trying again~%"  size))
       :first-time
         (when (= idx +fb-big+)
           ;; big block
           (when-debug (:allocate)
             (format t "gc-allocate #x~16,'0X searching a big block~%"  
                     size))
           (multiple-value-bind (prev fit double best bigger) 
               (fb-search-big-block free-list size)
             (let ((found (or fit double best bigger)))
               (when found
                 (fb-extract free-blocks prev found +fb-big+)
                 (cond
                   (fit      (return-fit    found size))
                   (double   (return-double found size))
                   (best     (return-best   found size))
                   (t        (return-bigger found size))))
               (go :collect))))
         ;; small block
         (when-debug (:allocate)
           (format t "gc-allocate #x~16,'0X searching a small block~%"  
                   size))
         ;; 1- search a block in the same bucket.
         (when (plusp free-list)
           (fb-extract free-blocks nil free-list size)
           (return-fit free-list size))
         ;; 2- search a block in bucket 2*size if<+fb-big+, or in big blocks.
         (when-debug (:allocate)
           (format t "gc-allocate #x~16,'0X searching a double~%"  size))
         (if (< (* 2 size) +fb-big+)
             (let ((free-list (cvm-deref (cvm-svref free-blocks 
                                                    (* 2 size)))))
               (when (plusp free-list)
                 (fb-extract free-blocks nil free-list (* 2 size))
                 (return-double free-list size)))
             (let ((free-list  (cvm-deref (cvm-svref free-blocks +fb-big+))))
               (multiple-value-setq (prev fit double best bigger) 
                 (fb-search-big-block free-list size))
               (setf big-searched t)
               (when double
                 (fb-extract free-blocks prev double +fb-big+)
                 (return-double double size))))
         ;;  3- search a best fit block (size+2+), from idx+2,...,in big blks
         (when-debug (:allocate)
           (format t "gc-allocate #x~16,'0X searching a best fit~%"  size))
         (loop for sidx from (+ 2 idx) to +fb-big+
            for free-list = (cvm-deref (cvm-svref free-blocks sidx))
            do (when-debug (:allocate)
                 (format t "try [~2,'0X]:" sidx)
                 (gc-dump-cell free-list))
            (when (plusp free-list)
              (if (= sidx +fb-big+)
                  (progn
                    (when-debug (:allocate)
                      (format t "gc-allocate big-searched=~A~%"
                              big-searched))
                    (unless big-searched
                      (multiple-value-setq
                          (prev fit double best bigger) 
                        (fb-search-big-block free-list size))
                      (setf big-searched t))
                    (when-debug (:allocate)
                      (when prev
                        (format t "gc-allocate previous:~%")
                        (gc-dump-cell prev))
                      (when fit
                        (format t "gc-allocate fit:~%")
                        (gc-dump-cell fit))
                      (when double
                        (format t "gc-allocate double:~%")
                        (gc-dump-cell double))
                      (when best
                        (format t "gc-allocate best:~%")
                        (gc-dump-cell best))
                      (when bigger
                        (format t "gc-allocate bigger:~%")
                        (gc-dump-cell bigger)))
                    (when best
                      ;; found
                      (fb-extract free-blocks prev best sidx)
                      (return-best best size)))
                  (progn ;; found
                    (fb-extract free-blocks nil free-list sidx)
                    (return-best free-list size)))))
         ;;  4- search a bigger block (size+1+), seach idx+1.
         (when-debug (:allocate)
           (format t "gc-allocate #x~16,'0X searching a bigger~%"  size))
         (when (< idx 30)
           (incf idx)
           (let ((free-list (cvm-deref (cvm-svref free-blocks  idx))))
             (when (plusp free-list)
               ;; found!
               (fb-extract free-blocks nil free-list idx)
               (return-best free-list size))))
       :collect
         ;;  5- if not already gc in this allocate, 
         ;;     then garbage collect and try once more from the start.
         (unless garbage-collected
           (when-debug (:allocate)
             (format t "gc-allocate #x~16,'0X calling garbage collector~%"
                     size))
           (gc-collect-internal)
           (setf garbage-collected t big-searched nil)
           (go :again))
         ;;  6- fail
         (error "Out of COMMON memory.")))))


(defun gc-allocate (size)
  ;; size = number of 8-byte cells to allocate, from 2 up.
  (with-gc-lock
    (when-debug (:check)
      (format *standard-output* "~&gc-allocate ~A~%Check before allocate~%" 
              size)
      (apply (function gc-check-internal)
             :stream *standard-output* *check-args*))
    (prog1 (let ((allocated  (cvm-form-address (gc-allocate-internal size)))
                 (generation (cvm-hh-new-generation +gc-heap-header+)))
             (unless (cvm-null generation)
               (when-debug (:ng)
                 (format t "ng:push  [~2D] ~16,'0X~%" 
                         (cvm-fill-pointer generation) allocated))
               (cvm-vector-push allocated generation))
             allocated)
      (when-debug (:check)
        (format *standard-output* "~&Check after allocate~%")
        (apply (function gc-check-internal) :allow-free-blocks t
               :stream *standard-output* *check-args*)))))


(defun gc-mark (address)
  (case (cvm-type-of address)
    ((#.ct-cons #.ct-vector #.ct-vector-fp #.ct-structure #.ct-array)
     (when-debug (:gc)
       (warn "gc-mark got a complex object instead of a ct-address.~%")))
    ((#.ct-free-block)
     (when-debug (:gc)
       (warn "gc-mark got a free-block ~A.~%" 
             (with-output-to-string (out)
               (gc-dump-cell address :stream out :contents nil)))))
    ((#.ct-address #.ct-readable)
     ;; Reference values are addresses to the real value which needs a mark.
     (let ((object (gc-load (cvm-deref address))))
       (unless (gc-marked object)
         (flet ((mark (address object)
                  (when-debug (:gc)
                    (format *trace-output* "~&M ")
                    (gc-dump-cell address :stream *trace-output*
                                  :contents t :margin "  "))
                  (gc-store (cvm-deref address) (gc-set-mark object))))
           (case (cvm-type-of object)
             ((#.ct-address #.ct-readable)
              (when-debug (:gc)
                (warn "gc-mark got an indirection, ~A to ~A~%~@
                     ~@{~A~%~}"
                      (cell-type-label (cvm-type-of address))
                      (cell-type-label (cvm-type-of object))
                      (with-output-to-string (out)
                        (gc-dump-cell address :stream out :contents nil))
                      (with-output-to-string (out)
                        (gc-dump-cell object  :stream out :contents nil))))
              ;; this should not occur, but let's do it anyway.
              (mark address object)
              (gc-mark object))
             ((#.ct-cons)
              (mark address object)
              (gc-mark (cvm-car address))
              (gc-mark (cvm-cdr address)))
             ((#.ct-vector #.ct-vector-fp #.ct-structure #.ct-array)
              ;; [[vector-data|structure-data] size
              ;; | [ element-type (T, unboxed-type) dimensions-1 ]
              ;; | data ]
              ;; [vector-fp-data size
              ;; | [ element-type (T, unboxed-type) dimensions-1 ]
              ;; | fill-pointer
              ;; | data ]
              ;; [array-data  size
              ;; | [ element-type (T, unboxed-type) ndimensions ]
              ;; | dimension_1 | ... | dimension_n
              ;; | data ]
              (mark address object)
              (let ((element-type (cvm-element-type address)))
                (cond
                  ((= ct-t element-type)
                   ;; elements can be anything
                   (multiple-value-bind (dimension address) (cvm-rows address)
                     (loop for address from address below (+ address dimension)
                        do (gc-mark (gc-load address))))))))
             ;; otherwise plain simple values don't need a mark
             ;;           since they are boxed.
             )))))
    ;; otherwise plain simple values don't need a mark
    ;;           since they are boxed.
    ))


(defun gc-sweep ()
  ;;  - sweep: start from first cell, and in sequence, check whether 
  ;;    it's marked, and gather free ranges.
  (let ((free-blocks (cvm-deref (cvm-hh-free-blocks +gc-heap-header+))))
    (multiple-value-bind (dimension address) (cvm-rows free-blocks)
      (loop for i from 0 below dimension
         do (gc-store address +cvm-nil+) (incf address)))
    (loop with free-start = nil
       for curadr = +gc-start+ then (+ curadr (cvm-size-of curobj))
       for curobj = (when (< curadr *gc-heap-size*) (gc-load curadr))
       while (< curadr *gc-heap-size*)
       do 
       (if (gc-marked curobj)
           (progn
             (when-debug (:gc) 
               (format *trace-output* "~&V ") 
               (gc-dump-cell curadr :stream *trace-output* :margin "  "))
             (gc-store curadr (gc-clear-mark curobj))
             (when free-start
               (gc-store free-start (cvm-form-free-block
                                     (- curadr free-start) 0))
               (fb-insert free-blocks free-start)
               (when-debug (:gc) 
                 (format *trace-output* "~&C ") 
                 (gc-dump-cell free-start :stream *trace-output* :margin "  "))
               (setf free-start nil)))
           (setf free-start (or free-start curadr)))
       finally (when free-start
                 (gc-store free-start (cvm-form-free-block 
                                       (- curadr free-start) 0))
                 (fb-insert free-blocks free-start)
                 (when-debug (:gc) 
                   (format *trace-output* "~&C ")
                   (gc-dump-cell free-start
                                 :stream *trace-output* :margin "  "))
                 (setf free-start nil))))) ;;gc-sweep


(defun gc-collect-internal ()
  ;; gc lock is already aquired.
  ;; garbage collect:
  ;;  - no copying collector, to avoid updating the references in other spaces.
  ;;  - mark:  recursively work from root and mark all accessible.
  ;;  - sweep: start from first cell, and in sequence, check whether 
  ;;    it's marked, and gather free ranges.
  (when-debug (:gc :check)
    (format *trace-output* "~&Check before garbage collection~%")
    (apply (function gc-check-internal)
           :stream *trace-output* *check-args*))
  (when-debug (:gc) (format *trace-output* "~&Marking root~%"))
  (gc-mark (cvm-hh-root           +gc-heap-header+))
  (when-debug (:gc) (format *trace-output* "~&Marking new generation~%"))
  (gc-mark (cvm-hh-new-generation +gc-heap-header+))
  (when-debug (:gc) (format *trace-output* "~&Sweeping~%"))
  (gc-sweep)
  (when-debug (:gc :check)
    (format *trace-output* "~&Check after garbage collection~%")
    (apply (function gc-check-internal)
           :stream *trace-output* *check-args*))) ;;gc-collect-internal
 

(defun gc-collect-garbage ()
  (with-gc-lock
    (gc-collect-internal)))


;;--------------------------

(defun gc-initialized-p () (= (gc-signature) *gc-magic-cookie*))

(defun gc-initialize (memory)
  "
DO:     Initialize the heap in *gc-memory*.
"
  (setf *gc-memory* memory
        *gc-heap-base* (base memory)
        *gc-heap-size* (truncate (size memory) 8))
  (unless (gc-initialized-p)
    (with-common-lock
        (gc-store +gc-heap-header+ (cvm-form-head ct-structure 8))
      (cvm-initialize-vector +gc-heap-header+ ct-structure ct-t 6)
      (cvm-hh-set-size +gc-heap-header+ (cvm-form-fixnum *gc-heap-size*))
      (let ((fbv (+ +gc-heap-header+ 
                    (cvm-size-of (gc-load +gc-heap-header+)))))
        (cvm-hh-set-free-blocks +gc-heap-header+ (cvm-form-address fbv))
        ;; (cvm-initialize-vector stores NIL/0 in the slots)
        (gc-store fbv (cvm-form-head ct-vector 34))
        (cvm-initialize-vector fbv ct-vector ct-t 32)
        (let ((ptr (+ fbv (cvm-size-of (gc-load fbv)))))
          (gc-store ptr (cvm-form-free-block (- 64 ptr) 0))
          (fb-insert fbv ptr)
          (setf ptr 64)
          (gc-store ptr (cvm-form-free-block (- 128 ptr) 0))
          (fb-insert fbv ptr)
          (setf ptr 128)
          (gc-store ptr (cvm-form-free-block (- *gc-heap-size* ptr) 0))
          (fb-insert fbv ptr)))
      (cvm-hh-set-new-generation 
       +gc-heap-header+ 
       (cvm-make-vector-fp ct-t +ng-size+ :fill-pointer (cvm-form-fixnum 0)))
      (let* ((name/common-lisp  (cvm-make-string :contents "COMMON-LISP"))
             (name/symbol       (cvm-make-string :contents "SYMBOL"))
             (name/package      (cvm-make-string :contents "PACKAGE"))
             (symbol            (cvm-make-structure +cvm-nil+ 5))
             (package           (cvm-make-structure symbol    5))
             (common-lisp       (cvm-make-structure package   5)))
        (setf *gc-symbol*  symbol)
        (setf *gc-package* package)
        (cvm-symbol-set-package    symbol      common-lisp)
        (cvm-symbol-set-name       symbol      name/symbol)
        (cvm-symbol-set-package    package     common-lisp)
        (cvm-symbol-set-name       package     name/package)
        (cvm-package-set-name      common-lisp name/common-lisp)
        (cvm-structure-store       symbol      0 symbol)
        (cvm-structure-store       package     0 symbol)
        (cvm-structure-store       common-lisp 0 package)
        (cvm-push package (cvm-package-symbols common-lisp))
        (cvm-push symbol  (cvm-package-symbols common-lisp))
        (gc-push-root common-lisp))
      (let*((name/system      (cvm-make-string :contents "SYSTEM"))
            (name/heap-header (cvm-make-string :contents "HEAP-HEADER"))
            (system           (cvm-make-package name/system))
            (heap-header      (cvm-intern name/heap-header  system)))
        (cvm-structure-store +gc-heap-header+ 0 heap-header))
      (gc-reset-generation)
      (cvm-hh-set-root +gc-heap-header+
                       (cvm-list-nreverse
                        (cvm-hh-root +gc-heap-header+)))
      (gc-sign  *gc-magic-cookie*)))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMMON FUNCTION INTEFACE (CFI)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   - functions to convert to and from shared memory values 
;;     and host lisp values.
;;   : used to pass data to and from shared and host.


(defvar *ld-values* nil)

(defun ld-get (value) 
  (when-debug (:ld)
    (let ((*print-circle* t))
      (format t "~&ld-get ~S [~D] --> ~S~%" 
              value (sxhash value) (gethash value *ld-values*))))
  (gethash value *ld-values*))

(defun ld-put (value cvm-value)
  (when-debug (:ld)
    (let ((*print-circle* t))
      (format t "~&ld-put ~S [~D] --: ~S~%" value (sxhash value) cvm-value)))
  (setf (gethash value *ld-values*) cvm-value))


(defun ld-lcache (l-value)
  (or (ld-get l-value) (ld-put l-value (cfi-copy-to-common l-value))))


(defmacro with-loop-detection (&body body)
  `(let ((old-values *ld-values*))
     (when (null *ld-values*)
       (setf *ld-values* (make-hash-table :test (function eql))))
     (unwind-protect (progn ,@body)
       (setf *ld-values* old-values))))


(defgeneric cfi-copy-to-common (value)
  (:method ((value (eql nil)))     (declare (ignorable value)) +cvm-nil+)
  (:method ((value (eql t)))       (declare (ignorable value)) +cvm-t+)
  (:method ((value character))     (cvm-form-character value))
  (:method ((value integer))       (cvm-form-fixnum    value)) ; no bignum yet
  (:method ((value float)) 
    (cond
      ((typep value 'short-float)  (cvm-form-single-float     value))
      ((typep value 'single-float) (cvm-form-single-float     value))
      (t (error "double-float and long-float unsupported yet."))))
  (:method ((value ratio))         (declare (ignorable value)) (error "No ratio yet."))
  (:method ((value complex))       (declare (ignorable value)) (error "No complex yet."))
  ;; 1- allocate the current node and store it to the ld hash before
  ;; 2- allocating the sub-nodes.
  (:method ((value cons))
    (or (ld-get value) (cfi-copy-tree value)))
  (:method ((value symbol))
    (or (ld-get value)
        (let* ((name (cfi-copy-to-common (symbol-name value)))
               (sym (ld-put value
                            (if (symbol-package value)
                                (cvm-intern name (cfi-copy-to-common
                                                  (symbol-package value)))
                                (cvm-make-symbol name +cvm-nil+)))))
          ;; PERHAPS we don't want to copy the value of a random symbol
          #||
          (with-generation
              ((plis (cfi-copy-to-common (symbol-plist value)))
               (valu (if (boundp value)
                         (cfi-copy-to-common (symbol-value value))
                         +cvm-unbound+))
               (func (if (fboundp value)
                         (cfi-copy-to-common
                          (FUNCTION-LAMBDA-EXPRESSION (SYMBOL-FUNCTION value)))
                         +cvm-unbound+)))
            (cvm-symbol-set-plist    sym plis)
            (cvm-symbol-set-value    sym valu)
            (cvm-symbol-set-function sym func))
          ||#
          sym)))
  (:method ((value string))
    ;; TODO: since we make a copy, the fill-pointers are not synchronized!
    (or (ld-get value)
        (ld-put value (cvm-make-string :contents value))))
  (:method ((value vector))
    (declare (ignorable value)) 
    ;; TODO: since we make a copy, the fill-pointers are not synchronized!
    ;; TODO: avoid circles
    #||
    (or (ld-get value)
    (ld-put value (cvm-make-vector <<<value>>> (length value))))
    ||#(error "not implemented yet"))
  (:method ((value array))
    (declare (ignorable value)) 
    ;; TODO: avoid circles
    #||
    (or (ld-get value)
    (ld-put value (cvm-make-array value)))
    ||#(error "not implemented yet"))
  (:method ((value structure-object))
    (declare (ignorable value))
    ;; TODO: avoid circles
    (error "Cannot handle structures yet."))
  (:method ((value package))
    (or (ld-get value)
        (ld-put value
                (with-generation
                    ((name (cfi-copy-to-common (package-name value)))
                     (pack (cvm-find-package name)))
                  (if (cvm-null pack)
                      ;; make-package pushes the new package onto the root.
                      (cvm-make-package name #|TODO:nicknames|#)
                      pack)))))
  (:method ((value t))
    ;; function
    ;; restart
    ;; method
    ;; method-combination
    ;; object
    ;; structure
    ;; condition
    ;; hashtable
    ;; pathname
    ;; stream
    ;; readtable
    ;; ...
    (error "Unsupported value ~S" value)))


(defun cfi-copy-tree (ltree)
  (let ((stack '()))
    (flet ((copy-cdr 
               (ltree ctree)
             (when-debug (:ct) (format t "~&PROCESSING CDR; LTREE: ~S~%" ltree))
             (loop for lprev = ltree then lnode
                for cprev = ctree then cnode
                for lnode = (cdr ltree) then (cdr lnode)
                with cnode
                while (consp lnode)
                do (with-generation ()
                     (setf cnode
                           (ld-put lnode (cvm-make-cons +cvm-nil+ +cvm-nil+)))
                     (cvm-setcdr cprev cnode)
                     (push lnode stack))
                finally (unless (null lnode)
                          (with-generation ()
                            (cvm-setcdr cprev (cfi-copy-to-common lnode)))))))
      ;; processing the root node ltree:
      (let ((ctree (ld-get ltree)))
        (when-debug (:ct)
          (format t "~&PROCESSING ROOT: ~S~%" ltree))
        (cond
          (ctree #|nothing more|#)
          ((consp ltree)
           ;; we keep ctree in the new-generation.
           (setf ctree (ld-put ltree (cvm-make-cons +cvm-nil+ +cvm-nil+)))
           (push ltree stack)  ; we'll process (car ltree) thereafter.
           (copy-cdr ltree ctree))      ; we process (cdr ltree) now.
          (t (ld-put ltree (cfi-copy-to-common ltree)))))
      ;; processing the CARs:
      (loop while stack do 
           (let* ((lpare (pop stack))
                  (cpare (ld-get lpare))
                  (ltree (car lpare))
                  (ctree))
             (when-debug (:ct) (format t "~&PROCESSING CAR; LTREE: ~S~%" ltree))
             (assert (not (null cpare)))
             (cond
               ((atom ltree)                       
                (with-generation ()
                  (cvm-setcar cpare (cfi-copy-to-common ltree))))
               (t
                (with-generation ()
                  (setf ctree 
                        (ld-put ltree (cvm-make-cons +cvm-nil+ +cvm-nil+)))
                  (cvm-setcar cpare ctree))
                (push ltree stack)
                (copy-cdr ltree ctree)))))))
  (ld-get ltree))


(defun cfi-make-symbol (sym)
  "
SYM:     The ct-address of a cvm symbol
RETURN:  An interned lisp symbol whose plist, value and function are updated
         with copies of those of SYM.
"
  (let ((name (cfi-copy-from-common (cvm-symbol-name    sym)))
        (pack (cfi-copy-from-common (cvm-symbol-package sym))))
    (intern name pack)))


(defun cfi-symbol-copy-from-common (lisp-symbol cvm-symbol)
  (setf (symbol-plist lisp-symbol)
        (cfi-copy-from-common (cvm-symbol-plist cvm-symbol)))
  (unless (or (eq (symbol-package lisp-symbol) 
                  (cvm-find-package (ld-lcache "COMMON-LISP")))
              (eq (symbol-package lisp-symbol)
                  (cvm-find-package (ld-lcache "KEYWORD"))))
    (let ((val (cvm-symbol-value cvm-symbol)))
      (unless (eql +cvm-unbound+ val)
        (setf (symbol-value lisp-symbol) (cfi-copy-from-common val))))
    (let ((val (cvm-symbol-function cvm-symbol)))
      (unless (eql +cvm-unbound+ val)
        (setf (symbol-function lisp-symbol) (cfi-copy-from-common val)))))
  lisp-symbol)
  

(defun cfi-copy-from-common (cvm-value &key typecode)
  (case (or typecode (cvm-type-of cvm-value))
    ((#.ct-nil)              nil)
    ((#.ct-t)                t)
    ((#.ct-unbound)          (error "Trying to convert an unbound cvm-value"))
    ((#.ct-bit)              (ldb (byte  1 0) cvm-value))
    ((#.ct-character-8)      (code-char (ldb (byte  8 0) cvm-value)))
    ((#.ct-character-16)     (code-char (ldb (byte 16 0) cvm-value)))
    ((#.ct-character-24)     (code-char (ldb (byte 32 0) cvm-value)))
    ((#.ct-character-32)     (code-char (ldb (byte 32 0) cvm-value)))
    ((#.ct-signed-byte-8)    (error "(signed-byte  8) unsupported yet."))
    ((#.ct-signed-byte-16)   (error "(signed-byte 16) unsupported yet."))
    ((#.ct-signed-byte-24)   (error "(signed-byte 24) unsupported yet."))
    ((#.ct-signed-byte-32)   (error "(signed-byte 32) unsupported yet."))
    ((#.ct-signed-byte-40)   (error "(signed-byte 40) unsupported yet."))
    ((#.ct-signed-byte-48)   (error "(signed-byte 48) unsupported yet."))
    ((#.ct-signed-byte-56)   (cvm-fixnum-value cvm-value))
    ((#.ct-signed-byte-64)   (error "(signed-byte 64) unsupported yet."))
    ((#.ct-unsigned-byte-8)  (ldb (byte  8 0) cvm-value))
    ((#.ct-unsigned-byte-16) (ldb (byte 16 0) cvm-value))
    ((#.ct-unsigned-byte-24) (ldb (byte 24 0) cvm-value))
    ((#.ct-unsigned-byte-32) (ldb (byte 32 0) cvm-value))
    ((#.ct-unsigned-byte-40) (ldb (byte 40 0) cvm-value))
    ((#.ct-unsigned-byte-48) (ldb (byte 48 0) cvm-value))
    ((#.ct-unsigned-byte-56) (ldb (byte 56 0) cvm-value))
    ((#.ct-unsigned-byte-64) (ldb (byte 64 0) cvm-value))
    ((#.ct-float-8)          (error "float-8 unsupported yet."))
    ((#.ct-float-16)         (error "float-16 unsupported yet."))
    ((#.ct-float-24)         (error "float-24 unsupported yet."))
    ((#.ct-float-32)         (cvm-single-float-value cvm-value))
    ((#.ct-float-40)         (error "float-40 unsupported yet."))
    ((#.ct-float-48)         (error "float-48 unsupported yet."))
    ((#.ct-float-56)         (error "float-56 unsupported yet."))
    ((#.ct-float-64)         (error "float-64 unsupported yet."))
    ((#.ct-structure)        (error "Needs an address to convert a structure."))
    ((#.ct-vector)           (error "Needs an address to convert a vector."))
    ((#.ct-vector-fp)        (error "Needs an address to convert a vector-fp."))
    ((#.ct-array)            (error "Needs an address to convert a array."))
    ((#.ct-cons)             (error "Needs an address to convert a cons."))
    ((#.ct-address #.ct-readable) ;; TODO: readable needs post processing!
     (let ((ref-value  (gc-load (cvm-deref cvm-value))))
       (case (cvm-type-of ref-value)
         ((#.ct-cons)
          (or (ld-get cvm-value)
              (let ((cons (ld-put cvm-value (cons nil nil))))
                (setf (car cons) (cfi-copy-from-common (cvm-car cvm-value))
                      (cdr cons) (cfi-copy-from-common (cvm-cdr cvm-value)))
                cons)))
         ((#.ct-structure) 
          (cond 
            ((cvm-symbol-p cvm-value) 
             (or (ld-get cvm-value)
                 (let  ((lsym (ld-put cvm-value (cfi-make-symbol cvm-value))))
                   ;; PERHAPS we don't want to copy the value of a random symbol
                   #|| (cfi-symbol-copy-from-common lsym cvm-value) ||#
                   lsym)))
            ((cvm-package-p cvm-value) 
             (or (ld-get cvm-value)
                 (ld-put cvm-value
                         (let* ((packname (cfi-copy-from-common 
                                           (cvm-package-name cvm-value)))
                                (pack (find-package packname)))
                           (or pack (make-package packname #|TODO:nickname|#))))))
            (t           (error "structure unsupported yet."))))
         ((#.ct-vector #.ct-vector-fp)
          (cond
            ((cvm-string-p cvm-value)
             (or (ld-get cvm-value)
                 (ld-put cvm-value (cvm-string-value cvm-value))))
            (t           (error "vector unsupported yet."))))
         ((#.ct-array) (error "array unsupported yet."))
         (otherwise (cfi-copy-from-common (ldb +ex-cons+ ref-value))))))
    ((#.ct-free-block) (error "Trying to convert a free block at ~8,'0X"
                              (cvm-deref cvm-value)))
    (otherwise         (error "Invalid type code (~D)" 
                              (or typecode (cvm-type-of cvm-value))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMMON USER-LEVEL API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *defined-common-variables* '())

;; Let's define a common list of common variables:
(define-symbol-macro *common-variables* (get-common '*common-variables*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation 'common-variables* 'variable)
        "List of symbols naming common variables."))


(defun common-initialize (memory)
  "
DOES:           Get a shared memory block and a semaphore set, 
                and initialize a shared heap.
KEY:            An IPC key, built with IPC:MAKE-KEY, used to get 
                the shared memory block ('common' memory) and the
                semaphore set from which a semaphore is needed.
COMMON-SEMAPHORE-INDEX:
                The index in the semphore set identified with the KEY
                of the semaphore to be used for common memory mutex.
                Other semaphores can be used by the application.
HEAP-SIZE:      The size in byte of the shared memory block.
SEMSET-SIZE:    The size (number of semaphores) in the semaphore set.
NOTE:           The shared memory block and the semaphore set may
                be created before hand or not. This function will
                create them if needed.
"
  (gc-initialize memory)
  (set-common '*common-variables* '(*common-variables*))
  (pushnew '*common-variables* *defined-common-variables*))


(defmacro defcommon (symbol &optional value docstring)
  "
DOES:           Defines a common variable, ie. a variable whose value
                is stored in the common heap. Everytime the variable
                is read, the value is copied from the common heap into
                the lisp heap, and vice-versa everytime it's written.
SYMBOL:         The name of the common variable. It'll be a symbol macro.
VALUE:          The initial value copied to the common variable.
DOCSTRING:      A variable documentation string attached to the SYMBOL.
NOTE:           A common variable named *COMMON-VARIABLES* contains a list
                of all common variables names (symbols).
NOTE:           Copying is done taking into account circles.
                The copying for the following object types is implemented:
                   (signed-byte 56)
                   single-float
                   nil
                   cons
                   character
                   string
                   symbol  ; only the package and name
                   package ; only the name. keeps the list of common variables
                           ; in the package.
"
  `(progn
     (define-symbol-macro ,symbol (get-common ',symbol))
     ,(when docstring `(setf (documentation ',symbol 'variable) ,docstring))
     (set-common ',symbol ,value)
     (pushnew ',symbol *common-variables*)
     (pushnew ',symbol *defined-common-variables*)
     ',symbol))


(defun update-common-variables ()
  "
DOES:           Defines a symbol-macro for the common variables available
                in the common heap that don't have beed defined in this process.
"
  (dolist (var (set-difference *common-variables* *defined-common-variables*))
    (eval `(define-symbol-macro ,var (get-common ',var)))
    (pushnew 'var *defined-common-variables*)))


(defun get-common (symbol)
  "
DOES:           Copies the value of the common variable from the common heap
                to the lisp heap and return this lisp value.
SYMBOL:         The name of the common variable. It'll be a symbol macro.
"
  (with-loop-detection
      (with-common-lock
          (unwind-protect
               (let*((pack (cfi-copy-to-common (symbol-package symbol)))
                     (symb (cvm-find-symbol (symbol-name symbol) pack)))
                 (if symb
                     (cfi-copy-from-common (cvm-symbol-value symb))
                     (error "There's no common symbol ~S" symbol)))
            (gc-reset-generation)))))


(defun set-common (symbol value)
  "
DOES:           Copies the given lisp VALUE into the common variable in
                the common heap. Return this lisp value.
SYMBOL:         The name of the common variable. It'll be a symbol macro.
VALUE:          The lisp value to be copied into the common heap and
                bound to the common variable.
"
  (with-loop-detection
      (with-common-lock
          (unwind-protect
               (let ((sym (cfi-copy-to-common symbol))
                     (val (cfi-copy-to-common value)))
                 (if sym
                     (cvm-symbol-set-value sym val)
                     (error "There's no common symbol ~S" symbol)))
            (gc-reset-generation))))
  value)


(defsetf get-common set-common)



#|
(defcommon *test* :test "A test common variable")
|#


#||

(load"loader")
(load"common")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.COMMON")
(setf COM.INFORMATIMAGO.COMMON-LISP.COMMON::*debug* '(:range :check)
      COM.INFORMATIMAGO.COMMON-LISP.COMMON::*check-args* '())

(common-initialize (ipc:make-key "/" (susv3:getpid)) 0 :heap-size (* 8 #x800))

(COM.INFORMATIMAGO.COMMON-LISP.COMMON::gc-sign 0)

(setf COM.INFORMATIMAGO.COMMON-LISP.COMMON::*debug*
      '(:range :check :objects :ng :gc)
      COM.INFORMATIMAGO.COMMON-LISP.COMMON::*check-args*
      '(:dump-bitmap t))

(defcommon *test* :test "A test common variable")
(setf *test* (com.informatimago.common-lisp.cesarum.list:iota 1000))

||#


;;;; THE END ;;;;
