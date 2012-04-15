;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               example-soft-opcodes.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Example of a processor with illegal instruction traps allowing
;;;;    one to implement missing instructions in software.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2011-01-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2011 - 2011
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


;;; First we define the processor (as a virtual machine here, but the
;;; same could be done in real hardware).

(deftype word () '(unsigned-byte 64))

(defparameter *memory-size* 65536 "Number of words in the memory") 

(defstruct machine 
  (pc          0 :type word)
  (accumulator 0 :type word)
  (memory (make-array *memory-size* :element-type 'word :initial-element 0)))

(defun opcode (word)  (ldb (byte  8 56) word))
(defun src    (word)  (ldb (byte 28 28) word))
(defun dst    (word)  (ldb (byte 28  0) word))

(defconstant +word-mask+ #xffffffffffffffff)
(defconstant +save-address+             1)
(defconstant +illegal-instruction-trap+ 2)
(defconstant +invalid-address-trap+     3)



(defun valid-address-p (address)
  (and (<= 0 address) (< address *memory-size*)))

(defun word (value) (logand value +word-mask+))

(defun load-machine (machine address words)
  "Loads a code vector WORDS in the MACHINE memory at the given ADDRESS."
  (replace (machine-memory machine) words :start1 address)
  machine)

(defun dump-machine (machine address length)
  "Dumps the MACHINE memory starting from ADDRESS, for LENGTH words."
  (loop
     :repeat length
     :for a :from address
     :do (format t "~4,'0X: ~16,'0X  ~:*~D~%"
                 a (aref (machine-memory machine) a)))
  machine)

(defun run (machine &key (verbose nil) (step nil))
  "Run the machine.
When verbose is true, prints the instructions and traps executed.
When step is true, executes only one step."
  (loop :named :machine :do
     (let ((pc (machine-pc machine)))
       (if (valid-address-p pc)
           (let* ((instruction (aref (machine-memory machine) pc))
                  (opcode (opcode instruction))
                  (src    (src    instruction))
                  (dst    (dst    instruction)))
             (flet ((trap (machine trap-address)
                      (when verbose
                        (format t "~4,'0X: ~A trap~%" (machine-pc machine)
                                (ecase trap-address
                                  (#.+illegal-instruction-trap+ "Illegal instruction")
                                  (#.+invalid-address-trap+     "Invalid address"))))
                      (shiftf (aref (machine-memory machine) +save-address+)
                              (machine-pc machine)
                              (aref (machine-memory machine) trap-address))))
               #+emacs (progn
                         (cl-indent 'src-instruction 1)
                         (cl-indent 'dst-instruction 1)
                         (cl-indent 'acc-instruction 1)
                         (cl-indent 'jmp-instruction 1))
               (macrolet ((src-instruction (instruction-form &body body)
                            `(cond
                               ((and verbose (format t "~4,'0X: ~S~%" pc ,instruction-form)))
                               ((not (valid-address-p src))
                                (trap machine +invalid-address-trap+))
                               ((not (zerop dst))
                                (trap machine +invalid-address-trap+))
                               (t
                                ,@body
                                (incf (machine-pc machine)))))
                          (dst-instruction (instruction-form &body body)
                            `(cond
                               ((and verbose (format t "~4,'0X: ~S~%" pc ,instruction-form)))
                               ((not (zerop src))
                                (trap machine +invalid-address-trap+))
                               ((not (valid-address-p dst))
                                (trap machine +invalid-address-trap+))
                               (t
                                ,@body
                                (incf (machine-pc machine)))))
                          (acc-instruction (instruction-form &body body)
                            `(cond
                               ((and verbose (format t "~4,'0X: ~S~%" pc ,instruction-form)))
                               ((not (zerop src))
                                (trap machine +invalid-address-trap+))
                               ((not (zerop dst))
                                (trap machine +invalid-address-trap+))
                               (t
                                ,@body
                                (incf (machine-pc machine)))))
                          (jmp-instruction (instruction-form &body body)
                            `(cond
                               ((and verbose (format t "~4,'0X: ~S~%" pc ,instruction-form)))
                               ((not (valid-address-p src))
                                (trap machine +invalid-address-trap+))
                               ((not (zerop dst))
                                (trap machine +invalid-address-trap+))
                               (t
                                ,@body))))
                 (if (zerop (ldb (byte 4 0) opcode))
                     (case (ldb (byte 4 4) opcode)
                       ;; Notice not all instructions are implemented:
                       ;; 0000 move src,dst
                       ;; 0001 load src,acc
                       (#b0001
                        (src-instruction `(load ,src acc)
                          (setf (machine-accumulator machine)
                                (aref (machine-memory machine) src))))
                       ;; 0010 store acc,dst
                       (#b0010
                        (dst-instruction `(store acc ,dst)
                          (setf (aref (machine-memory machine) dst)
                                (machine-accumulator machine))))
                       ;; 0100 add src,acc
                       (#b0100
                        (src-instruction `(add ,src acc)
                          (setf (machine-accumulator machine)
                                (word (+ (machine-accumulator machine)
                                         (aref (machine-memory machine) src))))))
                       ;; 0101 sub src,acc
                       ;; 0110 mul src,acc
                       ;; 0111 div src,acc
                       ;; 1000 not acc
                       (#b1000
                        (acc-instruction `(not acc)
                          (setf (machine-accumulator machine)
                                (word (lognot (machine-accumulator machine))))))
                       ;; 1001 neg acc
                       ;; (#b10100000 (lshift src acc))
                       (#b1010
                        (src-instruction `(lshift ,src acc)
                          (setf (machine-accumulator machine)
                                (word (ash (machine-accumulator machine)
                                           (aref (machine-memory machine) src))))))
                       ;; (#b10110000 (rshift src acc))
                       (#b1011
                        (src-instruction `(rshift ,src acc)
                          (setf (machine-accumulator machine)
                                (word (ash (machine-accumulator machine)
                                           (- (aref (machine-memory machine) src)))))))
                       ;; (#b11000000 (and src acc))
                       (#b1100
                        (src-instruction `(and ,src acc)
                          (setf (machine-accumulator machine)
                                (word (logand (machine-accumulator machine)
                                              (aref (machine-memory machine) src))))))
                       ;; (#b11010000 (or  src acc))
                       (#b1101
                        (src-instruction `(or ,src acc)
                          (setf (machine-accumulator machine)
                                (word (logior (machine-accumulator machine)
                                              (aref (machine-memory machine) src))))))
                       ;; 1110 jzero src
                       (#b1110
                        (jmp-instruction `(jzero ,src)
                          (if (zerop (machine-accumulator machine))
                              (setf (machine-pc machine) src)
                              (incf (machine-pc machine)))))
                       ;; 1111 jump  src
                       (#b1111
                        (jmp-instruction `(jump ,src)
                          (setf (machine-pc machine) src)))
                       (otherwise
                        (trap machine +illegal-instruction-trap+)))
                     (case opcode
                       (#b11111111
                        (jmp-instruction `(halt)
                          (return-from :machine)))
                       (otherwise
                        (trap machine +illegal-instruction-trap+)))))))
           (trap machine +invalid-address-trap+)))
     :until step))


;;; Next we write a little LAP assembler.
;;; Notice we may assemble instructions that are not implemented (yet)
;;; in hardware.

(defparameter *instructions*
  '((#b00000000 (move src dst))
    (#b00010000 (load src acc))
    (#b00100000 (store acc dst))
    (#b01000000 (add src acc))
    (#b01010000 (sub src acc))
    (#b01100000 (mul src acc))
    (#b01110000 (div src acc))
    (#b10000000 (not acc))
    (#b10010000 (neg acc))
    (#b10100000 (lshift src acc))
    (#b10110000 (rshift src acc))
    (#b11000000 (and src acc))
    (#b11010000 (or  src acc))
    (#b11100000 (jzero src))
    (#b11110000 (jump src))
    (#b11111111 (halt))))


(defun encode-instruction (op src dst)
  (word (dpb op (byte 8 56) (dpb src (byte 28 28) dst))))

(defun sym-eval (expression bindings)
  (progv
      (mapcar (function car) bindings)
      (mapcar (function cdr) bindings)
    (eval expression)))

(defun validate-operand (pattern operand symtable)
  (word
   (ecase pattern
     ((nil)
      (if (null operand)
          0
          (error "Expected no operand, got ~S in" operand)))
     ((acc)
      (if (eql operand 'acc)
          0
          (error "Expected ACC, got ~S in" operand)))
     ((src dst)
      (sym-eval operand symtable)))))

(defun assemble (address lap)
  "
ADDRESS is the base address of the code.
There's no operator to change the current address in a LAP list.

LAP is a list of symbols or LAP sexps.  Symbols are equaled to the
current address and entered in the symbol table during the first
phase.  LAP sexps may be either one of the instructions defined in
*INSTRUCTIONS*, or one of the two special operator defined below.

In addition to the instruction opcodes, there is:
    (eql symbol expr)
to enter a symbol in the symbol table. expr is evaluated right away.

And:
    (dcl expr ...)
to store arbitrary values in memory. * is the address of the first one.

The SRC, DST, and EXPR forms are evaluated in Lisp, in a context where
the symbols in the assembler symbol table are dynamically bound to
their value, with PROGV and EVAL.  Any lisp expression may be used
here, executed during the first phase for EQL, and the second phase
for the other LAP forms.

Return the code vector, and the symbol table,
an a-list of (symbol . values).
"
  (loop
     :with symtable ; a-list symbol -> address
     = (loop
          :named first-phase
          :with symtable = '()
          :with address = address
          :for statement :in lap
          :if (atom statement) :do (push (cons statement address) symtable)
          :else :do (case (first statement)
                      ((eql)
                       (unless (symbolp (second statement))
                         (error "EQL requires a symbol as first argument instead of ~S"
                                (second statement)))
                       (push (cons (second statement)
                                   (sym-eval (third statement)
                                             (acons '* address symtable)))
                             symtable))
                      ((dcl) (incf address (length (rest statement))))
                      (otherwise (incf address)))
          :finally (return-from first-phase symtable))
     :with address = address
     :with code = '()
     :for statement :in lap
     :do (when (listp statement)
           (let* ((op (first statement))
                  (instruction (find op *instructions* :key (function caadr))))
             (handler-case
                 (cond
                   ((eql op 'eql))
                   ((eql op 'dcl)
                    (let ((symtable  (acons '* address symtable)))
                      (dolist (expr (rest statement))
                        (push (sym-eval expr symtable) code))
                      (incf address (length (rest statement)))))
                   (instruction
                    (push
                     (encode-instruction
                      (first instruction)
                      (validate-operand (second (second instruction)) (second statement)
                                        (acons '* address symtable))
                      (validate-operand (third  (second instruction)) (third  statement)
                                        (acons '* address symtable)))
                     code)
                    (incf address))
                   (t
                    (error "Invalid opcode in lap statement")))
               (error (err)
                 (error "~A in ~A: ~S" err address statement)))))
     :finally (return (values (coerce (nreverse code) 'vector)
                              symtable))))


;;; Finally, we can write a few missing instructions in assembler,
;;; load them in the machine, and write a program using them.
;;;
;;; In real machines, these handlers are usually priviledged code, but
;;; nothing prevents a system to let application define their own
;;; opcodes too.


(defvar *machine* nil
  "for debugging purpose, we bind the last machine used here.")

(multiple-value-bind (code symtable)
    (assemble
     0
     '(
       ;; Address 0 contains a jump to start, the machine boots with pc=0
       (jump start)

       ;; Address 1 is where the PC of the instruction that is invalid
       ;; or accessed an invalid address is stored.
       return-pc           (dcl 0)

       ;; Address 2 is where the address of the illegal instruction
       ;; trap handler is stored.
       illegal-instruction (dcl process-illegal-instruction)

       ;; Address 3 is where the address of the invalid address
       ;; trap handler is stored.
       invalid-address     (dcl process-invalid-address)

       ;; Let's start with the illegal instruction handler:
       process-illegal-instruction
       
       (store acc save-acc)                ; save accumulator.
       ;; We really would need a stack and indirect addressing...
       ;; but this is to show that it's possible to start with a very
       ;; crude machine, and we may add higher level instructions first
       ;; in software.
                                        ; save the return address
       (load return-pc acc)                ; get the instruction address
       (add one acc)                       ; add one for return address
       (lshift src-offset acc)    ; store it into the return instruction.
       (add return-op acc)
       (store acc return-instruction)
       
       (load return-pc acc)          ; get the instruction address
       (lshift src-offset acc)       ; store it into the load instruction
       (add load-op acc)
       (store acc load-instruction)
       
       load-instruction (load 0 acc)       ; get the instruction
       (store acc instruction)
       (and opcode-lo-mask acc)
       (jzero lo-ok)
       (jump really-illegal-instruction)

       lo-ok
       (load instruction acc)
       (and opcode-hi-mask acc)
       (rshift opcode-hi-offset acc)
       (not acc)               ; remember, we don't have NEG nor SUB yet.
       (add one acc)           ; so we get two-complement of opcode
       (store acc -op-hi)

       (load -op-hi acc)
       (add move-op acc)
       (jzero move)

       (load -op-hi acc)
       (add sub-op acc)
       (jzero sub)

       (load -op-hi acc)
       (add neg-op acc)
       (jzero neg)

       (load -op-hi acc)
       (add or-op acc)
       (jzero or)

       move-op (dcl #b0000)
       sub-op  (dcl #b0101)
       mul-op  (dcl #b0110)
       div-op  (dcl #b0111)
       neg-op  (dcl #b1001)
       or-op   (dcl #b1101)
       
       -op-hi           (dcl 0)
       opcode-hi-mask   (dcl #xf000000000000000)
       opcode-hi-offset (dcl 60)
       opcode-lo-mask   (dcl #x0f00000000000000)


       ;; Not implemented yet, we'd need an OS to do something with those.
       really-illegal-instruction (halt)
       process-invalid-address    (halt)
       ;; eg. process-invalid-address could go to a VM manager.
       
       neg
       (load save-acc acc)
       (not acc)
       (add one acc)
       (store acc save-acc)
       (jump return)
       

       sub
       (load instruction acc)              ; load the src data
       (and src-mask acc)                  ; maskout the source address
       (add load-op acc)
       (store acc sub-load-data-instruction)
       sub-load-data-instruction (load 0 acc)
       (not acc)
       (add one acc)
       (add save-acc acc)
       (store acc save-acc)
       (jump return)
       
       or                ; implemented as (not (and (not mem) (not acc)))
       (load instruction acc)              ; load the src data
       (and src-mask acc)                  ; maskout the source address
       (add load-op acc)
       (store acc or-load-data-instruction)
       or-load-data-instruction (load 0 acc)
       (not acc)
       (store acc or-op)
       (load save-acc acc)
       (not acc)
       (and or-op acc)
       (not acc)
       (store acc save-acc)
       (jump return)
       or-op (dcl 0)
       
       move                                ; move src dst
       (load instruction acc)              ; load the src data:
       (and src-mask acc)                  ; maskout the source address
       (add load-op acc)
       (store acc move-load-data-instruction)
       move-load-data-instruction (load 0 acc)
       (store acc data)
       (load instruction acc)              ; store the data into the dst
       (and dst-mask acc)
       (add store-op acc)
       (store acc store-data-instruction)
       (load data acc)
       store-data-instruction (store acc 0)

       return   
       (load save-acc acc)                 ; restore the accumulator:
       return-instruction (jump 0)         ; and continue

       return-op   (jump 0)
       load-op     (load 0 acc)
       store-op    (store acc 0)
       
       src-offset  (dcl 28)
       src-mask*   (dcl #xff0000000fffffff)
       src-mask    (dcl #x00fffffff0000000)
       dst-mask    (dcl #x000000000fffffff)
       one         (dcl 1)
       save-acc    (dcl 0)
       instruction (dcl 0)
       data        (dcl 0)


       start ; let's test the new opcodes:

       (move src dst)
       (move (+ src 1) (+ dst 1))
       (move (+ src 2) (+ dst 2))
       (move (+ src 3) (+ dst 3))

       (load pi acc)
       (neg acc)
       (store acc -pi)

       (load a acc)
       (or b acc)
       (store acc a-or-b)

       (load pi acc)
       (sub pi-1 acc)
       (store acc pi-diff)

       (halt)

       ;; We will dump the memory from src to end when the machine
       ;; halts:
       
       src     (dcl #x1122334455667788 #xff #xdeadbeef #xfeedbabe)
       dst     (dcl 0 0 0 0)
       pi      (dcl 3141592653589)
       pi-1    (dcl 2141592653589)
       pi-diff (dcl 1000000000000)
       -pi     (dcl 0)
       a       (dcl #xffffffff00000000)
       b       (dcl #xffff0000ffff0000)
       a-or-b  (dcl 0)
       end     (dcl 0)

       ))
  (let ((start (cdr (assoc 'src symtable)))
        (end   (cdr (assoc 'end symtable)))
        (machine (make-machine)))
    (print code) (terpri) (finish-output)
    (setf *machine* machine)
    (load-machine machine 0 code)
    (let ((*print-length* 10)) (print machine)) (terpri) (finish-output)
    (run machine :verbose t)
    (dump-machine machine start (- end start))))


#||
0000: (JUMP 103)
0067: Illegal instruction trap
0004: (STORE ACC 100)
0005: (LOAD 1 ACC)
0006: (ADD 99 ACC)
0007: (LSHIFT 95 ACC)
0008: (ADD 92 ACC)
0009: (STORE ACC 91)
000A: (LOAD 1 ACC)
000B: (LSHIFT 95 ACC)
000C: (ADD 93 ACC)
000D: (STORE ACC 14)
000E: (LOAD 103 ACC)
000F: (STORE ACC 101)
0010: (AND 46 ACC)
0011: (JZERO 19)
0013: (LOAD 101 ACC)
0014: (AND 44 ACC)
0015: (RSHIFT 45 ACC)
0016: (NOT ACC)
0017: (ADD 99 ACC)
0018: (STORE ACC 43)
0019: (LOAD 43 ACC)
001A: (ADD 37 ACC)
001B: (JZERO 78)
004E: (LOAD 101 ACC)
004F: (AND 97 ACC)
0050: (ADD 93 ACC)
0051: (STORE ACC 82)
0052: (LOAD 117 ACC)
0053: (STORE ACC 102)
0054: (LOAD 101 ACC)
0055: (AND 98 ACC)
0056: (ADD 94 ACC)
0057: (STORE ACC 89)
0058: (LOAD 102 ACC)
0059: (STORE ACC 121)
005A: (LOAD 100 ACC)
005B: (JUMP 104)
0068: Illegal instruction trap
0004: (STORE ACC 100)
0005: (LOAD 1 ACC)
0006: (ADD 99 ACC)
0007: (LSHIFT 95 ACC)
0008: (ADD 92 ACC)
0009: (STORE ACC 91)
000A: (LOAD 1 ACC)
000B: (LSHIFT 95 ACC)
000C: (ADD 93 ACC)
000D: (STORE ACC 14)
000E: (LOAD 104 ACC)
000F: (STORE ACC 101)
0010: (AND 46 ACC)
0011: (JZERO 19)
0013: (LOAD 101 ACC)
0014: (AND 44 ACC)
0015: (RSHIFT 45 ACC)
0016: (NOT ACC)
0017: (ADD 99 ACC)
0018: (STORE ACC 43)
0019: (LOAD 43 ACC)
001A: (ADD 37 ACC)
001B: (JZERO 78)
004E: (LOAD 101 ACC)
004F: (AND 97 ACC)
0050: (ADD 93 ACC)
0051: (STORE ACC 82)
0052: (LOAD 118 ACC)
0053: (STORE ACC 102)
0054: (LOAD 101 ACC)
0055: (AND 98 ACC)
0056: (ADD 94 ACC)
0057: (STORE ACC 89)
0058: (LOAD 102 ACC)
0059: (STORE ACC 122)
005A: (LOAD 100 ACC)
005B: (JUMP 105)
0069: Illegal instruction trap
0004: (STORE ACC 100)
0005: (LOAD 1 ACC)
0006: (ADD 99 ACC)
0007: (LSHIFT 95 ACC)
0008: (ADD 92 ACC)
0009: (STORE ACC 91)
000A: (LOAD 1 ACC)
000B: (LSHIFT 95 ACC)
000C: (ADD 93 ACC)
000D: (STORE ACC 14)
000E: (LOAD 105 ACC)
000F: (STORE ACC 101)
0010: (AND 46 ACC)
0011: (JZERO 19)
0013: (LOAD 101 ACC)
0014: (AND 44 ACC)
0015: (RSHIFT 45 ACC)
0016: (NOT ACC)
0017: (ADD 99 ACC)
0018: (STORE ACC 43)
0019: (LOAD 43 ACC)
001A: (ADD 37 ACC)
001B: (JZERO 78)
004E: (LOAD 101 ACC)
004F: (AND 97 ACC)
0050: (ADD 93 ACC)
0051: (STORE ACC 82)
0052: (LOAD 119 ACC)
0053: (STORE ACC 102)
0054: (LOAD 101 ACC)
0055: (AND 98 ACC)
0056: (ADD 94 ACC)
0057: (STORE ACC 89)
0058: (LOAD 102 ACC)
0059: (STORE ACC 123)
005A: (LOAD 100 ACC)
005B: (JUMP 106)
006A: Illegal instruction trap
0004: (STORE ACC 100)
0005: (LOAD 1 ACC)
0006: (ADD 99 ACC)
0007: (LSHIFT 95 ACC)
0008: (ADD 92 ACC)
0009: (STORE ACC 91)
000A: (LOAD 1 ACC)
000B: (LSHIFT 95 ACC)
000C: (ADD 93 ACC)
000D: (STORE ACC 14)
000E: (LOAD 106 ACC)
000F: (STORE ACC 101)
0010: (AND 46 ACC)
0011: (JZERO 19)
0013: (LOAD 101 ACC)
0014: (AND 44 ACC)
0015: (RSHIFT 45 ACC)
0016: (NOT ACC)
0017: (ADD 99 ACC)
0018: (STORE ACC 43)
0019: (LOAD 43 ACC)
001A: (ADD 37 ACC)
001B: (JZERO 78)
004E: (LOAD 101 ACC)
004F: (AND 97 ACC)
0050: (ADD 93 ACC)
0051: (STORE ACC 82)
0052: (LOAD 120 ACC)
0053: (STORE ACC 102)
0054: (LOAD 101 ACC)
0055: (AND 98 ACC)
0056: (ADD 94 ACC)
0057: (STORE ACC 89)
0058: (LOAD 102 ACC)
0059: (STORE ACC 124)
005A: (LOAD 100 ACC)
005B: (JUMP 107)
006B: (LOAD 125 ACC)
006C: Illegal instruction trap
0004: (STORE ACC 100)
0005: (LOAD 1 ACC)
0006: (ADD 99 ACC)
0007: (LSHIFT 95 ACC)
0008: (ADD 92 ACC)
0009: (STORE ACC 91)
000A: (LOAD 1 ACC)
000B: (LSHIFT 95 ACC)
000C: (ADD 93 ACC)
000D: (STORE ACC 14)
000E: (LOAD 108 ACC)
000F: (STORE ACC 101)
0010: (AND 46 ACC)
0011: (JZERO 19)
0013: (LOAD 101 ACC)
0014: (AND 44 ACC)
0015: (RSHIFT 45 ACC)
0016: (NOT ACC)
0017: (ADD 99 ACC)
0018: (STORE ACC 43)
0019: (LOAD 43 ACC)
001A: (ADD 37 ACC)
001B: (JZERO 78)
001C: (LOAD 43 ACC)
001D: (ADD 38 ACC)
001E: (JZERO 54)
001F: (LOAD 43 ACC)
0020: (ADD 41 ACC)
0021: (JZERO 49)
0031: (LOAD 100 ACC)
0032: (NOT ACC)
0033: (ADD 99 ACC)
0034: (STORE ACC 100)
0035: (JUMP 90)
005A: (LOAD 100 ACC)
005B: (JUMP 109)
006D: (STORE ACC 128)
006E: (LOAD 129 ACC)
006F: (OR 130 ACC)
0070: (STORE ACC 131)
0071: (LOAD 125 ACC)
0072: Illegal instruction trap
0004: (STORE ACC 100)
0005: (LOAD 1 ACC)
0006: (ADD 99 ACC)
0007: (LSHIFT 95 ACC)
0008: (ADD 92 ACC)
0009: (STORE ACC 91)
000A: (LOAD 1 ACC)
000B: (LSHIFT 95 ACC)
000C: (ADD 93 ACC)
000D: (STORE ACC 14)
000E: (LOAD 114 ACC)
000F: (STORE ACC 101)
0010: (AND 46 ACC)
0011: (JZERO 19)
0013: (LOAD 101 ACC)
0014: (AND 44 ACC)
0015: (RSHIFT 45 ACC)
0016: (NOT ACC)
0017: (ADD 99 ACC)
0018: (STORE ACC 43)
0019: (LOAD 43 ACC)
001A: (ADD 37 ACC)
001B: (JZERO 78)
001C: (LOAD 43 ACC)
001D: (ADD 38 ACC)
001E: (JZERO 54)
0036: (LOAD 101 ACC)
0037: (AND 97 ACC)
0038: (ADD 93 ACC)
0039: (STORE ACC 58)
003A: (LOAD 126 ACC)
003B: (NOT ACC)
003C: (ADD 99 ACC)
003D: (ADD 100 ACC)
003E: (STORE ACC 100)
003F: (JUMP 90)
005A: (LOAD 100 ACC)
005B: (JUMP 115)
0073: (STORE ACC 127)
0074: (HALT)
0075: 1122334455667788  1234605616436508552
0076: 00000000000000FF  255
0077: 00000000DEADBEEF  3735928559
0078: 00000000FEEDBABE  4276992702
0079: 1122334455667788  1234605616436508552
007A: 00000000000000FF  255
007B: 00000000DEADBEEF  3735928559
007C: 00000000FEEDBABE  4276992702
007D: 000002DB75839F15  3141592653589
007E: 000001F2A0DE8F15  2141592653589
007F: 000000E8D4A51000  1000000000000
0080: FFFFFD248A7C60EB  18446740932116898027
0081: FFFFFFFF00000000  18446744069414584320
0082: FFFF0000FFFF0000  18446462603027742720
0083: FFFFFFFFFFFF0000  18446744073709486080

||#
