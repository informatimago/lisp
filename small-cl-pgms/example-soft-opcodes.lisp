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
;;;;    2014-11-30 <PJB> Added a few soft-ops (call, ret, etc). Added disasembler.
;;;;    2011-01-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2011 - 2015
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
(defpackage "EXAMPLE-SOFT-OPCODES"
  (:use "COMMON-LISP")
  (:shadow "DISASSEMBLE"))
(in-package "EXAMPLE-SOFT-OPCODES")


#+emacs (progn
          (cl-indent 'src-instruction 1)
          (cl-indent 'dst-instruction 1)
          (cl-indent 'acc-instruction 1)
          (cl-indent 'jmp-instruction 1))



;;; First we define the processor (as a virtual machine here, but the
;;; same could be done in real hardware).
(defconstant +bytes-per-word+ 8)
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

(defparameter *instructions*
  '((#b00000000 (move src dst))
    (#b00010000 (load src acc))
    (#b00100000 (store acc dst))
    (#b00011111 (input  src acc)) ; src is an I/O channel
    (#b00101111 (output acc dst)) ; dst is an I/O channel
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
    (#b11100000 (jzero dst))
    (#b11110000 (jump dst))
    (#b11110010 (call dst))
    (#b11110011 (ret))
    (#b11110100 (pusha))
    (#b11110101 (push src))
    (#b11110110 (popa))
    (#b11110111 (pop  dst))
    (#b11111111 (halt))))

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
    :for a :from address
    :repeat length
    :do (format t "~4,'0X: ~16,'0X  ~:*~D~%"
                a (aref (machine-memory machine) a)))
  machine)

(defun channel-to-stream (channel)
  (case channel
    ((0) *standard-input*)
    ((1) *standard-output*)
    ((2) *error-output*)
    ((3) *trace-output*)
    ((4) *terminal-io*)
    ((5) *query-io*)
    ((6) *debug-io*)
    (otherwise *terminal-io*)))

(defun run (machine &key (verbose nil) (step nil) (symtable nil))
  "Run the machine.
When verbose is true, prints the instructions and traps executed.
When step is true, executes only one step."
  (loop :named :machine :do
    (let ((pc (machine-pc machine)))
      (flet ((trap (machine trap-address)
               (when verbose
                 (format *trace-output* "~4,'0X: ~A trap (~16,'0X)~%" (machine-pc machine)
                         (ecase trap-address
                           (#.+illegal-instruction-trap+ "Illegal instruction")
                           (#.+invalid-address-trap+     "Invalid address"))
                         (aref (machine-memory machine) (machine-pc machine))))
               ;; pc -> save, trap -> pc
               (shiftf (aref (machine-memory machine) +save-address+)
                       (machine-pc machine)
                       (aref (machine-memory machine) trap-address)))
             (trace-instruction (instruction)
               (when verbose
                 (let ((*standard-output* *trace-output*))
                   (print-instruction pc instruction symtable)))))
        (if (valid-address-p pc)
            (let* ((instruction (aref (machine-memory machine) pc))
                   (opcode (opcode instruction))
                   (src    (src    instruction))
                   (dst    (dst    instruction)))
              (macrolet ((src-instruction (instruction-form &body body)
                           `(progn
                              (trace-instruction ,instruction-form)
                              (cond
                                ((not (valid-address-p src))
                                 (trap machine +invalid-address-trap+))
                                ((not (zerop dst))
                                 (trap machine +invalid-address-trap+))
                                (t
                                 ,@body
                                 (incf (machine-pc machine))))))
                         (dst-instruction (instruction-form &body body)
                           `(progn
                              (trace-instruction ,instruction-form)
                              (cond
                                ((not (zerop src))
                                 (trap machine +invalid-address-trap+))
                                ((not (valid-address-p dst))
                                 (trap machine +invalid-address-trap+))
                                (t
                                 ,@body
                                 (incf (machine-pc machine))))))
                         (acc-instruction (instruction-form &body body)
                           `(progn
                              (trace-instruction ,instruction-form)
                              (cond
                                ((not (zerop src))
                                 (trap machine +invalid-address-trap+))
                                ((not (zerop dst))
                                 (trap machine +invalid-address-trap+))
                                (t
                                 ,@body
                                 (incf (machine-pc machine))))))
                         (jmp-instruction (instruction-form &body body)
                           `(progn
                              (trace-instruction ,instruction-form)
                              (cond
                                ((not (zerop src))
                                 (trap machine +invalid-address-trap+))
                                ((not (valid-address-p dst))
                                 (trap machine +invalid-address-trap+))
                                (t
                                 ,@body)))))
                
                (case opcode
                  ;; Notice not all instructions are implemented:
                  ;; 0000 move src,dst
                  ;; 0001 load src,acc
                  (#b00010000
                   (src-instruction `(load ,src acc)
                     (setf (machine-accumulator machine)
                           (aref (machine-memory machine) src))))
                  (#b00011111
                   (src-instruction `(input ,src acc)
                     (setf (machine-accumulator machine)
                           (code-char (read-char (channel-to-stream src))))))
                  ;; 0010 store acc,dst
                  (#b00100000
                   (dst-instruction `(store acc ,dst)
                     (setf (aref (machine-memory machine) dst)
                           (machine-accumulator machine))))
                  (#b00101111
                   (dst-instruction `(output acc ,dst)
                     (princ (code-char (machine-accumulator machine)) (channel-to-stream dst))))
                  ;; 0100 add src,acc
                  (#b01000000
                   (src-instruction `(add ,src acc)
                     (setf (machine-accumulator machine)
                           (word (+ (machine-accumulator machine)
                                    (aref (machine-memory machine) src))))))
                  ;; 0101 sub src,acc
                  ;; 0110 mul src,acc
                  ;; 0111 div src,acc
                  ;; 1000 not acc
                  (#b10000000
                   (acc-instruction `(not acc)
                     (setf (machine-accumulator machine)
                           (word (lognot (machine-accumulator machine))))))
                  ;; 1001 neg acc
                  ;; (#b10100000 (lshift src acc))
                  (#b10100000
                   (src-instruction `(lshift ,src acc)
                     (setf (machine-accumulator machine)
                           (word (ash (machine-accumulator machine)
                                      (aref (machine-memory machine) src))))))
                  ;; (#b10110000 (rshift src acc))
                  (#b10110000
                   (src-instruction `(rshift ,src acc)
                     (setf (machine-accumulator machine)
                           (word (ash (machine-accumulator machine)
                                      (- (aref (machine-memory machine) src)))))))
                  ;; (#b11000000 (and src acc))
                  (#b11000000
                   (src-instruction `(and ,src acc)
                     (setf (machine-accumulator machine)
                           (word (logand (machine-accumulator machine)
                                         (aref (machine-memory machine) src))))))
                  ;; (#b11010000 (or  src acc))
                  (#b11010000
                   (src-instruction `(or ,src acc)
                     (setf (machine-accumulator machine)
                           (word (logior (machine-accumulator machine)
                                         (aref (machine-memory machine) src))))))
                  ;; 1110 jzero dst
                  (#b11100000
                   (jmp-instruction `(jzero ,dst)
                     (if (zerop (machine-accumulator machine))
                         (setf (machine-pc machine) dst)
                         (incf (machine-pc machine)))))
                  ;; 1111 jump  dst
                  (#b11110000
                   (jmp-instruction `(jump ,dst)
                     (setf (machine-pc machine) dst)))
                  (#b11111111
                   (trace-instruction '(halt))
                   (return-from :machine))
                  (otherwise
                   (trap machine +illegal-instruction-trap+)))))
            (trap machine +invalid-address-trap+))))
        :until step))

;;; Next we write a little LAP assembler.
;;; Notice we may assemble instructions that are not implemented (yet)
;;; in hardware.

(defun encode-instruction (op src dst)
  (word (dpb op (byte 8 56) (logior src dst))))

(defun sym-eval (expression bindings)
  (progv
      (mapcar (function car) bindings)
      (mapcar (function cdr) bindings)
    (word (eval expression))))

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
     ((src)
      (dpb (ldb (byte 28 0) (sym-eval operand symtable))
           (byte 28 28) 0))
     ((dst)
      (ldb (byte 28 0) (sym-eval operand symtable))))))

(defun text (items)
  (with-output-to-string (*standard-output*)
    (dolist (item items)
      (etypecase item
        (string  (princ item))
        (integer (princ (code-char item)))))))

(defun encode-ascii (string)
  (loop
    :with word = 0
    :for i :from 0
    :for byte = (if (< i (length string))
                    (char-code (aref string i))
                    0)
    :repeat +bytes-per-word+
    :do (setf word (+ (* 256 word) byte))
    :finally (return word)))

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

Return the code vector, the symbol table (an a-list of (symbol . values)),
and a bit-vector indicating instructions (vs. dcl) in the code vector.
"
  (loop
    :with bits = '()
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
                        ((dcl)
                         (loop :repeat (length (rest statement)) :do (push 0 bits))
                         (incf address (length (rest statement))))
                        ((ascii)
                         (let* ((string (text (rest statement)))
                                (size   (ceiling (length string) +bytes-per-word+)))
                           (loop :repeat size :do (push 0 bits))
                           (incf address size)))
                        (otherwise
                         (push 1 bits)
                         (incf address)))
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
                  ((eql op 'ascii)
                   (let* ((string (text (rest statement)))
                          (size   (ceiling (length string) +bytes-per-word+)))
                     (loop
                       :for i :from 0 :by +bytes-per-word+
                       :repeat size
                       :do (push (encode-ascii (subseq string i (min (length string) (+ i +bytes-per-word+)))) code))
                     (incf address size)))
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
                             symtable
                             (make-array (length bits) :element-type 'bit :initial-contents (nreverse bits))))))

(defun print-instruction (pc sexp &optional symtab)
  (format t "~4,'0X: " pc)
  (loop
    :initially (princ "(")
    :finally   (princ ")")
    :for sep = "" :then " "
    :for item :in sexp
    :do  (princ sep)
         (typecase item
           (symbol (princ item))
           (integer (let ((sym (rassoc item symtab)))
                      (if sym
                          (princ (car sym))
                          (format t "~X" item))))))
  (format t "~%"))

(defun disassemble (address code symtab &optional bits)
  (format t "~%Symbol Table~%--------------------~2%")
  (loop :for (symbol . address) :in (sort (copy-list symtab) (function <) :key (function cdr))
        :do (format t "~32A ~16,'0X  ~:*~D~%" symbol address))
  (format t "~%")
  (format t "~%Program~%--------------------~2%")
  (loop
    :for pc :from address
    :for instruction :across code
    :for bit :across (or bits (make-array (length code) :element-type 'bit :initial-element 1))
    :do (let ((symbols (remove pc symtab :key (function cdr) :test (function /=))))
          (if symbols
              (format t "~{~32A~^~%~} " (mapcar (function car) symbols))
              (format t "~33<~>")))
    :do (if (plusp bit)
            (flet ()
              (let* ((opcode (opcode instruction))
                     (src    (src    instruction))
                     (dst    (dst    instruction))
                     (entry  (assoc opcode *instructions*)))
                (if entry
                    (print-instruction pc (subst src 'src (subst dst 'dst (second entry))) symtab)
                    (print-instruction pc `(illegal-instruction ,instruction) symtab))))
            (print-instruction pc `(dcl ,instruction) symtab)))
  (format t "~%"))

;;; Finally, we can write a few missing instructions in assembler,
;;; load them in the machine, and write a program using them.
;;;
;;; In real machines, these handlers are usually priviledged code, but
;;; nothing prevents a system to let application define their own
;;; opcodes too.


(defvar *machine* nil
  "for debugging purpose, we bind the last machine used here.")

(defparameter *program* 
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

    ;; Address 4 is the stack pointer.
    stack               (dcl #.*memory-size*)


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
    (add jump-template acc)  ; store it into the return instruction.
    (store acc return-instruction)
    
    (load return-pc acc)          ; get the instruction address
    (lshift src-offset acc)       ; store it into the load instruction
    (add load-template acc)
    (store acc load-instruction)
    
    load-instruction (load 0 acc)       ; get the instruction
    (store acc instruction)
    (and opcode-mask acc)            ; extract the opcode
    (rshift opcode-offset acc)
    (not acc)               ; remember, we don't have NEG nor SUB yet.
    (add one acc)           ; so we get two-complement of opcode
    (store acc -op)

    ;; interpret the instruction according to the opcode:
    (load -op acc)
    (add move-op acc)
    (jzero move)

    (load -op acc)
    (add sub-op acc)
    (jzero sub)

    (load -op acc)
    (add neg-op acc)
    (jzero neg)

    (load -op acc)
    (add or-op acc)
    (jzero or)

    (load -op acc)
    (add call-op acc)
    (jzero call)

    (load -op acc)
    (add ret-op acc)
    (jzero ret)

    (jump really-illegal-instruction)

    ;; instruction templates
    #+(and (or) emacs) (loop for entry in *instructions*
                             do (insert (format "%s-template %S\n"
                                                (first (second entry))
                                                (subst 0 'src (subst 0 'dst (second entry))))))
    move-template (move 0 0)
    load-template (load 0 acc)
    store-template (store acc 0)
    add-template (add 0 acc)
    sub-template (sub 0 acc)
    mul-template (mul 0 acc)
    div-template (div 0 acc)
    not-template (not acc)
    neg-template (neg acc)
    lshift-template (lshift 0 acc)
    rshift-template (rshift 0 acc)
    and-template (and 0 acc)
    or-template (or 0 acc)
    jzero-template (jzero 0)
    jump-template (jump 0)
    call-template (call 0)
    ret-template (ret)
    pusha-template (pusha)
    push-template (push 0)
    popa-template (popa)
    pop-template (pop 0)
    halt-template (halt)

    
    
    ;; instruction op-codes
    #+(and (or) emacs) (loop for entry in *instructions*
                             do (insert (format "%s-op (dcl #b%s)\n"
                                                (first (second entry))
                                                (mapconcat (lambda (bit) (format "%d" bit))
                                                           (reverse (loop :for n = (first entry) :then (truncate n 2)
                                                                          :repeat 8
                                                                          :collect (mod n 2)))
                                                           ""))))
    move-op (dcl #b00000000)
    load-op (dcl #b00010000)
    store-op (dcl #b00100000)
    add-op (dcl #b01000000)
    sub-op (dcl #b01010000)
    mul-op (dcl #b01100000)
    div-op (dcl #b01110000)
    not-op (dcl #b10000000)
    neg-op (dcl #b10010000)
    lshift-op (dcl #b10100000)
    rshift-op (dcl #b10110000)
    and-op (dcl #b11000000)
    or-op (dcl #b11010000)
    jzero-op (dcl #b11100000)
    jump-op (dcl #b11110000)
    call-op (dcl #b11110010)
    ret-op (dcl #b11110011)
    pusha-op (dcl #b11110100)
    push-op (dcl #b11110101)
    popa-op (dcl #b11110110)
    pop-op (dcl #b11110111)
    halt-op (dcl #b11111111)
    

    -op           (dcl 0)
    opcode-mask   (dcl #xff00000000000000)
    opcode-offset (dcl 56)


    ;; Not implemented yet, we'd need an OS to do something with those.
    really-illegal-instruction (halt)
    process-invalid-address    (halt)
    ;; eg. process-invalid-address could go to a VM manager.
    
    move                                ; move src dst
    (load instruction acc)              ; load the src data:
    (and src-mask acc)                  ; maskout the source address
    (add load-template acc)
    (store acc move-load-data-instruction)
    move-load-data-instruction (load 0 acc)
    (store acc data)
    (load instruction acc)              ; store the data into the dst
    (and dst-mask acc)
    (add store-template acc)
    (store acc store-data-instruction)
    (load data acc)
    store-data-instruction (store acc 0)
    (jump return)
    
    sub
    (load instruction acc)              ; load the src data
    (and src-mask acc)                  ; maskout the source address
    (add load-template acc)
    (store acc sub-load-data-instruction)
    sub-load-data-instruction (load 0 acc)
    (not acc)
    (add one acc)
    (add save-acc acc)
    (jump return-instruction)

    mul (jump really-illegal-instruction)
    div (jump really-illegal-instruction)
    
    neg
    (load save-acc acc)
    (not acc)
    (add one acc)
    (jump return-instruction)
    
    or                ; implemented as (not (and (not mem) (not acc)))
    (load instruction acc)              ; load the src data
    (and src-mask acc)                  ; maskout the source address
    (add load-template acc)
    (store acc or-load-data-instruction)
    or-load-data-instruction (load 0 acc)
    (not acc)
    (store acc data)
    (load save-acc acc)
    (not acc)
    (and data acc)
    (not acc)
    (jump return-instruction)
    
    call
    (load stack acc) (add -one acc) (store acc stack)
    (add store-template acc)
    (store acc save-return)
    (load return-pc acc)
    (add one acc)
    save-return (store acc 0)
    (load instruction acc)
    (and dst-mask acc)
    (add jump-template acc)
    (store acc return-instruction)
    (jump return)
    
    ret
    (load stack acc)
    (lshift src-offset acc)
    (add load-template acc)
    (store acc load-return)
    load-return (load 0 acc)
    (add jump-template acc)
    (store acc return-instruction)
    (load stack acc)
    (add one acc)
    (store acc stack)
    (jump return)
    
    pusha (jump really-illegal-instruction)
    push  (jump really-illegal-instruction)
    popa  (jump really-illegal-instruction)
    pop   (jump really-illegal-instruction)


    
    return   
    (load save-acc acc)                 ; restore the accumulator:
    return-instruction (jump 0)         ; and continue

    
    src-offset  (dcl 28)
    src-mask*   (dcl #xff0000000fffffff)
    src-mask    (dcl #x00fffffff0000000)
    dst-mask    (dcl #x000000000fffffff)
    -one        (dcl -1)
    one         (dcl 1)
    save-acc    (dcl 0)
    instruction (dcl 0)
    data        (dcl 0)

    ;; ---------------------------------------------- ;;
    start ; let's test the new opcodes:

    
    (move src dst)
    (move (+ src 1) (+ dst 1))
    (move (+ src 2) (+ dst 2))
    (move (+ src 3) (+ dst 3))

    (load +pi acc)
    (neg acc)
    (store acc -pi)

    (load a acc)
    (or b acc)
    (store acc a-or-b)

    (load +pi acc)
    (sub pi-1 acc)
    (store acc pi-diff)

    (load message-address acc)
    (call write-string)

    (halt)
    
    message (ascii "Hello world!" 10 0)
    message-address (dcl message)
    ;; ---------------------------------------------- ;;
    ;; library

    load-indirect
    ;; input: acc = address of data
    ;; output: acc = data at given address
    (lshift src-offset acc)
    (add load-template acc)
    (store acc load-indirect/load-instruction)
    load-indirect/load-instruction (load 0 acc)
    (ret)
    
    write-string
    ;; acc = address of string.
    (store acc write-string/string)

    write-string/loop
    ;; load next chunk
    (load write-string/string acc)
    (call load-indirect)
    (store acc write-string/chunk)
    ;; increment string address
    (load write-string/string acc)
    (add one acc)
    (store acc write-string/string)
    ;; initialize byte mask and offset
    (move write-string/byte-initial write-string/byte)
    (move write-string/offset-initial write-string/offset)

    write-string/char-loop
    ;; load next byte
    (load write-string/chunk acc)
    (and write-string/byte acc)
    (rshift write-string/offset acc)
    ;; write it out
    (jzero write-string/end)
    (output acc stdout)
    ;; shift mask 
    (load write-string/byte acc)
    (rshift write-string/chunk-size acc)
    (store acc write-string/byte)
    ;; decrement offset
    (load write-string/offset acc)
    (jzero write-string/loop)
    (add write-string/offset-increment acc)
    (store acc write-string/offset)

    (jump write-string/char-loop)
    
    write-string/end
    (ret)
    
    write-string/string           (dcl 0) ; pointer to the next string chunk
    write-string/chunk            (dcl 0) ; current string chunk being processed
    write-string/byte             (dcl 0) ; current byte mask
    write-string/offset           (dcl 0)
    write-string/offset-initial   (dcl 56)
    write-string/offset-increment (dcl -8)
    write-string/byte-initial     (dcl #xff00000000000000)
    write-string/chunk-size       (dcl +bytes-per-word+)

    stdin (dcl 0)
    stdout (dcl 1)
    stderr (dcl 2)
    traceout (dcl 3)
    terminal-io (dcl 4)
    query-io (dcl 5)
    ;; ---------------------------------------------- ;;

    ;; We will dump the memory from src to end when the machine
    ;; halts:
    
    src     (dcl #x1122334455667788 #xff #xdeadbeef #xfeedbabe)
    dst     (dcl 0 0 0 0)
    +pi     (dcl 3141592653589)
    pi-1    (dcl 2141592653589)
    pi-diff (dcl 1000000000000)
    -pi     (dcl 0)
    a       (dcl #xffffffff00000000)
    b       (dcl #xffff0000ffff0000)
    a-or-b  (dcl 0)
    end     (dcl 0)

    )
  "This is an example program showing how a few soft opcodes are implemented,
including call/ret with a stack, and a couple of library routines.")

(defun example/assemble ()
  (multiple-value-bind (code symtable bits) (assemble 0 *program*)
    (disassemble 0 code symtable bits))
  (values))

(defun example/run (&key verbose)
  (multiple-value-bind (code symtable) (assemble 0 *program*)
    (let ((start (cdr (assoc 'src symtable))) 
          (end   (cdr (assoc 'end symtable)))
          (machine (make-machine)))
      (setf *machine* machine)
      (load-machine machine 0 code)
      ;; (let ((*print-length* 10)) (print machine)) (terpri) (finish-output)
      (if verbose
          (progn
            (princ (with-output-to-string (*trace-output*)
                     (run *machine* :verbose t)
                     (terpri)
                     (finish-output)))
            (terpri)
            (dump-machine machine start (- end start)))
          (run *machine*))
      (terpri)))
  (values))


#|

Usage:

    (example/assemble)        ; Assemble and disassemble the *program*
    (example/run)             ; Assemble and run the *program*
    (example/run :verbose t)  ; Assemble and run the *program* tracing each instruction
                              ; The trace dump is displayed after any output from the program.
|#

#|

example-soft-opcodes> (example/run)
Hello world!

; No value
example-soft-opcodes> (example/run :verbose t)
Hello world!
0000: (jump A8)
00A8: Illegal instruction trap (0000000E500000E9)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load A8 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0059: (load A6 acc)
005A: (and A1 acc)
005B: (add 29 acc)
005C: (store acc 5D)
005D: (load E5 acc)
005E: (store acc A7)
005F: (load A6 acc)
0060: (and A2 acc)
0061: (add 2A acc)
0062: (store acc 64)
0063: (load A7 acc)
0064: (store acc E9)
0065: (jump 9D)
009D: (load A5 acc)
009E: (jump A9)
00A9: Illegal instruction trap (0000000E600000EA)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load A9 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0059: (load A6 acc)
005A: (and A1 acc)
005B: (add 29 acc)
005C: (store acc 5D)
005D: (load E6 acc)
005E: (store acc A7)
005F: (load A6 acc)
0060: (and A2 acc)
0061: (add 2A acc)
0062: (store acc 64)
0063: (load A7 acc)
0064: (store acc EA)
0065: (jump 9D)
009D: (load A5 acc)
009E: (jump AA)
00AA: Illegal instruction trap (0000000E700000EB)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load AA acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0059: (load A6 acc)
005A: (and A1 acc)
005B: (add 29 acc)
005C: (store acc 5D)
005D: (load E7 acc)
005E: (store acc A7)
005F: (load A6 acc)
0060: (and A2 acc)
0061: (add 2A acc)
0062: (store acc 64)
0063: (load A7 acc)
0064: (store acc EB)
0065: (jump 9D)
009D: (load A5 acc)
009E: (jump AB)
00AB: Illegal instruction trap (0000000E800000EC)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load AB acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0059: (load A6 acc)
005A: (and A1 acc)
005B: (add 29 acc)
005C: (store acc 5D)
005D: (load E8 acc)
005E: (store acc A7)
005F: (load A6 acc)
0060: (and A2 acc)
0061: (add 2A acc)
0062: (store acc 64)
0063: (load A7 acc)
0064: (store acc EC)
0065: (jump 9D)
009D: (load A5 acc)
009E: (jump AC)
00AC: (load ED acc)
00AD: Illegal instruction trap (9000000000000000)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load AD acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0018: (load 54 acc)
0019: (add 42 acc)
001A: (jzero 66)
001B: (load 54 acc)
001C: (add 46 acc)
001D: (jzero 71)
0071: (load A5 acc)
0072: (not acc)
0073: (add A4 acc)
0074: (jump 9E)
009E: (jump AE)
00AE: (store acc F0)
00AF: (load F1 acc)
00B0: (or F2 acc)
00B1: (store acc F3)
00B2: (load ED acc)
00B3: Illegal instruction trap (5000000EE0000000)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load B3 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0018: (load 54 acc)
0019: (add 42 acc)
001A: (jzero 66)
0066: (load A6 acc)
0067: (and A1 acc)
0068: (add 29 acc)
0069: (store acc 6A)
006A: (load EE acc)
006B: (not acc)
006C: (add A4 acc)
006D: (add A5 acc)
006E: (jump 9E)
009E: (jump B4)
00B4: (store acc EF)
00B5: (load BA acc)
00B6: Illegal instruction trap (F2000000000000C0)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load B6 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0018: (load 54 acc)
0019: (add 42 acc)
001A: (jzero 66)
001B: (load 54 acc)
001C: (add 46 acc)
001D: (jzero 71)
001E: (load 54 acc)
001F: (add 4A acc)
0020: (jzero 75)
0021: (load 54 acc)
0022: (add 4D acc)
0023: (jzero 81)
0081: (load 4 acc)
0082: (add A3 acc)
0083: (store acc 4)
0084: (add 2A acc)
0085: (store acc 88)
0086: (load 1 acc)
0087: (add A4 acc)
0088: (store acc FFFF)
0089: (load A6 acc)
008A: (and A2 acc)
008B: (add 36 acc)
008C: (store acc 9E)
008D: (jump 9D)
009D: (load A5 acc)
009E: (jump C0)
00C0: (store acc D7)
00C1: (load D7 acc)
00C2: Illegal instruction trap (F2000000000000BB)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load C2 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0018: (load 54 acc)
0019: (add 42 acc)
001A: (jzero 66)
001B: (load 54 acc)
001C: (add 46 acc)
001D: (jzero 71)
001E: (load 54 acc)
001F: (add 4A acc)
0020: (jzero 75)
0021: (load 54 acc)
0022: (add 4D acc)
0023: (jzero 81)
0081: (load 4 acc)
0082: (add A3 acc)
0083: (store acc 4)
0084: (add 2A acc)
0085: (store acc 88)
0086: (load 1 acc)
0087: (add A4 acc)
0088: (store acc FFFE)
0089: (load A6 acc)
008A: (and A2 acc)
008B: (add 36 acc)
008C: (store acc 9E)
008D: (jump 9D)
009D: (load A5 acc)
009E: (jump BB)
00BB: (lshift 9F acc)
00BC: (add 29 acc)
00BD: (store acc BE)
00BE: (load B8 acc)
00BF: Illegal instruction trap (F300000000000000)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load BF acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0018: (load 54 acc)
0019: (add 42 acc)
001A: (jzero 66)
001B: (load 54 acc)
001C: (add 46 acc)
001D: (jzero 71)
001E: (load 54 acc)
001F: (add 4A acc)
0020: (jzero 75)
0021: (load 54 acc)
0022: (add 4D acc)
0023: (jzero 81)
0024: (load 54 acc)
0025: (add 4E acc)
0026: (jzero 8E)
008E: (load 4 acc)
008F: (lshift 9F acc)
0090: (add 29 acc)
0091: (store acc 92)
0092: (load FFFE acc)
0093: (add 36 acc)
0094: (store acc 9E)
0095: (load 4 acc)
0096: (add A4 acc)
0097: (store acc 4)
0098: (jump 9D)
009D: (load A5 acc)
009E: (jump C3)
00C3: (store acc D8)
00C4: (load D7 acc)
00C5: (add A4 acc)
00C6: (store acc D7)
00C7: Illegal instruction trap (0000000DD00000D9)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load C7 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0059: (load A6 acc)
005A: (and A1 acc)
005B: (add 29 acc)
005C: (store acc 5D)
005D: (load DD acc)
005E: (store acc A7)
005F: (load A6 acc)
0060: (and A2 acc)
0061: (add 2A acc)
0062: (store acc 64)
0063: (load A7 acc)
0064: (store acc D9)
0065: (jump 9D)
009D: (load A5 acc)
009E: (jump C8)
00C8: Illegal instruction trap (0000000DB00000DA)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load C8 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0059: (load A6 acc)
005A: (and A1 acc)
005B: (add 29 acc)
005C: (store acc 5D)
005D: (load DB acc)
005E: (store acc A7)
005F: (load A6 acc)
0060: (and A2 acc)
0061: (add 2A acc)
0062: (store acc 64)
0063: (load A7 acc)
0064: (store acc DA)
0065: (jump 9D)
009D: (load A5 acc)
009E: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00C1: (load D7 acc)
00C2: Illegal instruction trap (F2000000000000BB)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load C2 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0018: (load 54 acc)
0019: (add 42 acc)
001A: (jzero 66)
001B: (load 54 acc)
001C: (add 46 acc)
001D: (jzero 71)
001E: (load 54 acc)
001F: (add 4A acc)
0020: (jzero 75)
0021: (load 54 acc)
0022: (add 4D acc)
0023: (jzero 81)
0081: (load 4 acc)
0082: (add A3 acc)
0083: (store acc 4)
0084: (add 2A acc)
0085: (store acc 88)
0086: (load 1 acc)
0087: (add A4 acc)
0088: (store acc FFFE)
0089: (load A6 acc)
008A: (and A2 acc)
008B: (add 36 acc)
008C: (store acc 9E)
008D: (jump 9D)
009D: (load A5 acc)
009E: (jump BB)
00BB: (lshift 9F acc)
00BC: (add 29 acc)
00BD: (store acc BE)
00BE: (load B9 acc)
00BF: Illegal instruction trap (F300000000000000)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load BF acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0018: (load 54 acc)
0019: (add 42 acc)
001A: (jzero 66)
001B: (load 54 acc)
001C: (add 46 acc)
001D: (jzero 71)
001E: (load 54 acc)
001F: (add 4A acc)
0020: (jzero 75)
0021: (load 54 acc)
0022: (add 4D acc)
0023: (jzero 81)
0024: (load 54 acc)
0025: (add 4E acc)
0026: (jzero 8E)
008E: (load 4 acc)
008F: (lshift 9F acc)
0090: (add 29 acc)
0091: (store acc 92)
0092: (load FFFE acc)
0093: (add 36 acc)
0094: (store acc 9E)
0095: (load 4 acc)
0096: (add A4 acc)
0097: (store acc 4)
0098: (jump 9D)
009D: (load A5 acc)
009E: (jump C3)
00C3: (store acc D8)
00C4: (load D7 acc)
00C5: (add A4 acc)
00C6: (store acc D7)
00C7: Illegal instruction trap (0000000DD00000D9)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load C7 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0059: (load A6 acc)
005A: (and A1 acc)
005B: (add 29 acc)
005C: (store acc 5D)
005D: (load DD acc)
005E: (store acc A7)
005F: (load A6 acc)
0060: (and A2 acc)
0061: (add 2A acc)
0062: (store acc 64)
0063: (load A7 acc)
0064: (store acc D9)
0065: (jump 9D)
009D: (load A5 acc)
009E: (jump C8)
00C8: Illegal instruction trap (0000000DB00000DA)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load C8 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0059: (load A6 acc)
005A: (and A1 acc)
005B: (add 29 acc)
005C: (store acc 5D)
005D: (load DB acc)
005E: (store acc A7)
005F: (load A6 acc)
0060: (and A2 acc)
0061: (add 2A acc)
0062: (store acc 64)
0063: (load A7 acc)
0064: (store acc DA)
0065: (jump 9D)
009D: (load A5 acc)
009E: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00CD: (output acc E0)
00CE: (load D9 acc)
00CF: (rshift DE acc)
00D0: (store acc D9)
00D1: (load DA acc)
00D2: (jzero C1)
00D3: (add DC acc)
00D4: (store acc DA)
00D5: (jump C9)
00C9: (load D8 acc)
00CA: (and D9 acc)
00CB: (rshift DA acc)
00CC: (jzero D6)
00D6: Illegal instruction trap (F300000000000000)
0005: (store acc A5)
0006: (load 1 acc)
0007: (add A4 acc)
0008: (add 36 acc)
0009: (store acc 9E)
000A: (load 1 acc)
000B: (lshift 9F acc)
000C: (add 29 acc)
000D: (store acc E)
000E: (load D6 acc)
000F: (store acc A6)
0010: (and 55 acc)
0011: (rshift 56 acc)
0012: (not acc)
0013: (add A4 acc)
0014: (store acc 54)
0015: (load 54 acc)
0016: (add 3E acc)
0017: (jzero 59)
0018: (load 54 acc)
0019: (add 42 acc)
001A: (jzero 66)
001B: (load 54 acc)
001C: (add 46 acc)
001D: (jzero 71)
001E: (load 54 acc)
001F: (add 4A acc)
0020: (jzero 75)
0021: (load 54 acc)
0022: (add 4D acc)
0023: (jzero 81)
0024: (load 54 acc)
0025: (add 4E acc)
0026: (jzero 8E)
008E: (load 4 acc)
008F: (lshift 9F acc)
0090: (add 29 acc)
0091: (store acc 92)
0092: (load FFFF acc)
0093: (add 36 acc)
0094: (store acc 9E)
0095: (load 4 acc)
0096: (add A4 acc)
0097: (store acc 4)
0098: (jump 9D)
009D: (load A5 acc)
009E: (jump B7)
00B7: (halt)

00E5: 1122334455667788  1234605616436508552
00E6: 00000000000000FF  255
00E7: 00000000DEADBEEF  3735928559
00E8: 00000000FEEDBABE  4276992702
00E9: 1122334455667788  1234605616436508552
00EA: 00000000000000FF  255
00EB: 00000000DEADBEEF  3735928559
00EC: 00000000FEEDBABE  4276992702
00ED: 000002DB75839F15  3141592653589
00EE: 000001F2A0DE8F15  2141592653589
00EF: 000000E8D4A51000  1000000000000
00F0: FFFFFD248A7C60EB  18446740932116898027
00F1: FFFFFFFF00000000  18446744069414584320
00F2: FFFF0000FFFF0000  18446462603027742720
00F3: FFFFFFFFFFFF0000  18446744073709486080

; No value
example-soft-opcodes> 

|#
