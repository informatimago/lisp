;;;; -*- coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               raiden.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements the Raiden block cipher.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-10-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
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
;;;;**************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.RAIDEN" (:USE "COMMON-LISP")
            (:EXPORT "RAIDEN-DECIPHER" "RAIDEN-ENCIPHER")
            (:DOCUMENTATION
             "This package imlements the RAIDEN block cipher.
    http://raiden-cipher.sourceforge.net/

    Copyright Pascal J. Bourguignon 2006 - 2006
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.RAIDEN")



;; void raiden(unsigned long *data,unsigned long *result,unsigned long
;; *key)
;; {
;; 
;;         unsigned long b0=data[0],
;;         b1=data[1],i,k[4]={key[0],key[1],key[2],key[3]}, sk;
;;         for(i=0; i< 16; i++)
;;         {
;;                 sk=k[i%4]=((k[0]+k[1])+((k[2]+k[3])^(k[0]<<k[2])));
;;                 b0 +=((sk+b1)<<9)^((sk-b1)^((sk+b1)>>14));
;;                 b1 +=((sk+b0)<<9)^((sk-b0)^((sk+b0)>>14));
;;         }
;;         result[0]=b0;
;;         result[1]=b1;
;; }

(defun raiden-encipher (v w k)
  "
V:    (vector (unsigned-byte 32) 2), the clear text
W:    (vector (unsigned-byte 32) 2), the result encrypted text
K:    (vector (unsigned-byte 32) 4), the key
"
  (declare (type (vector (unsigned-byte 32) 2) v)
           (type (vector (unsigned-byte 32) 2) w)
           (type (vector (unsigned-byte 32) 4) k))
  (macrolet ((ref (vector index)
               `(the (unsigned-byte 32) (aref ,vector ,index))))
    (flet ((32bit (x) (logand #xffffffff x)))
      (declare (inline 32bit))
      (loop
         :with b0 :of-type (unsigned-byte 32) = (ref v 0)
         :with b1 :of-type (unsigned-byte 32) = (ref v 1)
         :with k :of-type (vector (unsigned-byte 32) 4) = (copy-seq k)
         :for i  :of-type fixnum :from 0 :below 16
         :for sk :of-type (unsigned-byte 32)
         = (32bit (+ (ref k 0) (ref k 1)
                     (logxor (+ (ref k 2) (ref k 3))
                             (32bit (ash (ref k 0)
                                         (mod (ref k 2) 32))))))
         :do
         (setf (ref k (mod i 4)) sk
               b0 (32bit (+ b0 (logxor (32bit (ash (+ sk b1) 9))
                                       (32bit (- sk b1))
                                       (32bit (ash (+ sk b1) -14)))))
               b1 (32bit (+ b1 (logxor (32bit (ash (+ sk b0) 9))
                                       (32bit (- sk b0))
                                       (32bit (ash (+ sk b0) -14))))))

         :finally (setf (ref w 0) b0
                        (ref w 1) b1)))))


;; void decode_raiden(unsigned long *data,unsigned long *result,unsigned
;; long *key)
;; {
;; 
;;         unsigned long b0=data[0],
;;         b1=data[1],i,k[4]={key[0],key[1],key[2],key[3]},
;;         subkeys[16];
;;         for(i=0;i< 16;i++){
;;                 //Subkeys are calculated
;;                 k[i%4]=((k[0]+k[1])+((k[2]+k[3])^(k[0]<<k[2])));
;;                 subkeys[i]=k[i%4];
;;         }
;;         for(i=15; i!=-1; i--)
;;         {
;;                 //Process is applied in the inverse order
;;                 b1 -= ((subkeys[i]+b0)<<9)^((subkeys[i]-b0)^
;;                           ((subkeys[i]+b0)>>14));
;;                 b0 -= ((subkeys[i]+b1)<<9)^((subkeys[i]-b1)^
;;                           ((subkeys[i]+b1)>>14));
;;         }
;;         result[0]=b0;
;;         result[1]=b1;
;; }

(defun raiden-decipher (v w k)
  "
V:    (vector (unsigned-byte 32) 2), the clear text
W:    (vector (unsigned-byte 32) 2), the result encrypted text
K:    (vector (unsigned-byte 32) 4), the key
"
  (declare (type (vector (unsigned-byte 32) 2) v)
           (type (vector (unsigned-byte 32) 2) w)
           (type (vector (unsigned-byte 32) 4) k))
  (flet ((32bit (x) (logand #xffffffff x)))
    (declare (inline 32bit))
    (loop
       :with b0  :of-type (unsigned-byte 32) = (aref v 0)
       :with b1  :of-type (unsigned-byte 32) = (aref v 1)
       :with k   :of-type (vector (unsigned-byte 32) 4) = (copy-seq k)
       :with sks :of-type (vector (unsigned-byte 32) 32)
       = (make-array 32 :element-type '(unsigned-byte 32) :initial-element 0)
       :for i fixnum :from 0 :below 16
       :do (setf (aref sks i)
                 (setf (aref k (mod i 4))
                       (32bit (+ (aref k 0) (aref k 1)
                                 (logxor (+ (aref k 2) (aref k 3))
                                         (32bit (ash (aref k 0)
                                                     (mod (aref k 2) 32))))))))
       :finally
       (loop
          :for i  :of-type fixnum :from 15 :downto 0
          :for sk :of-type (unsigned-byte 32) = (aref sks i)
          :do (setf b1 (32bit (- b1 (logxor (32bit (ash (+ sk b0) 9))
                                            (32bit (- sk b0))
                                            (32bit (ash (+ sk b0) -14)))))
                    b0 (32bit (- b0 (logxor (32bit (ash (+ sk b1) 9))
                                            (32bit (- sk b1))
                                            (32bit (ash (+ sk b1) -14))))))
          :finally (setf (aref w 0) b0
                         (aref w 1) b1)))))


(defun word (a b c d) 
  (dpb a (byte 8 24) (dpb b (byte 8 16) (dpb c (byte 8 8) d))))

(defun read-words (bits what)
  (loop
     for bytes = (progn (format t "Please enter ~D bits of ~A: " 
                                bits what)
                        (let ((buffer (read-line *standard-input* nil nil)))
                          (when buffer
                            #+clisp
                            (ext:convert-string-to-bytes
                             buffer ext:*TERMINAL-ENCODING*)
                            #-clisp
                            (coerce (loop :for ch :in buffer
                                       :collect (char-code ch)) 'vector))))
     while (and bytes (< (* 8 (length bytes)) bits))
     finally (return
               (and bytes
                    (loop for i from 0 by 4 below (truncate (+ 7 bits) 8)
                       collect (word (aref bytes (+ i 0))
                                     (aref bytes (+ i 1))
                                     (aref bytes (+ i 2))
                                     (aref bytes (+ i 3))) into words
                       finally (return (coerce words 'vector)))))))

(defun test ()
  (loop 
     with code = (make-array 2 :element-type '(unsigned-byte 32)
                             :initial-element 0)
     with decr = (make-array 2 :element-type '(unsigned-byte 32)
                             :initial-element 0)
     for clear = (prog1 (read-words  64 "clear text") (terpri))
     for key   = (prog1 (read-words 128 "key") (terpri))
     while (and clear key)
     do (progn (raiden-encipher clear code key)
               (format t "(encipher ~S ~S)~% -->      ~S~%" clear key code)
               (raiden-decipher code decr key)      
               (format t "(decipher ~S ~S)~% -->      ~S~%" code key decr)
               (unless (equalp clear decr) (format t "!!! ERROR !!!~%")))))

(defun auto-test ()
  (with-input-from-string (*standard-input* 
                           "Hello World!
John McCarthy invented LISP.
Big Unknown Secret.
Very very secret key. Forget it!
")
    (test)))




;; From: "Julio C. Hernandez Castro" <jcesar@inf.uc3m.es>
;; Subject: New lightweight block cipher algorithm
;; Newsgroups: comp.programming, comp.lang.c++, de.comp.security.misc,
;;  comp.lang.perl.misc, comp.lang.java.security
;; Date: 20 Oct 2006 06:49:35 -0700
;; Organization: http://groups.google.com
;; Message-ID: <1161352175.817923.35400@f16g2000cwb.googlegroups.com>
;; 
;; Dear all,
;; 
;; We have just developped a new block cipher called Raiden, following a
;; Feistel Network structure by means of genetic programming. Our
;; intention now consists on getting as much feedback as possible from
;; users, so we encourage you to test the algorithm and send us your
;; opinion. We would also like to receive enhancements and new versions of
;; the algorithm, developed in other source languages and platforms.
;; 
;; Our idea on developing this cipher is to propose it as an alternative
;; to TEA block cipher. TEA is a very interesting cipher with lots of real
;; applications. It is very simple and fast, and it reaches an acceptable
;; level of security by the application of a lot of cycles.
;; 
;; Nowadays TEA has been broken, and several weaknesses of the algorithm
;; have been discovered. Since most of these weaknesses are related to its
;; Key Shedule routine, the authors, Roger Needham and David Wheeler
;; proposed an extended version of the algorithm with a more complex one.
;; This new version, which they called XTEA, did not have the expected
;; success of its antecessor, in part because it is slower.
;; 
;; The algorithm known weaknesses, as well as its popularity, have made us
;; to think it was the time to develop an alternative to TEA. This
;; alternative, presented in this page, must have the next features:
;; 
;;     * It must be as quick as TEA, to be used in the same real world
;;        applications
;;     * It must be stronger, to avoid TEA weaknesses
;; 
;; To develop this new block cipher we have used a genetic
;; programming-based technique. More on this to follow in a coming
;; scientific paper.
;; 
;; Description of Raiden
;; ----------------------------
;; 
;; Raiden is a very small and compact cipher. It works with blocks of 64
;; bits and keys of 128. As it can be seen below, the algorithm has the
;; same main structure as TEA: it is structured in cycles, where one cycle
;; contains two feistel rounds, and for both of them, the same round key
;; is used. In each round, the output of the round function on a sub block
;; is used to feed the other one. The round function of the algorithm has
;; the same size as TEA's one, but, as commented in section Raiden
;; Strength, it seems to be stronger.
;; 
;; The Key Schedule routine is more complex than TEA's, but it is simple
;; enough to not overload the cipher execution time. To maximize the
;; entropy introduced by this function, in each round, its output feeds
;; the position i%4 of the key array. This makes the key array passed to
;; the function to evolve as the algorithm is executed, and thus
;; generating new round keys for each cycle with that 128 bit array. This
;; also makes that function to behave much as a PRNG. After analyzing the
;; algorithm, as commented in it homepage
;; (http://raiden-cipher.sourceforge.net/ in the Results section), we
;; propose the execution of sixteen cycles. Below, the code of Raiden
;; encoding routine is shown.
;; 
;; 
;; void raiden(unsigned long *data,unsigned long *result,unsigned long
;; *key)
;; {
;; 
;;         unsigned long b0=data[0],
;;         b1=data[1],i,k[4]={key[0],key[1],key[2],key[3]}, sk;
;;         for(i=0; i< 16; i++)
;;         {
;;                 sk=k[i%4]=((k[0]+k[1])+((k[2]+k[3])^(k[0]<<k[2])));
;;                 b0 +=((sk+b1)<<9)^((sk-b1)^((sk+b1)>>14));
;;                 b1 +=((sk+b0)<<9)^((sk-b0)^((sk+b0)>>14));
;;         }
;;         result[0]=b0;
;;         result[1]=b1;
;; }
;; 
;;  The cipher receives the plain text in 'data' parameter, and puts the
;; cipher text in the 'result'. Key contains the 128 bit cipher key.
;;  The cipher follows a Feistel structure, so the decoding is made in the
;; same way than the encoding but applying the round keys in the inverse
;; order. This is the Raiden decoding routine.
;; 
;; void decode_raiden(unsigned long *data,unsigned long *result,unsigned
;; long *key)
;; {
;; 
;;         unsigned long b0=data[0],
;;         b1=data[1],i,k[4]={key[0],key[1],key[2],key[3]},
;;         subkeys[16];
;;         for(i=0;i< 16;i++){
;;                 //Subkeys are calculated
;;                 k[i%4]=((k[0]+k[1])+((k[2]+k[3])^(k[0]<<k[2])));
;;                 subkeys[i]=k[i%4];
;;         }
;;         for(i=15; i!=-1; i--)
;;         {
;;                 //Process is applied in the inverse order
;;                 b1 -= ((subkeys[i]+b0)<<9)^((subkeys[i]-b0)^
;;                           ((subkeys[i]+b0)>>14));
;;                 b0 -= ((subkeys[i]+b1)<<9)^((subkeys[i]-b1)^
;;                           ((subkeys[i]+b1)>>14));
;;         }
;;         result[0]=b0;
;;         result[1]=b1;
;; }
;; 
;;  In this case, the function receives the ciphertext in the 'data'
;; parameter and puts the plain text in the 'result'. The round keys are
;; calculated at the beginning of the function, and then they are applied
;; in the inverse order as they were when ciphering.
;; 
;; The algorithm is free to anyone, and has been developed in ANSI C
;; source code under Linux.
;; We propose you to develop new versions of it using other programming
;; languages and platforms.
;; 
;; Raiden Strength
;; ---------------------
;; 
;; The main weaknesses of TEA, such as the Related Key Cryptanalysis, or
;; the equivalent keys, are related with its Key Shedule routine. Thus,
;; one of the main objectives when developing this new cipher has been to
;; develop an stronger Key Shedule function than TEA's.
;; 
;; The function developed is also very simple, but not as much as TEA's.
;; TEA Key Shedule function consists only on adding a constant
;; (0x9e3779b9) to a variable. Thus, it is very simple to, knowing the
;; round key of cycle i, obtain the keys corresponding to the previous and
;; the following cycles. This is not the case in our algorithm, in which
;; doing so it is not a trivial problem.
;; 
;; Therefore, the Key Shedule function behaves more as a Hash Function or
;; pseudorandom number generator, and does not have the same weaknesses as
;; TEA Key Schedule. Raiden's Round Function provided much better results
;; in the statistical tests than TEA's one, so we can conclude it is also
;; stronger, and, therefore, that the whole algorithm is also stronger.
;; 
;; When we analyzed the algorithm using the statistical tests ENT,
;; Diehard, NIST and Sexton, we realized that Raiden results when applied
;; 16 cycles were, in many ocassions better, and at least comparable, to
;; TEA results when applied 32. This made us to conclude Raiden is
;; stronger than TEA and that 16 cycles would be an enough security margin
;; for the algorithm to be applied. The mentioned results can be consulted
;; in the Results of statistical tests on Raiden section at
;; http://raiden-cipher.sourceforge.net/
;; 
;;  About the Authors
;; ------------------------
;; 
;; Raiden has been developed by:
;; 
;; Julio Cesar Hernandez Castro, e-mail: jcesar_at_inf_dot_uc3m_dot_es
;; Javier Polimon Olabarrieta, jpolimon_at_gmail_dot_com
;; 
;; Don't hesitate to contact the authors with your feedback.

