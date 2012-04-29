;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:             graf.lisp
;;;;LANGUAGE:         Common-Lisp
;;;;SYSTEM:           Common-Lisp
;;;;USER-INTERFACE:   NONE
;;;;NOWEB:              t
;;;;DESCRIPTION
;;;;
;;;;    This file defines Graf classes.
;;;;
;;;;    This is an old implementation adapted from a scheme version.
;;;;    The GRAF class merely encapsulates list-based data structures
;;;;    and functions manipulating graphs.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-06 <PJB> Added defgeneric.
;;;;    2003-06-17 <PJB> Updated to pure Common-Lisp.
;;;;    2003-01-09 <PJB> Added DEFPACKAGE. Made it DEFINE-PACKAGE.
;;;;    1996-10-25 <PJB> Updated to CLISP.
;;;;    1994-04-09 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 1996 - 2012
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;****************************************************************************

(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.GRAF"
  (:use "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST" "COMMON-LISP")
  (:export "MAKE-TREE" "MAKE-UNION" "MAKE-INTERSECTION" "TERMINAL-TOKENS"
           "INITIAL-TOKENS" "TERMINAL-NODES" "INITIAL-NODES" "FOLLOW" "PRECEDE"
           "DISPLAY-GAMMAS-INV" "DISPLAY-GAMMAS" "GAMMAS-INV" "GAMMAS"
           "GAMMA-INV-OF-NODE" "GAMMA-OF-NODE" "MINIMIZE-CROSSES" "IS-SUBSET-OF?"
           "IS-PARTIAL-OF?" "EQUAL?" "MAKE-REFLEXIVE" "IS-REFLEXIVE?" "MAKE-SYMETRIC"
           "SYMETRIC" "IS-SYMETRIC?" "REVERSE-EDGES" "REMOVE-EDGES" "REMOVE-EDGE"
           "CONTAINS-EDGE?" "ADD-EDGES" "ADD-EDGE" "EDGES" "REMOVE-NODES" "REMOVE-NODE"
           "CONTAINS-NODE?" "ADD-NODES" "ADD-NODE" "NODES" "EMPTY" "GRAF" "GRAF-SUMMARY"
           "GRAF-MINIMIZE-CROSSES-VERBOSELY" "GRAF-ONLIF-BUILD-EDGES"
           "GRAF-ONLIF-BUILD-EDGES-CURRENT" "GRAF-ONLIFBE-REPLACE" "FIND-ONLIFBE"
           "GRAF-ONL-ADD-INDICES-IJ" "GRAF-BUILD-ORDERED-NODE-LIST"
           "GRAF-BUILD-ORDERED-NODE-LIST" "GRAF-BUILD-ORDERED-NODE-LIST-STEP"
           "GRAF-MAKE-TREE" "GRAF-MAKE-REFLEXIVE" "GRAF-REFLEXIVE?" "AND-L"
           "GRAF-TERMINAL-TOKENS" "GRAF-INITIAL-TOKENS" "GRAF-TERMINAL-NODES"
           "GRAF-INITIAL-NODES" "SET-UNION-L" "GRAF-PRECEDE-L" "GRAF-FOLLOW-L"
           "GRAF-PRECEDE" "GRAF-FOLLOW" "GRAF-REPLACE-NODE" "GRAF-MINIMIZE-CROSSES"
           "LPRINT" "GRAF-MAKE-SYMETRIC" "GRAF-SYMETRIC?" "GRAF-SYMETRIC"
           "GRAF-REVERSE-EDGES" "EDGES-REVERSE" "GRAF-DISPLAY-GAMMAS-INV"
           "GRAF-DISPLAY-GAMMAS" "GRAF-GAMMAS-INV" "GRAF-GAMMAS"
           "GRAF-CONNECTED-CLASSES" "GRAF-GAMMA-INV-CLOSURE" "GRAF-GAMMA-CLOSURE"
           "GRAF-GAMMA-INV" "GRAF-GAMMA" "EDGES-PREV-NODES" "EDGES-NEXT-NODES"
           "GRAF-INTERSECTION" "GRAF-UNION" "GRAF-SUBSET?" "GRAF-PARTIAL?" "GRAF-EQUAL?"
           "EDGES-REMOVE-NODES" "EDGES-REMOVE-NODE" "GRAF-REMOVE-EDGE"
           "GRAF-REMOVE-EDGES" "GRAF-ADD-EDGES" "GRAF-ADD-EDGE" "GRAF-CONTAINS-EDGE?"
           "GRAF-EDGES" "GRAF-REMOVE-NODE" "GRAF-ADD-NODES" "GRAF-ADD-NODE"
           "GRAF-CONTAINS-NODE?" "GRAF-NODES" "GRAF-EMPTY" "SET-UNIQUE-L" "SET-MEMBER"
           "SET-INTERSECTION" "SET-DIFF" "SET-UNION" "SET-REMOVE" "SET-EQUAL?"
           "SET-SUBSET?" "SET-CONTAINS?")
  (:documentation
   "This file defines the GRAF class.
    These are oriented graphs
    This is an old implementation adapted from a scheme version.
    The GRAF class merely encapsulates list-based data structures
    and functions manipulating graphs.
    
    Copyright Pascal J. Bourguignon 1996 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details."))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.OBSOLETE-OR-INCOMPLEPTE.GRAF")


;;  egrep -e '^[(]DEFUN|^[(]DEFMETHOD|^[(]DEFCLASS' pjb-graph.lisp | sed -e 's/\((DEFUN\|(DEFMETHOD\|(DEFCLASS\)/;;/' | sort


;; ADD-EDGE ((SELF GRAF) E)
;; ADD-EDGES ((SELF GRAF) LE)
;; ADD-NODE ((SELF GRAF) N)
;; ADD-NODES ((SELF GRAF) LN)
;; AND-L (L)
;; CONTAINS-EDGE? ((SELF GRAF) E)
;; CONTAINS-NODE? ((SELF GRAF) N)
;; DISPLAY-GAMMAS ((SELF GRAF))
;; DISPLAY-GAMMAS-INV ((SELF GRAF))
;; EDGES ((SELF GRAF))
;; EDGES-NEXT-NODES (LE N)
;; EDGES-PREV-NODES (LE N)
;; EDGES-REMOVE-NODE (LE N)
;; EDGES-REMOVE-NODES (LE LN)
;; EDGES-REVERSE (LE)
;; EMPTY ((SELF GRAF))
;; EQUAL? ((SELF GRAF) OTHER-GRAF)
;; FIND-ONLIFBE (LI E)
;; FOLLOW ((SELF GRAF) N)
;; GAMMA-INV-OF-NODE ((SELF GRAF) N)
;; GAMMA-OF-NODE ((SELF GRAF) N)
;; GAMMAS ((SELF GRAF))
;; GAMMAS-INV ((SELF GRAF))
;; GRAF ()
;; GRAF-ADD-EDGE (G E)
;; GRAF-ADD-EDGES (G LE) 
;; GRAF-ADD-NODE (G N)
;; GRAF-ADD-NODES (G LN) 
;; GRAF-BUILD-ORDERED-NODE-LIST (G)
;; GRAF-BUILD-ORDERED-NODE-LIST (G)
;; GRAF-BUILD-ORDERED-NODE-LIST-STEP (G L)
;; GRAF-BUILD-ORDERED-NODE-LIST-STEP (G L)
;; GRAF-CONNECTED-CLASSES (G)
;; GRAF-CONTAINS-EDGE? (G E)
;; GRAF-CONTAINS-NODE? (G N)
;; GRAF-DIAGRAM (G OUT NODE-NAME)
;; GRAF-DIAGRAM-GENERATE-EDGES (OUT LEIJ N)
;; GRAF-DIAGRAM-GENERATE-LINE (OUT N L FROM TO VERTICES)
;; GRAF-DIAGRAM-GENERATE-NODE (OUT N L X Y SX SY NODE)
;; GRAF-DIAGRAM-GENERATE-VERTEX (OUT N L X Y)
;; GRAF-DISPLAY-GAMMAS (G)
;; GRAF-DISPLAY-GAMMAS-INV (G)
;; GRAF-EDGES (G)
;; GRAF-EMPTY ()
;; GRAF-EQUAL? (G1 G2)
;; GRAF-FOLLOW (G N)
;; GRAF-FOLLOW-L (G LN)
;; GRAF-GAMMA (G N)
;; GRAF-GAMMA-CLOSURE (G N)
;; GRAF-GAMMA-INV (G N)
;; GRAF-GAMMA-INV-CLOSURE (G N)
;; GRAF-GAMMAS (G)
;; GRAF-GAMMAS-INV (G)
;; GRAF-INITIAL-NODES (G)
;; GRAF-INITIAL-TOKENS (G)
;; GRAF-INTERSECTION (G1 G2)
;; GRAF-MAKE-REFLEXIVE (G)
;; GRAF-MAKE-SYMETRIC (G)
;; GRAF-MAKE-TREE (G ROOT)
;; GRAF-MINIMIZE-CROSSES (G)
;; GRAF-MINIMIZE-CROSSES-COUNT-CROSSES (G NV) ; private
;; GRAF-MINIMIZE-CROSSES-SEARCH-SWAP (G NV NC CC SCC) ; private
;; GRAF-NODES (G)
;; GRAF-ONL-ADD-INDICES-IJ (LL I J N)
;; GRAF-ONLIF-BUILD-EDGES (G LI N)
;; GRAF-ONLIF-BUILD-EDGES-CURRENT (G LI CLI N)
;; GRAF-ONLIFBE-REPLACE (LI LE N)
;; GRAF-PARTIAL? (G1 G2)
;; GRAF-PRECEDE (G N)
;; GRAF-PRECEDE-L (G LN)
;; GRAF-REFLEXIVE? (G)
;; GRAF-REMOVE-EDGE (G E)
;; GRAF-REMOVE-EDGES (G LE)
;; GRAF-REMOVE-NODE (G N)
;; GRAF-REMOVE-NODES (G LN)
;; GRAF-REPLACE-NODE (G ON NN)
;; GRAF-REVERSE-EDGES (G)
;; GRAF-SUBSET? (G1 G2)
;; GRAF-SUMMARY ()
;; GRAF-SYMETRIC (G)
;; GRAF-SYMETRIC? (G)
;; GRAF-TERMINAL-NODES (G)
;; GRAF-TERMINAL-TOKENS (G)
;; GRAF-UNION (G1 G2)
;; INITIAL-NODES ((SELF GRAF))
;; INITIAL-TOKENS ((SELF GRAF))
;; IS-PARTIAL-OF? ((SELF GRAF) OTHER-GRAF)
;; IS-REFLEXIVE? ((SELF GRAF))
;; IS-SUBSET-OF? ((SELF GRAF) OTHER-GRAF)
;; IS-SYMETRIC? ((SELF GRAF))
;; LPRINT (L)
;; MAKE-INTERSECTION ((SELF GRAF) OTHER-GRAF)
;; MAKE-REFLEXIVE ((SELF GRAF))
;; MAKE-SYMETRIC ((SELF GRAF))
;; MAKE-TREE ((SELF GRAF) ROOT)
;; MAKE-UNION ((SELF GRAF) OTHER-GRAF)
;; MINIMIZE-CROSSES ((SELF GRAF))
;; NODE-POS-X (N)
;; NODE-POS-Y (N)
;; NODE-SIZE-X (N)
;; NODE-SIZE-Y (N)
;; NODES ((SELF GRAF))
;; PRECEDE ((SELF GRAF) N)
;; REMOVE-EDGE ((SELF GRAF) E)
;; REMOVE-EDGES ((SELF GRAF) LE)
;; REMOVE-NODE ((SELF GRAF) N)
;; REMOVE-NODES ((SELF GRAF) N)
;; REVERSE-EDGES ((SELF GRAF))
;; SET-CONTAINS? (S E) 
;; SET-DIFF (S1 S2)
;; SET-EQUAL? (S1 S2)
;; SET-INTERSECTION (S1 S2)
;; SET-MEMBER (S LS)
;; SET-REMOVE (S E)
;; SET-SUBSET? (S1 S2)
;; SET-UNION (S1 S2)
;; SET-UNION-L (LS)
;; SET-UNIQUE-L (LS)
;; SYMETRIC ((SELF GRAF))
;; TERMINAL-NODES ((SELF GRAF))
;; TERMINAL-TOKENS ((SELF GRAF))



;;----------------------------------------------------------------------

(defun set-contains? (s e) 
  (cond
    ((null s) nil) 
    ((equalp (car s) e) t)
    (t (set-contains? (cdr s) e)))
  ) ;;SET-CONTAINS?

(defun set-subset? (s1 s2)
  (cond
    ((null s1)  t)
    ((set-contains? s2 (car s1)) (set-subset? (cdr s1) s2)))
  ) ;;SET-SUBSET?

(defun set-equal? (s1 s2)
  (and (set-subset? s1 s2) (set-subset? s2 s1))
  ) ;;SET-EQUAL?

(defun set-remove (s e)
  (cond
    ((null s) s)
    ((equalp (car s) e) (cdr s))        ; e is unique in a set
    (t (cons (car s) (set-remove (cdr s) e))))
  ) ;;SET-REMOVE


(defun set-union (s1 s2)
  (if (null s2)
      s1
      (if (set-contains? s1 (car s2)) 
          (set-union s1 (cdr s2))
          (set-union (cons (car s2) s1) (cdr s2))))
  ) ;;SET-UNION

;; ls = list of set '((a b c) (c d e) (f i)) --> '(a b c d e f i)
(defun set-union-l (ls)
  (cond 
    ((null ls) ls)
    ((null (cdr ls)) (car ls))
    (t (set-union-l (cons (set-union (car ls) (cadr ls)) (cddr ls)))))
  ) ;;SET-UNION-L

(defun set-diff (s1 s2)
  (if (null s1)
      s1
      (if (set-contains? s2 (car s1))
          (set-diff (cdr s1) s2)
          (cons (car s1) (set-diff (cdr s1) s2))))
  ) ;;SET-DIFF

(defun set-intersection (s1 s2)
  (if (null s1)
      s1
      (if (set-contains? s2 (car s1))
          (cons (car s1) (set-intersection (cdr s1) s2))
          (set-intersection (cdr s1) s2)))
  ) ;;SET-INTERSECTION

(defun set-member (s ls)
  (if (null ls)
      nil
      (if (set-equal? s (car ls))
          t
          (set-member s (cdr ls))))
  ) ;;SET-MEMBER

(defun set-unique-l (ls)
  (if (null ls)
      nil
      (if (set-member (car ls) (cdr ls))
          (set-unique-l (cdr ls))
          (cons (car ls) (set-unique-l (cdr ls)))))
  ) ;;SET-UNIQUE-L
            
    
    
(defun graf-empty ()
  '(())
  ) ;;GRAF-EMPTY

(defun graf-nodes (g)
  (car g)
  ) ;;GRAF-NODES

(defun graf-edges (g)
  (cdr g)
  ) ;;GRAF-EDGES

(defun graf-contains-node? (g n)
  (set-contains? (graf-nodes g) n)
  ) ;;GRAF-CONTAINS-NODE?

(defun graf-add-node (g n)
  (if (set-contains? (graf-nodes g) n)
      g
      (cons (cons n (graf-nodes g)) (graf-edges g)))
  ) ;;GRAF-ADD-NODE

(defun graf-add-nodes (g ln) 
  (if (null ln) g (graf-add-nodes (graf-add-node g (car ln)) (cdr ln)))
  ) ;;GRAF-ADD-NODES

(defun edges-remove-node (le n)
  (cond
    ((null le)      le)
    ((or (equalp (caar le) n) (equalp (cadar le) n))
     (edges-remove-node (cdr le) n))
    (t              (cons (car le) (edges-remove-node (cdr le) n))))
  ) ;;EDGES-REMOVE-NODE

(defun edges-remove-nodes (le ln)
  (if (null ln)
      le
      (edges-remove-nodes (edges-remove-node le (car ln)) (cdr ln)))
  ) ;;EDGES-REMOVE-NODES


(defun graf-remove-node (g n)
  (cons (set-remove (graf-nodes g) n) (edges-remove-node (graf-edges g) n))
  ) ;;GRAF-REMOVE-NODE

(defun graf-remove-nodes (g ln)
  (cons (set-diff (graf-nodes g) ln)
        (edges-remove-nodes (graf-edges g) ln))
  ) ;;GRAF-REMOVE-NODES


(defun graf-contains-edge? (g e)
  (set-contains? (graf-edges g) e)
  ) ;;GRAF-CONTAINS-EDGE?

(defun graf-add-edge (g e)
  (if (and (consp e) (null (cddr e))
           (graf-contains-node? g (car e)) (graf-contains-node? g (cadr e))
           (not (graf-contains-edge? g e)))
      (cons (graf-nodes g) (cons e (graf-edges g)))
      g)
  ) ;;GRAF-ADD-EDGE

(defun graf-add-edges (g le) 
  (if (null le) g (graf-add-edges (graf-add-edge g (car le)) (cdr le)))
  ) ;;GRAF-ADD-EDGES

(defun graf-remove-edges (g le)
  (cons (graf-nodes g) (set-diff (graf-edges g) le))
  ) ;;GRAF-REMOVE-EDGES

(defun graf-remove-edge (g e)
  (graf-remove-edges g (list e))
  ) ;;GRAF-REMOVE-EDGE




(defun graf-equal? (g1 g2)
  (and (set-equal? (graf-nodes g1) (graf-nodes g2))
       (set-equal? (graf-edges g1) (graf-edges g2)))
  ) ;;GRAF-EQUAL?

(defun graf-partial? (g1 g2)
  (and (set-equal? (graf-nodes g1) (graf-nodes g2))
       (set-subset? (graf-edges g1) (graf-edges g2)))
  ) ;;GRAF-PARTIAL?

(defun graf-subset? (g1 g2)
  (and (set-subset? (graf-nodes g1) (graf-nodes g2))
       (set-subset? (graf-edges g1) (graf-edges g2)))
  ) ;;GRAF-SUBSET?

(defun graf-union (g1 g2)
  (cons (set-union (graf-nodes g1) (graf-nodes g2))
        (set-union (graf-edges g1) (graf-edges g2)))
  ) ;;GRAF-UNION

(defun graf-intersection (g1 g2)
  (let ((nodes (set-intersection (graf-nodes g1) (graf-nodes g2))))
    (cons nodes
          (edges-remove-nodes 
           (set-intersection (graf-edges g1) (graf-edges g2))
           (set-diff 
            (set-union (graf-nodes g1) (graf-nodes g2)) nodes))))
  ) ;;GRAF-INTERSECTION


(defun edges-next-nodes (le n)
  (cond
    ((null le)              le)
    ((equalp (caar le) n)   (cons (car le) (edges-next-nodes (cdr le) n)))
    (t                      (edges-next-nodes (cdr le) n)))
  ) ;;EDGES-NEXT-NODES

(defun edges-prev-nodes (le n)
  (cond
    ((null le)              le)
    ((equalp (cadar le) n)  (cons (car le) (edges-prev-nodes (cdr le) n)))
    (t                      (edges-prev-nodes (cdr le) n)))
  ) ;;EDGES-PREV-NODES




(defun graf-gamma (g n)
  (edges-next-nodes (graf-edges g) n)
  ) ;;GRAF-GAMMA

(defun graf-gamma-inv (g n)
  (edges-prev-nodes (graf-edges g) n)
  ) ;;GRAF-GAMMA-INV


(defun graf-follow (g n)
  (mapcar 'cadr (graf-gamma g n))
  ) ;;GRAF-FOLLOW

(defun graf-precede (g n)
  (mapcar 'car (graf-gamma-inv g n))
  ) ;;GRAF-PRECEDE


(defun graf-follow-l (g ln)
  (if (null ln)
      nil
      (set-union (graf-follow g (car ln)) (graf-follow-l g (cdr ln))))
  ) ;;GRAF-FOLLOW-L
                

(defun graf-precede-l (g ln)
  (if (null ln)
      nil
      (set-union (graf-precede g (car ln)) (graf-precede-l g (cdr ln))))
  ) ;;GRAF-PRECEDE-L

(defun graf-gamma-closure (g n)
  (let* ( (prev-closure '())
         (new-closure (list n))
          (next (graf-follow-l g new-closure)))
    (loop while (not (set-equal? prev-closure new-closure)) do 
         (setq prev-closure new-closure)
         (setq new-closure (set-union new-closure next))
         (setq next (graf-follow-l g new-closure))
         )
    new-closure)
  ) ;;GRAF-GAMMA-CLOSURE


(defun graf-gamma-inv-closure (g n)
  (let* ( (prev-closure '())
         (new-closure (list n))
          (next (graf-precede-l g new-closure)))
    (loop while (not (set-equal? prev-closure new-closure)) do 
         (setq prev-closure new-closure)
         (setq new-closure (set-union new-closure next))
         (setq next (graf-precede-l g new-closure))
         )
    new-closure)
  ) ;;GRAF-GAMMA-INV-CLOSURE


(defun graf-connected-classes (g)
  (set-unique-l 
   (mapcar (lambda (x) 
             (set-intersection 
              (graf-gamma-inv-closure g x) 
              (graf-gamma-closure g x))) 
           (graf-nodes g)))
  ) ;;GRAF-CONNECTED-CLASSES
            
            
(defun graf-gammas (g)
  (mapcar 'graf-gamma 
          (make-list (length (graf-nodes g)) :initial-element g) 
          (graf-nodes g))
  ) ;;GRAF-GAMMAS

(defun graf-gammas-inv (g)
  (mapcar 'graf-gamma-inv
          (make-list (length (graf-nodes g)) :initial-element g) 
          (graf-nodes g))
  ) ;;GRAF-GAMMAS-INV

(defun graf-display-gammas (g)
  (mapcar (lambda (n) 
            (format t "~a -> ~a ~%" n (mapcar 'cadr (graf-gamma g n))))
          (graf-nodes g))
  ) ;;GRAF-DISPLAY-GAMMAS

(defun graf-display-gammas-inv (g)
  (mapcar (lambda (n) 
            (format t "~a <- ~a ~%" n (mapcar 'car (graf-gamma-inv g n)))) 
          (graf-nodes g))
  ) ;;GRAF-DISPLAY-GAMMAS-INV


(defun edges-reverse (le)
  (if (null le)
      le
      (cons (list (cadar le) (caar le)) (edges-reverse (cdr le))))
  ) ;;EDGES-REVERSE


(defun graf-reverse-edges (g)
  (cons (graf-nodes g) (edges-reverse (graf-edges g)))
  ) ;;GRAF-REVERSE-EDGES
        
(defun graf-symetric (g)
  (graf-reverse-edges g)
  ) ;;GRAF-SYMETRIC

(defun graf-symetric? (g)
  (set-equal? (graf-edges g) (edges-reverse (graf-edges g)))
  ) ;;GRAF-SYMETRIC?

(defun graf-make-symetric (g)
  (graf-union g (graf-reverse-edges g))
  ) ;;GRAF-MAKE-SYMETRIC
    
(defun lprint (l)
  (progn (print (length l))
         (cond 
           ((null l)           (print "---"))
           ((not (consp l))    (print l))
           (t                  (progn (print (car l)) (lprint (cdr l))))))
  ) ;;LPRINT


(defun graf-minimize-crosses-count-crosses (g nv) ; private
  (declare (type (simple-array cons *) nv))
  (let (  
        (nc (array-dimension nv 0))
        (cc (make-array (array-dimension nv 0) :initial-element 0))
        )
    (do ((i 0      (1+ i))) ((>= i nc))
      (do ((j (1+ i) (1+ j))) ((>= j nc))
        (do ((k (1+ j) (1+ k))) ((>= k nc))
          (do ((l (1+ k) (1+ l))) ((>= l nc))
            ;;  (format t 
            ;;  "~s ~s ~s ~s ([~s]~s,[~s]~s)=~s ([~s]~s,[~s]~s)=~s  ~s ~s~%" 
            ;;      i j k l
            ;;      i (aref nv i)
            ;;      k (aref nv k)
            ;;      (graf-contains-edge? g 
            ;;              (list (aref nv i) (aref nv k)))
            ;;      j (aref nv j)
            ;;      l (aref nv l)
            ;;      (graf-contains-edge? g 
            ;;              (list (aref nv j) (aref nv l)))
            ;;      (and
            ;;          (graf-contains-edge? g 
            ;;                  (list (aref nv i) (aref nv k)))
            ;;          (graf-contains-edge? g 
            ;;                  (list (aref nv j) (aref nv l)))
            ;;      )
            ;;      cc)
            (when (and (graf-contains-edge?
                        g (list (aref nv i) (aref nv k)))
                       (graf-contains-edge?
                        g (list (aref nv j) (aref nv l))))
              (setf (aref cc i) (1+ (aref cc i))
                    (aref cc j) (1+ (aref cc j))
                    (aref cc k) (1+ (aref cc k))
                    (aref cc l) (1+ (aref cc l)))
              )
            ))))
    cc) ;;LET
  )     ;;GRAF-MINIMIZE-CROSSES-COUNT-CROSSES


(defvar graf-minimize-crosses-verbosely  nil)

(defun graf-minimize-crosses-search-swap (g nv nc cc scc) ; private
  (declare (type (simple-array cons *) nv)
           (type (or null (simple-array integer *)) cc)
           (type integer nc scc))
  (let (
        (bi   nc)                       ; best swap index.
        (bj   nc)                       ; best swap index.
        (bsv  (copy-seq nv))            ; best swapped array.
        (bsc  (and cc (copy-seq cc))) ; best swapped count of crosses array.
        (bssc scc)       ; best sum of swapped count of crosses array.
        (sv   nil)                      ; swapped array.
        (sc   nil)                  ; swapped counts of crosses array.
        (ssc  0)              ; sum of swapped count of crosses array.
        )
    (declare (type (or null (simple-array integer *)) sc bsc)
             (type (or null (simple-array cons    *)) sv bsv))
    (do ((i 0 (1+ i))) ((<= nc i))
      (do ((j (1+ i) (1+ j))) ((<= nc j))
        (setq sv (copy-seq nv))
        (psetf (aref sv i) (aref sv j)
               (aref sv j) (aref sv i))
        (setq sc (graf-minimize-crosses-count-crosses g sv))
        (setq ssc (reduce (function +) sc))
        (if (< ssc bssc)
            (progn
              (if graf-minimize-crosses-verbosely
                  (format t "found less: ~s~%" (/ ssc 4)))
              (setq bi i)
              (setq bj i)
              (setq bsv (copy-seq sv))
              (setq bsc (copy-seq sc))
              (setq bssc ssc)
              ))))
    (if (= bi nc)
        nil
        (list bsv bsc bssc)
        ))
  ) ;;GRAF-MINIMIZE-CROSSES-SEARCH-SWAP


;;-----------------------
;;graf-minimize-crosses:
;;-----------------------
;;compute the new count of crosses
;;search for a better swap
;;while we can find a better swap do
;;  swap the nodes
;;  compute the new count of crosses
;;  search for a better swap
;;end

(defun graf-minimize-crosses (g)
  (if (not (graf-symetric? g))
      (graf-minimize-crosses (graf-make-symetric g))
      (let* ((nv (make-array (length (graf-nodes g))
                             :initial-contents (graf-nodes g)))
             (nc (length (graf-nodes g)))
             (cc  (graf-minimize-crosses-count-crosses g nv)) ; count of crosses array.
             (scc (reduce (function +) cc)) ; sum of cc.
             (ss  (graf-minimize-crosses-search-swap g nv nc cc scc)) ; search-swap result.
             )
        (declare (type (or null (simple-array integer *)) cc)
                 (type (simple-array cons *) nv)
                 (type integer nc scc)
                 (type list ss))
        (loop while (not (null ss)) do
             (setq nv (car ss))
             (setq cc (cadr ss))
             (setq scc (caddr ss))
             (setq ss (graf-minimize-crosses-search-swap g nv nc cc scc))
             )
        (if graf-minimize-crosses-verbosely
            (format t "crosses:    ~s ~%" cc))
        nv
        ))
  ) ;;GRAF-MINIMIZE-CROSSES
        
        
        
(defun graf-replace-node (g on nn)
  (graf-add-edges
   (graf-add-nodes (graf-empty)
                   (mapcar (lambda (n) (if (eq n on) nn n)) (graf-nodes g)))
   (mapcar (lambda (e) 
             (mapcar (lambda (n) (if (eq n on) nn n)) e)) 
           (graf-edges g)))
  ) ;;GRAF-REPLACE-NODE


;;TODO:(defun graf-collapse-links (g)
;;TODO:  (if (not (graf-symetric? g))
;;TODO:      (graf-collapse-links (graf-make-symetrical g))
;;TODO:      (graf-collapse-links-remove-link-nodes 
;;TODO:          (graf-collapse-link-collapse-paths g))))
;;TODO:
;;TODO:
;;TODO:(defun graf-collapse-link-get-path-from (path links)
;;TODO:  (if (null links)
;;TODO:      (cons links 
;;TODO:(defun graf-collapse-link-get-a-path (links)
;;TODO:  (if (null links)
;;TODO:      links
;;TODO:      (graf-collapse-link-get-path-from (car links) (cdr links))))
;;TODO:      
;;TODO:(defun graf-collapse-link-collapse-paths (g)
;;TODO:  (let (
;;TODO:          (links (flatten (mapcar (lambda (gam) 
;;TODO:                      (if (= (length gam) 2) gam '())) 
;;TODO:                      (graf-gammas g))))
;;TODO:          (paths '())
;;TODO:          (res)
;;TODO:      )
;;TODO:      
;;TODO:      (setq res (graf-collapse-link-get-a-path links))
;;TODO:      (loop while (not (null res)) do
;;TODO:          (setq links (car res))
;;TODO:          (append! paths (cdr res))
;;TODO:          (setq res (graf-collapse-link-get-a-path links))
;;TODO:      )
;;TODO:      
;;TODO:      (do ((curps paths (cdr curps))) ((null curps))
;;TODO:          (mapcar (lambda (oldnode)
;;TODO:              (setq g (graf-replace-node g oldnode (cdar curps))))
;;TODO:              (caar curps)))
;;TODO:      g))
;;TODO:
;;TODO:
;;TODO:(graf-collapse-link-collapse-paths cg)
;;TODO:          (mapcar (lambda (link)
;;TODO:      (let ((links '()))
;;TODO:          ; find the links ie. nodes with two adjacent nodes.
;;TODO:          (do ((nodes (graf-nodes g) (cdr nodes))) ((null nodes))
;;TODO:              (let ((gamma (set-remove 
;;TODO:                              (graf-gamma g (car nodes)) 
;;TODO:                              (list (car nodes) (car nodes)))))
;;TODO:                  (if (= 2 (length gamma))
;;TODO:                      (setq links (cons (cons (list (car nodes)) gamma) 
;;TODO:                                      links)))))
;;TODO:(format t "links=~s~%" links)
;;TODO:          (let ((linkheads (mapcar 'car links)))
;;TODO:          ; foreach linkhead 
;;TODO:          ;   foreach edge from linkhead
;;TODO:          ;      if adjacent node from linkhead is in linkheads then
;;TODO:          ;         collapse them
;;TODO:              (mapcar (lambda (link) 
;;TODO:                  (mapcar (lambda (edges)
;;TODO:                      (if (set-contains? linkheads (cdr edges))
;;TODO:                          (graf-replace-node g (car link) 
;;TODO:                              (list (car link) (cdr edges)))))
;;TODO:                      (cdr link)))
;;TODO:                  links)
;;TODO:              (do ((nodes links (cdr nodes))) ((null nodes))
;;TODO:                  (let (  
;;TODO:                          (node (caar nodes))
;;TODO:                          (next (set-remove (mapcar 'cadr (cdar nodes)) (caar nodes)))
;;TODO:                          )
;;TODO:  (format t "linkheads=~s node=~s nexts=~s ~%" linkheads  node next)
;;TODO:                      (do ((tails next (cdr tails))) ((null tails))
;;TODO:                          (if (set-contains? linkheads (car tails))
;;TODO:                              (progn
;;TODO:                                  
;;TODO:                              ))
;;TODO:                      )
;;TODO:                  )
;;TODO:              ))
;;TODO:      )))
;;TODO:(graf-collapse-links cg)
;;TODO:
;;TODO:






        

(defun graf-initial-nodes (g)
  (let ((tn '()) (nn (graf-nodes g)))
    (loop while (not (set-equal? tn nn)) do 
         (setq tn nn)
         (setq nn (set-union-l 
                   (mapcar (lambda (x) 
                             (let ((f (graf-precede g x)))
                               (if (null f)
                                   (list x)
                                   f))) tn))))
    nn)
  ) ;;GRAF-INITIAL-NODES

(defun graf-terminal-nodes (g)
  (let ((tn '()) (nn (graf-nodes g)))
    (loop while (not (set-equal? tn nn)) do 
         (setq tn nn)
         (setq nn (set-union-l 
                   (mapcar (lambda (x) 
                             (let ((f (graf-follow g x)))
                               (if (null f)
                                   (list x)
                                   f))) tn))))
    nn)
  ) ;;GRAF-TERMINAL-NODES

(defun graf-initial-tokens (g)
  (delete nil (mapcar 
               (lambda (x) (if (graf-precede g x) nil x)) (graf-nodes g)))
  ) ;;GRAF-INITIAL-TOKENS
        
(defun graf-terminal-tokens (g)
  (delete nil (mapcar 
               (lambda (x) (if (graf-follow g x) nil x)) (graf-nodes g)))
  ) ;;GRAF-TERMINAL-TOKENS
        

(defun and-l (l)
  (cond
    ((null l)   t)
    ((car l)    (and-l (cdr l)))
    (t          nil))
  ) ;;AND-L
        
(defun graf-reflexive? (g)
  (and-l
   (mapcar (lambda (n) (graf-contains-edge? g (list n n))) (graf-nodes g)))
  ) ;;GRAF-REFLEXIVE?
    
(defun graf-make-reflexive (g)
  (graf-add-edges g (mapcar (lambda (x) (list x x)) (graf-nodes g)))
  ) ;;GRAF-MAKE-REFLEXIVE


(defun graf-make-tree (g root)
  (cons root 
        (mapcar 
         (lambda (x) (graf-make-tree (graf-remove-node g root) x)) 
         (graf-follow g root)))
  ) ;;GRAF-MAKE-TREE



;; let s[0] be the set of terminal tokens of g
;; let s[i+1] be the set of precedent nodes of s[i] less the nodes already in 
;;   union(s[j],j in [0..i]).
;; k=MU i, s[k] equals the set of initial tokens of g
;; result=(s[k],...,s[0])

(defun graf-build-ordered-node-list-step (g l)
  (if (null (car l))  
      (cdr l)
      (graf-build-ordered-node-list-step
       g (cons (set-diff (graf-precede-l g (car l)) (set-union-l l)) l)))
  ) ;;GRAF-BUILD-ORDERED-NODE-LIST-STEP


;;; (DEFUN GRAF-BUILD-ORDERED-NODE-LIST (G)
;;;   (GRAF-BUILD-ORDERED-NODE-LIST-STEP G (LIST (GRAF-TERMINAL-TOKENS G))))

(defun graf-build-ordered-node-list (g)
  (reverse (graf-build-ordered-node-list-step
            g (list (graf-initial-tokens g))))
  ) ;;GRAF-BUILD-ORDERED-NODE-LIST
        
;;; (let* ((first-layer (graf-initial-tokens g))
;;;         (previous-layers first-layer)
;;;         (next-layer (graf-follow-l g first-layer)))
;;;     ;; si un noed dans next-layer a un precedant non dans previous-layers
;;;     ;; alors on le laisse pour plus tard.
        
;;; (graf-build-ordered-node-list-step g (list (graf-terminal-tokens g))))
    
;;; layers_suite[0]=((graf-inital-tokens g))
;;; previous_layers[0]=union(i,layers_suite[0][i])
;;;    
;;; new_layer[i]=(graf-follow-l g layers_suite[i][0]) 
;;;                 minus nodes who has a previous not in previous_layers[i].
    
    
    
    
(defun graf-onl-add-indices-ij (ll i j n)
  (cond
    ((null ll)
     nil)
    ((null (car ll))
     (if (null (cdr ll))
         (list nil)
         (cons nil (graf-onl-add-indices-ij (cdr ll) (1+ i) 0 n))))
    (t 
     (let ((r (graf-onl-add-indices-ij 
               (cons (cdar ll) (cdr ll)) i (1+ j) (1+ n))))
       (cons (cons (list (caar ll) i j n) (car r)) (cdr r)))))
  ) ;;GRAF-ONL-ADD-INDICES-IJ


(defun find-onlifbe (li e)
  (cond
    ((null li) nil)
    ((equal (caar li) e) (car li))
    (t (find-onlifbe (cdr li) e)))
  ) ;;FIND-ONLIFBE


(defun graf-onlifbe-replace (li le n)
  (if (null le)
      nil
      (cons (list (find-onlifbe li (caar le)) (find-onlifbe li (cadar le)) n)
            (graf-onlifbe-replace li (cdr le) (1+ n))))
  ) ;;GRAF-ONLIFBE-REPLACE


(defun graf-onlif-build-edges-current (g li cli n)
  (if (null cli)
      nil
      (let* ((ni (car cli)) (le (graf-gamma g (car ni)))
             (lr (if (null le) nil (graf-onlifbe-replace li le n)))
             (m  (if (null le) n   (1+ (apply 'max (mapcar #'third lr))))))
        (append lr
                (graf-onlif-build-edges-current g li (cdr cli) m))))
  ) ;;GRAF-ONLIF-BUILD-EDGES-CURRENT


(defun graf-onlif-build-edges (g li n)
  (graf-onlif-build-edges-current g li li n)
  ) ;;GRAF-ONLIF-BUILD-EDGES


(defun graf-diagram-generate-node (out n l x y sx sy node)
  (format out "symbol ~s~%" n)
  (format out "   layer ~s~%" l)
  (format out "   shape \"Rectangle\"~%")
  (format out "   location ~d.00 ~d.00~%" x y)
  (format out "   size ~d.00 ~d.00~%" sx sy)
  (format out "   framed~%")
  (format out "   fillColor colorIndex 0~%")
  (format out "   frameColor colorIndex 1~%")
  (format out "   shadowColor colorIndex 2~%")
  (format out "   lineWidth 1.00~%")
  (format out "   filled~%")
  (format out "   rtfText {\\rtf0\\ansi{\\fonttbl\\f0\\fswiss Helvetica;}\\margl40\\margr40\\pard\\tx960\\tx1920\\tx2880\\tx3840\\tx4800\\tx5760\\tx6720\\tx7680\\tx8640\\tx9600\\f0\\b\\i0\\ulnone\\qc\\fs20\\fc0\\cf0 ~a}~%" node)
  (format out "   textPlacement middle~%")
  (format out "end~%~%")
  ) ;;GRAF-DIAGRAM-GENERATE-NODE

(defun graf-diagram-generate-vertex (out n l x y)
  (format out "vertex ~s~%" n)
  (format out "   layer ~s~%" l)
  (format out "   location ~d.00 ~d.00~%" x y)
  (format out "end~%~%")
  ) ;;GRAF-DIAGRAM-GENERATE-VERTEX
    
(defun graf-diagram-generate-line (out n l from to vertices)
  (format out "line ~s~%" n)
  (format out "   from ~s~%" from)
  (format out "   to ~s~%" to)
  (do ((v vertices (cdr v)))
      ((null v) nil)
    (format out "   ~s~%" (caar v)))
  (format out "   layer ~s~%" l)
  (format out "   headType arrow~%")
  (format out "   lineWidth ~s~%" 1.00)
  (format out "   filled~%")
  (format out "   frameColor colorIndex ~s~%" 1)
  (format out "   fillColor colorIndex ~s~%" 0)
  (format out "end~%~%")
  ) ;;GRAF-DIAGRAM-GENERATE-LINE


(defun node-size-x (n)
  (declare (ignore n))
  100) ;;NODE-SIZE-X
(defun node-size-y (n)
  (declare (ignore n))
  20) ;;NODE-SIZE-Y
(defun node-pos-x (n)
  (+ 50 (* 180 (second n)))
  ) ;;NODE-POS-X
(defun node-pos-y (n)
  (+ 50 (*  50 (third  n)))
  ) ;;NODE-POS-Y

;;from-to
;;  v[0]=(from.pos.x+from.size.x,   from.pos.y+from.size.y/2)
;;  v[1]=(from.pos.x+from.size.x+4, from.pos.y+from.size.y/2)
;;  v[2]=(to.pos.x-4,               to.pos.y+to.size.y/2)
;;  v[3]=(to.pos.x,                 to.pos.y+to.size.y/2)
;;
;;from-+-+-to
;;  v[0]=(from.pos.x+from.size.x,   from.pos.y+from.size.y/2)
;;  v[1]=(from.pos.x+from.size.x+4, from.pos.y+from.size.y/2)
;;  v[2]=(from.pos.x+from.size.x+20,to.pos.y-4)
;;  v[3]=(to.pos.x-20,              to.pos.y-4)
;;  v[4]=(to.pos.x-4,               to.pos.y+to.size.y/2)
;;  v[5]=(to.pos.x,                 to.pos.y+to.size.y/2)
    
(defun graf-diagram-generate-edges (out leij n)
  (if (null leij)
      nil
      (let ((nn n) (allv nil) (alli nil))
        (do ((cur-eij leij (cdr cur-eij)) )
            ((null cur-eij) nil)
          (let* ((eij      (car cur-eij))  
                 (from    (first eij))
                 (to      (second eij))
                 (linenum (third eij))
                 (lv      nil))
            (setq lv (append lv
                             (list
                              (cons nn
                                    (list 
                                     (+ (node-pos-x from)
                                        (node-size-x from))
                                     (+ (node-pos-y from)
                                        (/ (node-size-y from) 2))))
                              (cons (1+ nn)
                                    (list
                                     (+ (node-pos-x from)
                                        (node-size-x from) 16)
                                     (+ (node-pos-y from)
                                        (/ (node-size-y from) 2)))))))
            (setq nn (+ nn 2))
            (when (not (equal (1+ (second from)) (second to)))
              (setq lv
                    (append lv
                            (list
                             (cons nn (list 
                                       (+ (node-pos-x from)
                                          (node-size-x from) 24)
                                       (+ (node-pos-y to)
                                          (node-size-y to) 16
                                          (* 2 (- (second from)
                                                  (second to))))))
                             (cons (1+ nn)
                                   (list
                                    (- (node-pos-x to) 24)
                                    (+ (node-pos-y to)
                                       (node-size-y to) 16
                                       (* 2 (- (second from)
                                               (second to))))))
                             ))
                    nn (+ nn 2)))
            (setq lv (append lv
                             (list
                              (cons nn
                                    (list 
                                     (- (node-pos-x to) 16)
                                     (+ (node-pos-y to)
                                        (/ (node-size-y to) 2))))
                              (cons (1+ nn)
                                    (list
                                     (node-pos-x to)
                                     (+ (node-pos-y to)
                                        (/ (node-size-y to) 2)))))))
            (setq nn (+ nn 2))
            (setq allv (append lv allv))
            (setq alli (cons (list out linenum linenum 
                                   (fourth from) (fourth to) lv) alli))
            ) ;;LET*
          )   ;;DO
        (mapcar (lambda (x) (graf-diagram-generate-vertex
                             out (first x) (first x) (second x) (third x)))
                allv)
        (mapcar (lambda (x) (apply 'graf-diagram-generate-line x))
                alli)                   
        ))
  ) ;;GRAF-DIAGRAM-GENERATE-EDGES

                    
(defun graf-diagram (g out node-name)
  (declare (type function node-name))
  (format out "#!DG_TEXT-Version-2~%")
  (format out "# D2 Version: Built by rob on Tue Sep 27 14:01:06 PDT 1994~%")
  (format out "~%")
  (format out "windowOrigin 222.00 2.00~%")
  (format out "viewOrigin 1.00 9.00~%")
  (format out "viewSize 825.00 617.00~%")
  (format out "visibleOrigin 0.00 0.00~%")
  (format out "showTools~%")
  (format out "snapToGrid~%")
  (format out "gridSize 4.000~%")
  (format out "defaultFont \"helvetica\"~%")
  (format out "defaultFontSize 10.00~%")
  (format out "printInfoFile \"printinfo\"~%")
  (format out "colorFile \"colors.clr\"~%")
  (format out "~%")
  (let* ((lnij (flatten (graf-onl-add-indices-ij 
                         (graf-build-ordered-node-list g) 0 0 1000)))
         (leij (graf-onlif-build-edges
                g lnij (1+ (apply 'max (mapcar #'fourth lnij))))))
    (mapcar (lambda (x) (graf-diagram-generate-node
                         out (fourth x) (fourth x) 
                         (node-pos-x x) (node-pos-y x)
                         (node-size-x x) (node-size-y x)
                         (apply  node-name (list (car x)))))
            lnij)
    (graf-diagram-generate-edges
     out leij (1+ (apply 'max (mapcar #'third leij))))
    )
  ) ;;GRAF-DIAGRAM
    
;;----------------------------------------------------------------------
;;;     (graf-onlif-build-edges fam 
;;;         (flatten (graf-onl-add-indices-ij 
;;;                     (graf-build-ordered-node-list fam) 0 0 1000)) 2000)
;;;     (graf-Diagram fam 
;;;         (open "/users/pascal/src/common/lisp/graf.diagram2/DiagramText") 
;;;         #'string)





;;----------------------------------------------------------------------


(defun graf-summary ()
  (format t "~%")
  ;; use the following two lines to update the class summary, but skip the first
  ;; semicolon.
  ;; egrep 'DEFCLASS|DEFMETHOD' pjb-graph.lisp |sed -e 's/(DEFCLASS \(.*\)/    (FORMAT T "Class \1~%")/' -e 's/(DEFMETHOD\(.*\)/    (FORMAT T "\1~%")/'
  (format t "Class GRAF ()~%")
  (format t " EMPTY ((SELF GRAF))~%")
  (format t " NODES ((SELF GRAF))~%")
  (format t " ADD-NODE ((SELF GRAF) N)~%")
  (format t " ADD-NODES ((SELF GRAF) LN)~%")
  (format t " CONTAINS-NODE? ((SELF GRAF) N)~%")
  (format t " REMOVE-NODE ((SELF GRAF) N)~%")
  (format t " REMOVE-NODES ((SELF GRAF) N)~%")
  (format t " EDGES ((SELF GRAF))~%")
  (format t " ADD-EDGE ((SELF GRAF) E)~%")
  (format t " ADD-EDGES ((SELF GRAF) LE)~%")
  (format t " CONTAINS-EDGE? ((SELF GRAF) E)~%")
  (format t " REMOVE-EDGE ((SELF GRAF) E)~%")
  (format t " REMOVE-EDGES ((SELF GRAF) LE)~%")
  (format t " REVERSE-EDGES ((SELF GRAF))~%")
  (format t " IS-SYMETRIC? ((SELF GRAF))~%")
  (format t " SYMETRIC ((SELF GRAF))~%")
  (format t " MAKE-SYMETRIC ((SELF GRAF))~%")
  (format t " IS-REFLEXIVE? ((SELF GRAF))~%")
  (format t " MAKE-REFLEXIVE ((SELF GRAF))~%")
  (format t " EQUAL? ((SELF GRAF) OTHER-GRAF)~%")
  (format t " IS-PARTIAL-OF? ((SELF GRAF) OTHER-GRAF)~%")
  (format t " IS-SUBSET-OF? ((SELF GRAF) OTHER-GRAF)~%")
  (format t " MINIMIZE-CROSSES ((SELF GRAF))~%")
  (format t " GAMMA-OF-NODE ((SELF GRAF) N)~%")
  (format t " GAMMA-INV-OF-NODE ((SELF GRAF) N)~%")
  (format t " GAMMAS ((SELF GRAF))~%")
  (format t " GAMMAS-INV ((SELF GRAF))~%")
  (format t " DISPLAY-GAMMAS ((SELF GRAF))~%")
  (format t " DISPLAY-GAMMAS-INV ((SELF GRAF))~%")
  (format t " PRECEDE ((SELF GRAF) N)~%")
  (format t " FOLLOW ((SELF GRAF) N)~%")
  (format t " INITIAL-NODES ((SELF GRAF))~%")
  (format t " TERMINAL-NODES ((SELF GRAF))~%")
  (format t " INITIAL-TOKENS ((SELF GRAF))~%")
  (format t " TERMINAL-TOKENS ((SELF GRAF))~%")
  (format t " MAKE-INTERSECTION ((SELF GRAF) OTHER-GRAF)~%")
  (format t " MAKE-UNION ((SELF GRAF) OTHER-GRAF)~%")
  (format t " MAKE-TREE ((SELF GRAF) ROOT)~%")
  ) ;;GRAF-SUMMARY


(defgeneric empty (self))
(defgeneric nodes (self))
(defgeneric add-node (self n))
(defgeneric add-nodes (self ln))
(defgeneric contains-node\? (self n))
(defgeneric remove-node (self n))
(defgeneric remove-nodes (self ln))
(defgeneric edges (self))
(defgeneric add-edge (self e))
(defgeneric add-edges (self le))
(defgeneric contains-edge\? (self e))
(defgeneric remove-edge (self e))
(defgeneric remove-edges (self le))
(defgeneric reverse-edges (self))
(defgeneric is-symetric\? (self))
(defgeneric symetric (self))
(defgeneric make-symetric (self))
(defgeneric is-reflexive\? (self))
(defgeneric make-reflexive (self))
(defgeneric equal\? (self other-graf))
(defgeneric is-partial-of\? (self other-graf))
(defgeneric is-subset-of\? (self other-graf))
(defgeneric minimize-crosses (self))
(defgeneric gamma-of-node (self n))
(defgeneric gamma-inv-of-node (self n))
(defgeneric gammas (self))
(defgeneric gammas-inv (self))
(defgeneric display-gammas (self))
(defgeneric display-gammas-inv (self))
(defgeneric precede (self n))
(defgeneric follow (self n))
(defgeneric initial-nodes (self))
(defgeneric terminal-nodes (self))
(defgeneric initial-tokens (self))
(defgeneric terminal-tokens (self))
(defgeneric make-intersection (self other-graf))
(defgeneric make-union (self other-graf))
(defgeneric make-tree (self root))


(defclass graf ()
  ((graf 
    :accessor graf
    :initform (graf-empty))))


(defmethod empty ((self graf))
  (setf (graf self) (graf-empty))
  ) ;;EMPTY
    


(defmethod nodes ((self graf))
  (graf-nodes (graf self))
  ) ;;NODES

(defmethod add-node ((self graf) n)
  (setf (graf self) (graf-add-node (graf self) n))
  ) ;;ADD-NODE

(defmethod add-nodes ((self graf) ln)
  (setf (graf self) (graf-add-nodes (graf self) ln))
  ) ;;ADD-NODES
    
(defmethod contains-node? ((self graf) n)
  (graf-contains-node?  (graf self) n)
  ) ;;CONTAINS-NODE?

(defmethod remove-node ((self graf) n)
  (setf (graf self) (graf-remove-node (graf self) n))
  ) ;;REMOVE-NODE
    
(defmethod remove-nodes ((self graf) ln)
  (setf (graf self) (graf-remove-nodes (graf self) ln))
  ) ;;REMOVE-NODES
    

(defmethod edges ((self graf))
  (graf-edges (graf self))
  ) ;;EDGES

(defmethod add-edge ((self graf) e)
  (setf (graf self) (graf-add-edge (graf self) e))
  ) ;;ADD-EDGE

(defmethod add-edges ((self graf) le)
  (setf (graf self) (graf-add-edges (graf self) le))
  ) ;;ADD-EDGES
    
(defmethod contains-edge? ((self graf) e)
  (graf-contains-edge?  (graf self) e)
  ) ;;CONTAINS-EDGE?

(defmethod remove-edge ((self graf) e)
  (setf (graf self) (graf-remove-edge (graf self) e))
  ) ;;REMOVE-EDGE
    
(defmethod remove-edges ((self graf) le)
  (setf (graf self) (graf-remove-edges (graf self) le))
  ) ;;REMOVE-EDGES
    
(defmethod reverse-edges ((self graf))
  (setf (graf self) (graf-reverse-edges (graf self)))
  ) ;;REVERSE-EDGES


(defmethod is-symetric? ((self graf))
  (graf-symetric? (graf self) )
  ) ;;IS-SYMETRIC?

(defmethod symetric ((self graf))
  (let ((g (make-instance 'graf)))
    (setf (graf g) (graf-symetric (graf self)))
    g)
  ) ;;SYMETRIC

(defmethod make-symetric ((self graf))
  (setf (graf self) (graf-make-symetric (graf self)))
  ) ;;MAKE-SYMETRIC


(defmethod is-reflexive? ((self graf))
  (graf-reflexive? (graf self))
  ) ;;IS-REFLEXIVE?
    
(defmethod make-reflexive ((self graf))
  (graf-make-reflexive (graf self))
  ) ;;MAKE-REFLEXIVE


(defmethod equal? ((self graf) other-graf)
  (graf-equal? (graf self) (graf  other-graf))
  ) ;;EQUAL?

(defmethod is-partial-of? ((self graf) other-graf)
  (graf-partial? (graf self) (graf  other-graf))
  ) ;;IS-PARTIAL-OF?

(defmethod is-subset-of? ((self graf) other-graf)
  (graf-subset? (graf self) (graf  other-graf))
  ) ;;IS-SUBSET-OF?



(defmethod minimize-crosses ((self graf))
  (setf (graf self) (graf-minimize-crosses (graf self)))
  ) ;;MINIMIZE-CROSSES
    


(defmethod gamma-of-node ((self graf) n)
  (graf-gamma (graf self) n)
  ) ;;GAMMA-OF-NODE

(defmethod gamma-inv-of-node ((self graf) n)
  (graf-gamma-inv (graf self) n)
  ) ;;GAMMA-INV-OF-NODE

(defmethod gammas ((self graf))
  (graf-gammas (graf self))
  ) ;;GAMMAS

(defmethod gammas-inv ((self graf))
  (graf-gammas-inv (graf self))
  ) ;;GAMMAS-INV

(defmethod display-gammas ((self graf))
  (graf-display-gammas (graf self))
  ) ;;DISPLAY-GAMMAS
        
(defmethod display-gammas-inv ((self graf))
  (graf-display-gammas-inv (graf self))
  ) ;;DISPLAY-GAMMAS-INV
    

(defmethod precede ((self graf) n)
  (graf-precede (graf self) n)
  ) ;;PRECEDE
    
(defmethod follow ((self graf) n)
  (graf-follow (graf self) n)
  ) ;;FOLLOW
    
(defmethod initial-nodes ((self graf))
  (graf-initial-nodes (graf self))
  ) ;;INITIAL-NODES
    
(defmethod terminal-nodes ((self graf))
  (graf-terminal-nodes (graf self))
  ) ;;TERMINAL-NODES

(defmethod initial-tokens ((self graf))
  (graf-initial-tokens (graf self))
  ) ;;INITIAL-TOKENS

(defmethod terminal-tokens ((self graf))
  (graf-terminal-tokens (graf self))
  ) ;;TERMINAL-TOKENS
    
    
    
(defmethod make-intersection ((self graf) other-graf)
  (let ((g (make-instance 'graf)))
    (setf (graf g) (graf-intersection (graf self) (graf other-graf)))
    g)
  ) ;;MAKE-INTERSECTION

(defmethod make-union ((self graf) other-graf)
  (let ((g (make-instance 'graf)))
    (setf (graf g) (graf-union (graf self) (graf other-graf)))
    g)
  ) ;;MAKE-UNION

(defmethod make-tree ((self graf) root)
  (graf-make-tree (graf self) root)
  ) ;;MAKE-TREE




;;;; graf.lisp                        --                     --          ;;;;
