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
;;;;    GPL
;;;;    Copyright Pascal J. Bourguignon 1996 - 2003
;;;;
;;;;    This  program is  free software;  you can  redistribute  it and/or
;;;;    modify it  under the  terms of the  GNU General Public  License as
;;;;    published by the Free Software Foundation; either version 2 of the
;;;;    License, or (at your option) any later version.
;;;;
;;;;    This program  is distributed in the  hope that it  will be useful,
;;;;    but  WITHOUT ANY WARRANTY;  without even  the implied  warranty of
;;;;    MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a  copy of the GNU General Public License
;;;;    along with  this program; see the  file COPYING; if  not, write to
;;;;    the Free  Software Foundation, Inc.,  59 Temple Place,  Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(IN-PACKAGE "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GRAF"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.LIST" "COMMON-LISP")
  (:EXPORT "MAKE-TREE" "MAKE-UNION" "MAKE-INTERSECTION" "TERMINAL-TOKENS"
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
  (:DOCUMENTATION
   "This file defines the GRAF class.
    These are oriented graphs
    This is an old implementation adapted from a scheme version.
    The GRAF class merely encapsulates list-based data structures
    and functions manipulating graphs.
    
    Copyright Pascal J. Bourguignon 1996 - 2003
    This package is provided under the GNU General Public License.
    See the source file for details."))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.GRAF")


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

(DEFUN SET-CONTAINS? (S E) 
  (COND
    ((NULL S) NIL) 
    ((EQUALP (CAR S) E) T)
    (T (SET-CONTAINS? (CDR S) E)))
  ) ;;SET-CONTAINS?

(DEFUN SET-SUBSET? (S1 S2)
  (COND
    ((NULL S1)  T)
    ((SET-CONTAINS? S2 (CAR S1)) (SET-SUBSET? (CDR S1) S2)))
  ) ;;SET-SUBSET?

(DEFUN SET-EQUAL? (S1 S2)
  (AND (SET-SUBSET? S1 S2) (SET-SUBSET? S2 S1))
  ) ;;SET-EQUAL?

(DEFUN SET-REMOVE (S E)
  (COND
    ((NULL S) S)
    ((EQUALP (CAR S) E) (CDR S))        ; e is unique in a set
    (T (CONS (CAR S) (SET-REMOVE (CDR S) E))))
  ) ;;SET-REMOVE


(DEFUN SET-UNION (S1 S2)
  (IF (NULL S2)
      S1
      (IF (SET-CONTAINS? S1 (CAR S2)) 
          (SET-UNION S1 (CDR S2))
          (SET-UNION (CONS (CAR S2) S1) (CDR S2))))
  ) ;;SET-UNION

;; ls = list of set '((a b c) (c d e) (f i)) --> '(a b c d e f i)
(DEFUN SET-UNION-L (LS)
  (COND 
    ((NULL LS) LS)
    ((NULL (CDR LS)) (CAR LS))
    (T (SET-UNION-L (CONS (SET-UNION (CAR LS) (CADR LS)) (CDDR LS)))))
  ) ;;SET-UNION-L

(DEFUN SET-DIFF (S1 S2)
  (IF (NULL S1)
      S1
      (IF (SET-CONTAINS? S2 (CAR S1))
          (SET-DIFF (CDR S1) S2)
          (CONS (CAR S1) (SET-DIFF (CDR S1) S2))))
  ) ;;SET-DIFF

(DEFUN SET-INTERSECTION (S1 S2)
  (IF (NULL S1)
      S1
      (IF (SET-CONTAINS? S2 (CAR S1))
          (CONS (CAR S1) (SET-INTERSECTION (CDR S1) S2))
          (SET-INTERSECTION (CDR S1) S2)))
  ) ;;SET-INTERSECTION

(DEFUN SET-MEMBER (S LS)
  (IF (NULL LS)
      NIL
      (IF (SET-EQUAL? S (CAR LS))
          T
          (SET-MEMBER S (CDR LS))))
  ) ;;SET-MEMBER

(DEFUN SET-UNIQUE-L (LS)
  (IF (NULL LS)
      NIL
      (IF (SET-MEMBER (CAR LS) (CDR LS))
          (SET-UNIQUE-L (CDR LS))
          (CONS (CAR LS) (SET-UNIQUE-L (CDR LS)))))
  ) ;;SET-UNIQUE-L
            
    
    
(DEFUN GRAF-EMPTY ()
  '(())
  ) ;;GRAF-EMPTY

(DEFUN GRAF-NODES (G)
  (CAR G)
  ) ;;GRAF-NODES

(DEFUN GRAF-EDGES (G)
  (CDR G)
  ) ;;GRAF-EDGES

(DEFUN GRAF-CONTAINS-NODE? (G N)
  (SET-CONTAINS? (GRAF-NODES G) N)
  ) ;;GRAF-CONTAINS-NODE?

(DEFUN GRAF-ADD-NODE (G N)
  (IF (SET-CONTAINS? (GRAF-NODES G) N)
      G
      (CONS (CONS N (GRAF-NODES G)) (GRAF-EDGES G)))
  ) ;;GRAF-ADD-NODE

(DEFUN GRAF-ADD-NODES (G LN) 
  (IF (NULL LN) G (GRAF-ADD-NODES (GRAF-ADD-NODE G (CAR LN)) (CDR LN)))
  ) ;;GRAF-ADD-NODES

(DEFUN EDGES-REMOVE-NODE (LE N)
  (COND
    ((NULL LE)      LE)
    ((OR (EQUALP (CAAR LE) N) (EQUALP (CADAR LE) N))
     (EDGES-REMOVE-NODE (CDR LE) N))
    (T              (CONS (CAR LE) (EDGES-REMOVE-NODE (CDR LE) N))))
  ) ;;EDGES-REMOVE-NODE

(DEFUN EDGES-REMOVE-NODES (LE LN)
  (IF (NULL LN)
      LE
      (EDGES-REMOVE-NODES (EDGES-REMOVE-NODE LE (CAR LN)) (CDR LN)))
  ) ;;EDGES-REMOVE-NODES


(DEFUN GRAF-REMOVE-NODE (G N)
  (CONS (SET-REMOVE (GRAF-NODES G) N) (EDGES-REMOVE-NODE (GRAF-EDGES G) N))
  ) ;;GRAF-REMOVE-NODE

(DEFUN GRAF-REMOVE-NODES (G LN)
  (CONS (SET-DIFF (GRAF-NODES G) LN)
        (EDGES-REMOVE-NODES (GRAF-EDGES G) LN))
  ) ;;GRAF-REMOVE-NODES


(DEFUN GRAF-CONTAINS-EDGE? (G E)
  (SET-CONTAINS? (GRAF-EDGES G) E)
  ) ;;GRAF-CONTAINS-EDGE?

(DEFUN GRAF-ADD-EDGE (G E)
  (IF (AND (CONSP E) (NULL (CDDR E))
           (GRAF-CONTAINS-NODE? G (CAR E)) (GRAF-CONTAINS-NODE? G (CADR E))
           (NOT (GRAF-CONTAINS-EDGE? G E)))
      (CONS (GRAF-NODES G) (CONS E (GRAF-EDGES G)))
      G)
  ) ;;GRAF-ADD-EDGE

(DEFUN GRAF-ADD-EDGES (G LE) 
  (IF (NULL LE) G (GRAF-ADD-EDGES (GRAF-ADD-EDGE G (CAR LE)) (CDR LE)))
  ) ;;GRAF-ADD-EDGES

(DEFUN GRAF-REMOVE-EDGES (G LE)
  (CONS (GRAF-NODES G) (SET-DIFF (GRAF-EDGES G) LE))
  ) ;;GRAF-REMOVE-EDGES

(DEFUN GRAF-REMOVE-EDGE (G E)
  (GRAF-REMOVE-EDGES G (LIST E))
  ) ;;GRAF-REMOVE-EDGE




(DEFUN GRAF-EQUAL? (G1 G2)
  (AND (SET-EQUAL? (GRAF-NODES G1) (GRAF-NODES G2))
       (SET-EQUAL? (GRAF-EDGES G1) (GRAF-EDGES G2)))
  ) ;;GRAF-EQUAL?

(DEFUN GRAF-PARTIAL? (G1 G2)
  (AND (SET-EQUAL? (GRAF-NODES G1) (GRAF-NODES G2))
       (SET-SUBSET? (GRAF-EDGES G1) (GRAF-EDGES G2)))
  ) ;;GRAF-PARTIAL?

(DEFUN GRAF-SUBSET? (G1 G2)
  (AND (SET-SUBSET? (GRAF-NODES G1) (GRAF-NODES G2))
       (SET-SUBSET? (GRAF-EDGES G1) (GRAF-EDGES G2)))
  ) ;;GRAF-SUBSET?

(DEFUN GRAF-UNION (G1 G2)
  (CONS (SET-UNION (GRAF-NODES G1) (GRAF-NODES G2))
        (SET-UNION (GRAF-EDGES G1) (GRAF-EDGES G2)))
  ) ;;GRAF-UNION

(DEFUN GRAF-INTERSECTION (G1 G2)
  (LET ((NODES (SET-INTERSECTION (GRAF-NODES G1) (GRAF-NODES G2))))
    (CONS NODES
          (EDGES-REMOVE-NODES 
           (SET-INTERSECTION (GRAF-EDGES G1) (GRAF-EDGES G2))
           (SET-DIFF 
            (SET-UNION (GRAF-NODES G1) (GRAF-NODES G2)) NODES))))
  ) ;;GRAF-INTERSECTION


(DEFUN EDGES-NEXT-NODES (LE N)
  (COND
    ((NULL LE)              LE)
    ((EQUALP (CAAR LE) N)   (CONS (CAR LE) (EDGES-NEXT-NODES (CDR LE) N)))
    (T                      (EDGES-NEXT-NODES (CDR LE) N)))
  ) ;;EDGES-NEXT-NODES

(DEFUN EDGES-PREV-NODES (LE N)
  (COND
    ((NULL LE)              LE)
    ((EQUALP (CADAR LE) N)  (CONS (CAR LE) (EDGES-PREV-NODES (CDR LE) N)))
    (T                      (EDGES-PREV-NODES (CDR LE) N)))
  ) ;;EDGES-PREV-NODES




(DEFUN GRAF-GAMMA (G N)
  (EDGES-NEXT-NODES (GRAF-EDGES G) N)
  ) ;;GRAF-GAMMA

(DEFUN GRAF-GAMMA-INV (G N)
  (EDGES-PREV-NODES (GRAF-EDGES G) N)
  ) ;;GRAF-GAMMA-INV


(DEFUN GRAF-FOLLOW (G N)
  (MAPCAR 'CADR (GRAF-GAMMA G N))
  ) ;;GRAF-FOLLOW

(DEFUN GRAF-PRECEDE (G N)
  (MAPCAR 'CAR (GRAF-GAMMA-INV G N))
  ) ;;GRAF-PRECEDE


(DEFUN GRAF-FOLLOW-L (G LN)
  (IF (NULL LN)
      NIL
      (SET-UNION (GRAF-FOLLOW G (CAR LN)) (GRAF-FOLLOW-L G (CDR LN))))
  ) ;;GRAF-FOLLOW-L
                

(DEFUN GRAF-PRECEDE-L (G LN)
  (IF (NULL LN)
      NIL
      (SET-UNION (GRAF-PRECEDE G (CAR LN)) (GRAF-PRECEDE-L G (CDR LN))))
  ) ;;GRAF-PRECEDE-L

(DEFUN GRAF-GAMMA-CLOSURE (G N)
  (LET* ( (PREV-CLOSURE '())
         (NEW-CLOSURE (LIST N))
          (NEXT (GRAF-FOLLOW-L G NEW-CLOSURE)))
    (LOOP WHILE (NOT (SET-EQUAL? PREV-CLOSURE NEW-CLOSURE)) DO 
         (SETQ PREV-CLOSURE NEW-CLOSURE)
         (SETQ NEW-CLOSURE (SET-UNION NEW-CLOSURE NEXT))
         (SETQ NEXT (GRAF-FOLLOW-L G NEW-CLOSURE))
         )
    NEW-CLOSURE)
  ) ;;GRAF-GAMMA-CLOSURE


(DEFUN GRAF-GAMMA-INV-CLOSURE (G N)
  (LET* ( (PREV-CLOSURE '())
         (NEW-CLOSURE (LIST N))
          (NEXT (GRAF-PRECEDE-L G NEW-CLOSURE)))
    (LOOP WHILE (NOT (SET-EQUAL? PREV-CLOSURE NEW-CLOSURE)) DO 
         (SETQ PREV-CLOSURE NEW-CLOSURE)
         (SETQ NEW-CLOSURE (SET-UNION NEW-CLOSURE NEXT))
         (SETQ NEXT (GRAF-PRECEDE-L G NEW-CLOSURE))
         )
    NEW-CLOSURE)
  ) ;;GRAF-GAMMA-INV-CLOSURE


(DEFUN GRAF-CONNECTED-CLASSES (G)
  (SET-UNIQUE-L 
   (MAPCAR (LAMBDA (X) 
             (SET-INTERSECTION 
              (GRAF-GAMMA-INV-CLOSURE G X) 
              (GRAF-GAMMA-CLOSURE G X))) 
           (GRAF-NODES G)))
  ) ;;GRAF-CONNECTED-CLASSES
            
            
(DEFUN GRAF-GAMMAS (G)
  (MAPCAR 'GRAF-GAMMA 
          (MAKE-LIST (LENGTH (GRAF-NODES G)) :INITIAL-ELEMENT G) 
          (GRAF-NODES G))
  ) ;;GRAF-GAMMAS

(DEFUN GRAF-GAMMAS-INV (G)
  (MAPCAR 'GRAF-GAMMA-INV
          (MAKE-LIST (LENGTH (GRAF-NODES G)) :INITIAL-ELEMENT G) 
          (GRAF-NODES G))
  ) ;;GRAF-GAMMAS-INV

(DEFUN GRAF-DISPLAY-GAMMAS (G)
  (MAPCAR (LAMBDA (N) 
            (FORMAT T "~a -> ~a ~%" N (MAPCAR 'CADR (GRAF-GAMMA G N))))
          (GRAF-NODES G))
  ) ;;GRAF-DISPLAY-GAMMAS

(DEFUN GRAF-DISPLAY-GAMMAS-INV (G)
  (MAPCAR (LAMBDA (N) 
            (FORMAT T "~a <- ~a ~%" N (MAPCAR 'CAR (GRAF-GAMMA-INV G N)))) 
          (GRAF-NODES G))
  ) ;;GRAF-DISPLAY-GAMMAS-INV


(DEFUN EDGES-REVERSE (LE)
  (IF (NULL LE)
      LE
      (CONS (LIST (CADAR LE) (CAAR LE)) (EDGES-REVERSE (CDR LE))))
  ) ;;EDGES-REVERSE


(DEFUN GRAF-REVERSE-EDGES (G)
  (CONS (GRAF-NODES G) (EDGES-REVERSE (GRAF-EDGES G)))
  ) ;;GRAF-REVERSE-EDGES
        
(DEFUN GRAF-SYMETRIC (G)
  (GRAF-REVERSE-EDGES G)
  ) ;;GRAF-SYMETRIC

(DEFUN GRAF-SYMETRIC? (G)
  (SET-EQUAL? (GRAF-EDGES G) (EDGES-REVERSE (GRAF-EDGES G)))
  ) ;;GRAF-SYMETRIC?

(DEFUN GRAF-MAKE-SYMETRIC (G)
  (GRAF-UNION G (GRAF-REVERSE-EDGES G))
  ) ;;GRAF-MAKE-SYMETRIC
    
(DEFUN LPRINT (L)
  (PROGN (PRINT (LENGTH L))
         (COND 
           ((NULL L)           (PRINT "---"))
           ((NOT (CONSP L))    (PRINT L))
           (T                  (PROGN (PRINT (CAR L)) (LPRINT (CDR L))))))
  ) ;;LPRINT


(DEFUN GRAF-MINIMIZE-CROSSES-COUNT-CROSSES (G NV) ; private
  (DECLARE (TYPE (SIMPLE-ARRAY CONS *) NV))
  (LET (  
        (NC (ARRAY-DIMENSION NV 0))
        (CC (MAKE-ARRAY (ARRAY-DIMENSION NV 0) :INITIAL-ELEMENT 0))
        )
    (DO ((I 0      (1+ I))) ((>= I NC))
      (DO ((J (1+ I) (1+ J))) ((>= J NC))
        (DO ((K (1+ J) (1+ K))) ((>= K NC))
          (DO ((L (1+ K) (1+ L))) ((>= L NC))
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
            (WHEN (AND (GRAF-CONTAINS-EDGE?
                        G (LIST (AREF NV I) (AREF NV K)))
                       (GRAF-CONTAINS-EDGE?
                        G (LIST (AREF NV J) (AREF NV L))))
              (SETF (AREF CC I) (1+ (AREF CC I))
                    (AREF CC J) (1+ (AREF CC J))
                    (AREF CC K) (1+ (AREF CC K))
                    (AREF CC L) (1+ (AREF CC L)))
              )
            ))))
    CC) ;;LET
  )     ;;GRAF-MINIMIZE-CROSSES-COUNT-CROSSES


(DEFVAR GRAF-MINIMIZE-CROSSES-VERBOSELY  NIL)

(DEFUN GRAF-MINIMIZE-CROSSES-SEARCH-SWAP (G NV NC CC SCC) ; private
  (DECLARE (TYPE (SIMPLE-ARRAY CONS *) NV)
           (TYPE (OR NULL (SIMPLE-ARRAY INTEGER *)) CC)
           (TYPE INTEGER NC SCC))
  (LET (
        (BI   NC)                       ; best swap index.
        (BJ   NC)                       ; best swap index.
        (BSV  (COPY-SEQ NV))            ; best swapped array.
        (BSC  (AND CC (COPY-SEQ CC))) ; best swapped count of crosses array.
        (BSSC SCC)       ; best sum of swapped count of crosses array.
        (SV   NIL)                      ; swapped array.
        (SC   NIL)                  ; swapped counts of crosses array.
        (SSC  0)              ; sum of swapped count of crosses array.
        )
    (DECLARE (TYPE (OR NULL (SIMPLE-ARRAY INTEGER *)) SC BSC)
             (TYPE (OR NULL (SIMPLE-ARRAY CONS    *)) SV BSV))
    (DO ((I 0 (1+ I))) ((<= NC I))
      (DO ((J (1+ I) (1+ J))) ((<= NC J))
        (SETQ SV (COPY-SEQ NV))
        (PSETF (AREF SV I) (AREF SV J)
               (AREF SV J) (AREF SV I))
        (SETQ SC (GRAF-MINIMIZE-CROSSES-COUNT-CROSSES G SV))
        (SETQ SSC (REDUCE (FUNCTION +) SC))
        (IF (< SSC BSSC)
            (PROGN
              (IF GRAF-MINIMIZE-CROSSES-VERBOSELY
                  (FORMAT T "found less: ~s~%" (/ SSC 4)))
              (SETQ BI I)
              (SETQ BJ I)
              (SETQ BSV (COPY-SEQ SV))
              (SETQ BSC (COPY-SEQ SC))
              (SETQ BSSC SSC)
              ))))
    (IF (= BI NC)
        NIL
        (LIST BSV BSC BSSC)
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

(DEFUN GRAF-MINIMIZE-CROSSES (G)
  (IF (NOT (GRAF-SYMETRIC? G))
      (GRAF-MINIMIZE-CROSSES (GRAF-MAKE-SYMETRIC G))
      (LET* ((NV (MAKE-ARRAY (LENGTH (GRAF-NODES G))
                             :INITIAL-CONTENTS (GRAF-NODES G)))
             (NC (LENGTH (GRAF-NODES G)))
             (CC  (GRAF-MINIMIZE-CROSSES-COUNT-CROSSES G NV)) ; count of crosses array.
             (SCC (REDUCE (FUNCTION +) CC)) ; sum of cc.
             (SS  (GRAF-MINIMIZE-CROSSES-SEARCH-SWAP G NV NC CC SCC)) ; search-swap result.
             )
        (DECLARE (TYPE (OR NULL (SIMPLE-ARRAY INTEGER *)) CC)
                 (TYPE (SIMPLE-ARRAY CONS *) NV)
                 (TYPE INTEGER NC SCC)
                 (TYPE LIST SS))
        (LOOP WHILE (NOT (NULL SS)) DO
             (SETQ NV (CAR SS))
             (SETQ CC (CADR SS))
             (SETQ SCC (CADDR SS))
             (SETQ SS (GRAF-MINIMIZE-CROSSES-SEARCH-SWAP G NV NC CC SCC))
             )
        (IF GRAF-MINIMIZE-CROSSES-VERBOSELY
            (FORMAT T "crosses:    ~s ~%" CC))
        NV
        ))
  ) ;;GRAF-MINIMIZE-CROSSES
        
        
        
(DEFUN GRAF-REPLACE-NODE (G ON NN)
  (GRAF-ADD-EDGES
   (GRAF-ADD-NODES (GRAF-EMPTY)
                   (MAPCAR (LAMBDA (N) (IF (EQ N ON) NN N)) (GRAF-NODES G)))
   (MAPCAR (LAMBDA (E) 
             (MAPCAR (LAMBDA (N) (IF (EQ N ON) NN N)) E)) 
           (GRAF-EDGES G)))
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






        

(DEFUN GRAF-INITIAL-NODES (G)
  (LET ((TN '()) (NN (GRAF-NODES G)))
    (LOOP WHILE (NOT (SET-EQUAL? TN NN)) DO 
         (SETQ TN NN)
         (SETQ NN (SET-UNION-L 
                   (MAPCAR (LAMBDA (X) 
                             (LET ((F (GRAF-PRECEDE G X)))
                               (IF (NULL F)
                                   (LIST X)
                                   F))) TN))))
    NN)
  ) ;;GRAF-INITIAL-NODES

(DEFUN GRAF-TERMINAL-NODES (G)
  (LET ((TN '()) (NN (GRAF-NODES G)))
    (LOOP WHILE (NOT (SET-EQUAL? TN NN)) DO 
         (SETQ TN NN)
         (SETQ NN (SET-UNION-L 
                   (MAPCAR (LAMBDA (X) 
                             (LET ((F (GRAF-FOLLOW G X)))
                               (IF (NULL F)
                                   (LIST X)
                                   F))) TN))))
    NN)
  ) ;;GRAF-TERMINAL-NODES

(DEFUN GRAF-INITIAL-TOKENS (G)
  (DELETE NIL (MAPCAR 
               (LAMBDA (X) (IF (GRAF-PRECEDE G X) NIL X)) (GRAF-NODES G)))
  ) ;;GRAF-INITIAL-TOKENS
        
(DEFUN GRAF-TERMINAL-TOKENS (G)
  (DELETE NIL (MAPCAR 
               (LAMBDA (X) (IF (GRAF-FOLLOW G X) NIL X)) (GRAF-NODES G)))
  ) ;;GRAF-TERMINAL-TOKENS
        

(DEFUN AND-L (L)
  (COND
    ((NULL L)   T)
    ((CAR L)    (AND-L (CDR L)))
    (T          NIL))
  ) ;;AND-L
        
(DEFUN GRAF-REFLEXIVE? (G)
  (AND-L
   (MAPCAR (LAMBDA (N) (GRAF-CONTAINS-EDGE? G (LIST N N))) (GRAF-NODES G)))
  ) ;;GRAF-REFLEXIVE?
    
(DEFUN GRAF-MAKE-REFLEXIVE (G)
  (GRAF-ADD-EDGES G (MAPCAR (LAMBDA (X) (LIST X X)) (GRAF-NODES G)))
  ) ;;GRAF-MAKE-REFLEXIVE


(DEFUN GRAF-MAKE-TREE (G ROOT)
  (CONS ROOT 
        (MAPCAR 
         (LAMBDA (X) (GRAF-MAKE-TREE (GRAF-REMOVE-NODE G ROOT) X)) 
         (GRAF-FOLLOW G ROOT)))
  ) ;;GRAF-MAKE-TREE



;; let s[0] be the set of terminal tokens of g
;; let s[i+1] be the set of precedent nodes of s[i] less the nodes already in 
;;   union(s[j],j in [0..i]).
;; k=MU i, s[k] equals the set of initial tokens of g
;; result=(s[k],...,s[0])

(DEFUN GRAF-BUILD-ORDERED-NODE-LIST-STEP (G L)
  (IF (NULL (CAR L))  
      (CDR L)
      (GRAF-BUILD-ORDERED-NODE-LIST-STEP
       G (CONS (SET-DIFF (GRAF-PRECEDE-L G (CAR L)) (SET-UNION-L L)) L)))
  ) ;;GRAF-BUILD-ORDERED-NODE-LIST-STEP


;;; (DEFUN GRAF-BUILD-ORDERED-NODE-LIST (G)
;;;   (GRAF-BUILD-ORDERED-NODE-LIST-STEP G (LIST (GRAF-TERMINAL-TOKENS G))))

(DEFUN GRAF-BUILD-ORDERED-NODE-LIST (G)
  (REVERSE (GRAF-BUILD-ORDERED-NODE-LIST-STEP
            G (LIST (GRAF-INITIAL-TOKENS G))))
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
    
    
    
    
(DEFUN GRAF-ONL-ADD-INDICES-IJ (LL I J N)
  (COND
    ((NULL LL)
     NIL)
    ((NULL (CAR LL))
     (IF (NULL (CDR LL))
         (LIST NIL)
         (CONS NIL (GRAF-ONL-ADD-INDICES-IJ (CDR LL) (1+ I) 0 N))))
    (T 
     (LET ((R (GRAF-ONL-ADD-INDICES-IJ 
               (CONS (CDAR LL) (CDR LL)) I (1+ J) (1+ N))))
       (CONS (CONS (LIST (CAAR LL) I J N) (CAR R)) (CDR R)))))
  ) ;;GRAF-ONL-ADD-INDICES-IJ


(DEFUN FIND-ONLIFBE (LI E)
  (COND
    ((NULL LI) NIL)
    ((EQUAL (CAAR LI) E) (CAR LI))
    (T (FIND-ONLIFBE (CDR LI) E)))
  ) ;;FIND-ONLIFBE


(DEFUN GRAF-ONLIFBE-REPLACE (LI LE N)
  (IF (NULL LE)
      NIL
      (CONS (LIST (FIND-ONLIFBE LI (CAAR LE)) (FIND-ONLIFBE LI (CADAR LE)) N)
            (GRAF-ONLIFBE-REPLACE LI (CDR LE) (1+ N))))
  ) ;;GRAF-ONLIFBE-REPLACE


(DEFUN GRAF-ONLIF-BUILD-EDGES-CURRENT (G LI CLI N)
  (IF (NULL CLI)
      NIL
      (LET* ((NI (CAR CLI)) (LE (GRAF-GAMMA G (CAR NI)))
             (LR (IF (NULL LE) NIL (GRAF-ONLIFBE-REPLACE LI LE N)))
             (M  (IF (NULL LE) N   (1+ (APPLY 'MAX (MAPCAR #'THIRD LR))))))
        (APPEND LR
                (GRAF-ONLIF-BUILD-EDGES-CURRENT G LI (CDR CLI) M))))
  ) ;;GRAF-ONLIF-BUILD-EDGES-CURRENT


(DEFUN GRAF-ONLIF-BUILD-EDGES (G LI N)
  (GRAF-ONLIF-BUILD-EDGES-CURRENT G LI LI N)
  ) ;;GRAF-ONLIF-BUILD-EDGES


(DEFUN GRAF-DIAGRAM-GENERATE-NODE (OUT N L X Y SX SY NODE)
  (FORMAT OUT "symbol ~s~%" N)
  (FORMAT OUT "   layer ~s~%" L)
  (FORMAT OUT "   shape \"Rectangle\"~%")
  (FORMAT OUT "   location ~d.00 ~d.00~%" X Y)
  (FORMAT OUT "   size ~d.00 ~d.00~%" SX SY)
  (FORMAT OUT "   framed~%")
  (FORMAT OUT "   fillColor colorIndex 0~%")
  (FORMAT OUT "   frameColor colorIndex 1~%")
  (FORMAT OUT "   shadowColor colorIndex 2~%")
  (FORMAT OUT "   lineWidth 1.00~%")
  (FORMAT OUT "   filled~%")
  (FORMAT OUT "   rtfText {\\rtf0\\ansi{\\fonttbl\\f0\\fswiss Helvetica;}\\margl40\\margr40\\pard\\tx960\\tx1920\\tx2880\\tx3840\\tx4800\\tx5760\\tx6720\\tx7680\\tx8640\\tx9600\\f0\\b\\i0\\ulnone\\qc\\fs20\\fc0\\cf0 ~a}~%" NODE)
  (FORMAT OUT "   textPlacement middle~%")
  (FORMAT OUT "end~%~%")
  ) ;;GRAF-DIAGRAM-GENERATE-NODE

(DEFUN GRAF-DIAGRAM-GENERATE-VERTEX (OUT N L X Y)
  (FORMAT OUT "vertex ~s~%" N)
  (FORMAT OUT "   layer ~s~%" L)
  (FORMAT OUT "   location ~d.00 ~d.00~%" X Y)
  (FORMAT OUT "end~%~%")
  ) ;;GRAF-DIAGRAM-GENERATE-VERTEX
    
(DEFUN GRAF-DIAGRAM-GENERATE-LINE (OUT N L FROM TO VERTICES)
  (FORMAT OUT "line ~s~%" N)
  (FORMAT OUT "   from ~s~%" FROM)
  (FORMAT OUT "   to ~s~%" TO)
  (DO ((V VERTICES (CDR V)))
      ((NULL V) NIL)
    (FORMAT OUT "   ~s~%" (CAAR V)))
  (FORMAT OUT "   layer ~s~%" L)
  (FORMAT OUT "   headType arrow~%")
  (FORMAT OUT "   lineWidth ~s~%" 1.00)
  (FORMAT OUT "   filled~%")
  (FORMAT OUT "   frameColor colorIndex ~s~%" 1)
  (FORMAT OUT "   fillColor colorIndex ~s~%" 0)
  (FORMAT OUT "end~%~%")
  ) ;;GRAF-DIAGRAM-GENERATE-LINE


(DEFUN NODE-SIZE-X (N)
  (DECLARE (IGNORE N))
  100) ;;NODE-SIZE-X
(DEFUN NODE-SIZE-Y (N)
  (DECLARE (IGNORE N))
  20) ;;NODE-SIZE-Y
(DEFUN NODE-POS-X (N)
  (+ 50 (* 180 (SECOND N)))
  ) ;;NODE-POS-X
(DEFUN NODE-POS-Y (N)
  (+ 50 (*  50 (THIRD  N)))
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
    
(DEFUN GRAF-DIAGRAM-GENERATE-EDGES (OUT LEIJ N)
  (IF (NULL LEIJ)
      NIL
      (LET ((NN N) (ALLV NIL) (ALLI NIL))
        (DO ((CUR-EIJ LEIJ (CDR CUR-EIJ)) )
            ((NULL CUR-EIJ) NIL)
          (LET* ((EIJ      (CAR CUR-EIJ))  
                 (FROM    (FIRST EIJ))
                 (TO      (SECOND EIJ))
                 (LINENUM (THIRD EIJ))
                 (LV      NIL))
            (SETQ LV (APPEND LV
                             (LIST
                              (CONS NN
                                    (LIST 
                                     (+ (NODE-POS-X FROM)
                                        (NODE-SIZE-X FROM))
                                     (+ (NODE-POS-Y FROM)
                                        (/ (NODE-SIZE-Y FROM) 2))))
                              (CONS (1+ NN)
                                    (LIST
                                     (+ (NODE-POS-X FROM)
                                        (NODE-SIZE-X FROM) 16)
                                     (+ (NODE-POS-Y FROM)
                                        (/ (NODE-SIZE-Y FROM) 2)))))))
            (SETQ NN (+ NN 2))
            (WHEN (NOT (EQUAL (1+ (SECOND FROM)) (SECOND TO)))
              (SETQ LV
                    (APPEND LV
                            (LIST
                             (CONS NN (LIST 
                                       (+ (NODE-POS-X FROM)
                                          (NODE-SIZE-X FROM) 24)
                                       (+ (NODE-POS-Y TO)
                                          (NODE-SIZE-Y TO) 16
                                          (* 2 (- (SECOND FROM)
                                                  (SECOND TO))))))
                             (CONS (1+ NN)
                                   (LIST
                                    (- (NODE-POS-X TO) 24)
                                    (+ (NODE-POS-Y TO)
                                       (NODE-SIZE-Y TO) 16
                                       (* 2 (- (SECOND FROM)
                                               (SECOND TO))))))
                             ))
                    NN (+ NN 2)))
            (SETQ LV (APPEND LV
                             (LIST
                              (CONS NN
                                    (LIST 
                                     (- (NODE-POS-X TO) 16)
                                     (+ (NODE-POS-Y TO)
                                        (/ (NODE-SIZE-Y TO) 2))))
                              (CONS (1+ NN)
                                    (LIST
                                     (NODE-POS-X TO)
                                     (+ (NODE-POS-Y TO)
                                        (/ (NODE-SIZE-Y TO) 2)))))))
            (SETQ NN (+ NN 2))
            (SETQ ALLV (APPEND LV ALLV))
            (SETQ ALLI (CONS (LIST OUT LINENUM LINENUM 
                                   (FOURTH FROM) (FOURTH TO) LV) ALLI))
            ) ;;LET*
          )   ;;DO
        (MAPCAR (LAMBDA (X) (GRAF-DIAGRAM-GENERATE-VERTEX
                             OUT (FIRST X) (FIRST X) (SECOND X) (THIRD X)))
                ALLV)
        (MAPCAR (LAMBDA (X) (APPLY 'GRAF-DIAGRAM-GENERATE-LINE X))
                ALLI)                   
        ))
  ) ;;GRAF-DIAGRAM-GENERATE-EDGES

                    
(DEFUN GRAF-DIAGRAM (G OUT NODE-NAME)
  (DECLARE (TYPE FUNCTION NODE-NAME))
  (FORMAT OUT "#!DG_TEXT-Version-2~%")
  (FORMAT OUT "# D2 Version: Built by rob on Tue Sep 27 14:01:06 PDT 1994~%")
  (FORMAT OUT "~%")
  (FORMAT OUT "windowOrigin 222.00 2.00~%")
  (FORMAT OUT "viewOrigin 1.00 9.00~%")
  (FORMAT OUT "viewSize 825.00 617.00~%")
  (FORMAT OUT "visibleOrigin 0.00 0.00~%")
  (FORMAT OUT "showTools~%")
  (FORMAT OUT "snapToGrid~%")
  (FORMAT OUT "gridSize 4.000~%")
  (FORMAT OUT "defaultFont \"helvetica\"~%")
  (FORMAT OUT "defaultFontSize 10.00~%")
  (FORMAT OUT "printInfoFile \"printinfo\"~%")
  (FORMAT OUT "colorFile \"colors.clr\"~%")
  (FORMAT OUT "~%")
  (LET* ((LNIJ (FLATTEN (GRAF-ONL-ADD-INDICES-IJ 
                         (GRAF-BUILD-ORDERED-NODE-LIST G) 0 0 1000)))
         (LEIJ (GRAF-ONLIF-BUILD-EDGES
                G LNIJ (1+ (APPLY 'MAX (MAPCAR #'FOURTH LNIJ))))))
    (MAPCAR (LAMBDA (X) (GRAF-DIAGRAM-GENERATE-NODE
                         OUT (FOURTH X) (FOURTH X) 
                         (NODE-POS-X X) (NODE-POS-Y X)
                         (NODE-SIZE-X X) (NODE-SIZE-Y X)
                         (APPLY  NODE-NAME (LIST (CAR X)))))
            LNIJ)
    (GRAF-DIAGRAM-GENERATE-EDGES
     OUT LEIJ (1+ (APPLY 'MAX (MAPCAR #'THIRD LEIJ))))
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


(DEFUN GRAF-SUMMARY ()
  (FORMAT T "~%")
  ;; use the following two lines to update the class summary, but skip the first
  ;; semicolon.
  ;; egrep 'DEFCLASS|DEFMETHOD' pjb-graph.lisp |sed -e 's/(DEFCLASS \(.*\)/    (FORMAT T "Class \1~%")/' -e 's/(DEFMETHOD\(.*\)/    (FORMAT T "\1~%")/'
  (FORMAT T "Class GRAF ()~%")
  (FORMAT T " EMPTY ((SELF GRAF))~%")
  (FORMAT T " NODES ((SELF GRAF))~%")
  (FORMAT T " ADD-NODE ((SELF GRAF) N)~%")
  (FORMAT T " ADD-NODES ((SELF GRAF) LN)~%")
  (FORMAT T " CONTAINS-NODE? ((SELF GRAF) N)~%")
  (FORMAT T " REMOVE-NODE ((SELF GRAF) N)~%")
  (FORMAT T " REMOVE-NODES ((SELF GRAF) N)~%")
  (FORMAT T " EDGES ((SELF GRAF))~%")
  (FORMAT T " ADD-EDGE ((SELF GRAF) E)~%")
  (FORMAT T " ADD-EDGES ((SELF GRAF) LE)~%")
  (FORMAT T " CONTAINS-EDGE? ((SELF GRAF) E)~%")
  (FORMAT T " REMOVE-EDGE ((SELF GRAF) E)~%")
  (FORMAT T " REMOVE-EDGES ((SELF GRAF) LE)~%")
  (FORMAT T " REVERSE-EDGES ((SELF GRAF))~%")
  (FORMAT T " IS-SYMETRIC? ((SELF GRAF))~%")
  (FORMAT T " SYMETRIC ((SELF GRAF))~%")
  (FORMAT T " MAKE-SYMETRIC ((SELF GRAF))~%")
  (FORMAT T " IS-REFLEXIVE? ((SELF GRAF))~%")
  (FORMAT T " MAKE-REFLEXIVE ((SELF GRAF))~%")
  (FORMAT T " EQUAL? ((SELF GRAF) OTHER-GRAF)~%")
  (FORMAT T " IS-PARTIAL-OF? ((SELF GRAF) OTHER-GRAF)~%")
  (FORMAT T " IS-SUBSET-OF? ((SELF GRAF) OTHER-GRAF)~%")
  (FORMAT T " MINIMIZE-CROSSES ((SELF GRAF))~%")
  (FORMAT T " GAMMA-OF-NODE ((SELF GRAF) N)~%")
  (FORMAT T " GAMMA-INV-OF-NODE ((SELF GRAF) N)~%")
  (FORMAT T " GAMMAS ((SELF GRAF))~%")
  (FORMAT T " GAMMAS-INV ((SELF GRAF))~%")
  (FORMAT T " DISPLAY-GAMMAS ((SELF GRAF))~%")
  (FORMAT T " DISPLAY-GAMMAS-INV ((SELF GRAF))~%")
  (FORMAT T " PRECEDE ((SELF GRAF) N)~%")
  (FORMAT T " FOLLOW ((SELF GRAF) N)~%")
  (FORMAT T " INITIAL-NODES ((SELF GRAF))~%")
  (FORMAT T " TERMINAL-NODES ((SELF GRAF))~%")
  (FORMAT T " INITIAL-TOKENS ((SELF GRAF))~%")
  (FORMAT T " TERMINAL-TOKENS ((SELF GRAF))~%")
  (FORMAT T " MAKE-INTERSECTION ((SELF GRAF) OTHER-GRAF)~%")
  (FORMAT T " MAKE-UNION ((SELF GRAF) OTHER-GRAF)~%")
  (FORMAT T " MAKE-TREE ((SELF GRAF) ROOT)~%")
  ) ;;GRAF-SUMMARY


(DEFGENERIC EMPTY (SELF))
(DEFGENERIC NODES (SELF))
(DEFGENERIC ADD-NODE (SELF N))
(DEFGENERIC ADD-NODES (SELF LN))
(DEFGENERIC CONTAINS-NODE\? (SELF N))
(DEFGENERIC REMOVE-NODE (SELF N))
(DEFGENERIC REMOVE-NODES (SELF LN))
(DEFGENERIC EDGES (SELF))
(DEFGENERIC ADD-EDGE (SELF E))
(DEFGENERIC ADD-EDGES (SELF LE))
(DEFGENERIC CONTAINS-EDGE\? (SELF E))
(DEFGENERIC REMOVE-EDGE (SELF E))
(DEFGENERIC REMOVE-EDGES (SELF LE))
(DEFGENERIC REVERSE-EDGES (SELF))
(DEFGENERIC IS-SYMETRIC\? (SELF))
(DEFGENERIC SYMETRIC (SELF))
(DEFGENERIC MAKE-SYMETRIC (SELF))
(DEFGENERIC IS-REFLEXIVE\? (SELF))
(DEFGENERIC MAKE-REFLEXIVE (SELF))
(DEFGENERIC EQUAL\? (SELF OTHER-GRAF))
(DEFGENERIC IS-PARTIAL-OF\? (SELF OTHER-GRAF))
(DEFGENERIC IS-SUBSET-OF\? (SELF OTHER-GRAF))
(DEFGENERIC MINIMIZE-CROSSES (SELF))
(DEFGENERIC GAMMA-OF-NODE (SELF N))
(DEFGENERIC GAMMA-INV-OF-NODE (SELF N))
(DEFGENERIC GAMMAS (SELF))
(DEFGENERIC GAMMAS-INV (SELF))
(DEFGENERIC DISPLAY-GAMMAS (SELF))
(DEFGENERIC DISPLAY-GAMMAS-INV (SELF))
(DEFGENERIC PRECEDE (SELF N))
(DEFGENERIC FOLLOW (SELF N))
(DEFGENERIC INITIAL-NODES (SELF))
(DEFGENERIC TERMINAL-NODES (SELF))
(DEFGENERIC INITIAL-TOKENS (SELF))
(DEFGENERIC TERMINAL-TOKENS (SELF))
(DEFGENERIC MAKE-INTERSECTION (SELF OTHER-GRAF))
(DEFGENERIC MAKE-UNION (SELF OTHER-GRAF))
(DEFGENERIC MAKE-TREE (SELF ROOT))


(DEFCLASS GRAF ()
  ((GRAF 
    :ACCESSOR GRAF
    :INITFORM (GRAF-EMPTY))))


(DEFMETHOD EMPTY ((SELF GRAF))
  (SETF (GRAF SELF) (GRAF-EMPTY))
  ) ;;EMPTY
    


(DEFMETHOD NODES ((SELF GRAF))
  (GRAF-NODES (GRAF SELF))
  ) ;;NODES

(DEFMETHOD ADD-NODE ((SELF GRAF) N)
  (SETF (GRAF SELF) (GRAF-ADD-NODE (GRAF SELF) N))
  ) ;;ADD-NODE

(DEFMETHOD ADD-NODES ((SELF GRAF) LN)
  (SETF (GRAF SELF) (GRAF-ADD-NODES (GRAF SELF) LN))
  ) ;;ADD-NODES
    
(DEFMETHOD CONTAINS-NODE? ((SELF GRAF) N)
  (GRAF-CONTAINS-NODE?  (GRAF SELF) N)
  ) ;;CONTAINS-NODE?

(DEFMETHOD REMOVE-NODE ((SELF GRAF) N)
  (SETF (GRAF SELF) (GRAF-REMOVE-NODE (GRAF SELF) N))
  ) ;;REMOVE-NODE
    
(DEFMETHOD REMOVE-NODES ((SELF GRAF) LN)
  (SETF (GRAF SELF) (GRAF-REMOVE-NODES (GRAF SELF) LN))
  ) ;;REMOVE-NODES
    

(DEFMETHOD EDGES ((SELF GRAF))
  (GRAF-EDGES (GRAF SELF))
  ) ;;EDGES

(DEFMETHOD ADD-EDGE ((SELF GRAF) E)
  (SETF (GRAF SELF) (GRAF-ADD-EDGE (GRAF SELF) E))
  ) ;;ADD-EDGE

(DEFMETHOD ADD-EDGES ((SELF GRAF) LE)
  (SETF (GRAF SELF) (GRAF-ADD-EDGES (GRAF SELF) LE))
  ) ;;ADD-EDGES
    
(DEFMETHOD CONTAINS-EDGE? ((SELF GRAF) E)
  (GRAF-CONTAINS-EDGE?  (GRAF SELF) E)
  ) ;;CONTAINS-EDGE?

(DEFMETHOD REMOVE-EDGE ((SELF GRAF) E)
  (SETF (GRAF SELF) (GRAF-REMOVE-EDGE (GRAF SELF) E))
  ) ;;REMOVE-EDGE
    
(DEFMETHOD REMOVE-EDGES ((SELF GRAF) LE)
  (SETF (GRAF SELF) (GRAF-REMOVE-EDGES (GRAF SELF) LE))
  ) ;;REMOVE-EDGES
    
(DEFMETHOD REVERSE-EDGES ((SELF GRAF))
  (SETF (GRAF SELF) (GRAF-REVERSE-EDGES (GRAF SELF)))
  ) ;;REVERSE-EDGES


(DEFMETHOD IS-SYMETRIC? ((SELF GRAF))
  (GRAF-SYMETRIC? (GRAF SELF) )
  ) ;;IS-SYMETRIC?

(DEFMETHOD SYMETRIC ((SELF GRAF))
  (LET ((G (MAKE-INSTANCE 'GRAF)))
    (SETF (GRAF G) (GRAF-SYMETRIC (GRAF SELF)))
    G)
  ) ;;SYMETRIC

(DEFMETHOD MAKE-SYMETRIC ((SELF GRAF))
  (SETF (GRAF SELF) (GRAF-MAKE-SYMETRIC (GRAF SELF)))
  ) ;;MAKE-SYMETRIC


(DEFMETHOD IS-REFLEXIVE? ((SELF GRAF))
  (GRAF-REFLEXIVE? (GRAF SELF))
  ) ;;IS-REFLEXIVE?
    
(DEFMETHOD MAKE-REFLEXIVE ((SELF GRAF))
  (GRAF-MAKE-REFLEXIVE (GRAF SELF))
  ) ;;MAKE-REFLEXIVE


(DEFMETHOD EQUAL? ((SELF GRAF) OTHER-GRAF)
  (GRAF-EQUAL? (GRAF SELF) (GRAF  OTHER-GRAF))
  ) ;;EQUAL?

(DEFMETHOD IS-PARTIAL-OF? ((SELF GRAF) OTHER-GRAF)
  (GRAF-PARTIAL? (GRAF SELF) (GRAF  OTHER-GRAF))
  ) ;;IS-PARTIAL-OF?

(DEFMETHOD IS-SUBSET-OF? ((SELF GRAF) OTHER-GRAF)
  (GRAF-SUBSET? (GRAF SELF) (GRAF  OTHER-GRAF))
  ) ;;IS-SUBSET-OF?



(DEFMETHOD MINIMIZE-CROSSES ((SELF GRAF))
  (SETF (GRAF SELF) (GRAF-MINIMIZE-CROSSES (GRAF SELF)))
  ) ;;MINIMIZE-CROSSES
    


(DEFMETHOD GAMMA-OF-NODE ((SELF GRAF) N)
  (GRAF-GAMMA (GRAF SELF) N)
  ) ;;GAMMA-OF-NODE

(DEFMETHOD GAMMA-INV-OF-NODE ((SELF GRAF) N)
  (GRAF-GAMMA-INV (GRAF SELF) N)
  ) ;;GAMMA-INV-OF-NODE

(DEFMETHOD GAMMAS ((SELF GRAF))
  (GRAF-GAMMAS (GRAF SELF))
  ) ;;GAMMAS

(DEFMETHOD GAMMAS-INV ((SELF GRAF))
  (GRAF-GAMMAS-INV (GRAF SELF))
  ) ;;GAMMAS-INV

(DEFMETHOD DISPLAY-GAMMAS ((SELF GRAF))
  (GRAF-DISPLAY-GAMMAS (GRAF SELF))
  ) ;;DISPLAY-GAMMAS
        
(DEFMETHOD DISPLAY-GAMMAS-INV ((SELF GRAF))
  (GRAF-DISPLAY-GAMMAS-INV (GRAF SELF))
  ) ;;DISPLAY-GAMMAS-INV
    

(DEFMETHOD PRECEDE ((SELF GRAF) N)
  (GRAF-PRECEDE (GRAF SELF) N)
  ) ;;PRECEDE
    
(DEFMETHOD FOLLOW ((SELF GRAF) N)
  (GRAF-FOLLOW (GRAF SELF) N)
  ) ;;FOLLOW
    
(DEFMETHOD INITIAL-NODES ((SELF GRAF))
  (GRAF-INITIAL-NODES (GRAF SELF))
  ) ;;INITIAL-NODES
    
(DEFMETHOD TERMINAL-NODES ((SELF GRAF))
  (GRAF-TERMINAL-NODES (GRAF SELF))
  ) ;;TERMINAL-NODES

(DEFMETHOD INITIAL-TOKENS ((SELF GRAF))
  (GRAF-INITIAL-TOKENS (GRAF SELF))
  ) ;;INITIAL-TOKENS

(DEFMETHOD TERMINAL-TOKENS ((SELF GRAF))
  (GRAF-TERMINAL-TOKENS (GRAF SELF))
  ) ;;TERMINAL-TOKENS
    
    
    
(DEFMETHOD MAKE-INTERSECTION ((SELF GRAF) OTHER-GRAF)
  (LET ((G (MAKE-INSTANCE 'GRAF)))
    (SETF (GRAF G) (GRAF-INTERSECTION (GRAF SELF) (GRAF OTHER-GRAF)))
    G)
  ) ;;MAKE-INTERSECTION

(DEFMETHOD MAKE-UNION ((SELF GRAF) OTHER-GRAF)
  (LET ((G (MAKE-INSTANCE 'GRAF)))
    (SETF (GRAF G) (GRAF-UNION (GRAF SELF) (GRAF OTHER-GRAF)))
    G)
  ) ;;MAKE-UNION

(DEFMETHOD MAKE-TREE ((SELF GRAF) ROOT)
  (GRAF-MAKE-TREE (GRAF SELF) ROOT)
  ) ;;MAKE-TREE




;;;; graf.lisp                        --                     --          ;;;;
