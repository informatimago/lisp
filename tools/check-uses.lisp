;;;;
;;;; Analyses the output of the objecteering macro dumpUses.jmf
;;;; 
;;;; (C) 2008 Anevia
;;;; 
;;;; Authors: Pascal J. Bourguignon
;;;; 
;;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "CHECK-USES"
  (:use "CL"
        "COM.INFORMATIMAGO.COMMON-LISP.GRAPH"
        "COM.INFORMATIMAGO.COMMON-LISP.GRAPH-DOT"))
(in-package "CHECK-USES")

(defgeneric uses (object)
  (:method ((self null)) '()))

(defclass uml-object (element-class)
  ((path        :initarg :path        :accessor path        :initform nil)
   (classof     :initarg :classof     :accessor classof     :initform nil)
   (description :initarg :description :accessor description :initform nil)
   (owns        :initarg :owns        :accessor owns        :initform '())
   (uses        :initarg :uses        :accessor uses        :initform '())
   (usedby      :initarg :usedby      :accessor usedby      :initform '())
   (owns-reachable     :accessor owns-reachable     :initform '())
   (uses-reachable     :accessor uses-reachable     :initform '())
   (usedby-reachable   :accessor usedby-reachable   :initform '())))

(defmethod initialize-instance :after ((self uml-object) &rest args)
  (declare (ignore args))
  (setf (slot-value self 'ident) (path self))
  self)

(defmethod name ((self uml-object))
  (format nil "~A ~A" (classof self) (path self)))

(defmethod print-object ((self uml-object) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (princ (name self) stream))
  self)




(defun load-uses (path)
  (with-open-file (stream path :external-format charset:iso-8859-1)
    (loop
       :for form = (read stream nil stream)
       :until (eq form stream)
       :when (listp form) :collect (loop
                                      :for clause :in (rest form)
                                      :when (eq (first clause) :owns)   :collect (rest clause) :into owns
                                      :when (eq (first clause) :uses)   :collect (rest clause) :into uses
                                      :when (eq (first clause) :usedby) :collect (rest clause) :into usedby
                                      :finally (return (make-instance 'uml-object
                                                           :classof (getf (first form) :classof)
                                                           :path (getf (first form) :path)
                                                           :description (first form)
                                                           :uses uses
                                                           :usedby usedby
                                                           :owns owns))))))


;;;
;;; :OWNS should be a tree, so we will only run a topological sort on it.
;;;

(DEFUN TOPOLOGICAL-SORT (NODES LESSP)
  "
RETURN: A list of NODES sorted topologically according to 
        the partial order function LESSP.
        If there are cycles (discounting reflexivity), 
        then the list returned won't contain all the NODES.
"
  (LOOP
     :WITH SORTED = '()
     :WITH INCOMING = (MAP 'VECTOR (LAMBDA (TO)
                                     (LOOP
                                        :FOR FROM :IN NODES
                                        :WHEN (AND (NOT (EQ FROM TO))
                                                   (FUNCALL LESSP FROM TO))
                                        :SUM 1))
                           NODES)
     :WITH Q = (LOOP
                  :FOR NODE :IN NODES
                  :FOR INCO :ACROSS INCOMING
                  :WHEN (ZEROP INCO)
                  :COLLECT NODE) 
     :WHILE Q
     :DO (LET ((N (POP Q)))
           (PUSH N SORTED)
           (LOOP
              :FOR M :IN NODES
              :FOR I :FROM 0
              :DO (WHEN (AND (AND (NOT (EQ N M))
                                  (FUNCALL LESSP N M))
                             (ZEROP (DECF (AREF INCOMING I))))
                    (PUSH M Q))))
     :FINALLY (RETURN (NREVERSE SORTED))))



(defun reachable (root successors)
  "
RETURN: A list of objects reachable from O traversing SUCCESSORS.
"
  (loop
     :with reachable = '()
     :with old = (funcall successors root)
     :while old
     :do (let ((current (pop old)))
           (pushnew current reachable)
           (dolist (successor (funcall successors current))
             (unless (member successor reachable)
               (pushnew  successor old))))
     :finally (return reachable)))



(defpackage "UML-OBJECT-NAMES" (:nicknames "UON") (:use))
(declaim (inline unify))
(defun unify (x)  (intern x "UML-OBJECT-NAMES"))

(defvar *objects* '())
(defvar *objects-index* (make-hash-table))
(defvar *sorted-objects*  '())
(declaim (inline find-object owns-objects uses-objects usedby-objects))
(defun find-object (path) (gethash path *objects-index*))
(defun owns-objects (object) (mapcar (function find-object) (owns object)))
(defun uses-objects (object) (mapcar (function find-object) (uses object)))
(defun usedby-objects (object) (mapcar (function find-object) (usedby object)))

(declaim (inline owns* uses* usedby*))
(defun owns*   (p q) (member q (owns-reachable p)))
(defun uses*   (p q) (member q (uses-reachable p)))
(defun usedby* (p q) (member q (usedby-reachable p)))





(defun closest-to-root (nodes)
  "
RETURN: the node in NODES that is the closest to ROOT according to *SORTED-OBJECTS*
"
  (loop
     :with closest = (pop nodes)
     :with minimum = (position closest *sorted-objects*)
     :for node :in nodes
     :for distance = (position node *sorted-objects*)
     :initially (assert minimum)
     :when (and distance (< distance minimum))
     :do (setf closest node
               minimum distance)
     :finally (return (values closest minimum))))

(defun closer-to-root (a b)
  (let ((p (position a *sorted-objects*))
        (q (position b *sorted-objects*)))
    (and p q (< p q))))



(defun find-shortest-path (from to successors)
  "
RETURN: The shortest path of length>0 from FROM to TO if it exists, or NIL.
"
  ;; breadth first search
  (loop
     :with processed = '()
     :for paths = (list (list from)) :then new-paths
     :for new-paths = (remove-if (lambda (head) (member head processed))
                                 (mapcan (lambda (path)
                                           (mapcar (lambda (new-node) (cons new-node path))
                                                   (funcall successors (first path))))
                                         paths)
                                 :key (function first))
     :for shortest-path = (find to new-paths :key (function first))
     :do (setf paths     (nconc paths new-paths)
               processed (nconc (delete-duplicates (mapcar (function first) new-paths)) processed))
     :until (or shortest-path (endp new-paths))
     :finally (return (reverse shortest-path))))


(defun print-cycle (path)
  (format t "~%There is a cycle going ~%from ~A" (name (first path)))
  (dolist (node (rest path))
    (format t "~%  to ~A" (name node)))
  (format t " !!!~%"))


(defun find-uses-cycles (objects)
  (mapcar (lambda (cycle) (find-shortest-path cycle cycle (function uses-objects)))
          (remove-if-not (lambda (x) (member x (uses-reachable x))) objects)))


(defun find-package-class-uses (objects)
  (let ((deps '()))
    (dolist (o objects deps)
      (when (string-equal "package" (classof o))
        (dolist (u (uses-objects o))
          (when (string-equal "class" (classof u))
            (push (list o u) deps)))))))


(defun report-problems (objects)
  (let ((cycles (find-uses-cycles objects)))
    (when cycles
      (format *error-output*
              "~&There are ~A cycles in the USES relationship!~%"
              (length cycles))
      (dolist (path cycles)
        (print-cycle path))))
  (let ((bad-uses (find-package-class-uses objects)))
    (when bad-uses
      (loop
         :with ps = (make-hash-table)
         :for (p c) :in bad-uses
         :do (pushnew c (gethash p ps '()))
         :finally (loop
                     :for p :being :the :hash-keys :in ps :using (:hash-value cs)
                     :initially (format *error-output*
                                        "~&There are ~A packages using classes!~%"
                                        (hash-table-count ps))
                     :do (format *error-output* "~%~A uses ~%" (name p))
                     (dolist (c cs)
                       (format *error-output* "~&      ~A~%" (name c))))))))


(defun uses-graph (objects)
  (let ((graph (make-instance 'graph-class :edge-class 'directed-edge-class )))
    (add-nodes graph objects)
    (dolist (from objects)
      (dolist (to (uses-objects from))
        (when (member to objects)
          (add-edge-between-nodes graph from to))))
    graph))


(defun generate-uses-graph (objects path)
  (with-open-file (dot path
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :external-format charset:iso-8859-1)
    (let ((graph  (uses-graph objects)))
      (set-property graph :nodesep 3)
      (set-property graph :ranksep 7)
      (set-property graph :page "64,48")
      (set-property graph :ratio :fill)
      (princ (generate-dot graph) dot))))



(defun process-dump (path)
  (setf *objects*  (load-uses path))
  (format *trace-output* "~&Read ~D objects~%" (length  *objects*))
  (setf *objects-index*
        (let ((table (make-hash-table)))
          (dolist (object *objects*)
            (setf (path   object) (unify (path object))
                  (owns   object) (mapcar (lambda (x) (unify (first x))) (owns object))
                  (uses   object) (mapcar (lambda (x) (unify (first x))) (uses object))
                  (usedby object) (mapcar (lambda (x) (unify (first x))) (usedby object)))
            (setf (gethash (path object) table) object))
          table))

  (dolist (object *objects*)
    (setf (owns-reachable   object) (reachable object (function owns-objects))
          (uses-reachable   object) (reachable object (function uses-objects))
          (usedby-reachable object) (reachable object (function usedby-objects))))

  ;; (print (list (mapcar (lambda (x) (length (owns-reachable x))) *objects*)
  ;;              (mapcar (lambda (x) (length (uses-reachable x))) *objects*)
  ;;              (mapcar (lambda (x) (length (usedby-reachable x))) *objects*))
  ;;        *trace-output*)


  (setf *sorted-objects*  (topological-sort *objects* (function owns*)))
  (unless (= (length *sorted-objects*) (length *objects*))
    (format *error-output*
            "~&The OWNS relationship contains cycles! It should be a tree.~%"))
  (report-problems *objects*))


#- (and) (progn
  
  (let ((filter (lambda (x)
                  (and (string= (classof x) "Package")
                       (COM.INFORMATIMAGO.COMMON-LISP.STRING:PREFIXP
                        "manager:Streamers:" (string (path x))))))
        (name "uses-packages"))
    (generate-uses-graph (remove-if-not filter *objects*) (make-pathname :name name :type "dot"))
    (ext:shell
     (format nil "(twopi -Tps  -Gcharset=latin1 -o~A.ps ~:*~A.dot;gsview ~:*~A.ps )&" name)))
  
  (let ((filter (lambda (x)
                  (and (string= (classof x) "Class")
                       (COM.INFORMATIMAGO.COMMON-LISP.STRING:PREFIXP
                        "manager:Streamers:" (string (path x))))))
         (name "uses-classes"))
  
    (generate-uses-graph (remove-if-not filter *objects*) (make-pathname :name name :type "dot"))
    (ext:shell
     (format nil "(twopi -Tps  -Gcharset=latin1 -o~A.ps ~:*~A.dot;gsview ~:*~A.ps )&" name)))

  )


(print '(in-package "CHECK-USES"))
(print '(check-uses:process-dump "~/uses.dump"))

