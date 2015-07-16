(defpackage "COM.INFORMATIMAGO.LANGUAGE.C11.GRAPH"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.GRAPH"
        "COM.INFORMATIMAGO.COMMON-LISP.GRAPHVIZ.GRAPH-DOT"))
(in-package "COM.INFORMATIMAGO.LANGUAGE.C11.GRAPH")


(defparameter *graph*
  (let ((graph (make-instance 'graph-class :edge-class 'directed-edge-class))
        (elements (make-hash-table)))
    (destructuring-bind ((terminal (&rest terminals)) &rest non-terminals)
    
        (cdddr (find 'define-parser
                     (sexp-list-file-contents "parser.lisp")
                     :key (function first)))
  
      (declare (ignore terminal))

      (dolist (item (remove-duplicates
                     (append terminals
                             (loop
                               :for (non-terminal . productions)
                                 :in non-terminals
                               :collect non-terminal
                               :append (loop
                                         :for production
                                           :in productions
                                         :if (atom production)
                                           :collect production
                                         :else
                                           :append (loop :for item
                                                           :in production
                                                         :unless (and (listp item)
                                                                      (eql 'function (first item)))
                                                           :collect item))))))
        (let ((node  (make-instance 'element-class)))
          (set-property node :dot-label item)
          (add-node graph node)
          (setf (gethash item elements) node)))

      (loop
        :for (non-terminal . productions) :in non-terminals
        :for from := (gethash non-terminal elements)
        :do (let ((to-nodes '()))
              (loop
                :for production :in productions
                :do (if (listp production)
                        (appendf to-nodes (let ((last  (first (last production))))
                                            (if (and (listp last)
                                                     (eql 'function (first last)))
                                                (butlast production)
                                                production)))
                        (push production to-nodes)))
              (dolist (to (mapcar (lambda (item)
                                    (gethash item elements))
                                  (remove-duplicates to-nodes)))
                (add-edge-between-nodes graph from to)))))
    graph))

(setf (text-file-contents "c11.dot") (generate-dot *graph*))

;; ;; dot -Kfdp -Tps c11.dot> c11.ps && open c11.ps
;; (uiop:run-program "bash -c 'dot -Kfdp -Tps c11.dot> c11.ps && open c11.ps'")
;; 
;; (cardinal (nodes *graph*))
;; (cardinal (edges *graph*))
;; 419
;; 248
