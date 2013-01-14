
(defvar *rewrite-rules* '())

(defmacro define-rewrite-rule (name antecedent --> consequent)
  `(push )
  )


(define-rewrite-rule setq-setf
    (setq (?n exprs (?+ ?ax)))
  --> `(setf ,@exprs))

(define-rewrite-rule setf-setf
    ((?n before (?* ?ax))
     (setf (?n exprs1 (?+ ?ax)))
     (setf (?n exprs2 (?+ ?ax)))
     (?n after (?* ?ax)))
  --> `(setf ,@exprs1 ,@exprs2))


(
 (map-sexps file
            (lambda (sexp start end)

              )
            :deeply t
            :aton nil))
