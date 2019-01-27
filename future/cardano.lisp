(defun cardano (a b c d)
  "Solve a 3rd degree equation."
  (let* ((Q   (/ (- (* 3 a c) (* b b))
                 9 a a))
         (R   (/ (- (* 9 a b c) (* 27 a a d) (* 2 b b b))
                 54 a a a))
         (vqr (sqrt (+ (* q q q) (* r r))))
         (S   (expt (+ r vqr) 1/3))
         (U   (expt (- r vqr) 1/3))
         (x1  (- (+ s u) (/ b 3 a)))
         (x2  (+ (/ (+ s u) -2) (/ b -3 a) (* #C(0 1) (/ (sqrt 3)  2) (- s u))))
         (x3  (+ (/ (+ s u) -2) (/ b -3 a) (* #C(0 1) (/ (sqrt 3) -2) (- s u)))))
    (list x1 x2 x3)))


(defun equa2 (a b c)
  (let ((delta (- (* b b) (* 4 a c))))
    (list (/ (+ (- b) (sqrt delta)) 2 a )
          (/ (- (- b) (sqrt delta)) 2 a ))))

