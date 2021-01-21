;; http://www.enseignement.polytechnique.fr/informatique/profs/Georges.Gonthier/pi98/compte.html
;; 
;; Le compte est bon
;; 
;; Sujet proposé par Jean-Eric Pin
;; 
;; pin@liafa.jussieu.fr
;; 
;; Préambule
;; 
;; ``Le compte est bon'' est l'une des composantes du jeu télévisé ``les
;; chiffres et les lettres'' qui a connu ses heures de gloire il y a
;; quelques années. Le but de ce projet est de réaliser un logiciel qui
;; donne la meilleure réponse à ce jeu dans un délai de quelques
;; secondes.  Les règles du jeu
;; 
;; Le joueur tire au hasard, sans remplacement, six plaques portant des
;; numéros parmi un ensemble de 28 plaques ainsi constitué
;; 
;; 1. 20 plaques numérotées de 1 à 10 (2 par nombre)
;; 2. 2 plaques de 25
;; 3. 2 plaques de 50
;; 4. 2 plaques de 75
;; 5. 2 plaques de 100
;; 
;; On tire également au hasard un nombre N entre 100 et 999. Le but du
;; jeu est de trouver une valeur aussi proche que possible de N, en
;; utilisant les nombres inscrits sur les plaques, en respectant les
;; règles suivantes :
;; 
;; 1. Chaque plaque doit être utilisée au plus une fois (mais il n'est
;; pas nécessaire d'utiliser toutes les plaques)
;; 
;; 2. Les seules opérations autorisées sont les quatre opérations
;; arithmétiques, +, -, $\times$ et /, restreintes aux entiers naturels
;; positifs : ainsi, les divisions ne sont autorisées que si leur reste
;; est nul et les nombres négatifs sont proscrits du calcul.  Dans le cas
;; où N ne peut être obtenu, on cherche à obtenir une valeur N-k ou N+k
;; avec k minimal. Lorsque plusieurs calculs sont possibles, on préfèrera
;; ceux qui utilisent le moins de plaques possibles.
;; 
;; 
;; 
;; Exemple 1. Si le nombre à trouver est 140 et si le tirage est 3  6  6  7  50
;;  50, on peut obtenir le bon compte en 5 plaques de la façon suivante :
;; 
;; Exemple 2. Si le nombre à trouver est 532 et si le tirage est 1  25  75  75 
;; 100  100, on peut obtenir le bon compte en 5 plaques de la façon suivante :
;; 
;; Exemple 3. Si le nombre à trouver est 660 et si le tirage est 2  2  7  50 
;; 75  100, on ne peut pas obtenir 660, mais on peut obtenir 661 de la façon suivante :
;; 
;; Détail du sujet
;; 
;; On suivra à la lettre les instructions données ci-dessous. On pourra,
;; si on le souhaite, tester d'autres heuristiques, mais uniquement une
;; fois que l'algorithme décrit ci-dessous aura été correctement
;; implanté.
;; 
;; La première partie consistera à réaliser le tirage aléatoire des six
;; plaques P[1], ..., P[6] (sans remplacement) et du nombre N. Aucun
;; algorithme spécifique n'est donné pour cette partie du travail, mais
;; on veillera à ce que les tirages soient équiprobables.
;; 
;; La deuxième partie consistera à rechercher la solution optimale. Pour
;; cela, on partira du résultat à trouver, N (ou en cas d'échec, N-1,
;; N+1, N-2, N+2, etc.), et on cherchera à obtenir en utilisant les six
;; plaques. Par exemple, dans l'exemple précédent, on aurait, en
;; remontant les calculs à partir de 661 , 661-50 = 611, 611-100 = 511,
;; 511/7 =73, 73+2 = 75, 75-75 = 0. Pour celà, on construira un tableau T
;; indexé par les 26 sous-ensembles de $\{1, \dots, 6\}$. Si $I = \{i_1,
;; \ldots, i_k\}$ est une partie de $\{1, \dots, 6\}$, T[I] contiendra
;; l'ensemble des nombres que l'on peut obtenir à partir de N en
;; utilisant les plaques P[i1], ..., P[ik]. Sur notre exemple, on devrait
;; trouver $P[\emptyset] = \{661\}$,$P[\{1\}] = P[\{2\}] = \{661, 661-2,
;; 661+2, 661\times 2\}$, etc. Pour le calcul du tableau, on écrira une
;; procédure qui prend pour paramètres deux ensembles d'entiers E et F,
;; et qui retourne l'ensemble $\langle E, F\rangle$ des entiers obtenus
;; en une seule opération arithmétique portant sur un élément de E et un
;; élément de F. Le calcul du tableau se fait ensuite en s'appuyant sur
;; la formule
;; 
;; \begin{displaymath}
;; T[I] = \bigcup_{(I_1, I_2)\hbox{ partition de $I$}}\langle T[I_1],
;; T[I_2]\rangle\end{displaymath}
;; 
;; Il faut donc coder les sous-ensembles de $\{1, \dots, 6\}$. La
;; solution la plus simple consiste à coder une partie I par la position
;; des 1 dans la représentation binaire d'un entier compris entre et
;; 63. Ainsi l'entier 43, dont la représentation binaire est 101011, code
;; le sous-ensemble $\{1,3,5,6\}$. Autrement dit, la partie I est codée
;; par l'entier $\sum_{i\in I}2^{6-i}$.  La seconde difficulté est de
;; choisir une bonne représentation pour les ensembles d'entiers. On
;; choisira la (ou les) représentation(s) qui permettent le calcul le
;; plus rapide. Le rapport devra être très explicite sur le choix de
;; cette représentation (tableaux, listes, listes ordonnées, listes
;; doublement chaînées, arbres binaires, arbres équilibrés, etc.) et sur
;; les arguments qui ont justifié ce choix.
;; 
;; Enfin, on pourra réfléchir sur la nécessité de calculer tous les
;; nombres accessibles à partir de N. Pour prendre un exemple, est-il
;; vraiment utile de savoir que $661 \times 50 \times 75 \times 100$ est
;; dans $T[\{4, 5, 6\}]$ ? La question est plus délicate qu'il n'y paraît
;; ...
;; 
;; Interface
;; 
;; On fera une interface propre, mais sans fioritures, car ce n'est pas
;; l'objectif du projet. Elle devra proposer à l'utilisateur le choix
;; entre un tirage aléatoire ou manuel (essentiel pour la mise au point
;; !) puis afficher le résultat du calcul dans une mise en page
;; correcte. On évitera donc simplement les sorties du type
;; 
;; 7 x 3 = 21
;; 7 x 3=21
;; 7 x 3       = 21
;; Voici un modèle simple, mais parfaitement suffisant, de ce qui est attendu.
;; Tirage manuel ou aleatoire ? (m ou a) a
;; 
;; Nombre a trouver :  140      Voici le tirage :    7   6   6  50  50   3
;; Le compte est bon ! (en 5 plaques)
;; 7 x 3 = 21
;; 21 - 6 = 15
;; 15 x 6 = 90
;; 90 + 50 = 140
;; Voulez-vous continuer ? (o ou n) o
;; 
;; Tirage manuel ou aleatoire ? (m ou a) m
;; Donnez votre tirage :
;; 
;; Premiere plaque : 2
;; 
;; Deuxieme plaque : 2
;; 
;; Troisieme plaque : 7
;; 
;; Quatrieme plaque : 50
;; 
;; Cinquieme plaque : 75
;; 
;; Sixieme plaque : 100
;; 
;; Donnez le nombre a trouver : 660
;; 
;; Nombre a trouver :  660      Voici le tirage :    2   2   7  50  75 100
;; J'ai trouve 661
;; 75 - 2 = 73
;; 73 x 7 = 511
;; 511 + 50 = 561
;; 561 + 100 = 661
;; Voulez-vous continuer ? (o ou n)
;; 
;; 
;; 1/6/1998


(defparameter *numbers*
  #(1 2 3 4 5 6 7 8 9 10 25 50 75 100
    1 2 3 4 5 6 7 8 9 10 25 50 75 100))

(defun select-numbers ()
  (loop
    :with remaining := (copy-seq *numbers*)
    :repeat 6
    :for l := (length remaining) :then (1- l)
    :for r := (random l)
    :collect (loop :for n := (prog1
                                 (aref remaining r)
                               (setf (aref remaining r) nil))
                     :then   (prog2 (setf r (mod (1+ r) (length remaining)))
                                 (aref remaining r)
                               (setf (aref remaining r) nil))
                   :until n
                   :finally (return n))
      :into numbers
    :finally (return (values numbers remaining))))

(defun random-number ()
  (+ 100 (random (- 999 100))))

(defun le-compte-est-bon-problem ()
  (list (select-numbers)
        (random-number)))

(defun problem-numbers (problem) (first  problem))
(defun problem-target  (problem) (second problem))


(defun sexp-replace-nth (new old sexp n)
  (loop
    :for current :on sexp
    :if (eql old (car current))
      :do (if (zerop n)
              (progn
                (setf (car current) new)
                (return-from sexp-replace-nth (values sexp -1)))
              (progn
                (decf n)))
    :else :if (consp (car current))
            :do (when (minusp (setf n (nth-value 1 (sexp-replace-nth new old (car current) n))))
                  (return-from sexp-replace-nth (values sexp -1))))
  (values sexp n))

(defun enumerate-trees (new old trees)
  (let ((leaves (count old (flatten (first trees)))))
    (remove-duplicates
     (loop
       :for tree :in trees
       :append (loop :for i :below leaves
                     :collect (sexp-replace-nth (copy-tree new) old (copy-tree tree) i)))
     :test (function equalp))))


;; (let ((*print-right-margin* 40))
;;   (pprint (enumerate-trees '(op n n) 'n '((op (op (op (op n n) n) n) n)
;;                                           (op (op (op n (op n n)) n) n)
;;                                           (op (op (op n n) (op n n)) n)
;;                                           (op (op n (op (op n n) n)) n)
;;                                           (op (op n (op n (op n n))) n)
;;                                           (op (op (op n n) n) (op n n))
;;                                           (op (op n (op n n)) (op n n))
;;                                           (op (op n n) (op (op n n) n))
;;                                           (op n (op (op (op n n) n) n))
;;                                           (op n (op (op n (op n n)) n))
;;                                           (op (op n n) (op n (op n n)))
;;                                           (op n (op (op n n) (op n n)))
;;                                           (op n (op n (op (op n n) n)))
;;                                           (op n (op n (op n (op n n)))))
;;                            )))


(defparameter *trees* '(((op n n))
 
                        ((op (op n n) n)
                         (op n (op n n)))
 
                        ((op (op (op n n) n) n)
                         (op (op n (op n n)) n)
                         (op (op n n) (op n n))
                         (op n (op (op n n) n))
                         (op n (op n (op n n))))

                        ((op (op (op (op n n) n) n) n)
                         (op (op (op n (op n n)) n) n)
                         (op (op (op n n) (op n n)) n)
                         (op (op n (op (op n n) n)) n)
                         (op (op n (op n (op n n))) n)
                         (op (op (op n n) n) (op n n))
                         (op (op n (op n n)) (op n n))
                         (op (op n n) (op (op n n) n))
                         (op n (op (op (op n n) n) n))
                         (op n (op (op n (op n n)) n))
                         (op (op n n) (op n (op n n)))
                         (op n (op (op n n) (op n n)))
                         (op n (op n (op (op n n) n)))
                         (op n (op n (op n (op n n)))))
 
                        ((op (op (op (op (op n n) n) n) n) n)
                         (op (op (op (op n (op n n)) n) n) n)
                         (op (op (op (op n n) (op n n)) n) n)
                         (op (op (op n (op (op n n) n)) n) n)
                         (op (op (op n (op n (op n n))) n) n)
                         (op (op (op (op n n) n) (op n n)) n)
                         (op (op (op n (op n n)) (op n n)) n)
                         (op (op (op n n) (op (op n n) n)) n)
                         (op (op n (op (op (op n n) n) n)) n)
                         (op (op n (op (op n (op n n)) n)) n)
                         (op (op (op n n) (op n (op n n))) n)
                         (op (op n (op (op n n) (op n n))) n)
                         (op (op n (op n (op (op n n) n))) n)
                         (op (op n (op n (op n (op n n)))) n)
                         (op (op (op (op n n) n) n) (op n n))
                         (op (op (op n (op n n)) n) (op n n))
                         (op (op (op n n) (op n n)) (op n n))
                         (op (op n (op (op n n) n)) (op n n))
                         (op (op n (op n (op n n))) (op n n))
                         (op (op (op n n) n) (op (op n n) n))
                         (op (op n (op n n)) (op (op n n) n))
                         (op (op n n) (op (op (op n n) n) n))
                         (op n (op (op (op (op n n) n) n) n))
                         (op n (op (op (op n (op n n)) n) n))
                         (op (op n n) (op (op n (op n n)) n))
                         (op n (op (op (op n n) (op n n)) n))
                         (op n (op (op n (op (op n n) n)) n))
                         (op n (op (op n (op n (op n n))) n))
                         (op (op (op n n) n) (op n (op n n)))
                         (op (op n (op n n)) (op n (op n n)))
                         (op (op n n) (op (op n n) (op n n)))
                         (op n (op (op (op n n) n) (op n n)))
                         (op n (op (op n (op n n)) (op n n)))
                         (op (op n n) (op n (op (op n n) n)))
                         (op n (op (op n n) (op (op n n) n)))
                         (op n (op n (op (op (op n n) n) n)))
                         (op n (op n (op (op n (op n n)) n)))
                         (op (op n n) (op n (op n (op n n))))
                         (op n (op (op n n) (op n (op n n))))
                         (op n (op n (op (op n n) (op n n))))
                         (op n (op n (op n (op (op n n) n))))
                         (op n (op n (op n (op n (op n n))))))))

(defun number-free-variables (tree result next-op next-value)
  (if (atom tree)
      (ecase tree
        ((nil) (values result next-op next-value))
        ((op) (number-free-variables nil (intern (format nil "OP~A" (incf next-op)))    next-op next-value))
        ((n)  (number-free-variables nil (intern (format nil "N~A"  (incf next-value))) next-op next-value)))
      (multiple-value-bind (left next-op next-value) (number-free-variables (car tree) nil next-op next-value)
        (multiple-value-bind (right next-op next-value) (number-free-variables (cdr tree) nil next-op next-value)
          (values (cons left right) next-op next-value)))))

(defparameter *numbered-trees*
  (mapcan (lambda (trees)
            (mapcar (lambda (tree) (number-free-variables tree nil 0 0))
                    trees))
          *trees*))

(defun substitute-expression (op1 op2 op3 op4 op5
                              n1 n2 n3 n4 n5 n6
                              expression)
  (loop :for (new old) :in (list (list op1 'op1)
                                 (list op2 'op2)
                                 (list op3 'op3)
                                 (list op4 'op4)
                                 (list op5 'op5)
                                 (list n1 'n1)
                                 (list n2 'n2)
                                 (list n3 'n3)
                                 (list n4 'n4)
                                 (list n5 'n5)
                                 (list n6 'n6))
        :for e := (subst new old expression)
          :then (subst new old e)
        :finally (return e)))

(defun op-or-dbz (op a b)
    (if (eq op '/)
        `(if (zerop ,b)
             (return-from :bad-division)
             (let ((quotient (,op ,a ,b)))
               (if (integerp quotient)
                   quotient
                   (return-from :bad-division))))
        `(,op ,a ,b)))

(defun infix (expression)
  (if (atom expression)
      expression
      (destructuring-bind (op a b) expression
        `(,(infix a) ,op ,(infix b)))))


(defun le-compte-est-bon (problem &key verbose)
  (let ((target (problem-target problem))
        (*closest* nil))
    (declare (special *closest*))
    (when verbose
      (format t "Nombres: ~{~A~^, ~}~%" (problem-numbers problem))
      (format t "Cible:   ~A~%"         (problem-target problem)))
    (dolist (expression *numbered-trees*)
      (dolist (numbers (com.informatimago.clext.association::permutations
                        (problem-numbers problem)))
        (dolist (op1 '(+ - * /))
          (dolist (op2 '(+ - * /))
            (dolist (op3 '(+ - * /))
              (dolist (op4 '(+ - * /))
                (dolist (op5 '(+ - * /))
                  (let ((try `(block
                                  :bad-division
                                (labels ((op1 (a b) ,(op-or-dbz op1 'a 'b))
                                         (op2 (a b) ,(op-or-dbz op2 'a 'b))
                                         (op3 (a b) ,(op-or-dbz op3 'a 'b))
                                         (op4 (a b) ,(op-or-dbz op4 'a 'b))
                                         (op5 (a b) ,(op-or-dbz op5 'a 'b))
                                         (expression (n1 n2 n3 n4 n5 n6)
                                           (declare (ignorable n1 n2 n3 n4 n5 n6))
                                           ,expression))
                                  (let ((target ,target)
                                        (result (expression ,@numbers)))
                                    (declare (special *closest*))
                                    (when (or (null *closest*)
                                              (= target result)
                                              (< (abs (- target result))
                                                 (abs (- target (first *closest*)))))
                                      (setf *closest* (list result (substitute-expression
                                                                    ',op1 ',op2 ',op3 ',op4 ',op5
                                                                    ,@numbers ',expression)))
                                      ,(when verbose
                                         `(format t "J'ai ~A = ~S~%" (first *closest*) (infix (second *closest*))))))))))
                    (eval try)
                    (when (= (first *closest*) target)
                      ;; early exit.
                      (return-from le-compte-est-bon *closest*))))))))))
    *closest*))

;; This brute force algorithm is to slow, the solution must be found in less than 40 seconds.
