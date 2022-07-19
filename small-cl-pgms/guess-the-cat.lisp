(defpackage "COM.INFORMATIMAGO.SMALL-CL-PGMS.GUESS-THE-CAT"
  (:use "COMMON-LISP")
  (:nicknames "GUESS-THE-CAT")
  (:export "MAIN")
  (:documentation "
https://www.youtube.com/watch?v=yZyx9gHhRXM
"))
(in-package "COM.INFORMATIMAGO.SMALL-CL-PGMS.GUESS-THE-CAT")

(defparameter *box-count* 5)
(defvar *debug* nil)

(defun explanations ()
  (format t "
Catch the Cat

A cat is hiding in one of ~:R boxes a that are lined up in a row.
The boxes are numbered from 1 to ~:*~:R.

Each night the cat hides in an adjacent box, exactly one number away.

Each morning, you can open a single box to try to find the cat.

Can you win this game of hide and seek?
What is your strategy to find the cat?

"
          *box-count*)
  (force-output))

(defun initial-cat-position ()
  (1+ (random *box-count*)))

(defun new-cat-position (old-position)
  (cond
    ((= 1 old-position)           2)
    ((= *box-count* old-position) (- *box-count* 1))
    (t (if (zerop (random 2))
           (- old-position 1)
           (+ old-position 1)))))

(defun catch-the-cat ()
  (let ((cat (initial-cat-position)))
    (loop
      :for guesses :from 1
      :do (format *query-io* "~%The cat is hidden in one of the boxes from 1 to ~A.~%" *box-count*)
          (when *debug*
            (format *query-io*  "~V@{~A~:*~}~*&~V@{~A~:*~}~&" (- cat 1) #\. (- *box-count* cat) #\.))
          (format *query-io* "Your guess: ")
          (finish-output *query-io*)
          (let ((guess (let ((line (read-line *query-io*)))
                         (ignore-errors (parse-integer line :junk-allowed t)))))
            (if (and (integerp guess) (<= 1 guess *box-count*))
                (if (= guess cat)
                    (progn
                      (format *query-io* "~&You found the cat!~%")
                      (loop-finish))
                    (progn
                      (format *query-io* "~&You missed the cat!~%")
                      (setf cat (new-cat-position cat))))
                (format *query-io* "~&Invalid input: type an integer from 1 to ~A.~%" *box-count*)))
      :finally (format *query-io* "~&You guessed ~A times.~%" guesses))))

(defun main ()
  (let ((*random-state* (make-random-state t)))
    (explanations)
    (catch-the-cat)))
