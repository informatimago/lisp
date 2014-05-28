;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               solitaire.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Plays solitaire... alone.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-12-06 <PJB> Corrected a few bugs.
;;;;    2004-03-19 <PJB> Added a player.
;;;;    2004-01-08 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2012
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
;;;;****************************************************************************

(defpackage :com.informatimago.common-lisp.solitaire
  (:use :common-lisp)
  (:export "MAIN" "AUTO-PLAY"))
(in-package :com.informatimago.common-lisp.solitaire)



(defconstant +base+ 53 "Number of different cards + one for no-card.")


(defstruct (card
             (:print-function
              (lambda (card stream level)
                (declare (ignore level))
                (format stream "[~2@A ~3A]"
                        (case (card-figure card)
                          ((1)  "As")
                          ((11) "Va")
                          ((12) "Re")
                          ((13) "Ro")
                          (otherwise (card-figure card)))
                        (subseq (symbol-name (card-family card)) 0 3)))))
  (figure 1      :type (integer 1 13))
  (family :coeur :type (member :coeur :carreau :trefle :pique)))


(defun card-number (card)
  (1+ (+ (* 13 (position (card-family card) #(:coeur :carreau :trefle :pique)))
         (1- (card-figure card)))))


(defun card-label (card)
  (declare (type card card))
  (format nil "~A" card))


(defun card-color (card)
  (declare (type card card))
  (if (or (eq (card-family card) :coeur)
          (eq (card-family card) :carreau)) :red :black))


(defun card-figure-is (card value)
  (= (card-figure card)
     (case value
       ((:as) 1)
       ((:valet) 11)
       ((:reine) 12)
       ((:roi)   13)
       (otherwise value))))


(deftype stock () 'list)


(defun make-card-stock ()
  (let ((stock '()))
    (dolist (family '(:coeur :carreau :trefle :pique))
      (dotimes (figure 13)
        (push (make-card :family family :figure (1+ figure)) stock)))
    stock))


(defun shuffle-stock (stock)
  (declare (type stock stock))
  (map 'list (function cdr)
       (sort (do ((stock  stock (cdr stock))
                  (i     0    (1+ i))
                  (order (make-array (list (length stock)))))
                 ((null stock) order)
               (setf (aref order i) (cons (random 1000000) (car stock))))
             (lambda (a b) (< (car a) (car b))))))


(defclass solitaire ()
  ((stock       :accessor stock       :type list)
   (dealt       :accessor dealt       :type list :initform nil)
   (discard     :accessor discard     :type list :initform nil)
   (foundations :accessor foundations :type (array list (4)))
   (hidden      :accessor hidden      :type (array list (7)))
   (visible     :accessor visible     :type (array list (7)))
   )
  (:documentation "A solitaire game. The one-by-one deal variant."))


(defun copy-array-of-list (array)
  (let ((copy (make-array (array-dimensions array))))
    (dotimes (i (apply (function *) (array-dimensions array)))
      (setf (row-major-aref copy i) (copy-seq (row-major-aref array i))))
    copy))


(defmethod copy ((self solitaire))
  (let ((copy (make-instance (class-of self))))
    (setf (stock       copy) (copy-seq (stock    self))
          (dealt       copy) (copy-seq (dealt    self))
          (discard     copy) (copy-seq (discard  self))
          (foundations copy) (copy-array-of-list (foundations self))
          (hidden      copy) (copy-array-of-list (hidden      self))
          (visible     copy) (copy-array-of-list (visible     self)))
    copy))


(defmethod can-deal ((self solitaire))
  (not (and (null (stock self)) (null (discard self)))))


(defmethod deal ((self solitaire))
  (when (= 3 (length (dealt self)))
    (push (car (last (dealt self))) (discard self))
    (setf (dealt self) (butlast (dealt self))))
  (when (null (stock self))
    (setf (stock    self) (nreverse (discard self))
          (discard self) nil))
  (if (null (stock self))
      nil
      (push (pop (stock self)) (dealt self)))
  self)


(defclass solitaire-3x3 (solitaire)
  ()
  (:documentation "A solitaire game. The three-by-three deal variant."))


(defmethod deal ((self solitaire-3x3))
  (setf (discard self) (nconc (dealt self) (discard self)))
  (setf (dealt self) nil)
  (dotimes (i 3)
    (when (null (stock self))
      (setf (stock    self) (discard self)
            (discard self) nil))
    (if (null (stock self))
        nil
        (push (pop (stock self)) (dealt self))))
  self)


(defmethod initialize-instance ((self solitaire) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (dealt      self) nil)
  (setf (discard    self) nil)
  (setf (stock       self) (shuffle-stock (make-card-stock)))
  (setf (foundations self) (make-array '(4) :initial-element nil))
  (setf (hidden     self) (make-array '(7) :initial-element nil))
  (setf (visible    self) (make-array '(7) :initial-element nil))
  (dotimes (i 7)
    (dotimes (j (- 7 i))
      (push (pop (stock self)) (aref (hidden self) (- 6 j))))
    (push (pop (aref (hidden self) i)) (aref (visible self) i)))
  self)


(defmethod columns ((self solitaire)) (array-dimension (hidden self) 0))


(defmethod state ((self solitaire))
  (let ((s  (cond
              ((stock self) (card-number(car (stock self))))
              ((discard self) (card-number(car (last (discard self)))))
              (t 0))))
    (setf s (+ (* +base+ s) (length (stock self)) (length (discard self))))
    (setf s (reduce (lambda (a b) (+ (* +base+ a) b))
                    (foundations self)
                    :key (lambda (c) (if (null c) 0 (card-number (car c))))
                    :initial-value s))
    (map 'nil (lambda (v h)
                (setf s (* s (expt +base+ (- +base+ (length v) (length h)))))
                (setf s (reduce (lambda (a b) (+ (* +base+ a) b))  v
                                :key (function card-number)
                                :initial-value s))
                (setf s (* s (expt +base+ (length h)))))
         (visible self) (hidden self))
    s))


(defparameter hline (make-string 66 :initial-element (character "-")))
(setf (aref hline 0)  (character "+")
      (aref hline 65) (character "+"))


(defmethod print-object ((self solitaire) (out stream))
  (format out "~%~A~%| " hline)
  ;; dealt cards:
  (do ((i 0 (1+ i))
       (d (reverse (dealt self)) (cdr d)))
      ((>= i 3))
    (if (null d)
        (format out "         ")
        (format out "~8A " (card-label (car d)))))
  ;; foundations:
  (do ((i 0 (1+ i))
       (f))
      ((>= i 4))
    (setf f (car (aref (foundations self) i)))
    (if (null f)
        (format out "         ")
        (format out "~8A " (card-label f))))
  ;; columns:
  (format out "|~%| ")
  (format out "                           ")
  (format out "-------- -------- -------- -------- |~%")
  (do ((hidden  (map 'vector (function length)  (hidden  self)))
       (visible (map 'vector (function reverse) (visible self))))
      ((every (function null) visible))
    (format out "| ")
    (dotimes (i (columns self))
      (cond
        ((< 0 (aref hidden i))
         (progn (format out "[******] ") (decf (aref hidden i))))
        ((aref visible i)
         (format out "~8A " (card-label (pop (aref visible i)))))
        (t (format out "         "))))
    (format out "|~%"))
  (format out "~A~%" hline))


(defmethod search-foundation-for-card ((self solitaire) (card card))
  (do ((i 0 (1+ i)))
      ((or (>= i 4)
           (and
            (null (aref (foundations self) i))
            (= 1 (card-figure card)))
           (and
            (aref (foundations self) i)
            (eql (card-family (car (aref (foundations self) i)))
                 (card-family card))
            (= (1+ (card-figure (car (aref (foundations self) i))))
               (card-figure card))))
       (and  (< i 4) i))))


(defmethod get-valid-moves ((self solitaire))
  "
RETURN: A list of valid moves.
"
  (let ((moves '()))
    (when (or (discard self) (stock self))
      (push '(:deal) moves))
    ;; dealt -> foundation
    (when (and (dealt self)
               (search-foundation-for-card self (car (dealt self))))
      (push '(:dealt :foundation) moves))
    ;; columns -> foundation
    (dotimes (n (columns self))
      (let ((column (aref (visible self) n)))
        (when (and column (search-foundation-for-card self (car column)))
          (push `((:column ,n 1) :foundation) moves))))
    ;; dealt -> columns
    (when (dealt self)
      (let ((card (car (dealt self))))
        (dotimes (n (columns self))
          (let ((topmost (car (aref (visible self) n))))
            (when (or (and (null topmost) (card-figure-is card :roi))
                      (and topmost
                           (= (card-figure topmost) (1+ (card-figure card)))
                           (not (eql (card-color topmost) (card-color card)))))
              (push `(:dealt (:column ,n)) moves))))))
    ;; columns -> other column
    (dotimes (n (columns self))
      (let ((from (aref (visible self) n)))
        (dolist (card from)
          (dotimes (m (columns self))
            (let ((topmost (car (aref (visible self) m))))
              (when (and (/= m n)
                         (or (and (null topmost) (card-figure-is card :roi))
                             (and topmost
                                  (= (card-figure topmost)
                                     (1+ (card-figure card)))
                                  (not (eql (card-color topmost)
                                            (card-color card))))))
                (push `((:column ,n ,(1+ (position card from)))
                        (:column ,m)) moves)))))))
    moves))


(defun select-forced-move (moves)
  (if (= 1 (length moves))
      (first moves)
      (let ((fmoves
             (remove-if
              (lambda (x) (not (and (= (length x) 2) (eq :foundation (second x)))))
              moves)))
        (when fmoves (first fmoves)))))


(defmethod move ((self solitaire) from to)
  "
FROM:   either (:column S N) or :dealt
TO:     either (:column D)   or :foundation

  :dealt     <=> (pop (dealt self))
  :foundation <=> (aref (fundations self) i) such as
                  (or (eq (family source) (family (aref (foundations self) i)))
                      (and (null (aref (foundations self) i))
                           (for all j
                              (not (eq (family source)
                                       (family (aref (foundations self) j)))))))
  (:column S N) specifies to move the N topmost cards from column S
                (all of which must be visible).
  (:column D)   specifies to move to the column D.
"
  (let ((cards nil))
    ;; from: fill cards with the cards to be moved (in reverse order).
    (cond
      ((eq from :dealt)
       (when (null (dealt self))
         (deal self)
         (when (null (dealt self))
           (error "No more cards in the stock to deal.")))
       (setf cards (list (pop (dealt self)))))
      ((and (listp from)
            (eq (car from) :column)
            (= 3 (length from)))
       (assert (and (<= 0 (second from) (1- (columns self)))
                    (<= 1 (length (aref (visible self) (second from)))))
               ((second from))
               "From column number must be between 0 and ~D, ~
               and from column must contain at least one visible card."
               (1- (columns self)))
       (assert (<= 1 (third from) (length (aref (visible self) (second from))))
               ((third from))
               "From card number must be between 1 and ~D."
               (length (aref (visible self) (second from))))
       (do ((i 0 (1+ i))) ((>= i (third from)))
         (push (pop (aref (visible self) (second from))) cards))
       (when (and (null (aref (visible self) (second from)))
                  (aref (hidden self)(second from)))
         (push (pop (aref (hidden self)(second from)))
               (aref (visible self) (second from)))))
      (t (error "Invalid FROM specifier. Must be :dealt or (:column S N).")))
    (when (null cards) (error "No card to move."))
    ;; to: move the cards to the destination.
    (cond
      ((eq to :foundation)
       (dolist (card cards)
         (let ((foundation (search-foundation-for-card self card)))
           (unless foundation
             (error "Cannot move card ~A to any foundation." (card-label card)))
           (push  card (aref (foundations self) foundation)))))
      ((and (listp to)
            (eq (car to) :column)
            (= 2 (length to)))
       (assert (<= 0 (second to) (1- (columns self)))
               ((second to))
               "To column number must be between 0 and ~D."
               (1- (columns self)))
       (if (or (and (null (aref (visible self) (second to)))
                    (null (aref (hidden  self) (second to)))
                    (card-figure-is (first cards) :roi))
               (and (aref (visible self) (second to))
                    (=  (card-figure
                         (first (aref (visible self) (second to))))
                        (1+ (card-figure (first cards))))
                    (not (eql (card-color
                               (first (aref (visible self) (second to))))
                              (card-color (first cards))))))
           (dolist (card cards)
             (push card (aref (visible self) (second to))))
           (error "Cannot move ~A over ~A." (first cards)
                  (if (first (aref (visible self) (second to)))
                      (first (aref (visible self) (second to)))
                      "an empty column."))))
      (t (error "Invalid TO specifier. Must be :foundation or (:column D)."))))
  self)


(defmethod play-move ((self solitaire) move)
  (if (eq :deal (first move))
      (deal self)
      (move self (first move) (second move))))


(defun read-move ()
  ;;  (mapcar
  ;; (lambda (x) (if (symbolp x) (string x) x))
  (handler-case  (let ((*package* (find-package "KEYWORD")))
                   (read-from-string (format nil "(~A)" (read-line))))
    (error (err) (format *error-output* "~A~%" err) nil)))


(defun unparse-move (command-sexp)
  (cond
    ((atom command-sexp)           '())
    ((equal command-sexp '(:deal)) '(:deal))
    ((equal command-sexp '(:quit)) '(:quit))
    ((and (cdr command-sexp)
          (null (cddr command-sexp)))
     `(:move ,@ (let ((from  (first command-sexp)))
                  (cond ((eq :dealt from) '(:dealt))
                        ((and (listp from)
                              (eq :column (first from))
                              (integerp (second from))
                              (integerp (third from))
                              (null (cdddr from)))
                         (rest from))
                        (t
                         (error "Invalid source in command-sexp ~S" command-sexp))))
             , (let ((to (second command-sexp)))
                 (cond
                   ((eq :foundation to) to)
                   ((and (listp to)
                         (eq :column (first to))
                         (integerp (second to))
                         (null (cddr to)))
                    (second to))
                   (t
                    (error "Invalid destination in command-sexp ~S" command-sexp)))) ))
    (error "Invalid command-sexp ~S" command-sexp)))


(defun test/unparse-move ()
  (loop
    :for (parsed unparsed)
    :in '((() nil)
          ((:deal) (:deal))
          ((:quit) (:quit))
          ((:dealt :foundation)          (:move :dealt :foundation))
          ((:dealt (:column 42))         (:move :dealt 42))
          (((:column 33 2) :foundation)  (:move 33 2 :foundation))
          (((:column 33 2) (:column 42)) (:move 33 2 42)))
    :do (assert (equal (unparse-move parsed) unparsed)
                () "(unparse-move '~S) -> ~S instead of ~S" parsed (unparse-move parsed) unparsed)
    :do (assert (equal (parse-move unparsed) parsed)
                () "(parse-move '~S) -> ~S instead of ~S" unparsed (parse-move unparsed) parsed))
  :success)

(test/unparse-move)


(defun parse-move (command)
  "
DO:     Parses the following grammar:
            COMMAND  -->  :deal | :move FROM TO | :quit | :exit .
            FROM     -->  :dealt | S N .
            TO       -->  :foundation | D .
            S        -->  (integer 0)
            D        -->  (integer 0)
            N        -->  (integer 0)
        and returns a list of the form:
          (), (:deal), (:quit), or (from to)
        with from of the form  :dealt or (:column S N)
        and to of the form:    (:column D) or :foundation
"
  (when (listp command)
    (case (car command)
      ((:quit :exit) '(:quit))
      ((:deal)       '(:deal))
      ((:move)
       (pop command)
       (let (from to)
         (cond
           ((eq (car command) :dealt)
            (setf from :dealt)
            (pop command))
           ((and (numberp (first command)) (numberp (second command)))
            (setf from (list :column (pop command) (pop command))))
           (t (return-from parse-move ())))
         (cond
           ((eq (car command) :foundation)
            (setf to :foundation)
            (pop command))
           ((numberp (first command))
            (setf to (list :column  (pop command))))
           (t (return-from parse-move ())))
         (when command (warn "Superfluous arguments ~S." command))
         (list from to)))
      (otherwise '()))))


(defmethod play ((self solitaire))
  (let ((nfmd 0))
    (loop
      (tagbody
       :loop
         ;; (format t "~%~A~%" (state self))
         (format t "~A" self)
         (let ((moves (get-valid-moves self)))
           (format t "Valid moves: ~%~{    ~{~A~^ ~}~%~}"
                   (mapcar (function unparse-move) (cons '(:quit) moves)))
           (let ((forced-move (select-forced-move moves)))
             (if forced-move
                 (progn
                   (format t "Forced move: ~S~%" forced-move)
                   (if (eq :deal (car forced-move))
                       (if (<= (incf nfmd)
                               (+ (length (stock self)) (length (discard self))))
                           (deal self)
                           (progn (format t "Draw~%") (return-from play)))
                       (progn
                         (setf nfmd 0)
                         (move self (first forced-move) (second forced-move)))))
                 (progn
                   (setf nfmd 0)
                   (format t "Your move: ")
                   (let* ((input (read-move))
                          (move (parse-move input)))
                     (cond
                       ((null move))
                       ((eq (car move) :quit)
                        (return-from play))
                       ((member move moves :test (function equalp))
                        (play-move self move) (go :loop)))
                     (format t "~&Invalid move: ~S~%" input))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Solitaire Player
;;

(defclass solitaire-state ()
  ((state
    :initform 0
    :initarg :state
    :reader state
    :type integer
    :documentation "The integer encoding the solitaire state.")
   (game
    :initarg :game
    :reader game
    :type solitaire
    :documentation "The solitaire state.")
   (valid-moves 
    :initform ()
    :initarg :valid-moves
    :accessor valid-moves
    :type list
    :documentation "A list of valid moves from this state.")
   (played-moves
    :initform ()
    :initarg :played-moves
    :accessor played-moves
    :type list
    :documentation "A list of moves already played from this state.")
   (consequent-states
    :initform ()
    :initarg :consequent-states
    :accessor consequent-states
    :type list
    :documentation
    "A list of (cons move solitaire-state) already reached form this state."))
  (:documentation
   "A visible solitaire game state, associated with the moves yet to play."))


;; states x valid-moves  --> states
;;( state , move ) --------> state


(defclass solitaire-player ()
  ((game
    :initform (make-instance 'solitaire)
    :initarg :game
    :accessor game
    :type solitaire
    :documentation "The game that is played by the player.")
   (states
    :initform (make-hash-table :test (function eql))
    :accessor states
    :type hash-table
    :documentation "A hash of solitaire-state played up to now.")
   (current-state
    :accessor current-state
    :type solitaire-state
    :documentation "The current state of the game."))
  (:documentation "A solitaire player."))



(defmethod register-state ((self solitaire-player) (state solitaire-state))
  (setf (gethash (state state) (states self)) state))



(defmethod initialize-instance ((self solitaire-player) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (current-state self)
        (make-instance 'solitaire-state :state (state (game self))))
  (register-state self (current-state self))
  self)


(defmethod play-move ((self solitaire-player) move)
  (let ((current-state (current-state self)))
    (push move (played-moves current-state))
    (if (eq :deal (car move))
        (deal (game self))
        (move (game self) (first move) (second move)))
    (let* ((new-game-state (state (game self)))
           (new-state (gethash new-game-state (states self))))
      (unless new-state
        (setf new-state (make-instance 'solitaire-state
                            :state new-game-state
                            :game (game self)))
        (register-state self new-state))
      (push (cons move new-state) (consequent-states current-state))
      (setf (current-state self) new-state))))



(defmethod search-game ((self solitaire-player)
                        &key (verbose t) (show-game t))
  (let ((nfmd 0) (steps 0))
    (loop
      (incf steps)
      (let ((current-state (current-state self))
            (moves (get-valid-moves (game self))))
        (setf (valid-moves current-state) moves)
        (when show-game
          (format t "~A" (game self)))
        (when verbose
          ;; (format t "~%~A~%" (state current-state ))
          (format t "Valid moves: ~%~{    ~S~%~}" moves))
        (let ((forced-move (select-forced-move moves)))
          (if forced-move
              (progn
                (when verbose
                  (format t "Forced move: ~S~%" forced-move))
                (if (eq :deal (car forced-move))
                    (if (<= (incf nfmd)
                            (+ (length (stock (game self)))
                               (length (discard (game self)))))
                        (play-move self forced-move)
                        (progn   (format t "Draw~%")
                                 (return-from search-game (values nil steps))))
                    (progn
                      (setf nfmd 0)
                      (play-move self forced-move)
                      (when (every (lambda (f) (and f (= 13 (card-figure (car f)))))
                                   (foundations (game self)))
                        (format t "Solitaire won!~%")
                        (return-from search-game (values t steps))))))
              (progn
                (setf nfmd 0)
                (setf moves (set-difference moves (played-moves current-state)
                                            :test (function equalp)))
                (cond
                  ((null moves)
                   (format t "Remains no unplayed move. I give up.~%")
                   (return-from search-game (values  nil steps)))
                  ((= 1 (length moves))
                   (when verbose
                     (format t "Remains to play: ~S~%" (car moves)))
                   (play-move self (car moves)))
                  (t
                   (when verbose
                     (format t "Choose to play: ~S~%" (car moves)))
                   (play-move self (car moves))  )))))))))


(defmethod play-game ((self solitaire-player)
                      &key (verbose t) (show-game t))
  (let ((nfmd 0) (steps 0))
    (loop
      (incf steps)
      (let ((current-state (current-state self))
            (moves (get-valid-moves (game self))))
        (when show-game
          (format t "~A" (game self)))
        (when verbose
          ;; (format t "~%~A~%" (state current-state ))
          (format t "Valid moves: ~%~{    ~S~%~}" moves))
        (let ((forced-move (select-forced-move moves)))
          (if forced-move
              (progn
                (when verbose
                  (format t "Forced move: ~S~%" forced-move))
                (if (eq :deal (car forced-move))
                    (if (<= (incf nfmd)
                            (+ (length (stock (game self)))
                               (length (discard (game self)))))
                        (play-move self forced-move)
                        (progn   (format t "Draw~%")
                                 (return-from play-game (values nil steps))))
                    (progn
                      (setf nfmd 0)
                      (play-move self forced-move)
                      (when (every (lambda (f) (and f (= 13 (card-figure (car f)))))
                                   (foundations (game self)))
                        (format t "Solitaire won!~%")
                        (return-from play-game (values t steps))))))
              (progn
                (setf nfmd 0)
                (setf moves (set-difference moves (played-moves current-state)
                                            :test (function equalp)))
                (cond
                  ((null moves)
                   (format t "Remains no unplayed move. I give up.~%")
                   (return-from play-game (values  nil steps)))
                  ((= 1 (length moves))
                   (when verbose
                     (format t "Remains to play: ~S~%" (car moves)))
                   (play-move self (car moves)))
                  (t
                   (when verbose
                     (format t "Choose to play: ~S~%" (car moves)))
                   (play-move self (car moves))  )))))))))



(defvar *player* "The last player who played. To debug.")


(defun auto-play (&key (verbose t) (show-game nil) (loop nil))
  (let ((n 0) (se 0) (sa 0) (w 0)
        (*random-state* (make-random-state t))
        (*print-circle* nil)
        (*print-readably* nil)) 
    (loop
      (setf *player* (make-instance 'solitaire-player))
      (multiple-value-bind (won steps) (play-game *player* :verbose verbose
                                                  :show-game show-game)
        (format t "~&~A~%" (game *player*))
        (format t "~:[LOST~;WON~]~%" won)
        (format t "-------------------------~%")
        (format t "Number of steps  : ~6D~%" steps)
        (format t "Number of states : ~6D~%"
                (hash-table-count (states *player*)))
        (format t "-------------------------~%")
        (unless loop (return-from auto-play won))
        (incf n) (incf se steps) (incf sa (hash-table-count (states *player*)))
        (when won (incf w))
        (format t "~6D ~12,6F ~12,6F ~8,6F~%" n (/ se n) (/ sa n) (/ w n))))))


(defun main ()
  (let ((*random-state* (make-random-state t))
        (*print-circle* nil)
        (*print-readably* nil))
    (play (make-instance 'solitaire))))



#||
Heuristics:

- remove cards from the highest hidden stacks first.
- remove cards from the lowest stacks to free spaces for a king.
- between two kings of different colors to be put on an empty space,
chose the one that will allow to move a queen (and the bigest number
of cards) next (preferably from the stacks, but otherwise from the stock).

- at the end, if there's nothing else to do, move away cards that free
cards that can go to the foundation.


- end-of-game constraints
||#

;;;; THE END ;;;;

