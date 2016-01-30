;;;;****************************************************************************
;;;;FILE:              geek-day.lisp
;;;;LANGUAGE:          Common-Lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This programs plays geek-day
;;;;    http://ars.userfriendly.org/cartoons/?id=20021215
;;;;USAGE
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2002-12-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2002
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;****************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(ql:quickload "split-sequence")

(defpackage "COM.INFORMATIMAGO.COMMON-LISP.GEEK-DAY"
  (:nicknames "GEEK-DAY")
  (:use "COMMON-LISP" "SPLIT-SEQUENCE")
  (:documentation "This programs plays geek-day.
    <http://ars.userfriendly.org/cartoons/?id=20021215>")
  (:export
   "PLAY-GEEK-DAY" "MAIN"
   ;; low-level:
   "PLAYER" "LOST" "UPDATE-SANITY" "UPDATE-POPULARITY" "HAVE-BREAKFAST"
   "HAVE-SHOWER" "CHOOSE-BACKWARD-OR-FORWARD" "RANDOMIZER" "*SQUARE-LIST*"
   "GEEK-DAY"  "INITIALIZE" "PLAY"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.GEEK-DAY")


(defgeneric lost (a))
(defgeneric update-sanity (a b))
(defgeneric update-popularity (a b))
(defgeneric have-breakfast (a))
(defgeneric have-shower (a))
(defgeneric choose-backward-or-forward (a))
(defgeneric play (a))
(defgeneric landing (a b c d))
(defgeneric take-off (a b c d))
(defgeneric initialize (a))


(defclass player ()
  ((name
    :initform "Unnamed Player"
    :initarg :name
    :accessor name
    :type string
    :documentation "This player's name.")
   (popularity
    :initform 5
    :initarg :popularity
    :accessor popularity
    :type integer
    :documentation "This player's popularity.")
   (sanity
    :initform 5
    :initarg :sanity
    :accessor sanity
    :type integer
    :documentation "This player's sanity.")
   (have-had-breakfast
    :initform nil
    :initarg :have-had-breakfast
    :accessor have-had-breakfast
    :type (or nil t)
    :documentation "This player have had a breakfast.")
   (have-had-shower
    :initform nil
    :initarg :have-had-shower
    :accessor have-had-shower
    :type (or nil t)
    :documentation "This player have had a shower."))
  (:documentation "A Player at Geek-Day."))


(defmethod lost ((self player))
  (or (<= (popularity self) 0)
      (<= (sanity     self) 0)))


(defmethod update-sanity ((self player) increment)
  (setf (sanity self) (+ (sanity self) increment))
  (format t "    Your sanity is ~[increased~;decreased~] by ~A.~%"
          (if (< increment 0) 1 0) (abs increment))
  (if (<=  (sanity self) 0)
      (format t "    YOU GO POSTAL AND LOSE!~%~%")))


(defmethod update-popularity ((self player) increment)
  (setf (popularity self) (+ (popularity self) increment))
  (format t "    Your popularity is ~[increased~;decreased~] by ~A.~%"
          (if (< increment 0) 1 0) (abs increment))
  (if (<= (popularity self) 0)
      (format t "    YOU'RE FIRED!~%~%")))


(defmethod have-breakfast ((self player))
  (setf (have-had-breakfast self) t))


(defmethod have-shower ((self player))
  (setf (have-had-shower self) t))


(defmethod choose-backward-or-forward ((self player))
  (format t "~%~A, please choose backward or forward? " (name self))
  (finish-output *standard-output*)
  (do ((rep (read-line) (read-line)))
      ((or (string-equal rep "backward")
           (string-equal rep "forward"))
       (string-equal rep "backward"))
    (format t "~%Becareful! Next time, pop-1!~&~A, ~
               please choose backward or forward? "
            (name self))))

  
(defvar random-state (make-random-state t))

(defun randomizer ()
  (1+ (random 6 random-state)))




;;("title" (before) (after))

(defparameter *square-list*
  '(
    ("Starting blocks."
     nil
     (lambda (square dice player)
       (declare (ignore player))
       (format t "~&    Therefore ~A."
               (if (<= 5 dice) "get out of bed" "keep sleeping"))
       (if (<= 5 dice)
           (+ dice square)
           square)))

    ("Ungum your eyes."            ( (popularity +1) ) nil)
    ("Get a hot shower."           ( (popularity +3)
                                    (lambda (player)
                                      (have-shower player)) ) nil)
    ("Depilate."                   ( (popularity +1) ) nil)
    ("Get a breakfast."            ( (sanity +3)
                                    (lambda (player)
                                      (have-breakfast player)) ) nil)
    ("Catch the bus."              ( (sanity -1) (popularity -1) ) nil)
    ("Oooh... Brain Engages."
     nil
     (lambda (square dice player)
       (if (choose-backward-or-forward player)
           (- square dice)
           (+ square dice))))
    ("Arrive \"late\"."            ( (popularity +1) (sanity -1) ) nil)
    ("First problem of the day."   ( (popularity -2) (sanity -3) ) nil)
    ("Called into meeting."        ( (sanity -4) ) nil)
    ("Boss in bad mood."           ( (popularity -2) ) nil)
    ("Water cooler break."         ( (lambda (player)
                                       (unless (have-had-shower player)
                                         (update-popularity player -3))) ) nil)
    ("Caffeine break!"             ( (sanity +3) ) nil)
    ("Caffeine break!"             ( (sanity +3) ) nil)
    ("Co-worker gives you the blame."
     ( (popularity +2) ) nil)
    ("Nap."                        ( (sanity +4) (popularity -2) ) nil)
    ("Wooo! A thought!"
     nil
     (lambda (square dice player)
       (if (choose-backward-or-forward player)
           (- square dice)
           (+ square dice))))
    ("Chair breaks."               ( (popularity +2) (sanity -1) ) nil)
    ("Machine locks up."           ( (sanity -2) ) nil)
    ("Power outage."               ( "It's okay, you're safe here." ) nil)
    ("Munchies!"                   ( (lambda (player)
                                       (unless (have-had-breakfast player)
                                         (update-sanity player -2))) ) nil)
    ("Caffeine run."               ( (popularity +1) ) nil)
    ("Sugar run."                  ( (sanity +1) ) nil)
    ("Co-worker takes credit."     ( (sanity -3) ) nil)
    ("Run to the loo."             ( (sanity +1) ) nil)
    ("Collapse on the couch at home."
     ("It's over. Start again tomorrow.")
     (lambda (square dice player)
       (declare (ignore square dice player))
       0))))


(defclass geek-day  nil
  ((board
    :initform (make-array (length *square-list*) :initial-contents *square-list*)
    :initarg :board
    :accessor board
    :documentation "The array of square compounding the game.")
   (players
    :initform nil
    :initarg :players
    :accessor players
    :documentation "The list of Player objects.")
   (markers
    :initform nil
    :accessor markers
    :documentation "A list of cons (player . square-index)."))
  (:documentation "A Geek-Day game."))





(defmethod play ((self geek-day))
  (do ()
      ((null (markers self)))
    ;; let's run a turn.
    (dolist (marker (markers self))
      ;; let's run a player.
      (let* ((player      (car marker))
             (square      (cdr marker))
             (square-data (aref (board self) square))
             )
        (setf (cdr marker) (take-off self player square square-data))
        (setq square       (cdr marker))
        (setq square-data  (aref (board self) square))
        (landing self player square square-data)
        )) ;;dolist
    (setf (markers self)
          (delete-if (lambda (item) (lost (car item))) (markers self)))
    (let ((lineform "~:(~20A~) ~16A ~16A ~16A~%"))
      (format t "~%~%")
      (format t lineform  "--------------------"
              "----------------"  "----------------"  "----------------")
      (format t lineform  "Name" "Popularity" "Sanity" "Square")
      (format t lineform  "--------------------"
              "----------------"  "----------------"  "----------------")
      (dolist (player (players self))
        (format t lineform  (name player) (popularity player) (sanity player)
                (if (lost player) "Lost" (cdr (assoc player (markers self)))))
        ) ;;dolist
      (format t lineform  "--------------------"
              "----------------"  "----------------"  "----------------"))))



(defmacro square-name (square-data) `(car   ,square-data))
(defmacro square-in   (square-data) `(cadr  ,square-data))
(defmacro square-out  (square-data) `(caddr ,square-data))


(defun lambdap (item)
  (and (consp item) (eq 'lambda (car item))))

(defmacro lamcall (lambda-expr &rest arguments)
  `(funcall (coerce ,lambda-expr 'function) ,@arguments))


(defmethod landing ((self geek-day) player square square-data)
  (declare (ignore square))
  (format t "~%~%~:(~A~):  ~A~%"
          (name player) (square-name square-data))
  (dolist (action (square-in square-data))
    (cond
     ((null action))
     ((stringp action)              (format t "    ~A~%" action) )
     ((lambdap action)              (lamcall action player) )
     ((eq 'popularity (car action)) (update-popularity player (cadr action)) )
     ((eq 'sanity (car action))     (update-sanity player (cadr action)) )
     (t (error "Invalid action in square-in ~W." action)))))


(defmethod take-off ((self geek-day) player square square-data)
  (let ((dice        (randomizer))
        (out         (square-out square-data)))
    (format t "~%~%~:(~A~), you roll"  (name player))
    (finish-output)
    (sleep 2)
    (format t " and get ~A.~%" dice)
    (finish-output)
    (min (1- (length (board self)))
               (if out (lamcall out square dice player)
                 (+ square dice)))))



(defmethod initialize ((self geek-day))
  (unless (players self)
    (error "Please give me some player with make-instance!"))
  (setf (markers self)
        (mapcar (lambda (player) (cons player 0)) (players self)))
  (dolist (marker (markers self))
    ;; let's run a player.
    (let* ((player      (car marker))
           (square      (cdr marker))
           (square-data (aref (board self) square)))
      (landing self player square square-data))))


(defun play-geek-day (&rest player-names)
  (let ((game (make-instance 'geek-day
                  :players (mapcar (lambda (name)
                                     (make-instance 'player :name name))
                                   player-names))) )
    (declare (type geek-day game)) 
    (initialize game)
    (play game)))



(defun main (&rest args)
  "
DO:     Ask for the names of the players from the terminal
        and call PLAY-GEEK-DAY.
"
  (declare (ignore args))
  (format t "~24%+----------------------------------+~&~
                 |          G E E K - D A Y         |~&~
                 +----------------------------------+~&~
             ~4%~
             Please enter the names of the players, ~
             or an empty line to abort: ~&")
  (let ((names (split-sequence #\space (read-line) :remove-empty-subseqs t)))
    (when names
      (apply (function play-geek-day) names))))

;;;; THE END ;;;;
