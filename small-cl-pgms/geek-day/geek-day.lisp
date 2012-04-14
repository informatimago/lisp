;;****************************************************************************
;;FILE:              geek-day.lisp
;;LANGUAGE:          Common-Lisp
;;SYSTEM:            UNIX
;;USER-INTERFACE:    UNIX
;;DESCRIPTION
;;    This programs plays geek-day
;;    http://ars.userfriendly.org/cartoons/?id=20021215
;;USAGE
;;    
;;AUTHORS
;;    <PJB> Pascal J. Bourguignon
;;MODIFICATIONS
;;    2002-12-15 <PJB> Created.
;;BUGS
;;LEGAL
;;    Copyright Pascal J. Bourguignon 2002 - 2002
;;
;;    This script is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU  General Public
;;    License as published by the Free Software Foundation; either
;;    version 2 of the License, or (at your option) any later version.
;;
;;    This script is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;    General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public
;;    License along with this library; see the file COPYING.LIB.
;;    If not, write to the Free Software Foundation,
;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;****************************************************************************

(DEFINE-PACKAGE COM.INFORMATIMAGO.COMMON-LISP.GEEK-DAY
  (:NICKNAMES GEEK-DAY)
  (:DOCUMENTATION "This programs plays geek-day.
    http://ars.userfriendly.org/cartoons/?id=20021215")
  (:FROM COMMON-LISP :IMPORT :ALL)
  (:EXPORT
   PLAY-GEEK-DAY MAIN
   ;; low-level:
   PLAYER LOST UPDATE-SANITY UPDATE-POPULARITY HAVE-BREAKFAST
   HAVE-SHOWER CHOOSE-BACKWARD-OR-FORWARD RANDOMIZER SQUARE-LIST
   GEEK-DAY  INITIALIZE PLAY )
  );;COM.INFORMATIMAGO.COMMON-LISP.GEEK-DAY


(DEFGENERIC LOST (A))
(DEFGENERIC UPDATE-SANITY (A B))
(DEFGENERIC UPDATE-POPULARITY (A B))
(DEFGENERIC HAVE-BREAKFAST (A))
(DEFGENERIC HAVE-SHOWER (A))
(DEFGENERIC CHOOSE-BACKWARD-OR-FORWARD (A))
(DEFGENERIC PLAY (A))
(DEFGENERIC LANDING (A B C D))
(DEFGENERIC TAKE-OFF (A B C D))
(DEFGENERIC INITIALIZE (A))


(DEFCLASS PLAYER NIL
  (
   (NAME
    :INITFORM "Unnamed Player"
    :INITARG :NAME
    :ACCESSOR NAME
    :TYPE STRING
    :DOCUMENTATION "This player's name.")
   (POPULARITY
    :INITFORM 5
    :INITARG :POPULARITY
    :ACCESSOR POPULARITY
    :TYPE INTEGER
    :DOCUMENTATION "This player's popularity.")
   (SANITY
    :INITFORM 5
    :INITARG :SANITY
    :ACCESSOR SANITY
    :TYPE INTEGER
    :DOCUMENTATION "This player's sanity.")
   (HAVE-HAD-BREAKFAST
    :INITFORM NIL
    :INITARG :HAVE-HAD-BREAKFAST
    :ACCESSOR HAVE-HAD-BREAKFAST
    :TYPE (OR NIL T)
    :DOCUMENTATION "This player have had a breakfast.")
   (HAVE-HAD-SHOWER
    :INITFORM NIL
    :INITARG :HAVE-HAD-SHOWER
    :ACCESSOR HAVE-HAD-SHOWER
    :TYPE (OR NIL T)
    :DOCUMENTATION "This player have had a shower.")
   )
  (:DOCUMENTATION "A Player at Geek-Day."));;PLAYER


(DEFMETHOD LOST ((SELF PLAYER))
  (OR (<= (POPULARITY SELF) 0)
      (<= (SANITY     SELF) 0))
  );;LOST


(DEFMETHOD UPDATE-SANITY ((SELF PLAYER) INCREMENT)
  (SETF (SANITY SELF) (+ (SANITY SELF) INCREMENT))
  (FORMAT T "    Your sanity is ~[increased~;decreased~] by ~A.~%"
          (IF (< INCREMENT 0) 1 0) (ABS INCREMENT))
  (IF (<=  (SANITY SELF) 0)
      (FORMAT T "    YOU GO POSTAL AND LOSE!~%~%"))
  );;UPDATE-SANITY


(DEFMETHOD UPDATE-POPULARITY ((SELF PLAYER) INCREMENT)
  (SETF (POPULARITY SELF) (+ (POPULARITY SELF) INCREMENT))
  (FORMAT T "    Your popularity is ~[increased~;decreased~] by ~A.~%"
          (IF (< INCREMENT 0) 1 0) (ABS INCREMENT))
  (IF (<= (POPULARITY SELF) 0)
      (FORMAT T "    YOU'RE FIRED!~%~%"))
  );;UPDATE-POPULARITY


(DEFMETHOD HAVE-BREAKFAST ((SELF PLAYER))
  (SETF (HAVE-HAD-BREAKFAST SELF) T)
  );;HAVE-BREAKFAST


(DEFMETHOD HAVE-SHOWER ((SELF PLAYER))
  (SETF (HAVE-HAD-SHOWER SELF) T)
  );;HAVE-SHOWER


(DEFMETHOD CHOOSE-BACKWARD-OR-FORWARD ((SELF PLAYER))
  (FORMAT T "~%~A, please choose backward or forward? " (NAME SELF))
  (FINISH-OUTPUT *STANDARD-OUTPUT*)
  (DO ((REP (READ-LINE) (READ-LINE)))
      ((OR (STRING-EQUAL REP "backward")
           (STRING-EQUAL REP "forward"))
       (STRING-EQUAL REP "backward"))
    (FORMAT T "~%Becareful! Next time, pop-1!~&~A, ~
               please choose backward or forward? "
            (NAME SELF)))
  );;CHOOSE-BACKWARD-OR-FORWARD


  
(DEFVAR RANDOM-STATE (MAKE-RANDOM-STATE T))

(DEFUN RANDOMIZER ()
  (1+ (RANDOM 6 RANDOM-STATE))
  );;RANDOMIZER




;;("title" (before) (after))

(DEFVAR SQUARE-LIST
  '(
    ("Starting blocks."
     NIL
     (LAMBDA (SQUARE DICE PLAYER)
       (FORMAT T "~&    Therefore ~A."
               (IF (<= 5 DICE) "get out of bed" "keep sleeping"))
       (IF (<= 5 DICE)
           (+ DICE SQUARE)
         SQUARE)))

    ("Ungum your eyes."            ( (POPULARITY +1) ) NIL)
    ("Get a hot shower."           ( (POPULARITY +3)
                                     (LAMBDA (PLAYER)
                                       (HAVE-SHOWER PLAYER)) ) NIL)
    ("Depilate."                   ( (POPULARITY +1) ) NIL)
    ("Get a breakfast."            ( (SANITY +3)
                                     (LAMBDA (PLAYER)
                                       (HAVE-BREAKFAST PLAYER)) ) NIL)
    ("Catch the bus."              ( (SANITY -1) (POPULARITY -1) ) NIL)
    ("Oooh... Brain Engages."
     NIL
     (LAMBDA (SQUARE DICE PLAYER)
       (IF (CHOOSE-BACKWARD-OR-FORWARD PLAYER)
           (- SQUARE DICE)
         (+ SQUARE DICE))))
    ("Arrive \"late\"."            ( (POPULARITY +1) (SANITY -1) ) NIL)
    ("First problem of the day."   ( (POPULARITY -2) (SANITY -3) ) NIL)
    ("Called into meeting."        ( (SANITY -4) ) NIL)
    ("Boss in bad mood."           ( (POPULARITY -2) ) NIL)
    ("Water cooler break."         ( (LAMBDA (PLAYER)
                                       (UNLESS (HAVE-HAD-SHOWER PLAYER)
                                         (UPDATE-POPULARITY PLAYER -3))) ) NIL)
    ("Caffeine break!"             ( (SANITY +3) ) NIL)
    ("Caffeine break!"             ( (SANITY +3) ) NIL)
    ("Co-worker gives you the blame."
     ( (POPULARITY +2) ) NIL)
    ("Nap."                        ( (SANITY +4) (POPULARITY -2) ) NIL)
    ("Wooo! A thouht!"
     NIL
     (LAMBDA (SQUARE DICE PLAYER)
       (IF (CHOOSE-BACKWARD-OR-FORWARD PLAYER)
           (- SQUARE DICE)
         (+ SQUARE DICE))))
    ("Chair breaks."               ( (POPULARITY +2) (SANITY -1) ) NIL)
    ("Machine locks up."           ( (SANITY -2) ) NIL)
    ("Power outage."                   ( "It's okay, you're safe here." ) NIL)
    ("Munchies!"                   ( (LAMBDA (PLAYER)
                                       (UNLESS (HAVE-HAD-BREAKFAST PLAYER)
                                         (UPDATE-SANITY PLAYER -2))) ) NIL)
    ("Caffeine run."               ( (POPULARITY +1) ) NIL)
    ("Sugar run."                  ( (SANITY +1) ) NIL)
    ("Co-worker takes credit."     ( (SANITY -3) ) NIL)
    ("Run to the loo."             ( (SANITY +1) ) NIL)
    ("Collapse on the couch at home."
     ("It's over. Start again tomorrow.")
     (LAMBDA (SQUARE DICE PLAYER)
       0))
    ));;SQUARE-LIST


(DEFCLASS GEEK-DAY  NIL
  (
   (BOARD
    :INITFORM (MAKE-ARRAY (LENGTH SQUARE-LIST) :INITIAL-CONTENTS SQUARE-LIST)
    :INITARG :BOARD
    :ACCESSOR BOARD
    :DOCUMENTATION "The array of square compounding the game.")
   (PLAYERS
    :INITFORM NIL
    :INITARG :PLAYERS
    :ACCESSOR PLAYERS
    :DOCUMENTATION "The list of Player objects.")
   (MARKERS
    :INITFORM NIL
    :ACCESSOR MARKERS
    :DOCUMENTATION "A list of cons (player . square-index).")
   )
  (:DOCUMENTATION "A Geek-Day game."));;GEEK-DAY





(DEFMETHOD PLAY ((SELF GEEK-DAY))
  (DO ()
      ((NULL (MARKERS SELF)))
    ;; let's run a turn.
    (DOLIST (MARKER (MARKERS SELF))
      ;; let's run a player.
      (LET* ((PLAYER      (CAR MARKER))
             (SQUARE      (CDR MARKER))
             (SQUARE-DATA (AREF (BOARD SELF) SQUARE))
             )
        (SETF (CDR MARKER) (TAKE-OFF SELF PLAYER SQUARE SQUARE-DATA))
        (SETQ SQUARE       (CDR MARKER))
        (SETQ SQUARE-DATA  (AREF (BOARD SELF) SQUARE))
        (LANDING SELF PLAYER SQUARE SQUARE-DATA)
        )) ;;dolist
    (SETF (MARKERS SELF)
          (DELETE-IF (LAMBDA (ITEM) (LOST (CAR ITEM))) (MARKERS SELF)))
    (LET ((LINEFORM "~:(~20A~) ~16A ~16A ~16A~%"))
      (FORMAT T "~%~%")
      (FORMAT T LINEFORM  "--------------------"
              "----------------"  "----------------"  "----------------")
      (FORMAT T LINEFORM  "Name" "Popularity" "Sanity" "Square")
      (FORMAT T LINEFORM  "--------------------"
              "----------------"  "----------------"  "----------------")
      (DOLIST (PLAYER (PLAYERS SELF))
        (FORMAT T LINEFORM  (NAME PLAYER) (POPULARITY PLAYER) (SANITY PLAYER)
                (IF (LOST PLAYER) "Lost" (CDR (ASSOC PLAYER (MARKERS SELF)))))
        ) ;;dolist
      (FORMAT T LINEFORM  "--------------------"
              "----------------"  "----------------"  "----------------")
      ) ;;let
    ) ;;do
  );;PLAY



(DEFMACRO SQUARE-NAME (SQUARE-DATA) `(CAR   ,SQUARE-DATA))
(DEFMACRO SQUARE-IN   (SQUARE-DATA) `(CADR  ,SQUARE-DATA))
(DEFMACRO SQUARE-OUT  (SQUARE-DATA) `(CADDR ,SQUARE-DATA))


(DEFUN LAMBDAP (ITEM)
  (AND (CONSP ITEM) (EQ 'LAMBDA (CAR ITEM))));;LAMBDAP

(DEFMACRO LAMCALL (LAMBDA-EXPR &REST ARGUMENTS)
  `(FUNCALL  (COERCE ,LAMBDA-EXPR 'FUNCTION) ,@ARGUMENTS));;LAMCALL


(DEFMETHOD LANDING ((SELF GEEK-DAY) PLAYER SQUARE SQUARE-DATA)
  (DECLARE (IGNORE SQUARE))
  (FORMAT T "~%~%~:(~A~):  ~A~%"
          (NAME PLAYER) (SQUARE-NAME SQUARE-DATA))
  (DOLIST (ACTION (SQUARE-IN SQUARE-DATA))
    (COND
     ((NULL ACTION))
     ((STRINGP ACTION)              (FORMAT T "    ~A~%" ACTION) )
     ((LAMBDAP ACTION)              (LAMCALL ACTION PLAYER) )
     ((EQ 'POPULARITY (CAR ACTION)) (UPDATE-POPULARITY PLAYER (CADR ACTION)) )
     ((EQ 'SANITY (CAR ACTION))     (UPDATE-SANITY PLAYER (CADR ACTION)) )
     (T (ERROR "Invalid action in square-in ~W." ACTION)))
    );;dolist
  );;LANDING


(DEFMETHOD TAKE-OFF ((SELF GEEK-DAY) PLAYER SQUARE SQUARE-DATA)
  (LET ((DICE        (RANDOMIZER))
        (OUT         (SQUARE-OUT SQUARE-DATA))
        )
    (FORMAT T "~%~%~:(~A~), you roll and get ~A.~%" (NAME PLAYER) DICE)
    (MIN (1- (LENGTH (BOARD SELF)))
               (IF OUT (LAMCALL OUT SQUARE DICE PLAYER)
                 (+ SQUARE DICE)))
    ));;TAKE-OFF



(DEFMETHOD INITIALIZE ((SELF GEEK-DAY))
  (UNLESS (PLAYERS SELF)
    (ERROR "Please give me some player with make-instance!"))
  (SETF (MARKERS SELF)
        (MAPCAR (LAMBDA (PLAYER) (CONS PLAYER 0)) (PLAYERS SELF)))
  (DOLIST (MARKER (MARKERS SELF))
    ;; let's run a player.
    (LET* ((PLAYER      (CAR MARKER))
           (SQUARE      (CDR MARKER))
           (SQUARE-DATA (AREF (BOARD SELF) SQUARE))
           )
      (LANDING SELF PLAYER SQUARE SQUARE-DATA)
      )) ;;DOLIST
  );;INITIALIZE


(DEFUN PLAY-GEEK-DAY (&REST PLAYER-NAMES)
  (LET ((GAME (MAKE-INSTANCE 'GEEK-DAY
                  :PLAYERS (MAPCAR (LAMBDA (NAME)
                                     (MAKE-INSTANCE 'PLAYER :NAME NAME))
                                   PLAYER-NAMES))) )
    (DECLARE (TYPE GEEK-DAY GAME)) 
    (INITIALIZE GAME)
    (PLAY GAME))
  );;PLAY-GEEK-DAY



(DEFUN MAIN (&rest args)
  "
DO:     Ask for the names of the players from the terminal
        and call PLAY-GEEK-DAY.
"
  (declare (ignore args))
  (FORMAT T "~24%+----------------------------------+~&~
                 |          G E E K - D A Y         |~&~
                 +----------------------------------+~&~
             ~4%~
             Please enter the names of the players, ~
             or an empty line to abort: ~&")
  (LET* ((NAMES-STR (READ-LINE))
         (NAMES (READ-FROM-STRING (FORMAT NIL "(~A)" NAMES-STR) T '())))
    (WHEN NAMES
      (APPLY (FUNCTION PLAY-GEEK-DAY) NAMES))));;MAIN

;;;; geek-day.lisp                    --                     --          ;;;;
