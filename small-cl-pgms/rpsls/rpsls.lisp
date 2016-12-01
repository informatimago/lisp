;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rpsls.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements the Rock Paper Scissor Lizard Spock game against the computer.
;;;;
;;;;    http://www.umop.com/rps25.htm
;;;;    http://www.umop.com/rps101.htm
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-07-17 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2016
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
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.COMMON-LISP.ROCK-PAPER-SCISSOR-LIZARD-SPOCK"
  (:nicknames "RPSLS")
  (:use "COMMON-LISP")
  (:export "MAIN")
  (:documentation "

Implements the Rock Paper Scissor Lizard Spock game against the computer.

"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.ROCK-PAPER-SCISSOR-LIZARD-SPOCK")


(defun sup (line) (first line))
(defun how (line) (subseq line 1 (1- (length line))))
(defun sub (line) (first (last line)))


(defun graph-nodes (graph)
  (sort
   (remove-duplicates
    (loop
       :for line :in graph
       :collect (sup line)
       :collect (sub line)))
   (function string<=)))

(defun set-equal-p (a b)
  (and (subsetp a b) (subsetp b a)))


(defun check-graph (graph)
  (loop
     :with nodes = (graph-nodes graph)
     :with others = (1- (length nodes))
     :for sup :in nodes
     :for succs = (mapcar (function sub) (remove sup graph :key (function sup) :test-not (function eql)))
     :for prevs = (mapcar (function sup) (remove sup graph :key (function sub) :test-not (function eql)))
     :do (unless (= (length succs) (length prevs))
           (let ((missing (set-difference nodes (append (list sup) succs prevs))))
             (error "Node ~A has a different number of predecessors (~D) than successors (~D). ~@
                     ~@[Perhaps ~A ~:[is~;are~] missing an edge with ~A?~]"
                    sup (length prevs) (length succs)
                    missing (< 1 (length missing)) sup)))
     :do (unless (= (+ (length succs) (length prevs)) others)
           (error "Node ~A adjacency lists has the wrong length ~D, expected ~D"
                  sup (+ (length succs) (length prevs)) others))
     :do (unless (set-equal-p (append (list sup) succs prevs) nodes)
           (error "The set of predecessors and successors of ~A is not the whole nodes, but ~S."
                  sup (append (list sup) succs prevs))))
  :success)


(defun one-of (seq) (elt seq (random (length seq))))

(defun read-one-amongst (choices)
  (loop
     (format *query-io*
             "Please choose an item amongst: ~%  ~(~{~A~^ ~}~)~%> "
             choices)
     (let ((choice (with-standard-io-syntax
                     (let ((*package* #.*package*)
                           (*read-eval* nil))
                       (read *query-io*)))))
       (if (find choice choices)
           (return-from read-one-amongst choice)
           (format *query-io* "Bad choice, try again.~%")))))


(defun play (graph)
  (let ((nodes (graph-nodes graph)))
    (loop
       :with user-wins = 0
       :with computer-wins = 0
       :for games :from 1
       :for user-choice = (read-one-amongst (cons 'quit nodes))
       :until (eql user-choice 'quit)
       :do (let* ((computer-choice (one-of nodes))
                  (relation (find-if (lambda (rel)
                                       (or (and (eql user-choice     (sup rel))
                                                (eql computer-choice (sub rel)))
                                           (and (eql computer-choice (sup rel))
                                                (eql user-choice     (sub rel)))))
                                     graph))
                  (winp (eql user-choice (sup relation))))
             (when winp (incf user-wins))
             (when (eql computer-choice (sup relation)) (incf computer-wins))
             (if (eql user-choice computer-choice)
                 (format t "You chose ~A.~%Computer chose ~A.~%This is a draw.~2%"
                         user-choice computer-choice)
                 (format t "You chose ~A.~%Computer chose ~A.~%~{~:(~A~)~( ~{~A ~}~A~)~}.~%Therefore you ~:[lose~;win~].~2%"
                         user-choice computer-choice
                         (list (sup relation) (how relation) (sub relation))
                         winp)))
       :finally (progn
                  (format t "You      won ~A of ~A games.~%" user-wins games)
                  (format t "Computer won ~A of ~A games.~%" computer-wins games)
                  (return (values user-wins
                                  computer-wins
                                  (- games user-wins computer-wins)
                                  games))))))




(defparameter *rock-paper-scissors-graph*
  '(
    (paper covers rock)
    (rock crushes scissors)
    (scissors cuts paper))
  "Standard Rock-Paper-Scissors")


(defparameter *rock-paper-scissors-lizard-spock-graph*
  '(
    (lizard eats paper)
    (lizard poisons spock)
    (paper covers rock)
    (paper disproves spock)
    (rock crushes lizard)
    (rock crushes scissors)
    (scissors cuts paper)
    (scissors decapitates lizard)
    (spock smashes scissors)
    (spock vaporises rock)
    )
  "Geeky Rock Paper Scissors Lizard Spock.")


(defparameter *rock-paper-scissors-gun-dynamite-nuke-lightning-devil-dragon-alien-water-bowl-air-moon-sponge-wolf-cockroach-tree-man-woman-monkey-snake-axe-fire-sun*
  '(
    (gun targets rock)
    (gun shoots at sun)
    (gun guns fire)
    (gun destroy scissors)
    (gun chips axe)
    (gun shoots snake)
    (gun shoots monkey)
    (gun shoots woman)
    (gun shoots man)
    (gun targets tree)
    (gun shoots cockroach)
    (gun shoots wolf)

    (dynamite outclasses gun)
    (dynamite explodes rock)
    (dynamite smoke blots out sun)
    (dynamite starts fire)
    (dynamite explodes scissors)
    (dynamite explodes axe)
    (dynamite explodes snake)
    (dynamite explodes monkey)
    (dynamite explodes woman)
    (dynamite explodes man)
    (dynamite explodes tree)
    (dynamite explodes cockroach)

    (nuke outclasses dynamite)
    (nuke outclasses gun)
    (nuke incinerates rock)
    (nuke has power of sun)
    (nuke starts massive fire)
    (nuke incinerates scissors)
    (nuke incinerates snake)
    (nuke incinerates axe)
    (nuke incinerates monkey)
    (nuke incinerates woman)
    (nuke incinerates man)
    (nuke incinerates tree)

    (lightning defuses nuke)
    (lightning ignites dynamite)
    (lightning melts gun)
    (lightning splits rock)
    (lightning storm blocks sun)
    (lightning starts fire)
    (lightning melts scissors)
    (lightning melts axe)
    (lightning strikes snake)
    (lightning strikes monkey)
    (lightning strikes woman)
    (lightning strikes man)

    (devil casts lightning)
    (devil inspires nuke)
    (devil inspires dynamite)
    (devil inspires gun)
    (devil hurls rock)
    (devil curses sun)
    (devil breathres fire)
    (devil is immune to scissors)
    (devil is immune to axe)
    (devil eats snake)
    (devil enrages monkey)
    (devil tempts woman)

    (dragon commands devil)
    (dragon breathes lightning)
    (dragon lived before nuke)
    (dragon flosses with dynamite)
    (dragon is immune to gun)
    (dragon rests upon rock)
    (dragon blots out sun)
    (dragon breathes fire)
    (dragon is immune to scissors)
    (dragon is immune to axe)
    (dragon spawns snake)
    (dragon chars monkey)

    (alien varporizes dragon)
    (alien does not believe in devil)
    (alien shoots lightning)
    (alien defuses nuke)
    (alien defuses dynamite)
    (alien force-fields gun)
    (alien varporize rock)
    (alien destroys sun)
    (alien fuses fire)
    (alien force-fields scissors)
    (alien force-fields axe)
    (alien mutates snake)

    (water is toxic to alien)
    (water drowns dragon)
    (water blesses devil)
    (water conducts lightning)
    (water short-circuits nuke)
    (water douses dynamite)
    (water rusts gun)
    (water erodes rock)
    (water reflects sun)
    (water puts out fire)
    (water rusts scissors)
    (water rusts axe)

    (bowl contains water)
    (bowl shapes craft of alien)
    (bowl drowns dragon)
    (bowl blesses devil)
    (bowl focuses lightning)
    (bowl encases core of nuke)
    (bowl splashes dynamite)
    (bowl splashes gun)
    (bowl once made of rock)
    (bowl focuses sun)
    (bowl snuffs out fire)
    (bowl covers scissors)

    (air tips over bowl)
    (air evaporates water)
    (air chokes alien)
    (air freezes dragon)
    (air chokes devil)
    (air creates lightning)
    (air blows astray nuke)
    (air blows out dynamite)
    (air tarnishes gun)
    (air erodes rock)
    (air cools heat of sun)
    (air blows out fire)

    (moon has no air)
    (moon is shaped like bowl)
    (moon has no water)
    (moon houses alien)
    (moon shines on dragon)
    (moon terrifies devil)
    (moon is far above lightning)
    (moon is too far for nuke)
    (moon sufficates dynamite)
    (moon shines on gun)
    (moon shines on rock)
    (moon eclipses sun)

    (paper papers moon)
    (paper fans air)
    (paper mache bowl)
    (paper floats on water)
    (paper disproves alien)
    (paper rebukes dragon)
    (paper rebukes devil)
    (paper defines lightning)
    (paper defines nuke)
    (paper encases dynamite)
    (paper outlaws gun)
    (paper covers rock)

    (sponge soaks paper)
    (sponge looks like moon)
    (sponge uses pockets like air)
    (sponge cleans bowl)
    (sponge absorbs water)
    (sponge intrigues alien)
    (sponge cleanses dragon)
    (sponge cleanses devil)
    (sponge cleanses conducts lightning)
    (sponge cleans nuke)
    (sponge soaks dynamite)
    (sponge cleans gun)

    (wolf chews up sponge)
    (wolf chews up paper)
    (wolf howls at moon)
    (wolf breathes air)
    (wolf drinks from bowl)
    (wolf drinks water)
    (wolf chases alien)
    (wolf outruns dragon)
    (wolf bites heiny of devil)
    (wolf outruns lightning)
    (wolf "WOLF-2" launches nuke)
    (wolf outruns dynamite)

    (cockroach sleeps in fur of wolf)
    (cockroach nests in sponge)
    (cockroach nests between paper)
    (cockroach is nocturnal with moon)
    (cockroach breathes air)
    (cockroach hides under bowl)
    (cockroach drinks water)
    (cockroach stows away with alien)
    (cockroach eats eggs of dragon)
    (cockroach makes man devil)
    (cockroach hides from lightning)
    (cockroach survives nuke)

    (tree shelters cockroach)
    (tree shelters wolf)
    (tree outlives sponge)
    (tree creates paper)
    (tree blocks moon)
    (tree produces air)
    (tree wood creates bowl)
    (tree drinks water)
    (tree ensnares ship of alien)
    (tree shelters dragon)
    (tree imprisons devil)
    (tree attrackts lightning)

    (man plants tree)
    (man steps on cockroach)
    (man tames wolf)
    (man cleans with sponge)
    (man writes paper)
    (man travels to moon)
    (man breathes air)
    (man eats from bowl)
    (man drinks water)
    (man disproves alien)
    (man slays dragon)
    (man exorcises devil)

    (woman temps man)
    (woman plants tree)
    (woman steps on cockroach)
    (woman tames wolf)
    (woman cleans with sponge)
    (woman writes paper)
    (woman aligns with moon)
    (woman breathes air)
    (woman eats from bowl)
    (woman drinks water)
    (woman disproves alien)
    (woman subdues dragon)

    (monkey flings poop at woman)
    (monkey flings poop at man)
    (monkey lives in tree)
    (monkey eats cockroach)
    (monkey enrages wolf)
    (monkey rips up sponge)
    (monkey rips up paper)
    (monkey screeches at moon)
    (monkey breathes air)
    (monkey smashes bowl)
    (monkey drinks water)
    (monkey infuriates alien)

    (snake bites monkey)
    (snake bites woman)
    (snake bites man)
    (snake lives in tree)
    (snake eats cockroach)
    (snake bites wolf)
    (snake swallows sponge)
    (snake nests in paper)
    (snake is nocturnal with moon)
    (snake breathres air)
    (snake sleeps in bowl)
    (snake drinks water)

    (axe chops snake)
    (axe cleaves monkey)
    (axe cleaves woman)
    (axe cleaves man)
    (axe chops down tree)
    (axe chops cockroach)
    (axe cleaves wolf)
    (axe chops sponge)
    (axe slices paper)
    (axe reflects moon)
    (axe flies through air)
    (axe chops bowl)

    (scissors is sharper than axe)
    (scissors stabs snake)
    (scissors stabs monkey)
    (scissors cuts hair of woman)
    (scissors cuts hair of man)
    (scissors carves tree)
    (scissors stabs cockroach)
    (scissors cuts hair of wolf)
    (scissors cuts up sponge)
    (scissors cuts paper)
    (scissors reflects moon)
    (scissors swishes through air)

    (fire melts scissors)
    (fire forges axe)
    (fire burns snake)
    (fire burns monkey)
    (fire burns woman)
    (fire burns man)
    (fire burns down tree)
    (fire burns cockroach)
    (fire burns wolf)
    (fire burns sponge)
    (fire burns paper)
    (fire heaths campfire by light of moon)

    (sun is hotter than fire)
    (sun melts scissors)
    (sun melts axe)
    (sun warms snake)
    (sun warms monkey)
    (sun warms woman)
    (sun warms man)
    (sun feeds tree)
    (sun warms cockroach)
    (sun warms wolf)
    (sun dries up sponge)
    (sun shines through paper)

    (rock shareds sun)
    (rock pounds out fire)
    (rock smashes scissors)
    (rock chips axe)
    (rock crushes snake)
    (rock crushes monkey)
    (rock crushes woman)
    (rock crushes man)
    (rock blocks roots of tree)
    (rock squishes cockroach)
    (rock crushes wolf)
    (rock crushes sponge)
    )
  "Hyper-Geeky Rock, Paper, Scissors, Gun, Dynamite, Nuke, Lightning,
Devil, Dragon, Alien, Water, Bowl, Air, Moon, Sponge, Wolf, Cockroach,
Tree, Man, Woman, Monkey, Snake, Axe, Fire, Sun.
<http://loscuatroojos.com/wp-content/uploads/2008/04/rock-paper-scissors.jpg>")


(defparameter *rps101*
  '(


    )
 "<http://www.umop.com/rps101.htm>")


(defparameter *games*
  (list (cons "Rock-Paper-Scissors"
              *rock-paper-scissors-graph*)
        (cons "Rock Paper Scissors Lizard Spock"
              *rock-paper-scissors-lizard-spock-graph*)
        (cons "Rock, Paper, Scissors, Gun, Dynamite, Nuke, Lightning, Devil, Dragon, Alien, Water, Bowl, Air, Moon, Sponge, Wolf, Cockroach, Tree, Man, Woman, Monkey, Snake, Axe, Fire, Sun"
               *rock-paper-scissors-gun-dynamite-nuke-lightning-devil-dragon-alien-water-bowl-air-moon-sponge-wolf-cockroach-tree-man-woman-monkey-snake-axe-fire-sun*)))


(defun choose-game (games)
  (loop
     (format *query-io* "Please choose a game: ~%")
     (format *query-io* "~2D) ~A~%" 0 "quit")
     (loop
        :for i :from 1
        :for (name . game) :in *games*
        :do (format *query-io* "~2D) ~A~%" i name))
     (format *query-io* "Game index? ")
     (let ((choice (with-standard-io-syntax
                     (let ((*package* #.*package*)
                           (*read-eval* nil))
                       (read *query-io*)))))
       (cond
         ((zerop choice)
          (throw 'quit nil))
         ((elt games (1- choice))
          (return-from choose-game (cdr (elt games (1- choice)))))))))


(defun main (&optional game)
  (map nil (lambda (game) (check-graph (cdr game))) *games*)
  (catch 'quit
    (play (etypecase game
            (null    (choose-game *games*))
            (integer (if (<= 0 game (1- (length *games*)))
                         (elt *games* game)
                         (error "Game index out of bounds [0,~D]" (1- (length *games*)))))
            (symbol  (if (and (boundp game)
                              (listp (symbol-value game))
                              (check-graph (symbol-value game)))
                         (symbol-value game)
                         (error "Invalid game name ~S" game)))
            (list    (if (check-graph game)
                         game
                         (error "Invalid game ~S" game)))))))

(with-standard-io-syntax
  (print `(main)))

;;;; THE END ;;;;
