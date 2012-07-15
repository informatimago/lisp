;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-c-syntax.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Some tests.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-02 <PJB> Extracted from c-syntax.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(cl:in-package "COM.INFORMATIMAGO.LINC.C")

;; (c-sexp (stmt-for (assign 'a 1) (expr-le 'a 10) (expr-postincr 'a) (expr-call 'printf 'a)))

;; (generate
;;  (stmt-block
;;   (list
;;    (stmt-for (assign 'a 1) (expr-le 'a 10) (expr-postincr 'a)
;;              (stmt-block
;;               (list
;;                (stmt-expr (expr-call 'do-something-with 'a))
;;                (stmt-expr (expr-call 'do-something-with-with 'a 'b)))))
;;    (stmt-expr (expr-call 'printf "Hello world %s\\n" 'a))
;;    (stmt-for (assign 'a 1) (expr-le 'a 10) (expr-postincr 'a)
;;              (stmt-expr (expr-call 'printf 'a))))))
;; 
;; (generate
;;  (stmt-block
;;   (list
;;    (stmt-for (assign 'a 1) (expr-le 'a 10) (expr-postincr 'a)
;;              (stmt-block
;;               (list
;;                (expr-call 'printf 'a)
;;                (expr-call 'printf 'b))))
;;    (stmt-for (assign 'a 1) (expr-le 'a 10) (expr-postincr 'a))
;;    (expr-call 'printf "Hello world %s\\n" 'a)
;;    (stmt-for (assign 'a 1) (expr-le 'a 10) (expr-postincr 'a)
;;              (expr-call 'printf 'a)))))
;; 

(generate (stmt-block
           (list
            (extern "C"
                    (list (asm "move.l d0,d1")
                          (asm "move.l d2,d2")))
            (pointer
             (pointer
              (member-pointer
               'Some-Class
               (reference (c-vector 
                           (c-function 'printf (list 'fmt 'data) :throw '()) 10))
               :volatile t)
              :const t :volatile nil))
            (stmt-do (expr-call 'printf "%d\\n" (expr-postincr 'i))
                     (expr-lt 'i 10))
            (stmt-return 0))))


;; (pointer
;;  (pointer
;;   (member-pointer Configuration::ManagerConfig::AipcConfiguration
;;                   (reference (vector ppmrv 10))
;;                   :volatile t)
;;   :const t :volatile nil))


;; (in-package :cl-user) (delete-package :linc) (load "loader.lisp") (in-package :linc)
