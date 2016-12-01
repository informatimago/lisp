(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
;; (in-package "COM.INFORMATIMAGO.LANGUAGES.C11.PARSER")

(defpackage "TEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.RDP"))
(in-package "TEST")

(defgrammar simple-ex
  :scanner t
  :terminals ((identifier "[A-Za-z][A-Za-z0-9]*")
              (integer    "[0-9]+"))
  :start expression
  :rules  ((--> expression
                (alt
                 (seq "+" integer)
                 (seq "-" integer)
                 (seq "*" integer)
                 (seq "/" integer)))))


(defgrammar ex
  :terminals ((identifier "[A-Za-z][A-Za-z0-9]*")
              (integer    "[0-9]+"))
  :start expression

  :rules ((--> expression
               (alt impl-seq-no-action
                    impl-seq-with-action
                    seq-no-action
                    seq-no-action-with-action
                    seq-with-action
                    seq-with-action-with-action
                    rep-no-action
                    rep-with-action
                    opt-no-action
                    opt-with-action
                    alt-no-action
                    alt-with-action))

          (--> impl-seq-no-action
               "impl-seq-no-action" identifier integer)

          (--> impl-seq-with-action
               "impl-seq-with-action" identifier integer
               :action `(impl-seq-with-action ,identifier ,integer))


          (--> seq-no-action
               (seq "seq-no-action" identifier integer))

          (--> seq-no-action-with-action
               (seq "seq-no-action-with-action" identifier integer
                    :action `(seq-no-action-with-action ,identifier ,integer)))

          (--> seq-with-action
               (seq "seq-with-action" identifier integer)
               :action $1)

          (--> seq-with-action-with-action
               (seq "seq-with-action-with-action" identifier integer
                    :action `(seq-with-action-with-action ,identifier ,integer))
               :action $1)


          (--> rep-no-action
               "rep-no-action" (rep identifier integer))

          (--> rep-with-action
               "rep-with-action" (rep identifier integer) ;  :action `(,identifier ,integer)
               :action `(rep-with-action ,@(reduce 'append $2)))


          (--> opt-no-action
               "opt-no-action" (opt identifier integer))
          (--> opt-with-action
               "opt-with-action" (opt identifier integer) ;  :action `(,identifier ,integer)
               :action `(opt-with-action ,@$2))


          (--> alt-no-action
               "alt-no-action" (alt "with" "without") identifier)

          (--> alt-with-action
               "alt-with-action" (alt (seq "with" :action 'with) (seq "without" :action 'without)) identifier)))





#-(and) (progn
          (parse-ex "impl-seq-no-action hello 42")
          (expression (impl-seq-no-action (|impl-seq-no-action| "impl-seq-no-action" 19) (identifier "hello" 25) (integer "42" 28)))

          (parse-ex "impl-seq-with-action hello 42")
          (expression (impl-seq-with-action (identifier "hello" 27) (integer "42" 30)))

          (parse-ex "seq-no-action hello 42")
          (expression (seq-no-action ((|seq-no-action| "seq-no-action" 14) (identifier "hello" 20) (integer "42" 23))))
          (parse-ex "seq-with-action hello 42")
          (expression ((|seq-with-action| "seq-with-action" 16) (identifier "hello" 22) (integer "42" 25)))

          (parse-ex "seq-no-action-with-action hello 42")
          (expression (seq-no-action-with-action (seq-no-action-with-action (identifier "hello" 32) (integer "42" 35))))
          (parse-ex "seq-with-action-with-action hello 42")
          (expression (seq-with-action-with-action (identifier "hello" 34) (integer "42" 37)))


          (list (parse-ex "rep-no-action")
                (parse-ex "rep-no-action hello 42")
                (parse-ex "rep-no-action hello 42 world 33"))
          ((expression (rep-no-action (|rep-no-action| "rep-no-action" 14) nil))
           (expression (rep-no-action (|rep-no-action| "rep-no-action" 14) (((identifier "hello" 20) (integer "42" 23)))))
           (expression (rep-no-action (|rep-no-action| "rep-no-action" 14) (((identifier "hello" 20) (integer "42" 23)) ((identifier "world" 29) (integer "33" 32))))))

          (list (parse-ex "rep-with-action")
                (parse-ex "rep-with-action hello 42")
                (parse-ex "rep-with-action hello 42 world 33"))

          ((expression (rep-with-action))
           (expression (rep-with-action (identifier "hello" 22) (integer "42" 25)))
           (expression (rep-with-action (identifier "hello" 22) (integer "42" 25) (identifier "world" 31) (integer "33" 34))))

          (parse-ex "opt-no-action")
          (expression (opt-no-action (|opt-no-action| "opt-no-action" 14) nil))

          (parse-ex "opt-no-action whatisit 42")
          (expression (opt-no-action (|opt-no-action| "opt-no-action" 14) (((identifier "whatisit" 23) (integer "42" 26)))))

          (parse-ex "opt-with-action")
          (expression (opt-with-action (|opt-with-action| "opt-with-action" 16) nil))

          (parse-ex "opt-with-action whatisit 42")
          (expression (opt-with-action ((identifier "whatisit" 25) (integer "42" 28))))

          (list (parse-ex "alt-no-action with      panache")
                (parse-ex "alt-no-action without   panache"))
          ((expression (alt-no-action (|alt-no-action| "alt-no-action" 14) (|with| "with" 19) (identifier "panache" 32)))
           (expression (alt-no-action (|alt-no-action| "alt-no-action" 14) (|without| "without" 22) (identifier "panache" 32))))

          (list (parse-ex "alt-with-action with      panache")
                (parse-ex "alt-with-action without   panache"))
          ((expression (alt-with-action (|alt-with-action| "alt-with-action" 16) with (identifier "panache" 34)))
           (expression (alt-with-action (|alt-with-action| "alt-with-action" 16) without (identifier "panache" 34))))

          )
