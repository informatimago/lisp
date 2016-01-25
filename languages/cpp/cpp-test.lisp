;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cpp-test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-07-06 <PJB> Extracted  from cpp.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2016
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
(in-package "COM.INFORMATIMAGO.LANGUAGES.CPP")

(define-test test/substitute-trigraphs ()
  (check equal (substitute-trigraphs (make-numbered-line :text (copy-seq "hello world")))
          '("hello world" 1 "-"))
  (check equal (substitute-trigraphs (make-numbered-line :text (copy-seq "??( brackets ??)???<braces??>??=??'a??!??-; (??\\?) (a==b???-c:'??/n')")))
          '("[ brackets ]?{braces}#^a|~; (??\\?) (a==b?~c:'\\n')" 1 "-"))
  :success)

(define-test test/number-lines ()
  (check equal (number-lines '("a" "b" "c") "file.c" :start 2)
          '(("a" 2 #1="file.c") ("b" 3 #1#) ("c" 4 #1#)))
  :success)

(define-test test/merge-continued-lines ()
  (check equal (merge-continued-lines '(("first line" 1 #1="file.c")
                                         ("abc\\" 2 #1#)
                                         ("def\\" 3 #1#)
                                         ("ghi" 4 #1#)
                                         ("middle line" 5 #1#)
                                         ("abc \\" 6 #1#)
                                         ("def \\  " 7 #1#)
                                         ("ghi" 8 #1#)
                                         ("last line" 9 #1#))
                                       :warn-spaces-in-continued-lines nil)
          '(("first line" 1 #2="file.c") ("abcdefghi" 2 #2#) ("middle line" 5 #2#) ("abc def ghi" 6 #2#) ("last line" 9 #2#)))
  :success)

(define-test test/remove-comments-in-line ()
  (flet ((try (text &optional (state :top) comment-start)
           (multiple-value-list (remove-comments-in-line comment-start
                                                         (make-numbered-line :text text
                                                                             :lino 42 :file "hw.c")
                                                         state t))))
    (check equal (try "Hello world")
            '(("Hello world" 42 "hw.c") :top))
    ;; comment in string
    (check equal (try "Hello \"salut /* le */ monde\" world")
           '(("Hello \"salut /* le */ monde\" world" 42 "hw.c") :top))
    (check equal (try "Hello \"salut \\\"/* le */\\\" monde\" world")
           '(("Hello \"salut \\\"/* le */\\\" monde\" world" 42 "hw.c") :top))
    (assert-true (typep (nth-value 1 (ignore-errors (try "Hello \"salut"))) 'error))
    (assert-true (typep (nth-value 1 (ignore-errors (try "Hello \"salut\\"))) 'error))
    ;; comment in characters
    (check equal (try "Hello 'salut /* le */ monde' world")
           '(("Hello 'salut /* le */ monde' world" 42 "hw.c") :top))
    (check equal (try "Hello 'salut \\'/* le */\\' monde' world")
           '(("Hello 'salut \\'/* le */\\' monde' world" 42 "hw.c") :top))
    (assert-true (typep (nth-value 1 (ignore-errors (try "Hello 'salut"))) 'error))
    (assert-true (typep (nth-value 1 (ignore-errors (try "Hello 'salut\\"))) 'error))
    ;; single line comment
    (check equal (try "Hello//monde*/world")   '(("Hello" 42 "hw.c") :top))
    ;; monoline block comment
    (check equal (try "Hello/*monde*/world")   '(("Hello world" 42 "hw.c") :top))
    (check equal (try "Hello/*mon//de*/world") '(("Hello world" 42 "hw.c") :top))
    (check equal (try "Hello/*mon/*de*/world") '(("Hello world" 42 "hw.c") :top))
    ;; multiline block comment first line
    (check equal (try "Hello world/*salut") '(("Hello world" 42 "hw.c") :in-multiline-comment))
    (check equal (try "Hello/*monde*/world/*salut") '(("Hello world" 42 "hw.c") :in-multiline-comment))
    ;; multiline block comment in the middle lines
    (check equal (try "in the middle comment" :in-multiline-comment '("Hello world" 42 "hw.c"))
           '(("Hello world" 42 "hw.c") :in-multiline-comment))
    (check equal (try "in /* the // middle comment" :in-multiline-comment '("Hello world" 42 "hw.c"))
           '(("Hello world" 42 "hw.c") :in-multiline-comment))
    ;; multiline block comment in the end line
    (check equal (try "end comment */end line" :in-multiline-comment '("Hello world" 42 "hw.c"))
           '(("Hello world end line" 42 "hw.c") :top))
    (check equal (try "end comment */end/*fin*/line" :in-multiline-comment '("Hello world" 42 "hw.c"))
           '(("Hello world end line" 42 "hw.c") :top))
    (check equal (try "end // comment */end/*fin*/line" :in-multiline-comment '("Hello world" 42 "hw.c"))
           '(("Hello world end line" 42 "hw.c") :top))
    (check equal (try "end // comment */end/*fin*/line// c'est fini" :in-multiline-comment '("Hello world" 42 "hw.c"))
           '(("Hello world end line" 42 "hw.c") :top))
    :success))

(define-test test/remove-comments ()
  (check equal (remove-comments '(("Line one" 1 #1="test.c")
                                  ("Line/**/two" 2 #1#)
                                  ("Line/*three*/3" 3 #1#)
                                  ("Line/*4*/four/*and*/some" 4 #1#)
                                  ("Line/*-*/five/*a//nd*/some" 5 #1#)
                                  ("Line/*/five/*a//nd some" 6 #1#)
                                  ("Line//6 */seven" 7 #1#)
                                  ("Line/*/eight/*a//nd some" 8 #1#)
                                  ("Line nine" 9 #1#)
                                  ("Line//9 */ten" 10 #1#)
                                  ("Line \"ele/*v*/en\"--" 11 #1#)
                                  ("Line 'tw//elv/*e*/'--" 12 #1#)))
         '(("Line one" 1 #2="test.c")
           ("Line two" 2 #2#)
           ("Line 3" 3 #2#)
           ("Line four some" 4 #2#)
           ("Line five some" 5 #2#)
           ("Line seven" 6 #2#)
           ("Line ten" 8 #2#)
           ("Line \"ele/*v*/en\"--" 11 #2#)
           ("Line 'tw//elv/*e*/'--" 12 #2#)))
  :success)

(define-test test/scan-identifier ()
  (check equal (multiple-value-list (scan-identifier '("   Hello world " 42 "t.c") 3 "_$" :accept-unicode-escapes t))
         '("Hello" 8))
  (check equal (multiple-value-list (scan-identifier '("   Hello _wo$rl42d "  42 "t.c") 9 "_$" :accept-unicode-escapes t))
         '("_wo$rl42d" 18))
  (check equal (multiple-value-list (scan-identifier '("   Hello _\\u0145teve "  42 "t.c") 9 "_$" :accept-unicode-escapes t))
         '("_\\u0145teve" 20))
  (check equal (multiple-value-list (scan-identifier '("   Hello _\\U0145BABEteve "  42 "t.c") 9 "_$" :accept-unicode-escapes t))
         '("_\\U0145BABEteve" 24))
  (check equal (multiple-value-list (scan-identifier '("   Hello _world\\u014 "  42 "t.c") 9 "_$" :accept-unicode-escapes t))
         '("_world" 15))
  (check equal (multiple-value-list (scan-identifier '("   Hello _world\\U0145BABXteve " 42 "t.c") 9 "_$" :accept-unicode-escapes t))
         '("_world" 15))
  :success)

(define-test test/scan-number ()
  (check equal (multiple-value-list (scan-number '("   123 " 42 "t.c") 3))
         '("123" 6))
  (check equal (multiple-value-list (scan-number '("   0xBABE42 _wo$rl42d " 42 "t.c") 3))
         '("0xBABE42" 11))
  (check equal (multiple-value-list (scan-number '("   0xBABE42+42 " 42 "t.c") 3))
         '("0xBABE42" 11))
  (check equal (multiple-value-list (scan-number '("   0xE+12/42 " 42 "t.c") 3))
         '("0xE+12" 9))
  (check equal (multiple-value-list (scan-number '("   0.123_4e-56*32 " 42 "t.c") 3))
         '("0.123_4e-56" 14))
  (check equal (multiple-value-list (scan-number '("   .9999+.1111 " 42 "t.c") 3))
         '(".9999" 8))
  :success)

(define-test test/scan-punctuation ()
  (check equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 0))
         '("{" 1))
  (check equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 1))
         '("&=" 3))
  (check equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 3))
         '("==" 5))
  (check equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 5))
         '(">>=" 8))
  (check equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 8))
         '("=" 9))
  (check equal (multiple-value-list (scan-punctuation '("{&===>>==..." 42 "test.c") 9))
         '("..." 12))
  :success)

(define-test test/scan-delimited-literal ()
  (check equal (multiple-value-list (scan-delimited-literal '("'e'" 42 "test.c") 0))
         '("'e'" 3))
  (check equal (multiple-value-list (scan-delimited-literal '("'\\x41'" 42 "test.c") 0))
         '("'\\x41'" 6))
  :success)

(define-test text/skip-spaces ()
  (check equal (skip-spaces-in-text "    xyz()" 0) 4)
  (check equal (skip-spaces-in-text "    xyz()" 7) 7)
  (check equal (skip-spaces-in-text "    xyz ()" 7) 8)
  (check equal (skip-spaces-but-one "    xyz()" 0) 3)
  (check equal (skip-spaces-but-one "    xyz()" 7) 7)
  (check equal (skip-spaces-but-one "    xyz ()" 7) 7)
  :success)

(define-test test/extract-path ()
  (check equal (multiple-value-list (extract-path "#include"
                                                   (list (make-instance 'string-literal-token
                                                                        :text "\"/usr/local/include/\\xe9t\\xe9.h\""))))
          '("/usr/local/include/été.h" :quote nil))
  (check equal (multiple-value-list (extract-path "#include"
                                                   (list (make-instance 'string-literal-token
                                                                        :text "</usr/local/include/\\xe9t\\xe9.h>"))))
          '("/usr/local/include/\\xe9t\\xe9.h" :bracket nil))
  (check equal (multiple-value-list (extract-path "#include"
                                                   (list (make-instance 'punctuation-token :text "<")
                                                         (make-instance 'string-literal-token
                                                                        :text "/usr/local/include")
                                                         (make-instance 'string-literal-token
                                                                        :text "/file.h")
                                                         (make-instance 'punctuation-token :text ">"))))
          '("/usr/local/include/file.h" :bracket nil))
  :success)

(define-test test/parse-f-m-call-arguments ()
  (let ((*context*       (make-instance 'context :base-file "test.c" :file "test.c"))
        (tokenized-lines (list (list (make-instance 'identifier-token :text "next_line"))))
        (after           (make-instance 'identifier-token :text "same_line"))
        (foo             (make-instance 'identifier-token :text "FOO"))
        (arg1            (make-instance 'identifier-token :text "arg1"))
        (arg2            (make-instance 'identifier-token :text "arg2"))
        (plus            (make-instance 'punctuation-token :text "+"))
        (minus           (make-instance 'punctuation-token :text "-"))
        (comma           (make-instance 'punctuation-token :text ","))
        (left            (make-instance 'punctuation-token :text "("))
        (right           (make-instance 'punctuation-token :text ")")))
    (check equal
           (multiple-value-list
            (parse-function-macro-call-arguments foo (list left right after)
                                                 tokenized-lines))
           (list '(())
                 (list after)
                 tokenized-lines))

    (check equal (multiple-value-list
                  (parse-function-macro-call-arguments foo (list left comma right after)
                                                       tokenized-lines))
           (list '(() ())
                 (list after)
                 tokenized-lines))

    (check equal (multiple-value-list
                  (parse-function-macro-call-arguments foo (list left arg1 right after)
                                                       tokenized-lines))
           `(((,arg1))
             (,after)
             ,tokenized-lines))
    
    (check equal (multiple-value-list
                  (parse-function-macro-call-arguments foo (list left arg1 comma arg2 right after)
                                                       tokenized-lines))
           `(((,arg1) (,arg2))
             (,after)
             ,tokenized-lines))
    
    (check equal (multiple-value-list
                  (parse-function-macro-call-arguments foo (list left arg1 comma right after)
                                                       tokenized-lines))
           `(((,arg1) ())
             (,after)
             ,tokenized-lines))
    
    (check equal (multiple-value-list
                  (parse-function-macro-call-arguments foo (list left arg1 plus arg2 comma arg1 minus arg2 right after)
                                                       tokenized-lines))
           `(((,arg1 ,plus ,arg2) (,arg1 ,minus ,arg2))
             (,after)
             ,tokenized-lines))
    
    (check equal (multiple-value-list
                  (parse-function-macro-call-arguments foo (list left arg1 left arg2 right comma arg1 minus arg2 right after)
                                                       tokenized-lines))
           `(((,arg1 ,left ,arg2 ,right) (,arg1 ,minus ,arg2))
             (,after)
             ,tokenized-lines))
    
    (check equal (multiple-value-list
                  (parse-function-macro-call-arguments foo (list left arg1 left  comma arg2  comma right
                                                                 comma arg1 minus arg2 right after)
                                                       tokenized-lines))
           `(((,arg1 ,left ,comma ,arg2 ,comma ,right) (,arg1 ,minus ,arg2))
             (,after)
             ,tokenized-lines)))
  :success)

(define-test test/all/cpp ()
  (test/read-c-string)
  (test/number-lines)
  (test/substitute-trigraphs)
  (test/merge-continued-lines)
  (test/remove-comments-in-line)
  (test/remove-comments)
  (test/scan-identifier)
  (test/scan-number)
  (test/scan-punctuation)
  (test/scan-delimited-literal)
  (text/skip-spaces)
  (test/extract-path)
  (test/parse-f-m-call-arguments)
  (test/character-value)
  (test/integer-value))

(defun test/all ()
  (test/all/cpp))

;;;; THE END ;;;;
