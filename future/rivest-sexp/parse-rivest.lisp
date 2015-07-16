;; Path: news.easynet.es!numbering.news.easynet.net!spool2.bllon.news.easynet.net!easynet-quince!easynet.net!newsfeeds.sol.net!newspump.sol.net!news.glorb.com!newsfeed2.telusplanet.net!newsfeed.telus.net!edtnps84.POSTED!53ab2750!not-for-mail
;; From: Wade Humeniuk <whumeniu+anti+spam@telus.net>
;; User-Agent: Mozilla Thunderbird 1.0 (Windows/20041206)
;; X-Accept-Language: en-us, en
;; MIME-Version: 1.0
;; Newsgroups: comp.lang.lisp
;; Subject: Re: S-expression grammar?
;; References: <87y8a97u34.fsf@shadizar.dyndns.org> <S-expressions-20050520183401@ram.dialup.fu-berlin.de> <87ekc1tbxp.fsf@david-steuber.com> <S-Expressions-20050521125627@ram.dialup.fu-berlin.de>
;; In-Reply-To: <S-Expressions-20050521125627@ram.dialup.fu-berlin.de>
;; Content-Type: text/plain; charset=ISO-8859-1; format=flowed
;; Content-Transfer-Encoding: 7bit
;; Lines: 69
;; Message-ID: <UyIje.4658$HI.1721@edtnps84>
;; Date: Sat, 21 May 2005 15:35:16 GMT
;; NNTP-Posting-Host: 142.59.106.101
;; X-Trace: edtnps84 1116689716 142.59.106.101 (Sat, 21 May 2005 09:35:16 MDT)
;; NNTP-Posting-Date: Sat, 21 May 2005 09:35:16 MDT
;; Xref: news.easynet.es comp.lang.lisp:87328
;; 
;; Stefan Ram wrote:
;; 
;; >   draft-rivest-sexp-00.txt introduces Base64-atom-literals, like
;; >   |YWJj|, which are not part of most Lisp-implementations,
;; >   AFAIK, and on the other hand, does not seem to include dotted
;; >   pairs.
;; > 
;; 
;; Speaking of which, here is a possible parser for Rivest's Canonical/Transport
;; sexprs (For LispWorks),
;; 
;; CL-USER 8 > (parse-rivest-sexp "(4:icon[12:image/bitmap]9:xxxxxxxxx)")
;; ("icon" (:DISPLAY-ENCODED "image/bitmap" #(120 120 120 120 120 120 120 120 120)))
;; NIL
;; 
;; CL-USER 9 > (parse-rivest-sexp "(7:subject(3:ref5:alice6:mother))")
;; ("subject" ("ref" "alice" "mother"))
;; NIL
;; 
;; Wade


(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "parsergen")
  (use-package :parsergen))

(defparser rivest-canonical-sexp-parser
  ((s-expression sexpr))
  ((sexpr string) $1)
  ((sexpr list) $1)
  ((string display simple-string)
   (list :display-encoded $1
         (map '(simple-array (unsigned-byte 8) (*)) #'char-int $2)))
  ((string simple-string) $1)
  ((display \[ simple-string \]) $2)
  ((simple-string raw) $1)
  ((elements sexpr) (list $1))
  ((elements sexpr elements) (cons $1 $2))
  ((list \( elements \)) $2))

(defun rivest-canonical-sexp-lexer (stream)
  (let ((c (read-char stream nil)))
    (cond
     ((null c) (values nil nil))
     ((member c '(#\space #\tab #\newline)) (error "No Whitespace
      Allowed in Rivest Canonical Form"))
     ((char= c #\() (values '\( c))
     ((char= c #\)) (values '\) c))
     ((char= c #\[) (values '\[ c))
     ((char= c #\]) (values '\] c))
     ((digit-char-p c)
      (let ((length (digit-char-p c)))
        (loop for c = (read-char stream) do
              (cond
               ((digit-char-p c)
                (setf length (+ (* 10 length) (digit-char-p c))))
               ((char= #\: c)
                (loop with string = (make-array length
                                                :element-type 'character)
                      for i from 0 below length
                      do (setf (aref string  i) (read-char stream))
                      finally (return-from rivest-canonical-sexp-lexer
                                (values 'raw string))))
               (t (error "Invalid Rivest Simple String")))))))))

(defun parse-rivest-sexp (string)
  (with-input-from-string (s string)
    (rivest-canonical-sexp-parser (lambda () (rivest-canonical-sexp-lexer s)))))

