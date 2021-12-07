(defparameter *combining-ranges*
  '((#x0300 #x036f diacritical-marks              (1.0 4.1))
    (#x1ab0 #x1aff diacritical-marks-extended     7.0)
    (#x1dc0 #x1dff diacritical-marks-supplement   (4.1 5.2))
    (#x20d0 #x20ff diacritical-marks-for-symbols  (1.0 5.1))
    (#xfe20 #xfe2f half-marks                     (1.0 8.0))
    (#x3099 dakuten)
    (#x309a handkuten)
    ;; etc other blocks contain combining codes
    ))

(defun combining-codepoint-p (codepoint)
  (loop
     :for (min max . nil) :in *combining-ranges*
     :thereis (if (integerp max)
                  (<= min codepoint max)
                  (= min codepoint))))


(deftype codepoint () `(integer 0 1114111))

(defclass unicode-character ()
  ((codepoints :initarg :codepoints
               :reader unicode-character-codepoints
               :type vector)))

(defmethod unicode-char= ((a unicode-character) (b unicode-character))
  (equalp (unicode-character-codepoints a)
          (unicode-character-codepoints b)))

(defclass unicode-string ()
  ((characters :initarg :characters
               :reader unicode-string-characters
               :type vector)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun list-of-codepoint (object)
    (and (consp object)
         (typep (car object) 'codepoint)
         (or (null (cdr object))
             (list-of-codepoint (cdr object))))
    (and (listp object)))

  (defun vector-of-codepoint (object)
    (and (vectorp object)
         (every (lambda (element)
                  (typep element 'codepoint))
                object)))
  
  (defun vector-of-unicode-character (object)
    (and (vectorp object)
         (every (lambda (element)
                  (typep element 'unicode-character))
                object))))

(deftype list-of   (element-type) `(and list   (satisfies ,(intern (format nil "LIST-OF-~A" element-type)))))
(deftype vector-of (element-type) `(and vector (satisfies ,(intern (format nil "VECTOR-OF-~A" element-type)))))



(defmethod print-object ((uchar unicode-character) stream)
  (when *print-readably*
    (format stream "#\\"))
  (format stream "~A" (map 'string 'code-char (unicode-character-codepoints uchar)))
  uchar)

(defun make-unicode-character (codepoint &rest codepoints)
  (let ((cpoints (if (consp codepoint)
                     (coerce codepoint 'vector)
                     (coerce (cons codepoint codepoints) 'vector))))
    (unless (every (lambda (codepoint) (typep codepoint 'codepoint)) cpoints)
      (error 'type-error :datum cpoints :expected-type 'vector-of-codepoint))
    (make-instance 'unicode-character :codepoints cpoints)))

(defmethod print-object ((ustring unicode-string) stream)
  (if *print-readably*
      (loop
         :with escape := (make-unicode-character (char-code #\\))
         :with dquote := (make-unicode-character (char-code #\"))
         :for uchar :across (unicode-string-characters ustring)
         :initially (format stream "\"")
         :if (or (unicode-char= escape uchar)
                 (unicode-char= dquote uchar))
         :do (format stream "\\")
         :end
         :do (princ uchar stream)
         :finally (format stream "\""))
      (loop :for uchar :across (unicode-string-characters ustring)
           :do (princ uchar stream)))
  ustring)

(defun codepoints-to-string (vector-of-codepoints)
  (loop
     :with characters := '()
     :with current-character := '()
     :for codepoint :across vector-of-codepoints
     :do (cond
           ((combining-codepoint-p codepoint)
            (push codepoint current-character))
           (current-character
            (push (make-unicode-character (nreverse current-character)) characters)
            (setf current-character (list codepoint)))
           (t
            (setf current-character (list codepoint))))
     :finally (when current-character
                (push (make-unicode-character (nreverse current-character)) characters))
       (return (make-instance 'unicode-string :characters (coerce (nreverse characters) 'vector)))))

(defmethod make-unicode-string ((initial-contents string))
  (codepoints-to-string (map 'vector 'char-code initial-contents)))

(make-unicode-character  (cons 97 (loop :for codepoint :from #x0300 :to #x036f :collect codepoint)))
;; --> à̴̵̶̷̸̡̢̧̨̛̖̗̘̙̜̝̞̟̠̣̤̥̦̩̪̫̬̭̮̯̰̱̲̳̹̺̻̼͇͈͉͍͎́̂̃̄̅̆̇̈̉̊̋̌̍̎̏̐̑̒̓̔̽̾̿̀́͂̓̈́͆͊͋͌̕̚ͅ͏͓͔͕͖͙͚͐͑͒͗͛ͣͤͥͦͧͨͩͪͫͬͭͮͯ͘͜͟͢͝͞͠͡


(defmethod unicode-length ((string unicode-string))
  (length (unicode-string-characters string)))

(defmethod unicode-subseq ((string unicode-string) start end)
  ;; O(- end start)
  (make-instance 'unicode-string
      :characters (subseq (unicode-string-characters string) start end)))

(make-instance 'unicode-string
    :characters (coerce (loop
                           :for ch :from 97 to (+ 97 25)
                           :collect (apply (function make-unicode-character)
                                           ch
                                           (loop :repeat (random 4)
                                              :collect (+ #x0300 (random (- #x036f #x0300)))))) 'vector))

;; ab̂̃͘cd͗e̋f̶g̋hͬi̥͉͎ǰ͚ͭḵ̏̈́l̳m̭ṉ̵̰o̮̳pq̻r̦͕̿ș̐͞t͓͝͡u̼̔͋v̞̝w̓xy̿̽zͣ
;; abc̝d̬͎͝e̠f̥̀͠g̭̫͐h̶i͚j͔̐ͅk̀l̨̄͡ṃ̂n̢̺o͛p̲̩q̿ͤ͜r͛ͫ͠s͜t̓͌uv͍w̬̙̃x̻̪y̸z
;; a͉̿̀b͌c̢̏d̟͙̺e̳ͭ͂fg͎̾̆hijk̈́ͫlm͇̮̂nͮͅǫ́p̹͐qr͖̝̔ṡ͂͑t̟̀u̜̲͝v̽wxy̳ͣz͉ͨ̅

(unicode-subseq
 (make-instance 'unicode-string
     :characters (coerce (loop
                            :for ch :from 97 to (+ 97 25)
                            :collect (apply (function make-unicode-character)
                                            ch
                                            (loop :repeat (random 4)
                                               :collect (+ #x0300 (random (- #x036f #x0300)))))) 'vector))
 5 15)

;; f̅gͥh̎î͍̻j͙̠̿ķ̢̏ḻ̉̽mn̞̜͘o̳͌̄
;; f̷̙̍g͍̻ͅȟ̜̰i͛͢jķ͇ḷ̂m̪̼̿n͝ȯ
;; f̀̒͆g̩h͇͑ị̕͜j̅k̬͆l̝̖ͦm͂͆n̤̅o

(rotate (length))
(apropos "ROTATE")

(defun nrotate-vector (n vector &optional (start 0) end)
  (let* ((end (or end (length vector)))
         (len (- end start))
         (n   (mod n len)))
    (when (plusp n)
      ;; 0     start    start+n  end        (length vector)
      ;; |-------|-----------|----|---------|
      (let ((temp (make-array n :element-type (array-element-type vector))))
        (replace temp vector   :start2 start :end2 (+ start n))
        (replace vector vector :start1 start :start2 (+ start n) :end2 end)
        (replace vector temp   :start1 (+ start (- len n)))))
    vector))

(defun test/nrotate-vector ()
  (assert (string= " World!Hello" 
                   (nrotate-vector 5 (copy-seq "Hello World!"))))
  (assert (string= "Hello ldWor!"
                   (nrotate-vector 3 (copy-seq "Hello World!") 6 11)))
  (assert (string= "Hello World!"
                   (nrotate-vector 0 (copy-seq "Hello World!") 3 10)))
  :success)

(nrotate-vector 1 "z͉ͨ̅" 1)
;; --> "z͉̅ͨ"

(let ((string  (make-unicode-string "hi z͉ͨ̅ cool")))
  (values (nrotate-vector 1 (unicode-character-codepoints (aref (unicode-string-characters string) 3)) 1)
          string))
;; --> #(122 841 773 872)
;;     "hi z͉̅ͨ cool" 


(let ((string (make-unicode-string "a͉̿̀b͌c̢̏d̟͙̺e̳ͭ͂fg͎̾̆hijk̈́ͫlm͇̮̂nͮͅǫ́p̹͐qr͖̝̔ṡ͂͑t̟̀u̜̲͝v̽wxy̳ͣz͉ͨ̅")))
  (values (unicode-length string)
          (copy-seq (unicode-string-characters string))
          (nrotate-vector 3 (unicode-string-characters string) 5 21)))
;; --> 26
;;     #(a͉̿̀ b͌ c̢̏ d̟͙̺ e̳ͭ͂ f g͎̾̆ h i j k̈́ͫ l m͇̮̂ nͮͅ ǫ́ p̹͐ q r͖̝̔ ṡ͂͑ t̟̀ u̜̲͝ v̽ w x y̳ͣ z͉ͨ̅)
;;     #(a͉̿̀ b͌ c̢̏ d̟͙̺ e̳ͭ͂ i j k̈́ͫ l m͇̮̂ nͮͅ ǫ́ p̹͐ q r͖̝̔ ṡ͂͑ t̟̀ u̜̲͝ f g͎̾̆ h v̽ w x y̳ͣ z͉ͨ̅)
(let ((string  "a͉̿̀b͌c̢̏d̟͙̺e̳ͭ͂fg͎̾̆hijk̈́ͫlm͇̮̂nͮͅǫ́p̹͐qr͖̝̔ṡ͂͑t̟̀u̜̲͝v̽wxy̳ͣz͉ͨ̅"))
  (values (length string)
          (copy-seq string)
          (nrotate-vector 3 string 5 21)))
;; --> 69
;;     "a͉̿̀b͌c̢̏d̟͙̺e̳ͭ͂fg͎̾̆hijk̈́ͫlm͇̮̂nͮͅǫ́p̹͐qr͖̝̔ṡ͂͑t̟̀u̜̲͝v̽wxy̳ͣz͉ͨ̅"
;;     "a͉̿̀b̏d̟͙̺e̳ͭ͂fg͎̾͌c̢̆hijk̈́ͫlm͇̮̂nͮͅǫ́p̹͐qr͖̝̔ṡ͂͑t̟̀u̜̲͝v̽wxy̳ͣz͉ͨ̅"
