(in-package :cliki)

;;; generalised topic searching

(defmethod search-term-relevance ((cliki cliki-view) page
				  (term (eql :or)) &rest args)
  (/ (apply #'+ (mapcar (lambda (x)
			  (apply #'search-term-relevance cliki page x))
			args)) 2))

(defmethod search-term-summary ((cliki cliki-view) page 
				  (term (eql :or)) &rest args)
  (loop for p in args
	if (> (apply #'search-term-relevance cliki page p) 0)
	append (apply #'search-term-summary cliki page  p)))

(defmethod search-term-relevance ((cliki cliki-view) page
				  (term (eql :and)) &rest args)
  (apply #'* (mapcar (lambda (x)
		       (apply #'search-term-relevance cliki page x))
		     args)))

(defmethod search-term-summary ((cliki cliki-view) page 
				  (term (eql :and)) &rest args)
  (if (> (apply #'search-term-relevance cliki page term args) 0)
      (loop for p in args
	    append (apply #'search-term-summary cliki page p))))

(defmethod search-term-relevance ((cliki cliki-view) page
				  (term (eql :not)) &rest args)
  (- 1 (apply #'search-term-relevance cliki page (car args))))

(defmethod search-term-summary ((cliki cliki-view) page 
				  (term (eql :not)) &rest args)
  nil)

(defmethod search-term-relevance ((cliki cliki-view) page
				  (term (eql :body)) &rest args)
  (let ((doc-terms (page-tfidf page))
	(terms (loop for word in (araneida:split (car args))
		     for stem = (stem-for-word word)
		     when (interesting-word-p stem)
		     collect (cons stem 1))))
    (if (and doc-terms terms)
	(document-vector-cosine terms doc-terms)
	0)))

(defmethod search-term-summary ((cliki cliki-view) page 
				(term (eql :body)) &rest args)
  (list
   (with-output-to-string (o)
     (let* ((e-stream (make-instance 'elided-stream
				     :important-words
				     (araneida:split (car args))
				     :output-stream o))
	    (h-stream (make-instance 'strip-html-stream
				     :output-stream  e-stream)))
       (labels ((dispatch (token arg)
		  (apply #'html-for-keyword
			 cliki h-stream (long-form-for token arg)))
		(output (c)
		  (write-char (if (graphic-char-p c) c #\Space) h-stream)))
	 (with-open-file (in-stream (page-pathname page) :direction :input)
	   (scan-stream (cliki-short-forms cliki)
			in-stream
			#'output #'dispatch))
	 (close h-stream)
	 (close e-stream))))))

;;; default method: for (foo "bar" "baz"), return 1 iff the search
;;; term is (foo "bar"), (foo "baz") or (foo)

(defmethod search-term-relevance ((cliki cliki-view) page
				  term &rest args)
  (if
   (cond (args
	  (member (car args) (cadr (assoc term (page-indices page)))
		  :test (lambda (x y)
			  (string-equal (princ-to-string x)
					(princ-to-string y)))))
	 (t (assoc term (page-indices page))))
   1 0))

(defmethod search-term-summary ((cliki cliki-view) page 
				  term &rest args)
  (if (> (apply #'search-term-relevance cliki page term args) 0)
      (list (format nil "<b>~A</b>: ~A<br>"
		    term (car args)))))

(defmethod search-results-blurb ((cliki cliki-view) stream)
  nil)


;;; old search for stuff.  This is the /(...) searching: the full-text
;;; search is in index.lisp

;;; XXX this shows all the signs of wanting to be a
;;; multiply-dispatched method on (attribute match)

;;; XXX is there a standard term for :case-sensitive in CLHS?

;;; TODO syntax errors and so forth should throw something
;;; standardised which write-stream-to-stream can catch (so fix search-error)

(defun legacy-search-pages
    (cliki term &key (attribute :body) (match :substring)
     (case-sensitive nil))
  "Search pages in PATHNAME for TERM according to the criteria in the keyword
arguments.  Returns a list of pages.
ATTRIBUTE is (or :title :topic :body)
MATCH is (or :exact :substring :regular-expression)
CASE-SENSITIVE is (or t nil)"
  (sort 
   (case attribute
     (:body (search-page-bodies cliki term match case-sensitive))
     (:title (search-page-titles cliki term match case-sensitive))
     (:topic (search-page-topics cliki term match case-sensitive))
     (t (search-error "Unknown search attribute")))
   #'string-lessp :key #'page-title))


(defun search-error (&rest args) (apply #'error args))

(defun search-predicate (match case-p)
  (cond
   ((and (eql match :substring) case-p)
    (lambda (term x) (search term x)))
   ((eql match :substring) 
    (lambda (term x) (search term x :test #'char-equal)))
   ((and (eql match :exact) case-p)
    #'string=)
   ((eql match :exact)
    #'string-equal)
   (t (search-error "Unknown search match criterion"))))

(defun search-page-titles (cliki term match case-sensitive)
  (let ((pred (search-predicate match case-sensitive)))
    (loop for page being the hash-values of (cliki-pages cliki)
	  if (funcall pred term (page-title page))
	  collect page)))

(defun search-page-bodies (cliki term match case-sensitive)
  (declare (ignorable term pathname match case-sensitive))
  ;; need to do this based on the tfidf stuff
  (search-error "Full body searching not yet implemented.  Sorry."))

(defun search-page-topics (cliki term match case-sensitive)
  (let ((pred (search-predicate match case-sensitive)))
    (remove-duplicates
     (sort 
      (loop for page being the hash-values of (cliki-pages cliki)
	    if (member term (page-index page :topic) :test pred :key #'car)
	    collect page)
      #'string-lessp
      :key #'page-title))))


     