(in-package :cliki)

(defmethod cliki-css-text ((cliki cliki-view) stream)
  (write-sequence "
/*
 * cliki.css
 * toplevel cliki style sheet
 */

/*
 * general styles
 */
body 
{  
  background: white;
  color: black;
  font-family: serif;
  margin: 0; padding: 0.5em;
}

h1, h2, h3, h4 
{ 
  color: #999;
  font-family: sans-serif;
/*  text-transform: lowercase; */
}
h3 { font-weight: bold; font-size: 175%; }
/*
h1:before, h2:before, h3:before, h4:before { color: gold; content: '(' }
h1:after, h2:after, h3:after, h4:after { color: gold; content: ')' }
*/

hr { border: 0; background: black; height: 1px }
/* a { font-weight: bold; font-family: sans-serif; text-decoration: none } */
/* a { color: blue } */
a.internal { color: #0077bb ; }
a:visited.internal { color: #004488; } 
a.hyperspec { font-weight: bold; }

a[href]:hover { background: #ff9 }

table { border-collapse: collapse; border: 1px solid #aaa; }
th 
{ 
  font-family: sans-serif; 
  background: royalblue;
  color: white;
  text-transform: lowercase 
}
th, td { padding: 0.2em 0.3em }

ul li { list-style-type: square }

pre
{ 
  background: #cdf; 
  border: 1px solid #679; 
  color: #235; 
  font-size: 90%;
  padding: 0.3em;
}

/*
 * banner
 */
#banner
{ 
  background: steelblue;
  border: 1px solid black;
  color: white;
  margin: 0; padding: 0.5em 0.5em 1.5em;
}
#banner a { color: white; font-weight: normal; text-decoration: none; padding: 0 }
#banner a:hover { background: transparent; color: white }
#banner a.logo { font-family: serif; font-weight: bold; font-size: 250% }
#banner a.logo .sub 
{ 
  color: gold;
  font-size: 90%; 
  position: relative; top: -0.3em; 
}

.search
{ 
/*   float: right; */
  text-align: right;
  text-transform: lowercase;
}
.search input { border: 1px solid black }

#navbar { margin: 1em 0 -0.5em 0 }
#navbar a 
{ 
  border: 1px solid gold;
  border-width: 1px 0 1px 0;
  font-size: 90%;
  margin: 0 1em; padding: 0 0.2em 
}
#navbar a:hover
{ 
  border: 1px solid white;
  border-width: 1px 0 1px 0;
}

.lastedit 
{ 
  color: gold;
  float: right;
  text-align: right; 
  text-transform: lowercase;
  margin: 0; padding: 0
}

/*
 * content
 */
#content
{ 
  clear: both;
  margin: 0; padding: 0 1.5em;
}

/*
 * footer
 */
#footer
{ 
  background: steelblue;
  color: white;
  border: 1px solid black;
  font-family: sans-serif;
  margin-top: 0.5em; padding: 0.5em;
}
#footer a[href] { color: gold; text-decoration: none }
#footer a[href]:hover { background: transparent; color: gold }
.disclaimer { font-weight: bold }
" stream))

(defun cliki-list-all-pages-handler (request)
  (let* ((cliki (request-cliki request))
	 (pages (loop for p being the hash-values of
		      (cliki-pages cliki)
		      when (page-pathname p) collect p)))
    (request-send-headers request)
    (html-stream
     (request-stream request)
     `(html
       (head (title "Cliki: All pages"))
       (body
	(h1 "All pages")
	(ul
	 ,(lambda (stream)
		  (dolist (x pages)
		    (html-stream 
		     stream
		     `(li ((a :href
			      ,(urlstring (page-url cliki x)))
			   ,(page-title x))))))))))))

(defun search-pages (cliki term)
  (when term
    (sort (loop for page being the hash-values of (cliki-pages cliki)
		for relevance = (apply #'search-term-relevance cliki page term)
		if (> relevance 0)
		collect (cons page relevance))
	  #'>
	  :key #'cdr)))

(defun complex-search-term (term)
  (cond ((zerop (length term)) nil)
	((eql (elt term 0) #\()
	 (let ((*read-eval* nil)
	       (*package* (find-package :keyword)))
	   (ignore-errors
	     (values (read-from-string
		      (urlstring-unescape term))))))
	(t `(:body ,term))))

(defmethod format-search-relevance ((cliki cliki-view) relevance)
  (format nil "(~,01F% relevant)" (* relevance 100)))

(defun cliki-search-handler (request)
  (let* ((url (request-url request))
	 (cliki (request-cliki request))
	 (term (car (url-query-param url  "words"))))
    (multiple-value-bind (c-term error) (complex-search-term term)
      (let* ((results (unless error (search-pages cliki c-term)))
	     (start (parse-integer
		     (or (car (url-query-param url "start"))
			 "0") :junk-allowed t))
	     (end (min (length results) (+ start 10)))
	     (out (request-stream request)))
	(request-send-headers request)
	(with-page-surround (cliki request "Search results")
	  (format out "<form action=\"~Aadmin/search\"> Search again: <input name=words size=60 value=~S></form>"
		  (urlstring (cliki-url-root cliki))
		  (html-escape
		   (cond (error term)
			 ((eql (car c-term) :body) (cadr c-term))
			 (t (prin1-to-string c-term)))))
	  (cond
	    (error
	     (format out "Sorry, your search term could not be read<pre>~A</pre>" (html-escape (princ-to-string error))))
	    (results
	     (search-results-blurb cliki out)
	     (format out "<p>~A result~:p found, showing results ~A to ~A.  " (length results) (1+ start)  end )
	     (loop for (name . rel) in (subseq results start end)
		   for j from start to end
		   do (format out "~&<p>~A <b><a href=\"~A\">~A</a></b> ~A<br>~A"
			      (1+ j)
			      (urlstring (page-url cliki name))
			      (page-title name)
			      (format-search-relevance cliki rel)
			      (let ((s (page-summary cliki name c-term)))
				(if s
				    (html
				     `((div :style "margin-top: -5px; margin-left: 5%; font-size: 80%")
				       ,@s))
				    ""))
			      ))
	     (print-page-selector
	      (request-stream request) start 10 (length results)
	      (format nil "~A?words=~A&start="
		      (url-path url)
		      (urlstring-escape
		       (format nil "~S" c-term)))))
	    (t (format out "Sorry, no pages match your search term."))))
	t))))

		     
;; XXX is this used for anything?
(defvar   *cliki-instance*)

