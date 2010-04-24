(in-package :cliki)

;;; once upon a time, cliki the engine was simply the code that runs
;;; cliki the web site.  These days there are multiple users of cliki
;;; the engine, and not all of them want our exact CSSified layout and
;;; crass donation solicitation.

(defclass cliki-net (cliki-instance) ())

(defmethod cliki-css-text ((cliki cliki-net) stream)
  (write-sequence "
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
}
h3 { font-weight: bold; font-size: 175%; }

hr { border: 0; background: black; height: 1px }
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
  margin: 0;  padding: 0.5em 0.5em 1.5em; 
}
#banner a { color: white; font-weight: normal; text-decoration: none; padding: 0 }
#banner a:hover { background: transparent; color: white }
#banner a:hover.logo { text-decoration: none }
#banner a.logo { font-family: serif; font-weight: bold; font-size: 250%; border-width: 0px }
#banner a.logo .sub 
{ 
  color: gold;
  font-size: 90%; 
  position: relative; top: -0.3em; 
}

.search
{ 
  float: right; 
  text-align: right;
  padding:  1em 0 0;
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
" stream))


(defmethod cliki-page-surround  ((cliki cliki-net) request function
				 &key title head)
  (let* ((stream (request-stream request))
	 (page (find-page cliki title))
         (home (cliki-url-root cliki)))
    (format stream
	    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">~%")
    (labels ((ahref (l) (urlstring (merge-url home l)))) 
      (html-stream
       stream
       `(html
	 (head (title ,(format nil "CLiki : ~A" title))
	       ,@head
	       ((link :rel "alternate"
		      :type "application/rss+xml"
		      :title "Recent Changes"
		      :href ,(ahref  "recent-changes.rdf")))
	       ((link :rel "stylesheet" :href ,(ahref "admin/cliki.css"))))
	 (body
	  ((form  :action ,(ahref "admin/search"))
	   ((div :id "banner")
	    ((span :class "search")
	     ((input :name "words" :size "30"))
	     ((input :type "submit" :value "search")))
	    ((a :title "CLiki home page" :class "logo" :href ,(ahref nil))
	     "CL" ((span :class "sub") "iki"))
	    "the common lisp wiki"
	    (br)
	    ((div :id "navbar")
	     ((a :href ,(ahref (cliki-default-page-name cliki))) "Home")
	     ((a :href ,(ahref "Recent%20Changes")) "Recent Changes")
	     ((a :href ,(ahref "CLiki")) "About CLiki")
	     ((a :href ,(ahref "Text%20Formatting")) "Text Formatting")
	     ((a :onclick ,(format nil "if(name=window.prompt('New page name ([A-Za-z0-9 ])')) document.location='~a'+name ;return false;" (ahref "edit/" ))
		 :href "#" )
	      "Create New Page"))))
	  (h1 ,title)
	  ,function
	  ,@(if page
		  `(((div :id "footer")
		     ((a :href 
			 ,(if page (format nil "edit/~A?v=~A"
					   (urlstring-escape title)
					   (request-for-version page request))
			      (format nil "edit/~A"
				      (urlstring-escape title))))
		      "Edit page")
		     " | " 
		     ((a :href ,(format nil "~A?source"
					(urlstring-escape title)))
		      "View source")
		     " | Revisions: "
		     ,@(version-links cliki page request)))

		  '(((div :id "footer") (br))))
	  (p "CLiki pages can be edited by anyone at any time.  Imagine a fearsomely comprehensive disclaimer of liability.  Now fear, comprehensively")
	  ))))))

(defparameter *blacklist* (cl-ppcre:create-scanner "interseo|transwell|navinic|activeshow|sba.com.cn|konseptech.com"))

(defmethod check-page-save-allowed ((cliki cliki-net) page version user)
  (call-next-method) 
  (when (string-prefix-p "A N Other" user)
    (signal 'cliki-page-save-rejected "Anonymous posting is disabled.  Please provide a name (preferably your own"))
  (let ((body (request-body *request*)))
    (loop for (name value) in body
	  for el =
	  (and (eql (elt name 0) #\T) (digit-char-p (elt name 1))
	       (parse-form-element-for-keyword
		cliki *request* (intern value :keyword)
		(parse-integer name :start 1 :junk-allowed t)))
	  when (typep el 'string)
	  do (if (cl-ppcre:scan *blacklist* el)
		 (signal 'cliki-page-save-rejected "Internal error")))))




