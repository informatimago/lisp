(in-package :cliki)

(defmethod cliki-page-surround  ((cliki cliki-view) request function
                                 &key title head)
  (cliki-page-header cliki request title head)
  (prog1
      (funcall function (request-stream request))
    (cliki-page-footer cliki request title)))

 
(defmethod cliki-page-header ((cliki cliki-view) request title &optional head)
  (let* ((stream (request-stream request))
         (home (cliki-url-root cliki)))
    (labels ((ahref (l) (urlstring (araneida:merge-url home l)))) 
      (let ((out
             (html
                 `(html
                      (head (title ,(format nil "CLiki : ~A" title))
                        ,@head
                        ((link :rel "alternate"
                               :type "application/rss+xml"
                               :title "Recent Changes"
                               :href ,(ahref  "recent-changes.rdf")))
                        ((link :rel "stylesheet" :href ,(ahref "admin/cliki.css"))))
                    (body
                        ((div :id "banner")
                         ((a :title "CLiki home page" :class "logo" :href ,(ahref nil))
                          "CL" ((span :class "sub") "iki"))
                         (span "the common lisp wiki")

                         ((div :id "navbar")
                          ((a :href ,(ahref (cliki-default-page-name cliki)) )
                           "Home")
                          ((a :href ,(ahref "Recent%20Changes")) "Recent Changes")
                          ((a :href ,(ahref "CLiki")) "About CLiki")
                          ((a :href ,(ahref "Text%20Formatting")) "Text Formatting")
                          ((a :onclick ,(format nil "if(name=window.prompt('New page name ([A-Za-z0-9 ])')) document.location='~a'+name ;return false;" (urlstring (merge-url home "edit/" ))) :href "#" )
                           "Create New Page")))

                      (h1 ,title)
                      (deleteme))))))
        (format stream
          "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">~%")
        (write-sequence
         (subseq out 0 (search "<DELETEME>" out))
         stream)))))

(defun request-for-version (page request)
  (let ((u (request-url request)))
    (integer-for (car (url-query-param u "v")) 
                 :default (car (page-versions page)))))

(defun version-links (cliki page request)
  (let ((ver (request-for-version page request)))
    (loop for v in (reverse
                    (subseq (page-versions page) 
                            0 (min 5 (length (page-versions page)))))
          if (= ver v)
          collect `((b :title 
                       ,(universal-time-to-http-date
                         (file-write-date (page-pathname page :version ver))))
                    ,ver)
          else
          collect `((a :href ,(format nil "~A?v=~A" 
                                      (urlstring (page-url cliki page))  v)
                       :title 
                       ,(universal-time-to-http-date
                         (file-write-date (page-pathname page :version v))))
                    ,v))))
  
(defmethod cliki-page-footer
  ((cliki cliki-view) request title)
  (let* ((page (find-page cliki title))
         (out (request-stream request))
         (text
          (format nil
            "<a href=\"edit/~A\">Edit page</a> | <a href=\"~A?source\">View source</a> | Revisions: "
            (urlstring-escape title) (urlstring-escape title))))
    (html-stream out
                 `((form  :action ,(urlstring (merge-url (cliki-url-root cliki)
                                                         "admin/search")))
                   ((div :id "footer")
                    ,text
                    ,@(and page (version-links cliki page request))
                    ((input :name "words" :size "30"))
                    ((input :type "submit" :value "search")))))
    (format out "<p>CLiki pages can be edited by anyone at any time.  Imagine a fearsomely comprehensive disclaimer of liability.  Now fear, comprehensively")
    ))


(defun print-page-selector
  (stream start-of-page number-on-page total-length urlstring-stub)
  "Print result page selector with `previous', `next', and numbered links to each result page. Form links by glomming offset to URLSTRING-STUB"
  (labels ((url (name offset)
                (format stream "~&<td><a href=\"~A~A\">~A</a></td>"
                        urlstring-stub offset name)))
    (princ "<center><table><tr><td>Result page:  </td><td> </td>" stream)
    (let ((first-on-screen
           (* (floor start-of-page number-on-page) number-on-page)))
      (if (> first-on-screen 0)
        (url "Previous"
             (- first-on-screen number-on-page)))
      (loop for i from 0 to total-length by number-on-page
            for j = 1 then (1+ j)
            if (<= i start-of-page (+ i number-on-page -1))
            do (format stream "~&<td>~A</td>" j)
            else do (url j i))
      (if (< (+ first-on-screen number-on-page) total-length)
        (url "Next" (+ first-on-screen number-on-page)))
      (princ "</tr></table></center>" stream))))


(defun long-form-for (token arg)
  (let ((*read-eval* nil)
        (*package* (find-package "KEYWORD")))
    (if (eq token :long-form)
      (multiple-value-bind (r e)
          (ignore-errors (values (read-from-string arg)))
        (if e (list :error (format nil "~A" e) arg)  r))
      (list (intern (string-upcase token) :keyword)
            (strip-outer-parens arg)))))

(defun subst-markup-in-stream (cliki in-stream out-stream)
  "Read the file for PAGE and write to OUT-STREAM, substituting weird markup language elements as we go. "  
  (let ((newlines 0)
        (returns 0))
    (labels ((dispatch (token arg)
                       (apply #'html-for-keyword
                              cliki out-stream (long-form-for token arg)))
             (output (c)
                     (cond
                      ((and (or (> newlines 1) (> returns 1))
                            (member c '(#\Newline #\Return)))
                       (write-sequence "<p>" out-stream)
                       (setf newlines 0 returns 0))
                      ((eql c #\Newline)
                       (incf newlines)
                       (write-char c out-stream))
                      ((eql c #\Return)
                       (incf returns)
                       (write-char c out-stream))
                      (t
                       (setf newlines 0 returns 0)
                       (write-char c out-stream)))))
      (scan-stream (cliki-short-forms cliki)
                   in-stream
                   #'output #'dispatch))))

(defun subst-markup-in-string (cliki in-string)
  (with-output-to-string (o)
    (with-input-from-string (in in-string)
      (subst-markup-in-stream cliki in o))))

(defun write-page-contents-to-stream (cliki page out-stream
                                            &key (version :newest))
  "Read the file for PAGE and write to OUT-STREAM, substituting weird markup language elements as we go. "  
  (if (page-pathname page)
    (with-open-file (in-stream (page-pathname page :version version)
                               :direction :input)
      (subst-markup-in-stream cliki in-stream out-stream))
    (format out-stream
      "This page doesn't exist yet.  Please create it if you want to")))

(defun view-page (cliki request page title &key (version :newest))
  (let ((lmtime (if page (page-last-modified page) (get-universal-time)))
        (google (and page (googlable-p page version))))
    (request-send-headers request :conditional t :last-modified lmtime)
    (with-page-surround (cliki request title
                               (unless google 
                                 '(((META :NAME "ROBOTS" :CONTENT "NOFOLLOW")))))
                        (if page
                          (progn
                            (let* ((topics 
                                    (sort (delete page (copy-list (page-topics page)))
                                          #'string-lessp
                                          :key #'page-title))
                                   (backlinks
                                    (sort (set-difference
                                           (delete page (copy-list (page-backlinks page)))
                                           topics)
                                          #'string-lessp :key #'page-title)))
                              (write-page-contents-to-stream cliki page out :version version)
                              (when topics
                                (format out "<hr><p><b>Page~p in this topic: </b> "
                                        (length topics))
                                (dolist (c topics)
                                  (format out "~A &nbsp; "
                                          (write-a-href cliki (page-title c) nil))))
                              (when backlinks
                                (format out "<hr><p><b>~A linked from: </b> "
                                        (if topics "Also" "This page is"))
                                (dolist (c backlinks)
                                  (format out "~A &nbsp; "
                                          (write-a-href cliki (page-title c) nil))))))
                          (format out
                            "This page doesn't exist yet.  Please create it if you want to"))))

  t)


(defgeneric html-for-keyword (cliki stream keyword &rest rest))

(defmethod html-for-keyword ((cliki cliki-view)
                             stream (keyword t) &rest args)
  (format stream "<b> [unrecognised ~A keyword occurred here: args ~S] </b>"
          keyword args))

(defmethod html-for-keyword ((cliki cliki-view)
                             stream (keyword (eql :error)) &rest args)
  (destructuring-bind (error form) args
    (format stream "<b> [Syntax error in tag: </b><br>~A<pre>~S</pre><b>] </b>"
            form (html-escape error))))

(defmethod html-for-keyword ((cliki cliki-view) stream
                             (keyword (eql :topic))
                             &rest args &aux (arg (car args)))
  (write-a-href cliki arg stream))

(defmethod html-for-keyword ((cliki cliki-view) stream
                             (keyword (eql :link))
                             &rest args &aux (arg (car args)))
  (write-a-href cliki arg stream))

(defmethod html-for-keyword ((cliki cliki-view) stream
                             (keyword (eql :legacy-search))
                             &rest args &aux (arg (car args)))
  (legacy-search-result cliki arg stream))

(defmethod html-for-keyword ((cliki cliki-view) stream
                             (keyword (eql :search))
                             &rest args)
  (destructuring-bind
      (&key title
            (no-results-message "No results from search")
            term show-relevance-p &allow-other-keys) args
    (let ((pages
           (sort 
            (loop for page being the hash-values of (cliki-pages cliki)
                  for r = (apply #'search-term-relevance cliki page term)
                  if (> r 0)
                  collect (list r page))
            #'>
            :key #'car))) 
      (cond (pages
             (format stream "~A<ul>" title)
             (dolist (p pages)
               (format stream
                 (if show-relevance-p "<li>~A (~A)</li>"
                     "<li>~A</li>")
                 (write-a-href cliki (page-title (cadr p)) nil)
                 (car p)))
             (format stream "</ul>"))
            (t (princ no-results-message stream))))))

(defmethod html-for-keyword ((cliki cliki-view) stream
                             (keyword (eql :clhs))
                             &rest args &aux (arg (car args)))
  (let* ((url (hyperspec-url arg)))
    (if url
      (format stream
		"<a class=\"hyperspec\" href = \"~a\"><b>~a</b></a>"
		url arg)
      (princ arg stream))))

(defmethod html-for-download-link ((cliki cliki-view) stream
                                   (from (eql :cclan)) name)
  (format stream
    "<a class=\"download\" href=\"http://ww.telent.net/cclan/~A\"
><b>Download CCLAN package ~A</b></a>"
    name name))

(defmethod html-for-download-link ((cliki cliki-view) stream
                                   (from t) name)
  (format stream
    "<a class=\"download\" href=\"~A\"
><b>Download from ~A</b></a>"
    name name))

(defmethod html-for-keyword ((cliki cliki-view) stream
                             (keyword (eql :download))
                             &rest args)
  (destructuring-bind (name &key (from :unknown) &allow-other-keys) args
    (html-for-download-link cliki stream from name)))

(defmethod html-for-keyword ((cliki cliki-view) stream
                             (keyword (eql :package))
                             &rest args)
  (let ((merged
         (urlstring (merge-url (parse-urlstring "http://ww.telent.net/cclan/")
                               (car args)))))
    (format stream
      "<a class=\"download\" href=\"~A\"
><b>Download ASDF package from ~A</b></a>"
      merged merged )))



(defun legacy-search-result (cliki string stream)
  ;; this business with read-from-string is a hangover from Ye Olde
  ;; search syntax
  (let* ((*read-eval* nil)
         (form (read-from-string (format nil "( ~A )" string) nil nil)))
    (destructuring-bind (term &key attribute match case-sensitive) form
      (let ((pages
             (legacy-search-pages
              cliki term :attribute attribute
              :match match :case-sensitive case-sensitive)))
        (html-stream 
         stream
         `(ul
              ,@(mapcar
                 (lambda (x)
                   (let ((title (page-title x))
                         (sentence (or (page-first-sentence x) "(no summary)")))
                     `(li ,(write-a-href cliki title nil)
                        " - " 
                        ,(subst-markup-in-string cliki sentence))))
                 pages)))))))

(defun strip-outer-parens (string)
  (and (eql (elt string 0) #\()
       (subseq string 1 (- (length string) 1))))


(defgeneric write-a-href (cliki-view title stream))
(defmethod write-a-href ((cliki cliki-view) title stream)
  "Write an A HREF element for the CLiki page TITLE.  STREAM may be an open stream or T or NIL, a la FORMAT"
  (let ((escaped (urlstring-escape title))
        (p (find-page cliki title)))
    (if (and p (page-pathname p))
      (format stream "<a class=\"internal\" href=\"~A\" >~A</a>" escaped title)
      (format stream "~A<a class=\"internal\" href=\"edit/~A\" >?</a>" title escaped))))

(defun read-matched-parens (stream)
  "Read from STREAM until we have seen as many #\) as #\(, returning
the string read.  Characters may be escaped by a preceding backslash;
this is left in the output but not counted by the bracket matcher"
  (let ((eof (gensym))
        (nesting 0))
    (with-output-to-string (out)
      (loop
       (let ((c (read-char stream nil eof)))
         (if (eql c eof) (return out))
         (if (eql c #\\)
           (progn
             (write-char c out)
             (setf c (read-char stream nil eof)))
           (progn
             (if (eql c #\() (incf nesting))
             (if (eql c #\)) (decf nesting))))
         (write-char c out)
         (if (eql nesting 0) (return out))
         )))))



