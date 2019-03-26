;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               gitlab.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This packages provides a lisp wrapper over the gitlab web API.
;;;;
;;;;    Currently implemented: fetching the project list,
;;;;    fetching, creating, deleting, and updating issues.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2019-03-26 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2019 - 2019
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

(defpackage "COM.INFORMATIMAGO.CLEXT.GITLAB"
  (:use "COMMON-LISP" "CL-JSON" "DRAKMA" "BABEL" "SPLIT-SEQUENCE")
  (:use "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SYMBOL")
  (:export
   "*VERBOSE*"
   "*SERVER*"
   "*PRIVATE-TOKEN*"

   "REQUEST-ERROR" "REQUEST-ERROR-STATUS-CODE"
   "REQUEST-ERROR-STATUS-TEXT" "REQUEST-ERROR-HEADERS"
   "REQUEST-ERROR-URI" "REQUEST-ERROR-MUST-CLOSE"
   "REQUEST-ERROR-HTTP-STREAM"

   "USER-ID" "USER-NAME" "USER-USERNAME" "USER-STATE" "USER-AVATAR-URL"
   "USER-WEB-URL" "MAKE-USER"

   "PROJECT-ID" "PROJECT-DESCRIPTION" "PROJECT-NAME" "PROJECT-PATH"
   "PROJECT-OWNER" "PROJECT-PATH-WITH-NAMESPACE" "MAKE-PROJECT"

   "ISSUE-ID" "ISSUE-IID" "ISSUE-PROJECT-ID" "ISSUE-TITLE"
   "ISSUE-DESCRIPTION" "ISSUE-STATE" "ISSUE-CREATED-AT"
   "ISSUE-UPDATED-AT" "ISSUE-CLOSED-AT" "ISSUE-DUE-DATE" "ISSUE-LABELS"
   "ISSUE-MILESTONE" "ISSUE-ASSIGNEES" "ISSUE-ASSIGNEE" "ISSUE-AUTHOR"
   "ISSUE-CONFIDENTIAL" "ISSUE-WEIGHT" "ISSUE-USER-NOTES-COUNT"
   "ISSUE-UPVOTES" "ISSUE-DOWNVOTES" "ISSUE-DISCUSSION-LOCKED"
   "ISSUE-WEB-URL" "ISSUE-TIME-STATS" "MAKE-ISSUE"

   "ISSUES" "CREATE-ISSUE" "DELETE-ISSUE" "UPDATE-ISSUE"

   "LIST-PROJECTS"))
(in-package  "COM.INFORMATIMAGO.CLEXT.GITLAB")

(defparameter *server* "149.202.216.117")
(defparameter *private-token* "Yh981vkXptkUJTzoZw4x")


(define-condition request-error (error)
  ((status-code :initarg :status-code :reader request-error-status-code)
   (status-text :initarg :status-text :reader request-error-status-text)
   (headers     :initarg :headers     :reader request-error-headers)
   (uri         :initarg :uri         :reader request-error-uri)
   (must-close  :initarg :must-close  :reader request-error-must-close)
   (http-stream :initarg :http-stream :reader request-error-http-stream))
  (:report (lambda (condition stream)
             (format stream "gitlab server returned error: ~S (~A) for request ~A~%~:{~A: ~A~%~}"
                     (request-error-status-text condition)
                     (request-error-status-code condition)
                     (request-error-uri condition)
                     (mapcar (lambda (cell) (list (car cell) (cdr cell)))
                             (request-error-headers condition))))))

(defvar *verbose* nil)


(defun aget (alist key) (cdr (assoc key alist)))
(defun (setf aget) (new-value alist key)
  (assert (not (null alist)) nil "Cannot set an empty a-list.")
  (let ((entry (assoc key alist)))
    (if entry
        (setf (cdr entry) new-value)
        (push (cons key new-value) (cdr alist))))
  new-value)

(defun check-response (expect-headers status-code headers uri http-stream must-close status-text)
  (if (= (truncate status-code 100) 2)
      (dolist (expected-header expect-headers)
        (let ((actual-header (aget headers (car expected-header))))
          (when actual-header
            (assert (string= (cdr expected-header) actual-header)))))
      (error 'request-error
             :status-code status-code
             :status-text status-text
             :headers headers
             :uri uri
             :must-close must-close
             :http-stream http-stream)))

(defun verbosity (output status-code headers uri http-stream must-close status-text)
  (declare (ignore must-close http-stream))
  (when *verbose*
    (format *trace-output* "~A~%~:{~A: ~A~%~}--> ~A ~S~%~S~%"
            uri (mapcar (lambda (cell) (list (car cell) (cdr cell))) headers) status-code status-text
            (ignore-errors (decode-json-bytes output)))))

(defun decode-json-bytes (bytes)
  (unless (zerop (length bytes))
    (decode-json-from-string (octets-to-string bytes))))

(defmacro handle-http-answer ((&key expect-headers) &body body)
  `(multiple-value-bind (output status-code headers uri http-stream must-close status-text)
       (progn ,@body)
     (verbosity output status-code headers uri http-stream must-close status-text)
     (check-response ,expect-headers status-code headers uri http-stream must-close status-text)
     output))

(defconstant +per-page+ 100)

(defun fetch-json-objects (query)
  (loop
    :with done := nil
    :for page :from 1
    :for bunch := (multiple-value-bind (output status-code headers uri http-stream must-close status-text)
                      (drakma:http-request (format nil "~A~Cper_page=~A&page=~A"
                                                   query
                                                   (if (find #\? query)
                                                       #\&
                                                       #\?)
                                                   +per-page+
                                                   page)
                                           :method :get
                                           :additional-headers (list (cons "PRIVATE-TOKEN" *private-token*)))
                    (verbosity output status-code headers uri http-stream must-close status-text)
                    (check-response '((:content-type . "application/json"))
                                    status-code headers uri http-stream must-close status-text)
                    (let ((decoded (decode-json-bytes output)))
                      (setf done (let ((total-page (aget headers :x-total-pages)))
                                   (if total-page
                                      (<= (parse-integer total-page) page)
                                       ;; we don't get x-total-pages when there are more than 10,000
                                      (< (length decoded) +per-page+))))
                      decoded))
    :append bunch
    :until done))

(defun operate-json-object (query parameters operation)
  (decode-json-bytes
   (handle-http-answer (:expect-headers '((:content-type . "application/json")))
     (drakma:http-request query
                          :external-format-out :utf-8
                          :form-data (and parameters (eq operation :post))
                          :parameters parameters
                          :method operation
                          :additional-headers (list (cons "PRIVATE-TOKEN" *private-token*))))))

(defun create-json-object (query parameters) (operate-json-object query parameters :post))
(defun update-json-object (query parameters) (operate-json-object query parameters :put))
(defun delete-json-object (query)            (operate-json-object query nil        :delete))


(defmacro define-json-struct (name &rest field-clauses)
  (let* ((fields    (mapcar (lambda (field-clause)
                              (if (symbolp field-clause)
                                  field-clause
                                  (first field-clause)))
                            field-clauses))
         (json-keys (mapcar (lambda (field-clause)
                              (if (symbolp field-clause)
                                  (intern (symbol-name field-clause) "KEYWORD")
                                  (second field-clause)))
                            field-clauses))
         (keys      (mapcar (lambda (field)
                              (intern (symbol-name field) "KEYWORD"))
                            fields)))
    `(progn
       ,@(mapcan (lambda (field key)
                   (let ((fname (scat name '-  field)))
                     (list
                      `(defun ,fname (,name) (aget ,name ,key))
                      `(defun (setf ,fname) (new-value ,name) (setf (aget ,name ,key) new-value)))))
                 fields json-keys)
       (defun ,(scat 'make- name) (&key ,@fields)
         (list ,@(mapcar (lambda (k f) `(cons ,k ,f)) keys fields)))
       ',name)))


(defun format-iso8601-time (time-value &optional include-timezone-p)
  "Formats a universal time TIME-VALUE in ISO 8601 format, with
    the time zone included if INCLUDE-TIMEZONE-P is non-NIL"
  ;; Taken from http://www.pvv.ntnu.no/~nsaa/ISO8601.html
  ;; Thanks, Nikolai Sandved and Thomas Russ!
  (flet ((format-iso8601-timezone (zone)
           (if (zerop zone)
               "Z"
               (multiple-value-bind (h m) (truncate (abs zone) 1.0)
                 ;; Tricky.  Sign of time zone is reversed in ISO 8601
                 ;; relative to Common Lisp convention!
                 (format nil "~:[+~;-~]~2,'0D:~2,'0D"
                         (> zone 0) h (round (* 60 m)))))))
    (multiple-value-bind (second minute hour day month year dow dst zone)
        (decode-universal-time time-value)
      (declare (ignore dow))
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~:[~*~;~A~]"
              year month day hour minute second
              include-timezone-p (format-iso8601-timezone (if dst
                                                              (+ zone 1)
                                                              zone))))))

(defun encode-boolean (value)
  (if value "true" "false"))

(deftype datetime () `(or string integer))

(defun encode-datetime (datetime)
  (url-encode (etypecase datetime
                (string datetime)
                (integer (format-iso8601-time datetime t))) :utf-8))

(defun decode-labels (json-value)
  (split-sequence #\, json-value :remove-empty-subseqs t))

(defun encode-labels (labels)
  (check-type labels (or string list))
  (if (stringp labels)
      labels
      (format nil "~{~A~^,~}" labels)))


(define-json-struct user
  id name
  username
  state
  (avatar-url :avatar--url)
  (web-url :web--url))

(define-json-struct project
  id description name path owner
  (path-with-namespace :path--with--namespace))

(defun projects (&key id)
  (fetch-json-objects (with-output-to-string (*standard-output*)
                        (format t "https://~A/api/v4/projects" *server*)
                        (when id (format t "/~A" id)))))

(defun project-named (name)
  (find-if (lambda (project)
             (or (string= name (project-name project))
                 (string= name (project-path project))
                 (string= name (project-path-with-namespace project))))
           (projects)))

(define-json-struct issue
  id iid
  (project-id :project--id)
  title description state
  (created-at :created--at)
  (updated-at :updated--at)
  (closed-at  :closed--at)
  (due-date   :due--date)
  labels
  milestone assignees assignee author
  confidential
  weight
  (user-notes-count :user--notes--count)
  upvotes downvotes
  (discussion-locked :discussion--locked)
  (web-url :web--url)
  (time-stats :time--stats))

(defun issues (&key
                 group-id
                 project-id
                 id
                 (scope :created-by-me)
                 (state nil statep)
                 (labels nil labelsp)
                 (milestone nil milestonep)
                 (iids nil iidsp)
                 author-id
                 assignee-id
                 (search nil searchp) (in nil inp)
                 (my-reaction-emoji nil my-reaction-emoji-p)
                 (confidential nil confidentialp)
                 (weight nil weightp)
                 order-by sort
                 created-after
                 updated-after
                 created-before
                 updated-before)
  (check-type id                (or null integer))
  (check-type scope             (member :created-by-me :assigned-to-me :all))
  (check-type state             (or null (member :opened :closed)))
  (check-type labels            (or null string list))
  (check-type group-id          (or null integer))
  (check-type project-id        (or null integer))
  (check-type author-id         (or null integer))
  (check-type assignee-id       (or null integer (member :none :any)))
  (check-type my-reaction-emoji (or null string  (member :none :any)))
  (check-type weight            (or null integer (member :none :any)))
  (check-type iids              (or null (vector integer)))
  (check-type order-by          (or null (member :created-at :updated-at)))
  (check-type sort              (or null (member :asc :desc)))
  (check-type search            (or null string))
  (check-type in                (or null (:member :title :description :both)))
  (check-type created-after     (or null (or string integer)))
  (check-type created-before    (or null (or string integer)))
  (check-type updated-after     (or null (or string integer)))
  (check-type updated-before    (or null (or string integer)))
  (assert (not (and project-id group-id)) (project-id group-id) "Cannot specify both group-id and project-id.")
  (let ((query (with-output-to-string (*standard-output*)
                 (format t "https://~A/api/v4" *server*)
                 (cond
                   (group-id   (format t "/groups/~A" group-id))
                   (project-id (format t "/projects/~A" project-id)))
                 (let ((attributes
                         (append
                          (when scope   (list (format nil "scope=~(~A~)" (url-encode (substitute #\- #\_ (princ-to-string scope)) :utf-8))))
                          (when statep  (list (format nil "state=~(~A~)" state)))
                          (when labelsp (list (format nil "labels=~A"    (url-encode (encode-labels labels) :utf-8))))
                          (when milestonep  (list (format nil "milestone=~A" (ecase milestone
                                                                               (:none "None")
                                                                               (:any  "Any")
                                                                               (otherwise (url-encode milestone :utf-8))))))
                          (when iidsp (map 'list (lambda (iid)
                                                   (format nil "iids[]=~A" iid))
                                           iids))
                          (when author-id   (list (format nil "author_id=~A" author-id)))
                          (when assignee-id (list (format nil "assignee_id=~A" (ecase assignee-id
                                                                                 (:none "None")
                                                                                 (:any  "Any")
                                                                                 (otherwise assignee-id)))))
                          (when searchp (list (format nil "search=~A" (url-encode search :utf-8))))
                          (when inp (list (case in
                                            (:title       "in=title")
                                            (:description "in=description")
                                            (:both        "in=title,description"))))
                          (when my-reaction-emoji-p (list (format nil "my_reaction_emoji=~A"
                                                                  (ecase my-reaction-emoji
                                                                    (:none "None")
                                                                    (:any  "Any")
                                                                    (otherwise (url-encode my-reaction-emoji :utf-8))))))
                          (when weightp (list (format nil "weight=~A"
                                                      (ecase weight
                                                        (:none "None")
                                                        (:any  "Any")
                                                        (otherwise  weight)))))
                          (when confidentialp (list (format nil "confidential=~A" (encode-boolean confidential))))
                          (when order-by (list (format nil "order_by=~A" (ecase order-by
                                                                           (:created-at "created_at")
                                                                           (:updated-at "updated_at")))))
                          (when sort (list (format nil "sort=~A" (ecase order-by
                                                                   (:asc "asc")
                                                                   (:desc "desc")))))
                          (when created-after  (list (format nil "created_after=~A"  (encode-datetime created-after))))
                          (when created-before (list (format nil "created_before=~A" (encode-datetime created-before))))
                          (when updated-after  (list (format nil "updated_after=~A"  (encode-datetime updated-after))))
                          (when updated-before (list (format nil "updated_before=~A" (encode-datetime updated-before)))))))
                   (format t "/issues~:[~;/~:*~A~]~:[~;?~:*~{~A~^&~}~]" id attributes)))))
    (fetch-json-objects query)))

(defun create-issue (project-id title
                     &key iid description (confidential nil confidentialp)
                       assignee-ids milestone-id
                       labels created-at due-date weight
                       merge-request-to-resolve-discussion-of discussion-to-resolve)
  (check-type project-id                             integer)
  (check-type title                                  string)
  (check-type iid                                    (or null integer string))
  (check-type description                            (or null string))
  (check-type assignee-ids                           list)
  (check-type milestone-id                           (or null integer))
  (check-type labels                                 (or list string))
  (check-type created-at                             (or null datetime))
  (check-type due-date                               (or null datetime))
  (check-type merge-request-to-resolve-discussion-of (or null integer))
  (check-type discussion-to-resolve                  (or null string))
  (check-type weight                                 (or null integer))
  (create-json-object (with-output-to-string (*standard-output*)
                        (format t "https://~A/api/v4" *server*)
                        (format t "/projects/~A" project-id)
                        (format t "/issues?~{~A~^&~}"
                                (append
                                 (when iid           (list (format nil "iid=~A"          iid)))
                                 (mapcar (lambda (assignee-id)
                                           (format nil "assignee_ids[]=~A" assignee-id))
                                         assignee-ids)
                                 (when milestone-id  (list (format nil "milestone_id=~A"  milestone-id)))
                                 (when weight        (list (format nil "weight=~A"        weight)))
                                 (when created-at    (list (format nil "created_at=~A"    (encode-datetime created-at))))
                                 (when due-date      (list (format nil "due_date=~A"      (encode-datetime due-date))))
                                 (when merge-request-to-resolve-discussion-of
                                   (list (format nil "merge_request_to_resolve_discussion_of=~A"
                                                 merge-request-to-resolve-discussion-of)))
                                 (when discussion-to-resolve
                                   (list (format nil "discussion_to_resolve=~A" (url-encode discussion-to-resolve :utf-8))))
                                 (when confidentialp (list (format nil "confidential=~A"  (encode-boolean confidential)))))))

                      (append
                       (when description (list (cons "description" description) ))
                       (when title       (list (cons "title"       title)))
                       (when labels      (list (cons "labels"      (encode-labels labels)))))))

(defun delete-issue (issue)
  (delete-json-object (with-output-to-string (*standard-output*)
                        (format t "https://~A/api/v4" *server*)
                        (format t "/projects/~A" (issue-project-id issue))
                        (format t "/issues/~A" (issue-iid issue)))))

(defun update-issue (issue &key state-event)
  (check-type state-event (or null (member :reopen :close)))
  (with-accessors ((id issue-id)
                   (iid issue-iid)
                   (project-id issue-project-id)
                   (title issue-title)
                   (description issue-description)
                   (state issue-state)
                   (created-at issue-created-at)
                   (updated-at issue-updated-at)
                   (closed-at issue-closed-at)
                   (due-date issue-due-date)
                   (milestone issue-milestone)
                   (assignees issue-assignees)
                   (assignee issue-assignee)
                   (author issue-author)
                   (confidential issue-confidential)
                   (weight issue-weight)
                   (labels issue-labels)) issue
    (update-json-object (with-output-to-string (*standard-output*)
                          (format t "https://~A/api/v4" *server*)
                          (format t "/projects/~A" project-id)
                          (format t "/issues/~A?~{~A~^&~}" iid
                                  (append
                                   (when confidential  (list (format nil "confidential=~A"  (encode-boolean confidential))))
                                   (mapcar (lambda (assignee-id)
                                             (format nil "assignee_ids[]=~A" assignee-id))
                                           assignees)
                                   (when milestone     (list (format nil "milestone_id=~A"  milestone)))
                                   (when updated-at    (list (format nil "updated_at=~A"    (encode-datetime updated-at))))
                                   (when due-date      (list (format nil "due_date=~A"      (encode-datetime due-date))))
                                   (when weight        (list (format nil "weight=~A"        weight))))))
                        (append
                         (when title         (list (cons "title"       title)))
                         (when description   (list (cons "description" description)))
                         (when labels        (list (cons "labels"      (encode-labels labels))))))))


(defun list-projects ()
  (map nil
       (lambda (project)
         (format t "~40A~:[ \"\"~;~:*~S~]~%   ~S~2%"
                 (project-name project)
                 (user-name (project-owner project))
                 (project-description project)))
       (projects)))





#|
(dolist (issue (remove-if (lambda (issue)
                            (string/= (issue-title issue) "test"))
                          (issues :project-id (project-id (project-named "sbde/laboite"))
                                  :scope :all)))
  (print issue)
;;   (setf (issue-labels issue) (format nil "~:{~A~^,~}"
;;                                      (cons "Backlog"
;;                                            (split-sequence #\,  (issue-labels issue) :remove-empty-subseqs t))))
;;   (setf (issue-description issue) "This is a test issue.
;; Don't make anything of it.")
;;   (update-issue issue)
  )

(dolist (issue (remove-if (lambda (issue)
                            (string/= (issue-title issue) "test"))
                          (issues :project-id (project-id (project-named "sbde/laboite"))
                                  :scope :all)))
  (delete-issue issue))


(let ((issue '((:id . 86) (:iid . 28) (:project--id . 34) (:title . "test") (:description . "This+is+a+test+issue.%0ADon't+make+anything+of+it.") (:state . "opened") (:created--at . "2019-03-25T17:53:26.942Z") (:updated--at . "2019-03-25T18:46:31.411Z") (:closed--at) (:labels "iOS") (:milestone) (:assignees) (:author (:id . 2) (:name . "Pascal J. Bourguignon") (:username . "pjb") (:state . "active") (:avatar--url . "https://gitlab.sbde.fr/uploads/-/system/user/avatar/2/avatar.png") (:web--url . "https://gitlab.sbde.fr/pjb")) (:assignee) (:user--notes--count . 0) (:upvotes . 0) (:downvotes . 0) (:due--date) (:confidential) (:discussion--locked) (:web--url . "https://gitlab.sbde.fr/sbde/laboite/issues/28") (:time--stats (:time--estimate . 0) (:total--time--spent . 0) (:human--time--estimate) (:human--total--time--spent)))))

  (print  (issue-labels issue))
  (print (cons "Backlog"
                (split-sequence #\,  (issue-labels issue) :remove-empty-subseqs t)))
  ;; (format t "~{~A~^,~}"
  ;;         (cons "Backlog"
  ;;               (split-sequence #\,  (issue-labels issue) :remove-empty-subseqs t)))
  )

("Backlog" ("iOS")) ("Backlog" ("iOS"))
  (issue-labels issue)
("iOS")





(in-package  "COM.INFORMATIMAGO.CLEXT.GITLAB")
(setf *verbose* t)
(projects :id 34)
(project-path-with-namespace (projects :id 34))

(create-issue (project-id (project-named "sbde/laboite"))
              "test"
              :description "This is a test issue.
Don't make anything of it."
              :labels '("iOS"))

(mapcar (function delete-issue)
        (remove-if (lambda (issue)
                     (string/= (issue-title issue) "test"))
                   (issues :project-id (project-id (project-named "sbde/laboite"))
                           :scope :all)))


(issue-description (first (issues :project-id (project-id (project-named "sbde/laboite"))
                                  :scope :all)))
"This+is+a+test+issue.%0ADon't+make+anything+of+it."


 (remove-if (lambda (issue)
                     (string/= (issue-title issue) "test"))
            (issues :project-id (project-id (project-named "sbde/laboite"))
                           :scope :all))



(delete-issue '((:id . 82) (:iid . 25) (:project--id . 34) (:title . "test") (:description . "This+is+a+test+issue.%0ADon't+make+anything+of+it.") (:state . "opened") (:created--at . "2019-03-25T17:29:50.653Z") (:updated--at . "2019-03-25T17:29:50.653Z") (:closed--at) (:labels "iOS") (:milestone) (:assignees) (:author (:id . 2) (:name . "Pascal J. Bourguignon") (:username . "pjb") (:state . "active") (:avatar--url . "https://gitlab.sbde.fr/uploads/-/system/user/avatar/2/avatar.png") (:web--url . "https://gitlab.sbde.fr/pjb")) (:assignee) (:user--notes--count . 0) (:upvotes . 0) (:downvotes . 0) (:due--date) (:confidential) (:discussion--locked) (:web--url . "https://gitlab.sbde.fr/sbde/laboite/issues/25") (:time--stats (:time--estimate . 0) (:total--time--spent . 0) (:human--time--estimate) (:human--total--time--spent)) (:--links (:self . "https://gitlab.sbde.fr/api/v4/projects/34/issues/25") (:notes . "https://gitlab.sbde.fr/api/v4/projects/34/issues/25/notes") (:award--emoji . "https://gitlab.sbde.fr/api/v4/projects/34/issues/25/award_emoji") (:project . "https://gitlab.sbde.fr/api/v4/projects/34")) (:subscribed . t)))





(projects :id 4)

(second(issues :scope :all))
(issues :project-id 34 :id 23)



(mapcar (lambda (issue) (multiple-value-list (delete-issue issue)))
(issues :project-id 4 :scope :all))
(mapcar (function issue-iid) (issues :project-id 4 :scope :all))

(mapcar (lambda (project)
(list (project-id project) (project-name project)))
(projects))

(mapcar (lambda (issue)
(list (issue-iid issue) (issue-title issue)))
(issues :project-id 35))

(mapcar (lambda (project) (list (project-id project) (project-name  project)
(issues :project-id (project-id project) :scope :all)))
(projects))





(dolist (project (projects))
(let ((pid (project-id project)))

))

(cl-user::lspack :drakma t)

DRAKMA
Symbols:         40 exported, 1340 total."GET"
Uses:          CHUNGA COMMON-LISP FLEXI-STREAMS
Used by:       COM.INFORMATIMAGO.CLEXT.GITLAB
Exported:      *ALLOW-DOTLESS-COOKIE-DOMAINS-P*
*BODY-FORMAT-FUNCTION*
*DEFAULT-HTTP-PROXY*
*DRAKMA-DEFAULT-EXTERNAL-FORMAT*
*DRAKMA-VERSION*
*HEADER-STREAM*
*IGNORE-UNPARSEABLE-COOKIE-DATES-P*
*NO-PROXY-DOMAINS*
*REMOVE-DUPLICATE-COOKIES-P*
*TEXT-CONTENT-TYPES*

DECODE-STREAM
DELETE-OLD-COOKIES
DRAKMA-CONDITION
DRAKMA-ERROR
DRAKMA-WARNING

GET-CONTENT-TYPE
HEADER-VALUE
HTTP-REQUEST

PARAMETER-ERROR
PARAMETER-PRESENT-P
PARAMETER-VALUE
PARSE-COOKIE-DATE
READ-TOKENS-AND-PARAMETERS
SPLIT-TOKENS
SYNTAX-ERROR
URL-ENCODE


nil
(drakma:get)

JSON
Symbols:         85 exported, 1297 total.
Nicknames:     CL-JSON
Uses:          COMMON-LISP
Used by:       JSON-RPC
Exported:      *AGGREGATE-SCOPE-VARIABLES* *ARRAY-MEMBER-HANDLER* *ARRAY-SCOPE-VARIABLES*
*BEGINNING-OF-ARRAY-HANDLER* *BEGINNING-OF-OBJECT-HANDLER* *BEGINNING-OF-STRING-HANDLER*
*BOOLEAN-HANDLER* *END-OF-ARRAY-HANDLER* *END-OF-OBJECT-HANDLER* *END-OF-STRING-HANDLER*
*IDENTIFIER-NAME-TO-KEY* *INTEGER-HANDLER* *INTERNAL-DECODER* *JSON-ARRAY-TYPE*
*JSON-IDENTIFIER-NAME-TO-LISP* *JSON-INPUT* *JSON-OUTPUT* *JSON-SYMBOLS-PACKAGE*
*LISP-IDENTIFIER-NAME-TO-JSON* *OBJECT-KEY-HANDLER* *OBJECT-SCOPE-VARIABLES* *OBJECT-VALUE-HANDLER*
*PROTOTYPE-NAME* *REAL-HANDLER* *STRING-CHAR-HANDLER* *STRING-SCOPE-VARIABLES*
*USE-STRICT-JSON-RULES* AS-ARRAY-MEMBER AS-OBJECT-MEMBER BIGNUMBER-STRING BIND-CUSTOM-VARS
CAMEL-CASE-TO-LISP CLEAR-CLASS-REGISTRY CURRENT-DECODER CUSTOM-DECODER DECODE-JSON
DECODE-JSON-FROM-SOURCE DECODE-JSON-FROM-STRING DECODE-JSON-STRICT ENCODE-ARRAY-MEMBER ENCODE-JSON
ENCODE-JSON-ALIST ENCODE-JSON-ALIST-TO-STRING ENCODE-JSON-PLIST ENCODE-JSON-PLIST-TO-STRING
ENCODE-JSON-TO-STRING ENCODE-OBJECT-MEMBER FLUID-CLASS FLUID-OBJECT JSON-BIND JSON-BOOL JSON-INTERN
JSON-OR-NULL JSON-SYNTAX-ERROR LISP-TO-CAMEL-CASE MAKE-OBJECT MAKE-OBJECT-PROTOTYPE NO-CHAR-FOR-CODE
PASS-CODE PLACEHOLDER PROTOTYPE RATIONAL-APPROXIMATION SAFE-JSON-INTERN SET-CUSTOM-VARS
SET-DECODER-SIMPLE-CLOS-SEMANTICS SET-DECODER-SIMPLE-LIST-SEMANTICS SIMPLIFIED-CAMEL-CASE-TO-LISP
STREAM-ARRAY-MEMBER-ENCODER STREAM-OBJECT-MEMBER-ENCODER SUBSTITUTE-CHAR
SUBSTITUTE-PRINTED-REPRESENTATION UNENCODABLE-VALUE-ERROR UNKNOWN-SYMBOL-ERROR USE-EXPLICIT-ENCODER
USE-GUESSING-ENCODER WITH-ARRAY WITH-CUSTOM-DECODER-LEVEL WITH-DECODER-SIMPLE-CLOS-SEMANTICS
WITH-DECODER-SIMPLE-LIST-SEMANTICS WITH-EXPLICIT-ENCODER WITH-GUESSING-ENCODER
WITH-LOCAL-CLASS-REGISTRY WITH-OBJECT WITH-SHADOWED-CUSTOM-VARS
WITH-SUBSTITUTE-PRINTED-REPRESENTATION-RESTART
nil


(ql:quickload "babel")
(ql:quickload "split-sequence")
(ql:quickload "com.informatimago.clext.gitlab")

(drakma:http-request "http://www.informatimago.com/")
(drakma:http-request "https://gitlab.sbde.fr/")


(setf *verbose* nil)
(setf *verbose* t)


|#

;;;; THE END ;;;;
