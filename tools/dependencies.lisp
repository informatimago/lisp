(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.TOOLS.DEPENDENCIES"
  (:use "COMMON-LISP"
        "QUICKLISP"
        "ASDF"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.TOOLS.ASDF"
        "COM.INFORMATIMAGO.TOOLS.QUICKLISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:shadowing-import-from "SPLIT-SEQUENCE" "SPLIT-SEQUENCE-IF")
  (:export )
  (:documentation "Manage dependencies of a system"))
(in-package "COM.INFORMATIMAGO.TOOLS.DEPENDENCIES")


         ;; (release      (ql-dist:release system))
         ;; (distribution (ql-dist:dist    system))
         ;; (dname        (ql-dist:name distribution))
         ;; (pname        (and release
         ;;                    (ql-dist:project-name release)))
         ;; (wfrom        (cond
         ;;                 ((null pname)
         ;;                  '())
         ;;                 ((string= dname "quicklisp")
         ;;                  (project-where-from pname))
         ;;                 (t
         ;;                  '())))


(defun system-dependencies (system &rest systems)
  (let ((features '()))
    (flet ((split-features (items)
             (loop :for item :in items
                   :if (stringp item) :collect item
                   :else :do (push item features))))
      (values
       (delete nil
               (transitive-closure
                (lambda (sname)
                  (let ((qsystem  (ql-dist:find-system sname))
                        (asystem  (asdf:find-system sname)))
                    (unless (or qsystem asystem)
                      (warn "System named ~S is not available." sname))
                    (delete nil
                            (nconc
                             (when qsystem (copy-list (ql-dist:required-systems qsystem)))
                             (when asystem
                               (nconc (split-features (asdf:system-depends-on asystem))
                                      (split-features (asdf:system-defsystem-depends-on asystem))))))))
                (delete-duplicates (mapcar (function string-downcase)
                                           (cons system systems))
                                   :test (function equal))
                :test (function equal)
                :use 'hash-table))
       (delete-duplicates features :test (function equalp))))))

(defvar *current-directory-namestring* "./")

(defun shell-command-to-string (control-string &rest arguments)
  (uiop:run-program (format nil "~?" control-string arguments)
                    :wait t :force-shell t
                    :output '(:string :stripped t)
                    :error-output '(:string :stripped t)
                    :ignore-error-status t
                    :directory *current-directory-namestring*))

(defun git-workspace-p (directory-pathname)
  (probe-file (merge-pathnames #P".git/index" directory-pathname)))

(defun git-workspace (directory-pathname)
  "Return the workspace toplevel directory or NIL."
  (multiple-value-bind (path error status)
      (shell-command-to-string "cd ~S ; git rev-parse --show-toplevel"
                               (namestring (truename directory-pathname)))
    (declare (ignore error))
    (when (zerop status)
      (concatenate 'string path "/"))))

(defun git-current-commit (directory-pathname)
  (let ((*current-directory-namestring* (namestring directory-pathname)))
    (multiple-value-bind (output error status)
        (shell-command-to-string "~S ~{~S~^ ~}" "git" '("rev-parse" "HEAD"))
      (unless (zerop status)
        (error "~A" error))
      output)))

(defun git-remotes (directory-pathname)
  (let ((*current-directory-namestring* (namestring directory-pathname)))
    (multiple-value-bind (output error status)
        (shell-command-to-string "~S ~{~S~^ ~}" "git" '("remote" "-v"))
      (unless (zerop status)
        (error "~A" error))
      (mapcar (lambda (line) (split-sequence-if (lambda (ch) (find ch #(#\space #\tab)))
                                           line :remove-empty-subseqs t))
              (split-sequence #\newline output)))))

(defun choose-best-git-remote (directory-pathname)
  "Choose gitlab.com over github.com over others,
and origin over other remote name (rejecting upstream),
and (fetch) over (push).
"
  (let ((remotes (sort (git-remotes directory-pathname)
                       (lambda (a b)
                         (cond
                           ((search "gitlab.com" (second a))
                            (if (search "gitlab.com" (second b))
                                (if (string= "origin" (first a))
                                    (if (string= "origin" (first b))
                                        (string= "(fetch)" (third a))
                                        nil)
                                    (if (string= "origin" (first b))
                                        nil
                                        (string= "(fetch)" (third a))))
                                t))
                           ((search "github.com" (second a))
                            (cond
                              ((search "gitlab.com" (second b))
                               nil)
                              ((search "github.com" (second b))
                               (if (string= "origin" (first a))
                                   (if (string= "origin" (first b))
                                       (string= "(fetch)" (third a))
                                       nil)
                                   (if (string= "origin" (first b))
                                       nil
                                       (string= "(fetch)" (third a)))))
                              (t
                               t)))
                           ((string= "origin" (first a))
                            (if (string= "origin" (first b))
                                (string= "(fetch)" (third a))
                                nil))
                           ((string= "upstream" (first a))
                            nil)
                           ((string/= "(fetch)" (third a))
                            nil)
                           (t
                            t))))))
    (first remotes)))

(defun get-system-git-info (sname directory)
  (let ((workspace (git-workspace directory)))
    (when (and workspace (git-workspace-p workspace))
      ;; query the git workspace at `directory' for the remote url, and the current commit
      (let ((remote (choose-best-git-remote workspace))
            (commit (git-current-commit workspace)))
        (list :system sname :directory workspace :url remote :commit commit)))))

(defun systems-info (system-names)
  (let ((dirs (make-hash-table :test (function equal)))) ; pathname keys
    (dolist (sname system-names)
      (let* ((from (com.informatimago.tools.quicklisp:system-where-from sname))
             (info (cond
                     ((getf from :where-from)
                      from)
                     ((or (null from) (eql (getf from :distribution) :local))
                      (or (get-system-git-info sname (com.informatimago.tools.quicklisp:system-where-is sname))
                          (get-system-git-info sname (asdf/system:system-relative-pathname (asdf:find-system sname) ""))
                          (progn (warn "No source for ~S" sname)
                                 (list :system sname))))
                     (t
                      (or (get-system-git-info sname (asdf/system:system-relative-pathname (asdf:find-system sname) ""))
                          (progn (warn "No source for ~S" sname)
                                 (list :system sname))))))
             (dir (getf info :directory))
             (entry (when dir (gethash dir dirs))))
        (when (or (null entry)
                  (< (length (getf info  :system))
                     (length (getf entry :system))))
          (setf (gethash dir dirs) info))))
    (let ((results '()))
      (maphash (lambda (dir info)
                 (declare (ignore dir))
                 (push info results))
               dirs)
      results)))

(defvar *all-kinds* '("LATEST-GITHUB-RELEASE"
                      "LATEST-GITHUB-TAG"
                      "branched-git"
                      "darcs"
                      "ediware-http"
                      "git"
                      "http"
                      "https"
                      "kmr-git"
                      "latest-github-release"
                      "latest-github-tag"
                      "latest-gitlab-release"
                      "mercurial"
                      "single-file"
                      "svn"
                      "tagged-git"))

(defvar *git-kinds* '("LATEST-GITHUB-RELEASE"
                      "LATEST-GITHUB-TAG"
                      "branched-git"
                      "git"
                      "kmr-git"
                      "latest-github-release"
                      "latest-github-tag"
                      "latest-gitlab-release"
                      "tagged-git"))

(defun get-dependency (info base-directory-pathname)
  "Clones the dependency specified by INFO in a subdirectory of BASE-DIRECTORY-PATHNAME."
  (declare (ignore info base-directory-pathname))
  (error "Not implemented yet")
  #-(and)
  (let ((w (getf info :where-from)))
    (scase (first w)
      (("LATEST-GITHUB-RELEASE"
        "latest-github-release"
        "latest-gitlab-release"))
      (("LATEST-GITHUB-TAG"
        "latest-github-tag"))
      (("tagged-git"))
      (("branched-git"))
      (("git"))
      (("darcs"))
      (("ediware-http"))
      (("http"))
      (("https"))
      (("kmr-git"))
      (("mercurial"))
      (("single-file"))
      (("svn"))
      )))



(defvar *hunchentoot-deps*)

(defun process-hunchentoot ()
  (setf *hunchentoot-deps* (system-dependencies :hunchentoot))
  (pprint (systems-info *hunchentoot-deps*)))


(defvar *uu-spa-deps*)

(defun process-uu-spa ()
  (setf *uu-spa-deps* (system-dependencies :uu-spa))
  (pprint (systems-info *uu-spa-deps*)))
