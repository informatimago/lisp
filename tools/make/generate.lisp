(in-package "COMMON-LISP-USER")

;; Remove used package from CL-USER to avoid conflicts.
;; Note: this doesn't remove directly imported symbols.
(mapc (lambda (package) (unuse-package package "COMMON-LISP-USER"))
      (set-difference
       (copy-seq (package-use-list "COMMON-LISP-USER"))
       (delete nil (list ;; A list of all the "CL" packages possible:
                         (find-package "COMMON-LISP")))))

;;; --------------------------------------------------------------------
;;; Utilities
;;;

(defun say (format-control &rest arguments)
  (format t "~&;;; ~?~%" format-control arguments)
  (finish-output))

(defun runtime-function (name) (read-from-string name))
(defun runtime-symbol   (name) (read-from-string name))
(defun runtime-value    (name) (symbol-value (read-from-string name)))
(defun (setf runtime-value) (new-value name) (setf (symbol-value (read-from-string name)) new-value))

(defun load-quicklisp ()
  (say "Loading quicklisp.")
  (load #P"~/quicklisp/setup.lisp")
  (setf (runtime-value "QUICKLISP-CLIENT:*QUICKLOAD-VERBOSE*") t))

(defun configure-asdf-directories (directories &key append)
  (if (member :asdf3 *features*)
      (funcall (runtime-function "ASDF:INITIALIZE-SOURCE-REGISTRY")
               `(:source-registry
                 :ignore-inherited-configuration
                 ,@(mapcar (lambda (dir) `(:directory ,dir))
                           (remove-duplicates directories :test (function equalp)))
                 ,@(when append `(:default-registry))))
      (setf (runtime-value "ASDF:*CENTRAL-REGISTRY*")
            (remove-duplicates (if append
                                   (append directories (runtime-value "ASDF:*CENTRAL-REGISTRY*"))
                                   directories)
                               :test (function equalp)))))

(defun not-implemented-yet (what)
  (error "~S is not implemented yet on ~A, please provide a patch!"
         what (lisp-implementation-type)))

#-(and)
(defun program-path ()
  (let* ((argv  (ext:argv))
         (largv (length argv))
         (args  ext:*args*)
         (largs (length args))
         (index (- largv largs 1))
         (path  (and (<= 0 index largv) (elt argv index))))
    (cond
      (path
       path)
      ((and *load-truename*
            (string/= (file-namestring *load-truename*) "script.lisp"))
       (namestring *load-truename*))
      (t
       *default-program-name*))))

(defun argv ()
  #+ccl   ccl:*command-line-argument-list*
  #+clisp (cons (elt (ext:argv) 0) ext:*args*)
  #+ecl   (si::command-args)
  #+sbcl  sb-ext:*posix-argv*
  #-(or ccl clisp ecl sbcl) (not-implemented-yet 'exit))

(defun exit (status)
  #+ccl   (ccl:quit status)
  #+clisp (ext:quit status)
  #+ecl   (ext:quit status)
  #+sbcl  (sb-ext:exit :code status)
  #-(or ccl clisp ecl sbcl) (not-implemented-yet 'exit))

(defun make-toplevel-function (main-function-name init-file)
  (let ((form `(lambda ()
                 (handler-case
                     (progn
                       ,@(when init-file
                           `((load ,init-file :if-does-not-exist nil)))
                       (apply (read-from-string ,main-function-name)
                              #+ecl (si::command-args) #-ecl (argv)))
                   (error (err)
			         (ignore-errors (finish-output *standard-output*))
                     (ignore-errors (finish-output *trace-output*))
                     (ignore-errors (format *error-output* "~%~A~%" err)
                                    (finish-output *error-output*))
                     #+ecl (ext:quit 1) #-ecl (exit 1)))
	             (ignore-errors (finish-output *standard-output*))
	             (ignore-errors (finish-output *trace-output*))
                 #+ecl (ext:quit 0) #-ecl (exit 0))))
    #+ecl (list form)
    #-ecl (coerce form 'function)))

(defun system-cl-source-files (system)
  (let ((system (funcall (runtime-function "ASDF:FIND-SYSTEM") system)))
    (remove-duplicates
     (append
      (mapcar (runtime-function "ASDF:COMPONENT-PATHNAME")
              (remove-if-not (lambda (component) (typep component (runtime-symbol "ASDF:CL-SOURCE-FILE")))
                             (funcall (runtime-function "ASDF:COMPONENT-CHILDREN") system)))
      (mapcan (function system-cl-source-files)
              (delete-duplicates
               (mapcan (lambda (depend) (copy-list (funcall (runtime-function depend) system)))
                       '("ASDF:SYSTEM-DEFSYSTEM-DEPENDS-ON"
                         "ASDF:SYSTEM-DEPENDS-ON"
                         "ASDF:SYSTEM-WEAKLY-DEPENDS-ON"))
               :test (function equal))))
     :test (function equal))))

(defun system-object-files (system)
  (let ((system (funcall (runtime-function "ASDF:FIND-SYSTEM") system)))
    (remove-duplicates
     (append
      (mapcan (lambda (component) (copy-list (funcall (runtime-function "ASDF:OUTPUT-FILES")
                                                      (runtime-symbol "ASDF:COMPILE-OP")
                                                      component)))
              (funcall (runtime-function "ASDF:COMPONENT-CHILDREN") system))
      (mapcan (function system-object-files)
              (delete-duplicates
               (mapcan (lambda (depend) (copy-list (funcall (runtime-function depend) system)))
                       '("ASDF:SYSTEM-DEFSYSTEM-DEPENDS-ON"
                         "ASDF:SYSTEM-DEPENDS-ON"
                         "ASDF:SYSTEM-WEAKLY-DEPENDS-ON"))
               :test (function equal))))
     :test (function equal))))

(defun slurp-stream (stream)
  (with-output-to-string (*standard-output*)
    (loop
      :for line := (read-line stream nil)
      :while line
      :do (write-line line))))

#+ecl
(defun pre-compile-with-quicklisp (program-name main-function
                                   system-name system-list
                                   source-directory asdf-directories release-directory
                                   init-file version copyright)
  (declare (ignorable program-name main-function
                      system-name system-list
                      source-directory asdf-directories release-directory
                      init-file version copyright))
  (say "Quickloading ~S" (cons system-name system-list))
  (multiple-value-bind (output status)
      (ext:run-program (first (argv))
                       (list "--norc" "--load" "generate.lisp"
                             "--eval" (prin1-to-string `(progn (require 'cmp)
                                                               (load-quicklisp)
                                                               (configure-asdf-directories ',asdf-directories)
                                                               (funcall (runtime-function "QL:QUICKLOAD") ',(cons system-name system-list)))))
                       :input nil
                       :output :stream
                       :wait t)
    (unless (zerop status)
      (write-string (slurp-stream output)))
    (say "   status ~S" status)))

(defun generate-program (&key program-name main-function
                           system-name system-list
                           source-directory asdf-directories release-directory
                           init-file version copyright)
  (declare (ignorable release-directory init-file
                      system-name system-list
                      version copyright source-directory))

  #+ecl (pre-compile-with-quicklisp program-name main-function
                                    system-name system-list
                                    source-directory asdf-directories release-directory
                                    init-file version copyright)

  #-ecl (load-quicklisp)
  #+ecl (progn (require 'cmp)
               (say "Requiring ASDF")
               (require 'asdf))

  (configure-asdf-directories asdf-directories)

  #-ecl (progn
          (say "Quickloading system ~A" system-name)
          (funcall (runtime-function "QL:QUICKLOAD") system-name)
          (when system-list
            (say "Quickloading systems ~A" system-list)
            (funcall (runtime-function "QL:QUICKLOAD") system-list)))

  (say "Generating program ~A" (merge-pathnames program-name release-directory))

  #+ccl (progn
          ;; This doesn't return.
          (ccl::save-application
           (merge-pathnames program-name release-directory  nil)
           :toplevel-function (make-toplevel-function main-function nil)
           :init-file init-file
           :mode #o755
           :prepend-kernel t
           :error-handler t))

  #+clisp (progn
            (ext::saveinitmem
             (merge-pathnames program-name release-directory  nil)
             :executable t
             :norc t
             :quiet t
             :verbose t
             :script t
             :documentation (format nil "~A version ~A~%~A~%" program-name version copyright)
             :start-package (find-package "COMMON-LISP-USER")
             ;; locked-packages
             ;; :keep-global-handlers
             :init-function (make-toplevel-function main-function init-file))
            (ext:quit 0))

  #+ecl (progn
          #-(and) (load "hw.asd")
          #-(and) (asdf:oos 'asdf:program-op system-name)

          (funcall (runtime-function "ASDF:MAKE-BUILD") system-name
                   :type :program
                   :monolithic t
                   :ld-flags '()
                   :prologue-code ""
                   :epilogue-code (make-toplevel-function main-function init-file)
                   :move-here release-directory)

          #-(and) (progn (c:build-program (merge-pathnames program-name release-directory nil)
                                          :lisp-files (system-object-files system-name)
                                          :ld-flags '()
                                          :prologue-code ""
                                          :epilogue-code (make-toplevel-function main-function init-file))

                         (rename-file
                          (make-pathname
                           :directory (append (pathname-directory
                                               (uiop/configuration::compute-user-cache))
                                              (rest (pathname-directory source-directory)))
                           :name (string-downcase system-name)
                           :type nil)
                          (merge-pathnames program-name release-directory nil)))
          (ext:quit 0))

  #+sbcl (sb-ext:save-lisp-and-die program-name
                                   :executable t
                                   :compression 9
                                   :toplevel (make-toplevel-function main-function init-file))

  #-(or ccl clisp ecl sbcl) (not-implemented-yet 'generate-program))

;;;; THE END ;;;;
