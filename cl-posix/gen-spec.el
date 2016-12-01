
(defun c-name->lisp-name (c-name kind)
  (let ((lisp-name (substitute  (character "-") (character "_")
                          (string-upcase (string c-name)))))
    (if (eq :constant kind)
      (format "+%s+" lisp-name)
      lisp-name)
    ));;c-name->lisp-name


(defun generate-type (desc)
  (let ((c-name (first desc))
        (options (second desc))
        (description (third desc))
        (supertype (fourth desc))
        (assertions (fifth desc))
        (lisp-name)
        (pack-ext))
    (setq lisp-name (c-name->lisp-name c-name :type))
    (setq pack-ext
          (cond
           ((null options) "")
           ((= 1 (length options))
            (format "-%s" (car options)))
           ((eq 'AND (car options))
            (format "-%s" (unsplit-string
                           (mapcar (function string) (cdr options) )
                           "-")))
           ((eq 'OR (car options))
            (format "-{%s}"  (unsplit-string
                              (mapcar (function string) (cdr options))
                              ",")))))
    (insert (format "<h4>Type SUSV3%s:%s</h4>\n\n" pack-ext lisp-name))
    (insert (format "<pre>\n   (DEFTYPE SUSV3%s:%s ()\n     \"%s\"\n"
                    pack-ext lisp-name description))
    (insert (format "     ;; implementation dependent\n    );;SUSV3%s:%s\n\n"
                    pack-ext lisp-name))
    (when supertype
      (push (format "(SUBTYPEP 'SUSV3%s:%s '%s)" pack-ext lisp-name supertype)
            assertions))
    (when assertions
      (insert "   (ASSERT ")
      (if (= 1 (length assertions))
        (insert (format "%s" (car assertions)))
        (progn
          (insert (format "(AND %s" (car assertions)))
          (dolist (ass (cdr assertions))
            (insert (format "\n                %s" ass)))
          (insert "))\n"))
        (insert ")\n")))
    (insert "</pre>\n\n")
    ));;generate-type


(dolist
    (desc
     '(
       (blkcnt_t
        ()
        "Used for file block counts."
        INTEGER)

       (blksize_t
        ()
        "Used for block sizes."
        INTEGER)

       (clock_t
        (XSI)
        "Used for system times in clock ticks or CLOCKS_PER_SEC; see <time.h> ."
        (OR INTEGER FLOAT))

       (clockid_t
        (TMR)
        "Used for clock ID type in the clock and timer functions.")

       (dev_t
        ()
        "Used for device IDs.")

       (fsblkcnt_t
        (XSI)
        "Used for file system block counts."
        (INTEGER 0))


       (fsfilcnt_t
        (XSI)
        "Used for file system file counts."
        (INTEGER 0))

       (gid_t
        ()
        "Used for group IDs."
        INTEGER)

       (id_t
        (XSI)
        "Used as a general identifier; can be used to contain at least a pid_t, uid_t, or gid_t."
        INTEGER
        ("(SUBTYPEP 'SUSV3-XSI:ID-T 'SUSV3:PID-T)"
         "(SUBTYPEP 'SUSV3-XSI:ID-T 'SUSV3:UID-T)"
         "(SUBTYPEP 'SUSV3-XSI:ID-T 'SUSV3:GID-T)"))

       (ino_t
        ()
        "Used for file serial numbers."
        (INTEGER 0))

       (key_t
        (XSI)
        "Used for XSI interprocess communication.")

       (mode_t
        ()
        "Used for some file attributes."
        INTEGER)

       (nlink_t
        ()
        "Used for link counts."
        INTEGER)

       (off_t
        ()
        "Used for file sizes."
        INTEGER)

       (pid_t
        ()
        "Used for process IDs and process group IDs."
        INTEGER)

       (pthread_attr_t
        (THR)
        "Used to identify a thread attribute object.")

       (pthread_barrier_t
        (BAR)
        "Used to identify a barrier.")

       (pthread_barrierattr_t
        (BAR)
        "Used to define a barrier attributes object.")

       (pthread_cond_t
        (THR)
        "Used for condition variables.")

       (pthread_condattr_t
        (THR)
        "Used to identify a condition attribute object.")

       (pthread_key_t
        (THR)
        "Used for thread-specific data keys.")

       (pthread_mutex_t
        (THR)
        "Used for mutexes.")

       (pthread_mutexattr_t
        (THR)
        "Used to identify a mutex attribute object.")

       (pthread_once_t
        (THR)
        "Used for dynamic package initialization.")

       (pthread_rwlock_t
        (THR)
        "Used for read-write locks.")

       (pthread_rwlockattr_t
        (THR)
        "Used for read-write lock attributes.")

       (pthread_spinlock_t
        (SPI)
        "Used to identify a spin lock.")

       (pthread_t
        (THR)
        "Used to identify a thread.")

       (size_t
        ()
        "Used for sizes of objects."
        (INTEGER 0))

       (ssize_t
        ()
        "Used for a count of bytes or an error indication."
        INTEGER
        ("(SUBTYPEP (INTEGER -1 SUSV3:SSIZE-MAX) SUSV3:SSIZE-T)"))

       (suseconds_t
        (XSI)
        "Used for time in microseconds.")

       (time_t
        ()
        "Used for time in seconds."
        (OR INTEGER FLOAT))

       (timer_t
        (TMR)
        "Used for timer ID returned by timer_create().")

       (trace_attr_t
        (TRC)
        "Used to identify a trace stream attributes object.")

       (trace_event_id_t
        (TRC)
        "Used to identify a trace event type.")

       (trace_event_set_t
        (AND  TRC TEF)
        "Used to identify a trace event type set.")

       (trace_id_t
        (TRC)
        "Used to identify a trace stream.")

       (uid_t
        ()
        "Used for user IDs."
        INTEGER)

       (useconds_t
        (XSI)
        "Used for time in microseconds.")
       ))

  (generate-type desc))


;;;; gen-spec.el                      -- 2003-06-14 09:40:40 -- pascal   ;;;;
