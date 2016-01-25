;;;; -*- mode:lisp; coding:utf-8 -*-

(asdf:defsystem :com.informatimago.common-lisp.virtual-file-system
    :name "Virtual File System"
    :description  "Implements a RAM-based Virtual File System."
    :author "<PJB> Pascal Bourguignon <pjb@informatimago.com>"
    :version "1.2.0"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2011")
                 ((#:albert #:output-dir)          . "../documentation/com.informatimago.vfs/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on (:split-sequence
                 :cl-ppcre
                 :com.informatimago.common-lisp.cesarum)
    :components ((:file "packages")
                 (:file "utility"              :depends-on ("packages"))
                 (:file "filenames"            :depends-on ("utility"))
                 (:file "streams"              :depends-on ("utility"))

                 (:file "virtual-fs"           :depends-on ("filenames"))
                 (:file "files"                :depends-on ("streams" "filenames" "virtual-fs"))
                 (:file "vfs-file-stream"      :depends-on ("streams" "filenames" "virtual-fs"))
                 
                 (:file "string-input"         :depends-on ("streams" "filenames"))
                 (:file "string-output"        :depends-on ("streams" "filenames"))
                 (:file "concatenated-stream"  :depends-on ("streams" "filenames"))
                 (:file "broadcast-stream"     :depends-on ("streams" "filenames"))
                 (:file "synonym-stream"       :depends-on ("streams" "filenames"))
                 (:file "two-way-stream"       :depends-on ("streams" "filenames"))
                 (:file "echo-stream"          :depends-on ("streams" "filenames"))
                 (:file "standard-streams"     :depends-on ("string-input" "string-output" "two-way-stream"))
                 (:file "cl-stream"            :depends-on ("standard-streams"))
                 (:file "general"              :depends-on ("streams" "filenames" "files"))
                 ;; ---
                 (:file "initialize"           :depends-on ("cl-stream" "virtual-fs"))
                 ))


;;;; THE END ;;;;
