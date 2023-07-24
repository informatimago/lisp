(defpackage "COM.INFORMATIMAGO.TOOLS.PATCH"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"))
(in-package "COM.INFORMATIMAGO.TOOLS.PATCH")


(defstruct patch
  name
  description
  files) ;; diff -- …/ +++ … / --- … section 

(defstruct file
  old
  new
  hunks) ;; @@ … @@ … section

(defstruct hunk
  ;; @@ … @@ … section
  header
  lines)


(defmethod equals ((a file) (b file))
  (and (equal (file-old a) (file-old b))
       (equal (file-new a) (file-new b))
       (= (length (file-hunks a)) (length (file-hunks b)))
       (every (function equals) (file-hunks a) (file-hunks b))))
       
(defmethod equals ((a hunk) (b hunk))
  (and (equal (hunk-header a) (hunk-header b))
       ;; (= (length (hunk-lines a)) (length (hunk-lines b)))
       ;; (every (function equal) (hunk-lines a) (hunk-lines b))
       ))

(defmethod print-object ((patch patch) stream)
  (print-unreadable-object (patch stream :type t :identity t)
    (format stream "~S~{~&~A~}"
            (patch-name patch)
            (patch-files patch)))
  patch)

(defmethod print-object ((file file) stream)
  (print-unreadable-object (file stream :type t :identity t)
    (format stream "~S~{~&~A~}"
            (subseq (file-old file) 4)
            (file-hunks file)))
  file)

(defmethod print-object ((hunk hunk) stream)
  (print-unreadable-object (hunk stream :type t :identity t)
    (let ((nlines (length (hunk-lines hunk))))
      (format stream "~S ~A line~P" (hunk-header hunk) nlines (/= 1 nlines))))
  hunk)


(defgeneric serialize (object stream))

(defmethod serialize ((patch patch) stream)
  (write-string (patch-description patch) stream)
  (dolist (file (patch-files patch))
    (serialize file stream))
  patch)

(defmethod serialize ((file file) stream)
  (write-line (file-old file) stream)
  (write-line (file-new file) stream)
  (dolist (hunk (file-hunks file))
    (serialize hunk stream))
  file)

(defmethod serialize ((hunk hunk) stream)
  (write-line (hunk-header hunk) stream)
  (dolist (line (hunk-lines hunk))
    (write-line line stream))
  hunk)

(defun split-hunks (lines)
  (loop :with hunks := '()
        :while (and lines
                    (prefixp "@@ " (first lines)))
        :do (push (make-hunk :header (pop lines)
                             :lines (loop :for line :in lines
                                          :while (and (not (or (prefixp "@@ " line)
                                                               (prefixp "--- " line)
                                                               (prefixp "+++ " line)))
                                                      (or (prefixp " " line)
                                                          (prefixp "-" line)
                                                          (prefixp "+" line)))
                                          :collect (pop lines)))
                  hunks)
        :finally (return (values (nreverse hunks) lines))))

(defun split-files (lines)
  (when (prefixp "diff --" (first lines))
    (pop lines))
  (loop :with files := '()
        :while (and lines
                    (prefixp "--- " (first lines))
                    (prefixp "+++ " (second lines)))
        :do (push (make-file :old (pop lines)
                             :new (pop lines)
                             :hunks (multiple-value-bind (hunks rest-lines)
                                        (split-hunks lines)
                                      (setf lines rest-lines)
                                      hunks))
                  files)
        :finally (return (nreverse files))))

(defun load-patch-file (pathname)
  (loop :with lines := (string-list-text-file-contents pathname)
        :for line := (and lines (pop lines))
        :while (and line (prefixp " " line))
        :collect line :into description-lines
        :finally (return (make-patch :name (namestring pathname)
                                     :description  (format nil "~{~A~%~}" description-lines)
                                     :files (split-files (cons line lines))))))



(defun diff-hunks (a b)
  "Return the hunks from the list A that are not in the list B."
  ;; Since hunks are sorted, we could be more efficient processing them sequencially.
  (set-difference a b :test (function equals)))


(defun diff-patches (a b)
  "Return a PATCH with only the hunks in A that are not in B
= A-B
files that have identical hunks in both are removed.
"
  (let ((a-files (patch-files a))
        (b-files (patch-files b)))
    (make-patch :name (format nil "diff-~A-~A" (patch-name a) (patch-name b))
                :description (format nil " diff-~A-~A~%~A~%~A"
                                     (patch-name a) (patch-name b)
                                     (patch-description a) (patch-description b))
                :files (loop :for a-file :in (loop ;; remove identical files
                                                   :for a-file :in a-files
                                                   :for b-file := (find a-file b-files :test (function equals))
                                                   :unless b-file :collect a-file)
                             :for b-file := (find (file-old a-file) b-files :key (function file-old) :test 'equal)
                             :if b-file :collect (make-file :old (file-old a-file)
                                                            :new (file-new a-file)
                                                            :hunks (diff-hunks (file-hunks a-file) (file-hunks b-file)))
                             :else :collect a-file))))

(defun save-patch-file (patch &optional pathname)
  (with-open-file (out (or pathname (patch-name patch))
                        :if-does-not-exist :create
                        :if-exists :supersede
                        :direction :output)
    (serialize patch out))
  patch)


#-(and)
(progn


  (load-patch-file "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1-o.patch")
  
  (diff-patches
   (load-patch-file "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1-o.patch")
   (load-patch-file "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1.patch"))




  (setf (text-file-contents "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1-r.patch")
        (with-output-to-string (out)
          (serialize (diff-patches
                      (load-patch-file "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1-o.patch")
                      (load-patch-file "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1.patch"))
                     out)))

                                        ;
  (save-patch-file (load-patch-file "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1-o.patch") ;
                   "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1-o-2.patch") ;


  (let ((p  (load-patch-file "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1-o.patch")))
    ;; keep only the hunks that contain "+.*VAR" or "+.*FLET" or "+.*};"
    (save-patch-file
     (let ((f
             (make-patch :name "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1-f.patch"
                         :description (patch-description p)
                         :files (loop :for f :in (patch-files p)
                                      :for remaining-hunks := (loop :for h :in (file-hunks f)
                                                                    :if (loop :for l :in (hunk-lines h)
                                                                              :thereis (or (and (prefixp "+" l) (search "VAR" l))
                                                                                           (and (prefixp "+" l) (search "FLET" l))
                                                                                           (and (prefixp "+" l) (search "};" l))))
                                                                    :collect h)
                                      :when remaining-hunks
                                      :collect (make-file :old (file-old f)
                                                          :new (file-new f)
                                                          :hunks remaining-hunks)))))
       (save-patch-file f)
       (save-patch-file (diff-patches p f)
                        "/build/pbourguignon/clang/src.devel/hypervisor/tests/unit/ceedling/nkernel/1-r.patch"))))


  ) ;; progn


