


(defun files-in-directory (directory file-type recursive)
  "Return a list of paths relative to `directory' of the files in the `directory'.
`file-type' is the extension of the files returned.
When `recursive' appends also the files in the sub-directory and in
the sub-sub-directory.
"
  (mapcar (lambda (path)
            (if (string= (subseq path 0 (length directory))
                         directory)
                (subseq path (length directory))
                path))
          (append (file-expand-wildcards (format "%s*.%s" directory file-type))
                  (when recursive
                    (append
                     (file-expand-wildcards (format "%s*%s*.%s" directory df file-type))
                     (file-expand-wildcards (format "%s*%s*%s*.%s" directory df df file-type)))))))


(defun make-component-list (paths &optional dependencies)
  (mapcar (lambda (path)
            `(:file ,path ,@(let ((entry (assoc* path dependencies :test (function string=))))
                              (when entry (cdr entry)))))
          paths))


(defvar *source-file-type* "lisp")


(defun insert-component-list (&optional recursive)
  (interactive "P")
  (loop with directory  = (file-name-directory  (buffer-file-name))
        with df = (subseq (file-name-as-directory directory)
                          (length (directory-file-name directory)))
        for component in (make-component-list
                          (mapcar (function file-name-sans-extension)
                                  (files-in-directory directory *source-file-type* recursive)))
        for sep = "(" then "\n               "
          initially (insert ":components ")
        finally (insert ")\n")
        do (insert (format "%s%S" sep component))))


