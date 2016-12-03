(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(progn   (defmacro comment (&rest ignore)
           (declare (ignore ignore))
           (values))
         (comment "
This file demonstrate how one can add a command
dynamically to sedit, thus demonstrating how sedit
is an emacs. ;-)
")
         (defun previous-cell (list cell)
         (loop :for current :on list
               :until (eq (cdr current) cell)
               :finally (return current)))

         (defun sedit-swap (buffer)
           "swaps the expression before the selection with the selection."
           (with-buffer (buffer root selection)
                        (let ((parent
                               (selection-parent-list selection))
                              (selection-cell
                               (sedit-find-object root selection)))
                          (if (eq (car selection-cell) selection)
                              (let ((previous-cell
                                     (previous-cell
                                      parent
                                      selection-cell)))
                                (when
                                 (and previous-cell selection-cell)
                                 (rotatef
                                  (car previous-cell)
                                  (car selection-cell))))
                              (progn (comment "selection is a cdr.")
                                     (rotatef
                                      (car selection-cell)
                                      (cdr selection-cell)))))))
         (add-command 'w
                      'swap
                      'sedit-swap
                      "swaps the expression before with the selection."))
