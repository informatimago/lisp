
(push (setf (ccl:current-directory)
            (setf *default-pathname-defaults* #p"/home/pjb/works/abalone-macosx/objcl/"))
      asdf:*central-registry*)


(cl-user::asdf-load :com.informatimago.objcl)
(in-package :cocoa-playground)
(objcl:set-objective-cl-syntax)

;; (asdf:oos 'asdf:load-op :com.informatimago.objc)



(progn
  (cl-user::asdf-delete-system  :com.informatimago.objcl)
  (delete-package :com.informatimago.objective-cl)
  (delete-package :com.informatimago.objective-c.lower)
  (delete-package :com.informatimago.simple-test)
  (cl-user::asdf-load :com.informatimago.objcl))



(x86-darwin64::|class_getInstanceMethod|
               (ccl:with-cstrs ((class-name "NSView")) (x86-darwin64::|objc_lookUpClass|  class-name))
               (objc:\@selector "initWithFrame:"))
#<a foreign pointer #x7fff70e5ae58>



(cl:in-package :cl-user)
(map nil (function delete-file)
     (directory  #p"~/.cache/common-lisp/galatea.lan.informatimago.com/ccl-1.6-f94-macosx-amd64/home/pjb/src/git/public/lisp/objcl/*.*"))
(cd #p"/home/pjb/src/public/lisp/objcl/")
(push #p"/home/pjb/src/public/lisp/objcl/" asdf:*central-registry*)
(asdf-load :com.informatimago.objcl)
(asdf-load :com.informatimago.cocoa-playground)
(in-package :cocoa-playground)
(objcl:set-objective-cl-syntax)


(every (lambda (x) (typep x 'ns:ns-string))    '(@"summer"  @"été"  @"καλοκαίρι"))
'(@"summer"  @"été"  @"καλοκαίρι")
(mapcar (lambda (str) (cons str (com.informatimago.objective-cl:lisp-string str)))
                          '(@"summer"  @"été"  @"καλοκαίρι"))
