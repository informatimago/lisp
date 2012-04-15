
(push (setf (ccl:current-directory)
            (setf *default-pathname-defaults* #P"/home/pjb/works/abalone-macosx/objcl/"))
      asdf:*central-registry*)


(cl-user::asdf-load :com.informatimago.objcl)
(in-package :cocoa-playground)
(objcl:set-objective-cl-syntax)

;; (asdf:oos 'asdf:load-op :com.informatimago.objc)



(progn
  (cl-user::asdf-delete-system  :com.informatimago.objcl)
  (delete-package :COM.INFORMATIMAGO.OBJECTIVE-CL)
  (delete-package :COM.INFORMATIMAGO.OBJECTIVE-C.LOWER)
  (delete-package :COM.INFORMATIMAGO.SIMPLE-TEST)
  (cl-user::asdf-load :com.informatimago.objcl))



(X86-DARWIN64::|class_getInstanceMethod|
               (CCL:WITH-CSTRS ((class-name "NSView")) (X86-DARWIN64::|objc_lookUpClass|  class-name))
               (objc:\@selector "initWithFrame:"))
#<A Foreign Pointer #x7FFF70E5AE58>



(cl:in-package :cl-user)
(map nil (function delete-file)
     (directory  #P"~/.cache/common-lisp/galatea.lan.informatimago.com/ccl-1.6-f94-macosx-amd64/home/pjb/src/git/public/lisp/objcl/*.*"))
(cd #P"/home/pjb/src/public/lisp/objcl/")
(push #P"/home/pjb/src/public/lisp/objcl/" asdf:*central-registry*)
(asdf-load :com.informatimago.objcl)
(asdf-load :com.informatimago.cocoa-playground)
(in-package :cocoa-playground)
(objcl:set-objective-cl-syntax)


(every (lambda (x) (typep x 'ns:ns-string))    '(@"summer"  @"été"  @"καλοκαίρι"))
'(@"summer"  @"été"  @"καλοκαίρι")
(mapcar (lambda (str) (cons str (COM.INFORMATIMAGO.OBJECTIVE-CL:LISP-STRING str)))
                          '(@"summer"  @"été"  @"καλοκαίρι"))
