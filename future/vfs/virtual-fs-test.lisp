(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(print
 (mapcar (lambda (path) (cons path (parse-logical-pathname path)))
         (list ""

               "HOST:;"
               "HOST:;*;"
               "HOST:;**;"
               "HOST:;DIR;"
               "HOST:;DIR1;DIR2;DIR3;"
               "HOST:;DIR1;*;DIR3;"
               "HOST:;DIR1;**;DIR3;"
               "HOST:"
               "HOST:*;"
               "HOST:**;"
               "HOST:DIR;"
               "HOST:DIR1;DIR2;DIR3;"
               "HOST:DIR1;*;DIR3;"
               "HOST:DIR1;**;DIR3;"

               "HOST:;NAME"
               "HOST:;*;NAME"
               "HOST:;**;NAME"
               "HOST:;DIR;NAME"
               "HOST:;DIR1;DIR2;DIR3;NAME"
               "HOST:;DIR1;*;DIR3;NAME"
               "HOST:;DIR1;**;DIR3;NAME"
               "HOST:"
               "HOST:*;NAME"
               "HOST:**;NAME"
               "HOST:DIR;NAME"
               "HOST:DIR1;DIR2;DIR3;NAME"
               "HOST:DIR1;*;DIR3;NAME"
               "HOST:DIR1;**;DIR3;NAME"

               "HOST:;*"
               "HOST:;*;*"
               "HOST:;**;*"
               "HOST:;DIR;*"
               "HOST:;DIR1;DIR2;DIR3;*"
               "HOST:;DIR1;*;DIR3;*"
               "HOST:;DIR1;**;DIR3;*"
               "HOST:"
               "HOST:*;*"
               "HOST:**;*"
               "HOST:DIR;*"
               "HOST:DIR1;DIR2;DIR3;*"
               "HOST:DIR1;*;DIR3;*"
               "HOST:DIR1;**;DIR3;*"


               "HOST:;.TYPE"
               "HOST:;*;.TYPE"
               "HOST:;**;.TYPE"
               "HOST:;DIR;.TYPE"
               "HOST:;DIR1;DIR2;DIR3;.TYPE"
               "HOST:;DIR1;*;DIR3;.TYPE"
               "HOST:;DIR1;**;DIR3;.TYPE"
               "HOST:"
               "HOST:*;.TYPE"
               "HOST:**;.TYPE"
               "HOST:DIR;.TYPE"
               "HOST:DIR1;DIR2;DIR3;.TYPE"
               "HOST:DIR1;*;DIR3;.TYPE"
               "HOST:DIR1;**;DIR3;.TYPE"

               "HOST:;NAME.TYPE"
               "HOST:;*;NAME.TYPE"
               "HOST:;**;NAME.TYPE"
               "HOST:;DIR;NAME.TYPE"
               "HOST:;DIR1;DIR2;DIR3;NAME.TYPE"
               "HOST:;DIR1;*;DIR3;NAME.TYPE"
               "HOST:;DIR1;**;DIR3;NAME.TYPE"
               "HOST:"
               "HOST:*;NAME.TYPE"
               "HOST:**;NAME.TYPE"
               "HOST:DIR;NAME.TYPE"
               "HOST:DIR1;DIR2;DIR3;NAME.TYPE"
               "HOST:DIR1;*;DIR3;NAME.TYPE"
               "HOST:DIR1;**;DIR3;NAME.TYPE"

               "HOST:;*.TYPE"
               "HOST:;*;*.TYPE"
               "HOST:;**;*.TYPE"
               "HOST:;DIR;*.TYPE"
               "HOST:;DIR1;DIR2;DIR3;*.TYPE"
               "HOST:;DIR1;*;DIR3;*.TYPE"
               "HOST:;DIR1;**;DIR3;*.TYPE"
               "HOST:"
               "HOST:*;*.TYPE"
               "HOST:**;*.TYPE"
               "HOST:DIR;*.TYPE"
               "HOST:DIR1;DIR2;DIR3;*.TYPE"
               "HOST:DIR1;*;DIR3;*.TYPE"
               "HOST:DIR1;**;DIR3;*.TYPE"



               "HOST:;.TYPE.123"
               "HOST:;*;.TYPE.123"
               "HOST:;**;.TYPE.123"
               "HOST:;DIR;.TYPE.123"
               "HOST:;DIR1;DIR2;DIR3;.TYPE.123"
               "HOST:;DIR1;*;DIR3;.TYPE.123"
               "HOST:;DIR1;**;DIR3;.TYPE.123"
               "HOST:"
               "HOST:*;.TYPE.123"
               "HOST:**;.TYPE.123"
               "HOST:DIR;.TYPE.123"
               "HOST:DIR1;DIR2;DIR3;.TYPE.123"
               "HOST:DIR1;*;DIR3;.TYPE.123"
               "HOST:DIR1;**;DIR3;.TYPE.123"

               "HOST:;NAME.TYPE.123"
               "HOST:;*;NAME.TYPE.123"
               "HOST:;**;NAME.TYPE.123"
               "HOST:;DIR;NAME.TYPE.123"
               "HOST:;DIR1;DIR2;DIR3;NAME.TYPE.123"
               "HOST:;DIR1;*;DIR3;NAME.TYPE.123"
               "HOST:;DIR1;**;DIR3;NAME.TYPE.123"
               "HOST:"
               "HOST:*;NAME.TYPE.123"
               "HOST:**;NAME.TYPE.123"
               "HOST:DIR;NAME.TYPE.123"
               "HOST:DIR1;DIR2;DIR3;NAME.TYPE.123"
               "HOST:DIR1;*;DIR3;NAME.TYPE.123"
               "HOST:DIR1;**;DIR3;NAME.TYPE.123"

               "HOST:;*.TYPE.123"
               "HOST:;*;*.TYPE.123"
               "HOST:;**;*.TYPE.123"
               "HOST:;DIR;*.TYPE.123"
               "HOST:;DIR1;DIR2;DIR3;*.TYPE.123"
               "HOST:;DIR1;*;DIR3;*.TYPE.123"
               "HOST:;DIR1;**;DIR3;*.TYPE.123"
               "HOST:"
               "HOST:*;*.TYPE.123"
               "HOST:**;*.TYPE.123"
               "HOST:DIR;*.TYPE.123"
               "HOST:DIR1;DIR2;DIR3;*.TYPE.123"
               "HOST:DIR1;*;DIR3;*.TYPE.123"
               "HOST:DIR1;**;DIR3;*.TYPE.123"


               "HOST:;.TYPE.newest"
               "HOST:;*;.TYPE.newest"
               "HOST:;**;.TYPE.newest"
               "HOST:;DIR;.TYPE.newest"
               "HOST:;DIR1;DIR2;DIR3;.TYPE.newest"
               "HOST:;DIR1;*;DIR3;.TYPE.newest"
               "HOST:;DIR1;**;DIR3;.TYPE.newest"
               "HOST:"
               "HOST:*;.TYPE.newest"
               "HOST:**;.TYPE.newest"
               "HOST:DIR;.TYPE.newest"
               "HOST:DIR1;DIR2;DIR3;.TYPE.newest"
               "HOST:DIR1;*;DIR3;.TYPE.newest"
               "HOST:DIR1;**;DIR3;.TYPE.newest"

               "HOST:;NAME.TYPE.newest"
               "HOST:;*;NAME.TYPE.newest"
               "HOST:;**;NAME.TYPE.newest"
               "HOST:;DIR;NAME.TYPE.newest"
               "HOST:;DIR1;DIR2;DIR3;NAME.TYPE.newest"
               "HOST:;DIR1;*;DIR3;NAME.TYPE.newest"
               "HOST:;DIR1;**;DIR3;NAME.TYPE.newest"
               "HOST:"
               "HOST:*;NAME.TYPE.newest"
               "HOST:**;NAME.TYPE.newest"
               "HOST:DIR;NAME.TYPE.newest"
               "HOST:DIR1;DIR2;DIR3;NAME.TYPE.newest"
               "HOST:DIR1;*;DIR3;NAME.TYPE.newest"
               "HOST:DIR1;**;DIR3;NAME.TYPE.newest"

               "HOST:;*.TYPE.newest"
               "HOST:;*;*.TYPE.newest"
               "HOST:;**;*.TYPE.newest"
               "HOST:;DIR;*.TYPE.newest"
               "HOST:;DIR1;DIR2;DIR3;*.TYPE.newest"
               "HOST:;DIR1;*;DIR3;*.TYPE.newest"
               "HOST:;DIR1;**;DIR3;*.TYPE.newest"
               "HOST:"
               "HOST:*;*.TYPE.newest"
               "HOST:**;*.TYPE.newest"
               "HOST:DIR;*.TYPE.newest"
               "HOST:DIR1;DIR2;DIR3;*.TYPE.newest"
               "HOST:DIR1;*;DIR3;*.TYPE.newest"
               "HOST:DIR1;**;DIR3;*.TYPE.newest"


               "HOST:;.TYPE.NEWEST"
               "HOST:;*;.TYPE.NEWEST"
               "HOST:;**;.TYPE.NEWEST"
               "HOST:;DIR;.TYPE.NEWEST"
               "HOST:;DIR1;DIR2;DIR3;.TYPE.NEWEST"
               "HOST:;DIR1;*;DIR3;.TYPE.NEWEST"
               "HOST:;DIR1;**;DIR3;.TYPE.NEWEST"
               "HOST:"
               "HOST:*;.TYPE.NEWEST"
               "HOST:**;.TYPE.NEWEST"
               "HOST:DIR;.TYPE.NEWEST"
               "HOST:DIR1;DIR2;DIR3;.TYPE.NEWEST"
               "HOST:DIR1;*;DIR3;.TYPE.NEWEST"
               "HOST:DIR1;**;DIR3;.TYPE.NEWEST"

               "HOST:;NAME.TYPE.NEWEST"
               "HOST:;*;NAME.TYPE.NEWEST"
               "HOST:;**;NAME.TYPE.NEWEST"
               "HOST:;DIR;NAME.TYPE.NEWEST"
               "HOST:;DIR1;DIR2;DIR3;NAME.TYPE.NEWEST"
               "HOST:;DIR1;*;DIR3;NAME.TYPE.NEWEST"
               "HOST:;DIR1;**;DIR3;NAME.TYPE.NEWEST"
               "HOST:"
               "HOST:*;NAME.TYPE.NEWEST"
               "HOST:**;NAME.TYPE.NEWEST"
               "HOST:DIR;NAME.TYPE.NEWEST"
               "HOST:DIR1;DIR2;DIR3;NAME.TYPE.NEWEST"
               "HOST:DIR1;*;DIR3;NAME.TYPE.NEWEST"
               "HOST:DIR1;**;DIR3;NAME.TYPE.NEWEST"

               "HOST:;*.TYPE.NEWEST"
               "HOST:;*;*.TYPE.NEWEST"
               "HOST:;**;*.TYPE.NEWEST"
               "HOST:;DIR;*.TYPE.NEWEST"
               "HOST:;DIR1;DIR2;DIR3;*.TYPE.NEWEST"
               "HOST:;DIR1;*;DIR3;*.TYPE.NEWEST"
               "HOST:;DIR1;**;DIR3;*.TYPE.NEWEST"
               "HOST:"
               "HOST:*;*.TYPE.NEWEST"
               "HOST:**;*.TYPE.NEWEST"
               "HOST:DIR;*.TYPE.NEWEST"
               "HOST:DIR1;DIR2;DIR3;*.TYPE.NEWEST"
               "HOST:DIR1;*;DIR3;*.TYPE.NEWEST"
               "HOST:DIR1;**;DIR3;*.TYPE.NEWEST"




               "HOST:;.TYPE.*"
               "HOST:;*;.TYPE.*"
               "HOST:;**;.TYPE.*"
               "HOST:;DIR;.TYPE.*"
               "HOST:;DIR1;DIR2;DIR3;.TYPE.*"
               "HOST:;DIR1;*;DIR3;.TYPE.*"
               "HOST:;DIR1;**;DIR3;.TYPE.*"
               "HOST:"
               "HOST:*;.TYPE.*"
               "HOST:**;.TYPE.*"
               "HOST:DIR;.TYPE.*"
               "HOST:DIR1;DIR2;DIR3;.TYPE.*"
               "HOST:DIR1;*;DIR3;.TYPE.*"
               "HOST:DIR1;**;DIR3;.TYPE.*"

               "HOST:;NAME.TYPE.*"
               "HOST:;*;NAME.TYPE.*"
               "HOST:;**;NAME.TYPE.*"
               "HOST:;DIR;NAME.TYPE.*"
               "HOST:;DIR1;DIR2;DIR3;NAME.TYPE.*"
               "HOST:;DIR1;*;DIR3;NAME.TYPE.*"
               "HOST:;DIR1;**;DIR3;NAME.TYPE.*"
               "HOST:"
               "HOST:*;NAME.TYPE.*"
               "HOST:**;NAME.TYPE.*"
               "HOST:DIR;NAME.TYPE.*"
               "HOST:DIR1;DIR2;DIR3;NAME.TYPE.*"
               "HOST:DIR1;*;DIR3;NAME.TYPE.*"
               "HOST:DIR1;**;DIR3;NAME.TYPE.*"

               "HOST:;*.TYPE.*"
               "HOST:;*;*.TYPE.*"
               "HOST:;**;*.TYPE.*"
               "HOST:;DIR;*.TYPE.*"
               "HOST:;DIR1;DIR2;DIR3;*.TYPE.*"
               "HOST:;DIR1;*;DIR3;*.TYPE.*"
               "HOST:;DIR1;**;DIR3;*.TYPE.*"
               "HOST:"
               "HOST:*;*.TYPE.*"
               "HOST:**;*.TYPE.*"
               "HOST:DIR;*.TYPE.*"
               "HOST:DIR1;DIR2;DIR3;*.TYPE.*"
               "HOST:DIR1;*;DIR3;*.TYPE.*"
               "HOST:DIR1;**;DIR3;*.TYPE.*"


               "HOST:;.*"
               "HOST:;*;.*"
               "HOST:;**;.*"
               "HOST:;DIR;.*"
               "HOST:;DIR1;DIR2;DIR3;.*"
               "HOST:;DIR1;*;DIR3;.*"
               "HOST:;DIR1;**;DIR3;.*"
               "HOST:"
               "HOST:*;.*"
               "HOST:**;.*"
               "HOST:DIR;.*"
               "HOST:DIR1;DIR2;DIR3;.*"
               "HOST:DIR1;*;DIR3;.*"
               "HOST:DIR1;**;DIR3;.*"

               "HOST:;NAME.*"
               "HOST:;*;NAME.*"
               "HOST:;**;NAME.*"
               "HOST:;DIR;NAME.*"
               "HOST:;DIR1;DIR2;DIR3;NAME.*"
               "HOST:;DIR1;*;DIR3;NAME.*"
               "HOST:;DIR1;**;DIR3;NAME.*"
               "HOST:"
               "HOST:*;NAME.*"
               "HOST:**;NAME.*"
               "HOST:DIR;NAME.*"
               "HOST:DIR1;DIR2;DIR3;NAME.*"
               "HOST:DIR1;*;DIR3;NAME.*"
               "HOST:DIR1;**;DIR3;NAME.*"

               "HOST:;*.*"
               "HOST:;*;*.*"
               "HOST:;**;*.*"
               "HOST:;DIR;*.*"
               "HOST:;DIR1;DIR2;DIR3;*.*"
               "HOST:;DIR1;*;DIR3;*.*"
               "HOST:;DIR1;**;DIR3;*.*"
               "HOST:"
               "HOST:*;*.*"
               "HOST:**;*.*"
               "HOST:DIR;*.*"
               "HOST:DIR1;DIR2;DIR3;*.*"
               "HOST:DIR1;*;DIR3;*.*"
               "HOST:DIR1;**;DIR3;*.*"



               "HOST:;.*.123"
               "HOST:;*;.*.123"
               "HOST:;**;.*.123"
               "HOST:;DIR;.*.123"
               "HOST:;DIR1;DIR2;DIR3;.*.123"
               "HOST:;DIR1;*;DIR3;.*.123"
               "HOST:;DIR1;**;DIR3;.*.123"
               "HOST:"
               "HOST:*;.*.123"
               "HOST:**;.*.123"
               "HOST:DIR;.*.123"
               "HOST:DIR1;DIR2;DIR3;.*.123"
               "HOST:DIR1;*;DIR3;.*.123"
               "HOST:DIR1;**;DIR3;.*.123"

               "HOST:;NAME.*.123"
               "HOST:;*;NAME.*.123"
               "HOST:;**;NAME.*.123"
               "HOST:;DIR;NAME.*.123"
               "HOST:;DIR1;DIR2;DIR3;NAME.*.123"
               "HOST:;DIR1;*;DIR3;NAME.*.123"
               "HOST:;DIR1;**;DIR3;NAME.*.123"
               "HOST:"
               "HOST:*;NAME.*.123"
               "HOST:**;NAME.*.123"
               "HOST:DIR;NAME.*.123"
               "HOST:DIR1;DIR2;DIR3;NAME.*.123"
               "HOST:DIR1;*;DIR3;NAME.*.123"
               "HOST:DIR1;**;DIR3;NAME.*.123"

               "HOST:;*.*.123"
               "HOST:;*;*.*.123"
               "HOST:;**;*.*.123"
               "HOST:;DIR;*.*.123"
               "HOST:;DIR1;DIR2;DIR3;*.*.123"
               "HOST:;DIR1;*;DIR3;*.*.123"
               "HOST:;DIR1;**;DIR3;*.*.123"
               "HOST:"
               "HOST:*;*.*.123"
               "HOST:**;*.*.123"
               "HOST:DIR;*.*.123"
               "HOST:DIR1;DIR2;DIR3;*.*.123"
               "HOST:DIR1;*;DIR3;*.*.123"
               "HOST:DIR1;**;DIR3;*.*.123"


               "HOST:;.*.newest"
               "HOST:;*;.*.newest"
               "HOST:;**;.*.newest"
               "HOST:;DIR;.*.newest"
               "HOST:;DIR1;DIR2;DIR3;.*.newest"
               "HOST:;DIR1;*;DIR3;.*.newest"
               "HOST:;DIR1;**;DIR3;.*.newest"
               "HOST:"
               "HOST:*;.*.newest"
               "HOST:**;.*.newest"
               "HOST:DIR;.*.newest"
               "HOST:DIR1;DIR2;DIR3;.*.newest"
               "HOST:DIR1;*;DIR3;.*.newest"
               "HOST:DIR1;**;DIR3;.*.newest"

               "HOST:;NAME.*.newest"
               "HOST:;*;NAME.*.newest"
               "HOST:;**;NAME.*.newest"
               "HOST:;DIR;NAME.*.newest"
               "HOST:;DIR1;DIR2;DIR3;NAME.*.newest"
               "HOST:;DIR1;*;DIR3;NAME.*.newest"
               "HOST:;DIR1;**;DIR3;NAME.*.newest"
               "HOST:"
               "HOST:*;NAME.*.newest"
               "HOST:**;NAME.*.newest"
               "HOST:DIR;NAME.*.newest"
               "HOST:DIR1;DIR2;DIR3;NAME.*.newest"
               "HOST:DIR1;*;DIR3;NAME.*.newest"
               "HOST:DIR1;**;DIR3;NAME.*.newest"

               "HOST:;*.*.newest"
               "HOST:;*;*.*.newest"
               "HOST:;**;*.*.newest"
               "HOST:;DIR;*.*.newest"
               "HOST:;DIR1;DIR2;DIR3;*.*.newest"
               "HOST:;DIR1;*;DIR3;*.*.newest"
               "HOST:;DIR1;**;DIR3;*.*.newest"
               "HOST:"
               "HOST:*;*.*.newest"
               "HOST:**;*.*.newest"
               "HOST:DIR;*.*.newest"
               "HOST:DIR1;DIR2;DIR3;*.*.newest"
               "HOST:DIR1;*;DIR3;*.*.newest"
               "HOST:DIR1;**;DIR3;*.*.newest"


               "HOST:;.*.NEWEST"
               "HOST:;*;.*.NEWEST"
               "HOST:;**;.*.NEWEST"
               "HOST:;DIR;.*.NEWEST"
               "HOST:;DIR1;DIR2;DIR3;.*.NEWEST"
               "HOST:;DIR1;*;DIR3;.*.NEWEST"
               "HOST:;DIR1;**;DIR3;.*.NEWEST"
               "HOST:"
               "HOST:*;.*.NEWEST"
               "HOST:**;.*.NEWEST"
               "HOST:DIR;.*.NEWEST"
               "HOST:DIR1;DIR2;DIR3;.*.NEWEST"
               "HOST:DIR1;*;DIR3;.*.NEWEST"
               "HOST:DIR1;**;DIR3;.*.NEWEST"

               "HOST:;NAME.*.NEWEST"
               "HOST:;*;NAME.*.NEWEST"
               "HOST:;**;NAME.*.NEWEST"
               "HOST:;DIR;NAME.*.NEWEST"
               "HOST:;DIR1;DIR2;DIR3;NAME.*.NEWEST"
               "HOST:;DIR1;*;DIR3;NAME.*.NEWEST"
               "HOST:;DIR1;**;DIR3;NAME.*.NEWEST"
               "HOST:"
               "HOST:*;NAME.*.NEWEST"
               "HOST:**;NAME.*.NEWEST"
               "HOST:DIR;NAME.*.NEWEST"
               "HOST:DIR1;DIR2;DIR3;NAME.*.NEWEST"
               "HOST:DIR1;*;DIR3;NAME.*.NEWEST"
               "HOST:DIR1;**;DIR3;NAME.*.NEWEST"

               "HOST:;*.*.NEWEST"
               "HOST:;*;*.*.NEWEST"
               "HOST:;**;*.*.NEWEST"
               "HOST:;DIR;*.*.NEWEST"
               "HOST:;DIR1;DIR2;DIR3;*.*.NEWEST"
               "HOST:;DIR1;*;DIR3;*.*.NEWEST"
               "HOST:;DIR1;**;DIR3;*.*.NEWEST"
               "HOST:"
               "HOST:*;*.*.NEWEST"
               "HOST:**;*.*.NEWEST"
               "HOST:DIR;*.*.NEWEST"
               "HOST:DIR1;DIR2;DIR3;*.*.NEWEST"
               "HOST:DIR1;*;DIR3;*.*.NEWEST"
               "HOST:DIR1;**;DIR3;*.*.NEWEST"

               "HOST:;.*.*"
               "HOST:;*;.*.*"
               "HOST:;**;.*.*"
               "HOST:;DIR;.*.*"
               "HOST:;DIR1;DIR2;DIR3;.*.*"
               "HOST:;DIR1;*;DIR3;.*.*"
               "HOST:;DIR1;**;DIR3;.*.*"
               "HOST:"
               "HOST:*;.*.*"
               "HOST:**;.*.*"
               "HOST:DIR;.*.*"
               "HOST:DIR1;DIR2;DIR3;.*.*"
               "HOST:DIR1;*;DIR3;.*.*"
               "HOST:DIR1;**;DIR3;.*.*"

               "HOST:;NAME.*.*"
               "HOST:;*;NAME.*.*"
               "HOST:;**;NAME.*.*"
               "HOST:;DIR;NAME.*.*"
               "HOST:;DIR1;DIR2;DIR3;NAME.*.*"
               "HOST:;DIR1;*;DIR3;NAME.*.*"
               "HOST:;DIR1;**;DIR3;NAME.*.*"
               "HOST:"
               "HOST:*;NAME.*.*"
               "HOST:**;NAME.*.*"
               "HOST:DIR;NAME.*.*"
               "HOST:DIR1;DIR2;DIR3;NAME.*.*"
               "HOST:DIR1;*;DIR3;NAME.*.*"
               "HOST:DIR1;**;DIR3;NAME.*.*"

               "HOST:;*.*.*"
               "HOST:;*;*.*.*"
               "HOST:;**;*.*.*"
               "HOST:;DIR;*.*.*"
               "HOST:;DIR1;DIR2;DIR3;*.*.*"
               "HOST:;DIR1;*;DIR3;*.*.*"
               "HOST:;DIR1;**;DIR3;*.*.*"
               "HOST:"
               "HOST:*;*.*.*"
               "HOST:**;*.*.*"
               "HOST:DIR;*.*.*"
               "HOST:DIR1;DIR2;DIR3;*.*.*"
               "HOST:DIR1;*;DIR3;*.*.*"
               "HOST:DIR1;**;DIR3;*.*.*"



               "HOST:;*DIR1;D*I*R*2;DIR3*;"
               "HOST:*DIR1;D*I*R*2;DIR3*;"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*"
               "HOST:*DIR1;D*I*R*2;DIR3*;*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.T*P*"
               "HOST:*DIR1;D*I*R*2;DIR3*;.T*P*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.T*P*"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.T*P*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.T*P*"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.T*P*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.T*P*.123"
               "HOST:*DIR1;D*I*R*2;DIR3*;.T*P*.123"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.T*P*.123"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.T*P*.123"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.T*P*.123"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.T*P*.123"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.T*P*.newest"
               "HOST:*DIR1;D*I*R*2;DIR3*;.T*P*.newest"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.T*P*.newest"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.T*P*.newest"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.T*P*.newest"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.T*P*.newest"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.T*P*.NEWEST"
               "HOST:*DIR1;D*I*R*2;DIR3*;.T*P*.NEWEST"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.T*P*.NEWEST"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.T*P*.NEWEST"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.T*P*.NEWEST"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.T*P*.NEWEST"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.T*P*.*"
               "HOST:*DIR1;D*I*R*2;DIR3*;.T*P*.*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.T*P*.*"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.T*P*.*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.T*P*.*"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.T*P*.*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.*"
               "HOST:*DIR1;D*I*R*2;DIR3*;.*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.*"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.*"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.*.123"
               "HOST:*DIR1;D*I*R*2;DIR3*;.*.123"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.*.123"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.*.123"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.*.123"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.*.123"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.*.newest"
               "HOST:*DIR1;D*I*R*2;DIR3*;.*.newest"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.*.newest"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.*.newest"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.*.newest"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.*.newest"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.*.NEWEST"
               "HOST:*DIR1;D*I*R*2;DIR3*;.*.NEWEST"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.*.NEWEST"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.*.NEWEST"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.*.NEWEST"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.*.NEWEST"
               "HOST:;*DIR1;D*I*R*2;DIR3*;.*.*"
               "HOST:*DIR1;D*I*R*2;DIR3*;.*.*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;N*M*.*.*"
               "HOST:*DIR1;D*I*R*2;DIR3*;N*M*.*.*"
               "HOST:;*DIR1;D*I*R*2;DIR3*;*.*.*"
               "HOST:*DIR1;D*I*R*2;DIR3*;*.*.*"

               )))




;; (ls -v --versions)
;; (ls -l --long) ; show author, write-date, element-type, size



(progn
  (ensure-directories-exist "HOME:A;B;C;D.E")
  (ensure-directories-exist "HOME:A;C;C;D.E")
  (ensure-directories-exist "HOME:A;D;C;D.E")
  (ensure-directories-exist "HOME:A;D;G;D.E")
  (ensure-directories-exist "HOME:B;C;D;E.E")
  (ensure-directories-exist "HOME:B;C;E;E.E")
  (ensure-directories-exist "HOME:B;C;F;E.E"))
(dotimes (i 2) (create-file-at-path "HOME:B;C;E;TEST.LISP"))
(dotimes (i 1) (create-file-at-path "HOME:A;B;EXEMPLE-1.TXT"))
(dotimes (i 2) (create-file-at-path "HOME:A;B;EXEMPLE-2.TXT"))
(dotimes (i 3) (create-file-at-path "HOME:A;B;EXEMPLE-3.TXT"))
(dotimes (i 2) (create-file-at-path "HOME:A;2-TOTO.TXT"))
(dotimes (i 3) (create-file-at-path "HOME:A;3-TOTO.TXT"))
(dotimes (i 4) (create-file-at-path "HOME:A;4-TOTO.TXT"))


(in-package "VFS-USER")
(INSTALL-PATHNAME-READER-MACRO)

(progn
  (vfs::create-file-at-path #P"HOME:TEST.TEXT")
  (vfs::create-file-at-path #P"HOME:EXAMPLE.TEXT")
  (vfs::create-new-version (vfs::file (vfs::file-entry #P"HOME:EXAMPLE.TEXT")))
  (vfs::dump (vfs::file-system-named "HOME")))

(let ((path #P"HOME:TEST.TEXT")
      (path #P"HOME:EXAMPLE.TEXT"))
 (defparameter *s* (open path :direction :output
                         :if-exists :new-version
                         :if-does-not-exist :create
                         :element-type 'base-char))
 (write-char   #\* *s*)
 (print (file-length *s*))
 (write-string " Hello World" *s*)
 (print (file-length *s*))
 (write-line   "! How do you do?" *s*)
 (print (file-length *s*))
 (close *s*)
 (terpri)
 (finish-output)
 (vfs::dump (vfs::file-system-named "HOME")))


(let ((path #P"HOME:EXAMPLE.DATA"))
 (defparameter *s* (open path
                         :direction :output
                         :element-type '(unsigned-byte 21)
                         :if-exists :new-version
                         :if-does-not-exist :create))
 (write-byte   (char-code #\*) *s*)
 (print (file-length *s*))
 (write-sequence (map 'vector 'char-code " Hello World") *s*)
 (print (file-length *s*))
 (write-sequence (map 'vector 'char-code "! How do you do?")  *s*)
 (write-byte (char-code #\newline) *s*)
 (print (file-length *s*))
 (close *s*)
 (terpri)
 (finish-output)
 (vfs::dump (vfs::file-system-named "HOME")))


(untrace replace)
(untrace vfs::!write-element replace)


(progn
  (defparameter *s* (open "HOME:EXAMPLE.TEXT" :direction :input
                          :element-type 'base-char))
  (prog1 (read-line *s*)
    (close *s*)
    (vfs::dump (vfs::file-system-named "HOME"))))

(progn
  (defparameter *s* (open "HOME:EXAMPLE.DATA" :direction :input
                          :element-type '(unsigned-byte 21)))
  (prog1 (read-sequence (make-array (file-length *s*)
                                    :element-type '(unsigned-byte 21))
                        *s*)
    (close *s*)
    (vfs::dump (vfs::file-system-named "HOME"))))



(setf (logical-pathname-translations "LISP") nil)
(setf (logical-pathname-translations "LISP") (list (list #P"LISP:**;*.*.*" #P"HOME:SRC;LISP;**;*.*.*")
                                                   (list #P"LISP:**;*.*"   #P"HOME:SRC;LISP;**;*.*")
                                                   (list #P"LISP:**;*"     #P"HOME:SRC;LISP;**;*")))


(let ((path #P"LISP:TEST.LISP"))
  (defparameter *s* (open path :direction :output
                          :if-exists :new-version
                          :if-does-not-exist :create))
  (write-line ";;;; -*- mode:lisp -*-" *s*)
  (write-string (prin1-to-string '(defun test (arg)
                                   (princ "Hello Test!") (terpri)
                                   (princ arg) (terpri)
                                   (princ "Done here." (terpri)
                                    arg)))
                *s*)
  (terpri *s*)
  (print (file-length *s*))
  (close *s*)
  (terpri)
  (finish-output)
  (vfs::dump (vfs::file-system-named "HOME")))
(vfs::dump-pathname  #P"LISP:TEST.LISP")
