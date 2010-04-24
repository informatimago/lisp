(in-package :cliki)

(defvar *daynames*
  '((0 . "Monday")
    (1 . "Tuesday")
    (2 . "Wednesday")
    (3 . "Thursday")
    (4 . "Friday")
    (5 . "Saturday")
    (6 . "Sunday")))
    
(defun dayname (stream arg colon-p at-p &optional width (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  "Print the day of the week (0=Sunday) corresponding to ARG on STREAM.  This is intended for embedding in a FORMAT directive: WIDTH governs the number of characters of text printed, MINCOL, COLINC, MINPAD, PADCHAR work as for ~A"
  (let ((daystring (cdr (assoc (mod arg 7) *daynames*))))
    (if (not daystring) (return-from dayname nil))
    (let ((truncate (if width (min width (length daystring)) nil)))
      (format stream
              (if at-p "~V,V,V,V@A" "~V,V,V,VA")
              mincol colinc minpad padchar
              (subseq daystring 0 truncate)))))

(defvar *monthnames*
  '((1 . "January")
    (2 . "February")
    (3 . "March")
    (4 . "April")
    (5 . "May")
    (6 . "June")
    (7 . "July")
    (8 . "August")
    (9 . "September")
    (10 . "October")
    (11 . "November")
    (12 . "December")))
    
(defun monthname (stream arg colon-p at-p &optional width (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  "Print the name of the month (1=January) corresponding to ARG on STREAM.  This is intended for embedding in a FORMAT directive: WIDTH governs the number of characters of text printed, MINCOL, COLINC, MINPAD, PADCHAR work as for ~A"
  (let ((monthstring (cdr (assoc arg *monthnames*))))
    (if (not monthstring) (return-from monthname nil))
    (let ((truncate (if width (min width (length monthstring)) nil)))
      (format stream
              (if at-p "~V,V,V,V@A" "~V,V,V,VA")
              mincol colinc minpad padchar
              (subseq monthstring 0 truncate)))))

(defmacro with-date (universal-time &body body)
  `(multiple-value-bind
       (second minute hour date month year day daylight-p zone)
       (decode-universal-time ,universal-time)
     (declare (ignorable second minute hour date month year day daylight-p zone))
     ,@body))


