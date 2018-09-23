;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               tests.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Some tests of the pkcs11 package.
;;;;
;;;;    Note they're not (YET) actual unit tests (automatic tests,
;;;;    resulting in success / failure status), but more debugging
;;;;    tools and exercises of the pkcs11 functions that need manual
;;;;    validation. (And definitely manual intervention: insert the
;;;;    Smartcard in the Smartcard reader, key-in PIN codes, etc).
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-04-25 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.CLEXT.PKCS11")


(defcfun (mtrace   "mtrace")   :void)
(defcfun (muntrace "muntrace") :void)

(defun call-with-mtrace (log-pathname thunk)
  #+darwin (declare (ignore log-pathname))
  ;; TODO: darwin: use libgmalloc.
  #+linux
  (let* ((mtrace-envvar "MALLOC_TRACE")
         (old-mtrace  (ccl:getenv mtrace-envvar)))
    (ccl:setenv mtrace-envvar (namestring log-pathname))
    (mtrace)
    (unwind-protect
         (funcall thunk)
      (muntrace)
      (if old-mtrace
          (ccl:setenv mtrace-envvar old-mtrace)
          (ccl:unsetenv mtrace-envvar))))
  #+darwin
  (funcall thunk))

(defmacro with-mtrace ((log-pathname) &body body)
  `(call-with-mtrace ,log-pathname (lambda () ,@body)))



(defun print-error (condition)
  (format t "~&~A~%" condition)
  (force-output))

(defun safe-get-list-of-slots-with-token ()
  (or (get-slot-list t)
      (progn
        (format t "No smartcart~%")
        (finish-output)
        '())))

(defun test/string-to-utf-8 ()
  (assert (equalp (string-to-utf-8 "Ça roule en été!" :size 18 :padchar #\space)
                  #(195 135 97 32 114 111 117 108 101 32 101 110 32 195 169 116 195 169)))
  (assert (nth-value 1 (ignore-errors (progn (string-to-utf-8 "Ça roule en été!" :size 17 :padchar #\space) t))))
  (assert (equalp (string-to-utf-8 "Ça roule en été!" :size 16 :padchar #\space)
                  #(195 135 97 32 114 111 117 108 101 32 101 110 32 195 169 116)))
  (assert (equalp (string-to-utf-8 "Ça roule en été!" :size 24 :padchar #\space)
                  #(195 135 97 32 114 111 117 108 101 32 101 110 32 195 169 116 195 169 33 32 32 32 32 32)))
  (assert (nth-value 1 (ignore-errors (progn (string-to-utf-8 "Ça roule en été!" :size 24 :padchar (character "∞")) t))))
  (assert (equalp (string-to-utf-8 "Ça roule en été!" :size 25 :padchar (character "∞"))
                  #(195 135 97 32 114 111 117 108 101 32 101 110 32 195 169 116 195 169 33 226 136 158 226 136 158)))
  :success)

(defun test/session ()
  (with-pkcs11
    (dolist (slot-id (safe-get-list-of-slots-with-token))
      (with-open-session (session slot-id :if-open-session-fails nil)
        (print (when session (get-session-info session))))))
  :success)

(defun test/operation-state ()
  (with-pkcs11
    (dolist (slot-id (safe-get-list-of-slots-with-token))
      (handler-case
          (with-open-session (session slot-id :if-open-session-fails nil)
            (let ((state (get-operation-state session)))
              (print state)
              (set-operation-state session state)))
        (pkcs11-error (err) (print-error err)))))
  :success)

(defun test/login ()
  (with-pkcs11
    (dolist (slot-id (safe-get-list-of-slots-with-token))
      (with-open-session (session slot-id)
            (let ((*verbose* t))
              (do-logged-in (session slot-id)
                (write-line "Inside logged session.")
                (finish-output))))))
  (finish-output)
  :success)


(defun test/slots-and-mechanisms ()
  (test/string-to-utf-8)
  (with-pkcs11
    (format t "Info: ~S~%" (get-info))
    (format t "All Slot IDs: ~S~%" (get-slot-list nil))
    (format t "~:{- Slot ID: ~A~%  Slot Info: ~S~%~%~}"
            (mapcar (lambda (slot-id)
                      (list slot-id
                            (handler-case (get-slot-info slot-id)
                              (error (err) (princ-to-string err)))))
                    (get-slot-list nil)))
    (format t "Slot IDs with a token: ~S~%" (get-slot-list t))
    (format t "~:{- Slot ID: ~A~%  Slot Info: ~S~%  Token Info: ~S~
               ~%  Mechanism list: ~{~A~^~%                  ~}~%~}"
            (mapcar (lambda (slot-id)
                      (list slot-id
                            (handler-case (get-slot-info slot-id)
                              (error (err) (princ-to-string err)))
                            (handler-case (get-token-info slot-id)
                              (error (err) (princ-to-string err)))
                            (handler-case (mapcar (lambda (mechanism-type)
                                                    (list mechanism-type
                                                          (get-mechanism-info slot-id mechanism-type)))
                                                  (get-mechanism-list slot-id))
                              (error (err) (list (princ-to-string err))))))
                    (get-slot-list t))))
  :success)

(defun test/template-encode ()
 (let ((template (template-encode
                  '((:class . :data)
                    (:application . "My Test Application")
                    (:token . t)
                    (:label . "Test Data")
                    (:value . "Some data object containing some test data. This is it!")))))
   (template-dump template)
   (template-free template))
  :success)


(defun test/create-object ()
  (with-pkcs11
    (dolist (slot-id (safe-get-list-of-slots-with-token))
      (with-open-session (session slot-id)
        (handler-case
            (print (create-object session '((:class . :data)
                                            (:application . "My Test Application")
                                            (:token . t)
                                            (:label . "Test Data")
                                            (:value . "Some data object containing some test data. This is it!"))))
          (pkcs11-error (err) (print-error err))))))
  :success)

;; (test/create-object) -> attribute value invalid. would that be the application?


(defun test/random ()
  (write-line ";; seed-random is not supported by ECC-MI.")
  (write-line ";; generate-random works only on length=0 on ECC-MI (useless).")
  (with-pkcs11
    (dolist (slot-id (safe-get-list-of-slots-with-token))
      (with-open-session (session slot-id)
        (handler-case
            (seed-random session (vector (random 256)
                                         (random 256)
                                         (random 256)
                                         (random 256)))
          (pkcs11-error (err)
            (print-error err)
            (unless (eql :function-not-supported (pkcs11-error-label err))
              (signal err))))
        (handler-case
            (generate-random session 4)
          (pkcs11-error (err)
            (print-error err)
            (unless (eql :data-len-range (pkcs11-error-label err))
              (signal err)))))))
  :success)


;; ; seed-random not supported
;; (untrace foreign-vector)
;; (test/random)

;; get-function-status and cancel-function are useless legacy functions. Not implemented.


;; (test/session)
;; (test)

;; (load-library)
;; (with-pkcs11 (wait-for-slot-event nil)) ;; not supported by   "/usr/local/lib/libiaspkcs11.so"



(defmacro possibly-logged-in ((session slot-id log-in) &body body)
  (let ((vsession (gensym "session"))
        (vslot-id (gensym "slot-id"))
        (vlog-in  (gensym "log-in"))
        (fbody    (gensym "body")))
    `(let ((,vsession ,session)
           (,vslot-id ,slot-id)
           (,vlog-in  ,log-in))
       (flet ((,fbody () ,@body))
         (if ,vlog-in
             (call-logged-in ,vsession ,vslot-id (function ,fbody))
             (,fbody))))))


(defun test/find-objects (&optional log-in)
  (with-pkcs11
      (dolist (slot-id (safe-get-list-of-slots-with-token))
        (format t "~2%Slot ~3D~%--------~2%" slot-id)
      (with-open-session (session slot-id)
        (possibly-logged-in (session slot-id log-in)
          (let ((all-objects (find-all-objects session nil)))
            (pprint all-objects)
            (dolist (object all-objects)
              (format t "~&Object Handle: ~A~%~{    ~S~%~}~%" object  (object-get-all-attributes session object))))))))
  :success)

;; (test/find-objects t)


(defun select/encrypt-key ())

(defun test/encrypt ()
  (with-pkcs11
    (format t "Info: ~S~%" (get-info))
    (format t "All Slot IDs: ~S~%" (get-slot-list nil))
    (format t "~:{- Slot ID: ~A~%  Slot Info: ~S~%~%~}"
            (mapcar (lambda (slot-id)
                      (list slot-id
                            (handler-case (get-slot-info slot-id)
                              (error (err) (princ-to-string err)))))
                    (get-slot-list nil)))
    (format t "Slot IDs with a token: ~S~%" (get-slot-list t))
    (format t "~:{- Slot ID: ~A~%  Slot Info: ~S~%  Token Info: ~S~
               ~%  Mechanism list: ~{~A~^~%                  ~}~%~}"
            (mapcar (lambda (slot-id)
                      (list slot-id
                            (handler-case (get-slot-info slot-id)
                              (error (err) (princ-to-string err)))
                            (handler-case (get-token-info slot-id)
                              (error (err) (princ-to-string err)))
                            (handler-case (mapcar (lambda (mechanism-type)
                                                    (list mechanism-type
                                                          (get-mechanism-info slot-id mechanism-type)))
                                                  (get-mechanism-list slot-id))
                              (error (err) (list (princ-to-string err))))))
                    (get-slot-list t)))))

(defun aget (k a) (cdr (assoc k a)))

(defun object-handle (session &key class id token key-type label)
  (first (find-all-objects session (append (when class    (list (cons :class    class)))
                                           (when id       (list (cons :id       id)))
                                           (when token    (list (cons :token    token)))
                                           (when key-type (list (cons :key-type key-type)))
                                           (when label    (list (cons :label    label)))))))

(defgeneric key-id (object)
  (:documentation "KEY-ID specifiers are integers or octet vectors.")
  (:method ((object vector)) object)
  (:method ((object list))   (coerce object '(vector octet)))
  (:method ((object integer))
    (loop :with id := (make-array 18 :element-type 'octet)
          :for j :from 0
          :for p :from (* 8 (1- (length id))) :downto 0 :by 8
          :do (setf (aref id j) (ldb (byte 8 p) object))
          :finally (return id))))


(defparameter *authentication-key-path* '(:slot-id 0 :token "ECC MI" :id #xe828bd080fd2500000104d494f4300010101))
(defparameter *signature-key-path*      '(:slot-id 1 :token "ECC MI" :id #xe828bd080fd2500000104d494f4300010103))

;; pub-key value <- authentication  (no login)
;; priv-key signature -> sign       (login)

(defun get-public-key-value (path)
  (with-open-session (session (getf path :slot-id))
    (let* ((pub-key (object-handle session :class :public-key :token (getf path :token) :id (key-id (getf path :id))))
           (pub-key-value     (let ((attributes (object-get-all-attributes session pub-key)))
                                (aget :value attributes))))
      pub-key-value)))

(defun /sign (data signature-key-path)
  (let ((slot-id (getf signature-key-path :slot-id)))
    (with-open-session (session slot-id)
      (do-logged-in (session slot-id "signing the authentication public-key")
        (let ((sign-private-key  (object-handle session :class :private-key
                                                        :token (getf signature-key-path :token)
                                                        :id (key-id (getf signature-key-path :id)))))
          (sign-init session :sha1-rsa-pkcs sign-private-key)
          (sign session data :output-end 256))))))

(defun /verify (data signature signature-key-path)
  (let ((slot-id (getf signature-key-path :slot-id)))
    (with-open-session (session slot-id)
      (let ((sign-public-key  (object-handle session :class :public-key
                                                     :token (getf signature-key-path :token)
                                                     :id (key-id (getf signature-key-path :id)))))
        (verify-init session :sha1-rsa-pkcs sign-public-key)
        (verify session data signature)))))

(defun test/sign&verify (&key
                           (authentication-key-path *authentication-key-path*)
                           (signature-key-path      *signature-key-path*))
    (with-pkcs11
      (handler-case
          (let* ((pub-key-value (get-public-key-value authentication-key-path))
                 (signature     (/sign pub-key-value signature-key-path)))
            (format t "~&Authentication public-key: ~S~%" pub-key-value)
            (format t "~&Signature: ~S~%" signature)
            (format t "~&Verifying signature.~%")
            (/verify pub-key-value signature signature-key-path)
            (format t "~&Verifying bad signature.~%")
            (setf (aref pub-key-value 0) (mod (1+ (aref pub-key-value 0)) 256))
            (unwind-protect
                 (princ (nth-value 1 (ignore-errors (/verify pub-key-value signature signature-key-path))))
              (setf (aref pub-key-value 0) (mod (1- (aref pub-key-value 0)) 256))))
        (pkcs11-error (err)
          (if (eql :token-not-present (pkcs11-error-label err))
              (progn
                (format t "No smartcart~%")
                (finish-output))
              (signal err)))))
  :success)

(defvar *session* nil)
(defvar *slot-id* nil)

(defparameter *pjb-auth-key-id* #xe828bd080fd2500000104d494f4300010101)
(defparameter *pjb-sign-key-id* #xe828bd080fd2500000104d494f4300010103)

(defun done ()
  (com.informatimago.common-lisp.interactive.interactive:repl-exit))

(defun test/repl (&key ((:slot-id *slot-id*) 0))
  (with-pkcs11
    (with-open-session (*session* *slot-id*)
      (do-logged-in (*session* *slot-id*)
        (format t "~&Evaluate (done) to log out from the smartcard.~%")
        (com.informatimago.common-lisp.interactive.interactive:repl :reset-history nil)))))

(defun test/all ()
  (test/string-to-utf-8)
  (test/session)
  (test/operation-state)
  (test/template-encode)
  (test/slots-and-mechanisms)
  (test/create-object)
  (test/random)
  (test/find-objects)
  (test/login)
  (test/sign&verify))


;;;; THE END ;;;;
