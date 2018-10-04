;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pkcs11.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Lispy interface over Cryptoki pkcs11 version 2.02.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-04-18 <PJB> Created.
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

(defpackage "COM.INFORMATIMAGO.CLEXT.PKCS11"
  (:use "COMMON-LISP" "CFFI" "BABEL")
  (:use "COM.INFORMATIMAGO.CLEXT.PKCS11.CFFI-UTILS")
  (:import-from "COM.INFORMATIMAGO.CLEXT.PKCS11.LOW" "LOAD-LIBRARY")
  (:shadowing-import-from "COM.INFORMATIMAGO.CLEXT.PKCS11.CFFI-DEBUG"
                          "FOREIGN-ALLOC" "FOREIGN-FREE")
  (:export "PKCS11-ERROR"
           "PKCS11-ERROR-CODE" "PKCS11-ERROR-LABEL" "PKCS11-ERROR-FUNCTION"
           "CHECK-RV" "WITH-PKCS11"
           "RETURN-VALUE" "CONVERT-SLOT-INFO-FLAGS" "CONVERT-TOKEN-INFO-FLAGS"
           "USER-TYPE" "STATE" "CONVERT-SESSION-INFO-FLAGS"
           "CONVERT-WAIT-FOR-SLOT-EVENT-FLAGS" "OBJECT-CLASS" "HARDWARE-FEATURE"
           "KEY-TYPE" "CERTIFICATE-TYPE" "ATTRIBUTE-TYPE" "MECHANISM-TYPE"
           "CONVERT-MECHANISM-INFO-FLAGS" "CKBOOL" "UNAVAILABLE-INFORMATION-P"
           "INVALID-POINTER-P" "VERSION" "MAKE-VERSION" "VERSION-P"
           "COPY-VERSION" "VERSION-MAJOR" "VERSION-MINOR" "VERSION" "INFO"
           "MAKE-INFO" "INFO-P" "COPY-INFO" "INFO-CRYPTOKI-VERSION"
           "INFO-MANUFACTURER-ID" "INFO-FLAGS" "INFO-LIBRARY-DESCRIPTION"
           "INFO-LIBRARY-VERSION" "GET-INFO" "GET-SLOT-LIST" "SLOT-INFO"
           "MAKE-SLOT-INFO" "SLOT-INFO-P" "COPY-SLOT-INFO"
           "SLOT-INFO-SLOT-DESCRIPTION" "SLOT-INFO-MANUFACTURER-ID"
           "SLOT-INFO-FLAGS" "SLOT-INFO-HARDWARE-VERSION"
           "SLOT-INFO-FIRMWARE-VERSION" "GET-SLOT-INFO" "TOKEN-INFO"
           "MAKE-TOKEN-INFO" "TOKEN-INFO-P" "COPY-TOKEN-INFO" "TOKEN-INFO-LABEL"
           "TOKEN-INFO-MANUFACTURER-ID" "TOKEN-INFO-MODEL"
           "TOKEN-INFO-SERIAL-NUMBER" "TOKEN-INFO-FLAGS"
           "TOKEN-INFO-MAX-SESSION-COUNT" "TOKEN-INFO-SESSION-COUNT"
           "TOKEN-INFO-MAX-RW-SESSION-COUNT" "TOKEN-INFO-RW-SESSION-COUNT"
           "TOKEN-INFO-MAX-PIN-LEN" "TOKEN-INFO-MIN-PIN-LEN"
           "TOKEN-INFO-TOTAL-PUBLIC-MEMORY" "TOKEN-INFO-FREE-PUBLIC-MEMORY"
           "TOKEN-INFO-TOTAL-PRIVATE-MMEORY" "TOKEN-INFO-FREE-PRIVATE-MEMORY"
           "TOKEN-INFO-HARDWARE-VERSION" "TOKEN-INFO-FIRMWARE-VERSION"
           "TOKEN-INFO-UTC-TIME" "GET-TOKEN-INFO" "WAIT-FOR-SLOT-EVENT"
           "GET-MECHANISM-LIST" "MECHANISM-INFO" "MAKE-MECHANISM-INFO"
           "MECHANISM-INFO-P" "COPY-MECHANISM-INFO" "MECHANISM-INFO-MIN-KEY-SIZE"
           "MECHANISM-INFO-MAX-KEY-SIZE" "MECHANISM-INFO-FLAGS"
           "GET-MECHANISM-INFO" "STRING-FROM-UTF-8" "INIT-TOKEN" "OPEN-SESSION"
           "CLOSE-SESSION" "CLOSE-ALL-SESSIONS" "WITH-OPEN-SESSION"
           "SESSION-INFO" "MAKE-SESSION-INFO" "SESSION-INFO-P"
           "COPY-SESSION-INFO" "SESSION-INFO-SLOT-ID" "SESSION-INFO-STATE"
           "SESSION-INFO-FLAGS" "SESSION-INFO-DEVICE-ERROR" "GET-SESSION-INFO"
           "GET-OPERATION-STATE" "SET-OPERATION-STATE" "LOGIN" "LOGOUT"
           "INIT-PIN" "SET-PIN" "READ-PIN" "CREATE-OBJECT" "COPY-OBJECT"
           "DESTROY-OBJECT" "GET-OBJECT-SIZE" "GET-ATTRIBUTE-VALUE"
           "SET-ATTRIBUTE-VALUE" "FIND-OBJECTS-INIT" "FIND-OBJECTS"
           "FIND-OBJECTS-FINAL" "FIND-ALL-OBJECTS" "OBJECT-GET-ALL-ATTRIBUTES"
           "OBJECT-GET-ATTRIBUTES"
           "SEED-RANDOM" "GENERATE-RANDOM" "LOAD-LIBRARY"
           "CALL-LOGGED-IN" "DO-LOGGED-IN")

  (:documentation "Lispy interface over Cryptoki pkcs11 version 2.02

License:

    AGPL3

    Copyright Pascal J. Bourguignon 2018 - 2018

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))

(in-package "COM.INFORMATIMAGO.CLEXT.PKCS11")

(deftype octet           () '(unsigned-byte 8))

(defconstant +ulong-bits+ (* 8 (foreign-type-size :ulong)))

(deftype session-handle  () `(unsigned-byte ,+ulong-bits+))
(deftype slot-id         () `(unsigned-byte ,+ulong-bits+))
(deftype mechanismi-type () `(unsigned-byte ,+ulong-bits+))
(deftype object-handle   () `(unsigned-byte ,+ulong-bits+))



(defun flags (operation flags map)
  (ecase operation
    ((:decode)
     (loop :for (flag . keyword) :in map
           :when (= flag (logand flags flag))
             :collect keyword))
    ((:encode)
     (loop :for (flag . keyword) :in map
           :when (member keyword flags)
             :sum flag))))

(defun enum (operation value map)
  (ecase operation
    ((:decode) (or (cdr (assoc  value map)) value))
    ((:encode) (or (car (rassoc value map))
                   (error "Unknown enum keyword ~S, expected one of `{~S~^ ~}."
                          value (mapcar (function cdr) map))))))

(defmacro define-flag-converter (name map)
  `(defun ,name (operation value)
     (flags operation value (load-time-value
                             (list ,@(mapcar (lambda (entry)
                                               "(ck-constant keyword) -> (cons ck-constant keyword)"
                                               `(cons ,@entry))
                                             map))))))

(defmacro define-enum-converter (name map)
  `(defun ,name (operation value)
     (enum operation value (load-time-value
                            (list ,@(mapcar (lambda (entry)
                                              "(ck-constant keyword) -> (cons ck-constant keyword)"
                                              `(cons ,@entry))
                                            map))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converters

(define-enum-converter return-value
    ((%ck:+ok+                                 :ok)
     (%ck:+cancel+                             :cancel)
     (%ck:+host-memory+                        :host-memory)
     (%ck:+slot-id-invalid+                    :slot-id-invalid)
     (%ck:+general-error+                      :general-error)
     (%ck:+function-failed+                    :function-failed)
     (%ck:+arguments-bad+                      :arguments-bad)
     (%ck:+no-event+                           :no-event)
     (%ck:+need-to-create-threads+             :need-to-create-threads)
     (%ck:+cant-lock+                          :cant-lock)
     (%ck:+attribute-read-only+                :attribute-read-only)
     (%ck:+attribute-sensitive+                :attribute-sensitive)
     (%ck:+attribute-type-invalid+             :attribute-type-invalid)
     (%ck:+attribute-value-invalid+            :attribute-value-invalid)
     (%ck:+data-invalid+                       :data-invalid)
     (%ck:+data-len-range+                     :data-len-range)
     (%ck:+device-error+                       :device-error)
     (%ck:+device-memory+                      :device-memory)
     (%ck:+device-removed+                     :device-removed)
     (%ck:+encrypted-data-invalid+             :encrypted-data-invalid)
     (%ck:+encrypted-data-len-range+           :encrypted-data-len-range)
     (%ck:+function-canceled+                  :function-canceled)
     (%ck:+function-not-parallel+              :function-not-parallel)
     (%ck:+function-not-supported+             :function-not-supported)
     (%ck:+key-handle-invalid+                 :key-handle-invalid)
     (%ck:+key-size-range+                     :key-size-range)
     (%ck:+key-type-inconsistent+              :key-type-inconsistent)
     (%ck:+key-not-needed+                     :key-not-needed)
     (%ck:+key-changed+                        :key-changed)
     (%ck:+key-needed+                         :key-needed)
     (%ck:+key-indigestible+                   :key-indigestible)
     (%ck:+key-function-not-permitted+         :key-function-not-permitted)
     (%ck:+key-not-wrappable+                  :key-not-wrappable)
     (%ck:+key-unextractable+                  :key-unextractable)
     (%ck:+mechanism-invalid+                  :mechanism-invalid)
     (%ck:+mechanism-param-invalid+            :mechanism-param-invalid)
     (%ck:+object-handle-invalid+              :object-handle-invalid)
     (%ck:+operation-active+                   :operation-active)
     (%ck:+operation-not-initialized+          :operation-not-initialized)
     (%ck:+pin-incorrect+                      :pin-incorrect)
     (%ck:+pin-invalid+                        :pin-invalid)
     (%ck:+pin-len-range+                      :pin-len-range)
     (%ck:+pin-expired+                        :pin-expired)
     (%ck:+pin-locked+                         :pin-locked)
     (%ck:+session-closed+                     :session-closed)
     (%ck:+session-count+                      :session-count)
     (%ck:+session-handle-invalid+             :session-handle-invalid)
     (%ck:+session-parallel-not-supported+     :session-parallel-not-supported)
     (%ck:+session-read-only+                  :session-read-only)
     (%ck:+session-exists+                     :session-exists)
     (%ck:+session-read-only-exists+           :session-read-only-exists)
     (%ck:+session-read-write-so-exists+       :session-read-write-so-exists)
     (%ck:+signature-invalid+                  :signature-invalid)
     (%ck:+signature-len-range+                :signature-len-range)
     (%ck:+template-incomplete+                :template-incomplete)
     (%ck:+template-inconsistent+              :template-inconsistent)
     (%ck:+token-not-present+                  :token-not-present)
     (%ck:+token-not-recognized+               :token-not-recognized)
     (%ck:+token-write-protected+              :token-write-protected)
     (%ck:+unwrapping-key-handle-invalid+      :unwrapping-key-handle-invalid)
     (%ck:+unwrapping-key-size-range+          :unwrapping-key-size-range)
     (%ck:+unwrapping-key-type-inconsistent+   :unwrapping-key-type-inconsistent)
     (%ck:+user-already-logged-in+             :user-already-logged-in)
     (%ck:+user-not-logged-in+                 :user-not-logged-in)
     (%ck:+user-pin-not-initialized+           :user-pin-not-initialized)
     (%ck:+user-type-invalid+                  :user-type-invalid)
     (%ck:+user-another-already-logged-in+     :user-another-already-logged-in)
     (%ck:+user-too-many-types+                :user-too-many-types)
     (%ck:+wrapped-key-invalid+                :wrapped-key-invalid)
     (%ck:+wrapped-key-len-range+              :wrapped-key-len-range)
     (%ck:+wrapping-key-handle-invalid+        :wrapping-key-handle-invalid)
     (%ck:+wrapping-key-size-range+            :wrapping-key-size-range)
     (%ck:+wrapping-key-type-inconsistent+     :wrapping-key-type-inconsistent)
     (%ck:+random-seed-not-supported+          :random-seed-not-supported)
     (%ck:+random-no-rng+                      :random-no-rng)
     (%ck:+domain-params-invalid+              :domain-params-invalid)
     (%ck:+buffer-too-small+                   :buffer-too-small)
     (%ck:+saved-state-invalid+                :saved-state-invalid)
     (%ck:+information-sensitive+              :information-sensitive)
     (%ck:+state-unsaveable+                   :state-unsaveable)
     (%ck:+cryptoki-not-initialized+           :cryptoki-not-initialized)
     (%ck:+cryptoki-already-initialized+       :cryptoki-already-initialized)
     (%ck:+mutex-bad+                          :mutex-bad)
     (%ck:+mutex-not-locked+                   :mutex-not-locked)
     (%ck:+function-rejected+                  :function-rejected)))

(define-flag-converter convert-slot-info-flags
    ((%ck:+token-present+     :token-present)
     (%ck:+removable-device+  :removable-device)
     (%ck:+hw-slot+           :hardware-slot)
     (%ck:+array-attribute+   :array-attribute)))

(define-flag-converter convert-token-info-flags
    ((%ck:+rng+                            :rng)
     (%ck:+write-protected+                :write-protected)
     (%ck:+login-required+                 :login-required)
     (%ck:+user-pin-initialized+           :user-pin-initialized)
     (%ck:+restore-key-not-needed+         :restore-key-not-needed)
     (%ck:+clock-on-token+                 :clock-on-token)
     (%ck:+protected-authentication-path+  :protected-authentication-path)
     (%ck:+dual-crypto-operations+         :dual-crypto-operations)
     (%ck:+token-initialized+              :token-initialized)
     (%ck:+secondary-authentication+       :secondary-authentication)
     (%ck:+user-pin-count-low+             :user-pin-count-low)
     (%ck:+user-pin-final-try+             :user-pin-final-try)
     (%ck:+user-pin-locked+                :user-pin-locked)
     (%ck:+user-pin-to-be-changed+         :user-pin-to-be-changed)
     (%ck:+so-pin-count-low+               :so-pin-count-low)
     (%ck:+so-pin-final-try+               :so-pin-final-try)
     (%ck:+so-pin-locked+                  :so-pin-locked)
     (%ck:+so-pin-to-be-changed+           :so-pin-to-be-changed)))

(define-enum-converter user-type
    ((%ck:+so+                :so)
     (%ck:+user+              :user)
     (%ck:+context-specific+  :context-specific)))

(define-enum-converter state
    ((%ck:+ro-public-session+  :ro-public-session)
     (%ck:+ro-user-functions+  :ro-user-functions)
     (%ck:+rw-public-session+  :rw-public-session)
     (%ck:+rw-user-functions+  :rw-user-functions)
     (%ck:+rw-so-functions+    :rw-so-functions)))

(define-flag-converter convert-session-info-flags
    ((%ck:+rw-session+        :rw-session)
     (%ck:+serial-session+    :serial-session)))

(define-flag-converter convert-wait-for-slot-event-flags
    ((%ck:+DONT-BLOCK+ :dont-block)))

(define-enum-converter object-class
    ((%ck:+o-data+              :data)
     (%ck:+o-certificate+       :certificate)
     (%ck:+o-public-key+        :public-key)
     (%ck:+o-private-key+       :private-key)
     (%ck:+o-secret-key+        :secret-key)
     (%ck:+o-hw-feature+        :hw-feature)
     (%ck:+o-domain-parameters+ :domain-parameters)
     (%ck:+o-mechanism+         :mechanism)
     (%ck:+vendor-defined+      :vendor-defined)))

(define-enum-converter hardware-feature
    ((%ck:+h-monotonic-counter+ :monotonic-count)
     (%ck:+h-clock+             :clock)
     (%ck:+h-user-interface+    :user-interface)
     (%ck:+vendor-defined+      :vendor-defined)))

(define-enum-converter key-type
    ((%ck:+k-rsa+             :rsa)
     (%ck:+k-dsa+             :dsa)
     (%ck:+k-dh+              :dh)
     (%ck:+k-ecdsa+           :ecdsa)
     (%ck:+k-ec+              :ec)
     (%ck:+k-x9-42-dh+        :x9-42-dh)
     (%ck:+k-kea+             :kea)
     (%ck:+k-generic-secret+  :generic-secret)
     (%ck:+k-rc2+             :rc2)
     (%ck:+k-rc4+             :rc4)
     (%ck:+k-des+             :des)
     (%ck:+k-des2+            :des2)
     (%ck:+k-des3+            :des3)
     (%ck:+k-cast+            :cast)
     (%ck:+k-cast3+           :cast3)
     (%ck:+k-cast128+         :cast128)
     (%ck:+k-rc5+             :rc5)
     (%ck:+k-idea+            :idea)
     (%ck:+k-skipjack+        :skipjack)
     (%ck:+k-baton+           :baton)
     (%ck:+k-juniper+         :juniper)
     (%ck:+k-cdmf+            :cdmf)
     (%ck:+k-aes+             :aes)
     (%ck:+k-blowfish+        :blowfish)
     (%ck:+k-twofish+         :twofish)
     (%ck:+vendor-defined+    :vendor-defined)))

(define-enum-converter certificate-type
    ((%ck:+c-x-509+            :x-509)
     (%ck:+c-x-509-attr-cert+  :x-509-attr-cert)
     (%ck:+c-wtls+             :wtls)
     (%ck:+vendor-defined+     :vendor-defined)))

(define-enum-converter attribute-type
    ((%ck:+a-class+                       :class)
     (%ck:+a-token+                       :token)
     (%ck:+a-private+                     :private)
     (%ck:+a-label+                       :label)
     (%ck:+a-application+                 :application)
     (%ck:+a-value+                       :value)
     (%ck:+a-object-id+                   :object-id)
     (%ck:+a-certificate-type+            :certificate-type)
     (%ck:+a-issuer+                      :issuer)
     (%ck:+a-serial-number+               :serial-number)
     (%ck:+a-ac-issuer+                   :ac-issuer)
     (%ck:+a-owner+                       :owner)
     (%ck:+a-attr-types+                  :attr-types)
     (%ck:+a-trusted+                     :trusted)
     (%ck:+a-certificate-category+        :certificate-category)
     (%ck:+a-java-midp-security-domain+   :java-midp-security-domain)
     (%ck:+a-url+                         :url)
     (%ck:+a-hash-of-subject-public-key+  :hash-of-subject-public-key)
     (%ck:+a-hash-of-issuer-public-key+   :hash-of-issuer-public-key)
     (%ck:+a-check-value+                 :check-value)
     (%ck:+a-key-type+                    :key-type)
     (%ck:+a-subject+                     :subject)
     (%ck:+a-id+                          :id)
     (%ck:+a-sensitive+                   :sensitive)
     (%ck:+a-encrypt+                     :encrypt)
     (%ck:+a-decrypt+                     :decrypt)
     (%ck:+a-wrap+                        :wrap)
     (%ck:+a-unwrap+                      :unwrap)
     (%ck:+a-sign+                        :sign)
     (%ck:+a-sign-recover+                :sign-recover)
     (%ck:+a-verify+                      :verify)
     (%ck:+a-verify-recover+              :verify-recover)
     (%ck:+a-derive+                      :derive)
     (%ck:+a-start-date+                  :start-date)
     (%ck:+a-end-date+                    :end-date)
     (%ck:+a-modulus+                     :modulus)
     (%ck:+a-modulus-bits+                :modulus-bits)
     (%ck:+a-public-exponent+             :public-exponent)
     (%ck:+a-private-exponent+            :private-exponent)
     (%ck:+a-prime-1+                     :prime-1)
     (%ck:+a-prime-2+                     :prime-2)
     (%ck:+a-exponent-1+                  :exponent-1)
     (%ck:+a-exponent-2+                  :exponent-2)
     (%ck:+a-coefficient+                 :coefficient)
     (%ck:+a-prime+                       :prime)
     (%ck:+a-subprime+                    :subprime)
     (%ck:+a-base+                        :base)
     (%ck:+a-prime-bits+                  :prime-bits)
     (%ck:+a-sub-prime-bits+              :sub-prime-bits)
     (%ck:+a-value-bits+                  :value-bits)
     (%ck:+a-value-len+                   :value-len)
     (%ck:+a-extractable+                 :extractable)
     (%ck:+a-local+                       :local)
     (%ck:+a-never-extractable+           :never-extractable)
     (%ck:+a-always-sensitive+            :always-sensitive)
     (%ck:+a-key-gen-mechanism+           :key-gen-mechanism)
     (%ck:+a-modifiable+                  :modifiable)
     (%ck:+a-ecdsa-params+                :ecdsa-params)
     (%ck:+a-ec-params+                   :ec-params)
     (%ck:+a-ec-point+                    :ec-point)
     (%ck:+a-secondary-auth+              :secondary-auth)
     (%ck:+a-auth-pin-flags+              :auth-pin-flags)
     (%ck:+a-always-authenticate+         :always-authenticate)
     (%ck:+a-wrap-with-trusted+           :wrap-with-trusted)
     (%ck:+a-hw-feature-type+             :hw-feature-type)
     (%ck:+a-reset-on-init+               :reset-on-init)
     (%ck:+a-has-reset+                   :has-reset)
     (%ck:+a-pixel-x+                     :pixel-x)
     (%ck:+a-pixel-y+                     :pixel-y)
     (%ck:+a-resolution+                  :resolution)
     (%ck:+a-char-rows+                   :char-rows)
     (%ck:+a-char-columns+                :char-columns)
     (%ck:+a-color+                       :color)
     (%ck:+a-bits-per-pixel+              :bits-per-pixel)
     (%ck:+a-char-sets+                   :char-sets)
     (%ck:+a-encoding-methods+            :encoding-methods)
     (%ck:+a-mime-types+                  :mime-types)
     (%ck:+a-mechanism-type+              :mechanism-type)
     (%ck:+a-required-cms-attributes+     :required-cms-attributes)
     (%ck:+a-default-cms-attributes+      :default-cms-attributes)
     (%ck:+a-supported-cms-attributes+    :supported-cms-attributes)
     (%ck:+a-wrap-template+               :wrap-template)
     (%ck:+a-unwrap-template+             :unwrap-template)
     (%ck:+a-allowed-mechanisms+          :allowed-mechanisms)
     (%ck:+vendor-defined+                :vendor-defined)))

(define-enum-converter mechanism-type
    ((%ck:+m-rsa-pkcs-key-pair-gen+      :rsa-pkcs-key-pair-gen)
     (%ck:+m-rsa-pkcs+                   :rsa-pkcs)
     (%ck:+m-rsa-9796+                   :rsa-9796)
     (%ck:+m-rsa-x-509+                  :rsa-x-509)
     (%ck:+m-md2-rsa-pkcs+               :md2-rsa-pkcs)
     (%ck:+m-md5-rsa-pkcs+               :md5-rsa-pkcs)
     (%ck:+m-sha1-rsa-pkcs+              :sha1-rsa-pkcs)
     (%ck:+m-ripemd128-rsa-pkcs+         :ripemd128-rsa-pkcs)
     (%ck:+m-ripemd160-rsa-pkcs+         :ripemd160-rsa-pkcs)
     (%ck:+m-rsa-pkcs-oaep+              :rsa-pkcs-oaep)
     (%ck:+m-rsa-x9-31-key-pair-gen+     :rsa-x9-31-key-pair-gen)
     (%ck:+m-rsa-x9-31+                  :rsa-x9-31)
     (%ck:+m-sha1-rsa-x9-31+             :sha1-rsa-x9-31)
     (%ck:+m-rsa-pkcs-pss+               :rsa-pkcs-pss)
     (%ck:+m-sha1-rsa-pkcs-pss+          :sha1-rsa-pkcs-pss)
     (%ck:+m-dsa-key-pair-gen+           :dsa-key-pair-gen)
     (%ck:+m-dsa+                        :dsa)
     (%ck:+m-dsa-sha1+                   :dsa-sha1)
     (%ck:+m-dh-pkcs-key-pair-gen+       :dh-pkcs-key-pair-gen)
     (%ck:+m-dh-pkcs-derive+             :dh-pkcs-derive)
     (%ck:+m-x9-42-dh-key-pair-gen+      :x9-42-dh-key-pair-gen)
     (%ck:+m-x9-42-dh-derive+            :x9-42-dh-derive)
     (%ck:+m-x9-42-dh-hybrid-derive+     :x9-42-dh-hybrid-derive)
     (%ck:+m-x9-42-mqv-derive+           :x9-42-mqv-derive)
     (%ck:+m-sha256-rsa-pkcs+            :sha256-rsa-pkcs)
     (%ck:+m-sha384-rsa-pkcs+            :sha384-rsa-pkcs)
     (%ck:+m-sha512-rsa-pkcs+            :sha512-rsa-pkcs)
     (%ck:+m-sha256-rsa-pkcs-pss+        :sha256-rsa-pkcs-pss)
     (%ck:+m-sha384-rsa-pkcs-pss+        :sha384-rsa-pkcs-pss)
     (%ck:+m-sha512-rsa-pkcs-pss+        :sha512-rsa-pkcs-pss)
     (%ck:+m-rc2-key-gen+                :rc2-key-gen)
     (%ck:+m-rc2-ecb+                    :rc2-ecb)
     (%ck:+m-rc2-cbc+                    :rc2-cbc)
     (%ck:+m-rc2-mac+                    :rc2-mac)
     (%ck:+m-rc2-mac-general+            :rc2-mac-general)
     (%ck:+m-rc2-cbc-pad+                :rc2-cbc-pad)
     (%ck:+m-rc4-key-gen+                :rc4-key-gen)
     (%ck:+m-rc4+                        :rc4)
     (%ck:+m-des-key-gen+                :des-key-gen)
     (%ck:+m-des-ecb+                    :des-ecb)
     (%ck:+m-des-cbc+                    :des-cbc)
     (%ck:+m-des-mac+                    :des-mac)
     (%ck:+m-des-mac-general+            :des-mac-general)
     (%ck:+m-des-cbc-pad+                :des-cbc-pad)
     (%ck:+m-des2-key-gen+               :des2-key-gen)
     (%ck:+m-des3-key-gen+               :des3-key-gen)
     (%ck:+m-des3-ecb+                   :des3-ecb)
     (%ck:+m-des3-cbc+                   :des3-cbc)
     (%ck:+m-des3-mac+                   :des3-mac)
     (%ck:+m-des3-mac-general+           :des3-mac-general)
     (%ck:+m-des3-cbc-pad+               :des3-cbc-pad)
     (%ck:+m-cdmf-key-gen+               :cdmf-key-gen)
     (%ck:+m-cdmf-ecb+                   :cdmf-ecb)
     (%ck:+m-cdmf-cbc+                   :cdmf-cbc)
     (%ck:+m-cdmf-mac+                   :cdmf-mac)
     (%ck:+m-cdmf-mac-general+           :cdmf-mac-general)
     (%ck:+m-cdmf-cbc-pad+               :cdmf-cbc-pad)
     (%ck:+m-md2+                        :md2)
     (%ck:+m-md2-hmac+                   :md2-hmac)
     (%ck:+m-md2-hmac-general+           :md2-hmac-general)
     (%ck:+m-md5+                        :md5)
     (%ck:+m-md5-hmac+                   :md5-hmac)
     (%ck:+m-md5-hmac-general+           :md5-hmac-general)
     (%ck:+m-sha-1+                      :sha-1)
     (%ck:+m-sha-1-hmac+                 :sha-1-hmac)
     (%ck:+m-sha-1-hmac-general+         :sha-1-hmac-general)
     (%ck:+m-ripemd128+                  :ripemd128)
     (%ck:+m-ripemd128-hmac+             :ripemd128-hmac)
     (%ck:+m-ripemd128-hmac-general+     :ripemd128-hmac-general)
     (%ck:+m-ripemd160+                  :ripemd160)
     (%ck:+m-ripemd160-hmac+             :ripemd160-hmac)
     (%ck:+m-ripemd160-hmac-general+     :ripemd160-hmac-general)
     (%ck:+m-sha256+                     :sha256)
     (%ck:+m-sha256-hmac+                :sha256-hmac)
     (%ck:+m-sha256-hmac-general+        :sha256-hmac-general)
     (%ck:+m-sha384+                     :sha384)
     (%ck:+m-sha384-hmac+                :sha384-hmac)
     (%ck:+m-sha384-hmac-general+        :sha384-hmac-general)
     (%ck:+m-sha512+                     :sha512)
     (%ck:+m-sha512-hmac+                :sha512-hmac)
     (%ck:+m-sha512-hmac-general+        :sha512-hmac-general)
     (%ck:+m-cast-key-gen+               :cast-key-gen)
     (%ck:+m-cast-ecb+                   :cast-ecb)
     (%ck:+m-cast-cbc+                   :cast-cbc)
     (%ck:+m-cast-mac+                   :cast-mac)
     (%ck:+m-cast-mac-general+           :cast-mac-general)
     (%ck:+m-cast-cbc-pad+               :cast-cbc-pad)
     (%ck:+m-cast3-key-gen+              :cast3-key-gen)
     (%ck:+m-cast3-ecb+                  :cast3-ecb)
     (%ck:+m-cast3-cbc+                  :cast3-cbc)
     (%ck:+m-cast3-mac+                  :cast3-mac)
     (%ck:+m-cast3-mac-general+          :cast3-mac-general)
     (%ck:+m-cast3-cbc-pad+              :cast3-cbc-pad)
     (%ck:+m-cast5-key-gen+              :cast5-key-gen)
     (%ck:+m-cast128-key-gen+            :cast128-key-gen)
     (%ck:+m-cast5-ecb+                  :cast5-ecb)
     (%ck:+m-cast128-ecb+                :cast128-ecb)
     (%ck:+m-cast5-cbc+                  :cast5-cbc)
     (%ck:+m-cast128-cbc+                :cast128-cbc)
     (%ck:+m-cast5-mac+                  :cast5-mac)
     (%ck:+m-cast128-mac+                :cast128-mac)
     (%ck:+m-cast5-mac-general+          :cast5-mac-general)
     (%ck:+m-cast128-mac-general+        :cast128-mac-general)
     (%ck:+m-cast5-cbc-pad+              :cast5-cbc-pad)
     (%ck:+m-cast128-cbc-pad+            :cast128-cbc-pad)
     (%ck:+m-rc5-key-gen+                :rc5-key-gen)
     (%ck:+m-rc5-ecb+                    :rc5-ecb)
     (%ck:+m-rc5-cbc+                    :rc5-cbc)
     (%ck:+m-rc5-mac+                    :rc5-mac)
     (%ck:+m-rc5-mac-general+            :rc5-mac-general)
     (%ck:+m-rc5-cbc-pad+                :rc5-cbc-pad)
     (%ck:+m-idea-key-gen+               :idea-key-gen)
     (%ck:+m-idea-ecb+                   :idea-ecb)
     (%ck:+m-idea-cbc+                   :idea-cbc)
     (%ck:+m-idea-mac+                   :idea-mac)
     (%ck:+m-idea-mac-general+           :idea-mac-general)
     (%ck:+m-idea-cbc-pad+               :idea-cbc-pad)
     (%ck:+m-generic-secret-key-gen+     :generic-secret-key-gen)
     (%ck:+m-concatenate-base-and-key+   :concatenate-base-and-key)
     (%ck:+m-concatenate-base-and-data+  :concatenate-base-and-data)
     (%ck:+m-concatenate-data-and-base+  :concatenate-data-and-base)
     (%ck:+m-xor-base-and-data+          :xor-base-and-data)
     (%ck:+m-extract-key-from-key+       :extract-key-from-key)
     (%ck:+m-ssl3-pre-master-key-gen+    :ssl3-pre-master-key-gen)
     (%ck:+m-ssl3-master-key-derive+     :ssl3-master-key-derive)
     (%ck:+m-ssl3-key-and-mac-derive+    :ssl3-key-and-mac-derive)
     (%ck:+m-ssl3-master-key-derive-dh+  :ssl3-master-key-derive-dh)
     (%ck:+m-tls-pre-master-key-gen+     :tls-pre-master-key-gen)
     (%ck:+m-tls-master-key-derive+      :tls-master-key-derive)
     (%ck:+m-tls-key-and-mac-derive+     :tls-key-and-mac-derive)
     (%ck:+m-tls-master-key-derive-dh+   :tls-master-key-derive-dh)
     (%ck:+m-ssl3-md5-mac+               :ssl3-md5-mac)
     (%ck:+m-ssl3-sha1-mac+              :ssl3-sha1-mac)
     (%ck:+m-md5-key-derivation+         :md5-key-derivation)
     (%ck:+m-md2-key-derivation+         :md2-key-derivation)
     (%ck:+m-sha1-key-derivation+        :sha1-key-derivation)
     (%ck:+m-pbe-md2-des-cbc+            :pbe-md2-des-cbc)
     (%ck:+m-pbe-md5-des-cbc+            :pbe-md5-des-cbc)
     (%ck:+m-pbe-md5-cast-cbc+           :pbe-md5-cast-cbc)
     (%ck:+m-pbe-md5-cast3-cbc+          :pbe-md5-cast3-cbc)
     (%ck:+m-pbe-md5-cast5-cbc+          :pbe-md5-cast5-cbc)
     (%ck:+m-pbe-md5-cast128-cbc+        :pbe-md5-cast128-cbc)
     (%ck:+m-pbe-sha1-cast5-cbc+         :pbe-sha1-cast5-cbc)
     (%ck:+m-pbe-sha1-cast128-cbc+       :pbe-sha1-cast128-cbc)
     (%ck:+m-pbe-sha1-rc4-128+           :pbe-sha1-rc4-128)
     (%ck:+m-pbe-sha1-rc4-40+            :pbe-sha1-rc4-40)
     (%ck:+m-pbe-sha1-des3-ede-cbc+      :pbe-sha1-des3-ede-cbc)
     (%ck:+m-pbe-sha1-des2-ede-cbc+      :pbe-sha1-des2-ede-cbc)
     (%ck:+m-pbe-sha1-rc2-128-cbc+       :pbe-sha1-rc2-128-cbc)
     (%ck:+m-pbe-sha1-rc2-40-cbc+        :pbe-sha1-rc2-40-cbc)
     (%ck:+m-pkcs5-pbkd2+                :pkcs5-pbkd2)
     (%ck:+m-pba-sha1-with-sha1-hmac+    :pba-sha1-with-sha1-hmac)
     (%ck:+m-key-wrap-lynks+             :key-wrap-lynks)
     (%ck:+m-key-wrap-set-oaep+          :key-wrap-set-oaep)
     (%ck:+m-skipjack-key-gen+           :skipjack-key-gen)
     (%ck:+m-skipjack-ecb64+             :skipjack-ecb64)
     (%ck:+m-skipjack-cbc64+             :skipjack-cbc64)
     (%ck:+m-skipjack-ofb64+             :skipjack-ofb64)
     (%ck:+m-skipjack-cfb64+             :skipjack-cfb64)
     (%ck:+m-skipjack-cfb32+             :skipjack-cfb32)
     (%ck:+m-skipjack-cfb16+             :skipjack-cfb16)
     (%ck:+m-skipjack-cfb8+              :skipjack-cfb8)
     (%ck:+m-skipjack-wrap+              :skipjack-wrap)
     (%ck:+m-skipjack-private-wrap+      :skipjack-private-wrap)
     (%ck:+m-skipjack-relayx+            :skipjack-relayx)
     (%ck:+m-kea-key-pair-gen+           :kea-key-pair-gen)
     (%ck:+m-kea-key-derive+             :kea-key-derive)
     (%ck:+m-fortezza-timestamp+         :fortezza-timestamp)
     (%ck:+m-baton-key-gen+              :baton-key-gen)
     (%ck:+m-baton-ecb128+               :baton-ecb128)
     (%ck:+m-baton-ecb96+                :baton-ecb96)
     (%ck:+m-baton-cbc128+               :baton-cbc128)
     (%ck:+m-baton-counter+              :baton-counter)
     (%ck:+m-baton-shuffle+              :baton-shuffle)
     (%ck:+m-baton-wrap+                 :baton-wrap)
     (%ck:+m-ecdsa-key-pair-gen+         :ecdsa-key-pair-gen)
     (%ck:+m-ec-key-pair-gen+            :ec-key-pair-gen)
     (%ck:+m-ecdsa+                      :ecdsa)
     (%ck:+m-ecdsa-sha1+                 :ecdsa-sha1)
     (%ck:+m-ecdh1-derive+               :ecdh1-derive)
     (%ck:+m-ecdh1-cofactor-derive+      :ecdh1-cofactor-derive)
     (%ck:+m-ecmqv-derive+               :ecmqv-derive)
     (%ck:+m-juniper-key-gen+            :juniper-key-gen)
     (%ck:+m-juniper-ecb128+             :juniper-ecb128)
     (%ck:+m-juniper-cbc128+             :juniper-cbc128)
     (%ck:+m-juniper-counter+            :juniper-counter)
     (%ck:+m-juniper-shuffle+            :juniper-shuffle)
     (%ck:+m-juniper-wrap+               :juniper-wrap)
     (%ck:+m-fasthash+                   :fasthash)
     (%ck:+m-aes-key-gen+                :aes-key-gen)
     (%ck:+m-aes-ecb+                    :aes-ecb)
     (%ck:+m-aes-cbc+                    :aes-cbc)
     (%ck:+m-aes-mac+                    :aes-mac)
     (%ck:+m-aes-mac-general+            :aes-mac-general)
     (%ck:+m-aes-cbc-pad+                :aes-cbc-pad)
     (%ck:+m-dsa-parameter-gen+          :dsa-parameter-gen)
     (%ck:+m-dh-pkcs-parameter-gen+      :dh-pkcs-parameter-gen)
     (%ck:+m-x9-42-dh-parameter-gen+     :x9-42-dh-parameter-gen)
     (%ck:+vendor-defined+               :vendor-defined)))

(define-flag-converter convert-mechanism-info-flags
    ((%ck:+f-hw+                 :hw)
     (%ck:+f-encrypt+            :encrypt)
     (%ck:+f-decrypt+            :decrypt)
     (%ck:+f-digest+             :digest)
     (%ck:+f-sign+               :sign)
     (%ck:+f-sign-recover+       :sign-recover)
     (%ck:+f-verify+             :verify)
     (%ck:+f-verify-recover+     :verify-recover)
     (%ck:+f-generate+           :generate)
     (%ck:+f-generate-key-pair+  :generate-key-pair)
     (%ck:+f-wrap+               :wrap)
     (%ck:+f-unwrap+             :unwrap)
     (%ck:+f-derive+             :derive)
     (%ck:+f-extension+          :extension)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PKCS11-ERROR

(define-condition pkcs11-error (error)
  ((label      :initarg :label     :reader pkcs11-error-label)
   (code       :initarg :code      :reader pkcs11-error-code)
   (function   :initarg :function  :reader pkcs11-error-function))
  (:report (lambda (condition stream)
             (format stream "PKCS11 Error: ~A (~A) in ~A"
                     (pkcs11-error-label condition)
                     (pkcs11-error-code condition)
                     (pkcs11-error-function condition))
             condition)))

(defun check-rv (rv &optional function continue)
  (unless (zerop rv)
    (let ((args  (list 'pkcs11-error :label (return-value :decode rv)
                                     :code rv
                                     :function function)))
      (if continue
          (apply (function cerror) "Ignore and continue" args)
          (apply (function error) args))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun ckbool (generalized-boolean)
  (if (integerp generalized-boolean)
      (if (zerop generalized-boolean)
          %ck:+false+
          %ck:+true+)
      (if generalized-boolean
          %ck:+true+
          %ck:+false+)))

(defun unavailable-information-p (value)
  (= %ck:+unavailable-information+
     (logand %ck:+unavailable-information+ value)))

(defun invalid-pointer-p (pointer)
  (or (null-pointer-p pointer)
      (unavailable-information-p (pointer-address pointer))))

(defmacro with-pkcs11 (&body body)
  `(progn
     (check-rv (%ck:initialize (null-pointer)) "C_Initialize" :continue)
     (unwind-protect (progn ,@body)
       (check-rv (%ck:finalize (null-pointer)) "C_Finalize" :continue))))

(defstruct version
  major
  minor)

(defun version (operation version)
  (ecase operation
    ((:decode) (with-foreign-slots ((%ck:major %ck:minor) version (:struct %ck:version))
                 (make-version :major %ck:major
                               :minor %ck:minor)))))

(defstruct info
  cryptoki-version
  manufacturer-id
  flags
  library-description
  library-version)

(defun get-info ()
  "RETURN: an INFO structure."
  (with-foreign-object (info '(:struct %ck:info))
    (check-rv (%ck:get-info info) "C_GetInfo")
    (flet ((str (slot size)
             (foreign-string-to-lisp
              (foreign-slot-pointer info '(:struct %ck:info) slot)
              :count size :encoding :ascii))
           (ver (slot)
             (version :decode (foreign-slot-pointer info '(:struct %ck:info) slot))))
      (make-info
       :cryptoki-version    (ver '%ck:cryptoki-version)
       :manufacturer-id     (str '%ck:manufacturer-id 32)
       ;; flags is reserved for future extensions, should be 0.
       :flags               (foreign-slot-value info '(:struct %ck:info) '%ck:flags)
       :library-description (str '%ck:library-description 32)
       :library-version     (ver '%ck:library-version)))))

(defun get-slot-list (token-present)
  "RETURN: a list of SLOT-IDs."
  (with-foreign-object (count :ulong)
    (check-rv (%ck:get-slot-list (ckbool token-present) (null-pointer) count) "C_GetSlotList")
    (let ((slot-count  (mem-ref count :ulong)))
      (when (plusp slot-count)
        (with-foreign-object (slot-ids '%ck:slot-id slot-count)
          (check-rv (%ck:get-slot-list (ckbool token-present) slot-ids count))
          (loop :for i :below slot-count
                :collect (mem-aref slot-ids '%ck:slot-id i)))))))

(defstruct slot-info
  slot-description
  manufacturer-id
  flags
  hardware-version
  firmware-version)

(defun get-slot-info (slot-id)
  "RETURN: a SLOT-INFO structure."
  (check-type slot-id slot-id)
  (with-foreign-object (info '(:struct %ck:slot-info))
    (check-rv (%ck:get-slot-info slot-id info) "C_GetSlotInfo")
    (flet ((str (slot size)
             (foreign-string-to-lisp
              (foreign-slot-pointer info '(:struct %ck:slot-info) slot)
              :count size :encoding :ascii))
           (ver (slot)
             (version :decode (foreign-slot-pointer info '(:struct %ck:slot-info) slot))))
      (make-slot-info
       :slot-description (str '%ck:slot-description 64)
       :manufacturer-id  (str '%ck:manufacturer-id  32)
       :flags            (convert-slot-info-flags :decode (foreign-slot-value info '(:struct %ck:slot-info) '%ck:flags))
       :hardware-version (ver '%ck:hardware-version)
       :firmware-version (ver '%ck:firmware-version)))))

(defstruct token-info
  label manufacturer-id model serial-number flags max-session-count
  session-count max-rw-session-count rw-session-count max-pin-len
  min-pin-len total-public-memory free-public-memory
  total-private-mmeory free-private-memory hardware-version
  firmware-version utc-time)

(defun get-token-info (slot-id)
  "RETURN: a TOKEN-INFO structure."
  (check-type slot-id slot-id)
  (with-foreign-object (info '(:struct %ck:token-info))
    (check-rv (%ck:get-token-info slot-id info) "C_GetTokenInfo")
    (flet ((str (slot size)
             (foreign-string-to-lisp
              (foreign-slot-pointer info '(:struct %ck:token-info) slot)
              :count size :encoding :ascii))
           (ver (slot)
             (version :decode (foreign-slot-pointer info '(:struct %ck:token-info) slot)))
           (long (slot)
             (let ((value (foreign-slot-value info '(:struct %ck:token-info) slot)))
               (cond
                 ((= value %ck:+unavailable-information+) nil)
                 ((= value %ck:+effectively-infinite+)    :infinite)
                 (t                                       value)))))
      (make-token-info
       :label                (str '%ck:label            32)
       :manufacturer-id      (str '%ck:manufacturer-id  32)
       :model                (str '%ck:model            16)
       :serial-number        (str '%ck:serial-number    16)
       :flags                (convert-token-info-flags :decode (foreign-slot-value info '(:struct %ck:token-info) '%ck:flags))
       :max-session-count    (long '%ck:max-session-count)
       :session-count        (long '%ck:session-count)
       :max-rw-session-count (long '%ck:max-rw-session-count)
       :rw-session-count     (long '%ck:rw-session-count)
       :max-pin-len          (long '%ck:max-pin-len)
       :min-pin-len          (long '%ck:min-pin-len)
       :total-public-memory  (long '%ck:total-public-memory)
       :free-public-memory   (long '%ck:free-public-memory)
       :total-private-mmeory (long '%ck:total-private-mmeory)
       :free-private-memory  (long '%ck:free-private-memory)
       :hardware-version     (ver '%ck:hardware-version)
       :firmware-version     (ver '%ck:firmware-version)
       :utc-time             (str '%ck:utc-time 16)))))

(defun wait-for-slot-event (flags)
  "RETURN: The SLOT-ID where an event occured."
  (check-type flags (or integer keyword))
  (with-foreign-object (slot-id :ulong)
    (check-rv (%ck:wait-for-slot-event (if (integerp flags)
                                           flags
                                           (convert-wait-for-slot-event-flags :encode flags))
                                       slot-id
                                       (null-pointer))
              "C_WaitForSlotEvent")
    (mem-ref slot-id :ulong)))

(defun get-mechanism-list (slot-id)
  "RETURN: a list of MECHANISM-TYPE."
  (check-type slot-id slot-id)
  (with-foreign-object (count :ulong)
    (check-rv (%ck:get-mechanism-list slot-id (null-pointer) count) "C_GetMechanismList")
    (let ((mechanism-count  (mem-ref count :ulong)))
      (when (plusp mechanism-count)
        (with-foreign-object (mechanism-types '%ck:mechanism-type mechanism-count)
          (check-rv (%ck:get-mechanism-list slot-id mechanism-types count))
          (loop :for i :below mechanism-count
                :collect (mechanism-type :decode (mem-aref mechanism-types '%ck:mechanism-type i))))))))

(defstruct mechanism-info
  min-key-size max-key-size flags)

(defun get-mechanism-info (slot-id mechanism-type)
  "RETURN: a MECHANIS-INFO structure."
  (check-type slot-id slot-id)
  (check-type mechanism-type mechanism-type)
  (with-foreign-object (info '(:struct %ck:mechanism-info))
    (check-rv (%ck:get-mechanism-info slot-id
                                      (if (integerp mechanism-type)
                                          mechanism-type
                                          (mechanism-type :encode mechanism-type))
                                      info)
              "C_GetMechanismInfo")
    (flet ((long (slot)
             (foreign-slot-value info '(:struct %ck:mechanism-info) slot)))
      (make-mechanism-info
       :min-key-size (long '%ck:min-key-size)
       :max-key-size (long '%ck:min-key-size)
       :flags        (convert-mechanism-info-flags :decode (foreign-slot-value info '(:struct %ck:mechanism-info) '%ck:flags))))))

(defun string-from-utf-8 (bytes)
  (octets-to-string bytes :encoding :utf-8 :errorp t))

(defun string-to-utf-8 (string &key size padchar)
  ;; Note: we could be more optimized by storing directly a pre-allocated result vector,
  ;;       but it's expected to be used only on small strings…
  (let ((bytes (string-to-octets string :encoding :utf-8 :use-bom nil)))
    (when (and size (< size (length bytes)))
      (setf bytes (subseq bytes 0 size))
      (unless (ignore-errors (octets-to-string bytes :encoding :utf-8 :errorp t))
        (error "Truncated utf-8 byte sequence in the middle of a utf-8 code sequence!~%string: ~S~%bytes: ~S~%"
               string bytes)))
    (if (and padchar size (< (length bytes) size))
        (let ((padstring (string-to-octets (string padchar) :encoding :utf-8 :use-bom nil)))
          (unless (zerop (mod (- size (length bytes)) (length padstring)))
            (error "pad character is encoded as a utf-8 code sequence of length ~D, which is not a divisor of the required padding length ~D. Try to specify an ASCII character as pad character!"
                   (length padstring)  (- size (length bytes))))
          (let ((result (apply (function make-array) size :element-type 'octet
                               (when (= 1 (length padstring))
                                 (list :initial-element (aref padstring 0))))))
            (replace result bytes)
            (unless (= 1 (length padstring))
              (loop :for s :from (length bytes) :below (length result) :by (length padstring)
                    :do (replace result padstring :start1 s)))
            result))
        bytes)))

(defun init-token (slot-id pin label)
  "RETURN: the PIN string as returned by C_InitToken."
  (check-type slot-id slot-id)
  (check-type pin     string)
  (check-type label   string)
  (warn "Not tested yet! Please, provide new smart-cards!")
  (let* ((label  (string-to-utf-8 label :size 32 :padchar #\space))
         (pin    (string-to-utf-8 pin))
         (pinlen (length pin)))
    (with-foreign-objects ((flabel  :uchar 32)
                           (fpin    :uchar pinlen)
                           (fpinlen :ulong))
      (dotimes (i 32)     (setf (mem-aref flabel :uchar i) (aref label i)))
      (dotimes (i pinlen) (setf (mem-aref fpin   :uchar i) (aref pin   i)))
      (setf (mem-ref fpinlen :ulong) pinlen)
      (check-rv (%ck:init-token slot-id fpin fpinlen flabel) "C_InitToken")
      (foreign-string-to-lisp fpin :count fpinlen :encoding :ascii))))

(defparameter *references* '() "A-list mapping foreign pointers with lisp objets")
(defstruct entry
  slot-id reference pointer session)
(defun enter-reference (slot-id reference)
  (let ((pointer (foreign-alloc :ulong :initial-element #x1BADBEEF)))
    (push (make-entry :slot-id slot-id :pointer pointer :reference reference) *references*)
    pointer))
(defun find-entry-with-pointer (pointer)
  (find pointer *references* :key (function entry-pointer)))
(defun find-entry-with-session (session)
  (find session *references* :key (function entry-session)))
(defun associate-pointer-with-session (pointer session)
  (let ((entry (find-entry-with-pointer pointer)))
    (when entry
      (setf (entry-session entry) session))))
(defun clear-entry-with-pointer (pointer)
  (setf *references* (delete (find-entry-with-pointer pointer) *references*)))
(defun clear-entry-with-session (session)
  (setf *references* (delete (find-entry-with-session session) *references*)))
(defun clear-entries-with-slot-id (slot-id)
  (setf *references* (delete slot-id  *references* :key (function entry-slot-id))))

(defun open-session (slot-id flags application-reference notify-function)
  "RETURN: a SESSION-HANDLER."
  (check-type slot-id slot-id)
  (check-type notify-function null "TODO: Sorry, notify-function are not supported yet.") ; TODO.
  (let ((application-reference (if application-reference
                                   (enter-reference slot-id application-reference)
                                   (null-pointer))))
    (with-foreign-object (session-handle '%ck:session-handle)
      (check-rv (%ck:open-session slot-id
                                  (if (integerp flags)
                                      flags
                                      (convert-session-info-flags :encode flags))
                                  application-reference
                                  (null-pointer) ; TODO notify-function
                                  session-handle)
                "C_OpenSession")
      (let ((session (mem-ref session-handle '%ck:session-handle)))
        (associate-pointer-with-session application-reference session)
        session))))

(defun close-session (session-handle)
  (check-type session-handle session-handle)
  (check-rv (%ck:close-session session-handle) "C_CloseSession")
  (clear-entry-with-session session-handle)
  (values))

(defun close-all-sessions (slot-id)
  (check-type slot-id slot-id)
  (check-rv (%ck:close-all-sessions slot-id) "C_CloseAllSessions")
  (clear-entries-with-slot-id slot-id)
  (values))

 (defmacro with-open-session ((session-var slot-id &key flags application-reference notify-function
                                                    (if-open-session-fails :error)) &body body)
  (let ((vflags (gensym))
        (vsession (gensym)))
    `(flet ((open-it ()
              (open-session ,slot-id
                            (let ((,vflags ,flags))
                              (if (integerp ,vflags)
                                  (logior ,vflags %ck:+serial-session+)
                                  (cons :serial-session ,vflags)))
                            ,application-reference
                            ,notify-function)))
       (let* ((,vsession (ecase ,if-open-session-fails
                           ((:error)  (open-it))
                           ((nil)     (ignore-errors (open-it)))))
              (,session-var ,vsession))
         (unwind-protect (progn ,@body)
           (when ,vsession (close-session ,vsession)))))))

(defstruct session-info
  slot-id state flags device-error)

(defun get-session-info (session)
  "RETURN: a SESSION-INFO structure."
  (check-type session session-handle)
  (with-foreign-object (info '(:struct %ck:session-info))
    (check-rv (%ck:get-session-info session info) "C_GetSessionInfo")
    (flet ((long (slot)
             (foreign-slot-value info '(:struct %ck:session-info) slot)))
      (make-session-info
       :slot-id      (long '%ck:slot-id)
       :state        (state :decode (long '%ck:state))
       :flags        (convert-session-info-flags :decode (long '%ck:flags))
       :device-error (long '%ck:device-error)))))

;; Probably better not to keep long lived C buffers.
;; Furthermore, the operation can be resumed in a different process,
;; so it's better to convert to lisp data that's more easily
;; serializable.
;;
;; (defstruct operation-state
;;   data
;;   size)
;; (defun free-operation-state (operation-state)
;;   (foreign-free (operation-state-data operation-state)))
;;         (make-operation-state :data operation-state
;;                               :size (mem-ref operation-state-len :ulong))

(defun get-operation-state (session)
  "RETURN: a vector of octet containing the session state."
  (check-type session session-handle)
  (with-foreign-object (operation-state-len :ulong)
    (check-rv (%ck:get-operation-state session (null-pointer) operation-state-len) "C_GetOperationState")
    (with-foreign-object (operation-state :uchar (mem-ref operation-state-len :ulong))
      (check-rv (%ck:get-operation-state session operation-state operation-state-len) "C_GetOperationState")
      (foreign-vector operation-state :uchar 'octet (mem-ref operation-state-len :ulong)))))


(defun set-operation-state (session state &key (encryption-key 0) (authentication-key 0))
  (check-type session             session-handle)
  (check-type state               (vector octet))
  (check-type encryption-key      object-handle)
  (check-type authentication-key  object-handle)
  (with-foreign-objects ((operation-state     :uchar (length state))
                         (operation-state-len :ulong))
    (loop :for i :below (length state)
          :do (setf (mem-aref operation-state :uchar i) (aref state i)))
    (setf (mem-ref operation-state-len :ulong) (length state))
    (check-rv (%ck:set-operation-state session operation-state operation-state-len
                                       encryption-key authentication-key)
              "C_SetOperationState")))

(defun login (session user-type pin)
  (check-type session   session-handle)
  (check-type user-type (or integer keyword))
  (check-type pin       (or null string))
  (if pin
      (let* ((pin    (string-to-utf-8 pin))
             (pinlen (length pin)))
        (with-foreign-objects ((fpin    :uchar pinlen))
          (dotimes (i pinlen) (setf (mem-aref fpin :uchar i) (aref pin i)))
          (check-rv (%ck:login session (if (integerp user-type)
                                           user-type
                                           (user-type :encode user-type))
                               fpin pinlen)
                    "C_Login")))
      (check-rv (%ck:login session (if (integerp user-type)
                                       user-type
                                       (user-type :encode user-type))
                           (null-pointer) 0)
                "C_Login")))

(defun logout (session)
  (check-type session   session-handle)
  (check-rv (%ck:logout session) "C_Logout"))

(defvar *verbose* t)

(defun call-logged-in (session slot-id thunk &key why)
  "RETURN: the results of THUNK, or NIL when login was impossible."
  (check-type session session-handle)
  (check-type slot-id slot-id)
  (check-type thunk   (or function symbol))
  (when (handler-case
            (let* ((info  (get-token-info slot-id))
                   (flags (token-info-flags info)))
              (login session %ck:+user+ (if (member :protected-authentication-path flags)
                                            (progn
                                              (format t "~&~@[Logging for ~A.~%~]Please, enter pin on pin-pad (~:[~D digits~;~D to ~D digits~]).~%"
                                                      why
                                                      (/= (token-info-min-pin-len info) (token-info-max-pin-len info))
                                                      (token-info-min-pin-len info)
                                                      (token-info-max-pin-len info))
                                              (finish-output)
                                              nil)
                                            (read-pin (token-info-min-pin-len info) (token-info-max-pin-len info))))
              t)
          (pkcs11-error (err)
            (princ err *error-output*)
            (terpri *error-output*)
            nil))
    (unwind-protect (progn
                      (when *verbose*
                        (fresh-line)
                        (format t "~&Logged in~@[, for ~A~].~%" why)
                        (finish-output))
                      (funcall thunk))
      (logout session)
      (when *verbose*
        (fresh-line)
        (format t "~&Logged out~@[ for ~A~].~%" why)
        (finish-output)))))

(defmacro do-logged-in ((session slot-id &optional why) &body body)
  `(call-logged-in ,session ,slot-id (lambda () ,@body) :why ,why))

(defun init-pin (session pin)
  (check-type session   session-handle)
  (check-type pin       string)
  (let* ((pin    (string-to-utf-8 pin))
         (pinlen (length pin)))
    (with-foreign-objects ((fpin    :uchar pinlen)
                           (fpinlen :ulong))
      (dotimes (i pinlen) (setf (mem-aref fpin :uchar i) (aref pin i)))
      (setf (mem-ref fpinlen :ulong) pinlen)
      (check-rv (%ck:init-pin session fpin fpinlen) "C_InitPIN"))))

(defun set-pin (session old-pin new-pin)
  (check-type session   session-handle)
  (check-type old-pin   string)
  (check-type new-pin   string)
  (let* ((old-pin    (string-to-utf-8 old-pin))
         (new-pin    (string-to-utf-8 new-pin))
         (old-pinlen (length old-pin))
         (new-pinlen (length new-pin)))
    (with-foreign-objects ((fold-pin    :uchar old-pinlen)
                           (fnew-pin    :uchar new-pinlen)
                           (fold-pinlen :ulong)
                           (fnew-pinlen :ulong))
      (dotimes (i old-pinlen) (setf (mem-aref fold-pin :uchar i) (aref old-pin i)))
      (dotimes (i new-pinlen) (setf (mem-aref fnew-pin :uchar i) (aref new-pin i)))
      (setf (mem-ref fold-pinlen :ulong) old-pinlen)
      (setf (mem-ref fnew-pinlen :ulong) new-pinlen)
      (check-rv (%ck:set-pin session fold-pin fold-pinlen fnew-pin fnew-pinlen) "C_SetPIN"))))

(defun read-pin (&optional min (max min))
  "Reads a PIN as a string, prompting for at least MIN characters, and at most MAX."
  (format *query-io* "~%Please enter your PIN~@[~* (~:[~D digits~;~D to ~D digits~])~]: "
          min (and min max (/= min max)) min max)
  (finish-output *query-io*)
  (prog1 (read-line *query-io*)
    (terpri *query-io*)))

(defun integer-to-bytes (value &key (endian :little)
                                 (step 8)    ; bits
                                 size        ; bits
                                 min-size    ; bits
                                 max-size)   ; bits
  "Converts an integer into a byte vector.
ENDIAN:     the byte order :little or :big -endian.
SIZE:       the size in bits of the integer.
MIN-SIZE:   the minimum size in bits for the integer.
MAX-SIZE:   the maximum size in bits for the integer.
STEP:       SIZE, MIN-SIZE and MAX-SIZE, when specified,
            should be multiples of STEP.
            The integer size is ceiled by STEP.
SIZE and (MIN-SIZE or MAX-SIZE) are mutually exclusive.
"
  (check-type endian (member :little :big))
  (assert (not (and size (or min-size max-size)))
          (size min-size max-size)
          "size and (min-size or max-size) are mutually exclusive.")
  (flet ((call-do-size (size thunk-i thunk-l)
           (cond
             ((null size))
             ((integerp size) (funcall thunk-i size))
             ((and (consp size) (eql 'member (first size)))
              (dolist (size (rest size))
                (funcall thunk-l size)))
             (t (error "Invalid size: ~S  Should be an integer or (member integer…)." size)))))
    (macrolet ((do-size ((size) &body body)
                 (let ((fbody (gensym)))
                  `(flet ((,fbody (,size) ,@body))
                     (call-do-size ,size (function ,fbody) (function ,fbody))))))
      (macrolet ((check-step (size)
                   `(assert (or (null ,size) (zerop (mod ,size step)))
                            (,size step)
                            "~A = ~D should be a multiple of ~A = ~D"
                            ',size ,size 'step step)))
        (do-size (size) (check-step size))
        (check-step min-size)
        (check-step max-size))

      (let ((value-length (* (ceiling (integer-length value) step) step)))

        (when size
          (block size-ok
            (call-do-size size
                          (lambda (size)
                            (if (<= value-length size)
                                (setf value-length size)
                                (error "Value #x~X has ~D bits (rounded to ~D) which is more than the specified size ~D"
                                       value value-length step size))
                            (return-from size-ok))
                          (lambda (size)
                            (when (= value-length size)
                              (return-from size-ok))))
            (error "Value #x~X has ~D bits (rounded to ~D) which is not one of ~S"
                   value value-length step (rest size))))
        (when (and min-size (< value-length min-size))
          (setf value-length min-size))
        (when (and max-size (< max-size value-length))
          (error "Value #x~X has ~D bits (rounded to ~D) which is less than the specified max-size ~D"
                 value value-length step max-size))

        (loop :with vector := (make-array (ceiling value-length 8) :element-type 'octet)
              :with increment := (if (eql endian :little) +1 -1)
              :repeat (length vector)
              :for i := (if (eql endian :little) 0 (1- (length vector))) :then (+ i increment)
              :for b :from 0 :by 8
              :do (setf (aref vector i) (ldb (byte 8 b) value))
              :finally (return vector))))))

(defun test/integer-to-bytes ()

  (assert (equalp (list (integer-to-bytes #x312312312312312301020304 :endian :little)
                        (integer-to-bytes #x312312312312312301020304 :endian :big))
                  '(#(4 3 2 1 35 49 18 35 49 18 35 49)
                    #(49 35 18 49 35 18 49 35 1 2 3 4))))

  (assert (equalp (list (integer-to-bytes #x312312312312312301020304 :endian :little :step 64)
                        (integer-to-bytes #x312312312312312301020304 :endian :big    :step 64))
                  '(#(4 3 2 1 35 49 18 35 49 18 35 49 0 0 0 0)
                    #(0 0 0 0 49 35 18 49 35 18 49 35 1 2 3 4))))

  (assert (equalp (list (integer-to-bytes #x312312312312312301020304 :endian :little :size 128)
                        (integer-to-bytes #x312312312312312301020304 :endian :big    :step 128))
                  '(#(4 3 2 1 35 49 18 35 49 18 35 49 0 0 0 0)
                    #(0 0 0 0 49 35 18 49 35 18 49 35 1 2 3 4))))

  (assert (equalp (list (integer-to-bytes #x312312312312312301020304 :endian :little :step 32 :min-size 160)
                        (integer-to-bytes #x312312312312312301020304 :endian :big    :step 32 :min-size 160))
                  '(#(4 3 2 1 35 49 18 35 49 18 35 49 0 0 0 0 0 0 0 0)
                    #(0 0 0 0 0 0 0 0 49 35 18 49 35 18 49 35 1 2 3 4))))

  :success)

(test/integer-to-bytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attribute

(defmacro attr-> (struct slot)
  `(foreign-slot-value ,struct '(:struct %ck:attribute) ',slot))

(defun attr-aptr (template index)
  (mem-aptr template '(:struct %ck:attribute) index))

;; For conversion of integers to:
;; :big-integer is most significant byte first (Big endian).
;; :der is  most significant byte first (Big endian).
;; :bytes is less significant byte first (Little endian).
;; :bytes-noint rejects integers.

;; (:big-integer [:size s])
;; (:big-integer [:min-size s1] [:max-size s2] [:step s3])
;; s := integer | (member integer…)


(defparameter *attribute-type-map*
  '((:class                       (:ulong object-class))
    (:token                       :bool)
    (:private                     :bool)
    (:label                       :string) ; rfc2279 (utf-8)
    (:application                 :string) ; rfc2279 (utf-8)
    (:value                       :bytes-noint)  ; or rfc2279 (utf-8)
    (:object-id                   :bytes)  ; DER encoding of the OID indicating the data object type (empty by default)
    (:certificate-type            (:ulong certificate-type))
    (:issuer                      :bytes)
    (:serial-number               :bytes)
    (:ac-issuer                   :string)
    (:owner                       :string)
    (:attr-types                  (:ulong attribute-type))
    (:trusted                     :bool)
    (:certificate-category        :ulong)
    (:java-midp-security-domain   :bytes)
    (:url                         :string)
    (:hash-of-subject-public-key  :bytes)
    (:hash-of-issuer-public-key   :bytes)
    (:check-value                 :bytes)
    (:key-type                    (:ulong key-type))
    (:subject                     :bytes)
    (:id                          :bytes) ; DER-encoding of the object identifier indicating the domain parameters
                                        ; Big endian: #xe828bd080fd2500000104d494f4300010101
                                        ;  <-> #(#xe8 #x28 #xbd #x08 #x0f #xd2 #x50 #x00 #x00
                                        ;        #x10 #x4d #x49 #x4f #x43 #x00 #x01 #x01 #x01)
    (:sensitive                   :bool)
    (:encrypt                     :bool)
    (:decrypt                     :bool)
    (:wrap                        :bool)
    (:unwrap                      :bool)
    (:sign                        :bool)
    (:sign-recover                :bool)
    (:verify                      :bool)
    (:verify-recover              :bool)
    (:derive                      :bool)
    (:start-date                  :date)
    (:end-date                    :date)
    (:modulus                     :big-integer)
    (:modulus-bits                :ulong)
    (:public-exponent             :big-integer)
    (:private-exponent            :big-integer)
    (:prime-1                     :big-integer)
    (:prime-2                     :big-integer)
    (:exponent-1                  :big-integer)
    (:exponent-2                  :big-integer)
    (:coefficient                 :big-integer)
    (:prime                       (:big-integer :min-size 512 :max-size 1024 :step 64))
    (:subprime                    (:big-integer :size (member 160 224 256)))
    (:base                        :big-integer)
    (:prime-bits                  :ulong)
    (:sub-prime-bits              :ulong)
    (:value-bits                  :ulong)
    (:value-len                   :ulong)
    (:extractable                 :bool)
    (:local                       :bool)
    (:never-extractable           :bool)
    (:always-sensitive            :bool)
    (:key-gen-mechanism           (:ulong mechanism-type))
    (:modifiable                  :bool)
    (:ecdsa-params                :bytes)
    (:ec-params                   :bytes)
    (:ec-point                    :bytes)
    (:secondary-auth              :bytes)
    (:auth-pin-flags              :bytes)
    (:always-authenticate         :bool)
    (:wrap-with-trusted           :bool)
    (:hw-feature-type             (:ulong hardware-feature))
    (:reset-on-init               :bool)
    (:has-reset                   :bool)
    (:pixel-x                     :ulong)
    (:pixel-y                     :ulong)
    (:resolution                  :ulong)
    (:char-rows                   :ulong)
    (:char-columns                :ulong)
    (:color                       :bool)
    (:bits-per-pixel              :ulong)
    (:char-sets                   :string) ; rfc2279 (utf-8)
    (:encoding-methods            :string) ; rfc2279 (utf-8)
    (:mime-types                  :string) ; rfc2279 (utf-8)
    (:mechanism-type              (:ulong mechanism-type))
    (:required-cms-attributes     :bytes)
    (:default-cms-attributes      :bytes)
    (:supported-cms-attributes    :bytes)
    (:wrap-template               (:array :attribute))
    (:unwrap-template             (:array :attribute))
    (:allowed-mechanisms          (:array (:ulong mechanism-type)))
    (:vendor-defined              :bytes)))

(defun attribute-type-to-ltype (atype &optional type)
  (or (second (assoc (attribute-type :decode atype) *attribute-type-map*))
      (error "Unknown attribute type: ~S~@[ (type = ~S)~]"
             (attribute-type :decode atype) type)))

(defun attribute-copy (destination source)
  (setf (attr-> destination %ck:type)      (attr-> source %ck:type)
        (attr-> destination %ck:value-len) (attr-> source %ck:value-len)
        (attr-> destination %ck:value)     (attr-> source %ck:value))
  destination)

(defun attribute-allocate-buffer (attribute)
  (let* ((len (attr-> attribute %ck:value-len))
         (val (foreign-alloc :uchar :count len :initial-element 0)))
    (setf (attr-> attribute %ck:value) val)))

(defun attribute-allocate-ulong-array (attribute)
  (let* ((len  (attr-> attribute %ck:value-len))
         (val  (foreign-alloc :ulong :count len :initial-element 0)))
    (setf (attr-> attribute  %ck:value) val)))

(defun attribute-allocate-attribute-array (attribute)
  (let* ((len (attr-> attribute %ck:value-len))
         (val (foreign-alloc :pointer :count len :initial-element (null-pointer))))
    (setf (attr-> attribute %ck:value) val)))

(defun base-ltype-p (ltype)
  (if (atom ltype)
      ltype
      (find (first ltype) '(:big-integer))))

(defun big-integer-attributes (ltype)
  (cond
    ((atom ltype)                     '())
    ((eql :big-integer (first ltype)) (rest ltype))
    (t                                '())))

(defun attribute-fill (attribute type value)
  ;; :ulong               - 4 bytes in big-endian order?
  ;; :bool                - 1 byte 0 or 1
  ;; :string              - any number of bytes. the strings are utf-8 encoded.
  ;;                        if we get a byte vector in, we pass it as is.
  ;; :bytes               - a vector of bytes. If we get a string, we encode it in utf-8, if we get an integer, we encode in little-endian order.
  ;; :bytes-noint         - a vector of bytes. If we get a string, we encode it in utf-8, if we get an integer, we reject it.
  ;; :big-integer         - a vector of bytes. If we get an integer, we encode in big-endian order.
  ;; :date                - a date (string) or universal-time.
  ;; (:array :ulong)      - a vector of ulong.
  ;; (:array :attribute)  - a vector of attributes (passed to the C_ function is a vector of pointers to attribute structures).
  ;; For strings, bytes and arrays, if the value is nil, then we set the pValue to NULL.
  (flet ((set-attribute-fields (attribute atype ctype count &optional value)
           (assert (not (and (null count) value)))
           (let* ((len (* (or count 0) (foreign-type-size ctype)))
                  (val (cond
                         ((null count) (null-pointer))
                         (value        value)
                         (t            (foreign-alloc :uchar :count len)))))
             (setf (attr-> attribute %ck:type)      atype
                   (attr-> attribute %ck:value)     val
                   (attr-> attribute %ck:value-len) len)
             val)))

    (let* ((atype      (if (integerp type)
                           type
                           (attribute-type :encode type)))
           (ltype      (attribute-type-to-ltype atype type))
           (base-ltype (base-ltype-p ltype)))

      (case base-ltype

        ((:ulong)  (set-attribute-fields attribute atype :ulong 1
                                         (foreign-alloc :ulong :initial-element value)))



        ((:bool)   (set-attribute-fields attribute atype :uchar 1
                                         (foreign-alloc :uchar :initial-element (ckbool value))))

        ((:string :bytes :bytes-noint :big-integer)
         (let* ((value (typecase value
                         (null nil)
                         (integer (case base-ltype
                                    (:string       (prin1-to-string value))
                                    (:bytes        (integer-to-bytes value :endian :little))
                                    (:big-integer  (apply (function integer-to-bytes) value :endian :big (big-integer-attributes ltype)))
                                    (:bytes-noint  (error "Please give a vector of bytes, not an integer for ~S" attribute))))
                         ((or string symbol character)  (string value))
                         ((or vector list)
                          (if (every (lambda (element)
                                       (typep element 'octet))
                                     value)
                              value
                              (prin1-to-string value)))
                         (t (prin1-to-string value))))
                (value (if (stringp value)
                           (string-to-utf-8 value)
                           value)))
           (set-attribute-fields attribute atype :uchar
                                 (when value (length value)) ;; TODO: why nil instead of 0?
                                 (when value (foreign-vector-copy-from
                                              (foreign-alloc :uchar :count (length value))
                                              :uchar
                                              (length value) value)))))

        ((:date)
         (let ((value (etypecase value
                        (integer (multiple-value-bind (se mi ho da mo ye) (decode-universal-time value 0)
                                   (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D00"
                                           ye mo da ho mi se)))
                        (string value))))

           (set-attribute-fields attribute atype :uchar  (length value)
                                 (foreign-vector-copy-from
                                  (foreign-alloc :uchar :count (length value))
                                  :uchar
                                  (length value) value))))

        (otherwise

         (unless (consp ltype)
           (error "Bad attribute lisp type: ~S from attribute type: ~S" ltype type))

         (ecase (first ltype)
           ((:ulong)
            (set-attribute-fields attribute atype :ulong 1
                                  (foreign-alloc :ulong
                                                 :initial-element
                                                 (if (integerp value)
                                                     value
                                                     (funcall (second ltype) :encode value)))))
           ((:array)
            (set-attribute-fields attribute atype :uchar
                                  (* (foreign-type-size :ulong)
                                     (length value))
                                  (foreign-vector-copy-from
                                   (foreign-alloc :ulong :count (length value))
                                   :ulong (length value)
                                   value)))
           ((:attribute)
            ;; value is a sequence of attributes
            (let ((template  (template-encode value)))
              (set-attribute-fields attribute atype :uchar
                                    (car template) (cdr template))))

           (otherwise
            (ecase (first (second ltype))
              ((:ulong)
               ;; value is a sequence of integers.
               (set-attribute-fields attribute atype :uchar
                                     (* (foreign-type-size :ulong)
                                        (length value))
                                     (let ((converter (second (second ltype))))
                                       (foreign-vector-copy-from value :ulong (length value)
                                                                 (map 'vector (lambda (value)
                                                                                (if (integerp value)
                                                                                    value
                                                                                    (funcall converter :encode value)))
                                                                      value))))))))))))
  attribute)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template

(deftype template ()
  `(cons integer
         #+ccl ccl:macptr
         #-ccl (progn (warn "What is the type of foreign pointer in ~A" (lisp-implementation-type))
                      t)))

(defun template-find-attribute (template atype)
  "RETURN: a foreign pointer to the attribute of type ATYPE in the TEMPLATE, or the foreign null pointer."
  (check-type template template)
  (loop
    :with atype := (attribute-type :encode atype)
    :with attributes := (cdr template)
    :for i :below (car template)
    :for attribute := (attr-aptr attributes i)
    :unless (null-pointer-p attribute)
      :do (when (= (attr-> attribute %ck:type) atype)
            (return attribute)))
  (null-pointer))

(defun template-free (template)
  "TEMPLATE is a cons (count . foreign-vector).
foreign-vector is a CK_ATTRIBUTE[count] array.
This function frees each foreign pointer in the foreign vector, and frees the foreign vector.
The cdr of the TEMPLATE cons cell is set to NIL."
  (check-type template template)
  (let ((vector (cdr template)))
    (dotimes (i (car template))
      (let ((attribute (attr-aptr vector i)))
        (when (= (logand %ck:+array-attribute+ (attr-> attribute %ck:type))
                 %ck:+array-attribute+)
          (template-free (cons (attr-> attribute %ck:value-len)
                               (attr-> attribute %ck:value))))))
    (foreign-free vector)
    (setf (cdr template) nil)
    (values)))

(defun attribute-dump (attribute)
  "RETURN: ATTRIBUTE"
  (let ((*print-circle* nil))
    (format t "~1@*~16,'0X~%~0@*~A~2@*type=~4X(hex) ~:*~D(dec)~%~0@*~A~4@*size=~4D~%~0@*~A~5@*value=~16,'0X~%"
            *dump-prefix*
            (pointer-address attribute)
            (attr-> attribute %ck:type)
            (attribute-type :decode (attr-> attribute %ck:type))
            (attr-> attribute %ck:value-len)
            (pointer-address (attr-> attribute %ck:value)))
    (if (= %ck:+array-attribute+ (logand %ck:+array-attribute+ (attr-> attribute  %ck:type)))
        (let ((*dump-prefix* (concatenate 'string *dump-prefix* "    ")))
          (template-dump (cons (attr-> attribute %ck:value-len)
                               (attr-> attribute %ck:value))))
        (let ((*dump-prefix* (concatenate 'string *dump-prefix* "  ")))
          (cond ((unavailable-information-p (attr-> attribute %ck:value-len))
                 (format t "~AType indicates unavailable information.~%" *dump-prefix*))
                ((null-pointer-p (attr-> attribute %ck:value))
                 (format t "~AValue is NULL!~%" *dump-prefix*))
                ((unavailable-information-p (pointer-address (attr-> attribute %ck:value)))
                 (format t "~AValue indicates unavailable information.~%" *dump-prefix*))
                (t
                 (dump (attr-> attribute %ck:value)
                       (attr-> attribute %ck:value-len)
                       :print-characters t))))))
  attribute)

(defun validate-pointer (pointer)
  "RETURN: POINTER"
  (let ((address (pointer-address pointer)))
    (assert (/= 0 address)      () "Null pointer!")
    (assert (/= -1 address)     () "Unavailable_information in pointer!")
    (assert (< #x10000 address) () "Small address!"))
  pointer)

(defun template-dump (template)
  "TEMPLATE is a cons (count . foreign-vector).
This function frees each foreign pointer in the foreign vector, and frees the foreign vector.
The cdr of the TEMPLATE cons cell is set to NIL.
RETURN: TEMPLATE"
  (check-type template template)
  (let ((vector (cdr template))
        (*print-circle* nil))
    (format t "~&~ADumping template ~S~%"  *dump-prefix* template)
    (unless (null-pointer-p vector)
      (validate-pointer vector)
      (dump vector (* (car template) (foreign-type-size '(:struct %ck:attribute))))
      (let ((*dump-prefix* (concatenate 'string *dump-prefix* "  ")))
        (dotimes (i (car template))
          (let ((attribute (attr-aptr vector i)))
            (format t "~AAttribute[~D]="  *dump-prefix* i)
            (let ((*dump-prefix* (concatenate 'string *dump-prefix* "    ")))
              (attribute-dump attribute))))))
    (finish-output)
    #-(and)(pause () "dumped"))
  template)

(defun attribute-decode (attribute)
  "RETURN: a cons cell containing the ATTRIBUTE key value pair,
           (:UNAVAILABLE-INFORMATION . NIL) if attribute is invalid, or
           NIL if ATTRIBUTE is the foreign null pointer."
  (cond
    ((null-pointer-p attribute)
     nil)
    ((invalid-pointer-p attribute)
     '(:unavailable-information))
    (t
     (let ((type (attr-> attribute %ck:type)))
       ;; CONS here matches TEMPLATE-ENCODE (atype . value).
       (cons (attribute-type :decode type)
             (let* ((ltype (or (second (assoc (attribute-type :decode type) *attribute-type-map*))
                               (error "Unknown attribute type: ~S" type)))
                    (base-ltype (base-ltype-p ltype))
                    (len (attr-> attribute %ck:value-len))
                    (val (attr-> attribute %ck:value)))
               (if (or (unavailable-information-p type)
                       (invalid-pointer-p val)
                       (unavailable-information-p len))
                   :unavailable-information
                   (case base-ltype
                     ((:ulong)
                      (assert (= (foreign-type-size :ulong) len))
                      (mem-ref val :ulong))
                     ((:bool)
                      (if (zerop len)
                          0 ; libiaspkcs11 returns 0-length values…
                          (progn
                            (assert (= (foreign-type-size :uchar) len))
                            (mem-ref val :uchar))))
                     ((:bytes :bytes-noint :big-integer)
                      (foreign-vector-copy-to val :uchar len (make-array len :element-type 'octet)))
                     ((:string)
                      (foreign-string-to-lisp val :count len :encoding :utf-8))
                     ((:date)
                      (foreign-string-to-lisp val :count len :encoding :utf-8)
                      #-(and) (let ((date (foreign-string-to-lisp var :count len :encoding :utf-8)))
                                (encode-universal-time
                                 (parse-integer date :start 12 :end 14 :junk-allowed nil)
                                 (parse-integer date :start 10 :end 12 :junk-allowed nil)
                                 (parse-integer date :start  8 :end 10 :junk-allowed nil)
                                 (parse-integer date :start  6 :end  8 :junk-allowed nil)
                                 (parse-integer date :start  4 :end  6 :junk-allowed nil)
                                 (parse-integer date :start  0 :end  4 :junk-allowed nil)
                                 0)))
                     (otherwise
                      (unless (consp ltype)
                        (error "Bad attribute lisp type: ~S from attribute type: ~S" ltype type))
                      (ecase (first ltype)
                        ((:ulong)
                         (funcall (second ltype) :decode (mem-ref val :ulong)))
                        ((:array)
                         (if (atom (second ltype))
                             (ecase (second ltype)
                               ((:ulong)
                                ;; value is a sequence of integers.
                                (foreign-vector-copy-from val :ulong len (make-array len)))
                               ((:attribute)
                                ;; value is a sequence of attributes
                                (template-decode (cons len val))))
                             (ecase (first (second ltype))
                               ((:ulong)
                                ;; value is a sequence of integer codes.
                                (let ((converter (second (second ltype))))
                                  (map 'list (lambda (value)
                                               (funcall converter :encode value))
                                       (foreign-vector-copy-from val :ulong len (make-array len))))))))))))))))))

(defun template-decode (template)
  "TEMPLATE:  a cons containing the number of attributes and a foreign pointer to the attributes.
RETURN: An a-list of (key . value) decoded attributes."
  (check-type template template)
  (handler-bind ((error (function invoke-debugger)))
    (let ((vector (cdr template))
          (result '()))
      (dotimes (i (car template) (nreverse result))
        (let ((attribute (attr-aptr vector i)))
          ;; Could test for value-len=-1 and skip those,
          ;; but attribute-decode returns :unavailable-information for them.
          (push (attribute-decode attribute) result))))))

(defun template-allocate-buffers (template)
    "Allocates buffers for all the attributes in the template.
TEMPLATE:  a cons containing the number of attributes and a foreign pointer to the attributes.
RETURN: TEMPLATE"
  (check-type template template)
  ;; Allocate for the value attribute a buffer of size the value of the attribute value-len.
  (let ((vector (cdr template)))
    (dotimes (i (car template))
      (let ((attribute (attr-aptr vector i)))
        (when (null-pointer-p (attr-> attribute %ck:value))
          (let ((type (attr-> attribute %ck:type)))
            (cond
              ((or (= type %ck:+a-wrap-template+) (= type %ck:+a-unwrap-template+))
               (attribute-allocate-attribute-array attribute))
              ((= type %ck:+a-allowed-mechanisms+)
               (attribute-allocate-ulong-array attribute))
              (t
               (attribute-allocate-buffer attribute))))))))
  template)

(defun template-encode (template)
  "Takes a list of (<attribute-type> . <attribute-value>) and
allocates and fills a foreign vector of CK_ATTRIBUTE_PTR and
returns a cons (count . foreign-vector).
For strings, bytes and arrays, if the value is nil, then we set the pValue to NULL."
  (check-type template list)
  (let* ((count   (length template))
         (result  (foreign-alloc '(:struct %ck:attribute) :count count))
         (aborted nil))
    (macrolet ()
      (unwind-protect
           (handler-bind ((error (lambda (condition)
                                   (setf aborted t)
                                   (error condition))))
             (loop
               ;;  (atype . value) here matches attribute-decode CONS.
               :for (atype . value) :in template
               :for i :from 0
               :for attribute := (attr-aptr result i)
               :do (attribute-fill attribute atype value)
               :finally (let ((template (cons count result)))
                          (return template))))
        (when aborted
          (template-free (cons count result)))))))

(defun template-pack (template)
  "Modifes the template, removing the attributes that have unavailable information values.
RETURN: TEMPLATE
"
  (check-type template template)
  (loop
    :with vector := (cdr template)
    :with length := (car template)
    :with j := 0
    :for i :below length
    :for attribute := (attr-aptr vector i)
    :do (let ((type (attr-> attribute %ck:type))
              (len  (attr-> attribute %ck:value-len)))
          (unless (or (unavailable-information-p type)
                      (unavailable-information-p len))
            (when (< j i)
              (attribute-copy (attr-aptr vector j) attribute))
            (incf j)))
    :finally (setf (car template) j)
             (return template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object

(defun create-object (session template)
  (check-type session    session-handle)
  (check-type template   list)
  (let ((template (template-encode template)))
    (unwind-protect
         (with-foreign-object (object '%ck:object-handle)
           (check-rv (%ck:create-object session (cdr template) (car template) object) "C_CreateObject")
           (mem-ref object '%ck:object-handle))
      (template-free template))))

(defun copy-object (session old-object template)
  (check-type session    session-handle)
  (check-type old-object object-handle)
  (check-type template   list)
  (let ((template (template-encode template)))
    (unwind-protect
         (with-foreign-object (object '%ck:object-handle)
           (check-rv (%ck:copy-object session old-object (cdr template) (car template) object) "C_CopyObject")
           (mem-ref object '%ck:object-handle))
      (template-free template))))

(defun destroy-object (session old-object)
  (check-type session    session-handle)
  (check-type old-object object-handle)
  (check-rv (%ck:destroy-object session old-object) "C_DestroyObject"))

(defun get-object-size (session object)
  (check-type session    session-handle)
  (check-type object     object-handle)
  (with-foreign-object (size :ulong)
    (check-rv (%ck:get-object-size session object size) "C_GetObjectSize")
    (mem-ref size :ulong)))

;; HERE
(defun get-attribute-value (session object template)
  (check-type session    session-handle)
  (check-type object     object-handle)
  (check-type template   list)
  (let ((template (template-encode template)))
    (unwind-protect
         (let ((status
                 (handler-case
                     (progn
                       #+debug (ignore-errors (write-line "Before 1st C_GetAttributeValue") (template-dump template))
                       (check-rv (%ck:get-attribute-value session object (cdr template) (car template)) "C_GetAttributeValue")
                       #+debug (ignore-errors (write-line "After 1st C_GetAttributeValue") (template-dump template))
                       (values))
                   (:no-error ()  #-(and)(pause () "Ok") :ok)
                   (pkcs11-error (err)
                     (case (pkcs11-error-label err)
                       ((:attribute-sensitive :attribute-type-invalid :buffer-too-small)
                        #-(and)(pause (list (list '*template* template)
                                            (list '*error*    err)) "pkcs11-error ~A" err)
                        (setf template (template-allocate-buffers (template-pack template)))
                        ;; try again:
                        (handler-case
                            (progn
                              #+debug (ignore-errors (write-line "Before 2nd C_GetAttributeValue") (template-dump template))
                              (check-rv (%ck:get-attribute-value session object (cdr template) (car template)) "C_GetAttributeValue")
                              #+debug (ignore-errors (write-line "After 2nd C_GetAttributeValue") (template-dump template))
                              (values))
                          (:no-error ()  #-(and)(pause () "Ok") :ok)
                          (pkcs11-error (err)
                            (case (pkcs11-error-label err)
                              ((:attribute-sensitive :attribute-type-invalid  :buffer-too-small)
                               #-(and)(pause (list (list '*template* template)
                                                   (list '*error* err)) "pkcs11-error ~A" err)
                               (pkcs11-error-label err))
                              (otherwise (error err))))))
                       (otherwise (error err)))))))
           #-(and) (pause (list (list '*template* template)
                                (list '*template* template)) "cleanup")
           #-(and) (template-dump template)
           (values (template-decode template) status))
      #-(and)(pause (list (list '*template* template)) "cleanup")
      (template-free template))))

(defun set-attribute-value (session object template)
  (check-type session    session-handle)
  (check-type object     object-handle)
  (check-type template   list)
  (let ((template (template-encode template)))
    (unwind-protect
         (check-rv (%ck:set-attribute-value session object (cdr template) (car template))
                   "C_SetAttributeValue")
      (template-free template))))

(defun find-objects-init (session template)
  (check-type session    session-handle)
  (check-type template   list)
  (let ((template (template-encode template)))
    (unwind-protect
         (check-rv (%ck:find-objects-init session (cdr template) (car template)) "C_FindObjectsInit")
      (template-free template))))

(defun find-objects (session)
  (check-type session    session-handle)
  (let ((buffer-size 128)
        (result '()))
    (with-foreign-objects ((objects '%ck:object-handle buffer-size)
                           (count :ulong))
      (check-rv (%ck:find-objects session objects buffer-size count) "C_FindObjects")
      (dotimes (i (mem-ref count :ulong))
        (push (mem-aref objects :ulong i) result)))
    (nreverse result)))

(defun find-objects-final (session)
  (check-type session    session-handle)
  (check-rv (%ck:find-objects-final session) "C_FindObjectsFinal"))

(defun find-all-objects (session template)
  (check-type session    session-handle)
  (check-type template   list)
  (find-objects-init session template)
  (unwind-protect
       (loop
         :for objects := (find-objects session)
         :while objects
         :append objects)
    (find-objects-final session)))

(defun template-from-attribute-type-map (attribute-type-map)
  (mapcar (lambda (entry)
            ;; This CONS here matches attribute-decode CONS.
            (cons (first entry)
                  (let ((type (second entry)))
                    (if (atom type)
                        (ecase type
                          ((:ulong)       0)
                          ((:bool)        nil)
                          ((:string)      nil)
                          ((:bytes :bytes-noint :big-integer)  nil)
                          ((:date)        "0000000000000000"))
                        (ecase (first type)
                          ((:ulong)       0)
                          ((:big-integer) nil)
                          ((:array)       nil))))))
          attribute-type-map))

(defun object-get-all-attributes (session object)
  (check-type session    session-handle)
  (check-type object     object-handle)
  (get-attribute-value session object
                       (template-from-attribute-type-map *attribute-type-map*)))

(defun object-get-attributes (session object attributes)
  (check-type session    session-handle)
  (check-type object     object-handle)
    (get-attribute-value session object
                         (template-from-attribute-type-map
                          (remove-if-not (lambda (entry)
                                           (member (first entry) attributes))
                                         *attribute-type-map*))))

;;; Encryption

;; Single block:
;;
;; (progn (encrypt-init session mechanims key)
;;        (encrypt bytes [encrypted-length])) -> encrypted-vector

;; Multiple blocks:
;;
;; (loop
;;   :with encrypted-blocks := '()
;;   :for block :in blocks
;;     :initially (encrypt-init session mechanims key)
;;   :do        (push (encrypt-update session block) encrypted-blocks)
;;   :finally   (push (encrypt-final  session)       encrypted-blocks)
;;              (return (apply (function concatenate) 'vector (nreverse encrypted-blocks))))n


;;; Decryption

;; Single block:
;;
;; (progn (decrypt-init session mechanims key)
;;        (decrypt bytes [decrypted-length])) -> decrypted-vector

;; Multiple blocks:
;;
;; (loop
;;   :with decrypted-blocks := '()
;;   :for block :in blocks
;;     :initially (decrypt-init session mechanims key)
;;   :do        (push (decrypt-update session block) decrypted-blocks)
;;   :finally   (push (decrypt-final  session)       decrypted-blocks)
;;              (return (apply (function concatenate) 'vector (nreverse decrypted-blocks))))n

(deftype mechanism () `(or integer keyword list))

(defun set-mechanism (fmechanism mechanism)
  ;; mechanism is either a mechanism-type integer or keyword, or a list (mechanism-type parameter parameter-length)
  (check-type mechanism mechanism)
  (setf (foreign-slot-value fmechanism '(:struct %ck:mechanism) '%ck:mechanism)     (mechanism-type :encode (if (listp mechanism) (first mechanism) mechanism))
        (foreign-slot-value fmechanism '(:struct %ck:mechanism) '%ck:parameter)     (or (when (listp mechanism) (second mechanism))  (null-pointer))
        (foreign-slot-value fmechanism '(:struct %ck:mechanism) '%ck:parameter-len) (or (when (listp mechanism) (third  mechanism))  0)))

(defmacro define-pkcs11-initializing-function (name low-name c-name &key (keyp t))
  `(defun ,name (session mechanism ,@(when keyp `(key)))
     (check-type session   session-handle)
     (check-type mechanism mechanism)
     ,@(when keyp `((check-type key object-handle)))
     (with-foreign-object (fmechanism '(:struct %ck:mechanism))
       (set-mechanism fmechanism mechanism)
       (check-rv (,low-name session fmechanism ,@(when keyp `(key))) ,c-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun scat (&rest args)
    (intern (reduce (lambda (a b) (concatenate 'string a b)) args :key (function string)))))

(defmacro define-pkcs11-processing-function (name low-name c-name &key (input '()) (outputp t))
  "Defines a function to process buffers.

NAME:       the symbol naming the defined function.
LOW-NAME:   the symbol naming the underlying FFI function.
C-NAME:     the string naming the C function (for error reporting).
INPUT:      a list of base parameters; additionnal &key parameters will be provided.
OUTPUTP:    whether the function returns some output buffer.

NOTE        When OUTPUTP, an additionnal &key OUTPUT parameter allow
            to pass a pre-allocated buffer.  For all the INPUT and
            OUTPUT parameters <p>, the additionnal &key <p>-START and
            <p>-END parameters are provided to specify the range of
            the buffers to use.  Those buffers are byte vectors.

RETURN:     the OUTPUT byte vector; the position beyond the last byte written.
"
  (let ((all-params  (append input (when outputp (list 'output)))))
    `(defun ,name (session ,@input &key
                                     ,@(when outputp `(output))
                                     ,@(mapcan (lambda (parameter)
                                                 (list `(,(scat parameter '-start) 0)
                                                       `(,(scat parameter '-end) (length ,parameter))))
                                               all-params))
       (check-type session    session-handle)
       ,@(mapcar (lambda (parameter) `(check-type ,parameter vector)) input)
       ,@(when outputp `((check-type output (or null vector))))
       ,@(mapcan (lambda (parameter)
                   (list `(check-type ,(scat parameter '-start) (integer 0))
                         `(check-type ,(scat parameter '-end)   (integer 0))))
                 all-params)
       (let (,@(mapcar (lambda (parameter)
                         `(,(scat parameter '-size)  (- ,(scat parameter '-end) ,(scat parameter '-start))))
                       all-params))
         (with-foreign-objects (,@(mapcan (lambda (parameter)
                                            (list `(,(scat 'f parameter) :uchar ,(scat parameter '-size))
                                                  `(,(scat 'f parameter '-length) :ulong)))
                                          all-params))
           ,@(mapcan (lambda (parameter)
                       (list `(foreign-vector-copy-from ,(scat 'f parameter)
                                                        :uchar
                                                        ,(scat parameter '-size)
                                                        ,parameter
                                                        :startl ,(scat parameter '-start)
                                                        :endl ,(scat parameter '-end))))
                     input)
           ,@(mapcan (lambda (parameter)
                       (list `(setf (mem-ref ,(scat 'f parameter '-length) :ulong)
                                    ,(scat parameter '-size))))
                     all-params)
           (check-rv (,low-name session
                                ,@(mapcan (lambda (parameter) (list (scat 'f parameter) (scat parameter '-size)))
                                          input)
                                ,@(mapcan (lambda (parameter) (list (scat 'f parameter) (scat 'f parameter '-length)))
                                          (when outputp '(output))))
                     ,c-name)
           ,@(when outputp
               `((let ((output (or output (make-array output-end :element-type 'octet)))
                       (folen  (mem-ref foutput-length :ulong)))
                   (foreign-vector-copy-to foutput :uchar folen
                                           output :startl output-start :endl output-end)
                   (values output (+ output-start folen))))))))))

(define-pkcs11-initializing-function encrypt-init          %ck:encrypt-init          "C_EncryptInit")
(define-pkcs11-processing-function   encrypt               %ck:encrypt               "C_Encrypt"             :input (clear)          :outputp t)
(define-pkcs11-processing-function   encrypt-update        %ck:encrypt-update        "C_EncryptUpdate"       :input (clear)          :outputp t)
(define-pkcs11-processing-function   encrypt-final         %ck:encrypt-final         "C_EncryptFinal"        :input ()               :outputp t)

(define-pkcs11-initializing-function decrypt-init          %ck:decrypt-init          "C_DecryptInit")
(define-pkcs11-processing-function   decrypt               %ck:decrypt               "C_Decrypt"             :input (crypted)        :outputp t)
(define-pkcs11-processing-function   decrypt-update        %ck:decrypt-update        "C_DecryptUpdate"       :input (crypted)        :outputp t)
(define-pkcs11-processing-function   decrypt-final         %ck:decrypt-final         "C_DecryptFinal"        :input ()               :outputp t)

(define-pkcs11-initializing-function sign-init             %ck:sign-init             "C_SignInit")
(define-pkcs11-processing-function   sign                  %ck:sign                  "C_Sign"                :input (text)           :outputp t)
(define-pkcs11-processing-function   sign-update           %ck:sign-update           "C_SignUpdate"          :input (text)           :outputp nil)
(define-pkcs11-processing-function   sign-final            %ck:sign-final            "C_SignFinal"           :input ()               :outputp t)
(define-pkcs11-initializing-function sign-recover-init     %ck:sign-recover-init     "C_SignRecoverInit")
(define-pkcs11-processing-function   sign-recover          %ck:sign-recover          "C_SignRecover"         :input (text)           :outputp t)


(define-pkcs11-initializing-function verify-init           %ck:verify-init           "C_VerifyInit")
(define-pkcs11-processing-function   verify                %ck:verify                "C_Verify"              :input (text signature) :outputp nil)
(define-pkcs11-processing-function   verify-update         %ck:verify-update         "C_VerifyUpdate"        :input (text)           :outputp nil)
(define-pkcs11-processing-function   verify-final          %ck:verify-final          "C_VerifyFinal"         :input (signature)      :outputp nil)
(define-pkcs11-initializing-function verify-recover-init   %ck:verify-recover-init   "C_VerifyRecoverInit")
(define-pkcs11-processing-function   verify-recover        %ck:verify-recover        "C_VerifyRecover"       :input (signature)      :outputp t)

(define-pkcs11-initializing-function digest-init           %ck:digest-init           "C_DigestInit"          :keyp  nil)
(define-pkcs11-processing-function   digest                %ck:digest                "C_Digest"              :input (data)           :outputp t)
(define-pkcs11-processing-function   digest-update         %ck:digest-update         "C_DigestUpdate"        :input (data)           :outputp nil)
(define-pkcs11-processing-function   digest-final          %ck:digest-final          "C_DigestFinal"         :input ()               :outputp t)

(define-pkcs11-processing-function   digest-encrypt-update %ck:digest-encrypt-update "C_DigestEncryptUpdate" :input (clear)          :outputp t)
(define-pkcs11-processing-function   decrypt-digest-update %ck:decrypt-digest-update "C_DecryptDigestUpdate" :input (crypted)        :outputp t)
(define-pkcs11-processing-function   sign-encrypt-update   %ck:sign-encrypt-update   "C_SignEncryptUpdate"   :input (clear)          :outputp t)
(define-pkcs11-processing-function   decrypt-verify-update %ck:decrypt-verify-update "C_DecryptVerifyUpdate" :input (crypted)        :outputp t)

(defun digest-key (session key)
  (check-type session session-handle)
  (check-type key     object-handle)
  (check-rv (%ck:digest-key session key) "C_DigestKey"))

(defun generate-key (session mechanism template)
  "Generates a new KEY following the TEMPLATE for the given encrytion MECHANISM.
RETURN: the new key object-handle."
  (check-type session    session-handle)
  (check-type mechanism  mechanism)
  (check-type template   list)
  (let ((template (template-encode template)))
    (unwind-protect
         (with-foreign-objects ((fmechanism '(:struct %ck:mechanism))
                                (fkey       '%ck:object-handle))
           (set-mechanism fmechanism mechanism)
           (check-rv (%ck:generate-key session fmechanism (cdr template) (car template) fkey) "C_GenerateKey")
           (mem-ref fkey '%ck:object-handle))
      (template-free template))))

(defun generate-key-pair (session mechanism public-key-template private-key-template)
    "Generates a new key pair (public and private keys) following the PUBLIC-KEY-TEMPLATE and PRIVATE-KEY-TEMPLATE for the given encrytion MECHANISM.
RETURN: a list containing the public and private key object-handles."
  (check-type session               session-handle)
  (check-type mechanism             mechanism)
  (check-type public-key-template   list)
  (check-type private-key-template  list)
  (let ((public-key-template (template-encode public-key-template)))
    (unwind-protect
         (let ((private-key-template (template-encode private-key-template)))
           (unwind-protect
                (with-foreign-objects ((fmechanism    '(:struct %ck:mechanism))
                                       (fpublic-key   '%ck:object-handle)
                                       (fprivate-key  '%ck:object-handle))
                  (set-mechanism fmechanism mechanism)
                  (check-rv (%ck:generate-key-pair session fmechanism
                                                   (cdr public-key-template)  (car public-key-template)
                                                   (cdr private-key-template) (car private-key-template)
                                                   fpublic-key
                                                   fprivate-key)
                            "C_GenerateKeyPair")
                  (list (mem-ref fpublic-key  '%ck:object-handle)
                        (mem-ref fprivate-key '%ck:object-handle)))
             (template-free private-key-template)))
      (template-free public-key-template))))


#-(and)
(progn

(defun wrap-key (session mechanism wrapping-key key)
  (check-type session      session-handle)
  (check-type mechanism    mechanism)
  (check-type wrapping-key object-handle)
  (check-type key          object-handle)
  (with-foreign-objects ((fmechanism    '(:struct %ck:mechanism))
                         )
    (set-mechanism fmechanism mechanism)
    (check-rv (%ck:wrap-key session fmechanism
                            wrapping-key key

                            )
              "C_WrapKey"))

  (defcfun (wrap-key)    rv
   (session         session-handle)
   (mechanism       (:pointer (:struct mechanism)))
   (wrapping-key    object-handle)
   (key             object-handle)
   (wrapped-key     (:pointer :uchar))
   (wrapped-key-len (:pointer :ulong))))


  (defcfun (unwrap-key "C_UnwrapKey")    rv
    (session         session-handle)
    (mechanism       (:pointer (:struct mechanism)))
    (unwrapping-key  object-handle)
    (wrapped-key     (:pointer :uchar))
    (wrapped-key-len :ulong)
    (templ           (:pointer (:struct attribute)))
    (attribute-count :ulong)
    (key             (:pointer object-handle)))

  (defcfun (derive-key "C_DeriveKey")    rv
    (session         session-handle)
    (mechanism       (:pointer (:struct mechanism)))
    (base-key        object-handle)
    (templ           (:pointer (:struct attribute)))
    (attribute-count :ulong)
    (key             (:pointer object-handle))))


#||

(with-pkcs11
  (let ((slotid 0))
    (with-open-session (session slotid)
      (call-logged-in session slotid (lambda () (encrypt-init session :rsa-pkcs 140415298986736))))))
;; function not supported

(with-pkcs11
  (let ((slotid 0))
    (with-open-session (session slotid)
      (call-logged-in session slotid (lambda () (encrypt-init session :rsa-pkcs 140415299275584))))))
;; function not supported

(with-pkcs11
  (let ((slotid 0))
    (with-open-session (session slotid)
      (call-logged-in session slotid (lambda () (encrypt-init session :rsa-pkcs 140415299275584))))))


(with-pkcs11
  (let ((slotid 1))
    (with-open-session (session slotid)
      (call-logged-in session slotid (lambda () (encrypt-init session :rsa-pkcs 140588238480352))))))

;; function not implemented (with those keys?)






(with-pkcs11
  (let ((slotid 1))
   (with-open-session (session slotid)
     (call-logged-in session slotid (lambda () (get-mechanism-list 0))))))
(with-pkcs11
  (get-mechanism-list 1))
(:sha-1 :sha256 :sha384 :sha512 :md5 :ripemd160 4624 :rsa-pkcs :sha1-rsa-pkcs :sha256-rsa-pkcs :rsa-pkcs-key-pair-gen)

||#


(defun seed-random (session seed)
  (with-foreign-object (fseed :uchar (length seed))
    (dotimes (i (length seed)) (setf (mem-aref fseed :uchar i) (aref seed i)))
    (check-rv (%ck:seed-random session fseed (length seed)) "C_SeedRandom")))

(defun generate-random (session length)
  (with-foreign-object (frandom :uchar length)
    (check-rv (%ck:generate-random session frandom length) "C_GenerateRandom")
    (foreign-vector frandom :uchar 'octet length)))


(defun decode-oid (bytes)
  ;; It looks like the oid stored in iac/ecc are missing the first two numbers…
  (list* 2 5 ; iso.ITU-T / X.500
   (flexi-streams:with-input-from-sequence (stream bytes)
     (asinine:decode-oid stream))))

(defun encode-oid (oid)
    (flexi-streams:with-output-to-sequence (stream)
    (asinine:encode-oid stream (if (and (= 2 (first oid))
                                        (= 5 (second oid)))
                                   (cddr oid)
                                   oid))))


;; (decode-oid #(232 40 189 8 15 210 80 0 0 16 77 73 79 67 0 1 1 1))
;; --> (2 5 4 29 8 15 10576 0 0 16 77 73 79 67 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

;;;; THE END ;;;;

