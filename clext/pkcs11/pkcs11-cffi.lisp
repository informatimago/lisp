;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pkcs11-cffi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    XXX
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

(defpackage "COM.INFORMATIMAGO.CLEXT.PKCS11.LOW"
  (:use "COMMON-LISP" "CFFI")
  (:nicknames "%CK")
  (:export "LOAD-LIBRARY")
  (:export "INITIALIZE" "FINALIZE" "GET-INFO" "GET-SLOT-LIST"
           "GET-SLOT-INFO" "GET-TOKEN-INFO" "WAIT-FOR-SLOT-EVENT"
           "GET-MECHANISM-LIST" "GET-MECHANISM-INFO" "INIT-TOKEN" "INIT-PIN"
           "SET-PIN" "OPEN-SESSION" "CLOSE-SESSION" "CLOSE-ALL-SESSIONS"
           "GET-SESSION-INFO" "GET-OPERATION-STATE" "SET-OPERATION-STATE"
           "LOGIN" "LOGOUT" "CREATE-OBJECT" "COPY-OBJECT" "DESTROY-OBJECT"
           "GET-OBJECT-SIZE" "GET-ATTRIBUTE-VALUE" "SET-ATTRIBUTE-VALUE"
           "FIND-OBJECTS-INIT" "FIND-OBJECTS" "FIND-OBJECTS-FINAL"
           "ENCRYPT-INIT" "ENCRYPT" "ENCRYPT-UPDATE" "ENCRYPT-FINAL"
           "DECRYPT-INIT" "DECRYPT" "DECRYPT-UPDATE" "DECRYPT-FINAL"
           "DIGEST-INIT" "DIGEST" "DIGEST-UPDATE" "DIGEST-KEY" "DIGEST-FINAL"
           "SIGN-INIT" "SIGN" "SIGN-UPDATE" "SIGN-FINAL" "SIGN-RECOVER-INIT"
           "SIGN-RECOVER" "VERIFY-INIT" "VERIFY" "VERIFY-UPDATE" "VERIFY-FINAL"
           "VERIFY-RECOVER-INIT" "VERIFY-RECOVER" "DIGEST-ENCRYPT-UPDATE"
           "DECRYPT-DIGEST-UPDATE" "SIGN-ENCRYPT-UPDATE"
           "DECRYPT-VERIFY-UPDATE" "GENERATE-KEY" "GENERATE-KEY-PAIR"
           "WRAP-KEY" "UNWRAP-KEY" "DERIVE-KEY" "SEED-RANDOM" "GENERATE-RANDOM"
           "GET-FUNCTION-STATUS" "CANCEL-FUNCTION")

  (:export "+TRUE+" "+FALSE+"
           "+SURRENDER+" "+TOKEN-PRESENT+" "+REMOVABLE-DEVICE+"
           "+HW-SLOT+" "+ARRAY-ATTRIBUTE+" "+RNG+" "+WRITE-PROTECTED+"
           "+LOGIN-REQUIRED+" "+USER-PIN-INITIALIZED+" "+RESTORE-KEY-NOT-NEEDED+"
           "+CLOCK-ON-TOKEN+" "+PROTECTED-AUTHENTICATION-PATH+"
           "+DUAL-CRYPTO-OPERATIONS+" "+TOKEN-INITIALIZED+"
           "+SECONDARY-AUTHENTICATION+" "+USER-PIN-COUNT-LOW+"
           "+USER-PIN-FINAL-TRY+" "+USER-PIN-LOCKED+" "+USER-PIN-TO-BE-CHANGED+"
           "+SO-PIN-COUNT-LOW+" "+SO-PIN-FINAL-TRY+" "+SO-PIN-LOCKED+"
           "+SO-PIN-TO-BE-CHANGED+" "+UNAVAILABLE-INFORMATION+"
           "+EFFECTIVELY-INFINITE+" "+INVALID-HANDLE+" "+SO+" "+USER+"
           "+CONTEXT-SPECIFIC+" "+RO-PUBLIC-SESSION+" "+RO-USER-FUNCTIONS+"
           "+RW-PUBLIC-SESSION+" "+RW-USER-FUNCTIONS+" "+RW-SO-FUNCTIONS+"
           "+RW-SESSION+" "+SERIAL-SESSION+" "+O-DATA+" "+O-CERTIFICATE+"
           "+O-PUBLIC-KEY+" "+O-PRIVATE-KEY+" "+O-SECRET-KEY+" "+O-HW-FEATURE+"
           "+O-DOMAIN-PARAMETERS+" "+O-MECHANISM+" "+VENDOR-DEFINED+"
           "+H-MONOTONIC-COUNTER+" "+H-CLOCK+" "+H-USER-INTERFACE+" "+K-RSA+"
           "+K-DSA+" "+K-DH+" "+K-ECDSA+" "+K-EC+" "+K-X9-42-DH+" "+K-KEA+"
           "+K-GENERIC-SECRET+" "+K-RC2+" "+K-RC4+" "+K-DES+" "+K-DES2+"
           "+K-DES3+" "+K-CAST+" "+K-CAST3+" "+K-CAST128+" "+K-RC5+" "+K-IDEA+"
           "+K-SKIPJACK+" "+K-BATON+" "+K-JUNIPER+" "+K-CDMF+" "+K-AES+"
           "+K-BLOWFISH+" "+K-TWOFISH+" "+C-X-509+" "+C-X-509-ATTR-CERT+"
           "+C-WTLS+" "+A-CLASS+" "+A-TOKEN+" "+A-PRIVATE+" "+A-LABEL+"
           "+A-APPLICATION+" "+A-VALUE+" "+A-OBJECT-ID+" "+A-CERTIFICATE-TYPE+"
           "+A-ISSUER+" "+A-SERIAL-NUMBER+" "+A-AC-ISSUER+" "+A-OWNER+"
           "+A-ATTR-TYPES+" "+A-TRUSTED+" "+A-CERTIFICATE-CATEGORY+"
           "+A-JAVA-MIDP-SECURITY-DOMAIN+" "+A-URL+"
           "+A-HASH-OF-SUBJECT-PUBLIC-KEY+" "+A-HASH-OF-ISSUER-PUBLIC-KEY+"
           "+A-CHECK-VALUE+" "+A-KEY-TYPE+" "+A-SUBJECT+" "+A-ID+"
           "+A-SENSITIVE+" "+A-ENCRYPT+" "+A-DECRYPT+" "+A-WRAP+" "+A-UNWRAP+"
           "+A-SIGN+" "+A-SIGN-RECOVER+" "+A-VERIFY+" "+A-VERIFY-RECOVER+"
           "+A-DERIVE+" "+A-START-DATE+" "+A-END-DATE+" "+A-MODULUS+"
           "+A-MODULUS-BITS+" "+A-PUBLIC-EXPONENT+" "+A-PRIVATE-EXPONENT+"
           "+A-PRIME-1+" "+A-PRIME-2+" "+A-EXPONENT-1+" "+A-EXPONENT-2+"
           "+A-COEFFICIENT+" "+A-PRIME+" "+A-SUBPRIME+" "+A-BASE+"
           "+A-PRIME-BITS+" "+A-SUB-PRIME-BITS+" "+A-VALUE-BITS+" "+A-VALUE-LEN+"
           "+A-EXTRACTABLE+" "+A-LOCAL+" "+A-NEVER-EXTRACTABLE+"
           "+A-ALWAYS-SENSITIVE+" "+A-KEY-GEN-MECHANISM+" "+A-MODIFIABLE+"
           "+A-ECDSA-PARAMS+" "+A-EC-PARAMS+" "+A-EC-POINT+" "+A-SECONDARY-AUTH+"
           "+A-AUTH-PIN-FLAGS+" "+A-ALWAYS-AUTHENTICATE+" "+A-WRAP-WITH-TRUSTED+"
           "+A-HW-FEATURE-TYPE+" "+A-RESET-ON-INIT+" "+A-HAS-RESET+"
           "+A-PIXEL-X+" "+A-PIXEL-Y+" "+A-RESOLUTION+" "+A-CHAR-ROWS+"
           "+A-CHAR-COLUMNS+" "+A-COLOR+" "+A-BITS-PER-PIXEL+" "+A-CHAR-SETS+"
           "+A-ENCODING-METHODS+" "+A-MIME-TYPES+" "+A-MECHANISM-TYPE+"
           "+A-REQUIRED-CMS-ATTRIBUTES+" "+A-DEFAULT-CMS-ATTRIBUTES+"
           "+A-SUPPORTED-CMS-ATTRIBUTES+" "+A-WRAP-TEMPLATE+"
           "+A-UNWRAP-TEMPLATE+" "+A-ALLOWED-MECHANISMS+"
           "+M-RSA-PKCS-KEY-PAIR-GEN+" "+M-RSA-PKCS+" "+M-RSA-9796+"
           "+M-RSA-X-509+" "+M-MD2-RSA-PKCS+" "+M-MD5-RSA-PKCS+"
           "+M-SHA1-RSA-PKCS+" "+M-RIPEMD128-RSA-PKCS+" "+M-RIPEMD160-RSA-PKCS+"
           "+M-RSA-PKCS-OAEP+" "+M-RSA-X9-31-KEY-PAIR-GEN+" "+M-RSA-X9-31+"
           "+M-SHA1-RSA-X9-31+" "+M-RSA-PKCS-PSS+" "+M-SHA1-RSA-PKCS-PSS+"
           "+M-DSA-KEY-PAIR-GEN+" "+M-DSA+" "+M-DSA-SHA1+"
           "+M-DH-PKCS-KEY-PAIR-GEN+" "+M-DH-PKCS-DERIVE+"
           "+M-X9-42-DH-KEY-PAIR-GEN+" "+M-X9-42-DH-DERIVE+"
           "+M-X9-42-DH-HYBRID-DERIVE+" "+M-X9-42-MQV-DERIVE+"
           "+M-SHA256-RSA-PKCS+" "+M-SHA384-RSA-PKCS+" "+M-SHA512-RSA-PKCS+"
           "+M-SHA256-RSA-PKCS-PSS+" "+M-SHA384-RSA-PKCS-PSS+"
           "+M-SHA512-RSA-PKCS-PSS+" "+M-RC2-KEY-GEN+" "+M-RC2-ECB+"
           "+M-RC2-CBC+" "+M-RC2-MAC+" "+M-RC2-MAC-GENERAL+" "+M-RC2-CBC-PAD+"
           "+M-RC4-KEY-GEN+" "+M-RC4+" "+M-DES-KEY-GEN+" "+M-DES-ECB+"
           "+M-DES-CBC+" "+M-DES-MAC+" "+M-DES-MAC-GENERAL+" "+M-DES-CBC-PAD+"
           "+M-DES2-KEY-GEN+" "+M-DES3-KEY-GEN+" "+M-DES3-ECB+" "+M-DES3-CBC+"
           "+M-DES3-MAC+" "+M-DES3-MAC-GENERAL+" "+M-DES3-CBC-PAD+"
           "+M-CDMF-KEY-GEN+" "+M-CDMF-ECB+" "+M-CDMF-CBC+" "+M-CDMF-MAC+"
           "+M-CDMF-MAC-GENERAL+" "+M-CDMF-CBC-PAD+" "+M-MD2+" "+M-MD2-HMAC+"
           "+M-MD2-HMAC-GENERAL+" "+M-MD5+" "+M-MD5-HMAC+" "+M-MD5-HMAC-GENERAL+"
           "+M-SHA-1+" "+M-SHA-1-HMAC+" "+M-SHA-1-HMAC-GENERAL+" "+M-RIPEMD128+"
           "+M-RIPEMD128-HMAC+" "+M-RIPEMD128-HMAC-GENERAL+" "+M-RIPEMD160+"
           "+M-RIPEMD160-HMAC+" "+M-RIPEMD160-HMAC-GENERAL+" "+M-SHA256+"
           "+M-SHA256-HMAC+" "+M-SHA256-HMAC-GENERAL+" "+M-SHA384+"
           "+M-SHA384-HMAC+" "+M-SHA384-HMAC-GENERAL+" "+M-SHA512+"
           "+M-SHA512-HMAC+" "+M-SHA512-HMAC-GENERAL+" "+M-CAST-KEY-GEN+"
           "+M-CAST-ECB+" "+M-CAST-CBC+" "+M-CAST-MAC+" "+M-CAST-MAC-GENERAL+"
           "+M-CAST-CBC-PAD+" "+M-CAST3-KEY-GEN+" "+M-CAST3-ECB+" "+M-CAST3-CBC+"
           "+M-CAST3-MAC+" "+M-CAST3-MAC-GENERAL+" "+M-CAST3-CBC-PAD+"
           "+M-CAST5-KEY-GEN+" "+M-CAST128-KEY-GEN+" "+M-CAST5-ECB+"
           "+M-CAST128-ECB+" "+M-CAST5-CBC+" "+M-CAST128-CBC+" "+M-CAST5-MAC+"
           "+M-CAST128-MAC+" "+M-CAST5-MAC-GENERAL+" "+M-CAST128-MAC-GENERAL+"
           "+M-CAST5-CBC-PAD+" "+M-CAST128-CBC-PAD+" "+M-RC5-KEY-GEN+"
           "+M-RC5-ECB+" "+M-RC5-CBC+" "+M-RC5-MAC+" "+M-RC5-MAC-GENERAL+"
           "+M-RC5-CBC-PAD+" "+M-IDEA-KEY-GEN+" "+M-IDEA-ECB+" "+M-IDEA-CBC+"
           "+M-IDEA-MAC+" "+M-IDEA-MAC-GENERAL+" "+M-IDEA-CBC-PAD+"
           "+M-GENERIC-SECRET-KEY-GEN+" "+M-CONCATENATE-BASE-AND-KEY+"
           "+M-CONCATENATE-BASE-AND-DATA+" "+M-CONCATENATE-DATA-AND-BASE+"
           "+M-XOR-BASE-AND-DATA+" "+M-EXTRACT-KEY-FROM-KEY+"
           "+M-SSL3-PRE-MASTER-KEY-GEN+" "+M-SSL3-MASTER-KEY-DERIVE+"
           "+M-SSL3-KEY-AND-MAC-DERIVE+" "+M-SSL3-MASTER-KEY-DERIVE-DH+"
           "+M-TLS-PRE-MASTER-KEY-GEN+" "+M-TLS-MASTER-KEY-DERIVE+"
           "+M-TLS-KEY-AND-MAC-DERIVE+" "+M-TLS-MASTER-KEY-DERIVE-DH+"
           "+M-SSL3-MD5-MAC+" "+M-SSL3-SHA1-MAC+" "+M-MD5-KEY-DERIVATION+"
           "+M-MD2-KEY-DERIVATION+" "+M-SHA1-KEY-DERIVATION+"
           "+M-PBE-MD2-DES-CBC+" "+M-PBE-MD5-DES-CBC+" "+M-PBE-MD5-CAST-CBC+"
           "+M-PBE-MD5-CAST3-CBC+" "+M-PBE-MD5-CAST5-CBC+"
           "+M-PBE-MD5-CAST128-CBC+" "+M-PBE-SHA1-CAST5-CBC+"
           "+M-PBE-SHA1-CAST128-CBC+" "+M-PBE-SHA1-RC4-128+"
           "+M-PBE-SHA1-RC4-40+" "+M-PBE-SHA1-DES3-EDE-CBC+"
           "+M-PBE-SHA1-DES2-EDE-CBC+" "+M-PBE-SHA1-RC2-128-CBC+"
           "+M-PBE-SHA1-RC2-40-CBC+" "+M-PKCS5-PBKD2+"
           "+M-PBA-SHA1-WITH-SHA1-HMAC+" "+M-KEY-WRAP-LYNKS+"
           "+M-KEY-WRAP-SET-OAEP+" "+M-SKIPJACK-KEY-GEN+" "+M-SKIPJACK-ECB64+"
           "+M-SKIPJACK-CBC64+" "+M-SKIPJACK-OFB64+" "+M-SKIPJACK-CFB64+"
           "+M-SKIPJACK-CFB32+" "+M-SKIPJACK-CFB16+" "+M-SKIPJACK-CFB8+"
           "+M-SKIPJACK-WRAP+" "+M-SKIPJACK-PRIVATE-WRAP+" "+M-SKIPJACK-RELAYX+"
           "+M-KEA-KEY-PAIR-GEN+" "+M-KEA-KEY-DERIVE+" "+M-FORTEZZA-TIMESTAMP+"
           "+M-BATON-KEY-GEN+" "+M-BATON-ECB128+" "+M-BATON-ECB96+"
           "+M-BATON-CBC128+" "+M-BATON-COUNTER+" "+M-BATON-SHUFFLE+"
           "+M-BATON-WRAP+" "+M-ECDSA-KEY-PAIR-GEN+" "+M-EC-KEY-PAIR-GEN+"
           "+M-ECDSA+" "+M-ECDSA-SHA1+" "+M-ECDH1-DERIVE+"
           "+M-ECDH1-COFACTOR-DERIVE+" "+M-ECMQV-DERIVE+" "+M-JUNIPER-KEY-GEN+"
           "+M-JUNIPER-ECB128+" "+M-JUNIPER-CBC128+" "+M-JUNIPER-COUNTER+"
           "+M-JUNIPER-SHUFFLE+" "+M-JUNIPER-WRAP+" "+M-FASTHASH+"
           "+M-AES-KEY-GEN+" "+M-AES-ECB+" "+M-AES-CBC+" "+M-AES-MAC+"
           "+M-AES-MAC-GENERAL+" "+M-AES-CBC-PAD+" "+M-DSA-PARAMETER-GEN+"
           "+M-DH-PKCS-PARAMETER-GEN+" "+M-X9-42-DH-PARAMETER-GEN+" "+F-HW+"
           "+F-ENCRYPT+" "+F-DECRYPT+" "+F-DIGEST+" "+F-SIGN+" "+F-SIGN-RECOVER+"
           "+F-VERIFY+" "+F-VERIFY-RECOVER+" "+F-GENERATE+"
           "+F-GENERATE-KEY-PAIR+" "+F-WRAP+" "+F-UNWRAP+" "+F-DERIVE+"
           "+F-EXTENSION+" "+DONT-BLOCK+")

  (:export "+OK+" "+CANCEL+" "+HOST-MEMORY+" "+SLOT-ID-INVALID+"
           "+GENERAL-ERROR+" "+FUNCTION-FAILED+" "+ARGUMENTS-BAD+" "+NO-EVENT+"
           "+NEED-TO-CREATE-THREADS+" "+CANT-LOCK+" "+ATTRIBUTE-READ-ONLY+"
           "+ATTRIBUTE-SENSITIVE+" "+ATTRIBUTE-TYPE-INVALID+"
           "+ATTRIBUTE-VALUE-INVALID+" "+DATA-INVALID+" "+DATA-LEN-RANGE+"
           "+DEVICE-ERROR+" "+DEVICE-MEMORY+" "+DEVICE-REMOVED+"
           "+ENCRYPTED-DATA-INVALID+" "+ENCRYPTED-DATA-LEN-RANGE+"
           "+FUNCTION-CANCELED+" "+FUNCTION-NOT-PARALLEL+"
           "+FUNCTION-NOT-SUPPORTED+" "+KEY-HANDLE-INVALID+" "+KEY-SIZE-RANGE+"
           "+KEY-TYPE-INCONSISTENT+" "+KEY-NOT-NEEDED+" "+KEY-CHANGED+"
           "+KEY-NEEDED+" "+KEY-INDIGESTIBLE+" "+KEY-FUNCTION-NOT-PERMITTED+"
           "+KEY-NOT-WRAPPABLE+" "+KEY-UNEXTRACTABLE+" "+MECHANISM-INVALID+"
           "+MECHANISM-PARAM-INVALID+" "+OBJECT-HANDLE-INVALID+"
           "+OPERATION-ACTIVE+" "+OPERATION-NOT-INITIALIZED+" "+PIN-INCORRECT+"
           "+PIN-INVALID+" "+PIN-LEN-RANGE+" "+PIN-EXPIRED+" "+PIN-LOCKED+"
           "+SESSION-CLOSED+" "+SESSION-COUNT+" "+SESSION-HANDLE-INVALID+"
           "+SESSION-PARALLEL-NOT-SUPPORTED+" "+SESSION-READ-ONLY+"
           "+SESSION-EXISTS+" "+SESSION-READ-ONLY-EXISTS+"
           "+SESSION-READ-WRITE-SO-EXISTS+" "+SIGNATURE-INVALID+"
           "+SIGNATURE-LEN-RANGE+" "+TEMPLATE-INCOMPLETE+"
           "+TEMPLATE-INCONSISTENT+" "+TOKEN-NOT-PRESENT+"
           "+TOKEN-NOT-RECOGNIZED+" "+TOKEN-WRITE-PROTECTED+"
           "+UNWRAPPING-KEY-HANDLE-INVALID+" "+UNWRAPPING-KEY-SIZE-RANGE+"
           "+UNWRAPPING-KEY-TYPE-INCONSISTENT+" "+USER-ALREADY-LOGGED-IN+"
           "+USER-NOT-LOGGED-IN+" "+USER-PIN-NOT-INITIALIZED+"
           "+USER-TYPE-INVALID+" "+USER-ANOTHER-ALREADY-LOGGED-IN+"
           "+USER-TOO-MANY-TYPES+" "+WRAPPED-KEY-INVALID+"
           "+WRAPPED-KEY-LEN-RANGE+" "+WRAPPING-KEY-HANDLE-INVALID+"
           "+WRAPPING-KEY-SIZE-RANGE+" "+WRAPPING-KEY-TYPE-INCONSISTENT+"
           "+RANDOM-SEED-NOT-SUPPORTED+" "+RANDOM-NO-RNG+"
           "+DOMAIN-PARAMS-INVALID+" "+BUFFER-TOO-SMALL+" "+SAVED-STATE-INVALID+"
           "+INFORMATION-SENSITIVE+" "+STATE-UNSAVEABLE+"
           "+CRYPTOKI-NOT-INITIALIZED+" "+CRYPTOKI-ALREADY-INITIALIZED+"
           "+MUTEX-BAD+" "+MUTEX-NOT-LOCKED+" "+FUNCTION-REJECTED+")

  (:export "FLAGS" "RV" "NOTIFICATION" "SLOT-ID" "NOTIFY" "SESSION-HANDLE"
           "USER-TYPE" "STATE" "OBJECT-HANDLE" "OBJECT-CLASS" "HW-FEATURE-TYPE"
           "KEY-TYPE" "CERTIFICATE-TYPE" "ATTRIBUTE-TYPE" "MECHANISM-TYPE")

  (:export "ATTRIBUTE" "CRYPTOKI-VERSION" "DATE" "DAY" "DEVICE-ERROR"
           "FIRMWARE-VERSION" "FLAGS" "FREE-PRIVATE-MEMORY" "FREE-PUBLIC-MEMORY"
           "HARDWARE-VERSION" "INFO" "LABEL" "LIBRARY-DESCRIPTION"
           "LIBRARY-VERSION" "MAJOR" "MANUFACTURER-ID" "MAX-KEY-SIZE"
           "MAX-PIN-LEN" "MAX-RW-SESSION-COUNT" "MAX-SESSION-COUNT" "MECHANISM"
           "MECHANISM-INFO" "MIN-KEY-SIZE" "MIN-PIN-LEN" "MINOR" "MODEL" "MONTH"
           "PARAMETER" "PARAMETER-LEN" "RW-SESSION-COUNT" "SERIAL-NUMBER"
           "SESSION-COUNT" "SESSION-INFO" "SLOT-DESCRIPTION" "SLOT-ID"
           "SLOT-INFO" "STATE" "TOKEN-INFO" "TOTAL-PRIVATE-MMEORY"
           "TOTAL-PUBLIC-MEMORY" "TYPE" "UTC-TIME" "VALUE" "VALUE-LEN" "VERSION"
           "YEAR")

  (:documentation "CFFI interface over Cryptoki pkcs11 version 2.02"))

(in-package "COM.INFORMATIMAGO.CLEXT.PKCS11.LOW")

(defvar *loaded-library-pathname* nil)

(defun load-library (&optional library-pathname)
  "Load the Cryptoki pkcs11 library found at LIBRARY-PATHNAME, or some default path if not given."
  (if library-pathname
      (progn (load-foreign-library library-pathname)
             (setf *loaded-library-pathname* library-pathname))
      (or (find-if (lambda (pathname)
                     (ignore-errors (load-foreign-library pathname))
                     (setf *loaded-library-pathname* pathname))
                   #+darwin '("/opt/local/lib/opensc-pkcs11.bundle/Contents/MacOS/opensc-pkcs11"
                              "/opt/local/lib/libopensc.dylib"
                              "/opt/local/lib/libpkcs11-helper.dylib")
                   #+linux  '("/usr/local/lib/opensc-pkcs11.so"
                              "/usr/local/lib/libiaspkcs11.so"
                              "/usr/lib/x86_64-linux-gnu/opensc-pkcs11.so" #|an old version without C_Initialize et al.|#)
                   #-(or darwin linux) (error "What Cryptoki pkcs11 library shall I load?"))
          (error "Cannot find a Cryptoki pkcs11 library."))))

(defconstant +true+  1)
(defconstant +false+ 0)

(defcstruct version
  (major :uchar)
  (minor :uchar))



(defctype flags         :ulong)
(defctype rv            :ulong)

(defconstant +OK+                                #x000)
(defconstant +CANCEL+                            #x001)
(defconstant +HOST-MEMORY+                       #x002)
(defconstant +SLOT-ID-INVALID+                   #x003)
(defconstant +GENERAL-ERROR+                     #x005)
(defconstant +FUNCTION-FAILED+                   #x006)
(defconstant +ARGUMENTS-BAD+                     #x007)
(defconstant +NO-EVENT+                          #x008)
(defconstant +NEED-TO-CREATE-THREADS+            #x009)
(defconstant +CANT-LOCK+                         #x00a)
(defconstant +ATTRIBUTE-READ-ONLY+               #x010)
(defconstant +ATTRIBUTE-SENSITIVE+               #x011)
(defconstant +ATTRIBUTE-TYPE-INVALID+            #x012)
(defconstant +ATTRIBUTE-VALUE-INVALID+           #x013)
(defconstant +DATA-INVALID+                      #x020)
(defconstant +DATA-LEN-RANGE+                    #x021)
(defconstant +DEVICE-ERROR+                      #x030)
(defconstant +DEVICE-MEMORY+                     #x031)
(defconstant +DEVICE-REMOVED+                    #x032)
(defconstant +ENCRYPTED-DATA-INVALID+            #x040)
(defconstant +ENCRYPTED-DATA-LEN-RANGE+          #x041)
(defconstant +FUNCTION-CANCELED+                 #x050)
(defconstant +FUNCTION-NOT-PARALLEL+             #x051)
(defconstant +FUNCTION-NOT-SUPPORTED+            #x054)
(defconstant +KEY-HANDLE-INVALID+                #x060)
(defconstant +KEY-SIZE-RANGE+                    #x062)
(defconstant +KEY-TYPE-INCONSISTENT+             #x063)
(defconstant +KEY-NOT-NEEDED+                    #x064)
(defconstant +KEY-CHANGED+                       #x065)
(defconstant +KEY-NEEDED+                        #x066)
(defconstant +KEY-INDIGESTIBLE+                  #x067)
(defconstant +KEY-FUNCTION-NOT-PERMITTED+        #x068)
(defconstant +KEY-NOT-WRAPPABLE+                 #x069)
(defconstant +KEY-UNEXTRACTABLE+                 #x06a)
(defconstant +MECHANISM-INVALID+                 #x070)
(defconstant +MECHANISM-PARAM-INVALID+           #x071)
(defconstant +OBJECT-HANDLE-INVALID+             #x082)
(defconstant +OPERATION-ACTIVE+                  #x090)
(defconstant +OPERATION-NOT-INITIALIZED+         #x091)
(defconstant +PIN-INCORRECT+                     #x0a0)
(defconstant +PIN-INVALID+                       #x0a1)
(defconstant +PIN-LEN-RANGE+                     #x0a2)
(defconstant +PIN-EXPIRED+                       #x0a3)
(defconstant +PIN-LOCKED+                        #x0a4)
(defconstant +SESSION-CLOSED+                    #x0b0)
(defconstant +SESSION-COUNT+                     #x0b1)
(defconstant +SESSION-HANDLE-INVALID+            #x0b3)
(defconstant +SESSION-PARALLEL-NOT-SUPPORTED+    #x0b4)
(defconstant +SESSION-READ-ONLY+                 #x0b5)
(defconstant +SESSION-EXISTS+                    #x0b6)
(defconstant +SESSION-READ-ONLY-EXISTS+          #x0b7)
(defconstant +SESSION-READ-WRITE-SO-EXISTS+      #x0b8)
(defconstant +SIGNATURE-INVALID+                 #x0c0)
(defconstant +SIGNATURE-LEN-RANGE+               #x0c1)
(defconstant +TEMPLATE-INCOMPLETE+               #x0d0)
(defconstant +TEMPLATE-INCONSISTENT+             #x0d1)
(defconstant +TOKEN-NOT-PRESENT+                 #x0e0)
(defconstant +TOKEN-NOT-RECOGNIZED+              #x0e1)
(defconstant +TOKEN-WRITE-PROTECTED+             #x0e2)
(defconstant +UNWRAPPING-KEY-HANDLE-INVALID+     #x0f0)
(defconstant +UNWRAPPING-KEY-SIZE-RANGE+         #x0f1)
(defconstant +UNWRAPPING-KEY-TYPE-INCONSISTENT+  #x0f2)
(defconstant +USER-ALREADY-LOGGED-IN+            #x100)
(defconstant +USER-NOT-LOGGED-IN+                #x101)
(defconstant +USER-PIN-NOT-INITIALIZED+          #x102)
(defconstant +USER-TYPE-INVALID+                 #x103)
(defconstant +USER-ANOTHER-ALREADY-LOGGED-IN+    #x104)
(defconstant +USER-TOO-MANY-TYPES+               #x105)
(defconstant +WRAPPED-KEY-INVALID+               #x110)
(defconstant +WRAPPED-KEY-LEN-RANGE+             #x112)
(defconstant +WRAPPING-KEY-HANDLE-INVALID+       #x113)
(defconstant +WRAPPING-KEY-SIZE-RANGE+           #x114)
(defconstant +WRAPPING-KEY-TYPE-INCONSISTENT+    #x115)
(defconstant +RANDOM-SEED-NOT-SUPPORTED+         #x120)
(defconstant +RANDOM-NO-RNG+                     #x121)
(defconstant +DOMAIN-PARAMS-INVALID+             #x130)
(defconstant +BUFFER-TOO-SMALL+                  #x150)
(defconstant +SAVED-STATE-INVALID+               #x160)
(defconstant +INFORMATION-SENSITIVE+             #x170)
(defconstant +STATE-UNSAVEABLE+                  #x180)
(defconstant +CRYPTOKI-NOT-INITIALIZED+          #x190)
(defconstant +CRYPTOKI-ALREADY-INITIALIZED+      #x191)
(defconstant +MUTEX-BAD+                         #x1a0)
(defconstant +MUTEX-NOT-LOCKED+                  #x1a1)
(defconstant +FUNCTION-REJECTED+                 #x200)
;; +VENDOR-DEFINED+


(defctype notification  :ulong)
(defctype slot-id       :ulong)

(defconstant +surrender+ 0)

(defctype notify :pointer)
;; typedef ck_rv_t (*ck_notify_t) (ck_session_handle_t session, ck_notification_t event, void *application);

(defcstruct info
  (cryptoki-version    (:struct version))
  (manufacturer-id     :uchar :count 32)
  (flags               flags)
  (library-description :uchar :count 32)
  (library-version     (:struct version)))


(defcstruct slot-info
  (slot-description :uchar :count 64)
  (manufacturer-id  :uchar :count 32)
  (flags            flags)
  (hardware-version (:struct version))
  (firmware-version (:struct version)))

(defconstant +token-present+    (ash 1 0))
(defconstant +removable-device+ (ash 1 1))
(defconstant +hw-slot+          (ash 1 2))
(defconstant +array-attribute+  (ash 1 30))


(defcstruct token-info
  (label                :uchar   :count 32)
  (manufacturer-id      :uchar   :count 32)
  (model                :uchar   :count 16)
  (serial-number        :uchar   :count 16)
  (flags                flags)
  (max-session-count    :ulong)
  (session-count        :ulong)
  (max-rw-session-count :ulong)
  (rw-session-count     :ulong)
  (max-pin-len          :ulong)
  (min-pin-len          :ulong)
  (total-public-memory  :ulong)
  (free-public-memory   :ulong)
  (total-private-mmeory :ulong)
  (free-private-memory  :ulong)
  (hardware-version     (:struct version))
  (firmware-version     (:struct version))
  (utc-time             :uchar   :count 16))

(defconstant +RNG+                           (ash 1 0))
(defconstant +WRITE-PROTECTED+               (ash 1 1))
(defconstant +LOGIN-REQUIRED+                (ash 1 2))
(defconstant +USER-PIN-INITIALIZED+          (ash 1 3))
(defconstant +RESTORE-KEY-NOT-NEEDED+        (ash 1 5))
(defconstant +CLOCK-ON-TOKEN+                (ash 1 6))
(defconstant +PROTECTED-AUTHENTICATION-PATH+ (ash 1 8))
(defconstant +DUAL-CRYPTO-OPERATIONS+        (ash 1 9))
(defconstant +TOKEN-INITIALIZED+             (ash 1 10))
(defconstant +SECONDARY-AUTHENTICATION+      (ash 1 11))
(defconstant +USER-PIN-COUNT-LOW+            (ash 1 16))
(defconstant +USER-PIN-FINAL-TRY+            (ash 1 17))
(defconstant +USER-PIN-LOCKED+               (ash 1 18))
(defconstant +USER-PIN-TO-BE-CHANGED+        (ash 1 19))
(defconstant +SO-PIN-COUNT-LOW+              (ash 1 20))
(defconstant +SO-PIN-FINAL-TRY+              (ash 1 21))
(defconstant +SO-PIN-LOCKED+                 (ash 1 22))
(defconstant +SO-PIN-TO-BE-CHANGED+          (ash 1 23))

(defconstant +unavailable-information+
  #+(or (and ccl 64-bit-host))
  (- (expt 2 64) 1)
  #+(or (and ccl 32-bit-host))
  (- (expt 2 32) 1))
(defconstant +effectively-infinite+ 0)

(defctype session-handle :ulong)
(defconstant +invalid-handle+ 0)

(defctype user-type :ulong)
(defconstant +so+               0)
(defconstant +user+             1)
(defconstant +context-specific+ 2)

(defctype state :ulong)

(defconstant +RO-PUBLIC-SESSION+  0)
(defconstant +RO-USER-FUNCTIONS+  1)
(defconstant +RW-PUBLIC-SESSION+  2)
(defconstant +RW-USER-FUNCTIONS+  3)
(defconstant +RW-SO-FUNCTIONS+    4)


(defcstruct session-info
  (slot-id slot-id)
  (state   state)
  (flags   flags)
  (device-error :ulong))

(defconstant +RW-SESSION+      (ash 1 1))
(defconstant +SERIAL-SESSION+  (ash 1 2))


(defctype object-handle :ulong)
(defctype object-class  :ulong)
(defconstant +O-DATA+               0)
(defconstant +O-CERTIFICATE+        1)
(defconstant +O-PUBLIC-KEY+         2)
(defconstant +O-PRIVATE-KEY+        3)
(defconstant +O-SECRET-KEY+         4)
(defconstant +O-HW-FEATURE+         5)
(defconstant +O-DOMAIN-PARAMETERS+  6)
(defconstant +O-MECHANISM+          7)

(defconstant +VENDOR-DEFINED+       (ash 1 31))

(defctype hw-feature-type :ulong)
(defconstant +H-MONOTONIC-COUNTER+  1)
(defconstant +H-CLOCK+              2)
(defconstant +H-USER-INTERFACE+     3)
;; +vendor-defined+


(defctype key-type :ulong)
(defconstant +K-RSA+             #x00)
(defconstant +K-DSA+             #x01)
(defconstant +K-DH+              #x02)
(defconstant +K-ECDSA+           #x03)
(defconstant +K-EC+              #x03)
(defconstant +K-X9-42-DH+        #x04)
(defconstant +K-KEA+             #x05)
(defconstant +K-GENERIC-SECRET+  #x10)
(defconstant +K-RC2+             #x11)
(defconstant +K-RC4+             #x12)
(defconstant +K-DES+             #x13)
(defconstant +K-DES2+            #x14)
(defconstant +K-DES3+            #x15)
(defconstant +K-CAST+            #x16)
(defconstant +K-CAST3+           #x17)
(defconstant +K-CAST128+         #x18)
(defconstant +K-RC5+             #x19)
(defconstant +K-IDEA+            #x1a)
(defconstant +K-SKIPJACK+        #x1b)
(defconstant +K-BATON+           #x1c)
(defconstant +K-JUNIPER+         #x1d)
(defconstant +K-CDMF+            #x1e)
(defconstant +K-AES+             #x1f)
(defconstant +K-BLOWFISH+        #x20)
(defconstant +K-TWOFISH+         #x21)
;; +vendor-defined+

(defctype certificate-type  :ulong)
(defconstant +C-X-509+            0)
(defconstant +C-X-509-ATTR-CERT+  1)
(defconstant +C-WTLS+             2)
;; +vendor-defined+

(defctype attribute-type :ulong)
(defconstant +A-CLASS+                       #x000)
(defconstant +A-TOKEN+                       #x001)
(defconstant +A-PRIVATE+                     #x002)
(defconstant +A-LABEL+                       #x003)
(defconstant +A-APPLICATION+                 #x010)
(defconstant +A-VALUE+                       #x011)
(defconstant +A-OBJECT-ID+                   #x012)
(defconstant +A-CERTIFICATE-TYPE+            #x080)
(defconstant +A-ISSUER+                      #x081)
(defconstant +A-SERIAL-NUMBER+               #x082)
(defconstant +A-AC-ISSUER+                   #x083)
(defconstant +A-OWNER+                       #x084)
(defconstant +A-ATTR-TYPES+                  #x085)
(defconstant +A-TRUSTED+                     #x086)
(defconstant +A-CERTIFICATE-CATEGORY+        #x087)
(defconstant +A-JAVA-MIDP-SECURITY-DOMAIN+   #x088)
(defconstant +A-URL+                         #x089)
(defconstant +A-HASH-OF-SUBJECT-PUBLIC-KEY+  #x08a)
(defconstant +A-HASH-OF-ISSUER-PUBLIC-KEY+   #x08b)
(defconstant +A-CHECK-VALUE+                 #x090)
(defconstant +A-KEY-TYPE+                    #x100)
(defconstant +A-SUBJECT+                     #x101)
(defconstant +A-ID+                          #x102)
(defconstant +A-SENSITIVE+                   #x103)
(defconstant +A-ENCRYPT+                     #x104)
(defconstant +A-DECRYPT+                     #x105)
(defconstant +A-WRAP+                        #x106)
(defconstant +A-UNWRAP+                      #x107)
(defconstant +A-SIGN+                        #x108)
(defconstant +A-SIGN-RECOVER+                #x109)
(defconstant +A-VERIFY+                      #x10a)
(defconstant +A-VERIFY-RECOVER+              #x10b)
(defconstant +A-DERIVE+                      #x10c)
(defconstant +A-START-DATE+                  #x110)
(defconstant +A-END-DATE+                    #x111)
(defconstant +A-MODULUS+                     #x120)
(defconstant +A-MODULUS-BITS+                #x121)
(defconstant +A-PUBLIC-EXPONENT+             #x122)
(defconstant +A-PRIVATE-EXPONENT+            #x123)
(defconstant +A-PRIME-1+                     #x124)
(defconstant +A-PRIME-2+                     #x125)
(defconstant +A-EXPONENT-1+                  #x126)
(defconstant +A-EXPONENT-2+                  #x127)
(defconstant +A-COEFFICIENT+                 #x128)
(defconstant +A-PRIME+                       #x130)
(defconstant +A-SUBPRIME+                    #x131)
(defconstant +A-BASE+                        #x132)
(defconstant +A-PRIME-BITS+                  #x133)
(defconstant +A-SUB-PRIME-BITS+              #x134)
(defconstant +A-VALUE-BITS+                  #x160)
(defconstant +A-VALUE-LEN+                   #x161)
(defconstant +A-EXTRACTABLE+                 #x162)
(defconstant +A-LOCAL+                       #x163)
(defconstant +A-NEVER-EXTRACTABLE+           #x164)
(defconstant +A-ALWAYS-SENSITIVE+            #x165)
(defconstant +A-KEY-GEN-MECHANISM+           #x166)
(defconstant +A-MODIFIABLE+                  #x170)
(defconstant +A-ECDSA-PARAMS+                #x180)
(defconstant +A-EC-PARAMS+                   #x180)
(defconstant +A-EC-POINT+                    #x181)
(defconstant +A-SECONDARY-AUTH+              #x200)
(defconstant +A-AUTH-PIN-FLAGS+              #x201)
(defconstant +A-ALWAYS-AUTHENTICATE+         #x202)
(defconstant +A-WRAP-WITH-TRUSTED+           #x210)
(defconstant +A-HW-FEATURE-TYPE+             #x300)
(defconstant +A-RESET-ON-INIT+               #x301)
(defconstant +A-HAS-RESET+                   #x302)
(defconstant +A-PIXEL-X+                     #x400)
(defconstant +A-PIXEL-Y+                     #x401)
(defconstant +A-RESOLUTION+                  #x402)
(defconstant +A-CHAR-ROWS+                   #x403)
(defconstant +A-CHAR-COLUMNS+                #x404)
(defconstant +A-COLOR+                       #x405)
(defconstant +A-BITS-PER-PIXEL+              #x406)
(defconstant +A-CHAR-SETS+                   #x480)
(defconstant +A-ENCODING-METHODS+            #x481)
(defconstant +A-MIME-TYPES+                  #x482)
(defconstant +A-MECHANISM-TYPE+              #x500)
(defconstant +A-REQUIRED-CMS-ATTRIBUTES+     #x501)
(defconstant +A-DEFAULT-CMS-ATTRIBUTES+      #x502)
(defconstant +A-SUPPORTED-CMS-ATTRIBUTES+    #x503)
(defconstant +A-WRAP-TEMPLATE+               (logior +ARRAY-ATTRIBUTE+ #x211))
(defconstant +A-UNWRAP-TEMPLATE+             (logior +ARRAY-ATTRIBUTE+ #x212))
(defconstant +A-ALLOWED-MECHANISMS+          (logior +ARRAY-ATTRIBUTE+ #x600))
;; +vendor-defined+


(defcstruct attribute
  (type       attribute-type)
  (value     :pointer)
  (value-len :ulong))

(defcstruct date
  (year  :uchar :count 4)
  (month :uchar :count 2)
  (day   :uchar :count 2))

(defctype mechanism-type :ulong)
(defconstant +M-RSA-PKCS-KEY-PAIR-GEN+      #x0000)
(defconstant +M-RSA-PKCS+                   #x0001)
(defconstant +M-RSA-9796+                   #x0002)
(defconstant +M-RSA-X-509+                  #x0003)
(defconstant +M-MD2-RSA-PKCS+               #x0004)
(defconstant +M-MD5-RSA-PKCS+               #x0005)
(defconstant +M-SHA1-RSA-PKCS+              #x0006)
(defconstant +M-RIPEMD128-RSA-PKCS+         #x0007)
(defconstant +M-RIPEMD160-RSA-PKCS+         #x0008)
(defconstant +M-RSA-PKCS-OAEP+              #x0009)
(defconstant +M-RSA-X9-31-KEY-PAIR-GEN+     #x000a)
(defconstant +M-RSA-X9-31+                  #x000b)
(defconstant +M-SHA1-RSA-X9-31+             #x000c)
(defconstant +M-RSA-PKCS-PSS+               #x000d)
(defconstant +M-SHA1-RSA-PKCS-PSS+          #x000e)
(defconstant +M-DSA-KEY-PAIR-GEN+           #x0010)
(defconstant +M-DSA+                        #x0011)
(defconstant +M-DSA-SHA1+                   #x0012)
(defconstant +M-DH-PKCS-KEY-PAIR-GEN+       #x0020)
(defconstant +M-DH-PKCS-DERIVE+             #x0021)
(defconstant +M-X9-42-DH-KEY-PAIR-GEN+      #x0030)
(defconstant +M-X9-42-DH-DERIVE+            #x0031)
(defconstant +M-X9-42-DH-HYBRID-DERIVE+     #x0032)
(defconstant +M-X9-42-MQV-DERIVE+           #x0033)
(defconstant +M-SHA256-RSA-PKCS+            #x0040)
(defconstant +M-SHA384-RSA-PKCS+            #x0041)
(defconstant +M-SHA512-RSA-PKCS+            #x0042)
(defconstant +M-SHA256-RSA-PKCS-PSS+        #x0043)
(defconstant +M-SHA384-RSA-PKCS-PSS+        #x0044)
(defconstant +M-SHA512-RSA-PKCS-PSS+        #x0045)
(defconstant +M-RC2-KEY-GEN+                #x0100)
(defconstant +M-RC2-ECB+                    #x0101)
(defconstant +M-RC2-CBC+                    #x0102)
(defconstant +M-RC2-MAC+                    #x0103)
(defconstant +M-RC2-MAC-GENERAL+            #x0104)
(defconstant +M-RC2-CBC-PAD+                #x0105)
(defconstant +M-RC4-KEY-GEN+                #x0110)
(defconstant +M-RC4+                        #x0111)
(defconstant +M-DES-KEY-GEN+                #x0120)
(defconstant +M-DES-ECB+                    #x0121)
(defconstant +M-DES-CBC+                    #x0122)
(defconstant +M-DES-MAC+                    #x0123)
(defconstant +M-DES-MAC-GENERAL+            #x0124)
(defconstant +M-DES-CBC-PAD+                #x0125)
(defconstant +M-DES2-KEY-GEN+               #x0130)
(defconstant +M-DES3-KEY-GEN+               #x0131)
(defconstant +M-DES3-ECB+                   #x0132)
(defconstant +M-DES3-CBC+                   #x0133)
(defconstant +M-DES3-MAC+                   #x0134)
(defconstant +M-DES3-MAC-GENERAL+           #x0135)
(defconstant +M-DES3-CBC-PAD+               #x0136)
(defconstant +M-CDMF-KEY-GEN+               #x0140)
(defconstant +M-CDMF-ECB+                   #x0141)
(defconstant +M-CDMF-CBC+                   #x0142)
(defconstant +M-CDMF-MAC+                   #x0143)
(defconstant +M-CDMF-MAC-GENERAL+           #x0144)
(defconstant +M-CDMF-CBC-PAD+               #x0145)
(defconstant +M-MD2+                        #x0200)
(defconstant +M-MD2-HMAC+                   #x0201)
(defconstant +M-MD2-HMAC-GENERAL+           #x0202)
(defconstant +M-MD5+                        #x0210)
(defconstant +M-MD5-HMAC+                   #x0211)
(defconstant +M-MD5-HMAC-GENERAL+           #x0212)
(defconstant +M-SHA-1+                      #x0220)
(defconstant +M-SHA-1-HMAC+                 #x0221)
(defconstant +M-SHA-1-HMAC-GENERAL+         #x0222)
(defconstant +M-RIPEMD128+                  #x0230)
(defconstant +M-RIPEMD128-HMAC+             #x0231)
(defconstant +M-RIPEMD128-HMAC-GENERAL+     #x0232)
(defconstant +M-RIPEMD160+                  #x0240)
(defconstant +M-RIPEMD160-HMAC+             #x0241)
(defconstant +M-RIPEMD160-HMAC-GENERAL+     #x0242)
(defconstant +M-SHA256+                     #x0250)
(defconstant +M-SHA256-HMAC+                #x0251)
(defconstant +M-SHA256-HMAC-GENERAL+        #x0252)
(defconstant +M-SHA384+                     #x0260)
(defconstant +M-SHA384-HMAC+                #x0261)
(defconstant +M-SHA384-HMAC-GENERAL+        #x0262)
(defconstant +M-SHA512+                     #x0270)
(defconstant +M-SHA512-HMAC+                #x0271)
(defconstant +M-SHA512-HMAC-GENERAL+        #x0272)
(defconstant +M-CAST-KEY-GEN+               #x0300)
(defconstant +M-CAST-ECB+                   #x0301)
(defconstant +M-CAST-CBC+                   #x0302)
(defconstant +M-CAST-MAC+                   #x0303)
(defconstant +M-CAST-MAC-GENERAL+           #x0304)
(defconstant +M-CAST-CBC-PAD+               #x0305)
(defconstant +M-CAST3-KEY-GEN+              #x0310)
(defconstant +M-CAST3-ECB+                  #x0311)
(defconstant +M-CAST3-CBC+                  #x0312)
(defconstant +M-CAST3-MAC+                  #x0313)
(defconstant +M-CAST3-MAC-GENERAL+          #x0314)
(defconstant +M-CAST3-CBC-PAD+              #x0315)
(defconstant +M-CAST5-KEY-GEN+              #x0320)
(defconstant +M-CAST128-KEY-GEN+            #x0320)
(defconstant +M-CAST5-ECB+                  #x0321)
(defconstant +M-CAST128-ECB+                #x0321)
(defconstant +M-CAST5-CBC+                  #x0322)
(defconstant +M-CAST128-CBC+                #x0322)
(defconstant +M-CAST5-MAC+                  #x0323)
(defconstant +M-CAST128-MAC+                #x0323)
(defconstant +M-CAST5-MAC-GENERAL+          #x0324)
(defconstant +M-CAST128-MAC-GENERAL+        #x0324)
(defconstant +M-CAST5-CBC-PAD+              #x0325)
(defconstant +M-CAST128-CBC-PAD+            #x0325)
(defconstant +M-RC5-KEY-GEN+                #x0330)
(defconstant +M-RC5-ECB+                    #x0331)
(defconstant +M-RC5-CBC+                    #x0332)
(defconstant +M-RC5-MAC+                    #x0333)
(defconstant +M-RC5-MAC-GENERAL+            #x0334)
(defconstant +M-RC5-CBC-PAD+                #x0335)
(defconstant +M-IDEA-KEY-GEN+               #x0340)
(defconstant +M-IDEA-ECB+                   #x0341)
(defconstant +M-IDEA-CBC+                   #x0342)
(defconstant +M-IDEA-MAC+                   #x0343)
(defconstant +M-IDEA-MAC-GENERAL+           #x0344)
(defconstant +M-IDEA-CBC-PAD+               #x0345)
(defconstant +M-GENERIC-SECRET-KEY-GEN+     #x0350)
(defconstant +M-CONCATENATE-BASE-AND-KEY+   #x0360)
(defconstant +M-CONCATENATE-BASE-AND-DATA+  #x0362)
(defconstant +M-CONCATENATE-DATA-AND-BASE+  #x0363)
(defconstant +M-XOR-BASE-AND-DATA+          #x0364)
(defconstant +M-EXTRACT-KEY-FROM-KEY+       #x0365)
(defconstant +M-SSL3-PRE-MASTER-KEY-GEN+    #x0370)
(defconstant +M-SSL3-MASTER-KEY-DERIVE+     #x0371)
(defconstant +M-SSL3-KEY-AND-MAC-DERIVE+    #x0372)
(defconstant +M-SSL3-MASTER-KEY-DERIVE-DH+  #x0373)
(defconstant +M-TLS-PRE-MASTER-KEY-GEN+     #x0374)
(defconstant +M-TLS-MASTER-KEY-DERIVE+      #x0375)
(defconstant +M-TLS-KEY-AND-MAC-DERIVE+     #x0376)
(defconstant +M-TLS-MASTER-KEY-DERIVE-DH+   #x0377)
(defconstant +M-SSL3-MD5-MAC+               #x0380)
(defconstant +M-SSL3-SHA1-MAC+              #x0381)
(defconstant +M-MD5-KEY-DERIVATION+         #x0390)
(defconstant +M-MD2-KEY-DERIVATION+         #x0391)
(defconstant +M-SHA1-KEY-DERIVATION+        #x0392)
(defconstant +M-PBE-MD2-DES-CBC+            #x03a0)
(defconstant +M-PBE-MD5-DES-CBC+            #x03a1)
(defconstant +M-PBE-MD5-CAST-CBC+           #x03a2)
(defconstant +M-PBE-MD5-CAST3-CBC+          #x03a3)
(defconstant +M-PBE-MD5-CAST5-CBC+          #x03a4)
(defconstant +M-PBE-MD5-CAST128-CBC+        #x03a4)
(defconstant +M-PBE-SHA1-CAST5-CBC+         #x03a5)
(defconstant +M-PBE-SHA1-CAST128-CBC+       #x03a5)
(defconstant +M-PBE-SHA1-RC4-128+           #x03a6)
(defconstant +M-PBE-SHA1-RC4-40+            #x03a7)
(defconstant +M-PBE-SHA1-DES3-EDE-CBC+      #x03a8)
(defconstant +M-PBE-SHA1-DES2-EDE-CBC+      #x03a9)
(defconstant +M-PBE-SHA1-RC2-128-CBC+       #x03aa)
(defconstant +M-PBE-SHA1-RC2-40-CBC+        #x03ab)
(defconstant +M-PKCS5-PBKD2+                #x03b0)
(defconstant +M-PBA-SHA1-WITH-SHA1-HMAC+    #x03c0)
(defconstant +M-KEY-WRAP-LYNKS+             #x0400)
(defconstant +M-KEY-WRAP-SET-OAEP+          #x0401)
(defconstant +M-SKIPJACK-KEY-GEN+           #x1000)
(defconstant +M-SKIPJACK-ECB64+             #x1001)
(defconstant +M-SKIPJACK-CBC64+             #x1002)
(defconstant +M-SKIPJACK-OFB64+             #x1003)
(defconstant +M-SKIPJACK-CFB64+             #x1004)
(defconstant +M-SKIPJACK-CFB32+             #x1005)
(defconstant +M-SKIPJACK-CFB16+             #x1006)
(defconstant +M-SKIPJACK-CFB8+              #x1007)
(defconstant +M-SKIPJACK-WRAP+              #x1008)
(defconstant +M-SKIPJACK-PRIVATE-WRAP+      #x1009)
(defconstant +M-SKIPJACK-RELAYX+            #x100a)
(defconstant +M-KEA-KEY-PAIR-GEN+           #x1010)
(defconstant +M-KEA-KEY-DERIVE+             #x1011)
(defconstant +M-FORTEZZA-TIMESTAMP+         #x1020)
(defconstant +M-BATON-KEY-GEN+              #x1030)
(defconstant +M-BATON-ECB128+               #x1031)
(defconstant +M-BATON-ECB96+                #x1032)
(defconstant +M-BATON-CBC128+               #x1033)
(defconstant +M-BATON-COUNTER+              #x1034)
(defconstant +M-BATON-SHUFFLE+              #x1035)
(defconstant +M-BATON-WRAP+                 #x1036)
(defconstant +M-ECDSA-KEY-PAIR-GEN+         #x1040)
(defconstant +M-EC-KEY-PAIR-GEN+            #x1040)
(defconstant +M-ECDSA+                      #x1041)
(defconstant +M-ECDSA-SHA1+                 #x1042)
(defconstant +M-ECDH1-DERIVE+               #x1050)
(defconstant +M-ECDH1-COFACTOR-DERIVE+      #x1051)
(defconstant +M-ECMQV-DERIVE+               #x1052)
(defconstant +M-JUNIPER-KEY-GEN+            #x1060)
(defconstant +M-JUNIPER-ECB128+             #x1061)
(defconstant +M-JUNIPER-CBC128+             #x1062)
(defconstant +M-JUNIPER-COUNTER+            #x1063)
(defconstant +M-JUNIPER-SHUFFLE+            #x1064)
(defconstant +M-JUNIPER-WRAP+               #x1065)
(defconstant +M-FASTHASH+                   #x1070)
(defconstant +M-AES-KEY-GEN+                #x1080)
(defconstant +M-AES-ECB+                    #x1081)
(defconstant +M-AES-CBC+                    #x1082)
(defconstant +M-AES-MAC+                    #x1083)
(defconstant +M-AES-MAC-GENERAL+            #x1084)
(defconstant +M-AES-CBC-PAD+                #x1085)
(defconstant +M-DSA-PARAMETER-GEN+          #x2000)
(defconstant +M-DH-PKCS-PARAMETER-GEN+      #x2001)
(defconstant +M-X9-42-DH-PARAMETER-GEN+     #x2002)
;; +VENDOR-DEFINED+

(defcstruct mechanism
  (mechanism mechanism-type)
  (parameter       :pointer)
  (parameter-len   :ulong))

(defcstruct mechanism-info
  (min-key-size :ulong)
  (max-key-size :ulong)
  (flags        flags))

(defconstant +F-HW+                 (ash 1 0))
(defconstant +F-ENCRYPT+            (ash 1 8))
(defconstant +F-DECRYPT+            (ash 1 9))
(defconstant +F-DIGEST+             (ash 1 10))
(defconstant +F-SIGN+               (ash 1 11))
(defconstant +F-SIGN-RECOVER+       (ash 1 12))
(defconstant +F-VERIFY+             (ash 1 13))
(defconstant +F-VERIFY-RECOVER+     (ash 1 14))
(defconstant +F-GENERATE+           (ash 1 15))
(defconstant +F-GENERATE-KEY-PAIR+  (ash 1 16))
(defconstant +F-WRAP+               (ash 1 17))
(defconstant +F-UNWRAP+             (ash 1 18))
(defconstant +F-DERIVE+             (ash 1 19))
(defconstant +F-EXTENSION+          (ash 1 31))

;; Flags for C-WaitForSlotEvent.
(defconstant +DONT-BLOCK+  1)

(defcfun (initialize "C_Initialize") rv (init-args :pointer))
(defcfun (finalize   "C_Finalize")   rv (reserved  :pointer))
(defcfun (get-info   "C_GetInfo")    rv (info     (:pointer (:struct info))))


(defcfun (get-slot-list       "C_GetSlotList")      rv
  (token-present :uchar)
  (slot-list (:pointer slot-id))
  (count (:pointer :ulong)))

(defcfun (get-slot-info       "C_GetSlotInfo")      rv
  (slot-id       slot-id)
  (info      (:pointer (:struct slot-info))))

(defcfun (get-token-info      "C_GetTokenInfo")     rv
  (slot-id       slot-id)
  (info      (:pointer (:struct token-info))))

(defcfun (wait-for-slot-event "C_WaitForSlotEvent") rv
  (flags         flags)
  (slot      (:pointer slot-id))
  (reserved  :pointer))

(defcfun (get-mechanism-list "C_GetMechanismList")    rv
  (slot-id        slot-id)
  (mechanism-list (:pointer mechanism-type))  (count (:pointer :ulong)))

(defcfun (get-mechanism-info "C_GetMechanismInfo")    rv
  (slot-id slot-id)
  (type    mechanism-type)
  (info    (:pointer (:struct mechanism-info))))

(defcfun (init-token "C_InitToken")    rv
  (slot-id slot-id)
  (pin     (:pointer :uchar))
  (pin-len :ulong)
  (label   (:pointer :uchar)))

(defcfun (init-pin "C_InitPIN") rv
  (session session-handle)
  (pin     (:pointer :uchar))
  (pin-len :ulong))

(defcfun (set-pin "C_SetPIN")    rv
  (session session-handle)
  (old-pin (:pointer :uchar))
  (old-len :ulong)
  (new-pin (:pointer :uchar))
  (new-len :ulong))

(defcfun (open-session "C_OpenSession")    rv
  (slot-id     slot-id)
  (flags       flags)
  (application :pointer)
  (notify      notify)
  (session     (:pointer session-handle)))

(defcfun (close-session "C_CloseSession") rv
  (session session-handle))

(defcfun (close-all-sessions "C_CloseAllSessions") rv
  (slot-id slot-id))

(defcfun (get-session-info "C_GetSessionInfo")    rv
  (session session-handle)
  (info    (:pointer (:struct session-info))))

(defcfun (get-operation-state "C_GetOperationState")    rv
  (session             session-handle)
  (operation-state     (:pointer :uchar))
  (operation-state-len (:pointer :ulong)))

(defcfun (set-operation-state "C_SetOperationState")    rv
  (session             session-handle)
  (operation-state     (:pointer :uchar))
  (operation-state-len :ulong)
  (encryption-key      object-handle)
  (authentication-key  object-handle))

(defcfun (login "C_Login")    rv
  (session   session-handle)
  (user-type user-type)
  (pin       (:pointer :uchar))
  (pin-len   :ulong))

(defcfun (logout "C_Logout") rv
  (session session-handle))

(defcfun (create-object "C_CreateObject")    rv
  (session session-handle)
  (templ (:pointer (:struct attribute)))
  (count :ulong)
  (object (:pointer object-handle)))

(defcfun (copy-object "C_CopyObject")    rv
  (session    session-handle)
  (object     object-handle)
  (templ      (:pointer (:struct attribute)))
  (count      :ulong)
  (new-object (:pointer object-handle)))

(defcfun (destroy-object "C_DestroyObject") rv
  (session session-handle)
  (object object-handle))

(defcfun (get-object-size "C_GetObjectSize")    rv
  (session session-handle)
  (object  object-handle)
  (size    (:pointer :ulong)))

(defcfun (get-attribute-value "C_GetAttributeValue")    rv
  (session session-handle)
  (object  object-handle)
  (templ   (:pointer (:struct attribute)))
  (count   :ulong))

(defcfun (set-attribute-value "C_SetAttributeValue")    rv
  (session session-handle)
  (object  object-handle)
  (templ   (:pointer (:struct attribute)))
  (count   :ulong))

(defcfun (find-objects-init "C_FindObjectsInit")    rv
  (session session-handle)
  (templ   (:pointer (:struct attribute)))
  (count   :ulong))

(defcfun (find-objects "C_FindObjects")    rv
  (session          session-handle)
  (object           (:pointer object-handle))
  (max-object-count :ulong)
  (object-count     (:pointer :ulong)))

(defcfun (find-objects-final "C_FindObjectsFinal") rv (session session-handle))

(defcfun (encrypt-init "C_EncryptInit")    rv
  (session   session-handle)
  (mechanism (:pointer (:struct mechanism)))
  (key       object-handle))

(defcfun (encrypt "C_Encrypt")    rv
  (session            session-handle)
  (data               (:pointer :uchar))
  (data-len           :ulong)
  (encrypted-data     (:pointer :uchar))
  (encrypted-data-len (:pointer :ulong)))

(defcfun (encrypt-update "C_EncryptUpdate")    rv
  (session            session-handle)
  (part               (:pointer :uchar))
  (part-len           :ulong)
  (encrypted-part     (:pointer :uchar))
  (encrypted-part-len (:pointer :ulong)))

(defcfun (encrypt-final "C_EncryptFinal")    rv
  (session                 session-handle)
  (last-encrypted-part     (:pointer :uchar))
  (last-encrypted-part-len (:pointer :ulong)))

(defcfun (decrypt-init "C_DecryptInit")    rv
  (session   session-handle)
  (mechanism (:pointer (:struct mechanism)))
  (key       object-handle))

(defcfun (decrypt "C_Decrypt")    rv
  (session            session-handle)
  (encrypted-data     (:pointer :uchar))
  (encrypted-data-len :ulong)
  (data               (:pointer :uchar))
  (data-len           (:pointer :ulong)))

(defcfun (decrypt-update "C_DecryptUpdate")    rv
  (session            session-handle)
  (encrypted-part     (:pointer :uchar))
  (encrypted-part-len :ulong)
  (part               (:pointer :uchar))
  (part-len           (:pointer :ulong)))

(defcfun (decrypt-final "C_DecryptFinal")    rv
  (session       session-handle)
  (last-part     (:pointer :uchar))
  (last-part-len (:pointer :ulong)))

(defcfun (digest-init "C_DigestInit") rv (session session-handle) (mechanism (:pointer (:struct mechanism))))

(defcfun (digest "C_Digest")    rv
  (session    session-handle)
  (data       (:pointer :uchar))
  (data-len   :ulong)
  (digest     (:pointer :uchar))
  (digest-len (:pointer :ulong)))

(defcfun (digest-update "C_DigestUpdate") rv (session session-handle) (part (:pointer :uchar)) (part-len :ulong))
(defcfun (digest-key    "C_DigestKey")    rv (session session-handle) (key  object-handle))

(defcfun (digest-final "C_DigestFinal")    rv
  (session    session-handle)
  (digest     (:pointer :uchar))
  (digest-len (:pointer :ulong)))

(defcfun (sign-init "C_SignInit")    rv
  (session   session-handle)
  (mechanism (:pointer (:struct mechanism)))
  (key       object-handle))

(defcfun (sign "C_Sign")    rv
  (session       session-handle)
  (data          (:pointer :uchar))
  (data-len      :ulong)
  (signature     (:pointer :uchar))
  (signature-len (:pointer :ulong)))

(defcfun (sign-update "C_SignUpdate") rv
  (session  session-handle)
  (part     (:pointer :uchar))
  (part-len :ulong))

(defcfun (sign-final "C_SignFinal")    rv
  (session       session-handle)
  (signature     (:pointer :uchar))
  (signature-len (:pointer :ulong)))

(defcfun (sign-recover-init "C_SignRecoverInit")    rv
  (session   session-handle)
  (mechanism (:pointer (:struct mechanism)))
  (key       object-handle))

(defcfun (sign-recover "C_SignRecover")    rv
  (session       session-handle)
  (data          (:pointer :uchar))
  (data-len      :ulong)
  (signature     (:pointer :uchar))
  (signature-len (:pointer :ulong)))

(defcfun (verify-init "C_VerifyInit")    rv
  (session   session-handle)
  (mechanism (:pointer (:struct mechanism)))
  (key       object-handle))

(defcfun (verify "C_Verify")    rv
  (session       session-handle)
  (data          (:pointer :uchar))
  (data-len      :ulong)
  (signature     (:pointer :uchar))
  (signature-len :ulong))

(defcfun (verify-update "C_VerifyUpdate") rv
  (session  session-handle)
  (part     (:pointer :uchar))
  (part-len :ulong))

(defcfun (verify-final "C_VerifyFinal")    rv
  (session       session-handle)
  (signature     (:pointer :uchar))
  (signature-len :ulong))

(defcfun (verify-recover-init "C_VerifyRecoverInit")    rv
  (session   session-handle)
  (mechanism (:pointer (:struct mechanism)))
  (key       object-handle))

(defcfun (verify-recover "C_VerifyRecover")    rv
  (session       session-handle)
  (signature     (:pointer :uchar))
  (signature-len :ulong)
  (data          (:pointer :uchar))
  (data-len      (:pointer :ulong)))

(defcfun (digest-encrypt-update "C_DigestEncryptUpdate")    rv
  (session            session-handle)
  (part               (:pointer :uchar))
  (part-len           :ulong)
  (encrypted-part     (:pointer :uchar))
  (encrypted-part-len (:pointer :ulong)))

(defcfun (decrypt-digest-update "C_DecryptDigestUpdate")    rv
  (session            session-handle)
  (encrypted-part     (:pointer :uchar))
  (encrypted-part-len :ulong)
  (part               (:pointer :uchar))
  (part-len           (:pointer :ulong)))

(defcfun (sign-encrypt-update "C_SignEncryptUpdate")    rv
  (session            session-handle)
  (part               (:pointer :uchar))
  (part-len           :ulong)
  (encrypted-part     (:pointer :uchar))
  (encrypted-part-len (:pointer :ulong)))

(defcfun (decrypt-verify-update "C_DecryptVerifyUpdate")  rv
  (session            session-handle)
  (encrypted-part     (:pointer :uchar))
  (encrypted-part-len :ulong)
  (part               (:pointer :uchar))
  (part-len           (:pointer :ulong)))

(defcfun (generate-key "C_GenerateKey")  rv
  (session   session-handle)
  (mechanism (:pointer (:struct mechanism)))
  (templ     (:pointer (:struct attribute)))
  (count     :ulong)
  (key       (:pointer object-handle)))

(defcfun (generate-key-pair "C_GenerateKeyPair") rv
  (session                     session-handle)
  (mechanism                   (:pointer (:struct mechanism)))
  (public-key-template         (:pointer (:struct attribute)))
  (public-key-attribute-count  :ulong)
  (private-key-template        (:pointer (:struct attribute)))
  (private-key-attribute-count :ulong)
  (public-key                  (:pointer object-handle))
  (private-key                 (:pointer object-handle)))

(defcfun (wrap-key "C_WrapKey")    rv
  (session         session-handle)
  (mechanism       (:pointer (:struct mechanism)))
  (wrapping-key    object-handle)
  (key             object-handle)
  (wrapped-key     (:pointer :uchar))
  (wrapped-key-len (:pointer :ulong)))

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
  (key             (:pointer object-handle)))

(defcfun (seed-random         "C_SeedRandom")        rv  (session session-handle)  (seed        (:pointer :uchar))  (seed-len   :ulong))
(defcfun (generate-random     "C_GenerateRandom")    rv  (session session-handle)  (random-data (:pointer :uchar))  (random-len :ulong))
(defcfun (get-function-status "C_GetFunctionStatus") rv  (session session-handle))
(defcfun (cancel-function     "C_CancelFunction")    rv  (session session-handle))


;;;; THE END ;;;;
